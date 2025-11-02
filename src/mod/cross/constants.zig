//! Stores constants for many passes of the compiler and interpreter. Created
//! using the contained `Builder` struct.
const std = @import("std");
const Variant = @import("variant.zig").Variant;
const Instruction = @import("../compiler/instruction.zig");
const Name = @import("../text/name.zig");

const Constants = @This();

pub const Symbols = struct {
	pub const Error = error {SymbolLimitExceeded};

	pub const SymbolID = @import("variant.zig").SymbolID;
	pub const max_symbol_count = std.math.maxInt(SymbolID);
	pub const NameHash = std.HashMap(Name, SymbolID, Name.HashContext, 80);

	alloc: std.mem.Allocator,
	name_to_id: NameHash.Unmanaged,
	names: std.ArrayList(Name),

	pub fn init(alloc: std.mem.Allocator) Symbols {
		return .{
			.alloc = alloc,
			.name_to_id = .empty,
			.names = .empty,
		};
	}

	/// Returns the ID of a string. Makes a copy of the provided name.
	pub fn idOfString(self: *Symbols, name_str: []const u8) (Error || Name.Error || std.mem.Allocator.Error)!SymbolID {
		const name = try Name.from(name_str);
		const pair = try self.name_to_id.getOrPut(self.alloc, name);

		if (pair.found_existing) return pair.value_ptr.*;
		errdefer _ = self.name_to_id.remove(name);

		if (self.name_to_id.size >= max_symbol_count) {
			return Error.SymbolLimitExceeded;
		}

		const array_spot = try self.names.addOne(self.alloc);
		errdefer _ = self.names.pop();

		const id: SymbolID = @intCast(self.name_to_id.size - 1);

		// The key should store an owned copy of the name
		pair.key_ptr.text = try self.alloc.dupe(u8, name_str);
		pair.value_ptr.* = id;
		array_spot.* = pair.key_ptr.*;
		return id;
	}

	pub fn variant(self: *Symbols, sym_name: []const u8) (Error || Name.Error || std.mem.Allocator.Error)!Variant {
		return .{.symbol = try self.idOfString(sym_name)};
	}

	pub fn nameOf(self: Symbols, id: SymbolID) *const Name {
		return self.names.items[id];
	}

	/// Clears the memory containing the text of all the names.
	pub fn deinit(self: Symbols) void {
		for (self.names.items) |name| {
			self.alloc.free(name.text);
		}
		self.name_to_id.deinit(self.alloc);
		self.names.deinit(self.alloc);
	}
};

const CodeHash = std.HashMap([]const Instruction, ID, struct {
	pub fn hash(_: @This(), arr: []const Instruction) u64 {
		var hasher = std.hash.Wyhash.init(arr.len);
		for (arr) |inst| {
			const h: u64 = inst.hash();
			const ptr: *const [8]u8 = @ptrCast(&h);
			hasher.update(ptr);
		}
		return hasher.final();
	}

	pub fn eql(_: @This(), a: []const Instruction, b: []const Instruction) bool {
		if (a.len != b.len) return false;
		for (a, b) |ai, bi| {
			if (!ai.eql(bi)) return false;
		}
		return true;
	}
}, 80);

pub const ID = u32;

pub const Function = struct {
	/// The code of the function. Owned by the function.
	code: []Instruction,
	/// This filename is not owned by the constant storage.
	filename: []const u8,
};

/// Builds a `Constants` while compiling code. Used when compiling.
pub const Builder = struct {
	alloc: std.mem.Allocator,
	functions: std.ArrayList(Function) = .empty,
	globals: std.ArrayList(Variant) = .empty,
	/// Maps string names to global IDs. The names are owned by the builder.
	names: std.StringHashMapUnmanaged(ID) = .empty,
	/// Caches function contents to existing IDs, to avoid duplicating functions.
	contents_hash: CodeHash.Unmanaged = .empty,
	/// Stores the symbols for the builder.
	syms: Symbols,

	pub fn init(alloc: std.mem.Allocator) Builder {
		return .{
			.alloc = alloc,
			.syms = .init(alloc),
		};
	}

	pub const CreateFunc = struct {
		builder: *Builder,
		insts: std.ArrayList(Instruction),
		filename: []const u8,

		pub fn add(self: *CreateFunc, i: Instruction) !void {
			try self.insts.append(self.builder.alloc, i);
		}

		/// Adds an instruction and returns its index.
		pub fn addOne(self: *CreateFunc) !usize {
			_ = try self.insts.addOne(self.builder.alloc);
			return self.insts.items.len - 1;
		}

		pub fn deinit(self: *CreateFunc) void {
			self.insts.deinit(self.builder.alloc);
		}

		pub fn finish(self: *CreateFunc) !ID {
			return try self.builder.addFunc(.{
				.code = try self.insts.toOwnedSlice(self.builder.alloc),
				.filename = self.filename,
			});
		}
	};

	/// Returns a struct used to create a function. Call finish() on the
	/// returned value to add a function to this struct.
	pub fn createFunc(self: *Builder, filename: []const u8) CreateFunc {
		return .{
			.builder = self,
			.insts = .empty,
			.filename = filename,
		};
	}

	/// Adds a function. This function's code must be owned by the same allocator!
	/// Returns the global ID of the variant pointing to the function.
	pub fn addFunc(self: *Builder, f: Function) std.mem.Allocator.Error!ID {
		const alloc = self.alloc;

		if (self.contents_hash.get(f.code)) |existing_id| return existing_id;

		const func_id: ID = @intCast(self.functions.items.len);
		try self.functions.append(alloc, f);
		errdefer _ = self.functions.pop();

		const global_id: ID = @intCast(self.globals.items.len);
		try self.globals.append(alloc, .{.function_ref = func_id});
		errdefer _ = self.globals.pop();

		//if (name) |name_n| {
			//const dup_name = try alloc.dupe(u8, name_n);
			//errdefer alloc.free(dup_name);
			//try self.names.put(alloc, dup_name, global_id);
		//}

		self.contents_hash.put(alloc, f.code, global_id) catch {};
		return global_id;
	}

	/// Finalizes and converts to a `Constants`. Currently does not properly
	/// free memory if the operation fails due to an out of memory!
	pub fn finalize(self: *Builder, alloc: std.mem.Allocator) std.mem.Allocator.Error!Constants {
		defer self.* = undefined;

		var iter = self.names.iterator();
		while (iter.next()) |pair| alloc.free(pair.key_ptr.*);
		self.names.deinit(alloc);

		return .{
			.alloc = alloc,
			.functions = try self.functions.toOwnedSlice(alloc),
			.globals = try self.globals.toOwnedSlice(alloc),
		};
	}
};

alloc: std.mem.Allocator,
/// Stores a list of functions. The code of each function is owned by this struct.
functions: []Function,
/// Stores a list of globals. By default, they have exactly 1 reference.
globals: []Variant,

pub fn get(self: @This(), id: ID) *Variant {
	return &self.globals[id];
}

pub fn deinit(self: Constants) void {
	for (self.functions) |f| self.alloc.free(f.code);
	for (self.globals) |v| v.dec(self.alloc);
	self.alloc.free(self.functions);
	self.alloc.free(self.globals);
}