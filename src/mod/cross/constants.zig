//! Stores constants for many passes of the compiler and interpreter. Created
//! using the contained `Builder` struct.
const std = @import("std");
const Variant = @import("variant.zig").Variant;
const Instruction = @import("../compiler/instruction.zig");
const Name = @import("../text/name.zig");
const Symbols = @import("symbols.zig");
const Constants = @This();

alloc: std.mem.Allocator,
functions: std.ArrayList(Function) = .empty,
globals: std.ArrayList(Variant) = .empty,
/// Maps string names to global IDs. The names are owned by `self.syms`.
names: std.HashMap(Name, ID, Name.HashContext, 80).Unmanaged = .empty,
/// Caches function contents to existing IDs, to avoid duplicating functions.
contents_hash: CodeHash.Unmanaged = .empty,
/// Stores the symbols for the builder.
syms: Symbols,

/// Hash map with bytecode as a key.
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

/// An ID for a global variant
pub const ID = u32;

pub const Function = struct {
	/// The code of the function. Owned by the function.
	code: []Instruction,
	/// This filename is not owned by the constant storage.
	filename: []const u8,

	pub fn deinit(self: Function, alloc: std.mem.Allocator) void {
		alloc.free(self.code);
	}
};

pub fn init(alloc: std.mem.Allocator) Constants {
	return .{
		.alloc = alloc,
		.syms = .init(alloc),
	};
}

/// Clears cached information. Saves memory but may make compiling to this
/// constants later less efficient, or work in unexpected ways.
pub fn clearCache(self: *Constants) void {
	self.contents_hash.clearAndFree(self.alloc);
}

/// Deinitializes the constants and all contained structures.
pub fn deinit(self: Constants) void {
	for (self.functions.items) |f| f.deinit(self.alloc);
	for (self.globals.items) |v| v.dec(self.alloc);

	// names for `names` are stored in `syms`.
	self.names.deinit(self.alloc);
	self.functions.deinit(self.alloc);
	self.globals.deinit(self.alloc);
	self.syms.deinit();
}

/// Returns a struct used to create a function. Call finish() on the
/// returned value to add a function to this struct.
pub fn createFunc(self: *Constants, filename: []const u8) CreateFunc {
	return .{
		.constants = self,
		.builder = .init(self.alloc),
		.filename = filename,
	};
}

/// Adds a function. This function's code must be owned by the same allocator!
/// Returns the global ID of the variant pointing to the function.
pub fn addFunc(self: *Constants, f: Function) std.mem.Allocator.Error!ID {
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
pub fn finalize(self: *Constants, alloc: std.mem.Allocator) std.mem.Allocator.Error!Constants {
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

pub fn get(self: @This(), id: ID) *Variant {
	return &self.globals.items[id];
}

/// Adds a named global. Returns an error if
pub inline fn addNamedByStr(self: *@This(), name: []const u8, value: Variant) !ID {
	return try self.addNamed(try Name.from(name), value);
}

pub fn addNamed(self: *@This(), in_name: Name, value: Variant) std.mem.Allocator.Error!ID {
	const var_id: ID = @intCast(self.globals.items.len);
	const name = try self.syms.ownName(in_name);
	const spot = try self.globals.addOne(self.alloc);
	errdefer _ = self.globals.pop();

	try self.names.put(self.alloc, name.*, var_id);
	spot.* = value;
	return var_id;
}

pub inline fn getNamedByStr(self: @This(), name: []const u8) Name.Error!?ID {
	return self.getNamed(try Name.from(name));
}

pub fn getNamed(self: @This(), in_name: Name) ?ID {
	return self.names.get(in_name);
}

pub const CreateFunc = struct {
	pub const Builder = @import("../compiler/funcs/create_func.zig");

	constants: *Constants,
	filename: []const u8,
	builder: Builder,

	pub fn deinit(self: *CreateFunc) void {
		self.builder.deinit();
	}

	/// Finishes the `CreateFunc`. `deinit()` does not need to be called after.
	pub fn finish(self: *CreateFunc) !ID {
		const code = try self.builder.finalize(self.constants.alloc);
		errdefer self.constants.alloc.free(code);
		return try self.constants.addFunc(.{
			.code = code,
			.filename = self.filename,
		});
	}
};