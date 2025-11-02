//! Keeps track of symbols.
const std = @import("std");
const Name = @import("../text/name.zig");
const Variant = @import("variant.zig").Variant;
const Symbols = @This();

pub const SymbolID = u16;
pub const Error = error {SymbolLimitExceeded};
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