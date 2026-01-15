//! Keeps track of symbols.
const std = @import("std");
const Name = @import("../text/name.zig");
const Variant = @import("variant.zig").Variant;
const Symbols = @This();

alloc: std.mem.Allocator,
name_to_id: NameHash.Unmanaged,
names: std.ArrayList(Name),

pub const SymbolID = u16;
pub const Error = error {SymbolLimitExceeded};
pub const max_symbol_count = std.math.maxInt(SymbolID);
pub const NameHash = std.HashMap(Name, SymbolID, Name.HashContext, 80);

pub fn init(alloc: std.mem.Allocator) Symbols {
	return .{
		.alloc = alloc,
		.name_to_id = .empty,
		.names = .empty,
	};
}

/// Returns the ID of a string. Makes a copy of the provided name.
pub inline fn idOfString(self: *Symbols, name_str: []const u8) (Error || Name.Error || std.mem.Allocator.Error)!SymbolID {
	const name = try Name.from(name_str);
	return (try self.addOrCreate(name)).id;
}

/// Returns the owned version of the name.
pub inline fn ownName(self: *Symbols, name: Name) std.mem.Allocator.Error!*const Name {
	return (try self.addOrCreate(name)).name;
}

pub fn addOrCreate(self: *Symbols, name: Name) std.mem.Allocator.Error!struct {name: *const Name, id: SymbolID} {
	const kv = try self.name_to_id.getOrPut(self.alloc, name);
	if (kv.found_existing) return .{.name = kv.key_ptr, .id = kv.value_ptr.*};
	errdefer _ = self.name_to_id.remove(name);

	const id: SymbolID = @intCast(self.names.items.len);
	const spot = try self.names.addOne(self.alloc);
	errdefer _ = self.names.pop();

	kv.key_ptr.* = name;
	kv.key_ptr.text = try self.alloc.dupe(u8, name.text);
	kv.value_ptr.* = id;
	spot.* = name;
	return .{.name = kv.key_ptr, .id = id};
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