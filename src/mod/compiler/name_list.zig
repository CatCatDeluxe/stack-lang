//! Stores a list of names which can be added to and searched.
//! Does not own the memory of the actual names.
const std = @import("std");

const Self = @This();

pub const NameIndex = u16;

names: std.ArrayList([]const u8) = .{},

const Iterator = struct {
	parent: *const Self,
	pos: usize,

	pub fn next(self: *@This()) ?[]const u8 {
		if (self.pos == 0) return null;
		self.pos -= 1;
		return self.parent.names.items[self.pos];
	}

	pub fn nexti(self: *@This()) ?struct {NameIndex, []const u8} {
		const str = self.next();
		if (str) |s| return .{@intCast(self.pos), s};
		return null;
	}
};

pub fn iterator(self: *const @This()) Iterator {
	return .{.parent = self, .pos = self.names.items.len};
}

pub fn get(self: @This(), name: []const u8) ?NameIndex {
	var i = self.iterator();
	while (i.nexti()) |pair| {
		const index, const string = pair;
		if (std.mem.eql(u8, name, string)) return index;
	}
	return null;
}

/// Adds a name, even if it already exists.
pub fn add(self: *@This(), alloc: std.mem.Allocator, name: []const u8) !NameIndex {
	const ptr = try self.names.addOne(alloc);
	ptr.* = name;
	return @intCast(self.names.items.len - 1);
}

/// Adds a name, or does nothing if it already exists. Returns the index of the name.
pub fn indexName(self: *@This(), alloc: std.mem.Allocator, name: []const u8) !NameIndex {
	if (self.get(name)) |i| return i;
	return try self.add(alloc, name);
}

/// Removes the `n` most recently-added names.
pub fn remove(self: *@This(), alloc: std.mem.Allocator, n: usize) void {
	self.names.items.len -= @min(n, self.names.items.len);
	if (self.names.items.len * 2 + 8 < self.names.capacity) {
		// shrinkAndFree panics if the array length (not capacity) is not less than the new capacity
		// (this is bullshit, so manually resize the slice for the operation)
		const old_len = self.names.items.len;
		self.names.items.len = self.names.capacity;
		self.names.shrinkAndFree(alloc, self.names.capacity / 2);
		self.names.items.len = old_len;
	}
}

/// Denitializes the name list. Does not free the string keys.
pub fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
	self.names.deinit(alloc);
}