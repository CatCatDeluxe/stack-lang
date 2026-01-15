//! Caches file names and contents while the interpreter is running.
const std = @import("std");
const text = @import("../text/text.zig");

alloc: std.mem.Allocator,
files: std.StringHashMap([]u8).Unmanaged,

pub fn init(alloc: std.mem.Allocator) @This() {
	return .{
		.alloc = alloc,
		.files = .empty,
	};
}

/// Opens the file at `path` and adds it to the FileCache.
/// Does some light preprocessing, like removing all unrecognized whitespace characters.
pub fn open(self: *@This(), path: []const u8) (std.fs.File.OpenError || std.fs.File.ReadError || std.mem.Allocator.Error)![]u8 {
	const kv = try self.files.getOrPut(self.alloc, path);
	if (kv.found_existing) return kv.value_ptr.*;

	errdefer _ = self.files.remove(path);
	kv.key_ptr.* = try self.alloc.dupe(u8, path);

	const file = try std.fs.cwd().openFile(path, .{.mode = .read_only});
	defer file.close();

	const buf = try self.alloc.alloc(u8, try file.getEndPos());
	errdefer self.alloc.free(buf);
	_ = try file.readAll(buf);
	_ = text.removeCr(buf);

	kv.value_ptr.* = buf;
	return buf;
}

pub fn get(self: *const @This(), name: []const u8) ?[]const u8 {
	return self.files.get(name);
}

pub fn deinit(self: *@This()) void {
	var iter = self.files.iterator();
	while (iter.next()) |val| {
		self.alloc.free(val.key_ptr.*);
		self.alloc.free(val.value_ptr.*);
	}
	self.files.deinit(self.alloc);
}