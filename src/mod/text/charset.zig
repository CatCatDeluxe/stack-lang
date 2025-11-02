//! Stores a bit for each possible u8.
const std = @import("std");
const one: u256 = 1;

mask: u256,

pub fn set(values: anytype) @This() {
	var res = @This() {.mask = 0};
	inline for (values) |val| {
		switch (@typeInfo(@TypeOf(val))) {
			.int, .comptime_int => {
				res.mask |= one << @intCast(val);
			},
			else => {
				res.mask |= val.mask;
			}
		}
	}
	return res;
}

pub fn range(low: u8, high: u8) @This() {
	var res = @This() {.mask = 0};
	for (low..high + 1) |val| {
		res.mask |= one << @intCast(val);
	}
	return res;
}

pub fn has(self: @This(), val: u8) bool {
	return (self.mask & (one << val)) != 0;
}

pub fn add(self: @This(), other: @This()) @This() {
	return @This() {.mask = self.mask | other.mask};
}

pub fn add1(self: @This(), other: u8) @This() {
	return @This() {.mask = self.mask | (one << other)};
}

pub fn inv(self: @This()) @This() {
	return @This() {.mask = ~self.mask};
}

pub fn format(self: @This(), writer: std.io.AnyWriter) !void {
	try writer.writeAll("Charset {");
	for (0..255) |i| {
		const c: u8 = @truncate(i);
		if (self.mask & (one << c) == 0) continue;
		if (c >= ' ') {
			try writer.print("'{c}' ", .{c});
		} else {
			try writer.print("'\\{}' ", .{c});
		}
	}
	try writer.writeAll("}");
}

pub fn pickSet(charsets: []const @This(), char: u8) ?*const @This() {
	for (charsets) |*cset| {
		if (cset.has(char)) return cset;
	}
	return null;
}
