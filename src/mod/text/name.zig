//! Utilties for splitting and comparing names
const std = @import("std");
const Allocator = std.mem.Allocator;
const Charset = @import("charset.zig");
const Scanner = @import("scanner.zig");
const Name = @This();

/// The name's text. This is not managed by the name.
text: []const u8,
/// The segments, as slices of the original text.
segment_buf: [max_segment_count]Slice,
segment_count: u8,

pub const max_segment_count = 12;
pub const Error = error {TooManySegments};

const Slice = packed struct {
	start: u16,
	len: u16,

	pub fn get(self: Slice, text: []const u8) []const u8 {
		return text[self.start..(self.start + self.len)];
	}

	pub fn from(parent: []const u8, slice: []const u8) Slice {
		const offs = @intFromPtr(slice.ptr) - @intFromPtr(parent.ptr);
		return .{
			.start = @intCast(offs),
			.len = @intCast(slice.len)
		};
	}
};

pub fn from(text: []const u8) Error!Name {
	var res = Name {
		.text = text,
		.segment_buf = undefined,
		.segment_count = 0,
	};

	var iter = segmentsOf(text);
	while (iter.next()) |seg| {
		if (res.segment_count > max_segment_count) return Error.TooManySegments;
		res.segment_buf[res.segment_count] = seg;
		res.segment_count += 1;
	}

	return res;
}

/// Returns whether 2 names are equal.
pub fn eql(a: Name, b: Name) bool {
	if (a.segment_count != b.segment_count) return false;

	for (0..a.segment_count) |index| {
		const sa: []const u8 = a.segment_buf[index].get(a.text);
		const sb: []const u8 = b.segment_buf[index].get(b.text);

		if (sa.len != sb.len) return false;
		for (sa, sb) |char_a, char_b| {
			if (std.ascii.toLower(char_a) != std.ascii.toLower(char_b)) return false;
		}
	}
	return true;
}

pub fn hash(self: Name) u64 {
	var h = std.hash.Wyhash.init(self.segment_count);
	for (0..self.segment_count) |i| {
		const seg = self.segment_buf[i].get(self.text);
		for (seg) |char| {
			// the hashing function, I think, works the same if you feed it
			// single chars individually as if you fed it a whole slice
			h.update(&.{std.ascii.toLower(char)});
		}
	}
	return h.final();
}

const regular_name_start = Charset.set(.{Charset.range('a', 'z'), Charset.range('A', 'Z')});

const Iter = struct {
	const included_sep = Charset.set(.{Charset.range('A', 'Z'), Charset.range('0', '1')});
	const sep = Charset.set(.{'-', '_'});

	s: Scanner,

	/// Returns the next name slice. This slice refers to the same string as
	/// was originally provided, so case-insensitivity must come after
	/// operating on these returned slices. However, these slices are properly
	/// placed.
	pub fn next(self: *Iter) ?Slice {
		if (!self.s.valid()) return null;

		if (!regular_name_start.has(self.s.text[0])) {
			self.s.state.position = self.s.text.len;
			return Slice {.start = 0, .len = @intCast(self.s.text.len)};
		}

		if (sep.has(self.s.nextc())) self.s.advance(1);
		return self.nextSegment();
	}

	fn nextSegment(self: *Iter) Slice {
		// uppercase letters are allowed at the start of a segment
		const start = self.s.state.position;
		var found_lower = false;

		while (self.s.valid()) {
			const c = self.s.nextc();
			const is_upper = included_sep.has(c);

			if (sep.has(c) or found_lower and is_upper) break;
			if (!is_upper) found_lower = true;
			self.s.advance(1);
		}

		return Slice {
			.start = @intCast(start),
			.len = @intCast(self.s.state.position - start)
		};
	}
};

pub fn segmentsOf(text: []const u8) Iter {
	return .{.s = .{.text = text}};
}

/// A context for a hash map to use.
pub const HashContext = struct {
	pub fn hash(_: HashContext, n: Name) u64 {
		return n.hash();
	}

	pub fn eql(_: HashContext, a: Name, b: Name) bool {
		return a.eql(b);
	}
};

pub fn format(self: Name, w: *std.Io.Writer) !void {
	for (0..self.segment_count) |i| {
		if (i > 0) try w.writeByte('-');
		const seg = self.segment_buf[i].get(self.text);
		for (seg) |char| try w.writeByte(std.ascii.toLower(char));
	}
}

test "names" {
	const t = std.testing;

	const names: []const Name = &.{
		try .from("test_name_1"),
		try .from("test-name-1"),
		try .from("test-name1"),
		try .from("test-name_1"),
		try .from("TestName1"),
		try .from("testName_1"),
		try .from("TEST_NAME_1"),
	};

	const not_equal: []const Name = &.{
		try .from("testname1"),
		try .from("te_st_na_me_1"),
		try .from("TESTNAME1"),
	};

	for (names[0..names.len - 1], names[1..]) |name_a, name_b| {
		try t.expect(name_a.eql(name_b));
	}

	for (names) |name| {
		for (not_equal) |not_name| {
			try t.expect(!name.eql(not_name));
		}

		const expected = "test-name-1";
		const text = try std.fmt.allocPrint(t.allocator, "{f}", .{name});
		defer t.allocator.free(text);

		try t.expectEqualStrings(expected, text);
	}
}