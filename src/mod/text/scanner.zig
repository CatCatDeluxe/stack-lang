//! Utility for easily iterating through text.
const std = @import("std");
const Charset = @import("charset.zig");
const Location = @import("location.zig");

pub const State = struct {
	position: usize,
	line: u32,
	col: u16,
	/// The number of contiguous whitespace characters encountered before this character.
	pre_spacing: u16,
	/// The current indent level, only influenced by real tab characters.
	indent_level: u16,
	/// Whether the indentation should currently increase if encountering tab
	/// characters.
	increase_indent: bool
};

/// The text the parser reads.
text: []const u8,

/// The current position state of the parser. Do not rewind the parser by decreasing the position!
/// This can cause the line number to be off from what it actually is. Instead, copy the state object
/// and go forward. If you need to go backwards, set the state to the previously copied object.
state: State = .{.position = 0, .line = 1, .col = 1, .indent_level = 0, .increase_indent = true, .pre_spacing = 0},

pub fn valid(self: @This()) bool {
	return self.state.position < self.text.len;
}

pub fn advance(self: *@This(), n: usize) void {
	for (0..n) |_| {
		self.state.col += 1;
		switch (self.nextc()) {
			// ignore \r completely
			'\r' => continue,
			'\n' => {
				self.state.line += 1;
				self.state.col = 1;
				self.state.indent_level = 0;
				self.state.increase_indent = true;
				self.state.pre_spacing += 1;
			},
			'\t' => {
				if (self.state.increase_indent) self.state.indent_level += 1;
				self.state.pre_spacing += 1;
			},
			' ' => {
				self.state.increase_indent = false;
				self.state.pre_spacing += 1;
			},
			else => {
				self.state.increase_indent = false;
			}
		}
		self.state.position += 1;
	}
}

pub fn nextc(self: @This()) u8 {
	return if (self.valid()) self.text[self.state.position] else 0;
}

pub fn next(self: @This(), n: usize) []const u8 {
	return self.text[self.state.position..@min(self.state.position + n, self.text.len)];
}

/// Gets a character around the current position. `nextc()` is equivalent to `peekc(0)`.
/// Returns 0 if the index is out of bounds.
pub fn peekc(self: @This(), offs: isize) u8 {
	const idx: isize = @as(isize, @intCast(self.state.position)) + offs;
	if (idx < 0 or idx >= self.text.len) return 0;
	return self.text[@intCast(idx)];
}

/// Advances `n` characters and returns the slice of characters advanced.
/// Returns null if the parser could not eat that many characters,
pub fn eat(self: *@This(), n: usize) []const u8 {
	if (self.state.position + n > self.text.len) return self.text[0..0];
	const start = self.state.position;
	self.advance(n);
	return self.text[start..self.state.position];
}

/// Eats one character and returns it. Returns 0 if the parser is past the end
/// of the string.
pub fn eatc(self: *@This()) u8 {
	if (!self.valid()) return 0;
	self.advance(1);
	return self.text[self.state.position - 1];
}

/// Eats until the next character is not in `set`. Returns the slice.
pub fn eatIn(self: *@This(), set: Charset) []const u8 {
	const start = self.state.position;
	while (self.valid() and set.has(self.nextc())) self.advance(1);
	return self.text[start..self.state.position];
}

/// Returns whether the next part of the string is the same as `expect`. If the
/// remaining text is too short, returns false.
pub fn nextIs(self: @This(), expect: []const u8) bool {
	if (self.state.position + expect.len > self.text.len) return false;
	for (0..expect.len) |i| {
		if (self.text[self.state.position + i] != expect[i]) {
			return false;
		}
	}
	return true;
}

/// Returns the current parser location as a `Location`.
pub fn location(self: @This()) Location {
	return .{.line = self.state.line, .col = self.state.col};
}

//pub fn ok(self: @This(), err: Error) error {UnhandledParseError}!void {
	//if (self.err == .none or self.err == err) return;
	//return error.UnhandledParseError;
//}

// Returns `true` if the next string is equivalent to `text`, and that the character after is not
// in a-z, A-Z, 0-9, and _. Otherwise returns `false`.
pub fn nextWordIs(self: @This(), text: []const u8) bool {
	const nameChars = Charset.set(.{
		Charset.range('a', 'z'),
		Charset.range('A', 'Z'),
		Charset.range('0', '9'),
		'_'
	});

	if (!self.nextIs(text)) return false;
	if (nameChars.has(self.peekc(text.len))) return false;
	return true;
}
