//! Utility for reading a readonly string with escape codes applied.
const Scanner = @import("../text/scanner.zig");
const chars_whitespace = @import("../parser/token.zig").chars_whitespace;

scanner: Scanner,

pub const Error = error {UnknownEscapeCode};

pub fn init(text: []const u8) @This() {
	return .{
		.scanner = .{
			.text = text,
		},
	};
}

/// Returns the next char of the text, parsing escape codes.
/// if `Error.UnknownEscapeCode` is returned, `scanner`'s next char is the unknown character of the
/// escape code. after advancing `scanner`, `next` may be called again without errors.
pub fn next(self: *@This()) ?Error!u8 {
	// \ + whitespace is ignored
	while (true) {
		if (self.scanner.nextc() == '\\' and chars_whitespace.has(self.scanner.peekc(1))) {
			self.scanner.advance(1);
			_ = self.scanner.eatIn(chars_whitespace);
		} else break;
	}

	if (!self.scanner.valid()) return null;

	defer self.scanner.advance(1);
	switch (self.scanner.nextc()) {
		'\\' => {
			self.scanner.advance(1);
			return switch (self.scanner.nextc()) {
				'n' => '\n',
				'r' => '\r',
				't' => '\t',
				'e' => '\x1b',
				'\\', '"' => |c| c,
				else => error.UnknownEscapeCode,
			};
		},
		else => |c| return c,
	}
}