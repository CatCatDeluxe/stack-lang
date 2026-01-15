pub const Scanner = @import("scanner.zig");
pub const Charset = @import("charset.zig");
pub const Location = @import("location.zig");
pub const Name = @import("name.zig");

/// Removes all `\r `characters from `text` and shifts text back. Fills the leftover space at the end with `\n`.
/// Returns the slice without the trailing bytes.
pub fn removeCr(text: []u8) []u8 {
	var read: usize = 0;
	var write: usize = 0;
	while (read < text.len) {
		text[write] = text[read];
		if (text[read] != '\r') write += 1;
		read += 1;
	}
	@memset(text[write..read], '\n');
	return text[0..write];
}

test { @import("std").testing.refAllDecls(@This()); }
