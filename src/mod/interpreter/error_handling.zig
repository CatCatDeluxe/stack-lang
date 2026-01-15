const std = @import("std");
const Env = @import("interpreter.zig");
const ErrorList = @import("../error_list.zig");

/// Pushes an error message to `e` at the interpreter's current position, then logs all other
/// stack frames as well.
pub fn logStackTrace(env: Env, e: *ErrorList, comptime errmsg: []const u8, errformat: anytype) void {
	var i = env.frames.items.len;
	while (i > 0) {
		i -= 1;
		const frame = &env.frames.items[i];
		if (frame.position > frame.code.len) continue;
		// when calling, the position is incremented so on return it is at the right place. The
		// calling instruction is always 1 before the current position.
		const instruction = &frame.code[@max(frame.position, 1) - 1];

		if (i == env.frames.items.len - 1) {
			e.pushError(.err, instruction.position, errmsg, errformat);
		} else {
			e.pushError(.info, instruction.position, "(called here)", .{});
		}
	}
}