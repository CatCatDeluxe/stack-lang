const std = @import("std");
const FileCache = @import("cross/file_cache.zig");

pub const ErrorLevel = enum { info, warning, err };

const Position = struct {
	pos: usize,
	line: u32,
	col: u32,

	pub fn any(from: anytype) Position {
		const From = @TypeOf(from);
		return .{
			.pos = @intCast(if (@hasField(From, "position")) from.position else from.pos),
			.line = @intCast(from.line),
			.col = @intCast(from.col),
		};
	}
};

const Error = struct {
	/// What kind of error it is
	level: ErrorLevel,
	/// Which compilation/interpreter step the error took place in
	step: []const u8,
	filename: ?[]u8,
	text: []u8,
	/// The character position from the start of the file.
	pos: usize,
	/// The line the error occurred on. Starts at 0.
	line: u32,
	/// The column the error occurred on. Starts at 0.
	col: u32,

	fn printPos(self: Error, writer: anytype) !void {
		try writer.print("\x1b[0;2mat \x1b[0;34m{s}:{}:{}", .{ self.filename orelse "(unknown)", self.line, self.col });
	}

	pub fn format(
		self: Error,
		writer: *std.Io.Writer,
	) !void {
		const levelName =
			switch (self.level) {
				.info => "\x1b[35;1minfo",
				.warning => "\x1b[33;1mwarn",
				.err => "\x1b[31;1merror",
			};

		try writer.print("({s} {s} ", .{self.step, levelName});
		try self.printPos(writer);
		try writer.print("\x1b[0m) {s}", .{self.text});
	}
};

alloc: std.mem.Allocator,
errors: std.ArrayList(Error),
/// The currently compiling file. The string does not need to stay alive after
/// this name is changed.
current_filename: []const u8,
/// Stores the current execution step, to print in errors. The string should
/// last as long as the error list object.
current_step: []const u8 = "",
/// The current stack of positions.
positions: std.ArrayList(Position),

pub fn init(alloc: std.mem.Allocator) @This() {
	return .{
		.alloc = alloc,
		.errors = .empty,
		.positions = .empty,
		.current_filename = "",
	};
}

/// Saves a new position to the stack.
pub fn pushPos(self: *@This(), pos: anytype) void {
	self.positions.append(self.alloc, .any(pos)) catch {};
}

/// Gets the last saved position.
pub fn getPos(self: @This()) Position {
	return self.positions.getLastOrNull() orelse .{.pos = 0, .line = 0, .col = 0};
}

/// Overwrites the last saved position.
pub fn setPos(self: @This(), pos: anytype) void {
	if (self.positions.items.len == 0) return;
	self.positions.items[self.positions.items.len - 1] = .any(pos);
}

/// Pops the last saved position.
pub fn popPos(self: *@This()) Position {
	return self.positions.pop() orelse .{.pos = 0, .line = 0, .col = 0};
}

/// Tries to push an error to the error list. If an operation fails, this function
/// returns silently.
/// Param `position` must have `line`, `col`, and `pos` fields that are all
/// integers, and represent the line, column, and character position in their
/// containing file. If you cannot obtain these values, set them all to 0.
pub fn pushError(self: *@This(), level: ErrorLevel, position: anytype, comptime format: []const u8, format_args: anytype) void {
	self.ePushError(level, position, format, format_args) catch {};
}

/// Tries to push an error to the error list.
/// Param `position` must have `line`, `col`, and `pos` fields that are all
/// integers, and represent the line, column, and character position in their
/// containing file. If you cannot obtain these values, set them all to 0.
pub fn ePushError(self: *@This(), level: ErrorLevel, position: anytype, comptime format: []const u8, format_args: anytype) error{OutOfMemory}!void {
	var buf = try std.ArrayList(u8).initCapacity(self.alloc, 16);
	errdefer buf.deinit(self.alloc);

	try buf.print(self.alloc, format, format_args);

	const filename = try self.alloc.dupe(u8, self.current_filename);
	errdefer self.alloc.free(filename);

	const pos = Position.any(position);

	try self.errors.append(self.alloc, .{
		.level = level,
		.step = self.current_step,
		.text = try buf.toOwnedSlice(self.alloc),
		.line = pos.line,
		.col = pos.col,
		.pos = pos.pos,
		.filename = filename,
	});
}

/// Clears the error list. Leaves it in a usable state.
pub fn clear(self: *@This()) void {
	for (self.errors.items) |err| {
		self.alloc.free(err.text);
		if (err.filename) |f| self.alloc.free(f);
	}
	self.errors.clearAndFree(self.alloc);
	self.positions.clearAndFree(self.alloc);
}

/// Prints an error list.
/// TODO: make the error list able to select files based on filename.
pub fn printTo(self: @This(), writer: *std.Io.Writer, files: ?*const FileCache) std.Io.Writer.Error!void {
	const Scanner = @import("text/scanner.zig");

	for (self.errors.items) |err| {
		try writer.print("{f}\n", .{err});
		const file_text: ?[]const u8 =
			if (err.filename) |filename|
				(if (files) |f| f.get(filename) else null)
			else null;

		var temp_s = Scanner {.text = file_text orelse ""};
		while (temp_s.valid()) {
			if (temp_s.state.line == err.line) {
				const start = temp_s.state.position;
				while (temp_s.nextc() != '\n' and temp_s.valid()) temp_s.advance(1);
				const line = temp_s.text[start..temp_s.state.position];

				try writer.print("\x1b[2m{:>4}. \x1b[0;32m", .{ err.line });

				var pointer_pos: usize = 0;
				for (line, 0..) |byte, i| {
					switch (byte) {
						'\t' => _ = try writer.write("  "),
						else => try writer.writeByte(byte),
					}
					if (i < err.col) pointer_pos +=
						switch (byte) { '\t' => 2, else => 1 };
				}

				try writer.writeByte('\n');

				for (0..pointer_pos + 5) |_| try writer.writeByte(' ');
				try writer.print("\x1b[34;1m^\x1b[0m (here)\n", .{});

				break;
			}
			temp_s.advance(1);
		}
	}
}