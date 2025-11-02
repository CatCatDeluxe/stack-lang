const std = @import("std");
const sl = @import("../../root.zig");

const StepExit = enum {
	normal,
	at_end,
	breakpoint,
};

/// Steps the context's interpreter. Assumes there is a next instruction to execute.
fn runStep(c: Context) !StepExit {
	const next_instruction = c.env.nextInstruction() orelse return .at_end;

	c.env.stepAssumeNext() catch |err| switch (err) {
		error.Breakpoint => return .breakpoint,
		else => {
			var temp = sl.ErrorList.init(c.env.alloc);
			defer temp.clear();
			temp.pushError(.err, next_instruction.position, "{s}", .{@errorName(err)});
			try temp.printTo(c.out, c.files);
		}
	};

	return .normal;
}

const param = struct {
	const String = []const u8;
	const RestOfLine = struct {[]const u8};
};

/// Conatins the commands for the debugger.
const commands = struct {
	pub fn next(c: Context) !void {
		try instruction(c);
		switch (try runStep(c)) {
			.breakpoint => {
				c.env.skip();
			},
			.at_end => {},
			.normal => {},
		}
	}

	pub fn repl(c: Context, code_p: param.RestOfLine) !void {
		const code: []const u8 = code_p.@"0";

		var errs = sl.ErrorList.init(c.env.alloc); defer errs.clear();
		errs.current_filename = "(repl)";

		var arena = std.heap.ArenaAllocator.init(c.env.alloc); defer arena.deinit();

		const func_id = try sl.compileFunction(c.constants, .{
			.code = code,
			.errs = &errs,
			.filename = errs.current_filename.?,
			.temp_alloc = arena.allocator(),
		});

		try errs.printTo(c.out, c.files);

		// Don't run if there are any errors
		for (errs.errors.items) |e| {
			if (e.level == .err) {
				try c.out.print("\x1b[2;3mREPL execution canceled because of errors\x1b[0m\n", .{});
				return;
			}
		}

		try c.env.callId(func_id);
	}

	pub fn run(c: Context) !void {
		while (true) {
			switch (try runStep(c)) {
				.normal => {},
				.at_end => break,
				.breakpoint => {
					_ = try c.out.write("\x1b[31;1m * \x1b[0mbreakpoint\n");
					break;
				}
			}
		}
	}

	pub fn instruction(c: Context) !void {
		if (c.env.nextInstruction() == null) {
			try c.out.print("\x1b[2;3mno next instruction\x1b[0m\n", .{});
			return;
		}
		const cframe = c.env.frames.items[c.env.frames.items.len - 1];
		try c.out.print("\x1b[2m{:0>3}.\x1b[0m {f}\n", .{cframe.position, cframe.code[cframe.position]});
	}

	pub fn list(c: Context) !void {
		if (c.env.frames.items.len == 0) {
			try c.out.print("\x1b[2;3mcall stack empty\x1b[0m\n", .{});
			return;
		}

		const cframe = c.env.frames.items[c.env.frames.items.len - 1];
		for (cframe.code, 0..) |inst, i| {
			if (i == cframe.position) try c.out.print("\x1b[32;1m", .{})
				else try c.out.print("\x1b[2m", .{});
			try c.out.print("{:0>3}.\x1b[0m {f}\n", .{i, inst});
		}
	}

	pub fn stack(c: Context) !void {
		for (c.env.stacks.items, 0..) |s, i| {
			try c.out.print("\x1b[2m", .{});
			if (i == 0) try c.out.print("(main)", .{})
				else try c.out.print("{: >5}.", .{i});
			try c.out.print(" \x1b[0m[", .{});

			for (s.items, 0..) |v, j| {
				if (j > 0) try c.out.print(", ", .{});
				try c.out.print("{f}", .{v.colorize()});
			}

			try c.out.print("]\n", .{});
		}
	}

	pub fn locals(c: Context) !void {
		if (c.env.frames.items.len == 0) {
			try c.out.print("\x1b[2;3mcall stack empty\x1b[0m\n", .{});
			return;
		}

		const cframe = c.env.frames.items[c.env.frames.items.len - 1];
		for (cframe.locals.items, 0..) |v, i| {
			try c.out.print("  \x1b[32;2m#\x1b[0;32m{:0>2}\x1b[0m: {f}\n", .{i, v});
		}
	}

	pub fn frames(c: Context) !void {
		for (c.env.frames.items, 0..) |cframe, frame_index| {
			try c.out.print("  Frame #{}\n", .{frame_index});
			for (cframe.code, 0..) |inst, i| {
				if (i == cframe.position) try c.out.print("\x1b[32;1m", .{})
					else try c.out.print("\x1b[2m", .{});
				try c.out.print("    {:0>3}.\x1b[0m {f}\n", .{i, inst});
			}
		}
	}

	pub fn help(ctx: Context) !void {
		inline for (comptime std.meta.declarations(commands)) |decl| {
			try ctx.out.print(" - {s} : {s}\n", .{decl.name, @typeName(@TypeOf(@field(commands, decl.name)))});
		}
	}

	pub fn quit(ctx: Context) !void {
		ctx.quit.* = true;
	}
};

pub const Context = struct {
	/// A mutable reference to the same constants used by `env`.
	constants: *sl.Constants,
	env: *sl.Env,
	files: *const sl.FileCache,
	out: *std.Io.Writer,
	in: *std.Io.Reader,
	/// Used internally. Don't worry about it.
	quit: *bool = undefined,
};

/// User interface for debugging. Blocks until the debugging process is done.
pub fn debug(ctx_in: Context) !void {
	var ctx = ctx_in;
	var quit = false;
	ctx.quit = &quit;

	while (!quit) {
		try ctx.out.print("> ", .{});
		try ctx.out.flush();

		var s = sl.text.Scanner {.text = try ctx.in.takeSentinel('\n')};
		const command_name = s.eatIn(sl.parser.Token.chars_name);
		_ = s.eatIn(sl.parser.Token.chars_whitespace);

		inline for (comptime std.meta.declarations(commands)) |decl| {
			// try ctx.out.print("{s}\n", .{decl.name});
			if (std.mem.startsWith(u8, decl.name, command_name)) {
				const func = @field(commands, decl.name);
				const Args = std.meta.ArgsTuple(@TypeOf(func));
				var args: Args = undefined;
				args.@"0" = ctx;

				// Parse each argument. Error if not found.
				inline for (std.meta.fields(Args)[1..], 1..) |field, i| {
					if (field.type == param.RestOfLine) {
						const all_chars = sl.text.Charset.set(.{}).inv();
						const rest = s.eatIn(all_chars);
						args[i] = .{rest};
					}
				}

				try @call(.auto, func, args);
				break;
			}
		} else {
			try ctx.out.print("Invalid command '{s}'\n", .{s.text});
		}
	}
}