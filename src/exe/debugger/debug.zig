const std = @import("std");
const sl = @import("../../root.zig");

const StepExit = enum {
	normal,
	at_end,
	breakpoint,
	runtime_error,
};

/// Steps the context's interpreter. Assumes there is a next instruction to execute.
fn runStep(c: Context) !StepExit {
	const next_instruction = c.env.nextInstruction() orelse return .at_end;

	_ = c.env.stepAssumeNext() catch |err| switch (err) {
		error.Breakpoint => return .breakpoint,
		else => {
			var temp = sl.ErrorList.init(c.env.alloc);
			temp.current_step = "runtime";
			defer temp.clear();
			temp.pushError(.err, next_instruction.position, "{s}", .{@errorName(err)});
			try temp.printTo(c.out, c.files);
			return .runtime_error;
		}
	};

	return .normal;
}

const param = struct {
	const String = []const u8;
	const Int = isize;
	const RestOfLine = struct {[]const u8};
	fn Default(T: type, comptime val: T) type {
		return struct {
			pub const default = val;
			value: T,
		};
	}
};

/// Conatins the commands for the debugger.
const commands = struct {
	pub const descriptions = .{
		.next = "Runs the next instruction",
		.run = "Runs until an error/breakpoint or the end of the program is reached",
		.repl = "Compiles user code to a function, and pushes it onto the call stack",
		.rrepl = "Same as repl, but automatically runs the function",
		.instruction = "Shows the next instruction",
		.list = "Shows the list of instructions in the current function",
		.stack = "Shows the values on the stack, and temporary stack(s) if present",
		.locals = "Shows the list of currently declared locals in the current function",
		.declare = "Declares a global with the provided name. Its value is popped from the stack",
		.frames = "Shows the call stack",
		.@"return" = "Returns from the current function",
		.skip = "Skips the next instruction",
		.help = "Shows this help text",
		.quit = "Exits the program",
	};

	pub fn next(c: Context) !void {
		try instruction(c);
		switch (try runStep(c)) {
			.breakpoint => {
				c.env.skip();
			},
			else => {},
		}
	}

	pub fn run(c: Context) !void {
		while (true) {
			switch (try runStep(c)) {
				.normal => {},
				.at_end, .runtime_error => break,
				.breakpoint => {
					_ = try c.out.write("\x1b[31;1m * \x1b[0mbreakpoint\n");
					break;
				},
			}
		}
	}

	pub fn repl(c: Context, code_p: param.RestOfLine) !void {
		const code: []const u8 = code_p.@"0";

		var errs = sl.ErrorList.init(c.env.alloc); defer errs.clear();
		errs.current_filename = "(repl)";

		var arena = std.heap.ArenaAllocator.init(c.env.alloc); defer arena.deinit();

		const func_var_id = sl.compileFunction(c.constants, .{
			.code = code,
			.errs = &errs,
			.filename = errs.current_filename,
			.temp_alloc = arena.allocator(),
		}) catch |err| e: {
			try errs.ePushError(.err, .{.line = 0, .col = 0, .position = 0}, "fatal error: {s}", .{@errorName(err)});
			break :e undefined; // will always return early because error list is nonempty
		};

		try errs.printTo(c.out, c.files);

		// Don't run if there are any errors
		for (errs.errors.items) |e| {
			if (e.level == .err) {
				try c.out.print("\x1b[2;3mREPL execution canceled because of errors\x1b[0m\n", .{});
				return;
			}
		}

		try c.env.call(c.constants.globals.items[func_var_id]);
	}

	/// Similar to `repl`, but automatically runs the code.
	pub fn rrepl(c: Context, code_p: param.RestOfLine) !void {
		try @This().repl(c, code_p);
		const stack_size = c.env.frames.items.len;
		while (c.env.checkExit() and c.env.frames.items.len >= stack_size) {
			switch (try runStep(c)) {
				.normal => {},
				.at_end, .runtime_error => break,
				.breakpoint => {
					_ = try c.out.write("\x1b[31;1m * \x1b[0mbreakpoint\n");
					break;
				},
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
			try c.out.print("\x1b[0m ", .{});

			for (s.items, 0..) |v, j| {
				if (j > 0) try c.out.print(", ", .{});
				try c.out.print("{f}", .{v.colorize()});
			}

			try c.out.print("\n", .{});
		}
	}

	pub fn locals(c: Context) !void {
		if (c.env.frames.items.len == 0) {
			try c.out.print("\x1b[2;3mcall stack empty\x1b[0m\n", .{});
			return;
		}

		const cframe = c.env.frames.items[c.env.frames.items.len - 1];
		for (cframe.locals.items, 0..) |v, i| {
			try c.out.print("  \x1b[32;2m#\x1b[0;32m{:0>2}\x1b[0m: {f}\n", .{i, v.colorize()});
		}
	}

	pub fn declare(c: Context, name_text: param.RestOfLine) !void {
		const val = c.env.topStack().pop() orelse {
			try c.out.print("\x1b[2;3mtop stack is empty\x1b[0m\n", .{});
			return;
		};
		errdefer val.dec(c.env.alloc);
		const name = sl.text.Name.from(name_text.@"0") catch |err| {
			return c.out.print("\x1b[2;3minvalid name '{s}': {s}", .{name_text.@"0", @errorName(err)});
		};
		if (c.constants.getNamed(name) != null) {
			return c.out.print("\x1b[2;3mglobal {f} already exists\x1b[0m\n", .{name});
		}
		_ = try c.constants.addNamed(name, val);
	}

	pub fn frames(c: Context) !void {
		for (c.env.frames.items, 0..) |cframe, frame_index| {
			try c.out.print("Frame #{}\n", .{frame_index});

			for (cframe.locals.items, 0..) |v, i| {
				try c.out.print("  local \x1b[32;2m#\x1b[0;32m{:0>2}\x1b[0m: {f}\n", .{i, v.colorize()});
			}
			for (cframe.code, 0..) |inst, i| {
				if (i == cframe.position) try c.out.print("\x1b[32;1m", .{})
					else try c.out.print("\x1b[2m", .{});
				try c.out.print("  {:0>3}.\x1b[0m {f}\n", .{i, inst});
			}
		}
	}

	pub fn @"return"(c: Context) !void {
		var f = c.env.frames.pop() orelse return;
		c.env.exitFrame(f);
		f.deinit(c.env.alloc);
	}

	pub fn skip(c: Context, n: param.Default(usize, 1)) !void {
		if (c.env.frames.items.len == 0) {
			try c.out.print("\x1b[2;3mcall stack empty\x1b[0m\n", .{});
			return;
		}
		c.env.topFrame().position += n.value;
	}

	pub fn help(ctx: Context) !void {
		inline for (comptime std.meta.declarations(commands)) |decl| {
			switch (@typeInfo(@TypeOf(@field(@This(), decl.name)))) {
				.@"fn" => {},
				else => continue,
			}

			const description: []const u8 =
				if (@hasField(@TypeOf(descriptions), decl.name)) @field(descriptions, decl.name)
				else "<no description provided>";

			try ctx.out.print(" - \x1b[34m{s}\x1b[0m: {s}\n", .{decl.name, description});
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

	fn readline(self: Context) ![]u8 {
		// compat for 0.15.1 and 0.15.2
		// remove once 0.15.1 is not supported
		const take =
			if (comptime @import("builtin").zig_version.order(try .parse("0.15.2")) == .lt)
				std.Io.Reader.takeDelimiterInclusive
			else
				std.Io.Reader.takeDelimiter;

		const buf = try take(self.in, '\n');
		return sl.text.removeCr(buf[0..buf.len - 1]);
	}
};

/// Parses and runs a command. Returns the name of the command that was run.
fn parseAndRunCommand(ctx: Context, s: *sl.text.Scanner) !void {
	const command_name = s.eatIn(sl.parser.Token.chars_name);
	_ = s.eatIn(sl.parser.Token.chars_whitespace);

	inline for (comptime std.meta.declarations(commands)) |decl| {
		const func = @field(commands, decl.name);
		switch (@typeInfo(@TypeOf(func))) {
			.@"fn" => {},
			else => continue,
		}

		if (std.mem.startsWith(u8, decl.name, command_name)) {
			// show the name of the called command. Assumes the prompt was
			try ctx.out.print("\r\x1b[1A\x1b[{}C\x1b[0;2m: {s}\x1b[0m\n", .{s.text.len + 3, decl.name});

			const Args = std.meta.ArgsTuple(@TypeOf(func));
			var args: Args = undefined;
			args[0] = ctx;

			// Parse each argument. Error if not found.
			inline for (std.meta.fields(Args)[1..], 1..) |field, i| {
				args[i] = parseArg(s, field.type) catch |err| {
					try ctx.out.print("Formatting error or missing arg: arg {}: {s}", .{i, @errorName(err)});
					return;
				};
			}

			_ = s.eatIn(sl.parser.Token.chars_whitespace);
			if (s.valid()) {
				try ctx.out.print("\x1b[2;3munused arguments '{s}'\x1b[0m\n", .{s.text[s.state.position..]});
			}

			try @call(.auto, func, args);
			return;
		}
	} else {
		try ctx.out.print("Unknown command '{s}'\n", .{command_name});
	}
}

pub fn repl(ctx_in: Context) !void {
	var ctx = ctx_in;
	var quit = false;
	ctx.quit = &quit;

	while (!quit) {
		try ctx.out.print("> ", .{});
		try ctx.out.flush();
		const input = try ctx.readline();

		var s = sl.text.Scanner {.text = input};
		_ = s.eatIn(sl.parser.Token.chars_whitespace);

		if (s.nextIs("help")) {
			try ctx.out.print("To run these commands, start the line with `\\`.\n", .{});
			try commands.help(ctx);
			continue;
		}

		if (s.nextc() == '\\') {
			s.advance(1);
			try parseAndRunCommand(ctx, &s);
			continue;
		}

		try commands.rrepl(ctx, .{s.text});
		try commands.stack(ctx);
	}
}

/// User interface for debugging. Blocks until the debugging process is done.
pub fn debug(ctx_in: Context) !void {
	var ctx = ctx_in;
	var quit = false;
	ctx.quit = &quit;

	while (!quit) {
		try ctx.out.print("> ", .{});
		try ctx.out.flush();
		const input = try ctx.readline();

		var s = sl.text.Scanner {.text = input};
		try parseAndRunCommand(ctx, &s);
	}
}

fn parseArg(s: *sl.text.Scanner, Arg: type) !Arg {
	switch (@typeInfo(Arg)) {
		.int => {
			return try std.fmt.parseInt(Arg, s.eatIn(sl.text.Charset.range('0', '9')), 10);
		},
		.@"struct" => {
			if (Arg == param.RestOfLine) {
				const all_chars = sl.text.Charset.set(.{}).inv();
				return Arg {s.eatIn(all_chars)};
			}
			if (@hasDecl(Arg, "default")) {
				return Arg {.value = parseArg(s, @TypeOf(Arg.default)) catch Arg.default};
			}
		},
		else => {},
	}
	@compileError(std.fmt.comptimePrint("Unsupported param type: {}", .{Arg}));
}