const std = @import("std");
const flags = @import("flags");
const sl = @import("root.zig");
const dbg = @import("exe/debugger/debug.zig");

test {
	std.testing.refAllDecls(@This());
}

fn staticBuffer(comptime size: usize) []u8 {
	return &struct {
		var buf: [size]u8 = undefined;
	}.buf;
}

pub fn main() !void {
	var dbg_alloc = std.heap.DebugAllocator(.{}).init;
	defer _ = dbg_alloc.deinit();

	const alloc =
		if (comptime @import("builtin").mode != .ReleaseFast) dbg_alloc.allocator()
		else std.heap.smp_allocator;

	const cmd_args = try std.process.argsAlloc(alloc);
	defer std.process.argsFree(alloc, cmd_args);

	const args = flags.parse(
		cmd_args, "stack_lang",
		struct {
			pub const description =
				\\A stack-based language interpreter. Currently not yet finished!
				;
			pub const descriptions = .{
				.show = "Shows the info for a specified compilation step and then exits.",
				.debug = "Prints each instruction being executed, and provides debugging features",
				.nostd = "Don't include the standard library",
			};

			positional: struct {
				pub const descriptions = .{
					.file = "The file to parse and run."
				};

				file: ?[]const u8,
			},

			show: enum {
				pub const descriptions = .{
					.ast = "Prints the AST.",
					.ir = "Prints the IR.",
					.bytecode = "Prints out the instructions for each function.",
				};
				none, ast, ir, bytecode
			} = .none,

			nostd: bool = false,
			debug: bool = false,
			debug_on_fail: bool = false,
			skip_optimization: bool = false,

			pub const switches = .{
				.debug = 'd',
				.debug_on_fail = 'D',
			};
		},
		.{});

	const stderr_file = std.fs.File.stderr();
	var stderr = stderr_file.writer(staticBuffer(1024));
	defer stderr.interface.flush() catch {};

	var file_alloc = std.heap.ArenaAllocator.init(alloc); defer file_alloc.deinit();
	var files = sl.FileCache.init(file_alloc.allocator());

	const main_text =
		if (args.positional.file) |filename|
			files.open(filename) catch |err| {
				std.log.err(
					"Error: Could not open file '{s}': {s}\n",
					.{filename, @errorName(err)});
				return;
			}
		else "";

	var errors = sl.ErrorList.init(alloc);
	defer {
		errors.printTo(&stderr.interface, &files) catch {};
		errors.clear();
	}

	// Parse step
	errors.current_filename = args.positional.file;
	errors.current_step = "parser";
	var ast_alloc = std.heap.ArenaAllocator.init(alloc); defer ast_alloc.deinit();
	const ast = sl.parser.parseText(main_text, ast_alloc.allocator(), &errors)
		catch return; // errors are always logged on exit

	if (args.show == .ast) {
		for (ast) |ast_node| {
			try ast_node.print(&stderr.interface, 0, " ");
			_ = try stderr.interface.write("\n");
		}
		return;
	}

	// Initialize constant builder
	var compiler_alloc = std.heap.ArenaAllocator.init(alloc);
	defer compiler_alloc.deinit();

	var constants = sl.Constants.init(compiler_alloc.allocator());

	// IR generation
	errors.current_step = "analyzer";
	var ir_alloc = std.heap.ArenaAllocator.init(alloc); defer ir_alloc.deinit();

	const ir_nodes = try sl.ir.analyzeAll(ast, .{
		.alloc = ir_alloc.allocator(),
		.constants = &constants,
		.errs = &errors,
	});

	if (!args.skip_optimization) {
		const context = sl.ir.optimizer.Context {
			.alloc = ir_alloc.allocator(),
			.temp = ir_alloc.allocator(),
			.errors = &errors,
		};

		sl.ir.optimizer.findTailCalls(context, ir_nodes);
		try sl.ir.optimizer.findUnusedNames(context, ir_nodes, null);
	}

	if (args.show == .ir) {
		sl.ir.print_ir(ir_nodes);
		std.debug.print("\n", .{});
		return;
	}

	// Inject stdlib right before compilation
	if (!args.nostd) {
		try sl.stdlib.addLibrary(&constants, sl.stdlib.stdlib);
	}

	// Generate instructions
	errors.current_step = "compiler";

	var compiler_temp_alloc = std.heap.ArenaAllocator.init(alloc);
	defer compiler_temp_alloc.deinit();

	const main_func_constant = try sl.compiler.compile(ir_nodes, .{
		.constants = &constants,
		.errors = &errors,
		.filename = args.positional.file orelse "<no file>",
		.temp_alloc = compiler_temp_alloc.allocator(),
	});

	if (args.show == .bytecode) {
		std.debug.print("======= GLOBALS =======\n", .{});
		for (constants.globals.items) |v| {
			std.debug.print("{f}, ", .{v});
		}
		std.debug.print("\n\n====== FUNCTIONS ======\n", .{});
		for (constants.functions.items, 0..) |func, id| {
			std.debug.print(":: #{} / {s}\n", .{id, func.filename});
			for (func.code, 0..) |inst, idx| {
				std.debug.print(" \x1b[2m{:0>3}.\x1b[0m {f}\n", .{idx, inst});
			}
		}
		return;
	}

	// Print all errors accumulated over the compilation process
	try errors.printTo(&stderr.interface, &files);
	try stderr.interface.flush();
	errors.clear();
	errors.current_step = "runtime";

	// Run the program
	var env = try sl.Env.init(alloc, &constants);
	defer env.deinit();
	var dbg_stdin = std.fs.File.stdin().reader(staticBuffer(2048));

	// enter repl mode if there is no file
	if (args.positional.file == null) {
		try stderr.interface.print("\x1b[2;3mEntering REPL.\n\x1b[0m", .{});
		try dbg.repl(.{
			.constants = &constants,
			.env = &env,
			.files = &files,
			.in = &dbg_stdin.interface,
			.out = &stderr.interface,
		});
	}

	try env.call(constants.get(main_func_constant).*);

	if (args.debug) {
		try dbg.debug(.{
			.constants = &constants,
			.env = &env,
			.files = &files,
			.in = &dbg_stdin.interface,
			.out = &stderr.interface,
		});
	}

	execute: while (true) {
		if (env.nextInstruction() == null) break;
		const frame = env.topFrame();
		while (frame.position < frame.code.len) {
			const stack_changed = env.stepAssumeNext() catch |err| {
				errors.current_filename = env.getFilename(env.topFrame().*) orelse "<unknown>";
				const i_pos =
					if (frame.position < frame.code.len) frame.code[frame.position].position
					else @TypeOf(frame.code[0].position) {.line = 0, .col = 0, .position = 0};

				errors.pushError(.err, i_pos, "{s}", .{@errorName(err)});
				try errors.printTo(&stderr.interface, &files);
				try stderr.interface.flush();
				errors.clear();

				if (args.debug_on_fail) {
					try stderr.interface.print("\x1b[2;3mEntering debugger\x1b[0m\n", .{});
					try dbg.debug(.{
						.constants = &constants,
						.env = &env,
						.files = &files,
						.in = &dbg_stdin.interface,
						.out = &stderr.interface,
					});
				}

				break :execute;
			};
			if (stack_changed) break;
		}
	}

	if (env.stacks.items.len < 0) {
		errors.pushError(.err, .{.line = 0, .col = 0, .pos = 0}, "More than 1 stack on exit", .{});
	}

	//for (env.stacks.items[env.stacks.items.len - 1].items, 0..) |val, i| {
		//if (i > 0) std.debug.print(", ", .{});
		//std.debug.print("{f}", .{val.colorize()});
	//}
	//std.debug.print("\n", .{});
}