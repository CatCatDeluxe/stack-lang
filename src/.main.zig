//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.

const std = @import("std");
const Scanner = @import("text/scanner.zig");
const Token = @import("parser/token.zig");
const parser = @import("parser/parser.zig");
const Node = @import("parser/node.zig").Node;
const ObjWriter = @import("utils/o_writer.zig").ObjWriter;
const ErrList = @import("error_list.zig");
const ir = @import("ir/analyzer.zig");
const compiler = @import("compiler/compiler.zig");
const interpreter = @import("interpreter/interpreter.zig");

pub fn main() !void {
	var gpa = std.heap.DebugAllocator(.{}){};
	const alloc = gpa.allocator();
	defer _ = gpa.deinit();

	var _stderr = std.fs.File.stderr().writerStreaming(&struct {
		var stderr_buf: [1024]u8 = undefined; // static buffer
	}.stderr_buf);
	const stderr = &_stderr.interface;

	// this text should be kept through the whole compilation process
	const text: []const u8 = rd: {
		const f = try std.fs.cwd().openFile("test.sl", .{});
		defer f.close();
		break :rd try f.readToEndAlloc(alloc, std.math.maxInt(usize));
	};
	defer alloc.free(text);

	var errs = ErrList.init(alloc);
	defer errs.clear();

	////////// Parse //////////
	var scanner = Scanner{ .text = text };
	var arr = std.array_list.Managed(Node).init(alloc);
	defer {
		for (arr.items) |node| node.deinit(alloc);
		arr.deinit();
	}

	const node_writer = ObjWriter(Node).from_arr(&arr);
	errs.current_filename = "test.sl";
	_ = parser.parseBlock(&scanner, node_writer, alloc) catch |e| switch (e) {
		error.OutOfMemory => return e,
		else => {
			try errs.ePushError(.err, scanner.state, "Syntax error: {s}", .{@errorName(e)});
			try errs.printTo(stderr, text);
			try stderr.flush();
			return;
		},
	};

	for (arr.items) |node| {
		parser.print(node, 1, "\n");
		std.debug.print("\n", .{});
	}

	var ir_arr = std.array_list.Managed(ir.IRNode).init(alloc);
	const ir_writer = ObjWriter(ir.IRNode).from_arr(&ir_arr);

	defer {
		for (ir_arr.items) |n| n.deinit(alloc);
		ir_arr.deinit();
	}

	for (arr.items) |ast_node| {
		ir.analyze(ast_node, ir_writer, &errs, alloc) catch {};
	}

	try errs.printTo(stderr, text);
	try stderr.flush();
	errs.clear();

	ir.print_ir(ir_arr.items);
	std.debug.print("\n", .{});

	var env_arena_alloc = std.heap.ArenaAllocator.init(alloc);
	defer env_arena_alloc.deinit();

	const env_alloc = env_arena_alloc.allocator();
	var env = compiler.Env.init(env_alloc, env_alloc, &errs);
	var insts = std.array_list.Managed(compiler.Instruction).init(env_alloc);

	const inst_w = ObjWriter(compiler.Instruction).from_arr(&insts);
	for (ir_arr.items) |ir_node| {
		try compiler.compile(&env, ir_node, inst_w);
	}

	try errs.printTo(stderr, text);
	try stderr.flush();

	var ienv: interpreter.Env = undefined;
	ienv.alloc = alloc;
	{
		var funcs = std.ArrayList(interpreter.Env.Function).empty;
		var globals = std.ArrayList(compiler.Variant).empty;

		for (env.functions.items) |code_n| {
			const code = code_n orelse continue;
			funcs.append(alloc, .{
				.code = code,
				.filename = "",
			});
		}
	}

}

fn explore(env: *compiler.Env, alloc: std.mem.Allocator) !void {
	_ = alloc;

	const stdin_buf = &struct {var buf: [1024]u8 = undefined;}.buf;
	var _stdin = std.fs.File.stdin().reader(stdin_buf);
	const stdin = &_stdin.interface;

	const stdout_buf = &struct {var buf: [1024]u8 = undefined;}.buf;
	var _stdout = std.fs.File.stdin().writer(stdout_buf);
	const stdout = &_stdout.interface;

	while (true) {
		for (env.functions.items, 0..) |func_n, id| if (func_n) |_| {
			try stdout.print("Func \x1b[32m{}\x1b[0m\n", .{id});
		};

		const func_id: compiler.Env.ID = while (true) {
			_ = try stdout.write("View function #");
			try stdout.flush();

			const text = try stdin.takeDelimiterExclusive('\n');
			if (std.fmt.parseInt(compiler.Env.ID, text, 10)) |res| {
				if (res < env.functions.items.len)
					if (env.functions.items[res]) |_|
						break res;
			} else |_| {}

			_ = try stdout.write("\r\x1b[1A\x1b[2K\r");
			try stdout.flush();
			continue;
		};

		const func = env.functions.items[func_id].?;

		for (func) |inst| {
			try stdout.print("  - {f}\n", .{inst});
		}
		_ = try stdout.write("\n");
		try stdout.flush();
	}
}

test {
	std.testing.refAllDecls(@This());
}
