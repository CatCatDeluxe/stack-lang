const std = @import("std");

pub const text = @import("mod/text/text.zig");
pub const parser = @import("mod/parser/parser.zig");
pub const ir = @import("mod/ir/analyzer.zig");
pub const compiler = @import("mod/compiler/compiler.zig");

pub const Constants = @import("mod/cross/constants.zig");
pub const Env = @import("mod/interpreter/interpreter.zig");
pub const FileCache = @import("mod/cross/file_cache.zig");
pub const Variant = @import("mod/cross/variant.zig").Variant;
pub const ErrorList = @import("mod/error_list.zig");
pub const ObjWriter = @import("mod/utils/o_writer.zig").ObjWriter;

pub const Options = struct {
	/// The allocator to allocate temporary memory to. It is safe to reset it
	/// after the compiler function exits.
	temp_alloc: std.mem.Allocator,
	/// The error list to output to. Set to null to ignore non-fatal errors (not a good idea!)
	errs: ?*ErrorList,
	/// The code of the function to compile.
	code: []const u8,
	/// The filename to show in the
	filename: []const u8 = "<temp>",
};

/// Compiles a string of code into a function in `constants`.
/// This process can be customized a lot more by using
pub fn compileFunction(out: *Constants, opts: Options) (parser.SyntaxError || ir.AnalyzerError || compiler.Error || error {OutOfMemory})!Constants.ID {
	var dummy_errs = ErrorList.init(opts.temp_alloc);
	defer dummy_errs.clear();
	const errs = if (opts.errs) |e| e else &dummy_errs;

	errs.current_step = "parser";
	const ast_nodes = try parser.parseText(opts.code, opts.temp_alloc, errs);

	errs.current_step = "analyzer";
	const ir_nodes = try ir.analyzeAll(ast_nodes, .{
		.errs = errs,
		.alloc = opts.temp_alloc,
		.constants = out,
	});

	errs.current_step = "compiler";
	return try compiler.compile(ir_nodes, .{
		.constants = out,
		.errors = errs,
		.filename = opts.filename,
		.temp_alloc = opts.temp_alloc,
	});
}

test {
	std.testing.refAllDecls(@This());
}