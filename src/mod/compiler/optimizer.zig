const Instruction = @import("instruction.zig");
const cm = @import("compiler.zig");
const Func = @import("funcs/create_func.zig");

/// Does simple, obvious optimizations on bytecode.
pub fn optimize(opts: cm.CompileOptions, func: *Func) void {
	_ = opts;
	condensePopPass(func);
}

/// Replaces instances of add_local + pop with take_local
fn condensePopPass(func: *Func) void {
	var iter = func.items();
	while (iter.next()) |i| {
		switch (i) {
			.instruction => |p| {
				if (p.data != .add_local) continue;
			},
			else => continue,
		}

		const next_inst = switch (iter.instructionPeek(1) orelse continue) {
			.instruction => |p| p,
			else => continue,
		};

		if (next_inst.data == .pop) {
			_ = func.remove(iter.index);
			next_inst.data = .{.take_local = {}};
		}
	}
}