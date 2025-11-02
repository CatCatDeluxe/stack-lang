const Instruction = @import("instruction.zig");
const cm = @import("compiler.zig");

pub fn optimize(env: *cm.Env, code: []Instruction) []Instruction {
	_ = env;
	return code;
}