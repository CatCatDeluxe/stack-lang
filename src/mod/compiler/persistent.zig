const std = @import("std");
const Instruction = @import("instruction.zig");
const Variant = @import("variant.zig");

pub const ID = u32;

pub const Function = struct {
	code: []Instruction,
	filename: ?[]u8,
};

/// The output of a persistent data being baked after compilation.
pub const Baked = struct {
	alloc: std.mem.Allocator,
	functions: []Function,
	globals: []Variant,
};

/// The allocator used for this persistent data.
alloc: std.mem.Allocator,

/// The list of functions.
functions: std.ArrayList(?Function),

/// The list of globals.
globals: std.ArrayList(Variant),

/// Because `functions` may change, this keeps a mapping of input IDs to output
/// IDs. This allows for optimizations like merging same functions.
/// The order can be reset using rebake().
function_ids: std.AutoHashMap(ID, ID),

pub fn addFunction(self: *@This(), func: Function) !*Function {
	for (self.functions.items) |*f| {
		if (f.* == null) {
			f.* = func;
			return &f.?;
		}
	}
	const res = try self.functions.addOne(self.alloc);
	res.* = func;
	return &res.?;
}

pub fn getFunction(self: @This(), id: ID) ?*Function {
	return &self.functions[self.function_ids.get(id) orelse return null].?;
}

pub fn removeFunction(self: *@This(), id: ID) ?Function {
	const true_id = self.function_ids.get(id) orelse return null;
	const res = self.functions[true_id];
	self.functions[true_id] = null;
	return res;
}

pub fn functionIDs(self: @This()) std.AutoHashMap(ID, ID).KeyIterator {
	return self.function_ids.keyIterator();
}

pub fn bake() Baked {

}