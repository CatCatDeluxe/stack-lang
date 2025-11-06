const std = @import("std");
const Instruction = @This();
const Symbols = @import("../cross/symbols.zig");
pub const debug_mode = true;

/// Creates an enum from a union type.
fn EnumFromUnion(U: type) type {
	const EnumField = std.builtin.Type.EnumField;

	var res = std.builtin.Type.Enum {
		.decls = &.{},
		.fields = &.{},
		.is_exhaustive = true,
		.tag_type = u0,
	};

	for (std.meta.fields(U), 0..) |field, i| {
		res.fields = res.fields ++ &[_]EnumField {.{.name = field.name, .value = i}};
	}

	res.tag_type = std.meta.Int(.unsigned, std.math.log2_int_ceil(usize, res.fields.len));

	return @Type(.{.@"enum" = res});
}

pub const Data = union(enum) {
	/// An invalid instruction. The interpreter will error if it encounters this.
	invalid,
	push_float: f64,
	push_sym: Symbols.SymbolID,
	push_local: u16,
	push_capture: u16,
	push_global: u32,
	/// Pushes the currently running function onto the stack.
	push_own_func,
	/// Assumes that the value on top of the stack is a function with unassigned
	/// captures. pops that function, and then pops the top `n` values off the
	/// stack and assigns them to the function's captures. This is done in
	/// reverse order, so the top of the stack is assigned to the function's
	/// last capture. The newly-assigned function is then pushed back to the
	/// top of the stack.
	assign_captures: u16,
	/// Calls the function on the top of the stack.
	call,
	/// Calls the function on the top of the stack, reusing the current stack
	/// frame.
	tail_call,
	/// Removes the top `n` values from the top of the stack.
	pop: u8,

	/// Jumps relatively `n` instructions.
	jump: i16,

	/// Sets the branch check list to the top `n` values from the stack.
	/// Also pushes a local count, which should later be popped with `pop_local_count`
	///
	/// Keeps a copy of the top `n` values of the stack. If the branch fails,
	/// the stack is reset to its previous state. This operation assumes that
	/// the only change to the stack while a branch check is taking place is
	/// popping values (which is true if bytecode is properly generated).
	///
	/// If called again, the branch-reset storage is replaced. However,
	/// `push_temp_stack` also pushes a new branch-reset storage.
	///
	/// This instruction does a lot, which is probably fine because what it
	/// does is only ever needed in its single use case (starting a match block)
	branch_check_begin: packed struct {jump: u16, n_locals: u8},

	/// Fails the check if the popped value is falsy, by jumping forward `n`
	/// instructions to the end of the branch.
	/// Both this instruction and `fail_check`, if jumping into a `pop_stack_count`
	/// instruction, will execute that instruction before restoring the stack backup.
	/// TODO: remove this feature, and replace all fail_check_if_false's with a jump
	/// and a `fail_check` instruction.
	fail_check_if_false: u16,

	/// Removes locals until there is the same amount as when the previous
	/// `push_local_count`/`branch_check_begin` was run.
	pop_local_count,

	/// Adds a temporary stack. Copies the top `n` values of the current stack over.
	push_temp_stack: u8,
	/// Removes the top temporary stack. Copies the top `n` values over to the
	/// new top stack.
	pop_temp_stack: u8,

	/// Pushes 1 if the current stack has at least `n` items, and pushes 0 otherwise.
	check_stack_length: u8,

	/// Adds a local to the top of the local stack.
	add_local,

	/// Pops the top value of the stack and adds it as a new local.
	/// The same as `add_local` + `pop`, but a simpler operation on its own.
	take_local,

	/// Pops the top two values. Pushes true if they are equal, and false otherwise.
	test_equal,

	/// Pushes a temp stack count.
	push_stack_count,
	/// Pops a temp stack count.
	pop_stack_count,
	fail_check: u16,
	jump_if: u16,
	jump_unless: u16,

	/// Debugging instruction
	breakpoint,
};

pub const Type: type = EnumFromUnion(Data);

const type_per_type: []const type = blk: {
	var res: []const type = &.{};
	for (std.meta.fieldNames(Type)) |enum_name| {
		const T = @FieldType(Data, enum_name);
		res = res ++ &[_]type {T};
	}
	break :blk res;
};

const size_per_type: []const u8 = blk: {
	var res: []const usize = &.{};
	for (type_per_type) |tp| {
		res = res ++ &[_]usize {@sizeOf(tp)};
	}
	break :blk res;
};

position: if (debug_mode) struct {position: u64, line: u32, col: u32} else void,
data: Data,

/// Returns whether two instructions are equal. Ignores the instruction's position data.
pub fn eql(a: Instruction, b: Instruction) bool {
	// if (std.meta.activeTag(a.data) != std.meta.activeTag(b.data)) return false;
	return std.meta.eql(a.data, b.data);
}

/// Returns the hash of the instruction. Does not take into account the position data.
pub fn hash(self: Instruction) u64 {
	var h = std.hash.Wyhash.init(@intFromEnum(std.meta.activeTag(self.data)));
	switch (self.data) {
		.push_float => |f| {
			// autoHash does not allow floats. Just do bitwise comparisons.
			const ptr: *const [8]u8 = @ptrCast(&f);
			h.update(ptr);
		},
		inline else => |payload| {
			std.hash.autoHash(&h, payload);
		}
	}
	return h.final();
}

/// Returns the size of an instruction of type `tp`.
pub fn sizeOfType(tp: Type) usize {
	const index: usize = @intFromEnum(tp);
	return @sizeOf(Type) + size_per_type[index];
}

/// Returns the storage type of an instruction of type `tp`.
pub fn DataOfType(tp: Type) type {
	const index: usize = @intFromEnum(tp);
	return type_per_type[index];
}

pub fn init(comptime i_type: Type, pos: anytype, data: DataOfType(i_type)) Instruction {
	var res: Instruction = undefined;
	res.data = @unionInit(@TypeOf(res.data), @tagName(i_type), data);
	if (debug_mode) res.position = .{.line = @intCast(pos.line), .col = @intCast(pos.col), .position = @intCast(pos.position)};
	return res;
}

const Color = enum {
	red, yellow, green, blue, cyan, purple
};

fn printCol(w: anytype, col: Color, comptime fmt_s: []const u8, args: anytype) !void {
	const color_code = switch (col) {
		.red => "31",
		.green => "32",
		.yellow => "33",
		.blue => "34",
		.purple => "35",
		.cyan => "36",
	};
	try w.print("\x1b[{s}m", .{color_code});
	try w.print(fmt_s, args);
	try w.print("\x1b[0m", .{});
}

pub fn format(
	self: Instruction,
	writer: *std.io.Writer,
) !void {
	try writer.print("\x1b[1m{s}\x1b[0m", .{@tagName(std.meta.activeTag(self.data))});
	switch (self.data) {
		.push_float => |f|
			try printCol(writer, .yellow, " {d}", .{f}),

		inline .push_global, .push_local, .push_capture, .push_sym => |id|
			try printCol(writer, .green, " \x1b[2m#\x1b[0;32m{}", .{id}),

		.pop, .push_temp_stack, .pop_temp_stack, .assign_captures, .check_stack_length => |n|
			try printCol(writer, .cyan, " {} \x1b[2mvalues", .{n}),

		.branch_check_begin => |params|
			try printCol(writer, .purple, " jump {}, locals {}", params),

		inline .fail_check_if_false, .jump => |jumplen|
			try printCol(writer, .purple, " -> {}", .{jumplen}),

		//.add_locals, .remove_locals => |n|
			//try printCol(writer, .cyan, "{}", .{n}),

		else => {}
	}
	if (debug_mode) {
		try writer.print(" \x1b[2m[line {}:{}]\x1b[0m", .{self.position.col, self.position.line});
	}
}
