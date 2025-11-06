//! An interpreter environment. Contains globals used in the interpreter and
//! the current execution state.
const std = @import("std");
const Instruction = @import("../compiler/instruction.zig");

const Constants = @import("../cross/constants.zig");
const Variant = @import("../cross/variant.zig").Variant;
const Rc = @import("../cross/variant.zig").Rc;
const BufArrayList = @import("utils/buf_array_list.zig").BufArrayList;

pub const InterpreterError = error {
	StackEmpty,
	StackStackEmpty,
	InvalidInstruction,
	NotFunction,
	StackUnderflow,
	MatchFailed,
	Breakpoint,
	TypeError,
};

/// Stores the context for a stack frame.
pub const Frame = struct {
	id: ?Constants.ID,
	/// The code to execute.
	code: []const Instruction,
	/// The current position of the
	position: usize,
	/// The captures, if any, of the current frame.
	captures: ?Rc([]Variant),
	/// The locals of the frame. Every frame starts with zero locals, and can
	/// gain and lose them with match branches.
	locals: std.ArrayList(Variant),
	/// When the `pop_local_count` instruction is executed, the top of this
	/// array is popped and the number of locals is reduced to that number.
	/// This is also manipulated during match branches.
	/// This only increases past 1 in nested match branches
	/// (for example: `((x= [match nesting occurs here]): ) x: ...`)
	/// ```
	/// (
	///   (
	///     (
	///       x= -- nesting occurs here
	///     ): 1
	///     | 0
	///   ) x:
	///   x
	/// )
	/// ```
	local_counts: BufArrayList(u16, 2),

	n_stack_backups: u16,

	/// Frees memory and decrements Rc'd variants.
	pub fn deinit(self: *Frame, alloc: std.mem.Allocator) void {
		for (self.locals.items) |*v| {
			v.dec(alloc);
		}
		self.locals.deinit(alloc);
		self.local_counts.deinit(alloc);

		if (self.captures) |captures| {
			if (captures.dec(alloc)) |c| {
				for (c) |*v| v.dec(alloc);
				alloc.free(c);
			}
		}
	}

	fn pushLocalCount(self: *@This(), alloc: std.mem.Allocator) !void {
		const n: u16 = @intCast(self.locals.items.len);
		try self.local_counts.add(alloc, n);
	}

	/// Loads the local count from the top of `local_counts`. Does not pop the local count.
	/// Removed locals are properly freed.
	fn loadLocalCount(self: *@This(), alloc: std.mem.Allocator) void {
		const count = self.local_counts.last().*;
		if (count > self.locals.items.len) {
			std.debug.panic("Cannot load local count greater than current local count", .{});
		}

		for (self.locals.items[count..]) |*v| {
			v.dec(alloc);
		}
		self.locals.items.len = count;
	}
};

const Stack = std.ArrayList(Variant);

const StackBackup = struct {
	items: BufArrayList(Variant, 4),
	/// The stack length, including the stored items.
	len_without: usize,

	fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
		for (0..self.items.len) |i| self.items.get(i).dec(alloc);
		self.items.deinit(alloc);
	}
};

/// Creates a stack backup. Increments the stored variants.
fn createStackBackup(self: @This(), nitems: usize) !StackBackup {
	const stack = self.topStack();
	const slice = stack.items[stack.items.len - nitems..];
	for (slice) |*v| _ = v.inc();
	return .{
		.items = try .from(self.alloc, slice),
		.len_without = stack.items.len - nitems,
	};
}

/// Loads a stack backup. Decrements the stored variants since they were incremented for storage.
fn loadStackBackup(self: *@This(), backup: *StackBackup) !void {
	const stack = self.topStack();
	if (stack.items.len < backup.len_without) @panic("Cannot load stack state backup: not enough items");
	for (stack.items[backup.len_without..]) |*v_delete| v_delete.dec(self.alloc);
	stack.items.len = backup.len_without;

	for (try stack.addManyAsSlice(self.alloc, backup.items.len), 0..) |*new_val, i| {
		new_val.* = backup.items.get(i).*;
	}
}

/// The working allocator while executing.
alloc: std.mem.Allocator,
/// The constants for the environment.
constants: *const Constants,
/// The stack of temp stacks. The first element is the main stack.
/// Items from lower stacks than the top one should never be accessed!
stacks: std.ArrayList(Stack),
/// The call stack.
frames: std.ArrayList(Frame),
/// The stack backups created by match branches. When adding or removing from
/// this list, make sure to update the current frame's count of stack backups!
stack_backups: std.ArrayList(StackBackup) = .empty,

pub fn init(alloc: std.mem.Allocator, constants: *const Constants) !@This() {
	var res = @This() {
		.alloc = alloc,
		.constants = constants,
		.stacks = try .initCapacity(alloc, 2),
		.frames = undefined,
	};
	errdefer res.stacks.deinit(alloc);
	res.stacks.appendAssumeCapacity(.empty);
	res.frames = try .initCapacity(alloc, 8);
	return res;
}

pub fn deinit(self: *@This()) void {
	for (self.stacks.items) |*i| {
		for (i.items) |v| v.dec(self.alloc);
		i.deinit(self.alloc);
	}
	self.stacks.deinit(self.alloc);
	for (self.frames.items) |*f| f.deinit(self.alloc);
	self.frames.deinit(self.alloc);
	self.stack_backups.deinit(self.alloc);
}

/// Adds the variant onto the stack frame. If it refers to a builtin function,
/// no stack frame is created.
pub fn call(self: *@This(), variant: Variant) (InterpreterError || std.mem.Allocator.Error)!void {
	switch (variant) {
		.builtin => |f| try f(self),
		.function_instance, .function_ref => {
			const ptr = try self.frames.addOne(self.alloc);
			errdefer _ = self.frames.pop();
			ptr.* = try self.frameFrom(variant);
		},
		else => return error.NotFunction,
	}
}

/// Calls the function at id `id`.
pub fn callId(self: *@This(), id: Constants.ID) std.mem.Allocator.Error!void {
	const spot = try self.frames.addOne(self.alloc);
	errdefer _ = self.frames.pop();
	spot.* = try self.frameFrom(.{.function_ref = id});
}

/// Utility function. Returns the current stack frame.
pub inline fn topFrame(self: @This()) *Frame {
	return &self.frames.items[self.frames.items.len - 1];
}

/// Utility function. Returns the current temp (or main) stack.
pub inline fn topStack(self: @This()) *Stack {
	return &self.stacks.items[self.stacks.items.len - 1];
}

/// Gets the filename of the specified frame. May return null if the frame
/// does not originate from a function with a filename.
pub fn getFilename(self: @This(), frame: Frame) ?[]const u8 {
	if (frame.id) |id| {
		const f = self.constants.functions.items[id];
		if (f.filename.len > 0) return f.filename;
	}
	return null;
}

/// Creates a new stack frame based on the provided variant, which must be
/// either a function_ref or function_instance.
/// Panics if `func` is not a function type.
inline fn frameFrom(self: @This(), func: Variant) std.mem.Allocator.Error!Frame {
	switch (func) {
		.function_ref => |id| {
			return .{
				.id = id,
				.code = self.constants.functions.items[id].code,
				.captures = null,
				.locals = .empty,
				.local_counts = .empty,
				.n_stack_backups = 0,
				.position = 0,
			};
		},
		.function_instance => |inst| {
			return .{
				.id = inst.id,
				.code = self.constants.functions.items[inst.id].code,
				.captures = inst.captures.inc(),
				.locals = try .initCapacity(self.alloc, 2),
				.local_counts = .empty,
				.n_stack_backups = 0,
				.position = 0,
			};
		},
		else => @panic("Invalid variant type to create frame"),
	}
}

/// Does the proper cleanup after exiting frame `f`. Does not deinit the frame
/// itself.
/// If managing frames manually, this must be called when you make the
/// interpreter exit a frame!
pub fn exitFrame(self: *@This(), f: Frame) void {
	for (0..f.n_stack_backups) |i| {
		var bak = &self.stack_backups.items[self.stack_backups.items.len - 1 - i];
		bak.deinit(self.alloc);
	}
	self.stack_backups.items.len -= f.n_stack_backups;
}

/// While the top stack frame is past the last instruction, removes it from the
/// call stack. Returns whether there is a next instruction to execute.
pub fn checkExit(self: *@This()) bool {
	while (true) {
		if (self.frames.items.len == 0) return false;
		const frame = self.topFrame();
		if (frame.position < frame.code.len) break;
		self.exitFrame(frame.*);
		frame.deinit(self.alloc);
		_ = self.frames.pop();
	}
	return true;
}

/// Returns the next instruction to be executed.
pub fn nextInstruction(self: *@This()) ?*const Instruction {
	if (!self.checkExit()) return null;
	const frame = &self.frames.items[self.frames.items.len - 1];
	return &frame.code[frame.position];
}

/// Skips the next instruction. Assumes there is a next instruction.
pub fn skip(self: *@This()) void {
	self.topFrame().position += 1;
}

/// Tries to execute a single instruction. If it was executed successfully,
/// returns true. If there was no instruction to execute, returns false.
/// Otherwise, an error is returned. The interpreter should be in a recoverable
/// state as long as the returned error is not an OOM error.
pub inline fn step(self: *@This()) (InterpreterError || std.mem.Allocator.Error)!bool {
	if (!self.checkExit()) return false;
	_ = try self.stepAssumeNext();
	return true;
}

/// Does the same as `step`, but assumes there is a next instruction to execute.
/// Returns whether the call stack changed.
pub fn stepAssumeNext(self: *@This()) (InterpreterError || std.mem.Allocator.Error)!bool {
	const frame = self.topFrame();
	const current = frame.code[frame.position];
	switch (current.data) {
		.invalid => return InterpreterError.InvalidInstruction,
		.breakpoint => {
			return error.Breakpoint;
		},
		.push_float => |value| {
			try self.topStack().append(self.alloc, .{.num = value});
		},
		.push_sym => |id| {
			try self.topStack().append(self.alloc, .{.symbol = id});
		},
		.push_local => |index| {
			try self.topStack().append(self.alloc, frame.locals.items[index].inc());
		},
		.push_capture => |index| {
			try self.topStack().append(self.alloc, frame.captures.?.val[index].inc());
		},
		.push_global => |index| {
			try self.topStack().append(self.alloc, self.constants.get(index).inc());
		},
		.assign_captures => |nvalues| {
			const func_ref = self.topStack().pop() orelse return error.StackEmpty;
			errdefer func_ref.dec(self.alloc);

			const captures = self.topStack().items[self.topStack().items.len - nvalues..];
			self.topStack().items.len -= nvalues;

			const new: Variant = b: switch (func_ref) {
				.function_ref => {
					//for (captures) |*v| _ = v.inc();
					break :b try func_ref.withCaptures(self.alloc, captures);
				},
				else => return error.NotFunction,
			};
			self.topStack().appendAssumeCapacity(new);
		},
		.call, => {
			const function = self.topStack().pop() orelse return error.StackEmpty;
			defer function.dec(self.alloc);
			switch (function) {
				.function_ref, .function_instance => {
					frame.position += 1;
					const ptr = try self.frames.addOne(self.alloc); errdefer _ = self.frames.pop();
					ptr.* = try self.frameFrom(function);
					return true;
				},
				.builtin => |builtin| {
					try builtin(self);
				},
				else => return error.NotFunction,
			}
		},
		.tail_call => {
			const function = self.topStack().pop() orelse return error.StackEmpty;
			// When not returning an error, the memory in the variant is
			// handled in more specific ways and this is not needed.
			errdefer function.dec(self.alloc);

			switch (function) {
				.function_ref => |id| {
					const func = self.constants.functions.items[id];

					self.exitFrame(frame.*);

					if (frame.captures) |captures| {
						if (captures.dec(self.alloc)) |slice| {
							for (slice) |*v| v.dec(self.alloc);
							self.alloc.free(slice);
						}
					}

					for (frame.locals.items) |*v| v.dec(self.alloc);
					frame.local_counts.len = 0;
					frame.locals.items.len = 0;

					frame.* = .{
						.captures = null,
						.id = id,
						.code = func.code,
						.local_counts = frame.local_counts,
						.locals = frame.locals,
						.n_stack_backups = 0,
						.position = 0,
					};
				},
				.function_instance => |data| {
					const func = self.constants.functions.items[data.id];
					self.exitFrame(frame.*);

					// if the captures are the same, we can skip incrementing and decrementing
					if (data.captures != frame.captures) {
						_ = data.captures.inc();
						if (frame.captures) |captures| {
							if (captures.dec(self.alloc)) |slice| {
								for (slice) |*v| v.dec(self.alloc);
								self.alloc.free(slice);
							}
						}
					}

					for (frame.locals.items) |*v| v.dec(self.alloc);
					frame.local_counts.len = 0;
					frame.locals.items.len = 0;

					frame.* = .{
						.captures = data.captures,
						.id = data.id,
						.code = func.code,
						.local_counts = frame.local_counts,
						.locals = frame.locals,
						.n_stack_backups = 0,
						.position = 0,
					};
				},
				.builtin => |builtin| {
					self.exitFrame(frame.*);
					frame.deinit(self.alloc);
					self.frames.items.len -= 1;
					try builtin(self);
					return true;
				},
				else => return error.NotFunction,
			}
			return true;
		},
		.pop => |n| {
			const stack = self.topStack();
			if (stack.items.len < n) return error.StackEmpty;
			for ((stack.items.len - n) .. stack.items.len) |i| {
				stack.items[i].dec(self.alloc);
			}
			stack.items.len -= n;
		},
		.jump => |offs| {
			if (offs > 0) frame.position += @intCast(offs)
			else frame.position -= @intCast(-offs);
			return false;
		},
		// This instruction does a lot. See the full details in Instruction.Data.branch_check_begin.
		.branch_check_begin => |params| {
			const stack = self.topStack();
			if (stack.items.len < params.n_locals) {
				if (params.jump == 0) return InterpreterError.MatchFailed;
				frame.position += params.jump;
				// Return without incrementing the position any more
				return false;
			}
			// This instruction executes the imaginary `push_local_count`:
			try frame.pushLocalCount(self.alloc);
			errdefer _ = frame.local_counts.pop();

			// It also stores a backup of the pushed values
			const backup = try self.stack_backups.addOne(self.alloc);
			errdefer _ = self.stack_backups.pop();
			backup.* = try self.createStackBackup(params.n_locals);
			frame.n_stack_backups += 1;
		},
		.fail_check_if_false => |offs| {
			var truthy = false;
			if (self.topStack().pop()) |v| {
				truthy = v.truthy();
				v.dec(self.alloc);
			}

			if (!truthy) {
				if (offs == 0) return InterpreterError.MatchFailed;
				frame.position += offs;
				// Also retrieve the old local count
				frame.loadLocalCount(self.alloc);

				// Also load a backup of the stack from before the branch check
				frame.n_stack_backups -= 1;
				var backup = self.stack_backups.pop().?;
				errdefer backup.deinit(self.alloc);
				try self.loadStackBackup(&backup);
				return false;
			}
		},
		.pop_local_count => {
			frame.loadLocalCount(self.alloc);
			_ = frame.local_counts.pop();
		},
		.push_temp_stack => |ncopy| {
			// TODO: make sure that it's ok to not increase the variant's reference counts here
			if (self.topStack().items.len < ncopy) return error.StackEmpty;
			const new = try self.stacks.addOne(self.alloc);
			errdefer _ = self.stacks.pop();

			const old: *Stack = &self.stacks.items[self.stacks.items.len - 2];
			const slice: []Variant = old.items[old.items.len - ncopy..old.items.len];

			new.* = .fromOwnedSlice(try self.alloc.dupe(Variant, slice));
			for (new.items) |*v| _ = v.inc();
		},
		.pop_temp_stack => |ncopy| {
			var old = self.stacks.pop() orelse return error.StackStackEmpty;
			defer {
				for (old.items) |*v| v.dec(self.alloc);
				old.deinit(self.alloc);
			}

			if (old.items.len < ncopy) return error.StackEmpty;
			const slice = old.items[old.items.len - ncopy..old.items.len];
			old.items.len -= ncopy;

			try self.topStack().appendSlice(self.alloc, slice);
		},
		.add_local => {
			const stack = self.topStack();
			if (stack.items.len < 1) return error.StackEmpty;

			const item = try frame.locals.addOne(self.alloc);
			item.* = stack.items[stack.items.len - 1].inc();
		},
		.test_equal => {
			const stack = self.topStack();
			if (stack.items.len < 2) return error.StackEmpty;
			const a = stack.pop().?;
			const b = stack.pop().?;
			defer { a.dec(self.alloc); b.dec(self.alloc); }
			try stack.append(self.alloc, .fromPrimitive(a.eql(b)));
		},
		.push_own_func => {
			const ptr = try self.topStack().addOne(self.alloc);
			ptr.* =
				if (frame.captures) |c| Variant { .function_instance = .{
					.id = frame.id.?,
					.captures = c.inc(),
				}}
				else Variant {
					.function_ref = frame.id.?
				};
		},
	}

	frame.position += 1;
	return false;
}