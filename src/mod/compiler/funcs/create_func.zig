//! Contains a pre-compiled function body.
const std = @import("std");
const Allocator = std.mem.Allocator;
const Instruction = @import("../instruction.zig");
const Self = @This();

alloc: std.mem.Allocator,
code: std.ArrayList(CodeItem),
label: packed struct {
	/// The last used label ID.
	last_id: u24 = 0,
	/// Sub-functions will have a func_id 1 higher than their parent, to
	/// ensure the generation of unique labels.
	func_id: u8 = 0,
},

pub const Label = u32;

const Jump = struct {
	base: Instruction,
	to: Label,

	fn resolve(self: Jump, offs: isize) Instruction {
		var res = self.base;
		switch (self.base.data) {
			.jump => res.data.jump = @intCast(offs),
			.jump_if => res.data.jump_if = @intCast(offs),
			.jump_unless => res.data.jump_unless = @intCast(offs),
			.fail_check => res.data.fail_check = @intCast(offs),
			.fail_check_if_false => res.data.fail_check_if_false = @intCast(offs),
			.branch_check_begin => res.data.branch_check_begin.jump = @intCast(offs),
			else => std.debug.panic("Instruction is not a jump: {s}", .{@tagName(self.base.data)}),
		}
		return res;
	}
};

const CodeItem = union(enum) {
	jump: Jump,
	label: Label,
	instruction: Instruction,

	fn from(i: Instruction) CodeItem {
		return .{.instruction = i};
	}
};

pub fn init(alloc: Allocator) @This() {
	return .{
		.alloc = alloc,
		.code = .empty,
		.label = .{},
	};
}

pub fn append(self: *@This(), i: Instruction) Allocator.Error!void {
	try self.code.append(self.alloc, .from(i));
}

pub fn appendJump(self: *@This(), label_id: Label, base: Instruction) Allocator.Error!void {
	try self.code.append(self.alloc, .{.jump = .{.base = base, .to = label_id}});
}

/// Creates a sub-body for the function.
pub fn sub(self: @This()) @This() {
	return .{
		.alloc = self.alloc,
		.code = .empty,
		.label = .{
			.last_id = 0,
			.func_id = self.label.func_id + 1,
		},
	};
}

/// Appends the code of `sub` to `this`. `sub` must be a sub-body of `this`.
/// Does not deinit `sub`.
pub fn appendSub(self: *@This(), sub_body: @This()) Allocator.Error!void {
	std.debug.assert(self.label.func_id + 1 == sub_body.label.func_id);
	try self.code.appendSlice(self.alloc, sub_body.code.items);
}

/// Inserts the provided instructions before index `before`. Any labels at
/// position `before` or after will be shifted.
pub fn insert(self: *@This(), before: usize, code: []const Instruction) Allocator.Error!void {
	const slice = try self.code.addManyAt(self.alloc, before, code.len);
	for (slice, code) |*item, *i| {
		item.* = .from(i.*);
	}
}

/// Generates a new label ID. Does not set its location.
pub fn newLabel(self: *@This()) Label {
	self.label.last_id += 1;
	return (@as(u32, self.label.func_id) << 24) | self.label.last_id;
}

/// Sets a label's position to jump to the next instruction added.
pub fn setLabel(self: *@This(), id: Label) Allocator.Error!void {
	try self.code.append(self.alloc, .{.label = id});
}

const Iter = struct {
	parent: *const Self,
	index: usize = 0,
	pos: usize = 0,
	pub fn next(self: *@This()) ?struct {item: *CodeItem, pos: usize} {
		if (self.index >= self.parent.code.items.len) {
			return null;
		}
		const pos = self.pos;
		switch (self.parent.code.items[self.index]) {
			.instruction, .jump => self.pos += 1,
			.label => {}
		}
		self.index += 1;
		return .{.item = &self.parent.code.items[self.index - 1], .pos = pos};
	}
};

pub fn items(self: *const @This()) Iter {
	return .{.parent = self};
}

pub fn deinit(self: *@This()) void {
	self.code.deinit(self.alloc);
}

pub fn finalize(self: *@This(), to_alloc: Allocator) Allocator.Error![]Instruction {
	var res_size: usize = 0;
	var iter = self.items();
	while (iter.next()) |main_item| switch (main_item.item.*) {
		.label => |label_id| {
			var l_iter = self.items();
			while (l_iter.next()) |from_item| switch (from_item.item.*) {
				.jump => |j| if (j.to == label_id) {
					const diff: isize =
						if (main_item.pos >= from_item.pos) @intCast(main_item.pos - from_item.pos)
						else -@as(isize, @intCast(from_item.pos - main_item.pos));
					from_item.item.* = .{.instruction = j.resolve(diff)};
				},
				else => {},
			};
		},
		else => {
			res_size += 1;
		},
	};

	const res = try to_alloc.alloc(Instruction, res_size);

	// set any jumps without a label to 0
	iter = self.items();
	while (iter.next()) |item| {
		switch (item.item.*) {
			.jump => |j| res[item.pos] = j.resolve(0),
			.instruction => |ins| res[item.pos] = ins,
			.label => {},
		}
	}

	self.code.clearAndFree(self.alloc);
	return res;
}