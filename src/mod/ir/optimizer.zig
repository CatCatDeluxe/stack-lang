const std = @import("std");
const ir = @import("ir.zig");
const ErrorList = @import("../error_list.zig");
const Name = @import("../text/name.zig");

pub const Context = struct {
	/// The allocator used for the IR nodes.
	alloc: std.mem.Allocator,
	temp: std.mem.Allocator,
	errors: *ErrorList,
};

const Scope = struct {
	const NameInfo = struct {
		origin: *ir.MatchCheckNode,
		uses: u32,
	};

	parent: ?*Scope,
	info: std.HashMap(Name, NameInfo, Name.HashContext, 80).Unmanaged = .empty,

	fn deinit(self: Scope, alloc: std.mem.Allocator) void {
		self.info.deinit(alloc);
	}

	fn use(self: *Scope, name: Name) bool {
		var s: ?*Scope = self;
		while (s) |scope| {
			if (scope.info.getPtr(name)) |info| {
				info.uses += 1;
				return true;
			}
			s = scope.parent;
		}
		return false;
	}
};

// May reallocate `nodes`.
pub fn rewrite(c: Context, body: []ir.IRNode) !void {
	var new = std.ArrayList(ir.IRNode).empty;
	_ = body;
	return try new.toOwnedSlice(c.alloc);
}

/// A pass to find tail calls. Marks any appropriate calls as tail calls in
/// the block and any contained branches.
pub fn findTailCalls(c: Context, block: []ir.IRNode) void {
	var can_tail_call = true;
	var i = block.len;

	while (i > 0) { i -= 1;
		switch (block[i].data) {
			.call => if (can_tail_call) {
				block[i].data = .{.tail_call = {}};
				can_tail_call = false;
			},
			.match => |branches| if (can_tail_call) {
				for (branches) |branch| {
					_, const body = branch;
					findTailCalls(c, body);
				}
				can_tail_call = false;
			},
			// TODO: check if a directive affects control flow
			.directive => {},
			.func => |code| findTailCalls(c, code),
			else => can_tail_call = false,
		}
	}
}

fn findUnusedNamesBranches(c: Context, pattern: []ir.MatchCheckNode, scope: *Scope) std.mem.Allocator.Error!void {
	// iterate through patterns in reverse order for proper semantics
	var i = pattern.len;
	while (i > 0) { i -= 1;
		const check = &pattern[i];
		if (check.name) |name| {
			const p = try scope.info.getOrPut(c.temp, name);
			if (p.found_existing) {
				p.value_ptr.uses += 1;
			} else {
				p.value_ptr.* = .{
					.origin = check,
					.uses = 0,
				};
			}
		}
		switch (check.check) {
			.none => {},
			.regular => |code| {
				try findUnusedNames(c, code, scope);
			},
			.func_expand => |sub_pattern| {
				try findUnusedNamesBranches(c, sub_pattern, scope);
			},
		}
	}
}

/// Does everyhing something that needs to know about local variables would do.
pub fn findUnusedNames(c: Context, block: []ir.IRNode, parent_scope: ?*Scope) std.mem.Allocator.Error!void {
	for (block) |node| {
		switch (node.data) {
			.match => |branches| for (branches) |branch| {
				var scope = Scope {
					.parent = parent_scope,
				};
				const pattern: []ir.MatchCheckNode, const body: []ir.IRNode = branch;
				try findUnusedNamesBranches(c, pattern, &scope);
				try findUnusedNames(c, body, &scope);

				// detect the unused names
				var iter = scope.info.iterator();
				while (iter.next()) |pair| {
					if (pair.value_ptr.uses == 0) {
						c.errors.pushError(.warning, pair.value_ptr.origin.root.position, "unused name '{f}'", .{pair.key_ptr});
						// remove the name, since it is never used
						c.alloc.free(pair.value_ptr.origin.name.?.text);
						pair.value_ptr.origin.name = null;
					}
				}
			},
			.push_named => |name| {
				if (parent_scope) |scope| {
					_ = scope.use(name);
				}
			},
			.func => |body| {
				var scope = Scope {
					.parent = parent_scope,
				};
				try findUnusedNames(c, body, &scope);
			},
			else => {},
		}
	}
}