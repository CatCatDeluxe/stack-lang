const std = @import("std");
const IRNode = @import("../ir/analyzer.zig").IRNode;
const MatchCheckNode = @import("../ir/analyzer.zig").MatchCheckNode;
const ObjWriter = @import("../utils/o_writer.zig").ObjWriter;
const ErrorList = @import("../error_list.zig");
const NameList = @import("./name_list.zig");
const Constants = @import("../cross/constants.zig");
const Name = @import("../text/name.zig");

pub const Instruction = @import("instruction.zig");
const Variant = @import("../cross/variant.zig").Variant;

pub const debug_mode = true;
/// Change to u16 or u32 to support more locals
pub const LocalIndex = u8;

/// Any error emitted just from the compilation process.
pub const Error = error {} || Name.Error;

pub const CompileOptions = struct {
	temp_alloc: std.mem.Allocator,
	constants: *Constants,
	filename: []const u8,
	errors: *ErrorList,
};

const Scope = struct {
	parent: ?*Scope,
	is_function: bool,
	locals: NameList = .{},

	fn deinit(self: *Scope, alloc: std.mem.Allocator) void {
		self.locals.deinit(alloc);
	}
};

const JumpList = struct {
	const JumpType = enum {jump, fail_check_if_false};
	const UnresolvedJump = struct {
		from: usize,
		type: JumpType,
	};

	const empty = @This() {.jumps = .empty};

	jumps: std.ArrayList(UnresolvedJump),

	fn addJump(self: *@This(), alloc: std.mem.Allocator, func: *Constants.CreateFunc, j_type: JumpType) !void {
		const idx = try func.addOne();
		errdefer _ = func.insts.pop();
		try self.jumps.append(alloc, .{.from = idx, .type = j_type});
	}

	/// Writes to `body` at the location of the jumps in `jumps`.
	fn resolve(self: @This(), body: []Instruction, to: usize) void {
		for (self.jumps.items) |jump| {
			const diff: isize =
				if (to >= jump.from) @intCast(to - jump.from)
				else -@as(isize, @intCast(jump.from - to));

			body[jump.from] = switch (jump.type) {
				.jump => .init(.jump, .{.line = 0, .col = 0, .position = 0}, @intCast(diff)),
				.fail_check_if_false => .init(.fail_check_if_false, .{.line = 0, .col = 0, .position = 0}, @intCast(diff)),
			};
		}
	}

	/// Deinit the jump list. Probably will not be used, because when compiling,
	/// memory is allocated in an arena.
	fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
		self.jumps.deinit(alloc);
	}
};

/// Generates the checks for a single branch.
fn genChecks(
	func: *Constants.CreateFunc,
	checks: []const MatchCheckNode,
	added_locals: *NameList,
	scope: *Scope,
	/// Appends the locations of the jump-if-falses to the next case to this array.
	next_case_jumps: *JumpList,
	/// Not really used by the checks, but must be passed for compilation of inner blocks
	captures: *NameList,
	opts: CompileOptions,
) !void {
	// Add names and insert checks
	var index = checks.len;
	while (index > 0) {
		index -= 1;
		const check = checks[index];
		var add_pop = true;

		if (check.name) |name| {
			if (added_locals.get(name) != null) {
				const local_index = scope.locals.get(name).?;
				// existing name used -- check for equality instead
				try func.add(.init(.push_local, check.root.position, local_index));
				try func.add(.init(.test_equal, check.root.position, {}));
				// test_equal pops the values automatically
				add_pop = false;

				// Store the index of an empty instruction, later it will be changed
				// to a jump to the next branch
				try next_case_jumps.addJump(opts.temp_alloc, func, .fail_check_if_false);
			} else {
				// new name introduced -- add local
				if (scope.locals.names.items.len == std.math.maxInt(u8)) {
					opts.errors.pushError(.err, check.root.position,
						"Number of locals exceeded the maximum of {}",
						.{std.math.maxInt(u8)});
				}

				_ = try added_locals.add(opts.temp_alloc, name);
				_ = try scope.locals.add(opts.temp_alloc, name);
				try func.add(.init(.add_local, check.root.position, {}));
			}
		}

		switch (check.check) {
			.none => {},
			.func_expand => |func_checks| {
				add_pop = false;
				try func.add(.init(.push_temp_stack, check.root.position, 1));
				try func.add(.init(.call, check.root.position, {}));

				var func_next_case_jumps = JumpList.empty;
				defer func_next_case_jumps.deinit(opts.temp_alloc);

				try genChecks(func, func_checks, added_locals, scope, &func_next_case_jumps, captures, opts);

				try func.add(.init(.pop_temp_stack, check.root.position, 0));

				if (func_next_case_jumps.jumps.items.len > 0) {
					// jump past the other cleanup for failed branches
					try func.add(.init(.jump, check.root.position, 3));

					func_next_case_jumps.resolve(func.insts.items, func.insts.items.len);
					try func.add(.init(.pop_temp_stack, check.root.position, 0));
					try next_case_jumps.addJump(opts.temp_alloc, func, .jump);
				}
			},
			.regular => |nodes| {
				try func.add(.init(.push_temp_stack, check.root.position, 1));
				for (nodes) |node| try compileIn(node, opts, func, captures, scope);
				try func.add(.init(.pop_temp_stack, check.root.position, 1));

				// jump to the next branch if the result is not truthy
				try next_case_jumps.addJump(opts.temp_alloc, func, .fail_check_if_false);
			}
		}

		if (add_pop) try func.add(.init(.pop, check.root.position, 1));
	}
}

fn compileIn(
	ir: IRNode,
	opts: CompileOptions,
	/// The function to write instructions to.
	func: *Constants.CreateFunc,
	captures: *NameList,
	scope: *Scope
) (Error || Name.Error || error {OutOfMemory})!void {
	// const alloc = w.allocator;

	switch (ir.data) {
		.call => try func.add(.init(.call, ir.root.position, {})),
		.tail_call => try func.add(.init(.tail_call, ir.root.position, {})),
		.pop => try func.add(.init(.pop, ir.root.position, 1)),
		.push_own_func => try func.add(.init(.push_own_func, ir.root.position, {})),
		.directive => |directive| switch (directive) {
			.breakpoint => try func.add(.init(.breakpoint, ir.root.position, {})),
		},
		.func => |body| {
			var new_func = opts.constants.createFunc(opts.filename);
			errdefer new_func.deinit();

			var f_captures = NameList {};
			var f_scope = Scope {.is_function = true, .parent = scope};
			defer f_scope.deinit(opts.temp_alloc);
			defer f_captures.deinit(opts.temp_alloc);

			for (body) |child_ir| try compileIn(child_ir, opts, &new_func, &f_captures, &f_scope);

			const func_id = try new_func.finish();

			// For each name the function captures, locate that name and push it to the stack.
			for (f_captures.names.items) |name| {
				if (scope.locals.get(name)) |index| {
					try func.add(.init(.push_local, ir.root.position, index));
				} else {
					// We can assume that if not a local, the name is otherwise
					// a capture from an upper scope, because of the checks run
					// when adding a capture.
					const capture_id = captures.indexName(opts.temp_alloc, name) catch |err| {
						if (err == error.NameListOverflow) {
							opts.errors.pushError(.err, ir.root.position, "Capture list has too many items", .{});
						}
						return err;
					};
					try func.add(.init(.push_capture, ir.root.position, capture_id));
				}
			}

			try func.add(.init(.push_global, ir.root.position, func_id));

			if (f_captures.names.items.len > 0) {
				try func.add(.init(.assign_captures, ir.root.position, @intCast(f_captures.names.items.len)));
			}
		},
		.push => |variant| switch (variant) {
			.num => |f| try func.add(.init(.push_float, ir.root.position, f)),
			.symbol => |s| try func.add(.init(.push_sym, ir.root.position, s)),
			else => std.debug.panic("Invalid variant type for 'push': {} ({f} in {any})",
				.{std.meta.activeTag(variant), variant, ir.root.position}),
		},
		.push_named => |name| {
			if (scope.locals.get(name)) |index| {
				// Use a local
				try func.add(.init(.push_local, ir.root.position, index));
			} else if (parentLocalExists(scope.parent, name)) {
				const capture_id = captures.indexName(opts.temp_alloc, name) catch |err| {
					if (err == error.NameListOverflow) {
						opts.errors.pushError(.err, ir.root.position, "Capture list has too many items", .{});
					}
					return err;
				};

				try func.add(.init(.push_capture, ir.root.position, capture_id));
			} else if (opts.constants.names.get(name)) |global_id| {
				// Use a global variable
				try func.add(.init(.push_global, ir.root.position, global_id));
			} else {
				opts.errors.pushError(.err, ir.root.position,
					"Cannot find any local, capture, or global named '{f}' in this scope",
					.{name});
			}
		},
		.match => |cases| {
			// The list of indices of `jump` instructions that should jump to
			// the end of the whole match block.
			var end_jumps = JumpList.empty;
			defer end_jumps.deinit(opts.temp_alloc);

			for (cases, 0..) |case, case_index| {
				const checks, const body = case;

				// placeholder position for instructions
				const placeholder_pos = if (checks.len > 0) checks[0].root.position else undefined;

				if (checks.len > std.math.maxInt(u8)) {
					opts.errors.pushError(.err, placeholder_pos,
						"Maximum number of terms in a match is {}, but got {}",
						.{std.math.maxInt(u8), checks.len});
					return;
				}

				const first_pos = func.insts.items.len;
				try func.add(.init(.branch_check_begin, placeholder_pos,
					.{.n_locals = @intCast(checks.len), .jump = undefined}));

				// The list of names used in the check
				var check_name_list = NameList {};
				// The list of indices of instructions that should jump to the next case
				var next_case_jumps = JumpList.empty;
				defer next_case_jumps.deinit(opts.temp_alloc);

				try genChecks(
					func, checks, &check_name_list,
					scope, &next_case_jumps,
					captures, opts);

				for (body) |node| try compileIn(node, opts, func, captures, scope);

				// If the branch is successful, jumps to after the last branch.
				// This is not needed for the last branch.
				if (case_index < cases.len - 1) {
					try end_jumps.addJump(opts.temp_alloc, func, .jump);
				}

				// Write to all locations where a jump to the next branch is needed:

				// The branch_check_begin will jump to this location if there
				// are not enough items on the stack. At the end of the match
				// block, it needs to jump *past* the pop_local_count, since
				// it would have never added the locals in the first place.
				const end_pos = func.insts.items.len;
				{
					const fail_jump_to: u16 =
						if (case_index < cases.len - 1) @intCast(end_pos - first_pos)
						else 0; // on the last branch, the jump offset is 0, signalling the match failed
					func.insts.items[first_pos].data.branch_check_begin.jump = fail_jump_to;
				}
				next_case_jumps.resolve(func.insts.items, end_pos);

				// Remove all the locals that might have been added by this check.
				scope.locals.remove(opts.temp_alloc, check_name_list.names.items.len);
			}

			// Write to all locations where a jump to the end is needed
			end_jumps.resolve(func.insts.items, func.insts.items.len);

			// Only add a pop_local_count to the end of the last
			// branch. Other branches can jump to it at the end of
			// their code.
			try func.add(.init(.pop_local_count, .{.line = 0, .col = 0, .position = 0}, {}));
		},
	}
}

/// Returns whether a local exists in the scope `first_parent` or any of its parent scopes.
fn parentLocalExists(first_parent: ?*Scope, name: Name) bool {
	var parent = first_parent;
	while (parent) |p| {
		if (p.locals.get(name)) |_| return true;
		parent = p.parent;
	}
	return false;
}

/// Compiles one IR node and writes the resulting instruction(s) to `func`.
/// This may also modify the provided `Constants` inside of `opts`.
pub fn compileFunc(ir: IRNode, opts: CompileOptions, func: *Constants.CreateFunc) (Error || Name.Error || error {OutOfMemory})!void {
	var scope = Scope {.parent = null, .is_function = false, .locals = .{}};
	var captures = NameList {};
	defer captures.deinit(opts.temp_alloc);

	try compileIn(ir, opts, func, &captures, &scope);
}

/// Compiles all of the IR nodes to a function. Returns the ID of the *variant*
/// referring to that function in the provided `Constants`.
pub fn compile(nodes: []const IRNode, opts: CompileOptions) (Error || Name.Error || error {OutOfMemory})!Constants.ID {
	var func = opts.constants.createFunc(opts.filename);
	errdefer func.deinit();
	for (nodes) |ir| {
		try compileFunc(ir, opts, &func);
	}
	return try func.finish();
}