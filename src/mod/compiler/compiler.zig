const std = @import("std");
const IRNode = @import("../ir/analyzer.zig").IRNode;
const MatchCheckNode = @import("../ir/analyzer.zig").MatchCheckNode;
const ObjWriter = @import("../utils/o_writer.zig").ObjWriter;
const ErrorList = @import("../error_list.zig");
const NameList = @import("./name_list.zig");
const Constants = @import("../cross/constants.zig");
const Name = @import("../text/name.zig");
const Func = @import("funcs/create_func.zig");

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

/// The context for compiling a match.
const MatchContext = struct {
	has_temp_stacks: bool,
	scope: *Scope,
	captures: *NameList,
	end_label: Func.Label,
};

/// Generates the checks for a single branch.
fn genChecks(
	func: *Func,
	ctx: *MatchContext,
	checks: []const MatchCheckNode,
	added_locals: *NameList,
	next_case: Func.Label,
	opts: CompileOptions,
) !void {
	// Add names and insert checks
	for (checks) |*check| {
		var add_pop = true;

		if (check.name) |name| {
			if (added_locals.get(name) != null) {
				const local_index = ctx.scope.locals.get(name).?;
				// existing name used -- check for equality instead
				try func.append(.init(.push_local, check.root.position, local_index));
				try func.append(.init(.test_equal, check.root.position, {}));
				// test_equal pops the values automatically
				add_pop = false;

				// Store the index of an empty instruction, later it will be changed
				// to a jump to the next branch
				try func.appendJump(next_case, .init(.fail_check_if_false, check.root.position, undefined));
			} else {
				// new name introduced -- add local
				if (ctx.scope.locals.names.items.len == std.math.maxInt(u8)) {
					opts.errors.pushError(.err, check.root.position,
						"Number of locals exceeded the maximum of {}",
						.{std.math.maxInt(u8)});
				}

				_ = try added_locals.add(opts.temp_alloc, name);
				_ = try ctx.scope.locals.add(opts.temp_alloc, name);
				try func.append(.init(.add_local, check.root.position, {}));
			}
		}

		switch (check.check) {
			.none => {},
			.func_expand => |func_checks| {
				ctx.has_temp_stacks = true;
				//var func_next_case_jumps = JumpList.empty;
				//defer func_next_case_jumps.deinit(opts.temp_alloc);

				try func.append(.init(.push_temp_stack, check.root.position, 1));
				try func.append(.init(.call, check.root.position, {}));
				if (func_checks.len > 0) {
					try func.append(.init(.check_stack_length, check.root.position, @intCast(func_checks.len)));
					try func.appendJump(next_case, .init(.fail_check_if_false, check.root.position, undefined));
				}

				try genChecks(func, ctx, func_checks, added_locals, next_case, opts);
				try func.append(.init(.pop_temp_stack, check.root.position, 0));
				//if (func_next_case_jumps.jumps.items.len > 0) {
					// jump past the other cleanup for failed branches
					//try func.append(.init(.jump, check.root.position, 3));

					//try func.append(.init(.pop_temp_stack, check.root.position, 0));
					//try next_case_jumps.addJump(opts.temp_alloc, func, .fail_check);
				//}
			},
			.regular => |nodes| {
				try func.append(.init(.push_temp_stack, check.root.position, 1));
				for (nodes) |node| try compileIn(node, opts, func, ctx.captures, ctx.scope);
				try func.append(.init(.pop_temp_stack, check.root.position, 1));

				// jump to the next branch if the result is not truthy
				try func.appendJump(next_case, .init(.fail_check_if_false, check.root.position, undefined));
			}
		}

		if (add_pop) try func.append(.init(.pop, check.root.position, 1));
	}
}

fn compileIn(
	ir: IRNode,
	opts: CompileOptions,
	/// The function to write instructions to.
	func: *Func,
	captures: *NameList,
	scope: *Scope
) (Error || Name.Error || error {OutOfMemory})!void {
	// const alloc = w.allocator;

	switch (ir.data) {
		.call => try func.append(.init(.call, ir.root.position, {})),
		.tail_call => try func.append(.init(.tail_call, ir.root.position, {})),
		.pop => try func.append(.init(.pop, ir.root.position, 1)),
		.push_own_func => try func.append(.init(.push_own_func, ir.root.position, {})),
		.directive => |directive| switch (directive) {
			.breakpoint => try func.append(.init(.breakpoint, ir.root.position, {})),
		},
		.func => |body| {
			var new_func = opts.constants.createFunc(opts.filename);
			errdefer new_func.deinit();

			var f_captures = NameList {};
			var f_scope = Scope {.is_function = true, .parent = scope};
			defer f_scope.deinit(opts.temp_alloc);
			defer f_captures.deinit(opts.temp_alloc);

			for (body) |child_ir| try compileIn(child_ir, opts, &new_func.builder, &f_captures, &f_scope);

			const func_id = try new_func.finish();

			// For each name the function captures, locate that name and push it to the stack.
			for (f_captures.names.items) |name| {
				if (scope.locals.get(name)) |index| {
					try func.append(.init(.push_local, ir.root.position, index));
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
					try func.append(.init(.push_capture, ir.root.position, capture_id));
				}
			}

			try func.append(.init(.push_global, ir.root.position, func_id));

			if (f_captures.names.items.len > 0) {
				try func.append(.init(.assign_captures, ir.root.position, @intCast(f_captures.names.items.len)));
			}
		},
		.push => |variant| switch (variant) {
			.num => |f| try func.append(.init(.push_float, ir.root.position, f)),
			.symbol => |s| try func.append(.init(.push_sym, ir.root.position, s)),
			else => std.debug.panic("Invalid variant type for 'push': {} ({f} in {any})",
				.{std.meta.activeTag(variant), variant, ir.root.position}),
		},
		.push_named => |name| {
			if (scope.locals.get(name)) |index| {
				// Use a local
				try func.append(.init(.push_local, ir.root.position, index));
			} else if (parentLocalExists(scope.parent, name)) {
				const capture_id = captures.indexName(opts.temp_alloc, name) catch |err| {
					if (err == error.NameListOverflow) {
						opts.errors.pushError(.err, ir.root.position, "Capture list has too many items", .{});
					}
					return err;
				};

				try func.append(.init(.push_capture, ir.root.position, capture_id));
			} else if (opts.constants.names.get(name)) |global_id| {
				// Use a global variable
				try func.append(.init(.push_global, ir.root.position, global_id));
			} else {
				opts.errors.pushError(.err, ir.root.position,
					"Cannot find any local, capture, or global named '{f}' in this scope",
					.{name});
			}
		},
		.match => |cases| {
			var ctx = MatchContext {
				.captures = captures,
				.scope = scope,
				.has_temp_stacks = false,
				.end_label = func.newLabel(),
			};

			for (cases, 0..) |case, case_index| {
				// TODO: optimize branches that can't fail after branch_check_begin
				const next_case_label = func.newLabel();
				const checks, const body = case;
				// placeholder position for instructions
				const placeholder_pos = if (checks.len > 0) checks[0].root.position else undefined;

				if (checks.len > std.math.maxInt(u8)) {
					opts.errors.pushError(.err, placeholder_pos,
						"Maximum number of terms in a match is {}, but got {}",
						.{std.math.maxInt(u8), checks.len});
					return;
				}

				// Compile the check to an inner function.
				// This is helpful, because after compiling the check, there is
				// information that the compiler might want to use to place
				// instructions right after the branch_check_begin.
				// this way, that can be done without shifting other
				// instructions over.
				var check_code = func.sub();
				defer check_code.deinit();

				// this is reset per-check
				ctx.has_temp_stacks = false;

				try func.appendJump(next_case_label, .init(.branch_check_begin, placeholder_pos,
					.{.n_locals = @intCast(checks.len), .jump = undefined}));

				// The list of names used in the check
				var check_name_list = NameList {};


				// generate the checks (but don't append them to the main func yet)
				try genChecks(
					&check_code, &ctx, checks, &check_name_list,
					next_case_label, opts);

				try func.appendSub(check_code);
				try func.append(.init(.end_check, placeholder_pos, {}));

				for (body) |node| try compileIn(node, opts, func, captures, scope);
				// If the branch is successful, jumps to after the last branch.
				// This is not needed for the last branch, if it is not popping a stack count.
				if (case_index < cases.len - 1 or ctx.has_temp_stacks) {
					try func.appendJump(ctx.end_label, .init(.jump, placeholder_pos, undefined));
				}

				// If this is the last case, all error jumps to the next case
				// should not be set
				if (case_index < cases.len - 1) {
					try func.setLabel(next_case_label);
				}

				// Remove all the locals that might have been added by this check.
				scope.locals.remove(opts.temp_alloc, check_name_list.names.items.len);
			}

			try func.setLabel(ctx.end_label);
			// Only add a pop_local_count to the end of the last
			// branch. Other branches can jump to it at the end of
			// their code.
			try func.append(.init(.pop_local_count, .{.line = 0, .col = 0, .position = 0}, {}));
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

	try compileIn(ir, opts, &func.builder, &captures, &scope);
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