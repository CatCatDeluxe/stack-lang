const std = @import("std");
const Token = @import("../parser/token.zig");
const ASTNode = @import("../parser/node.zig").Node;
const o_writer = @import("../utils/o_writer.zig");
const ObjWriter = o_writer.ObjWriter;
const AnyScanner = @import("any_scanner.zig").AnyScanner;
const ErrList = @import("../error_list.zig");
const Variant = @import("../cross/variant.zig").Variant;
const Constants = @import("../cross/constants.zig");
const Name = @import("../text/name.zig");

const ir = @import("ir.zig");
pub const IRNode = ir.IRNode;
pub const MatchCheckNode = ir.MatchCheckNode;
pub const optimizer = @import("optimizer.zig");

pub const AnalyzerError = error {AnalyzerError};

pub const Context = struct {
	/// The error list to output to.
	errs: *ErrList,
	/// The allocator with which to allocate IR nodes. The IR nodes are not
	/// needed in memory after they are used in a later compilation step, so
	/// this can be a separate temporary allocator.
	alloc: std.mem.Allocator,
	constants: *Constants,
};

const Scope = struct {
	parent: ?*Scope,
	locals: [][]u8,
	children: []Scope,

	pub fn deinit(self: *Scope, alloc: std.mem.Allocator) void {
		for (self.locals) |lname| alloc.free(lname);
		alloc.free(self.locals);

		// for (children, child: *mut) child.deinit(alloc);

		for (self.children) |*child| child.deinit(alloc);
		alloc.free(self.children);

		self.locals = &.{};
		self.children = &.{};
	}
};

/// Returns the name of the next match node, and whether it has a body after it.
fn getMatchName(s: *AnyScanner(ASTNode), errs: *ErrList) struct {?[]const u8, bool} {
	const name: ?[]const u8 =
		nm: switch (s.next().?.*) {
			.basic => |tk| switch (tk.type) {
				.name_regular => {
					s.advance(1);
					break :nm
						if (tk.text.len == 0 or tk.text.len == 1 and tk.text[0] == '_') null
						else tk.text;
				},
				.name_operator => {
					s.advance(1);
					break :nm tk.text;
				},
				.number, .symbol => {
					// s.advance(1);
					return .{null, true};
				},
				else => {
					errs.pushError(.err, tk.position, "Unexpected token '{s}' in match names", .{tk.text});
					s.advance(1);
					return .{null, true};
				},
			},
			else => return .{null, true},
		};

	if (s.next()) |n| if (n.is_token(.sym_dot)) {
		s.advance(1);
		return .{name, true};
	};

	return .{name, false};
}

fn analyzeMatchNames(s: *AnyScanner(ASTNode), w: ObjWriter(MatchCheckNode), ctx: Context) (AnalyzerError || o_writer.Error)!void {
	while (s.valid()) {
		const name_tk = s.next().?;
		const name, const has_body = getMatchName(s, ctx.errs);
		const body: ?*ASTNode = if (has_body) s.next() else null;

		const own_name: ?Name =
			if (name != null) b: {
				var n = try parseName(ctx.errs, name_tk.basic);
				n.text = try ctx.alloc.dupe(u8, n.text);
				break :b n;
			} else null;
		errdefer if (own_name) |n| ctx.alloc.free(n.text);

		if (body) |b| {
			switch (b.*) {
				.directive => |dir_data| {
					const tk, _ = dir_data;
					ctx.errs.pushError(.err, tk.position, "Invalid directive #{s} in this context", .{tk.text});
					s.advance(1);
				},
				.match => {
					ctx.errs.pushError(.err, .{.line = 0, .col = 0, .pos = 0}, "Unexpected match-expr before ':'", .{});
					s.advance(1);
					return error.AnalyzerError;
				},
				.basic => |tk| {
					s.advance(1);

					// try to match equality for literals
					switch (tk.type) {
						.number, .symbol => {},
						else => {
							ctx.errs.pushError(.err, tk.position, "Unexpected token '{s}' as condition in match names", .{tk.text});
							return error.AnalyzerError;
						}
					}

					const inner = try w.add(); errdefer _ = w.unadd();
					const equals_name = try ctx.alloc.dupe(u8, "="); errdefer ctx.alloc.free(equals_name);

					var arr = std.array_list.Managed(IRNode).init(ctx.alloc); errdefer arr.deinit();
					const child_w = ObjWriter(IRNode).from_arr(&arr);

					try analyze(b.*, child_w, ctx);
					try child_w.writeAll(&.{
						IRNode {
							.data = .{.push_named = Name.from(equals_name) catch unreachable},
							.root = tk,
						},
						IRNode {
							.data = .{.call = {}},
							.root = tk,
						}
					});

					inner.* = MatchCheckNode {
						.root = tk,
						.name = own_name,
						.check = .{.regular = try arr.toOwnedSlice()},
					};
				},
				.block => |block| {
					const res = try w.add();
					errdefer _ = w.unadd();

					const block_root, const children = block;

					var arr = std.array_list.Managed(IRNode).init(ctx.alloc);
					errdefer arr.deinit();
					const child_w = ObjWriter(IRNode).from_arr(&arr);

					for (children) |ast| {
						try analyze(ast, child_w, ctx);
					}

					res.* = MatchCheckNode {
						.root = block_root,
						.name = own_name,
						.check = .{.regular = try arr.toOwnedSlice()}
					};
					s.advance(1);
				},
				.func_block => |block| {
					const block_root, const children = block;
					var child_s = AnyScanner(ASTNode).init(children);

					const res = try w.add();
					errdefer _ = w.unadd();

					var arr = std.array_list.Managed(MatchCheckNode).init(ctx.alloc);
					const child_w = ObjWriter(MatchCheckNode).from_arr(&arr);
					errdefer {
						for (arr.items) |n| n.deinit(ctx.alloc);
						arr.deinit();
					}

					try analyzeMatchNames(&child_s, child_w, ctx);

					res.* = MatchCheckNode {
						.root = block_root,
						.name = own_name,
						.check = .{.func_expand = try arr.toOwnedSlice()}
					};
					s.advance(1);
				}
			}
		} else {
			try w.write(.{
				.root = name_tk.basic,
				.name = own_name,
				.check = .{.none = {}},
			});
		}
	}
}

pub fn analyze(ast: ASTNode, w: ObjWriter(IRNode), ctx: Context) (AnalyzerError || o_writer.Error)!void {
	switch (ast) {
		.basic => |tk| {
			switch (tk.type) {
				.name_regular, .name_operator => {
					var name = try parseName(ctx.errs, tk);
					name.text = try ctx.alloc.dupe(u8, name.text);
					errdefer ctx.alloc.free(name.text);

					try w.write(.init(tk, .{.push_named = name}));
					if (tk.type == .name_operator) try w.write(.init(tk, .{.call = {}}));
				},
				.number => {
					const parsed = std.fmt.parseFloat(f64, tk.text) catch |err| {
						ctx.errs.pushError(.err, tk.position, "Invalid format for number: {}", .{err});
						return error.AnalyzerError;
					};
					try w.write(.init(tk, .{.push = .{.num = parsed}}));
				},
				.symbol => {
					const sym_var = ctx.constants.syms.variant(tk.text)
						catch |err| switch (err) {
							error.SymbolLimitExceeded => {
								ctx.errs.pushError(.err, tk.position, "Symbol limit exceeded!", .{});
								return;
							},
							error.TooManySegments => {
								ctx.errs.pushError(.err, tk.position, "Name has too many segments", .{});
								return;
							},
							else => |e| return e,
						};
					try w.write(.init(tk, .{.push = sym_var}));
				},
				.sym_call => try w.write(.init(tk, .{.call = {}})),
				.sym_this_func => try w.write(.init(tk, .{.push_own_func = {}})),
				else => {
					ctx.errs.pushError(.err, tk.position, "Token type '{s}' is invalid in this context", .{@tagName(tk.type)});
					return;
				},
			}
		},
		.directive => |dir_args| {
			const tk, const directive = dir_args;
			try w.write(.init(tk, .{.directive = directive}));
		},
		.block => |block| { // simple blocks in the AST don't get any special treatment
			_, const nodes = block;
			for (nodes) |node| try analyze(node, w, ctx);
		},
		.func_block => |block| {
			const root, const nodes = block;
			var arr = std.array_list.Managed(IRNode).init(ctx.alloc);
			const child_w = ObjWriter(IRNode).from_arr(&arr);

			errdefer {
				for (arr.items) |node| node.deinit(ctx.alloc);
				arr.deinit();
			}

			for (nodes) |node| try analyze(node, child_w, ctx);
			const res = IRNode.init(root, .{.func = try arr.toOwnedSlice()});
			errdefer res.deinit(ctx.alloc); try w.write(res);
		},
		.match => |ast_cases| {
			const res = try w.add();
			errdefer _ = w.unadd();


			var cases = std.array_list.Managed(IRNode.MatchCase).init(ctx.alloc);
			errdefer {
				for (cases.items) |case| IRNode.deinitCase(case, ctx.alloc);
				cases.deinit();
			}

			for (ast_cases) |ast_case| {
				const ast_names, const ast_body = ast_case;

				const case = try cases.addOne();
				errdefer IRNode.deinitCase(cases.pop().?, ctx.alloc);
				case.* = .{&.{}, &.{}};

				var names = std.array_list.Managed(MatchCheckNode).init(ctx.alloc);
				const names_w = ObjWriter(MatchCheckNode).from_arr(&names);
				var names_r = AnyScanner(ASTNode).init(ast_names);

				var body = std.array_list.Managed(IRNode).init(ctx.alloc);
				const body_w = ObjWriter(IRNode).from_arr(&body);

				errdefer {
					for (body.items) |n| n.deinit(ctx.alloc);
					for (names.items) |m| m.deinit(ctx.alloc);
					body.deinit();
					names.deinit();
				}

				try analyzeMatchNames(&names_r, names_w, ctx);
				for (ast_body) |node| try analyze(node, body_w, ctx);

				case.* = .{try names.toOwnedSlice(), try body.toOwnedSlice()};
			}

			// TODO: give proper line information for match nodes
			res.* = .init(undefined, .{.match = try cases.toOwnedSlice()});
		}
	}
}

/// Analyzes all the AST nodes and outputs a list of IR nodes.
/// This is the same as calling `analyze` a lot of times.
pub fn analyzeAll(asts: []const ASTNode, ctx: Context) (AnalyzerError || error {OutOfMemory})![]IRNode {
	var res = std.array_list.Managed(ir.IRNode).init(ctx.alloc);
	const writer = ObjWriter(ir.IRNode).from_arr(&res);

	for (asts) |ast_node| {
		analyze(ast_node, writer, ctx) catch |err| switch (err) {
			error.WriteFailed => unreachable,
			else => |e| return e,
		};
	}

	return try res.toOwnedSlice();
}

fn print_m(matches: []const MatchCheckNode) void {
	for (matches, 0..) |m, i| {
		if (i > 0) std.debug.print(" ", .{});
		if (m.name) |n| {
			if (n.text.len == 0) std.debug.print("\x1b[32;2m_\x1b[0m", .{})
			else std.debug.print("\x1b[32m{f}\x1b[0m", .{n});
		}
		switch (m.check) {
			.none => {
				if (m.name == null) std.debug.print("_", .{});
			},
			.func_expand => |children| {
				std.debug.print("\x1b[35m{{\x1b[0m", .{});
				print_m(children);
				std.debug.print("\x1b[35m}}\x1b[0m", .{});
			},
			.regular => |children| {
				std.debug.print("\x1b[35m(\x1b[0m", .{});
				print_ir(children);
				std.debug.print("\x1b[35m)\x1b[0m", .{});
			}
		}
	}
}

pub fn print_ir(nodes: []const IRNode) void {
	for (nodes, 0..) |node, i| {
		if (i > 0) std.debug.print(" ", .{});
		switch (node.data) {
			.push_named => |name| std.debug.print("\x1b[34m{f}\x1b[0m", .{name}),
			.push => |v| std.debug.print("\x1b[33m{f}\x1b[0m", .{v}),
			.call => std.debug.print("!", .{}),
			.tail_call => std.debug.print("! \x1b[2m(tail call)\x1b[0m", .{}),
			.match => |cases| {
				std.debug.print("\x1b[1m(\x1b[0m", .{});
				for (cases, 0..) |case, case_i| {
					if (case_i > 0) std.debug.print("\x1b[1m | \x1b[0m", .{});
					const checks, const body = case;
					print_m(checks);
					std.debug.print("\x1b[1m: \x1b[0m", .{});
					print_ir(body);
				}
				std.debug.print("\x1b[1m)\x1b[0m", .{});
			},
			.func => |body| {
				std.debug.print("\x1b[1m{{\x1b[0m", .{});
				print_ir(body);
				std.debug.print("\x1b[1m}}\x1b[0m", .{});
			},
			.pop => std.debug.print("pop", .{}),
			.push_own_func => std.debug.print("@", .{}),
			.directive => |d| std.debug.print("#{s}", .{@tagName(d)}),
			//else => std.debug.print("<unsupported>: {}", .{node}),
		}
	}
}

fn parseName(errs: *ErrList, tk: Token) AnalyzerError!Name {
	return Name.from(tk.text) catch |err| {
		errs.pushError(.err, tk.position, "Invalid name '{s}': {s}", .{tk.text, @errorName(err)});
		return AnalyzerError.AnalyzerError;
	};
}