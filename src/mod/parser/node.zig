const std = @import("std");
const Token = @import("token.zig");
const Directive = @import("../cross/directive.zig").Directive;

/// An AST node.
pub const Node = union(enum) {
	/// Contains information for a block. Has the starting token of the block and its children.
	pub const Block = struct { Token, []Node };
	/// Contains information for a match. Has the name block and the child block.
	pub const Match = struct { []Node, []Node, };

	/// A simple node that only needs one token of information.
	basic: Token,
	/// A list of matches, which consist of a name-list and a body.
	match: []Match,
	/// A grouping of tokens. This may or may not be redundant.
	block: Block,
	/// A grouping of tokens into a function.
	func_block: Block,
	/// Any directive.
	directive: struct {Token, Directive},

	pub fn matchDeinit(m: Match, alloc: std.mem.Allocator) void {
		const names, const body = m;
		for (names) |n| n.deinit(alloc);
		for (body) |n| n.deinit(alloc);
		alloc.free(names);
		alloc.free(body);
	}

	pub fn deinit(self: Node, alloc: std.mem.Allocator) void {
		switch (self) {
			.basic => {},
			.match => |matches| {
				for (matches) |m| matchDeinit(m, alloc);
				alloc.free(matches);
			},
			.block, .func_block => |b| {
				_, const children = b;
				for (children) |child| child.deinit(alloc);
				alloc.free(children);
			},
			.directive => {},
		}
	}

	/// Returns whether this AST node is basic and its token type is `tp`.
	pub fn is_token(self: Node, tp: Token.Type) bool {
		return switch (self) {
			.basic => |tk| tk.type == tp,
			else => false,
		};
	}

	/// Prints a colourized version of the AST node.
	pub fn print(node: Node, out: *std.Io.Writer, indent: usize, sep: []const u8) error{WriteFailed}!void {
		for (0..indent) |_| try out.print("  ", .{});
		switch (node) {
			.basic => |tk| try out.print("{s}\x1b[32m'{s}'\x1b[0m", .{@tagName(tk.type), tk.text}),
			.block, .func_block => |b| {
				_, const children = b;
				const brackets = switch (node) {.block => "()", .func_block => "{}", else => unreachable};
				try out.print("{c}{s}", .{brackets[0], sep});
				for (children) |child| {
					try child.print(out, if (indent > 0) indent + 1 else 0, sep);
					try out.print("{s}", .{sep});
				}
				for (0..indent) |_| std.debug.print("  ", .{});
				try out.print("{c}", .{brackets[1]});
			},
			.match => |matches| {
				for (matches, 0..) |m, mi| {
					if (mi > 0) {
						try out.print("{s}", .{sep});
						for (0..indent) |_| std.debug.print("  ", .{});
					}
					const names, const children = m;
					try out.print("match (", .{});
					for (names, 0..) |n, i| {
						if (i > 0) try out.print(" ", .{});
						try n.print(out, 0, " ");
					}
					try out.print("):{s}", .{sep});
					for (children, 0..) |child, i| {
						if (i > 0) std.debug.print("{s}", .{sep});
						try child.print(out, indent + 1, sep);
					}
				}
			},
			.directive => |d| {
				try out.print("#{s}", .{d.@"0".text});
			},
		}
	}

	pub fn format(self: @This(), writer: *std.Io.Writer) error{WriteFailed}!void {
		try self.print(writer, 0, " ");
	}
};