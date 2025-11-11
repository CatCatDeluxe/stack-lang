const std = @import("std");
const Token = @import("../parser/token.zig");
const Variant = @import("../cross/variant.zig").Variant;
const Directive = @import("../cross/directive.zig").Directive;
const Name = @import("../text/name.zig");

pub const MatchCheckNode = struct {
	/// The name of the match node. Empty if it should not bind to any name.
	root: Token,
	/// The name. The text is owned by the node.
	name: ?Name,
	check: union(enum) {
		none: void,
		/// Runs some code to check if the value is matched
		regular: []IRNode,
		/// Matches the result of calling a function
		func_expand: []MatchCheckNode,
		/// Checks equality with a constant.
		/// The variant should not have any allocated data.
		literal: Variant,
	},

	pub fn deinit(self: MatchCheckNode, alloc: std.mem.Allocator) void {
		if (self.name) |n| alloc.free(n.text);
		switch (self.check) {
			.none => {},
			.regular => |nodes| {
				for (nodes) |n| n.deinit(alloc);
				alloc.free(nodes);
			},
			.func_expand => |nodes| {
				for (nodes) |n| n.deinit(alloc);
				alloc.free(nodes);
			},
			.literal => {},
		}
	}
};

/// An intermediate representation node.
pub const IRNode = struct {
	const Data = union(enum) {
		/// Pushes some named value. The string is owned by the node, but this may change in the future.
		push_named: Name,
		/// Pushes a variant. The variant is not owned by any environment, and
		/// should not be of any ref-counted type.
		push: Variant,
		/// A match block. For example: `(a b: b a | (2=): 2)`
		/// Stores each match case.
		match: []MatchCase,
		/// Stores the code of a function.
		func: []IRNode,
		/// Affects the program, but not directly. May affect compliation in various ways.
		directive: Directive,
		/// Basic instruction to pop the top value.
		pop,
		call,
		tail_call,
		push_own_func,
	};

	/// Contains the match names/checks, and the body of the case.
	pub const MatchCase = struct {[]MatchCheckNode, []IRNode};

	root: Token,
	data: Data,
	// /// Optional optimization data, which may or may not be present on the node.
	//opt_data: ?*union {
		//match: struct {

		//},
	//},

	/// Utility function that deinitis a MatchCase.
	pub fn deinitCase(case: MatchCase, alloc: std.mem.Allocator) void {
		const checks, const body = case;
		for (checks) |c| c.deinit(alloc);
		for (body) |ir| ir.deinit(alloc);
		alloc.free(checks);
		alloc.free(body);
	}

	/// Utility function that deinits an array of nodes, and frees them.
	pub fn deinitArr(nodes: []IRNode, alloc: std.mem.Allocator) void {
		for (nodes) |n| n.deinit(alloc);
		alloc.free(nodes);
	}

	pub fn deinit(self: IRNode, alloc: std.mem.Allocator) void {
		switch (self.data) {
			.push_named => |name| alloc.free(name.text),
			.push => |v| v.dec(alloc),
			.match => |cases| {
				for (cases) |case| deinitCase(case, alloc);
				alloc.free(cases);
			},
			.func => |nodes| deinitArr(nodes, alloc),
			.directive => {},
			.pop, .call, .tail_call, .push_own_func => {},
		}
	}

	pub fn init(root: Token, data: Data) IRNode {
		return .{
			.root = root,
			.data = data,
		};
	}
};