const std = @import("std");
const Parser = @import("../text/parser.zig");
const Charset = @import("../text/charset.zig");

const whitespace_chars = Charset.set(" \n\r\t");
const name_chars = Charset.set(whitespace_chars, '(', ')').inv();

pub const Node = struct {
	/// The name of the node. A slice from the text passed into `parse`.
	name: []const u8,
	children: ?[]Node,

	pub fn deinit(self: Node, alloc: std.mem.Allocator) void {
		if (self.children) |children| {
			for (children) |child| child.deinit(alloc);
			alloc.free(children);
		}
	}
};

pub fn parseP(parser: *Parser, alloc: std.mem.Allocator) error {OutOfMemory}!Node {
	_ = parser.eatIn(whitespace_chars);
	const name = parser.eatIn(name_chars);
	_ = parser.eatIn(whitespace_chars);
	if (parser.nextc() != '(') return .{.name = name, .children = null};

	parser.advance(1);
	var children: std.ArrayList(Node) = .{};
	errdefer children.deinit(alloc);

	while (parser.valid() and parser.nextc() != ')') {
		try children.append(alloc, try parseP(parser, alloc));
	}

	parser.advance(1);
	_ = parser.eatIn(whitespace_chars);

	return .{.name = name, .children = try children.toOwnedSlice(alloc)};
}

/// Parses text into a node tree. Examples of node trees:
/// ```
/// node_name
/// node_name (child_node_name child_node_name)
/// block (name name thingy block (a b c d(e))
/// ```
pub fn parse(text: []const u8, alloc: std.mem.Allocator) !Node {
	var parser = Parser {.text = text};
	return parseP(&parser, alloc);
}
