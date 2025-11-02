pub const Token = @import("token.zig");
pub const Node = @import("node.zig").Node;

const std = @import("std");
const Scanner = @import("../text/scanner.zig");
const Charset = @import("../text/charset.zig");
const o_writer = @import("../utils/o_writer.zig");
const ObjWriter = o_writer.ObjWriter;
const Directive = @import("../cross/directive.zig").Directive;

/// Some kind of syntax error.
pub const SyntaxError = error {InvalidCharacter, InvalidClosingBracket, UnknownDirective};

/// parses as many tokens as possible from `s` and puts them in `w`.
/// returns whether there are more tokens to parse or not.
/// always stops after reading a non-basic token. returns the token
/// that this function could not handle.
fn eatBasicExpr(s: *Scanner, w: ObjWriter(Node)) !Token {
	while (true) {
		_ = s.eatIn(Token.chars_whitespace);
		const tk = Token.read(s.*) orelse return SyntaxError.InvalidCharacter;
		switch (tk.type) {
			.comment => {
				tk.eat(s);
				continue;
			},
			.directive => {
				tk.eat(s);
				try w.write(.{.directive = .{tk, try eatDirective(tk, s)}});
				continue;
			},
			// All token types included in a basic expression
			.name_operator, .name_regular, .number, .sym_call,
			.sym_call_cond, .sym_this_func, .sym_dot, .symbol => {},
			else => return tk,
		}
		try w.write(.{.basic = tk});
		tk.eat(s);
	}
}

const ChildArr = std.array_list.Managed(Node);

fn arrDeinit(arr: anytype, alloc: std.mem.Allocator) void {
	for (arr.items) |n| n.deinit(alloc);
	arr.deinit();
}

fn nodeWriter(arr: *ChildArr) ObjWriter(Node) {
	return ObjWriter(Node).from_arr(arr);
}

/// reads as many basic tokens, and round- and curly- bracketed blocks, as
/// possible from `s`. returns when the next token cannot be handled by a
/// simple parser. returns the token that could not be handled. non-handleable
/// token types include EOF, `|`, `:`, and closing brackets not used up by a
/// contained block.
fn parseSimpleExpr(s: *Scanner, w: ObjWriter(Node), alloc: std.mem.Allocator) !Token {
	while (true) {
		// the token that ended the basic expr.
		const tk = try eatBasicExpr(s, w);
		switch (tk.type) {
			.lbracket, .lcurly => { // open a contained block
				tk.eat(s);

				var children = ChildArr.init(alloc);
				const child_w = nodeWriter(&children);
				errdefer arrDeinit(&children, alloc);

				const block_end_tk = try parseBlock(s, child_w, alloc);
				const node =
					if (tk.type == .lbracket) Node {.block = .{tk, try children.toOwnedSlice()}}
					else Node {.func_block = .{tk, try children.toOwnedSlice()}};
				errdefer node.deinit(alloc);

				// verify the end token for the block is correct
				if (block_end_tk.type != tk.type.counterpart())	return SyntaxError.InvalidClosingBracket;
				block_end_tk.eat(s); // consume the block's ending token

				try w.write(node);
			},
			else => return tk,
		}
	}
}

/// parses a single match. if `names` is not null, starts the match from the
/// right of its colon and uses the array as its name-block.
/// Returns the parsed match, and the token that caused parsing to end, and
/// whether a colon was used in the text of the match.
fn parseMatch(s: *Scanner, alloc: std.mem.Allocator, names: ?[]Node) !struct { Node.Match, Token, bool } {
	var has_names = names != null;
	var res = Node.Match {names orelse &.{}, &.{}};

	// no handling necessary for res[1] because it is only assigned at the end
	// of the function
	errdefer {
		for (res[0]) |n| n.deinit(alloc);
		alloc.free(res[0]);
	}

	var temp = ChildArr.init(alloc);
	const temp_w = nodeWriter(&temp);
	defer arrDeinit(&temp, alloc);

	const end: Token = while (true) {
		const end_tk = try parseSimpleExpr(s, temp_w, alloc);
		switch (end_tk.type) {
			.sym_colon => {
				// TODO make another colon start another match
				if (has_names) return SyntaxError.InvalidCharacter;
				res[0] = try temp.toOwnedSlice();
				has_names = true;
				end_tk.eat(s);
			},
			else => break end_tk,
		}
	};

	res[1] = try temp.toOwnedSlice();
	return .{res, end, has_names};
}

/// parses a block of code from `s` and writes the resulting AST nodes to `w`.
/// Returns the token that caused parsing to end.
pub fn parseBlock(s: *Scanner, w: ObjWriter(Node), alloc: std.mem.Allocator) (SyntaxError || o_writer.Error)!Token {
	var match_list = std.array_list.Managed(Node.Match).init(alloc);
	errdefer {
		for (match_list.items) |m| Node.matchDeinit(m, alloc);
		match_list.deinit();
	}

	const end: Token = while (true) {
		const match, const end_tk, const used_colon = try parseMatch(s, alloc, null); // parse a match, ex. `a b c(1=) : a a b c`

		// The text "
		// (
		// | a b: 2
		// | a: 1
		// | 0
		// )"
		// would technically contain an empty match right at the start (
		// between the '(' and the first '|'), causing that branch to always
		// be chosen. This is stupid, so completely empty matches are ignored.
		// However, if you want to add a totally empty branch, you can either
		// give it an empty block: "(a: ... | ())", or add a colon: "(a: ... |:)".
		// Both would evaluate to the same IR eventually, so it's up to the
		// programmer. Personally, I prefer adding a colon, which is why this
		// code exists.
		if (match[0].len > 0 or match[1].len > 0 or used_colon) {
			try match_list.append(match);
		}

		switch (end_tk.type) {
			.sym_pipe => end_tk.eat(s),
			else => break end_tk,
		}
	};

	// no point in adding a completely empty match
	if (match_list.items.len == 0) return end;

	// the only match is a simple block -- return its contents in a simpler token
	// assumes that if the name-block's length is 0 it does not need to be freed
	if (match_list.items.len == 1 and match_list.items[0][0].len == 0) {
		const children = match_list.items[0][1];
		defer match_list.deinit();
		defer alloc.free(children);

		for (children, 0..) |n, completed| {
			w.write(n) catch |err| {
				// since we can't un-write, only destroy the tokens that haven't
				// been written to w, and then exit.
				for (completed..children.len) |i| children[i].deinit(alloc);
				return err;
			};
		}

		return end;
	}

	var match_block = Node {.match = try match_list.toOwnedSlice()};
	errdefer match_block.deinit(alloc);
	try w.write(match_block);

	return end;
}

/// Parses a directive from the stream. `s` should be positioned right after
/// the directive.
fn eatDirective(dir: Token, s: *Scanner) !Directive {
	if (std.mem.eql(u8, dir.text, "breakpoint")) {
		return .{.breakpoint = {}};
	}
	// some directives may need additional tokens
	_ = s;
	return error.UnknownDirective;
}

/// Parses an entire file's text.
pub fn parseText(text: []const u8, alloc: std.mem.Allocator, errs: *@import("../error_list.zig")) (SyntaxError || error {OutOfMemory})![]Node {
	var arr = std.array_list.Managed(Node).init(alloc);
	const writer = ObjWriter(Node).from_arr(&arr);
	var scanner = Scanner {.text = text};

	const end_token = parseBlock(&scanner, writer, alloc) catch |err| switch (err) {
		error.WriteFailed => unreachable,
		else => |e| {
			errs.pushError(.err, scanner.state, "Parse error: {s}", .{@errorName(err)});
			arr.deinit();
			return e;
		}
	};

	if (end_token.type != .end) {
		errs.pushError(.err, end_token.position, "Parser: Unexpected token '{s}' ({s})", .{end_token.text, @tagName(end_token.type)});
		arr.deinit();
		return error.InvalidCharacter;
	}

	return arr.toOwnedSlice();
}