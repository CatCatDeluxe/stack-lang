const std = @import("std");
const Env = @import("../interpreter/interpreter.zig");
const Symbols = @import("symbols.zig");

/// A basic implementation of a ref-counted pointer.
/// Does not do anything about cyclic references.
pub fn Rc(T: type) type {
	return *struct {
		val: T,
		refcount: u32,

		/// Increases the reference count. Use this when storing the refc
		/// somewhere it will later be removed from.
		/// Returns the same pointer that was passed in.
		pub fn inc(self: *@This()) *@This() {
			self.refcount += 1;
			return self;
		}

		/// Decreases the reference count. If the count reaches 0, returns a
		/// copy of the contained instance, and frees the pointer to this
		/// instance.
		pub fn dec(self: *@This(), alloc: std.mem.Allocator) ?T {
			if (self.refcount <= 1) {
				const res = self.val;
				alloc.destroy(self);
				return res;
			}
			self.refcount -= 1;
			return null;
		}
	};
}

fn RcInner(T: type) type {
	const Res = Rc(T);
	return @typeInfo(Res).pointer.child;
}

pub fn rc(val: anytype, alloc: std.mem.Allocator) !Rc(@TypeOf(val)) {
	const res = try alloc.create(RcInner(@TypeOf(val)));
	res.* = .{.val = val, .refcount = 1};
	return res;
}

/// A variant. May be used to store a value at compile-time, or be used in the
/// interpreter. For simplicity, the same type is used to store values at
/// compile-time and at runtime.
/// Used across various compilation steps.
pub const Variant = union(enum) {
	/// Just a number.
	num: f64,
	symbol: Symbols.SymbolID,
	/// A reference to a function in the current environment.
	function_ref: u32,
	/// An instance of a function, including captures. The memory is managed
	/// by the current environment.
	function_instance: struct {
		id: u32,
		captures: Rc([]Variant),
	},
	/// A builtin function. Modifies an interpreter's environment directly.
	builtin: *const fn(env: *Env) (Env.InterpreterError || error {OutOfMemory})!void,

	/// If the variant is not a `function_ref`, panics.
	/// Copies `captures` into the function's own memory.
	/// Does not increment the captured variants.
	pub fn withCaptures(self: Variant, alloc: std.mem.Allocator, captures: []const Variant) !Variant {
		const id = self.function_ref;
		const newSlice = try alloc.dupe(Variant, captures);
		errdefer alloc.free(newSlice);

		return .{.function_instance = .{
			.id = id,
			.captures = try rc(newSlice, alloc),
		}};
	}

	pub fn fromPrimitive(val: anytype) Variant {
		return switch (@typeInfo(@TypeOf(val))) {
			.bool => .{.num = if (val) 1 else 2},
			.int, .comptime_int => .{.num = @floatFromInt(val)},
			.float, .comptime_float => .{.num = val},
			else => @compileError("Cannot create a variant from type " ++ @typeName(@TypeOf(val))),
		};
	}

	pub fn format(self: @This(), writer: *std.Io.Writer) !void {
		switch (self) {
			.num => |num| try writer.print("{}", .{num}),
			.function_ref => |id| try writer.print("fn {}", .{id}),
			.function_instance => |data| {
				try writer.print("fn {} [captures ({}): ", .{data.id, data.captures.val.len});
				for (data.captures.val, 0..) |*capture, i| {
					if (i > 0) _ = try writer.write(", ");
					try writer.print("({f})", .{capture});
				}
				_ = try writer.write("]");
			},
			.builtin => |func| try writer.print("fn [builtin@{}]", .{@intFromPtr(func)}),
			.symbol => |id| try writer.print("(sym #{})", .{id}),
		}
	}

	pub fn eql(a: @This(), b: @This()) bool {
		if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
		switch (a) {
			.num => |num| return num == b.num,
			.function_ref => |id| return id == b.function_ref,
			.function_instance => |data| {
				if (data.id != b.function_instance.id) return false;
				if (data.captures.val.len != b.function_instance.captures.val.len) return false;
				for (data.captures.val, b.function_instance.captures.val) |ca, cb| {
					if (!ca.eql(cb)) return false;
				}
				return true;
			},
			.builtin => |func| return func == b.builtin,
			.symbol => |id| return id == b.symbol,
		}
	}

	pub fn inc(self: @This()) @This() {
		switch (self) {
			.function_instance => |f| _ = f.captures.inc(),
			else => {}
		}
		return self;
	}

	pub fn dec(self: @This(), alloc: std.mem.Allocator) void {
		switch (self) {
			.function_instance => |func| {
				if (func.captures.dec(alloc)) |captures| {
					for (captures) |v| v.dec(alloc);
					alloc.free(captures);
				}
			},
			else => {},
		}
	}

	pub fn truthy(self: @This()) bool {
		switch (self) {
			.num => |num| return num != 0,
			.function_ref => return true,
			.function_instance => return true,
			.builtin => return true,
			.symbol => return true,
		}
	}

	pub fn colorize(self: *const @This()) Colorize {
		return .{.variant = self};
	}
};

const Colorize = struct {
	variant: *const Variant,
	pub fn format(container: @This(), w: *std.Io.Writer) !void {
		const self = container.variant;
		switch (self.*) {
			.num => |num| try w.print("\x1b[33m{}\x1b[0m", .{num}),
			.function_ref => |id| try w.print("fn\x1b[32;2m#\x1b[0;32m{}\x1b[0m", .{id}),
			.function_instance => |inst| {
				try w.print("fn\x1b[32;2m#\x1b[0;32m{}\x1b[0m {{", .{inst.id});
				for (inst.captures.val, 0..) |capture, i| {
					if (i > 0) try w.print(", ", .{});
					try w.print("{f}", .{capture.colorize()});
				}
				try w.print("}}", .{});
			},
			.symbol => |id| try w.print("sym\x1b[2;32m#\x1b[0;32m{}\x1b[0m", .{id}),
			.builtin => |ptr| try w.print("builtin@{}", .{@intFromPtr(ptr)}),
		}
	}
};