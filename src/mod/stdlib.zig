//! Standard library functions for the language.
const std = @import("std");
const sl = @import("../root.zig");

inline fn require(e: *sl.Env, comptime n: comptime_int) !*[n]sl.Variant {
	const s = e.topStack().items;
	if (s.len < n) return sl.Env.Error.StackEmpty;
	return @ptrCast(s[s.len - n .. s.len]);
}

inline fn allAs(comptime n: comptime_int, vars: *[n]sl.Variant, T: type) ![n]*T {
	if (comptime T == f64) {
		var res: [n]*T = undefined;
		for (vars, 0..) |v, i| switch (v) {
			.num => res[i] = &vars[i].num,
			else => return sl.Env.Error.TypeError,
		};
		return res;
	}
}

pub const stdlib = struct {
	var stdout_buf: [512]u8 = undefined;

	pub fn @"+"(e: *sl.Env) !void {
		const args = try require(e, 2);
		const params = try allAs(2, args, f64);
		e.topStack().items.len -= 2;
		e.topStack().appendAssumeCapacity(.{.num = params[0].* + params[1].*});
	}

	pub fn @"-"(e: *sl.Env) !void {
		const args = try require(e, 2);
		const params = try allAs(2, args, f64);
		e.topStack().items.len -= 2;
		e.topStack().appendAssumeCapacity(.{.num = params[0].* - params[1].*});
	}

	pub fn @"*"(e: *sl.Env) !void {
		const args = try require(e, 2);
		const params = try allAs(2, args, f64);
		e.topStack().items.len -= 2;
		e.topStack().appendAssumeCapacity(.{.num = params[0].* * params[1].*});
	}

	pub fn @"/"(e: *sl.Env) !void {
		const args = try require(e, 2);
		const params = try allAs(2, args, f64);
		e.topStack().items.len -= 2;
		e.topStack().appendAssumeCapacity(.{.num = params[0].* / params[1].*});
	}

	pub fn @"%"(e: *sl.Env) !void {
		const args = try require(e, 2);
		const params = try allAs(2, args, f64);
		e.topStack().items.len -= 2;
		e.topStack().appendAssumeCapacity(.{.num = @mod(params[0].*, params[1].*)});
	}

	pub fn floor(e: *sl.Env) !void {
		const s = e.topStack();
		if (s.items.len < 1) return sl.Env.Error.StackEmpty;
		switch (s.items[s.items.len - 1]) {
			.num => |n| s.items[s.items.len - 1] = .fromPrimitive(std.math.floor(n)),
			else => return sl.Env.Error.TypeError,
		}
	}

	pub fn @"<"(e: *sl.Env) !void {
		const args = try require(e, 2);
		const params = try allAs(2, args, f64);
		e.topStack().items.len -= 2;
		e.topStack().appendAssumeCapacity(.fromPrimitive(params[0].* < params[1].*));
	}

	pub fn @">"(e: *sl.Env) !void {
		const args = try require(e, 2);
		const params = try allAs(2, args, f64);
		e.topStack().items.len -= 2;
		e.topStack().appendAssumeCapacity(.fromPrimitive(params[0].* > params[1].*));
	}

	pub fn @"="(e: *sl.Env) !void {
		const args = try require(e, 2);
		const res = sl.Variant.fromPrimitive(args[0].eql(args[1]));
		for (args) |v| v.dec(e.alloc);

		e.topStack().items.len -= 2;
		e.topStack().appendAssumeCapacity(res);
	}

	pub fn getch(e: *sl.Env) !void {
		var buf: [1]u8 = undefined;
		var r = std.fs.File.stdin().reader(&.{});
		// can't return error.ReadFailed in the interpreter
		if ((r.read(&buf) catch return error.OutOfMemory) < 1) return error.OutOfMemory;
		try e.topStack().append(e.alloc, .fromPrimitive(buf[0]));
	}

	pub fn putch(e: *sl.Env) !void {
		const args = try require(e, 1);
		switch (args[0]) {
			.num => |n| {
				_ = std.fs.File.stdout().write(&.{std.math.lossyCast(u8, n)}) catch {};
				_ = e.topStack().pop();
				return;
			},
			else => {},
		}
		return error.TypeError;
	}

	pub fn printStack(e: *sl.Env) !void {
		var stdout = std.fs.File.stdout().writer(&stdout_buf);

		const stack = e.topStack();
		for (stack.items, 0..) |*v, i| {
			if (i > 0) _ = stdout.interface.write(", ") catch {};
			stdout.interface.print("{f}", .{v.colorize()}) catch {};
		}
		_ = stdout.interface.write("\n") catch {};
		stdout.interface.flush() catch {};
	}
};

/// Adds the functions declarations from `Lib` to `to`.
pub fn addLibrary(globals: *sl.Constants, Lib: type) !void {
	inline for (comptime std.meta.declarations(Lib)) |decl| {
		const func = @field(Lib, decl.name);
		const Func = @TypeOf(func);
		switch (@typeInfo(Func)) {
			.@"fn" => {},
			else => continue,
		}
		_ = try globals.addNamed(try .from(decl.name), .{.builtin = &func});
	}
}