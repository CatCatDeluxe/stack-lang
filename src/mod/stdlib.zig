//! Standard library functions for the language.
const std = @import("std");
const sl = @import("../root.zig");

inline fn require(e: *sl.Env, comptime n: comptime_int) !*[n]sl.Variant {
	const s = e.topStack().items;
	if (s.len < n) return error.StackEmpty;
	return @ptrCast(s[s.len - n .. s.len]);
}

inline fn allAs(comptime n: comptime_int, vars: *[n]sl.Variant, T: type) ![n]*T {
	if (comptime T == f64) {
		var res: [n]*T = undefined;
		for (vars, 0..) |v, i| switch (v) {
			.num => res[i] = &vars[i].num,
			else => return error.TypeError,
		};
		return res;
	}
}

pub const stdlib = struct {
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

	pub fn @"="(e: *sl.Env) !void {
		const args = try require(e, 2);
		e.topStack().items.len -= 2;
		e.topStack().appendAssumeCapacity(.fromPrimitive(args[0].eql(args[1])));
		for (args) |v| v.dec(e.alloc);
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