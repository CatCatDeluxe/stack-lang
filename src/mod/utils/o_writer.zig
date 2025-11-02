//! Provides an interface for writing generic structs to something, probably an array.
const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Error = std.Io.Writer.Error || Allocator.Error;

// TODO: refactor to writable and indexable interfaces
pub fn ObjWriter(T: type) type {
	return struct {
		pub const WriteError = Error;
		const FAdd = fn(o: *anyopaque, n: usize) Error![]T;
		const FUnadd = fn(o: *anyopaque) ?T;

		ref: *anyopaque,
		i_add: *const FAdd,
		i_unadd: *const FUnadd,

		/// Adds a pointer. This pointer is only guaranteed to be valid until another element is added.
		pub fn add(self: @This()) Error!*T {
			const slice = try self.i_add(self.ref, 1);
			return &slice[0];
		}

		/// If add() was just called, undoes the operation.
		/// If add() is called twice, this is only required to work the first time.
		pub fn unadd(self: @This()) T {
			return self.i_unadd(self.ref).?;
		}

		pub fn write(self: @This(), t: T) Error!void {
			(try self.add()).* = t;
		}

		pub fn addMany(self: @This(), n: usize) Error![]T {
			return try self.i_add(self.ref, n);
		}

		pub fn writeAll(self: @This(), items: []const T) Error!void {
			const slice: []T = try self.addMany(items.len);
			for (slice, 0..) |_, i| slice[i] = items[i];
		}

		pub fn from(
			ref: *anyopaque,
			i_add: *const FAdd,
			i_unadd: *const FUnadd,
		) @This() {
			return .{
				.ref = ref,
				.i_add = @ptrCast(i_add),
				.i_unadd = @ptrCast(i_unadd),
			};
		}

		/// Creates an ObjWriter from an ArrayList (or anything else that uses addOne() as a method to add one)
		pub fn from_arr(ref: anytype) @This() {
			return .{
				.ref = ref,
				.i_add = @ptrCast(&@TypeOf(ref.*).addManyAsSlice),
				.i_unadd = @ptrCast(&@TypeOf(ref.*).pop),
			};
		}
	};
}