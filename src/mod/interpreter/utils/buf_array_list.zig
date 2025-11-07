const std = @import("std");
const Allocator = std.mem.Allocator;

fn CopyConst(From: type, To: type) type {
	switch (@typeInfo(From)) {
		.pointer => |p| {
			switch (@typeInfo(To)) {
				.pointer => |to_p| {
					var res = to_p;
					res.is_const = p.is_const;
					return res;
				},
				else => @compileError(std.fmt.comptimePrint("Cannot copy const from {} to {}", .{From, To})),
			}
		},
		else => @compileError(std.fmt.comptimePrint("Cannot copy const from {}", .{From})),
	}
}

/// Similar to an array list, but uses an internal buffer to store items if
/// there is a small enough count of them.
pub fn BufArrayList(T: type, base_cap: comptime_int) type {
	return struct {
		/// The short buffer, to store some items without allocating anything
		short: [base_cap]T,
		/// The long buffer for additional memory
		long: ?[*]T,
		/// The number of items in the list
		len: u16,
		/// The total capacity of the list, including the short buffer.
		cap: u16,

		pub const empty = @This() {
			.short = undefined,
			.long = null,
			.len = 0,
			.cap = base_cap,
		};

		pub fn initWithCap(alloc: Allocator, initial_cap: usize) !@This() {
			return .{
				.short = undefined,
				.long =
					if (initial_cap > base_cap) (try alloc.alloc(T, initial_cap - base_cap)).ptr
					else null,
				.cap = @intCast(@max(initial_cap, base_cap)),
				.len = 0,
			};
		}

		/// Returns the two slices of this array.
		/// The first slice is a slice of the short buffer, and the second is
		/// the long buffer. If the array is short enough, either the long slice
		/// or both slices may be empty.
		pub fn slices(self: *@This()) [2][]T {
			return .{
				(&self.short)[0..@min(self.len, base_cap)],
				self.long[0..@max(self.len, base_cap) - base_cap],
			};
		}

		pub fn from(alloc: Allocator, slice: []const T) !@This() {
			var res = try initWithCap(alloc, slice.len);
			res.len = @intCast(slice.len);
			@memcpy(res.short[0..@min(slice.len, base_cap)], slice[0..@min(base_cap, slice.len)]);
			if (slice.len > base_cap) {
				@memcpy(res.long.?[0..(slice.len - base_cap)], slice[base_cap..]);
			}
			return res;
		}

		pub inline fn get(self: *@This(), index: usize) *T {
			return
				if (index < base_cap) &self.short[index]
				else &self.long.?[index - base_cap];
		}

		pub fn setCap(self: *@This(), alloc: Allocator, cap: usize) !void {
			if (cap <= base_cap) {
				if (self.long) |arr| alloc.free(arr[0..self.cap - base_cap]);
				self.long = null;
				self.cap = base_cap;
				return;
			}
			const new_slice: []T =
				if (self.long) |arr| try alloc.realloc(arr[0..self.cap - base_cap], cap - base_cap)
				else try alloc.alloc(T, cap - base_cap);
			self.long = new_slice.ptr;
			self.cap = @intCast(cap);
		}

		pub fn addOne(self: *@This(), alloc: Allocator) !*T {
			if (self.len >= self.cap) {
				try self.setCap(alloc, self.cap * 2);
			}
			self.len += 1;
			return self.get(self.len - 1);
		}

		pub fn add(self: *@This(), alloc: Allocator, item: T) !void {
			(try self.addOne(alloc)).* = item;
		}

		pub fn pop(self: *@This()) ?T {
			if (self.len == 0) return null;
			self.len -= 1;
			return self.get(self.len).*;
		}

		pub fn last(self: *@This()) *T {
			return self.get(self.len - 1);
		}

		pub fn clear(self: *@This(), alloc: Allocator) void {
			self.deinit(alloc);
			self.long = null;
			self.len = 0;
			self.cap = base_cap;
		}

		/// Deinitializes the array. Leaves it in an undefined state.
		pub fn deinit(self: @This(), alloc: Allocator) void {
			if (self.long) |arr| alloc.free(arr[0..(self.cap - base_cap)]);
		}
	};
}