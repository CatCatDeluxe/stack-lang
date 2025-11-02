pub fn AnyScanner(T: type) type {
	return struct {
		data: []T,
		position: usize,

		pub fn valid(self: @This()) bool {
			return self.position < self.data.len;
		}

		pub fn next(self: @This()) ?*T {
			if (self.position < self.data.len) return &self.data[self.position];
			return null;
		}

		pub fn advance(self: *@This(), n: usize) void {
			self.position += n;
		}

		pub fn init(arr: []T) @This() {
			return .{.data = arr, .position = 0};
		}
	};
}