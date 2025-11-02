line: u32,
col: u32,

/// The value for an invalid location. Locations with a line and col of 0 are
/// invalid, since as locations are meant to be displayed to the user, line
/// and column numbers start at 1.
pub const invalid = @This() {.line = 0, .col = 0};

pub fn is_invalid(self: @This()) bool { return self.line == 0 or self.col == 0; }