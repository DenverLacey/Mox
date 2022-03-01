const std = @import("std");
const CodeLocation = @import("parser.zig").CodeLocation;

pub const ErrMsg = struct {
    loc: ?CodeLocation,
    msg: []const u8,

    const Self = @This();

    pub fn format(self: *const Self, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        if (std.mem.eql(u8, fmt, "?")) {
            unreachable;
        } else {
            if (self.loc) |loc| {
                try writer.print("{}: Error: {s}", .{ loc, self.msg });
            } else {
                try writer.print("Error: {s}", .{self.msg});
            }
        }
    }
};

pub fn raise(err: anytype, loc: ?CodeLocation, msg: []const u8, out: *ErrMsg) @TypeOf(err) {
    out.* = ErrMsg{ .loc = loc, .msg = msg };
    return err;
}
