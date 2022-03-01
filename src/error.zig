const std = @import("std");
const CodeLocation = @import("parser.zig").CodeLocation;

pub const ErrMsg = struct {
    loc: ?CodeLocation,
    msg: []const u8,

    const This = @This();

    pub fn default() This {
        return This{ .loc = null, .msg = "" };
    }

    pub fn format(this: *const This, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        if (std.mem.eql(u8, fmt, "?")) {
            unreachable;
        } else {
            if (this.loc) |loc| {
                try writer.print("{}: Error: {s}", .{ loc, this.msg });
            } else {
                try writer.print("Error: {s}", .{this.msg});
            }
        }
    }
};

pub fn raise(err: anytype, loc: ?CodeLocation, msg: []const u8, out: *ErrMsg) @TypeOf(err) {
    out.* = ErrMsg{ .loc = loc, .msg = msg };
    return err;
}

pub fn todo(msg: []const u8) noreturn {
    std.debug.print("Todo: {s}\n", .{msg});
    std.debug.assert(false);
}

pub const TodoError = error{Todo};

pub fn todoAsErr(msg: []const u8) TodoError {
    std.debug.print("Todo: {s}\n", .{msg});
    return TodoError.Todo;
}
