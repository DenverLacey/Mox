const std = @import("std");
const parser = @import("parser.zig");

const Token = parser.Token;
const TokenData = parser.TokenData;
const CodeLocation = parser.CodeLocation;

pub fn main() anyerror!void {
    const token = Token.new(TokenData{ .Int = 69 }, CodeLocation.new(0, 0, "NOWHERE.txt"));
    std.log.info("{?}", .{token});
}
