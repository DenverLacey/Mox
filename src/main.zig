const std = @import("std");
const parser = @import("parser.zig");

const Token = parser.Token;
const Tokenizer = parser.Tokenizer;
const TokenData = parser.TokenData;
const CodeLocation = parser.CodeLocation;

pub fn main() !void {
    const source =
        \\1 + 2
        \\
    ;

    const Gpa = std.heap.GeneralPurposeAllocator(.{});
    var gpa = Gpa{};

    var tokenizer = try Tokenizer.init(source, "<SOURCE>", gpa.allocator());

    while (try tokenizer.next()) |token| {
        std.debug.print("{?}\n", .{token});
    }
}
