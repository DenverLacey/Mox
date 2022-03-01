const std = @import("std");
const parser = @import("parser.zig");
const err = @import("error.zig");

const Token = parser.Token;
const Tokenizer = parser.Tokenizer;
const TokenData = parser.TokenData;
const CodeLocation = parser.CodeLocation;

const ErrMsg = err.ErrMsg;

pub fn main() !void {
    const source =
        \\if a + 1 {
        \\	"Hello"
        \\} else {
        \\	"Goodbye"
        \\}
        \\
    ;

    const Gpa = std.heap.GeneralPurposeAllocator(.{});
    var gpa = Gpa{};

    var tokenizer = try Tokenizer.init(source, "<SOURCE>", gpa.allocator());
    while (true) {
        const next = tokenizer.next() catch {
            std.debug.print("{}\n", .{tokenizer.err_msg});
            break;
        };

        if (next) |token| {
            std.debug.print("{?}\n", .{token});
        } else {
            break;
        }
    }
}
