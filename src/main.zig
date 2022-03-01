const std = @import("std");

const parser = @import("parser.zig");
const Parser = parser.Parser;

const err = @import("error.zig");
const ErrMsg = err.ErrMsg;

pub fn main() !void {
    const source =
        \\1 * 2 + 3 / 4;
        \\-5
        \\
    ;

    const Gpa = std.heap.GeneralPurposeAllocator(.{});
    var gpa = Gpa{};

    var p = try Parser.init(gpa.allocator(), source, "<SOURCE>");
    const nodes = p.parse() catch |err| {
        switch (err) {
            error.TokenizerError => {
                const err_msg = p.tokenizer.err_msg;
                std.debug.print("{}\n", .{err_msg});
            },
            error.ParserError => {
                const err_msg = p.err_msg;
                std.debug.print("{}\n", .{err_msg});
            },
            else => {
                std.debug.print("Error: {}\n", .{err});
            },
        }

        return;
    };

    for (nodes.items) |node| {
        std.debug.print("{}\n\n", .{node});
    }
}
