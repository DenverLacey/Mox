const std = @import("std");

const parser = @import("parser.zig");
const Parser = parser.Parser;

const evaluator = @import("evaluator.zig");
const Evaluator = evaluator.Evaluator;

const err = @import("error.zig");
const ErrMsg = err.ErrMsg;

pub fn main() !void {
    const Gpa = std.heap.GeneralPurposeAllocator(.{});
    var gpa = Gpa{};
    var allocator = gpa.allocator();

    var args = std.process.args();
    allocator.free(try (args.next(allocator).?));
    var filename: [:0]const u8 = undefined;
    if (args.next(allocator)) |next| {
        filename = try next;
    } else {
        std.debug.print("Error: No file given to compile.\n", .{});
        return;
    }
    defer allocator.free(filename);

    const source = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
    defer allocator.free(source);

    var p = try Parser.init(allocator, source, filename);
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

    var e = try Evaluator.init(allocator);
    defer e.deinit();

    e.evaluate(nodes) catch |err| {
        switch (err) {
            error.InvalidOperation, error.TypeMismatch => {
                const err_msg = e.err_msg;
                std.debug.print("{}\n", .{err_msg});
            },
            else => {
                std.debug.print("Error: {}\n", .{err});
            },
        }

        return;
    };
}
