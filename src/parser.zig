const std = @import("std");

pub const CodeLocation = struct {
    line: usize,
    col: usize,
    file: []const u8,

    pub fn new(line: usize, col: usize, file: []const u8) This {
        return This{ .line = line, .col = col, .file = file };
    }

    pub fn format(self: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{}:{}:{s}", .{ self.line, self.col, self.file });
    }

    const This = @This();
};

pub const Token = struct {
    data: TokenData,
    location: CodeLocation,

    pub fn new(data: TokenData, location: CodeLocation) This {
        return This{ .data = data, .location = location };
    }

    fn precedence(self: *Token) TokenPrecedence {
        return switch (self.data) {};
    }

    pub fn format(self: *const This, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        if (std.mem.eql(u8, fmt, "?")) {
            try writer.print("Token {{ .data: {}, .location: {} }}", .{ self.data, self.location });
        } else {
            switch (self.data) {
                // Literals
                .Bool => |value| {
                    const value_as_string: []const u8 = if (value) "true" else "false";
                    try writer.print("{s}", .{value_as_string});
                },
                .Int => |value| try writer.print("{}", .{value}),
                .Num => |value| try writer.print("{d}", .{value}),
                .Str => |value| try writer.print("{s}", .{value}),
                .Ident => |ident| try writer.print("{s}", .{ident}),

                // Delimeters
                .Newline => try writer.print("newline", .{}),
                .Comma => try writer.print(",", .{}),
                .Semicolon => try writer.print(";", .{}),
                .LeftParen => try writer.print("(", .{}),
                .RightParen => try writer.print(")", .{}),
                .LeftCurly => try writer.print("{{", .{}),
                .RightCurly => try writer.print("}}", .{}),

                // Operators
                .Plus => try writer.print("+", .{}),
                .Dash => try writer.print("-", .{}),
                .Star => try writer.print("*", .{}),
                .Slash => try writer.print("/", .{}),

                // Keywords
                .If => try writer.print("if", .{}),
                .Else => try writer.print("else", .{}),
                .While => try writer.print("while", .{}),
                .Def => try writer.print("def", .{}),
            }
        }
    }

    const This = @This();
};

pub const TokenKind = enum {
    // Literals
    Bool,
    Int,
    Num,
    Str,
    Ident,

    // Delimeters
    Newline,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,

    // Operators
    Plus,
    Dash,
    Star,
    Slash,

    // Keywords
    If,
    Else,
    While,
    Def,
};

pub const TokenData = union(TokenKind) {
    // Literals
    Bool: bool,
    Int: i64,
    Num: f64,
    Str: []const u8,
    Ident: []const u8,

    // Delimeters
    Newline,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,

    // Operators
    Plus,
    Dash,
    Star,
    Slash,

    // Keywords
    If,
    Else,
    While,
    Def,
};

const TokenPrecedence = enum {
    None,
    Assignment, // = += -= *= /= &= etc.
    Colon, // :
    Cast, // as
    Range, // .. ...
    Or, // ||
    And, // &&
    BitOr, // |
    Xor, // ^
    BitAnd, // &
    Equality, // == !=
    Comparison, // < > <= >=
    Shift, // << >>
    Term, // + -
    Factor, // * / %
    Unary, // ! ~
    Call, // . () []
    Primary,

    fn next(self: TokenPrecedence) TokenPrecedence {
        const primary: u8 = @enumToInt(TokenPrecedence.Primary);
        var p = @enumToInt(self);
        return @intToEnum(TokenPrecedence, if (p + 1 > primary) primary else p + 1);
    }
};
