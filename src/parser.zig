const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Utf8View = std.unicode.Utf8View;
const Utf8Iterator = std.unicode.Utf8Iterator;

pub const CodeLocation = struct {
    line: usize,
    col: usize,
    file: []const u8,

    const Self = @This();

    pub fn init(line: usize, col: usize, file: []const u8) Self {
        return Self{ .line = line, .col = col, .file = file };
    }

    pub fn format(self: *const Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{}:{}:{s}", .{ self.line + 1, self.col + 1, self.file });
    }
};

pub const Token = struct {
    data: TokenData,
    location: CodeLocation,

    const Self = @This();

    pub fn init(data: TokenData, location: CodeLocation) Self {
        return Self{ .data = data, .location = location };
    }

    fn precedence(self: *Token) TokenPrecedence {
        return switch (self.data) {
            // Literals
            .Bool => TokenPrecedence.None,
            .Int => TokenPrecedence.None,
            .Num => TokenPrecedence.None,
            .Str => TokenPrecedence.None,
            .Ident => TokenPrecedence.None,

            // Delimeters
            .Newline => TokenPrecedence.None,
            .Comma => TokenPrecedence.None,
            .Semicolon => TokenPrecedence.None,
            .LeftParen => TokenPrecedence.Call,
            .RightParen => TokenPrecedence.None,
            .LeftCurly => TokenPrecedence.None,
            .RightCurly => TokenPrecedence.None,

            // Operators
            .Plus => TokenPrecedence.Term,
            .Dash => TokenPrecedence.Term,
            .Star => TokenPrecedence.Factor,
            .Slash => TokenPrecedence.Factor,

            // Keywords
            .If => TokenPrecedence.None,
            .Else => TokenPrecedence.None,
            .While => TokenPrecedence.None,
            .Def => TokenPrecedence.None,
        };
    }

    pub fn format(self: *const Self, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
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
        const p = @enumToInt(self);
        return @intToEnum(TokenPrecedence, if (p + 1 > primary) primary else p + 1);
    }
};

pub const ParseError = error{
    // Tokenizer Errors
    UnendedStringLiteral,
    UnknownOperator,
};

pub const Tokenizer = struct {
    source: Utf8Iterator,
    filename: []const u8,
    peeked_tokens: ArrayList(Token),
    previous_was_newline: bool,

    line: usize,
    column: usize,
    token_location: CodeLocation,

    const Self = @This();
    const Char = u21;

    pub fn init(source: []const u8, filename: []const u8, allocator: Allocator) !Self {
        return Self{
            .source = (try Utf8View.init(source)).iterator(),
            .filename = filename,
            .peeked_tokens = ArrayList(Token).init(allocator),
            .previous_was_newline = true, // to skip leading newlines in source file
            .line = 0,
            .column = 0,
            .token_location = CodeLocation.init(0, 0, filename),
        };
    }

    fn bytesRemaining(self: *Self) usize {
        return self.source.bytes.len - self.source.i;
    }

    fn recordTokenLocation(self: *Self) void {
        self.token_location.line = self.line;
        self.token_location.col = self.column;
    }

    fn currentLocation(self: *Self) CodeLocation {
        return CodeLocation.init(self.line, self.column, self.filename);
    }

    fn isIdentBegin(c: Char) bool {
        // @TODO:
        // properly handle utf8.
        //
        const seqLength = std.unicode.utf8CodepointSequenceLength(c) catch 0;
        std.debug.assert(seqLength == 1);
        const ascii = @intCast(u8, c);

        return std.ascii.isAlpha(ascii) or c == '_';
    }

    fn isIdentChar(c: Char) bool {
        if (isIdentBegin(c)) {
            return true;
        }

        // @TODO:
        // properly handle utf8.
        //
        const seqLength = std.unicode.utf8CodepointSequenceLength(c) catch unreachable;
        if (seqLength != 1) {
            return false;
        }
        const ascii = @intCast(u8, c);

        return std.ascii.isDigit(ascii);
    }

    fn isStringBegin(c: Char) bool {
        return c == '"' or c == '\'';
    }

    // @TODO:
    // Handle UTF8.
    //
    fn isWhitespace(c: Char) bool {
        const seqLength = std.unicode.utf8CodepointSequenceLength(c) catch unreachable;
        if (seqLength != 1) {
            return false;
        }
        const ascii = @intCast(u8, c);

        return std.ascii.isSpace(ascii);
    }

    fn isDigit(c: Char) bool {
        return c >= '0' and c <= '9';
    }

    fn peekChar(self: *Self) ?Char {
        if (self.bytesRemaining() == 0) {
            return null;
        }

        const peeked = self.source.peek(1);
        return std.unicode.utf8Decode(peeked) catch |e| blk: {
            switch (e) {
                error.Utf8CodepointTooLarge => std.log.err("{}", .{e}),
                error.Utf8EncodesSurrogateHalf => std.log.err("{}", .{e}),
                error.Utf8ExpectedContinuation => std.log.err("{}", .{e}),
                error.Utf8OverlongEncoding => std.log.err("{}", .{e}),
            }
            break :blk null;
        };
    }

    fn nextChar(self: *Self) ?Char {
        const maybe_next_char = self.source.nextCodepoint();
        if (maybe_next_char) |c| {
            if (c == '\n') {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }
        return maybe_next_char;
    }

    fn nextCharIf(self: *Self, f: fn (Char) bool) ?Char {
        const peeked = self.peekChar();
        if (peeked) |c| {
            if (!f(c)) {
                return null;
            }
        }
        return self.nextChar();
    }

    fn nextCharIfEqual(self: *Self, c: Char) ?Char {
        const peeked = self.peekChar();
        if (peeked) |pc| {
            if (c != pc) {
                return null;
            }
        }
        return self.nextChar();
    }

    pub fn peek(self: *Self) !?Token {
        if (self.peeked_tokens.items.len == 0) {
            const maybe_token = try self.next();
            if (maybe_token) |t| {
                try self.peeked_tokens.append(t);
            } else {
                return null;
            }
        }
        return self.peeked_tokens.items[0];
    }

    pub fn peekN(self: *Self, n: usize) !?Token {
        while (self.peeked_tokens.items.len <= n) {
            if (try self.nextNoPeeking()) |t| {
                try self.peeked_tokens.append(t);
            } else {
                return null;
            }
        }
        return self.peeked_tokens.items[n];
    }

    pub fn next(self: *Self) ParseError!?Token {
        if (self.peeked_tokens.items.len != 0) {
            return self.peeked_tokens.orderedRemove(0);
        }
        return try self.nextNoPeeking();
    }

    fn nextNoPeeking(self: *Self) ParseError!?Token {
        self.skipToBeginningOfNextToken();
        self.recordTokenLocation();
        self.previous_was_newline = false;

        var token: ?Token = undefined;
        if (self.peekChar()) |c| {
            if (c == 0) {
                token = null;
            } else if (c == '\n') {
                self.previous_was_newline = true;
                _ = self.nextChar();
                token = Token.init(TokenData.Newline, self.token_location);
            } else if (isDigit(c)) {
                token = self.tokenizeNumber();
            } else if (isStringBegin(c)) {
                token = try self.tokenizeString();
            } else if (isIdentBegin(c)) {
                token = self.tokenizeIdentOrKeyword();
            } else {
                token = try self.tokenizePunctuation();
            }
        } else {
            return null;
        }

        return token;
    }

    fn skipToBeginningOfNextToken(self: *Self) void {
        while (true) {
            if (self.peekChar()) |c| {
                switch (c) {
                    '#' => while (true) {
                        if (self.nextChar()) |next_char| {
                            if (next_char == '\n') {
                                break;
                            }
                        } else {
                            break;
                        }
                    },
                    '\n' => {
                        if (!self.previous_was_newline) {
                            break;
                        }

                        _ = self.nextChar();
                    },
                    else => {
                        if (!isWhitespace(c)) {
                            break;
                        }
                        _ = self.nextChar();
                    },
                }
            } else {
                break;
            }
        }
    }

    fn tokenizeNumber(self: *Self) Token {
        const start_index = self.source.i;
        var end_index: usize = start_index;
        while (self.nextCharIf(isDigit)) |_| {
            end_index = self.source.i;
        }

        const reset_index = self.source.i;
        if (self.nextCharIfEqual('.')) |_| {
            if (self.peekChar()) |c| {
                if (!isDigit(c)) {
                    self.source.i = reset_index;
                } else {
                    while (self.nextCharIf(isDigit)) |_| {
                        end_index = self.source.i;
                    }

                    const word = self.source.bytes[start_index..end_index];
                    return Token.init(TokenData{ .Num = std.fmt.parseFloat(f64, word) catch unreachable }, self.token_location);
                }
            }
        }

        const word = self.source.bytes[start_index..end_index];
        return Token.init(TokenData{ .Int = std.fmt.parseInt(i64, word, 10) catch unreachable }, self.token_location);
    }

    // @TODO:
    // Handled escape sequences.
    //
    fn tokenizeString(self: *Self) ParseError!Token {
        const terminator = self.nextChar().?;

        const start_index = self.source.i;
        var end_index = start_index;

        while (true) {
            if (self.nextChar()) |c| {
                if (c == terminator) {
                    break;
                }
                end_index = self.source.i;
            } else {
                return ParseError.UnendedStringLiteral;
            }
        }

        const word = self.source.bytes[start_index..end_index];
        return Token.init(TokenData{ .Str = word }, self.token_location);
    }

    fn tokenizeIdentOrKeyword(self: *Self) Token {
        const start_index = self.source.i;
        var end_index = self.source.i;
        while (self.nextCharIf(isIdentChar)) |_| {
            end_index = self.source.i;
        }

        const word = self.source.bytes[start_index..end_index];
        return if (std.mem.eql(u8, word, "if"))
            Token.init(TokenData.If, self.token_location)
        else if (std.mem.eql(u8, word, "else"))
            Token.init(TokenData.Else, self.token_location)
        else if (std.mem.eql(u8, word, "while"))
            Token.init(TokenData.While, self.token_location)
        else if (std.mem.eql(u8, word, "def"))
            Token.init(TokenData.Def, self.token_location)
        else
            Token.init(TokenData{ .Ident = word }, self.token_location);
    }

    fn tokenizePunctuation(self: *Self) ParseError!Token {
        return switch (self.nextChar().?) {
            ',' => Token.init(TokenData.Comma, self.token_location),
            ';' => Token.init(TokenData.Semicolon, self.token_location),
            '(' => Token.init(TokenData.LeftParen, self.token_location),
            ')' => Token.init(TokenData.RightParen, self.token_location),
            '{' => Token.init(TokenData.LeftCurly, self.token_location),
            '}' => Token.init(TokenData.RightCurly, self.token_location),
            '+' => Token.init(TokenData.Plus, self.token_location),
            '-' => Token.init(TokenData.Dash, self.token_location),
            '*' => Token.init(TokenData.Star, self.token_location),
            '/' => Token.init(TokenData.Slash, self.token_location),
            else => ParseError.UnknownOperator,
        };
    }
};
