const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const Utf8View = std.unicode.Utf8View;
const Utf8Iterator = std.unicode.Utf8Iterator;

const ast = @import("ast.zig");
const Ast = ast.Ast;
const AstKind = ast.AstKind;
const AstIdent = ast.AstIdent;
const AstLiteral = ast.AstLiteral;
const AstUnary = ast.AstUnary;
const AstBinary = ast.AstBinary;
const AstBlock = ast.AstBlock;
const AstIf = ast.AstIf;
const AstWhile = ast.AstWhile;
const AstDef = ast.AstDef;
const AstVar = ast.AstVar;
const AstStruct = ast.AstStruct;

const err = @import("error.zig");
const ErrMsg = err.ErrMsg;
const raise = err.raise;
const todo = err.todo;
const todoAsErr = err.todoAsErr;

pub const CodeLocation = struct {
    line: usize,
    col: usize,
    file: []const u8,

    const This = @This();

    pub fn init(line: usize, col: usize, file: []const u8) This {
        return This{ .line = line, .col = col, .file = file };
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{s}:{}:{}", .{ this.file, this.line + 1, this.col + 1 });
    }
};

pub const Token = struct {
    data: TokenData,
    location: CodeLocation,

    const This = @This();

    pub fn init(data: TokenData, location: CodeLocation) This {
        return This{ .data = data, .location = location };
    }

    fn precedence(this: *const Token) TokenPrecedence {
        return switch (this.data) {
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
            .LeftSquare => TokenPrecedence.Call,
            .RightSquare => TokenPrecedence.None,

            // Operators
            .Bang => TokenPrecedence.Unary,
            .BangEqual => TokenPrecedence.Equality,
            .Plus => TokenPrecedence.Term,
            .Dash => TokenPrecedence.Term,
            .Star => TokenPrecedence.Factor,
            .Slash => TokenPrecedence.Factor,
            .Equal => TokenPrecedence.Assignment,
            .DoubleEqual => TokenPrecedence.Equality,

            // Keywords
            .If => TokenPrecedence.None,
            .Else => TokenPrecedence.None,
            .While => TokenPrecedence.None,
            .Def => TokenPrecedence.None,
            .Var => TokenPrecedence.None,
            .Struct => TokenPrecedence.None,
        };
    }

    pub fn format(this: *const This, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        if (std.mem.eql(u8, fmt, "!")) {
            switch (this.data) {
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
                .LeftSquare => try writer.print("[", .{}),
                .RightSquare => try writer.print("]", .{}),

                // Operators
                .Bang => try writer.print("!", .{}),
                .BangEqual => try writer.print("!=", .{}),
                .Plus => try writer.print("+", .{}),
                .Dash => try writer.print("-", .{}),
                .Star => try writer.print("*", .{}),
                .Slash => try writer.print("/", .{}),
                .Equal => try writer.print("=", .{}),
                .DoubleEqual => try writer.print("==", .{}),

                // Keywords
                .If => try writer.print("if", .{}),
                .Else => try writer.print("else", .{}),
                .While => try writer.print("while", .{}),
                .Def => try writer.print("def", .{}),
                .Var => try writer.print("var", .{}),
                .Struct => try writer.print("struct", .{}),
            }
        } else {
            try writer.print("Token {{ .data: {}, .location: {} }}", .{ this.data, this.location });
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
    LeftSquare,
    RightSquare,

    // Operators
    Bang,
    BangEqual,
    Plus,
    Dash,
    Star,
    Slash,
    Equal,
    DoubleEqual,

    // Keywords
    If,
    Else,
    While,
    Def,
    Var,
    Struct,
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
    LeftSquare,
    RightSquare,

    // Operators
    Bang,
    BangEqual,
    Plus,
    Dash,
    Star,
    Slash,
    Equal,
    DoubleEqual,

    // Keywords
    If,
    Else,
    While,
    Def,
    Var,
    Struct,

    const This = @This();

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{s} {{ ", .{@typeName(This)});
        switch (this.*) {
            // Literals
            .Bool => |value| try writer.print(".Bool = {}", .{value}),
            .Int => |value| try writer.print(".Int = {}", .{value}),
            .Num => |value| try writer.print(".Num = {}", .{value}),
            .Str => |str| try writer.print(".Str = {s}", .{str}),
            .Ident => |id| try writer.print(".Ident = {s}", .{id}),

            // Delimeters
            .Newline => _ = try writer.write(".Newline"),
            .Comma => _ = try writer.write(".Comma"),
            .Semicolon => _ = try writer.write(".Semicolon"),
            .LeftParen => _ = try writer.write(".LeftParen"),
            .RightParen => _ = try writer.write(".RightParen"),
            .LeftCurly => _ = try writer.write(".LeftCurly"),
            .RightCurly => _ = try writer.write(".RightCurly"),
            .LeftSquare => _ = try writer.write(".LeftSqaure"),
            .RightSquare => _ = try writer.write(".RightSquare"),

            // Operators
            .Bang => _ = try writer.write(".Bang"),
            .BangEqual => _ = try writer.write(".BangEqual"),
            .Plus => _ = try writer.write(".Plus"),
            .Dash => _ = try writer.write(".Dash"),
            .Star => _ = try writer.write(".Star"),
            .Slash => _ = try writer.write(".Slash"),
            .Equal => _ = try writer.write(".Equal"),
            .DoubleEqual => _ = try writer.write(".DoubleEqual"),

            // Keywords
            .If => _ = try writer.write(".If"),
            .Else => _ = try writer.write(".Else"),
            .While => _ = try writer.write(".While"),
            .Def => _ = try writer.write(".Def"),
            .Var => _ = try writer.write(".Var"),
            .Struct => _ = try writer.write(".Struct"),
        }
        _ = try writer.write(" }");
    }
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

    fn next(this: TokenPrecedence) TokenPrecedence {
        const primary: u8 = @enumToInt(TokenPrecedence.Primary);
        const p = @enumToInt(this);
        return @intToEnum(TokenPrecedence, if (p + 1 > primary) primary else p + 1);
    }
};

pub const ParseError = error{
    TokenizerError,
    ParserError,
};

const Tokenizer = struct {
    allocator: Allocator,
    source: Utf8Iterator,
    filename: []const u8,
    peeked_tokens: ArrayList(Token),
    previous_was_newline: bool,

    line: usize,
    column: usize,
    token_location: CodeLocation,

    err_msg: ErrMsg,

    const This = @This();
    const Char = u21;

    fn init(allocator: Allocator, source: []const u8, filename: []const u8) !This {
        return This{
            .allocator = allocator,
            .source = (try Utf8View.init(source)).iterator(),
            .filename = filename,
            .peeked_tokens = ArrayList(Token).init(allocator),
            .previous_was_newline = true, // to skip leading newlines in source file
            .line = 0,
            .column = 0,
            .token_location = CodeLocation.init(0, 0, filename),
            .err_msg = ErrMsg.default(),
        };
    }

    fn bytesRemaining(this: *This) usize {
        return this.source.bytes.len - this.source.i;
    }

    fn recordTokenLocation(this: *This) void {
        this.token_location.line = this.line;
        this.token_location.col = this.column;
    }

    fn currentLocation(this: *This) CodeLocation {
        return CodeLocation.init(this.line, this.column, this.filename);
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

    fn peekChar(this: *This) ?Char {
        if (this.bytesRemaining() == 0) {
            return null;
        }

        const peeked = this.source.peek(1);
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

    fn nextChar(this: *This) ?Char {
        const maybe_next_char = this.source.nextCodepoint();
        if (maybe_next_char) |c| {
            if (c == '\n') {
                this.line += 1;
                this.column = 0;
            } else {
                this.column += 1;
            }
        }
        return maybe_next_char;
    }

    fn nextCharIf(this: *This, f: fn (Char) bool) ?Char {
        const peeked = this.peekChar();
        if (peeked) |c| {
            if (!f(c)) {
                return null;
            }
        }
        return this.nextChar();
    }

    fn nextCharIfEqual(this: *This, c: Char) bool {
        const peeked = this.peekChar();
        if (peeked) |pc| {
            if (c != pc) {
                return false;
            }
        }
        return this.nextChar() != null;
    }

    fn peek(this: *This) !?Token {
        if (this.peeked_tokens.items.len == 0) {
            const maybe_token = try this.next();
            if (maybe_token) |t| {
                try this.peeked_tokens.append(t);
            } else {
                return null;
            }
        }
        return this.peeked_tokens.items[0];
    }

    fn peekN(this: *This, n: usize) !?Token {
        while (this.peeked_tokens.items.len <= n) {
            if (try this.nextNoPeeking()) |t| {
                try this.peeked_tokens.append(t);
            } else {
                return null;
            }
        }
        return this.peeked_tokens.items[n];
    }

    fn next(this: *This) !?Token {
        if (this.peeked_tokens.items.len != 0) {
            return this.peeked_tokens.orderedRemove(0);
        }
        return try this.nextNoPeeking();
    }

    fn nextNoPeeking(this: *This) !?Token {
        this.skipToBeginningOfNextToken();
        this.recordTokenLocation();
        this.previous_was_newline = false;

        var token: ?Token = undefined;
        if (this.peekChar()) |c| {
            if (c == 0) {
                token = null;
            } else if (c == '\n') {
                this.previous_was_newline = true;
                _ = this.nextChar();
                token = Token.init(TokenData.Newline, this.token_location);
            } else if (isDigit(c)) {
                token = this.tokenizeNumber();
            } else if (isStringBegin(c)) {
                token = try this.tokenizeString();
            } else if (isIdentBegin(c)) {
                token = this.tokenizeIdentOrKeyword();
            } else {
                token = try this.tokenizePunctuation();
            }
        } else {
            token = null;
        }

        return token;
    }

    fn skipToBeginningOfNextToken(this: *This) void {
        while (true) {
            if (this.peekChar()) |c| {
                switch (c) {
                    '#' => while (true) {
                        if (this.nextChar()) |next_char| {
                            if (next_char == '\n') {
                                break;
                            }
                        } else {
                            break;
                        }
                    },
                    '\n' => {
                        if (!this.previous_was_newline) {
                            break;
                        }

                        _ = this.nextChar();
                    },
                    else => {
                        if (!isWhitespace(c)) {
                            break;
                        }
                        _ = this.nextChar();
                    },
                }
            } else {
                break;
            }
        }
    }

    fn tokenizeNumber(this: *This) Token {
        const start_index = this.source.i;
        var end_index: usize = start_index;
        while (this.nextCharIf(isDigit) != null) {
            end_index = this.source.i;
        }

        const reset_index = this.source.i;
        if (this.nextCharIfEqual('.')) {
            if (this.peekChar()) |c| {
                if (!isDigit(c)) {
                    this.source.i = reset_index;
                } else {
                    while (this.nextCharIf(isDigit) != null) {
                        end_index = this.source.i;
                    }

                    const word = this.source.bytes[start_index..end_index];
                    return Token.init(TokenData{ .Num = std.fmt.parseFloat(f64, word) catch unreachable }, this.token_location);
                }
            }
        }

        const word = this.source.bytes[start_index..end_index];
        return Token.init(TokenData{ .Int = std.fmt.parseInt(i64, word, 10) catch unreachable }, this.token_location);
    }

    // @TODO:
    // Handled escape sequences.
    //
    fn tokenizeString(this: *This) ParseError!Token {
        const terminator = this.nextChar().?;

        const start_index = this.source.i;
        var end_index = start_index;

        while (true) {
            if (this.nextChar()) |c| {
                if (c == terminator) {
                    break;
                }
                end_index = this.source.i;
            } else {
                return raise(ParseError.TokenizerError, this.token_location, "Unended string literal.", &this.err_msg);
            }
        }

        const word = this.source.bytes[start_index..end_index];
        return Token.init(TokenData{ .Str = word }, this.token_location);
    }

    fn tokenizeIdentOrKeyword(this: *This) Token {
        const start_index = this.source.i;
        var end_index = this.source.i;
        while (this.nextCharIf(isIdentChar) != null) {
            end_index = this.source.i;
        }
        const word = this.source.bytes[start_index..end_index];

        return if (std.mem.eql(u8, word, "true"))
            Token.init(TokenData{ .Bool = true }, this.token_location)
        else if (std.mem.eql(u8, word, "false"))
            Token.init(TokenData{ .Bool = false }, this.token_location)
        else if (std.mem.eql(u8, word, "if"))
            Token.init(TokenData.If, this.token_location)
        else if (std.mem.eql(u8, word, "else"))
            Token.init(TokenData.Else, this.token_location)
        else if (std.mem.eql(u8, word, "while"))
            Token.init(TokenData.While, this.token_location)
        else if (std.mem.eql(u8, word, "def"))
            Token.init(TokenData.Def, this.token_location)
        else if (std.mem.eql(u8, word, "var"))
            Token.init(TokenData.Var, this.token_location)
        else if (std.mem.eql(u8, word, "struct"))
            Token.init(TokenData.Struct, this.token_location)
        else
            Token.init(TokenData{ .Ident = word }, this.token_location);
    }

    fn tokenizePunctuation(this: *This) !Token {
        return switch (this.nextChar().?) {
            ',' => Token.init(TokenData.Comma, this.token_location),
            ';' => Token.init(TokenData.Semicolon, this.token_location),
            '(' => Token.init(TokenData.LeftParen, this.token_location),
            ')' => Token.init(TokenData.RightParen, this.token_location),
            '{' => Token.init(TokenData.LeftCurly, this.token_location),
            '}' => Token.init(TokenData.RightCurly, this.token_location),
            '[' => Token.init(TokenData.LeftSquare, this.token_location),
            ']' => Token.init(TokenData.RightSquare, this.token_location),
            '!' => if (this.nextCharIfEqual('='))
                Token.init(TokenData.BangEqual, this.token_location)
            else
                Token.init(TokenData.Bang, this.token_location),
            '+' => Token.init(TokenData.Plus, this.token_location),
            '-' => Token.init(TokenData.Dash, this.token_location),
            '*' => Token.init(TokenData.Star, this.token_location),
            '/' => Token.init(TokenData.Slash, this.token_location),
            '=' => if (this.nextCharIfEqual('='))
                Token.init(TokenData.DoubleEqual, this.token_location)
            else
                Token.init(TokenData.Equal, this.token_location),
            else => |c| raise(ParseError.TokenizerError, this.token_location, try std.fmt.allocPrint(this.allocator, "Unknown operator `{u}`", .{c}), &this.err_msg),
        };
    }
};

pub const Parser = struct {
    allocator: Allocator,
    tokenizer: Tokenizer,

    err_msg: ErrMsg,

    const This = @This();

    pub fn init(allocator: Allocator, source: []const u8, filename: []const u8) !This {
        return This{ .allocator = allocator, .tokenizer = try Tokenizer.init(allocator, source, filename), .err_msg = ErrMsg.default() };
    }

    fn check(this: *This, kind: TokenKind) !bool {
        const next = try this.tokenizer.peek();
        return if (next) |n| n.data == kind else false;
    }

    fn skipCheck(this: *This, kind: TokenKind) !bool {
        try this.skipNewlines();
        return this.check(kind);
    }

    fn peekCheck(this: *This, kind: TokenKind) !bool {
        const i = try this.peekNewlines();
        return if (try this.tokenizer.peekN(i)) |token| token.data == kind else false;
    }

    fn checkEof(this: *This) !bool {
        return (try this.tokenizer.peek()) == null;
    }

    fn skipCheckEof(this: *This) !bool {
        try this.skipNewlines();
        return this.checkEof();
    }

    fn peekCheckEof(this: *This) !bool {
        const i = try this.peekNewlines();
        return (try this.tokenizer.peekN(i)) == null;
    }

    fn match(this: *This, kind: TokenKind) !?Token {
        if (try this.check(kind)) {
            return this.tokenizer.next();
        }
        return null;
    }

    fn skipMatch(this: *This, kind: TokenKind) !?Token {
        try this.skipNewlines();
        return this.match(kind);
    }

    fn peekMatch(this: *This, kind: TokenKind) !?Token {
        const i = try this.peekNewlines();
        if (try this.tokenizer.peekN(i)) |t| {
            if (t.data == kind) {
                this.flushPeekedNewlines();
                return this.tokenizer.next();
            }
        }
        return null;
    }

    fn eat(this: *This, kind: TokenKind) !void {
        _ = try this.match(kind);
    }

    fn expect(this: *This, kind: TokenKind, err_msg: []const u8) !Token {
        const next = try this.tokenizer.next();
        if (next) |n| {
            if (n.data != kind) {
                return raise(ParseError.ParserError, n.location, err_msg, &this.err_msg);
            } else {
                return n;
            }
        }
        return raise(ParseError.ParserError, this.tokenizer.currentLocation(), err_msg, &this.err_msg);
    }

    fn skipExpect(this: *This, kind: TokenKind, err_msg: []const u8) !Token {
        try this.skipNewlines();
        return this.expect(kind, err_msg);
    }

    fn peekExpect(this: *This, kind: TokenKind, err_msg: []const u8) !Token {
        const i = this.peekNewlines();
        if (try this.tokenizer.peekN(i)) |t| {
            if (t.data == kind) {
                this.flushPeekedNewlines();
                return (this.tokenizer.next() catch unreachable).?;
            } else {
                return raise(ParseError.ParserError, t.location, err_msg, &this.err_msg);
            }
        }
        return raise(ParseError.ParserError, this.tokenizer.currentLocation(), err_msg, &this.err_msg);
    }

    fn expectStatementTerminator(this: *This) !bool {
        if (try this.tokenizer.next()) |token| {
            return switch (token.data) {
                .Newline, .Semicolon => true,
                else => false,
            };
        }

        return true;
    }

    fn skipNewlines(this: *This) !void {
        while (try this.tokenizer.peek()) |t| {
            if (t.data != .Newline) {
                break;
            }
            _ = this.tokenizer.next() catch unreachable;
        }
    }

    fn peekNewlines(this: *This) !usize {
        var i: usize = 0;
        while (try this.tokenizer.peekN(i)) |t| {
            if (t.data != .Newline) {
                break;
            }
            i += 1;
        }
        return i;
    }

    fn flushPeekedNewlines(this: *This) void {
        while (true) {
            if (this.tokenizer.peeked_tokens.items.len == 0 or this.tokenizer.peeked_tokens.items[0].data != .Newline) {
                break;
            }
            _ = this.tokenizer.peeked_tokens.orderedRemove(0);
        }
    }

    fn parseDeclaration(this: *This) !*Ast {
        return if (try this.match(.Def)) |token|
            (try this.parseDef(token)).asAst()
        else if (try this.match(.Struct)) |token|
            (try this.parseStruct(token)).asAst()
        else
            this.parseStatement();
    }

    fn parseStatement(this: *This) !*Ast {
        if (false) {
            // @TODO:
            // This is where statement parsing functions will go.
            //
            unreachable;
        } else {
            const node = try this.parseExpressionOrAssignment();
            _ = try this.expectStatementTerminator();
            return node;
        }
    }

    fn parseExpressionOrAssignment(this: *This) anyerror!*Ast {
        return this.parsePrecedence(.Assignment);
    }

    fn parseExpression(this: *This) anyerror!*Ast {
        const node = try this.parseExpressionOrAssignment();
        if (node.kind == .Assign) {
            return raise(ParseError.ParserError, node.token.location, "Cannot assign in expression context.", &this.err_msg);
        }

        return node;
    }

    fn parsePrecedence(this: *This, precedence: TokenPrecedence) anyerror!*Ast {
        const maybe_token = try this.tokenizer.next();
        if (maybe_token == null) {
            return raise(ParseError.ParserError, this.tokenizer.currentLocation(), "Unexpected end of file.", &this.err_msg);
        }

        const token = maybe_token.?;

        var previous = try this.parsePrefix(token);
        while (true) {
            if (try this.tokenizer.peek()) |infix| {
                if (@enumToInt(precedence) > @enumToInt(infix.precedence())) {
                    break;
                }
                const next_token = (this.tokenizer.next() catch unreachable).?;
                previous = try this.parseInfix(next_token, previous);
            } else {
                return raise(ParseError.ParserError, this.tokenizer.currentLocation(), "Unexpected end of file.", &this.err_msg);
            }
        }

        return previous;
    }

    fn parsePrefix(this: *This, token: Token) !*Ast {
        switch (token.data) {
            // Literals
            .Bool => |value| {
                return (try this.createLiteral(.Bool, token, AstLiteral.Literal{ .Bool = value })).asAst();
            },
            .Int => |value| {
                return (try this.createLiteral(.Int, token, AstLiteral.Literal{ .Int = value })).asAst();
            },
            .Num => |value| {
                return (try this.createLiteral(.Num, token, AstLiteral.Literal{ .Num = value })).asAst();
            },
            .Str => |value| {
                return (try this.createLiteral(.Str, token, AstLiteral.Literal{ .Str = value })).asAst();
            },
            .Ident => |ident| {
                var node = try this.allocator.create(AstIdent);
                node.* = AstIdent.init(token, ident);
                return node.asAst();
            },

            // Delimeters
            .LeftParen => {
                const expr = try this.parseExpression();
                _ = try this.expect(.RightParen, "Expected `)` to terminate parenthesized expression.");
                return expr;
            },
            .LeftCurly => return (try this.parseBlockNoExpect(token)).asAst(),
            .LeftSquare => return (try this.parseList(token)).asAst(),

            // Operators
            .Bang => {
                return (try this.parseUnary(.Not, token)).asAst();
            },
            .Dash => {
                return (try this.parseUnary(.Negate, token)).asAst();
            },

            // Keywords
            .If => {
                return (try this.parseIf(token)).asAst();
            },
            .While => {
                return (try this.parseWhile(token)).asAst();
            },
            .Var => {
                return (try this.parseVar(token)).asAst();
            },

            else => {
                return raise(ParseError.ParserError, token.location, try std.fmt.allocPrint(this.allocator, "`{!}` is not a prefix operation.", .{token}), &this.err_msg);
            },
        }
    }

    fn parseInfix(this: *This, token: Token, previous: *Ast) !*Ast {
        const prec = token.precedence();
        switch (token.data) {
            .Plus => {
                return (try this.parseBinary(prec, .Add, token, previous)).asAst();
            },
            .Dash => {
                return (try this.parseBinary(prec, .Subtract, token, previous)).asAst();
            },
            .Star => {
                return (try this.parseBinary(prec, .Multiply, token, previous)).asAst();
            },
            .Slash => {
                return (try this.parseBinary(prec, .Divide, token, previous)).asAst();
            },
            .Equal => {
                return (try this.parseBinary(prec, .Assign, token, previous)).asAst();
            },
            .BangEqual => {
                return (try this.parseBinary(prec, .NotEqual, token, previous)).asAst();
            },
            .DoubleEqual => {
                return (try this.parseBinary(prec, .Equal, token, previous)).asAst();
            },

            .LeftParen => {
                const args = try this.parseCommaSeparatedExpressions(.RightParen, token);
                _ = try this.expect(.RightParen, "Expected `)` to terminate call operator.");

                var call = try this.allocator.create(AstBinary);
                call.* = AstBinary.init(.Call, token, previous, args.asAst());

                return call.asAst();
            },
            .LeftSquare => {
                const expr = (try this.parseBinary(prec, .Index, token, previous)).asAst();
                _ = try this.expect(.RightSquare, "Expected `]` to terminate index operator.");
                return expr;
            },

            else => {
                return raise(ParseError.ParserError, token.location, try std.fmt.allocPrint(this.allocator, "`{!}` is not an infix operation.", .{token}), &this.err_msg);
            },
        }
    }

    fn createLiteral(this: *This, kind: AstKind, token: Token, literal: AstLiteral.Literal) !*AstLiteral {
        var node = try this.allocator.create(AstLiteral);
        node.* = AstLiteral.init(kind, token, literal);
        return node;
    }

    fn parseUnary(this: *This, kind: AstKind, token: Token) anyerror!*AstUnary {
        const sub = try this.parsePrecedence(.Unary);

        var node = try this.allocator.create(AstUnary);
        node.* = AstUnary.init(kind, token, sub);

        return node;
    }

    fn parseBinary(this: *This, precedence: TokenPrecedence, kind: AstKind, token: Token, lhs: *Ast) anyerror!*AstBinary {
        const rhs = try this.parsePrecedence(precedence.next());

        var node = try this.allocator.create(AstBinary);
        node.* = AstBinary.init(kind, token, lhs, rhs);

        return node;
    }

    fn parseBlock(this: *This) anyerror!*AstBlock {
        const block_token = try this.skipExpect(.LeftCurly, "Expected `{` to begin block.");
        return this.parseBlockNoExpect(block_token);
    }

    fn parseBlockNoExpect(this: *This, token: Token) anyerror!*AstBlock {
        var nodes = ArrayListUnmanaged(*Ast){};

        while (true) {
            try this.skipNewlines();
            if ((try this.check(.RightCurly)) or (try this.checkEof())) {
                break;
            }
            try nodes.append(this.allocator, try this.parseDeclaration());
        }

        _ = try this.expect(.RightCurly, "Expected `}` to terminate block.");

        var block = try this.allocator.create(AstBlock);
        block.* = AstBlock.init(.Block, token, nodes.items);

        return block;
    }

    fn parseCommaSeparatedExpressions(this: *This, terminator: TokenKind, token: Token) anyerror!*AstBlock {
        var nodes = ArrayListUnmanaged(*Ast){};

        while (true) {
            try this.skipNewlines();
            if ((try this.check(terminator)) or (try this.checkEof())) {
                break;
            }

            try nodes.append(this.allocator, try this.parseExpression());

            if ((try this.match(.Newline)) != null) {
                try this.skipNewlines();
                try this.eat(.Comma);
            } else if ((try this.match(.Comma)) != null) {
                // carry on
            } else {
                break;
            }
        }

        var block = try this.allocator.create(AstBlock);
        block.* = AstBlock.init(.Comma, token, nodes.items);

        return block;
    }

    fn parseList(this: *This, token: Token) anyerror!*AstBlock {
        const list = try this.parseCommaSeparatedExpressions(.RightSquare, token);
        _ = try this.expect(.RightSquare, "Expected `]` to terminate List literal.");

        list.kind = .List;
        return list;
    }

    fn parseIdent(this: *This) std.mem.Allocator.Error!?*AstIdent {
        const ident_token = this.expect(.Ident, "") catch return null;
        switch (ident_token.data) {
            .Ident => |ident| {
                var node = try this.allocator.create(AstIdent);
                node.* = AstIdent.init(ident_token, ident);
                return node;
            },
            else => unreachable,
        }
    }

    fn parseIf(this: *This, token: Token) anyerror!*AstIf {
        try this.skipNewlines();
        const condition = try this.parseExpression();

        const then_block = try this.parseBlock();
        const else_block = if ((try this.peekMatch(.Else)) != null)
            if (try this.skipCheck(.If))
                (try this.parseIf((this.tokenizer.next() catch unreachable).?)).asAst()
            else
                (try this.parseBlock()).asAst()
        else
            null;

        var node = try this.allocator.create(AstIf);
        node.* = AstIf.init(.If, token, condition, then_block, else_block);

        return node;
    }

    fn parseWhile(this: *This, token: Token) anyerror!*AstWhile {
        try this.skipNewlines();
        const condition = try this.parseExpression();
        const block = try this.parseBlock();

        var node = try this.allocator.create(AstWhile);
        node.* = AstWhile.init(.While, token, condition, block);

        return node;
    }

    fn parseVar(this: *This, token: Token) anyerror!*AstVar {
        try this.skipNewlines();
        const ident = (try this.parseIdent()) orelse {
            return raise(ParseError.ParserError, token.location, "Expected identifier after `var` keyword.", &this.err_msg);
        };

        _ = try this.expect(.Equal, "Expected `=` before initializer expression of variable declaration.");

        const initializer = try this.parseExpression();

        var node = try this.allocator.create(AstVar);
        node.* = AstVar.init(token, ident, initializer);

        return node;
    }

    fn parseDef(this: *This, token: Token) anyerror!*AstDef {
        const ident_token = try this.expect(.Ident, "Expected an identifer after keyword `def`.");
        const ident = switch (ident_token.data) {
            .Ident => |id| id,
            else => unreachable,
        };

        _ = try this.expect(.LeftParen, "Expected `(` to begin parameter list.");

        var param_nodes = ArrayList(*Ast).init(this.allocator);
        while (true) {
            if ((try this.check(.RightParen)) or (try this.checkEof())) {
                break;
            }

            const param_token = try this.expect(.Ident, "Expected parameter name.");
            const param_name = switch (param_token.data) {
                .Ident => |id| id,
                else => unreachable,
            };

            var param_ident = try this.allocator.create(AstIdent);
            param_ident.* = AstIdent.init(param_token, param_name);

            try param_nodes.append(param_ident.asAst());

            if ((try this.match(.Comma)) == null or (try this.checkEof())) {
                break;
            }
        }

        var params = try this.allocator.create(AstBlock);
        params.* = AstBlock.init(.Comma, token, param_nodes.items);

        _ = try this.expect(.RightParen, "Expected `)` to terminate parameter list.");

        const body = try this.parseBlock();

        var node = try this.allocator.create(AstDef);
        node.* = AstDef.init(token, ident, params, body);

        return node;
    }

    fn parseStruct(this: *This, token: Token) anyerror!*ast.AstStruct {
        try this.skipNewlines();
        const ident = (try this.parseIdent()) orelse {
            return raise(ParseError.ParserError, token.location, "Expected identifier after `struct` keyword.", &this.err_msg);
        };

        const left_curly_token = try this.skipExpect(.LeftCurly, "Expected `{` to begin struct body.");
        const body = try this.parseCommaSeparatedExpressions(.RightCurly, left_curly_token);
        _ = try this.expect(.RightCurly, "Expected `}` to terminate struct body.");

        var node = try this.allocator.create(AstStruct);
        node.* = AstStruct.init(token, ident, body);

        return node;
    }

    pub fn parse(this: *This) !ArrayList(*Ast) {
        var nodes = ArrayList(*Ast).init(this.allocator);

        while (true) {
            try this.skipNewlines();
            if ((try this.tokenizer.peek()) == null) {
                break;
            }

            const node = try this.parseDeclaration();
            try nodes.append(node);
        }

        return nodes;
    }
};
