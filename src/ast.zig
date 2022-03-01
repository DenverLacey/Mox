const std = @import("std");
const parser = @import("parser.zig");
const Token = parser.Token;

pub const Ast = struct {
    kind: AstKind,
    token: Token,

    const This = @This();

    fn init(kind: AstKind, token: Token) This {
        return This{ .kind = kind, .token = token };
    }

    pub fn downcast(this: *This, comptime T: type) *T {
        return @ptrCast(*T, this);
    }

    pub fn downcastConst(this: *const This, comptime T: type) *const T {
        return @ptrCast(*const T, this);
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (this.kind) {
            // Literals
            .Bool, .Int, .Num, .Str => try writer.print("{}", .{this.downcastConst(AstLiteral)}),
            .Ident => try writer.print("{}", .{this.downcastConst(AstIdent)}),

            // Unary
            .Negate => try writer.print("{}", .{this.downcastConst(AstUnary)}),

            // Binary
            .Assign, .Add, .Subtract, .Multiply, .Divide => try writer.print("{}", .{this.downcastConst(AstBinary)}),
        }
    }
};

pub const AstKind = enum {
    // Literals
    Bool,
    Int,
    Num,
    Str,
    Ident,

    // Unary
    Negate,

    // Binary
    Assign,
    Add,
    Subtract,
    Multiply,
    Divide,
};

pub const AstLiteral = struct {
    kind: AstKind,
    token: Token,
    literal: Literal,

    const This = @This();

    pub const Literal = union(enum) {
        Bool: bool,
        Int: i64,
        Num: f64,
        Str: []const u8,
    };

    pub fn init(kind: AstKind, token: Token, literal: Literal) This {
        return This{ .kind = kind, .token = token, .literal = literal };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstIdent = struct {
    kind: AstKind,
    token: Token,
    ident: []const u8,

    const This = @This();

    pub fn init(token: Token, ident: []const u8) This {
        return This{ .kind = .Ident, .token = token, .ident = ident };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstUnary = struct {
    kind: AstKind,
    token: Token,
    sub: *Ast,

    const This = @This();

    pub fn init(kind: AstKind, token: Token, sub: *Ast) This {
        return This{ .kind = kind, .token = token, .sub = sub };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstBinary = struct {
    kind: AstKind,
    token: Token,
    lhs: *Ast,
    rhs: *Ast,

    const This = @This();

    pub fn init(kind: AstKind, token: Token, lhs: *Ast, rhs: *Ast) This {
        return This{ .kind = kind, .token = token, .lhs = lhs, .rhs = rhs };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};
