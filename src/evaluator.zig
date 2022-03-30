const std = @import("std");
const ast = @import("ast.zig");
const err = @import("error.zig");
const val = @import("value.zig");

const RuntimeError = error{ InvalidOperation, TypeMismatch };

const Scope = struct {
    rc_values: std.ArrayList(val.Value),

    fn init(allocator: std.mem.Allocator) @This() {
        return Scope{
            .rc_values = std.ArrayList(val.Value).init(allocator),
        };
    }

    fn deinit(this: @This()) void {
        for (this.rc_values.items) |value| {
            value.drop();
        }

        this.rc_values.deinit();
    }
};

pub const Evaluator = struct {
    allocator: std.mem.Allocator,
    scopes: std.ArrayList(Scope),
    err_msg: err.ErrMsg,

    pub fn init(allocator: std.mem.Allocator) anyerror!@This() {
        var scopes = std.ArrayList(Scope).init(allocator);
        try scopes.append(Scope.init(allocator));

        return Evaluator{
            .allocator = allocator,
            .scopes = scopes,
            .err_msg = err.ErrMsg.default(),
        };
    }

    pub fn deinit(this: *@This()) void {
        for (this.scopes.items) |scope| {
            scope.deinit();
        }
    }

    pub fn evaluate(this: *@This(), nodes: std.ArrayList(*ast.Ast)) anyerror!void {
        for (nodes.items) |node| {
            const v = try this.evaluateNode(node);
            std.debug.print("{}\n", .{v});
        }
    }

    fn evaluateNode(this: *@This(), node: *ast.Ast) anyerror!val.Value {
        return switch (node.kind) {
            // Literals
            .Bool, .Int, .Num, .Str => blk: {
                const l = node.downcast(ast.AstLiteral);
                break :blk this.evaluateLiteral(l);
            },
            .Ident => err.todo("Implement Ident evaluation"),

            // Unary
            .Negate => blk: {
                const unary = node.downcast(ast.AstUnary);
                break :blk this.evaluateUnary(unary);
            },

            // Binary
            .Add, .Subtract, .Multiply, .Divide => blk: {
                const binary = node.downcast(ast.AstBinary);
                break :blk this.evaluateBinary(binary);
            },

            .Assign => err.todo("Implement assign"),

            // Blocks
            .Block, .Comma => blk: {
                const block = node.downcast(ast.AstBlock);
                break :blk this.evaluateBlock(block);
            },

            .If => blk: {
                const _if = node.downcast(ast.AstIf);
                break :blk this.evaluateIf(_if);
            },
            .While => blk: {
                const _while = node.downcast(ast.AstWhile);
                break :blk this.evaluateWhile(_while);
            },
            .Def => err.todo("Implement Def evaluation"),
        };
    }

    fn evaluateLiteral(this: *@This(), literal: *ast.AstLiteral) anyerror!val.Value {
        return switch (literal.literal) {
            .Bool => |value| val.Value{ .Bool = value },
            .Int => |value| val.Value{ .Int = value },
            .Num => |value| val.Value{ .Num = value },
            .Str => |value| blk: {
                const v = val.Value{ .Str = try val.RefCounted([]const u8).create(this.allocator, value) };
                try this.scopes.items[this.scopes.items.len - 1].rc_values.append(v);
                break :blk v;
            },
        };
    }

    fn evaluateUnary(this: *@This(), unary: *ast.AstUnary) anyerror!val.Value {
        return switch (unary.kind) {
            .Negate => this.evaluateNegate(unary.sub),
            else => err.raise(RuntimeError.InvalidOperation, unary.token.location, "Invalid unary operation", &this.err_msg),
        };
    }

    fn evaluateNegate(this: *@This(), sub_node: *ast.Ast) anyerror!val.Value {
        const sub = try this.evaluateNode(sub_node);

        return switch (sub) {
            .Int => |value| val.Value{ .Int = -value },
            .Num => |value| val.Value{ .Num = -value },
            else => err.raise(RuntimeError.TypeMismatch, sub_node.token.location, "`-` requires its operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateBinary(this: *@This(), binary: *ast.AstBinary) anyerror!val.Value {
        return switch (binary.kind) {
            .Add => this.evaluateAdd(binary.lhs, binary.rhs),
            .Subtract => this.evaluateSubtract(binary.lhs, binary.rhs),
            .Multiply => this.evaluateMultiply(binary.lhs, binary.rhs),
            .Divide => this.evaluateDivide(binary.lhs, binary.rhs),
            else => err.raise(RuntimeError.InvalidOperation, binary.token.location, "Invalid binary operation", &this.err_msg),
        };
    }

    fn evaluateAdd(this: *@This(), lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Int = lhs_value + rhs_value },
                .Num => |rhs_value| val.Value{ .Num = @intToFloat(f64, lhs_value) + rhs_value },
                else => err.raise(RuntimeError.TypeMismatch, rhs_node.token.location, "`+` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Num = lhs_value + @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| val.Value{ .Num = lhs_value + rhs_value },
                else => err.raise(RuntimeError.TypeMismatch, rhs_node.token.location, "`+` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => err.raise(RuntimeError.TypeMismatch, lhs_node.token.location, "`+` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateSubtract(this: *@This(), lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Int = lhs_value - rhs_value },
                .Num => |rhs_value| val.Value{ .Num = @intToFloat(f64, lhs_value) - rhs_value },
                else => err.raise(RuntimeError.TypeMismatch, rhs_node.token.location, "`-` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Num = lhs_value - @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| val.Value{ .Num = lhs_value - rhs_value },
                else => err.raise(RuntimeError.TypeMismatch, rhs_node.token.location, "`-` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => err.raise(RuntimeError.TypeMismatch, lhs_node.token.location, "`-` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateMultiply(this: *@This(), lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Int = lhs_value * rhs_value },
                .Num => |rhs_value| val.Value{ .Num = @intToFloat(f64, lhs_value) * rhs_value },
                else => err.raise(RuntimeError.TypeMismatch, rhs_node.token.location, "`*` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Num = lhs_value * @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| val.Value{ .Num = lhs_value * rhs_value },
                else => err.raise(RuntimeError.TypeMismatch, rhs_node.token.location, "`*` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => err.raise(RuntimeError.TypeMismatch, lhs_node.token.location, "`*` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateDivide(this: *@This(), lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Int = @divTrunc(lhs_value, rhs_value) },
                .Num => |rhs_value| val.Value{ .Num = @divTrunc(@intToFloat(f64, lhs_value), rhs_value) },
                else => err.raise(RuntimeError.TypeMismatch, rhs_node.token.location, "`/` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Num = @divTrunc(lhs_value, @intToFloat(f64, rhs_value)) },
                .Num => |rhs_value| val.Value{ .Num = @divTrunc(lhs_value, rhs_value) },
                else => err.raise(RuntimeError.TypeMismatch, rhs_node.token.location, "`/` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => err.raise(RuntimeError.TypeMismatch, lhs_node.token.location, "`/` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateBlock(this: *@This(), block: *ast.AstBlock) anyerror!val.Value {
        var rval: val.Value = .None;

        if (block.kind == .Block) {
            try this.scopes.append(Scope.init(this.allocator));
        }

        for (block.nodes) |node| {
            rval = try this.evaluateNode(node);
        }

        if (block.kind == .Block) {
            _ = this.scopes.pop();
        }

        return rval;
    }

    fn evaluateIf(this: *@This(), _if: *ast.AstIf) anyerror!val.Value {
        const cond = try this.evaluateNode(_if.condition);

        return if (cond.isTrue())
            this.evaluateNode(_if.then_block.asAst())
        else if (_if.else_block) |else_block|
            this.evaluateNode(else_block)
        else
            @as(val.Value, val.Value.None);
    }

    fn evaluateWhile(this: *@This(), _while: *ast.AstWhile) anyerror!val.Value {
        var rval: val.Value = .None;

        while ((try this.evaluateNode(_while.condition)).isTrue()) {
            rval = try this.evaluateNode(_while.block.asAst());
        }

        return rval;
    }
};
