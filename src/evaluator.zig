const std = @import("std");
const StringArrayHashMap = std.StringArrayHashMap;

const ast = @import("ast.zig");
const err = @import("error.zig");
const val = @import("value.zig");
const BucketArray = @import("bucket_array.zig").BucketArray;

const Scope = struct {
    parent: ?*This,
    variables: StringArrayHashMap(val.Value),
    rc_values: std.ArrayList(val.Value),

    const This = @This();

    fn init(allocator: std.mem.Allocator, parent: ?*This) This {
        return Scope{
            .parent = parent,
            .variables = StringArrayHashMap(val.Value).init(allocator),
            .rc_values = std.ArrayList(val.Value).init(allocator),
        };
    }

    fn deinit(this: *This) void {
        {
            var it = this.variables.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.drop();
            }
        }
        this.variables.deinit();

        for (this.rc_values.items) |value| {
            value.drop();
        }
        this.rc_values.deinit();
    }
};

pub const Evaluator = struct {
    allocator: std.mem.Allocator,
    global_scope: *Scope,
    scopes: BucketArray(8, Scope),
    err_msg: err.ErrMsg,

    const This = @This();

    pub fn init(allocator: std.mem.Allocator) anyerror!This {
        var scopes = BucketArray(8, Scope).init(allocator);
        try scopes.push(Scope.init(allocator, null));

        return Evaluator{
            .allocator = allocator,
            .global_scope = scopes.top().?,
            .scopes = scopes,
            .err_msg = err.ErrMsg.default(),
        };
    }

    pub fn deinit(this: *This) void {
        this.global_scope.deinit();
    }

    fn currentScope(this: *This) *Scope {
        return this.scopes.top().?;
    }

    fn beginScope(this: *This) !void {
        try this.scopes.push(Scope.init(this.allocator, this.currentScope()));
    }

    fn endScope(this: *This) void {
        const scope = this.currentScope();
        std.debug.assert(scope != this.global_scope);

        scope.deinit();

        this.scopes.pop();
    }

    fn getVariable(this: *This, ident: []const u8) ?*val.Value {
        var it: ?*Scope = this.currentScope();

        while (it) |scope| {
            if (scope.variables.getPtr(ident)) |value| {
                return value;
            }
            it = scope.parent;
        }

        return this.global_scope.variables.getPtr(ident);
    }

    pub fn evaluate(this: *This, nodes: std.ArrayList(*ast.Ast)) anyerror!void {
        for (nodes.items) |node| {
            const v = try this.evaluateNode(node);
            std.debug.print("{}\n", .{v});
            v.drop();
        }
    }

    fn evaluateNode(this: *This, node: *ast.Ast) anyerror!val.Value {
        return switch (node.kind) {
            // Literals
            .Bool, .Int, .Num, .Str => blk: {
                const l = node.downcast(ast.AstLiteral);
                break :blk this.evaluateLiteral(l);
            },
            .Ident => blk: {
                const ident = node.downcast(ast.AstIdent);
                break :blk this.evaluateIdent(ident);
            },

            // Unary
            .Negate => blk: {
                const unary = node.downcast(ast.AstUnary);
                break :blk this.evaluateUnary(unary);
            },

            // Binary
            .Add,
            .Subtract,
            .Multiply,
            .Divide,
            => blk: {
                const binary = node.downcast(ast.AstBinary);
                break :blk this.evaluateBinary(binary);
            },

            .Assign => blk: {
                const assign = node.downcast(ast.AstBinary);
                break :blk this.evaluateAssign(assign);
            },

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
            .Def => {
                _ = node.downcast(ast.AstDef);
                err.todo("Implement Def evaluation");
            },
            .Var => blk: {
                const _var = node.downcast(ast.AstVar);
                break :blk this.evaluateVar(_var);
            },
        };
    }

    fn evaluateLiteral(this: *This, literal: *ast.AstLiteral) anyerror!val.Value {
        return switch (literal.literal) {
            .Bool => |value| val.Value{ .Bool = value },
            .Int => |value| val.Value{ .Int = value },
            .Num => |value| val.Value{ .Num = value },
            .Str => |value| blk: {
                const v = val.Value{ .Str = try val.RefCounted([]const u8).create(this.allocator, value) };
                // try this.currentScope().rc_values.append(v);
                break :blk v;
            },
        };
    }

    fn evaluateIdent(this: *This, ident: *ast.AstIdent) anyerror!val.Value {
        if (this.getVariable(ident.ident)) |variable_ptr| {
            return variable_ptr.dupe();
        }
        return err.raise(error.RuntimeError, ident.token.location, "Unknown identifier!", &this.err_msg);
    }

    fn evaluateUnary(this: *This, unary: *ast.AstUnary) anyerror!val.Value {
        return switch (unary.kind) {
            .Negate => this.evaluateNegate(unary.sub),
            else => err.raise(error.RuntimeError, unary.token.location, "Invalid unary operation", &this.err_msg),
        };
    }

    fn evaluateNegate(this: *This, sub_node: *ast.Ast) anyerror!val.Value {
        const sub = try this.evaluateNode(sub_node);

        return switch (sub) {
            .Int => |value| val.Value{ .Int = -value },
            .Num => |value| val.Value{ .Num = -value },
            else => err.raise(error.RuntimeError, sub_node.token.location, "`-` requires its operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateBinary(this: *This, binary: *ast.AstBinary) anyerror!val.Value {
        return switch (binary.kind) {
            .Add => this.evaluateAdd(binary.lhs, binary.rhs),
            .Subtract => this.evaluateSubtract(binary.lhs, binary.rhs),
            .Multiply => this.evaluateMultiply(binary.lhs, binary.rhs),
            .Divide => this.evaluateDivide(binary.lhs, binary.rhs),
            else => err.raise(error.RuntimeError, binary.token.location, "Invalid binary operation", &this.err_msg),
        };
    }

    fn evaluateAdd(this: *This, lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Int = lhs_value + rhs_value },
                .Num => |rhs_value| val.Value{ .Num = @intToFloat(f64, lhs_value) + rhs_value },
                else => err.raise(error.RuntimeError, rhs_node.token.location, "`+` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Num = lhs_value + @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| val.Value{ .Num = lhs_value + rhs_value },
                else => err.raise(error.RuntimeError, rhs_node.token.location, "`+` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => err.raise(error.RuntimeError, lhs_node.token.location, "`+` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateSubtract(this: *This, lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Int = lhs_value - rhs_value },
                .Num => |rhs_value| val.Value{ .Num = @intToFloat(f64, lhs_value) - rhs_value },
                else => err.raise(error.RuntimeError, rhs_node.token.location, "`-` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Num = lhs_value - @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| val.Value{ .Num = lhs_value - rhs_value },
                else => err.raise(error.RuntimeError, rhs_node.token.location, "`-` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => err.raise(error.RuntimeError, lhs_node.token.location, "`-` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateMultiply(this: *This, lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Int = lhs_value * rhs_value },
                .Num => |rhs_value| val.Value{ .Num = @intToFloat(f64, lhs_value) * rhs_value },
                else => err.raise(error.RuntimeError, rhs_node.token.location, "`*` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Num = lhs_value * @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| val.Value{ .Num = lhs_value * rhs_value },
                else => err.raise(error.RuntimeError, rhs_node.token.location, "`*` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => err.raise(error.RuntimeError, lhs_node.token.location, "`*` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateDivide(this: *This, lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Int = @divTrunc(lhs_value, rhs_value) },
                .Num => |rhs_value| val.Value{ .Num = @divTrunc(@intToFloat(f64, lhs_value), rhs_value) },
                else => err.raise(error.RuntimeError, rhs_node.token.location, "`/` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| val.Value{ .Num = @divTrunc(lhs_value, @intToFloat(f64, rhs_value)) },
                .Num => |rhs_value| val.Value{ .Num = @divTrunc(lhs_value, rhs_value) },
                else => err.raise(error.RuntimeError, rhs_node.token.location, "`/` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => err.raise(error.RuntimeError, lhs_node.token.location, "`/` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateAssign(this: *This, assign: *ast.AstBinary) anyerror!val.Value {
        return switch (assign.lhs.kind) {
            .Ident => this.evaluateAssignIdent(assign.lhs.downcast(ast.AstIdent), assign.rhs),
            else => err.raise(error.RuntimeError, assign.lhs.token.location, "Cannot assign to this kind of expression!", &this.err_msg),
        };
    }

    fn evaluateAssignIdent(this: *This, ident: *ast.AstIdent, expr: *ast.Ast) anyerror!val.Value {
        const value_ptr = this.getVariable(ident.ident) orelse return err.raise(error.RuntimeError, ident.token.location, "Unknown identifier!", &this.err_msg);
        const new_value = try this.evaluateNode(expr);
        value_ptr.drop();
        value_ptr.* = new_value;
        return val.Value.None;
    }

    fn evaluateBlock(this: *This, block: *ast.AstBlock) anyerror!val.Value {
        var trval: val.Value = .None;

        if (block.kind == .Block) {
            try this.beginScope();
        }

        for (block.nodes) |node| {
            trval = try this.evaluateNode(node);

            if (trval.isRedCounted()) {
                try this.currentScope().rc_values.append(trval);
            }
        }

        const rval = trval.dupe();

        if (block.kind == .Block) {
            this.endScope();
        }

        return rval;
    }

    fn evaluateIf(this: *This, _if: *ast.AstIf) anyerror!val.Value {
        const cond = try this.evaluateNode(_if.condition);

        return if (cond.isTrue())
            this.evaluateNode(_if.then_block.asAst())
        else if (_if.else_block) |else_block|
            this.evaluateNode(else_block)
        else
            @as(val.Value, val.Value.None);
    }

    fn evaluateWhile(this: *This, _while: *ast.AstWhile) anyerror!val.Value {
        var rval: val.Value = .None;

        while ((try this.evaluateNode(_while.condition)).isTrue()) {
            rval = try this.evaluateNode(_while.block.asAst());
        }

        return rval;
    }

    fn evaluateVar(this: *This, _var: *ast.AstVar) anyerror!val.Value {
        const var_ident = _var.ident.ident;
        const initial = try this.evaluateNode(_var.initializer);

        const current_scope = this.currentScope();
        const entry = try current_scope.variables.getOrPut(var_ident);
        if (entry.found_existing) {
            return err.raise(error.RuntimeError, _var.ident.token.location, "Redeclared identifier!", &this.err_msg);
        }

        entry.value_ptr.* = initial;

        return val.Value.None;
    }
};
