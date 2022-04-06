const std = @import("std");
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringArrayHashMap = std.StringArrayHashMap;
const StringArrayHashMapUnmanaged = std.StringArrayHashMapUnmanaged;

const CodeLocation = @import("parser.zig").CodeLocation;
const ast = @import("ast.zig");
const err = @import("error.zig");
const val = @import("value.zig");
const BucketArray = @import("bucket_array.zig").BucketArrayUnmanaged;

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
                entry.value_ptr.drop(this.variables.allocator);
            }
        }
        this.variables.deinit();

        for (this.rc_values.items) |value| {
            value.drop(this.rc_values.allocator);
        }
        this.rc_values.deinit();
    }

    fn addVariable(this: *This, ident: []const u8, value: val.Value, location: CodeLocation, out_err_msg: *err.ErrMsg) !*val.Value {
        const entry = try this.variables.getOrPut(ident);
        if (entry.found_existing) {
            return err.raise(error.RuntimeError, location, "Redeclared identifier!", out_err_msg);
        }

        entry.value_ptr.* = value;
        return entry.value_ptr;
    }

    fn findVariable(this: *This, ident: []const u8) ?*val.Value {
        var it: ?*Scope = this;

        while (it) |scope| {
            if (scope.variables.getPtr(ident)) |value| {
                return value;
            }
            it = scope.parent;
        }

        return null;
    }
};

pub const Evaluator = struct {
    allocator: std.mem.Allocator,
    global_scope: *Scope,
    scopes: BucketArray(8, Scope),
    // strings: StringArrayHashMap(u0),
    err_msg: err.ErrMsg,

    const This = @This();

    pub fn init(allocator: std.mem.Allocator) anyerror!This {
        var scopes = BucketArray(8, Scope).init();
        try scopes.push(allocator, Scope.init(allocator, null));

        return Evaluator{
            .allocator = allocator,
            .global_scope = scopes.top().?,
            .scopes = scopes,
            .err_msg = err.ErrMsg.default(),
        };
    }

    pub fn deinit(this: *This) void {
        this.global_scope.deinit();
        val.debug_print_rc_info();
    }

    fn currentScope(this: *This) *Scope {
        return this.scopes.top().?;
    }

    fn beginScope(this: *This) !void {
        return this.pushScope(Scope.init(this.allocator, this.currentScope()));
    }

    fn pushScope(this: *This, scope: Scope) !void {
        try this.scopes.push(this.allocator, scope);
    }

    fn endScope(this: *This) void {
        const scope = this.currentScope();
        std.debug.assert(scope != this.global_scope);

        scope.deinit();

        this.scopes.pop(this.allocator);
    }

    pub fn evaluate(this: *This, nodes: std.ArrayList(*ast.Ast)) anyerror!void {
        for (nodes.items) |node| {
            const v = try this.evaluateNode(node);
            replPrint(node.kind, v);
            v.drop(this.allocator);
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
            .Negate,
            .Not,
            => blk: {
                const unary = node.downcast(ast.AstUnary);
                break :blk this.evaluateUnary(unary);
            },

            // Binary
            .Add,
            .Subtract,
            .Multiply,
            .Divide,
            .Equal,
            .NotEqual,
            .Index,
            .Call,
            .Dot,
            => blk: {
                const binary = node.downcast(ast.AstBinary);
                break :blk this.evaluateBinary(binary);
            },

            .Assign => blk: {
                const assign = node.downcast(ast.AstBinary);
                try this.evaluateAssign(assign);
                break :blk val.Value.None;
            },

            // Blocks
            .Block, .Comma => blk: {
                const block = node.downcast(ast.AstBlock);
                break :blk this.evaluateBlock(block);
            },
            .List => blk: {
                const list = node.downcast(ast.AstBlock);
                break :blk this.evaluateList(list);
            },

            .If => blk: {
                const _if = node.downcast(ast.AstIf);
                break :blk this.evaluateIf(_if);
            },
            .While => blk: {
                const _while = node.downcast(ast.AstWhile);
                break :blk this.evaluateWhile(_while);
            },
            .Def => blk: {
                const def = node.downcast(ast.AstDef);
                try this.evaluateDef(def);
                break :blk val.Value.None;
            },
            .Var => blk: {
                const _var = node.downcast(ast.AstVar);
                try this.evaluateVar(_var);
                break :blk val.Value.None;
            },
            .Struct => blk: {
                const _struct = node.downcast(ast.AstStruct);
                try this.evaluateStruct(_struct);
                break :blk val.Value.None;
            },
            .Extend => blk: {
                const extend = node.downcast(ast.AstExtend);
                try this.evaluateExtend(extend);
                break :blk val.Value.None;
            },
        };
    }

    fn evaluateLiteral(this: *This, literal: *ast.AstLiteral) anyerror!val.Value {
        return switch (literal.literal) {
            .Bool => |value| val.Value{ .Bool = value },
            .Int => |value| val.Value{ .Int = value },
            .Num => |value| val.Value{ .Num = value },
            .Str => |value| blk: {
                const allocated = try this.allocator.dupe(u8, value);
                const v = val.Value{ .Str = try val.RefCounted([]const u8).create(this.allocator, allocated) };
                break :blk v;
            },
        };
    }

    fn evaluateIdent(this: *This, ident: *ast.AstIdent) anyerror!val.Value {
        if (this.currentScope().findVariable(ident.ident)) |variable_ptr| {
            return variable_ptr.dupe();
        }
        return err.raise(error.RuntimeError, ident.token.location, "Unknown identifier!", &this.err_msg);
    }

    fn evaluateUnary(this: *This, unary: *ast.AstUnary) anyerror!val.Value {
        return switch (unary.kind) {
            .Negate => this.evaluateNegate(unary.sub),
            .Not => this.evaluateNot(unary.sub),
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

    fn evaluateNot(this: *This, sub_node: *ast.Ast) anyerror!val.Value {
        const sub = try this.evaluateNode(sub_node);
        return switch (sub) {
            .Bool => |value| val.Value{ .Bool = !value },
            else => err.raise(error.RuntimeError, sub_node.token.location, "`!` requires its operand to be a `Bool`.", &this.err_msg),
        };
    }

    fn evaluateBinary(this: *This, binary: *ast.AstBinary) anyerror!val.Value {
        return switch (binary.kind) {
            .Add => this.evaluateAdd(binary.lhs, binary.rhs),
            .Subtract => this.evaluateSubtract(binary.lhs, binary.rhs),
            .Multiply => this.evaluateMultiply(binary.lhs, binary.rhs),
            .Divide => this.evaluateDivide(binary.lhs, binary.rhs),
            .Equal => this.evaluateEqual(binary.lhs, binary.rhs),
            .NotEqual => this.evaluateNotEqual(binary.lhs, binary.rhs),
            .Index => this.evaluateIndex(binary.lhs, binary.rhs),
            .Call => this.evaluateCall(binary.lhs, binary.rhs.downcast(ast.AstBlock)),
            .Dot => this.evaluateDot(binary.lhs, binary.rhs.downcast(ast.AstIdent)),
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

    fn evaluateEqual(this: *This, lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        const equal = lhs.equals(rhs);
        return val.Value{ .Bool = equal };
    }

    fn evaluateNotEqual(this: *This, lhs_node: *ast.Ast, rhs_node: *ast.Ast) anyerror!val.Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        const equal = lhs.equals(rhs);
        return val.Value{ .Bool = !equal };
    }

    fn evaluateIndex(this: *This, container_node: *ast.Ast, index_node: *ast.Ast) anyerror!val.Value {
        const container = try this.evaluateNode(container_node);
        defer container.drop(this.allocator);

        const index = switch (try this.evaluateNode(index_node)) {
            .Int => |value| value,
            else => return err.raise(error.RuntimeError, index_node.token.location, "Cannot index a container with something other than an `Int`.", &this.err_msg),
        };

        return switch (container) {
            .Str => |rc| this.evaluateIndexStr(rc, index, index_node.token.location),
            .List => |rc| this.evaluateIndexList(rc, index, index_node.token.location),
            else => err.raise(error.RuntimeError, container_node.token.location, "`[` requires its first operand to be either a 'Str' or a 'List'.", &this.err_msg),
        };
    }

    fn evaluateIndexStr(_: *This, _: *val.RefCounted([]const u8), _: i64, _: CodeLocation) anyerror!val.Value {
        err.todo("Implement Index operator for strings.");
    }

    fn evaluateIndexList(this: *This, list: *val.RefCounted(std.ArrayListUnmanaged(val.Value)), index: i64, index_location: CodeLocation) anyerror!val.Value {
        try arrayBoundsCheck(val.Value, list.value.items, @intCast(usize, index), index_location, &this.err_msg);
        return list.value.items[@intCast(usize, index)].dupe();
    }

    fn evaluateCall(this: *This, callable_node: *ast.Ast, args_node: *ast.AstBlock) anyerror!val.Value {
        const callable = try this.evaluateNode(callable_node);
        defer callable.drop(this.allocator);

        return switch (callable) {
            .Closure => |rc| this.evaluateCallClosure(rc, args_node, callable_node.token.location),
            .Struct => |rc| this.evaluateCallStruct(rc, args_node),
            else => err.raise(error.RuntimeError, callable_node.token.location, "Cannot call something that isn't a `Closure`.", &this.err_msg),
        };
    }

    fn setupScopeForClosureCall(this: *This, scope: *Scope, closure: *val.Closure, args: *ast.AstBlock) anyerror!void {
        var it = closure.closed_values.iterator();
        while (it.next()) |entry| {
            _ = try scope.addVariable(entry.key_ptr.*, entry.value_ptr.dupe(), args.token.location, &this.err_msg);
        }

        if (args.nodes.len != closure.params.len) {
            return err.raise(error.RuntimeError, args.token.location, try std.fmt.allocPrint(this.allocator, "Inccorect number of arguments! `{s}` expects {} arguments but was given {}.", .{ closure.name, closure.params.len, args.nodes.len }), &this.err_msg);
        }

        var i: usize = 0;
        while (i < closure.params.len) : (i += 1) {
            const arg_name = closure.params[i].name;
            const arg_value = try this.evaluateNode(args.nodes[i]);
            _ = try scope.addVariable(arg_name, arg_value, args.nodes[i].token.location, &this.err_msg);
        }
    }

    fn callClosure(this: *This, closure: *val.Closure) anyerror!val.Value {
        // @TODO:
        // Handle early return
        //
        return this.evaluateBlock(closure.code);
    }

    fn evaluateCallClosure(
        this: *This,
        closure: *val.RefCounted(val.Closure),
        args_node: *ast.AstBlock,
        location: CodeLocation,
    ) anyerror!val.Value {
        var closure_scope = Scope.init(this.allocator, this.global_scope);
        _ = try closure_scope.addVariable(closure.value.name, val.Value{ .Closure = closure.dupe() }, location, &this.err_msg);
        try this.setupScopeForClosureCall(&closure_scope, &closure.value, args_node);

        try this.pushScope(closure_scope);
        defer this.endScope();
        return this.callClosure(&closure.value);
    }

    fn evaluateCallStruct(
        this: *This,
        _struct: *val.RefCounted(val.Struct),
        args_node: *ast.AstBlock,
    ) anyerror!val.Value {
        if (args_node.nodes.len > _struct.value.fields.len) {
            return err.raise(error.RuntimeError, args_node.nodes[_struct.value.fields.len].token.location, "Too many arguments passed to struct initializer.", &this.err_msg);
        }

        var fields = StringArrayHashMapUnmanaged(val.Value){};

        for (_struct.value.fields) |field, i| {
            if (i >= args_node.nodes.len) {
                try fields.putNoClobber(this.allocator, field.name, val.Value.None);
            } else {
                const field_value = try this.evaluateNode(args_node.nodes[i]);
                try fields.putNoClobber(this.allocator, field.name, field_value);
            }
        }

        var instance_val = val.Instance.init(_struct.dupe(), fields);
        return val.Value{ .Instance = try val.RefCounted(val.Instance).create(this.allocator, instance_val) };
    }

    fn evaluateDot(this: *This, instance_node: *ast.Ast, field_ident_node: *ast.AstIdent) anyerror!val.Value {
        const instance = try this.evaluateNode(instance_node);
        defer instance.drop(this.allocator);

        return switch (instance) {
            .Instance => |rc| this.evaluateDotInstance(rc, field_ident_node),
            else => err.raise(error.RuntimeError, instance_node.token.location, "`.` requires its first operand to be an instance of a struct.", &this.err_msg),
        };
    }

    fn evaluateDotInstance(
        this: *This,
        instance: *val.RefCounted(val.Instance),
        field_ident_node: *ast.AstIdent,
    ) anyerror!val.Value {
        const ident = field_ident_node.ident;

        if (instance.value.fields.getPtr(ident)) |field_ptr| {
            return field_ptr.dupe();
        } else if (instance.value._struct.value.methods.get(ident)) |method| {
            const receiver = val.Value{ .Instance = instance };
            const bound_closure = try method.value.makeBound(this.allocator, receiver, field_ident_node.token.location, &this.err_msg);
            return val.Value{ .Closure = try val.RefCounted(val.Closure).create(this.allocator, bound_closure) };
        } else {
            return err.raise(error.RuntimeError, field_ident_node.token.location, "Instance does not have a field with this name.", &this.err_msg);
        }
    }

    fn evaluateAssign(this: *This, assign: *ast.AstBinary) anyerror!void {
        switch (assign.lhs.kind) {
            .Ident => try this.evaluateAssignIdent(assign.lhs.downcast(ast.AstIdent), assign.rhs),
            .Index => try this.evaluateAssignIndex(assign.lhs.downcast(ast.AstBinary), assign.rhs),
            .Dot => try this.evaluateAssignDot(assign.lhs.downcast(ast.AstBinary), assign.rhs),
            else => return err.raise(error.RuntimeError, assign.lhs.token.location, "Cannot assign to this kind of expression!", &this.err_msg),
        }
    }

    fn evaluateAssignIdent(this: *This, ident: *ast.AstIdent, expr: *ast.Ast) anyerror!void {
        const value_ptr = this.currentScope().findVariable(ident.ident) orelse return err.raise(error.RuntimeError, ident.token.location, "Unknown identifier!", &this.err_msg);
        const new_value = try this.evaluateNode(expr);
        value_ptr.drop(this.allocator);
        value_ptr.* = new_value;

        replPrintAssign(ident.ident, value_ptr.*);
    }

    fn evaluateAssignIndex(this: *This, target: *ast.AstBinary, expr: *ast.Ast) anyerror!void {
        const container = try this.evaluateNode(target.lhs);
        defer container.drop(this.allocator);

        switch (container) {
            .Str => err.todo("Implement index-assign for Str."),
            .List => |rc| try this.evaluateAssignIndexList(rc, target.rhs, expr),
            else => return err.raise(error.RuntimeError, target.lhs.token.location, "Cannot index something that isn't a `Str` or a `List`.", &this.err_msg),
        }
    }

    fn evaluateAssignDot(this: *This, target: *ast.AstBinary, expr: *ast.Ast) anyerror!void {
        const instance = try this.evaluateNode(target.lhs);
        defer instance.drop(this.allocator);

        switch (instance) {
            .Instance => |rc| try this.evaluateAssignDotInstance(rc, target.rhs.downcast(ast.AstIdent), expr),
            else => return err.raise(error.RuntimeError, target.lhs.token.location, "`.` requires its first operand to be an instance of a struct.", &this.err_msg),
        }
    }

    fn evaluateAssignDotInstance(
        this: *This,
        instance: *val.RefCounted(val.Instance),
        field_ident_node: *ast.AstIdent,
        expr: *ast.Ast,
    ) anyerror!void {
        const field_ident = field_ident_node.ident;
        var field_ptr = instance.value.fields.getPtr(field_ident) orelse return err.raise(error.RuntimeError, field_ident_node.token.location, "Instance does not have a field with this name.", &this.err_msg);
        field_ptr.drop(this.allocator);

        field_ptr.* = try this.evaluateNode(expr);
    }

    fn evaluateAssignIndexList(this: *This, list: *val.RefCounted(std.ArrayListUnmanaged(val.Value)), index_node: *ast.Ast, expr: *ast.Ast) anyerror!void {
        const index = switch (try this.evaluateNode(index_node)) {
            .Int => |value| value,
            else => return err.raise(error.RuntimeError, index_node.token.location, "Cannot index a container with something other than an `Int`.", &this.err_msg),
        };

        const value_ptr = &list.value.items[@intCast(usize, index)];
        value_ptr.drop(this.allocator);

        const new_value = try this.evaluateNode(expr);
        value_ptr.* = new_value;
    }

    fn evaluateBlock(this: *This, block: *ast.AstBlock) anyerror!val.Value {
        var trval: val.Value = .None;

        if (block.kind == .Block) {
            try this.beginScope();
        }

        defer if (block.kind == .Block) {
            this.endScope();
        };

        for (block.nodes) |node| {
            trval = try this.evaluateNode(node);

            if (trval.isRefCounted()) {
                try this.currentScope().rc_values.append(trval);
            }
        }

        const rval = trval.dupe();

        return rval;
    }

    fn evaluateList(this: *This, list: *ast.AstBlock) anyerror!val.Value {
        var items = try std.ArrayListUnmanaged(val.Value).initCapacity(this.allocator, list.nodes.len);

        for (list.nodes) |node| {
            try items.append(this.allocator, try this.evaluateNode(node));
        }

        const rc = try val.RefCounted(std.ArrayListUnmanaged(val.Value)).create(this.allocator, items);
        return val.Value{ .List = rc };
    }

    fn evaluateIf(this: *This, _if: *ast.AstIf) anyerror!val.Value {
        const cond = try this.evaluateNode(_if.condition);
        defer if (cond.isRefCounted()) {
            cond.drop(this.allocator);
        };

        const rval = if (cond.isTrue())
            this.evaluateNode(_if.then_block.asAst())
        else if (_if.else_block) |else_block|
            this.evaluateNode(else_block)
        else
            @as(val.Value, val.Value.None);

        return rval;
    }

    fn evaluateWhile(this: *This, _while: *ast.AstWhile) anyerror!val.Value {
        var rval: val.Value = .None;

        while (true) {
            const cond = try this.evaluateNode(_while.condition);
            defer if (cond.isRefCounted()) cond.drop(this.allocator);

            if (!cond.isTrue()) {
                break;
            }

            rval = try this.evaluateNode(_while.block.asAst());
        }

        return rval;
    }

    fn evaluateDef(this: *This, def: *ast.AstDef) anyerror!void {
        const closure_rc = try this.createDefClosure(def);
        const closure = val.Value{ .Closure = closure_rc };
        _ = try this.currentScope().addVariable(def.name, closure, def.token.location, &this.err_msg);

        replPrintAssign(def.name, closure);
    }

    fn createDefClosure(this: *This, def: *ast.AstDef) anyerror!*val.RefCounted(val.Closure) {
        // @TODO:
        // Find closure values and close them.
        //

        var params = try ArrayListUnmanaged(val.Closure.Parameter).initCapacity(this.allocator, def.params.nodes.len);
        for (def.params.nodes) |param_node| {
            switch (param_node.kind) {
                .Ident => {
                    const ident = param_node.downcast(ast.AstIdent);
                    const param = val.Closure.Parameter{ .name = ident.ident };
                    try params.append(this.allocator, param);
                },
                else => unreachable,
            }
        }

        const closed_values = StringArrayHashMapUnmanaged(val.Value){};

        return try val.RefCounted(val.Closure).create(this.allocator, val.Closure.init(def.name, params.items, def.body, closed_values));
    }

    fn evaluateVar(this: *This, _var: *ast.AstVar) anyerror!void {
        const var_ident = _var.ident.ident;
        const initial = try this.evaluateNode(_var.initializer);

        const value_ptr = try this.currentScope().addVariable(var_ident, initial, _var.ident.token.location, &this.err_msg);

        replPrintAssign(var_ident, value_ptr.*);
    }

    fn evaluateStruct(this: *This, _struct: *ast.AstStruct) anyerror!void {
        const ident = _struct.ident.ident;
        var fields = ArrayListUnmanaged(val.Struct.Field){};

        for (_struct.body.nodes) |node| {
            switch (node.kind) {
                .Ident => {
                    const node_id = node.downcast(ast.AstIdent);
                    try fields.append(this.allocator, val.Struct.Field{ .name = node_id.ident });
                },
                else => return err.raise(error.RuntimeError, node.token.location, "Expected a field name in struct body.", &this.err_msg),
            }
        }

        const struct_val = val.Value{ .Struct = try val.RefCounted(val.Struct).create(this.allocator, val.Struct.init(ident, fields.items)) };

        const struct_ptr = try this.currentScope().addVariable(ident, struct_val, _struct.token.location, &this.err_msg);

        replPrintAssign(ident, struct_ptr.*);
    }

    fn evaluateExtend(this: *This, extend: *ast.AstExtend) anyerror!void {
        const struct_value = try this.evaluateNode(extend._struct);
        defer struct_value.drop(this.allocator);

        const _struct = switch (struct_value) {
            .Struct => |rc| rc,
            else => return err.raise(error.RuntimeError, extend._struct.token.location, "Can only extend structs.", &this.err_msg),
        };

        for (extend.body.nodes) |method_node| {
            switch (method_node.kind) {
                .Def => {
                    const def = method_node.downcast(ast.AstDef);
                    const method = try this.createDefClosure(def);
                    try _struct.value.methods.put(this.allocator, def.name, method);
                },
                else => return err.raise(error.RuntimeError, method_node.token.location, "Can only extend structs with methods.", &this.err_msg),
            }
        }
    }
};

fn replPrint(kind: ast.AstKind, value: val.Value) void {
    switch (kind) {
        .Assign, .Var, .Def, .Struct, .Extend, .Block, .If, .While => {},
        else => std.debug.print("{}\n", .{value}),
    }
}

fn replPrintAssign(ident: []const u8, value: val.Value) void {
    std.debug.print("{s} = {}\n", .{ ident, value });
}

fn arrayBoundsCheck(comptime T: type, array: []T, index: usize, index_location: CodeLocation, out_err_msg: *err.ErrMsg) error{RuntimeError}!void {
    if (index < 0 or index >= array.len) {
        return err.raise(error.RuntimeError, index_location, "Array bounds check failure!", out_err_msg);
    }
}
