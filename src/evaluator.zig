const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringArrayHashMap = std.StringArrayHashMap;
const StringArrayHashMapUnmanaged = std.StringArrayHashMapUnmanaged;

const CodeLocation = @import("parser.zig").CodeLocation;

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
const AstExtend = ast.AstExtend;

const err = @import("error.zig");
const ErrMsg = err.ErrMsg;
const raise = err.raise;
const todo = err.todo;

const val = @import("value.zig");
const Value = val.Value;
const Closure = val.Closure;
const Struct = val.Struct;
const Instance = val.Instance;

const GarbageCollector = @import("gc.zig").GarbageCollector;

const BucketArrayUnmanaged = @import("bucket_array.zig").BucketArrayUnmanaged;

const DEBUG_PRINT_BASE_NODE_RESULTS = false;
pub const SCOPE_BUCKET_SIZE = 8;

pub const Scope = struct {
    parent: ?*This,
    variables: StringArrayHashMap(Value),

    const This = @This();

    fn init(allocator: Allocator, parent: ?*This) This {
        return Scope{
            .parent = parent,
            .variables = StringArrayHashMap(Value).init(allocator),
        };
    }

    fn deinit(this: *This) void {
        this.variables.deinit();
    }

    fn addVariable(this: *This, ident: []const u8, value: Value, location: CodeLocation, out_err_msg: *ErrMsg) !*Value {
        const entry = try this.variables.getOrPut(ident);
        if (entry.found_existing) {
            return raise(error.RuntimeError, location, "Redeclared identifier!", out_err_msg);
        }

        entry.value_ptr.* = value;
        return entry.value_ptr;
    }

    fn findVariable(this: *This, ident: []const u8) ?*Value {
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
    allocator: Allocator,
    gc: GarbageCollector,
    global_scope: *Scope,
    scopes: BucketArrayUnmanaged(SCOPE_BUCKET_SIZE, Scope),
    err_msg: ErrMsg,

    const This = @This();

    pub fn init(allocator: Allocator) anyerror!This {
        var scopes = BucketArrayUnmanaged(SCOPE_BUCKET_SIZE, Scope).init();
        try scopes.push(allocator, Scope.init(allocator, null));

        return Evaluator{
            .allocator = allocator,
            .gc = GarbageCollector.init(allocator),
            .global_scope = scopes.top().?,
            .scopes = scopes,
            .err_msg = ErrMsg.default(),
        };
    }

    pub fn deinit(this: *This) void {
        this.global_scope.deinit();
        this.gc.deinit();
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

        this.gc.collectGarbage(&this.scopes);
    }

    pub fn evaluate(this: *This, nodes: std.ArrayList(*Ast)) anyerror!void {
        for (nodes.items) |node| {
            const v = try this.evaluateNode(node);
            if (DEBUG_PRINT_BASE_NODE_RESULTS) {
                std.debug.print("?> {}\n------------\n", .{v});
            }
        }
    }

    fn evaluateNode(this: *This, node: *Ast) anyerror!Value {
        return switch (node.kind) {
            // Literals
            .Bool, .Int, .Num, .Str => blk: {
                const l = node.downcast(AstLiteral);
                break :blk this.evaluateLiteral(l);
            },
            .Ident => blk: {
                const ident = node.downcast(AstIdent);
                break :blk this.evaluateIdent(ident);
            },

            // Unary
            .Negate,
            .Not,
            .Println,
            => blk: {
                const unary = node.downcast(AstUnary);
                break :blk this.evaluateUnary(unary);
            },

            // Binary
            .Add,
            .Subtract,
            .Multiply,
            .Divide,
            .Equal,
            .NotEqual,
            .LessThan,
            .GreaterThan,
            .Index,
            .Call,
            .Dot,
            => blk: {
                const binary = node.downcast(AstBinary);
                break :blk this.evaluateBinary(binary);
            },

            .Assign => blk: {
                const assign = node.downcast(AstBinary);
                try this.evaluateAssign(assign);
                break :blk Value.None;
            },

            // Blocks
            .Block, .Comma => blk: {
                const block = node.downcast(AstBlock);
                break :blk this.evaluateBlock(block);
            },
            .List => blk: {
                const list = node.downcast(AstBlock);
                break :blk this.evaluateList(list);
            },

            .If => blk: {
                const _if = node.downcast(AstIf);
                break :blk this.evaluateIf(_if);
            },
            .While => blk: {
                const _while = node.downcast(AstWhile);
                break :blk this.evaluateWhile(_while);
            },
            .Def => blk: {
                const def = node.downcast(AstDef);
                try this.evaluateDef(def);
                break :blk Value.None;
            },
            .Var => blk: {
                const _var = node.downcast(AstVar);
                try this.evaluateVar(_var);
                break :blk Value.None;
            },
            .Struct => blk: {
                const _struct = node.downcast(AstStruct);
                try this.evaluateStruct(_struct);
                break :blk Value.None;
            },
            .Extend => blk: {
                const extend = node.downcast(AstExtend);
                try this.evaluateExtend(extend);
                break :blk Value.None;
            },
        };
    }

    fn evaluateLiteral(this: *This, literal: *AstLiteral) anyerror!Value {
        return switch (literal.literal) {
            .Bool => |value| Value{ .Bool = value },
            .Int => |value| Value{ .Int = value },
            .Num => |value| Value{ .Num = value },
            .Str => |value| blk: {
                const allocated = try this.gc.copyString(value);
                const v = Value{ .Str = allocated };
                break :blk v;
            },
        };
    }

    fn evaluateIdent(this: *This, ident: *AstIdent) anyerror!Value {
        if (this.currentScope().findVariable(ident.ident)) |variable_ptr| {
            return variable_ptr.*;
        }
        return raise(error.RuntimeError, ident.token.location, "Unknown identifier!", &this.err_msg);
    }

    fn evaluateUnary(this: *This, unary: *AstUnary) anyerror!Value {
        return switch (unary.kind) {
            .Negate => this.evaluateNegate(unary.sub),
            .Not => this.evaluateNot(unary.sub),
            .Println => blk: {
                try this.evaluatePrintln(unary.sub);
                break :blk Value.None;
            },
            else => raise(error.RuntimeError, unary.token.location, "Invalid unary operation", &this.err_msg),
        };
    }

    fn evaluateNegate(this: *This, sub_node: *Ast) anyerror!Value {
        const sub = try this.evaluateNode(sub_node);

        return switch (sub) {
            .Int => |value| Value{ .Int = -value },
            .Num => |value| Value{ .Num = -value },
            else => raise(error.RuntimeError, sub_node.token.location, "`-` requires its operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateNot(this: *This, sub_node: *Ast) anyerror!Value {
        const sub = try this.evaluateNode(sub_node);
        return switch (sub) {
            .Bool => |value| Value{ .Bool = !value },
            else => raise(error.RuntimeError, sub_node.token.location, "`!` requires its operand to be a `Bool`.", &this.err_msg),
        };
    }

    fn evaluatePrintln(this: *This, sub_node: *Ast) anyerror!void {
        const args = sub_node.downcast(AstBlock);
        var stdout = std.io.getStdOut().writer();

        for (args.nodes) |node| {
            const arg = try this.evaluateNode(node);
            try stdout.print("{} ", .{arg});
        }

        _ = try stdout.write("\n");
    }

    fn evaluateBinary(this: *This, binary: *AstBinary) anyerror!Value {
        return switch (binary.kind) {
            .Add => this.evaluateAdd(binary.lhs, binary.rhs),
            .Subtract => this.evaluateSubtract(binary.lhs, binary.rhs),
            .Multiply => this.evaluateMultiply(binary.lhs, binary.rhs),
            .Divide => this.evaluateDivide(binary.lhs, binary.rhs),
            .Equal => this.evaluateEqual(binary.lhs, binary.rhs),
            .NotEqual => this.evaluateNotEqual(binary.lhs, binary.rhs),
            .LessThan => this.evaluateLessThan(binary.lhs, binary.rhs),
            .GreaterThan => this.evaluateGreaterThan(binary.lhs, binary.rhs),
            .Index => this.evaluateIndex(binary.lhs, binary.rhs),
            .Call => this.evaluateCall(binary.lhs, binary.rhs.downcast(AstBlock)),
            .Dot => this.evaluateDot(binary.lhs, binary.rhs.downcast(AstIdent)),
            else => raise(error.RuntimeError, binary.token.location, "Invalid binary operation", &this.err_msg),
        };
    }

    fn evaluateAdd(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Int = lhs_value + rhs_value },
                .Num => |rhs_value| Value{ .Num = @intToFloat(f64, lhs_value) + rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`+` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Num = lhs_value + @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| Value{ .Num = lhs_value + rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`+` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => raise(error.RuntimeError, lhs_node.token.location, "`+` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateSubtract(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Int = lhs_value - rhs_value },
                .Num => |rhs_value| Value{ .Num = @intToFloat(f64, lhs_value) - rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`-` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Num = lhs_value - @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| Value{ .Num = lhs_value - rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`-` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => raise(error.RuntimeError, lhs_node.token.location, "`-` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateMultiply(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Int = lhs_value * rhs_value },
                .Num => |rhs_value| Value{ .Num = @intToFloat(f64, lhs_value) * rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`*` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Num = lhs_value * @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| Value{ .Num = lhs_value * rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`*` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => raise(error.RuntimeError, lhs_node.token.location, "`*` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateDivide(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Int = @divTrunc(lhs_value, rhs_value) },
                .Num => |rhs_value| Value{ .Num = @divTrunc(@intToFloat(f64, lhs_value), rhs_value) },
                else => raise(error.RuntimeError, rhs_node.token.location, "`/` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Num = @divTrunc(lhs_value, @intToFloat(f64, rhs_value)) },
                .Num => |rhs_value| Value{ .Num = @divTrunc(lhs_value, rhs_value) },
                else => raise(error.RuntimeError, rhs_node.token.location, "`/` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => raise(error.RuntimeError, lhs_node.token.location, "`/` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateEqual(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        const equal = lhs.equals(rhs);
        return Value{ .Bool = equal };
    }

    fn evaluateNotEqual(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        const equal = lhs.equals(rhs);
        return Value{ .Bool = !equal };
    }

    fn evaluateLessThan(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Bool = lhs_value < rhs_value },
                .Num => |rhs_value| Value{ .Bool = @intToFloat(f64, lhs_value) < rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`<` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Bool = lhs_value < @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| Value{ .Bool = lhs_value < rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`<` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => raise(error.RuntimeError, lhs_node.token.location, "`<` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateGreaterThan(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Bool = lhs_value > rhs_value },
                .Num => |rhs_value| Value{ .Bool = @intToFloat(f64, lhs_value) > rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`>` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Bool = lhs_value > @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| Value{ .Bool = lhs_value > rhs_value },
                else => raise(error.RuntimeError, rhs_node.token.location, "`>` requires its second operand to be either an `Int` or a `Num`.", &this.err_msg),
            },
            else => raise(error.RuntimeError, lhs_node.token.location, "`>` requires its first operand to be either an `Int` or a `Num`.", &this.err_msg),
        };
    }

    fn evaluateIndex(this: *This, container_node: *Ast, index_node: *Ast) anyerror!Value {
        const container = try this.evaluateNode(container_node);

        const index = switch (try this.evaluateNode(index_node)) {
            .Int => |value| value,
            else => return raise(error.RuntimeError, index_node.token.location, "Cannot index a container with something other than an `Int`.", &this.err_msg),
        };

        return switch (container) {
            .Str => |value| this.evaluateIndexStr(value, index, index_node.token.location),
            .List => |value| this.evaluateIndexList(value, index, index_node.token.location),
            else => raise(error.RuntimeError, container_node.token.location, "`[` requires its first operand to be either a 'Str' or a 'List'.", &this.err_msg),
        };
    }

    fn evaluateIndexStr(_: *This, _: []const u8, _: i64, _: CodeLocation) anyerror!Value {
        todo("Implement Index operator for strings.");
    }

    fn evaluateIndexList(this: *This, list: *std.ArrayListUnmanaged(Value), index: i64, index_location: CodeLocation) anyerror!Value {
        try arrayBoundsCheck(Value, list.items, @intCast(usize, index), index_location, &this.err_msg);
        return list.items[@intCast(usize, index)];
    }

    fn evaluateCall(this: *This, callable_node: *Ast, args_node: *AstBlock) anyerror!Value {
        const callable = try this.evaluateNode(callable_node);

        return switch (callable) {
            .Closure => |rc| this.evaluateCallClosure(rc, args_node, callable_node.token.location),
            .Struct => |rc| this.evaluateCallStruct(rc, args_node),
            else => raise(error.RuntimeError, callable_node.token.location, "Cannot call something that isn't a `Closure`.", &this.err_msg),
        };
    }

    fn setupScopeForClosureCall(this: *This, scope: *Scope, closure: *Closure, args: *AstBlock) anyerror!void {
        var it = closure.closed_values.iterator();
        while (it.next()) |entry| {
            _ = try scope.addVariable(entry.key_ptr.*, entry.value_ptr.*, args.token.location, &this.err_msg);
        }

        if (args.nodes.len != closure.params.len) {
            return raise(error.RuntimeError, args.token.location, try std.fmt.allocPrint(this.allocator, "Inccorect number of arguments! `{s}` expects {} arguments but was given {}.", .{ closure.name, closure.params.len, args.nodes.len }), &this.err_msg);
        }

        var i: usize = 0;
        while (i < closure.params.len) : (i += 1) {
            const arg_name = closure.params[i].name;
            const arg_value = try this.evaluateNode(args.nodes[i]);
            _ = try scope.addVariable(arg_name, arg_value, args.nodes[i].token.location, &this.err_msg);
        }
    }

    fn callClosure(this: *This, closure: *Closure) anyerror!Value {
        // @TODO:
        // Handle early return
        //
        return this.evaluateBlock(closure.code);
    }

    fn evaluateCallClosure(
        this: *This,
        closure: *Closure,
        args_node: *AstBlock,
        location: CodeLocation,
    ) anyerror!Value {
        var closure_scope = Scope.init(this.allocator, this.global_scope);
        _ = try closure_scope.addVariable(closure.name, Value{ .Closure = closure }, location, &this.err_msg);

        try this.setupScopeForClosureCall(&closure_scope, closure, args_node);

        try this.pushScope(closure_scope);
        defer this.endScope();

        return this.callClosure(closure);
    }

    fn evaluateCallStruct(
        this: *This,
        _struct: *Struct,
        args_node: *AstBlock,
    ) anyerror!Value {
        if (args_node.nodes.len > _struct.fields.len) {
            return raise(error.RuntimeError, args_node.nodes[_struct.fields.len].token.location, "Too many arguments passed to struct initializer.", &this.err_msg);
        }

        var fields = StringArrayHashMapUnmanaged(Value){};

        for (_struct.fields) |field, i| {
            if (i >= args_node.nodes.len) {
                try fields.putNoClobber(this.allocator, field.name, Value.None);
            } else {
                const field_value = try this.evaluateNode(args_node.nodes[i]);
                try fields.putNoClobber(this.allocator, field.name, field_value);
            }
        }

        var instance_val = Instance.init(_struct, fields);
        const allocated_instance = try this.gc.allocateInstance(instance_val);
        return Value{ .Instance = allocated_instance };
    }

    fn evaluateDot(this: *This, instance_node: *Ast, field_ident_node: *AstIdent) anyerror!Value {
        const instance = try this.evaluateNode(instance_node);

        return switch (instance) {
            .Instance => |rc| this.evaluateDotInstance(rc, field_ident_node),
            else => raise(error.RuntimeError, instance_node.token.location, "`.` requires its first operand to be an instance of a struct.", &this.err_msg),
        };
    }

    fn evaluateDotInstance(
        this: *This,
        instance: *Instance,
        field_ident_node: *AstIdent,
    ) anyerror!Value {
        const ident = field_ident_node.ident;

        if (instance.fields.getPtr(ident)) |field_ptr| {
            return field_ptr.*;
        } else if (instance._struct.methods.get(ident)) |method| {
            const receiver = Value{ .Instance = instance };
            const bound_closure = try method.makeBound(this.allocator, receiver, field_ident_node.token.location, &this.err_msg);
            const allocated_bound_closure = try this.gc.allocateClosure(bound_closure);
            return Value{ .Closure = allocated_bound_closure };
        } else {
            return raise(error.RuntimeError, field_ident_node.token.location, "Instance does not have a field with this name.", &this.err_msg);
        }
    }

    fn evaluateAssign(this: *This, assign: *AstBinary) anyerror!void {
        switch (assign.lhs.kind) {
            .Ident => try this.evaluateAssignIdent(assign.lhs.downcast(AstIdent), assign.rhs),
            .Index => try this.evaluateAssignIndex(assign.lhs.downcast(AstBinary), assign.rhs),
            .Dot => try this.evaluateAssignDot(assign.lhs.downcast(AstBinary), assign.rhs),
            else => return raise(error.RuntimeError, assign.lhs.token.location, "Cannot assign to this kind of expression!", &this.err_msg),
        }
    }

    fn evaluateAssignIdent(this: *This, ident: *AstIdent, expr: *Ast) anyerror!void {
        const value_ptr = this.currentScope().findVariable(ident.ident) orelse return raise(error.RuntimeError, ident.token.location, "Unknown identifier!", &this.err_msg);
        const new_value = try this.evaluateNode(expr);
        value_ptr.* = new_value;
    }

    fn evaluateAssignIndex(this: *This, target: *AstBinary, expr: *Ast) anyerror!void {
        const container = try this.evaluateNode(target.lhs);

        switch (container) {
            .Str => todo("Implement index-assign for Str."),
            .List => |rc| try this.evaluateAssignIndexList(rc, target.rhs, expr),
            else => return raise(error.RuntimeError, target.lhs.token.location, "Cannot index something that isn't a `Str` or a `List`.", &this.err_msg),
        }
    }

    fn evaluateAssignDot(this: *This, target: *AstBinary, expr: *Ast) anyerror!void {
        const instance = try this.evaluateNode(target.lhs);

        switch (instance) {
            .Instance => |rc| try this.evaluateAssignDotInstance(rc, target.rhs.downcast(AstIdent), expr),
            else => return raise(error.RuntimeError, target.lhs.token.location, "`.` requires its first operand to be an instance of a struct.", &this.err_msg),
        }
    }

    fn evaluateAssignDotInstance(
        this: *This,
        instance: *Instance,
        field_ident_node: *AstIdent,
        expr: *Ast,
    ) anyerror!void {
        const field_ident = field_ident_node.ident;
        var field_ptr = instance.fields.getPtr(field_ident) orelse return raise(error.RuntimeError, field_ident_node.token.location, "Instance does not have a field with this name.", &this.err_msg);

        field_ptr.* = try this.evaluateNode(expr);
    }

    fn evaluateAssignIndexList(this: *This, list: *std.ArrayListUnmanaged(Value), index_node: *Ast, expr: *Ast) anyerror!void {
        const index = switch (try this.evaluateNode(index_node)) {
            .Int => |value| value,
            else => return raise(error.RuntimeError, index_node.token.location, "Cannot index a container with something other than an `Int`.", &this.err_msg),
        };

        const value_ptr = &list.items[@intCast(usize, index)];

        const new_value = try this.evaluateNode(expr);
        value_ptr.* = new_value;
    }

    fn evaluateBlock(this: *This, block: *AstBlock) anyerror!Value {
        var trval: Value = .None;

        if (block.kind == .Block) {
            try this.beginScope();
        }

        defer if (block.kind == .Block) {
            this.endScope();
        };

        for (block.nodes) |node| {
            trval = try this.evaluateNode(node);
        }

        return trval;
    }

    fn evaluateList(this: *This, list: *AstBlock) anyerror!Value {
        var items = try this.gc.allocateList();
        items.* = try std.ArrayListUnmanaged(Value).initCapacity(this.allocator, list.nodes.len);

        for (list.nodes) |node| {
            try items.append(this.allocator, try this.evaluateNode(node));
        }

        return Value{ .List = items };
    }

    fn evaluateIf(this: *This, _if: *AstIf) anyerror!Value {
        const cond = try this.evaluateNode(_if.condition);

        const rval = if (cond.isTrue())
            this.evaluateNode(_if.then_block.asAst())
        else if (_if.else_block) |else_block|
            this.evaluateNode(else_block)
        else
            @as(Value, Value.None);

        return rval;
    }

    fn evaluateWhile(this: *This, _while: *AstWhile) anyerror!Value {
        var rval: Value = .None;

        while (true) {
            const cond = try this.evaluateNode(_while.condition);

            if (!cond.isTrue()) {
                break;
            }

            rval = try this.evaluateNode(_while.block.asAst());
        }

        return rval;
    }

    fn evaluateDef(this: *This, def: *AstDef) anyerror!void {
        const closure_rc = try this.createDefClosure(def);
        const closure = Value{ .Closure = closure_rc };
        _ = try this.currentScope().addVariable(def.name, closure, def.token.location, &this.err_msg);
    }

    fn createDefClosure(this: *This, def: *AstDef) anyerror!*Closure {
        // @TODO:
        // Find closure values and close them.
        //

        var params = try ArrayListUnmanaged(Closure.Parameter).initCapacity(this.allocator, def.params.nodes.len);
        for (def.params.nodes) |param_node| {
            switch (param_node.kind) {
                .Ident => {
                    const ident = param_node.downcast(AstIdent);
                    const param = Closure.Parameter{ .name = ident.ident };
                    try params.append(this.allocator, param);
                },
                else => unreachable,
            }
        }

        const closed_values = StringArrayHashMapUnmanaged(Value){};

        return try this.gc.allocateClosure(Closure.init(def.name, params.items, def.body, closed_values));
    }

    fn evaluateVar(this: *This, _var: *AstVar) anyerror!void {
        const var_ident = _var.ident.ident;
        const initial = try this.evaluateNode(_var.initializer);

        _ = try this.currentScope().addVariable(var_ident, initial, _var.ident.token.location, &this.err_msg);
    }

    fn evaluateStruct(this: *This, _struct: *AstStruct) anyerror!void {
        const ident = _struct.ident.ident;
        var fields = ArrayListUnmanaged(Struct.Field){};

        for (_struct.body.nodes) |node| {
            switch (node.kind) {
                .Ident => {
                    const node_id = node.downcast(AstIdent);
                    try fields.append(this.allocator, Struct.Field{ .name = node_id.ident });
                },
                else => return raise(error.RuntimeError, node.token.location, "Expected a field name in struct body.", &this.err_msg),
            }
        }

        const struct_val = Value{ .Struct = try this.gc.allocateStruct(Struct.init(ident, fields.items)) };

        _ = try this.currentScope().addVariable(ident, struct_val, _struct.token.location, &this.err_msg);
    }

    fn evaluateExtend(this: *This, extend: *AstExtend) anyerror!void {
        const struct_value = try this.evaluateNode(extend._struct);

        const _struct = switch (struct_value) {
            .Struct => |rc| rc,
            else => return raise(error.RuntimeError, extend._struct.token.location, "Can only extend structs.", &this.err_msg),
        };

        for (extend.body.nodes) |method_node| {
            switch (method_node.kind) {
                .Def => {
                    const def = method_node.downcast(AstDef);
                    const method = try this.createDefClosure(def);
                    try _struct.methods.put(this.allocator, def.name, method);
                },
                else => return raise(error.RuntimeError, method_node.token.location, "Can only extend structs with methods.", &this.err_msg),
            }
        }
    }
};

fn arrayBoundsCheck(
    comptime T: type,
    array: []T,
    index: usize,
    index_location: CodeLocation,
    out_err_msg: *ErrMsg,
) error{RuntimeError}!void {
    if (index < 0 or index >= array.len) {
        return raise(error.RuntimeError, index_location, "Array bounds check failure!", out_err_msg);
    }
}
