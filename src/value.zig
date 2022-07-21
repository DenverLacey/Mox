const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringArrayHashMapUnmanaged = std.StringArrayHashMapUnmanaged;

const AstBlock = @import("ast.zig").AstBlock;
const err = @import("error.zig");
const ErrMsg = err.ErrMsg;
const raise = err.raise;
const todo = err.todo;

const CodeLocation = @import("parser.zig").CodeLocation;

pub const Value = union(ValueKind) {
    None,
    Bool: bool,
    Char: Char,
    Int: i64,
    Num: f64,
    Str: []const u8,
    Range: Range,
    List: *List,
    Closure: *Closure,
    Struct: *Struct,
    Instance: *Instance,

    const This = @This();

    pub fn isTrue(this: This) bool {
        return switch (this) {
            .None => false,
            .Bool => |value| value,
            .Char => |value| value != 0,
            .Int => |value| value != 0,
            .Num => |value| value != 0.0,
            .Str => |value| value.len != 0,
            .Range => true,
            .List => |value| value.items.len != 0,
            .Closure => true,
            .Struct => true,
            .Instance => true,
        };
    }

    pub fn equals(this: This, other: This) bool {
        return switch (this) {
            .None => switch (other) {
                .None => true,
                else => false,
            },
            .Bool => |value| switch (other) {
                .Bool => |other_value| value == other_value,
                else => false,
            },
            .Char => |value| switch (other) {
                .Char => |other_value| value == other_value,
                else => false,
            },
            .Int => |value| switch (other) {
                .Int => |other_value| value == other_value,
                else => false,
            },
            .Num => |value| switch (other) {
                .Num => |other_value| value == other_value,
                else => false,
            },
            .Str => |value| switch (other) {
                .Str => |other_value| std.mem.eql(u8, value, other_value),
                else => false,
            },
            .Range => |value| switch (other) {
                .Range => |other_value| value.start == other_value.start and value.end == other_value.end,
                else => false,
            },
            .List => |value| switch (other) {
                .List => |other_value| blk: {
                    if (value.items.len != other_value.items.len) {
                        break :blk false;
                    }

                    for (value.items) |item, i| {
                        const other_item = other_value.items[i];
                        if (!item.equals(other_item)) {
                            break :blk false;
                        }
                    }

                    break :blk true;
                },
                else => false,
            },
            .Closure => |value| switch (other) {
                .Closure => |other_value| value.code == other_value.code,
                else => false,
            },
            .Struct => switch (other) {
                .Struct => todo("Implement Struct equality."),
                else => false,
            },
            .Instance => switch (other) {
                .Instance => todo("Implement Instance equality."),
                else => false,
            },
        };
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (this.*) {
            .None => try writer.print("null", .{}),
            .Bool => |value| try writer.print("{}", .{value}),
            .Char => |value| try writer.print("{u}", .{value}),
            .Int => |value| try writer.print("{}", .{value}),
            .Num => |value| try writer.print("{d}", .{value}),
            .Str => |value| try writer.print("{s}", .{value}),
            .Range => |value| try writer.print("{}", .{value}),
            .List => |value| {
                try writer.print("[", .{});
                var i: usize = 0;
                while (i < value.items.len) : (i += 1) {
                    try writer.print("{}", .{value.items[i]});
                    if (i + 1 < value.items.len) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print("]", .{});
            },
            .Closure => |value| {
                try writer.print("{}", .{value});
            },
            .Struct => |value| {
                try writer.print("{}", .{value});
            },
            .Instance => |value| {
                try writer.print("{}", .{value});
            },
        }
    }
};

pub const ValueKind = enum {
    None,
    Bool,
    Char,
    Int,
    Num,
    Str,
    Range,
    List,
    Closure,
    Struct,
    Instance,
};

pub const Char = u21;

pub const Range = struct {
    start: i64,
    end: i64,

    const This = @This();

    pub fn init(start: i64, end: i64) This {
        return This{ .start = start, .end = end };
    }

    pub fn format(
        this: *const This,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{}..{}", .{this.start, this.end});
    }
};

pub const List = ArrayListUnmanaged(Value);

pub const Closure = struct {
    name: []const u8,
    params: []Parameter,
    code: *AstBlock,
    closed_values: StringArrayHashMapUnmanaged(Value),

    const This = @This();

    pub const Parameter = struct {
        name: []const u8,
    };

    pub fn init(
        name: []const u8,
        params: []Parameter,
        code: *AstBlock,
        closed_values: StringArrayHashMapUnmanaged(Value),
    ) This {
        return This{ .name = name, .params = params, .code = code, .closed_values = closed_values };
    }

    pub fn deinit(this: *This, allocator: Allocator) void {
        allocator.free(this.params);
    }

    pub fn makeBound(
        this: *This,
        allocator: Allocator,
        receiver: Value,
        location: CodeLocation,
        out_err_msg: *ErrMsg,
    ) !This {
        var bound: This = undefined;
        bound.name = this.name;
        bound.code = this.code;

        bound.params = try allocator.alloc(Parameter, this.params.len - 1);
        std.mem.copy(Parameter, bound.params, this.params[1..]);

        bound.closed_values = .{};
        bound.closed_values.putNoClobber(allocator, this.params[0].name, receiver) catch unreachable;

        var it = this.closed_values.iterator();
        while (it.next()) |entry| {
            if (bound.closed_values.contains(entry.key_ptr.*)) {
                const err_msg = try std.fmt.allocPrint(allocator, "`{s}` appears in closure `{s}` more than once.", .{ entry.key_ptr.*, this.name });
                return raise(error.RuntimeError, location, err_msg, out_err_msg);
            }

            try bound.closed_values.putNoClobber(allocator, entry.key_ptr.*, entry.value_ptr.*);
        }

        return bound;
    }

    pub fn format(
        this: *const This,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}(", .{this.name});

        var i: usize = 0;
        while (i < this.params.len) : (i += 1) {
            try writer.print("{s}", .{this.params[i].name});
            if (i + 1 < this.params.len) {
                try writer.print(", ", .{});
            }
        }

        try writer.print(")", .{});
    }
};

pub const Struct = struct {
    name: []const u8,
    fields: []Field,
    methods: StringArrayHashMapUnmanaged(*Closure),

    const This = @This();

    pub const Field = struct {
        name: []const u8,
    };

    pub fn init(name: []const u8, fields: []Field) This {
        return This{ .name = name, .fields = fields, .methods = .{} };
    }

    pub fn deinit(this: *This, allocator: Allocator) void {
        this.methods.deinit(allocator);
        allocator.free(this.fields);
    }

    pub fn format(
        this: *const This,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}{{", .{this.name});

        for (this.fields) |field, i| {
            try writer.print(" .{s}", .{field.name});
            if (i + 1 < this.fields.len) {
                try writer.print(",", .{});
            }
        }

        try writer.print(" }}", .{});
    }
};

pub const Instance = struct {
    _struct: *Struct,
    fields: StringArrayHashMapUnmanaged(Value),

    const This = @This();

    pub fn init(_struct: *Struct, fields: StringArrayHashMapUnmanaged(Value)) This {
        return This{ ._struct = _struct, .fields = fields };
    }

    pub fn deinit(this: *This, allocator: Allocator) void {
        this.fields.deinit(allocator);
    }

    pub fn format(
        this: *const This,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}(", .{this._struct.name});

        for (this._struct.fields) |field, i| {
            const field_value = this.fields.getPtr(field.name).?;
            try writer.print("{s}={}", .{ field.name, field_value.* });
            if (i + 1 < this._struct.fields.len) {
                try writer.print(", ", .{});
            }
        }

        try writer.print(")", .{});
    }
};
