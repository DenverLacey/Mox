const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringArrayHashMapUnmanaged = std.StringArrayHashMapUnmanaged;

const AstBlock = @import("ast.zig").AstBlock;
const todo = @import("error.zig").todo;

pub const Value = union(ValueKind) {
    None,
    Bool: bool,
    Int: i64,
    Num: f64,
    Str: *RefCounted([]const u8),
    List: *RefCounted(ArrayListUnmanaged(Value)),
    Closure: *RefCounted(Closure),
    Struct: *RefCounted(Struct),
    Instance: *RefCounted(Instance),

    const This = @This();

    pub fn isTrue(this: This) bool {
        return switch (this) {
            .None => false,
            .Bool => |value| value,
            .Int => |value| value != 0,
            .Num => |value| value != 0.0,
            .Str => |rc| rc.value.len != 0,
            .List => |rc| rc.value.items.len != 0,
            .Closure => true,
            .Struct => true,
            .Instance => true,
        };
    }

    pub fn isRefCounted(this: This) bool {
        return this == .Str or this == .List or this == .Closure or this == .Struct;
    }

    pub fn dupe(this: This) This {
        return switch (this) {
            .Str => |rc| Value{ .Str = rc.dupe() },
            .List => |rc| Value{ .List = rc.dupe() },
            .Closure => |rc| Value{ .Closure = rc.dupe() },
            .Struct => |rc| Value{ .Struct = rc.dupe() },
            .Instance => |rc| Value{ .Instance = rc.dupe() },
            else => this,
        };
    }

    pub fn drop(this: This, allocator: Allocator) void {
        switch (this) {
            .Str => |rc| {
                if (rc.num_references - 1 == 0) {
                    allocator.free(rc.value);
                }
                rc.drop(allocator);
            },
            .List => |rc| {
                if (rc.num_references - 1 == 0) {
                    for (rc.value.items) |item| {
                        item.drop(allocator);
                    }
                    rc.value.deinit(allocator);
                }
                rc.drop(allocator);
            },
            .Closure => |rc| {
                if (rc.num_references - 1 == 0) {
                    rc.value.deinit(allocator);
                }
                rc.drop(allocator);
            },
            .Struct => |rc| {
                if (rc.num_references - 1 == 0) {
                    rc.value.deinit(allocator);
                }
                rc.drop(allocator);
            },
            .Instance => |rc| {
                if (rc.num_references - 1 == 0) {
                    rc.value.deinit(allocator);
                }
                rc.drop(allocator);
            },
            else => {},
        }
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
            .Int => |value| switch (other) {
                .Int => |other_value| value == other_value,
                else => false,
            },
            .Num => |value| switch (other) {
                .Num => |other_value| value == other_value,
                else => false,
            },
            .Str => |rc| switch (other) {
                .Str => |other_rc| std.mem.eql(u8, rc.value, other_rc.value),
                else => false,
            },
            .List => |rc| switch (other) {
                .List => |other_rc| blk: {
                    if (rc.value.items.len != other_rc.value.items.len) {
                        break :blk false;
                    }

                    for (rc.value.items) |item, i| {
                        const other_item = other_rc.value.items[i];
                        if (!item.equals(other_item)) {
                            break :blk false;
                        }
                    }

                    break :blk true;
                },
                else => false,
            },
            .Closure => |rc| switch (other) {
                .Closure => |other_rc| rc.value.code == other_rc.value.code,
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
            .None => try writer.print("none", .{}),
            .Bool => |value| try writer.print("{}", .{value}),
            .Int => |value| try writer.print("{}", .{value}),
            .Num => |value| try writer.print("{d}", .{value}),
            .Str => |rc| try writer.print("{s}", .{rc.value}),
            .List => |rc| {
                try writer.print("[", .{});
                var i: usize = 0;
                while (i < rc.value.items.len) : (i += 1) {
                    try writer.print("{}", .{rc.value.items[i]});
                    if (i + 1 < rc.value.items.len) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print("]", .{});
            },
            .Closure => |rc| {
                try writer.print("{}", .{rc.value});
            },
            .Struct => |rc| {
                try writer.print("{}", .{rc.value});
            },
            .Instance => |rc| {
                try writer.print("{}", .{rc.value});
            },
        }
    }
};

pub const ValueKind = enum {
    None,
    Bool,
    Int,
    Num,
    Str,
    List,
    Closure,
    Struct,
    Instance,
};

pub fn RefCounted(comptime T: type) type {
    return struct {
        num_references: usize,
        value: T,

        const This = @This();

        pub fn create(allocator: Allocator, value: T) !*This {
            var rc = try allocator.create(@This());
            // rc.allocator = allocator;
            rc.num_references = 1;
            rc.value = value;
            return rc;
        }

        pub fn dupe(this: *This) *This {
            this.num_references += 1;
            std.debug.print("DUPE {} -> {}: 0x{X}\n", .{ this.num_references - 1, this.num_references, @ptrToInt(this) });
            return this;
        }

        pub fn drop(this: *This, allocator: Allocator) void {
            this.num_references -= 1;
            std.debug.print("DROP {} -> {}: 0x{X}\n", .{ this.num_references + 1, this.num_references, @ptrToInt(this) });
            if (this.num_references == 0) {
                allocator.destroy(this);
            }
        }
    };
}

pub const Closure = struct {
    name: []const u8,
    params: []Parameter,
    code: *AstBlock,
    closed_values: StringArrayHashMapUnmanaged(Value),

    const This = @This();

    pub const Parameter = struct {
        name: []const u8,
    };

    pub fn init(name: []const u8, params: []Parameter, code: *AstBlock, closed_values: StringArrayHashMapUnmanaged(Value)) This {
        return This{ .name = name, .params = params, .code = code, .closed_values = closed_values };
    }

    pub fn deinit(this: *This, allocator: Allocator) void {
        var it = this.closed_values.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.drop(allocator);
        }

        allocator.free(this.params);
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
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

    const This = @This();

    pub const Field = struct {
        name: []const u8,
    };

    pub fn init(name: []const u8, fields: []Field) This {
        return This{ .name = name, .fields = fields };
    }

    pub fn deinit(this: *This, allocator: Allocator) void {
        allocator.free(this.fields);
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
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
    _struct: *RefCounted(Struct),
    fields: StringArrayHashMapUnmanaged(Value),

    const This = @This();

    pub fn init(_struct: *RefCounted(Struct), fields: StringArrayHashMapUnmanaged(Value)) This {
        return This{ ._struct = _struct, .fields = fields };
    }

    pub fn deinit(this: *This, allocator: Allocator) void {
        var it = this.fields.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.drop(allocator);
        }

        this.fields.deinit(allocator);
        this._struct.drop(allocator);
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{s}(", .{this._struct.value.name});

        for (this._struct.value.fields) |field, i| {
            const field_value = this.fields.getPtr(field.name).?;
            try writer.print("{s}={}", .{ field.name, field_value.* });
            if (i + 1 < this._struct.value.fields.len) {
                try writer.print(", ", .{});
            }
        }

        try writer.print(")", .{});
    }
};
