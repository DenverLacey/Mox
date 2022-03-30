const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Value = union(ValueKind) {
    None,
    Bool: bool,
    Int: i64,
    Num: f64,
    Str: *RefCounted([]const u8),

    pub fn isTrue(this: @This()) bool {
        return switch (this) {
            .None => false,
            .Bool => |value| value,
            .Int => |value| value != 0,
            .Num => |value| value != 0.0,
            .Str => |rc| rc.value.len != 0,
        };
    }

    pub fn dupe(this: @This()) @This() {
        return switch (this) {
            .Str => |rc| Value{ .Str = rc.dupe() },
            else => this,
        };
    }

    pub fn drop(this: @This()) void {
        switch (this) {
            .Str => |rc| rc.drop(),
            else => {},
        }
    }
};

pub const ValueKind = enum {
    None,
    Bool,
    Int,
    Num,
    Str,
};

pub fn RefCounted(comptime T: type) type {
    return struct {
        allocator: Allocator,
        num_references: usize,
        value: T,

        pub fn create(allocator: Allocator, value: T) !*@This() {
            var rc = try allocator.create(@This());
            rc.allocator = allocator;
            rc.num_references = 1;
            rc.value = value;
            return rc;
        }

        pub fn dupe(this: *@This()) *@This() {
            this.num_references += 1;
            return this;
        }

        pub fn drop(this: *@This()) void {
            this.num_references -= 1;
            if (this.num_references == 0) {
                std.debug.print("DROP!!!\n", .{});
                this.allocator.destroy(this);
            }
        }
    };
}
