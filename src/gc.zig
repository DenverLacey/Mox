const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const evaluator = @import("evaluator.zig");
const SCOPE_BUCKET_SIZE = evaluator.SCOPE_BUCKET_SIZE;
const Scope = evaluator.Scope;

const val = @import("value.zig");
const Value = val.Value;
const Closure = val.Closure;
const Struct = val.Struct;
const Instance = val.Instance;

const err = @import("error.zig");
const todo = err.todo;

const BucketArrayUnmanaged = @import("bucket_array.zig").BucketArrayUnmanaged;

const DEBUG_ALWAYS_COLLECT = false;

pub const GarbageCollector = struct {
    allocator: Allocator,

    strings: ArrayListUnmanaged(GCValue([]const u8)),
    lists: ArrayListUnmanaged(GCValue(*ArrayListUnmanaged(Value))),
    closures: ArrayListUnmanaged(GCValue(*Closure)),
    structs: ArrayListUnmanaged(GCValue(*Struct)),
    instances: ArrayListUnmanaged(GCValue(*Instance)),

    const This = @This();

    pub fn init(allocator: Allocator) This {
        return This{
            .allocator = allocator,
            .strings = ArrayListUnmanaged(GCValue([]const u8)){},
            .lists = ArrayListUnmanaged(GCValue(*ArrayListUnmanaged(Value))){},
            .closures = ArrayListUnmanaged(GCValue(*Closure)){},
            .structs = ArrayListUnmanaged(GCValue(*Struct)){},
            .instances = ArrayListUnmanaged(GCValue(*Instance)){}
        };
    }

    pub fn deinit(this: *This) void {
        for (this.strings.items) |string| {
            this.allocator.free(string.value);
        }
        this.strings.deinit(this.allocator);

        for (this.lists.items) |list| {
            this.allocator.destroy(list.value);
        }
        this.lists.deinit(this.allocator);

        for (this.closures.items) |closure| {
            this.allocator.destroy(closure.value);
        }
        this.closures.deinit(this.allocator);

        for (this.structs.items) |struct_| {
            struct_.value.deinit(this.allocator);
            this.allocator.destroy(struct_.value);
        }
        this.structs.deinit(this.allocator);

        for (this.instances.items) |instance| {
            instance.value.deinit(this.allocator);
            this.allocator.destroy(instance.value);
        }
        this.instances.deinit(this.allocator);
    }

    pub fn copyString(this: *This, s: []const u8) ![]const u8 {
        const allocated = try this.allocator.dupe(u8, s);
        try this.strings.append(this.allocator, GCValue([]const u8){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateList(this: *This) !*ArrayListUnmanaged(Value) {
        const list = try this.allocator.create(ArrayListUnmanaged(Value));
        try this.lists.append(this.allocator, GCValue(*ArrayListUnmanaged(Value)){ .marked = false, .value = list });
        return list;
    }

    pub fn allocateClosure(this: *This, closure: Closure) !*Closure {
        const allocated = try this.allocator.create(Closure);
        allocated.* = closure;
        try this.closures.append(this.allocator, GCValue(*Closure){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateStruct(this: *This, struct_: Struct) !*Struct {
        const allocated = try this.allocator.create(Struct);
        allocated.* = struct_;
        try this.structs.append(this.allocator, GCValue(*Struct){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateInstance(this: *This, instance: Instance) !*Instance {
        const allocated = try this.allocator.create(Instance);
        allocated.* = instance;
        try this.instances.append(this.allocator, GCValue(*Instance){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn collectGarbage(this: *This, scopes: *BucketArrayUnmanaged(SCOPE_BUCKET_SIZE, Scope)) void {
        if (!DEBUG_ALWAYS_COLLECT and this.notEnoughGarbage()) return;

        var it = scopes.buckets.first;
        while (it) |bucket| {
            for (bucket.data) |scope| {
                this.markVariables(scope.variables.values());
            }
            it = bucket.next;
        }

        this.clearUnmarkedValues();
    }

    fn notEnoughGarbage(_: *This) bool {
        return true;
    }

    fn markVariables(_: *This, _: []Value) void {
        todo("Implement `markVariables()`");
    }

    fn clearUnmarkedValues(_: *This) void {
        todo("Implement `clearUnmarkedValues()`");
    }
};

fn GCValue(comptime T: type) type {
    return struct {
        marked: bool,
        value: T,
    };
}
