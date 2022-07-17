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

const DEBUG_ALWAYS_COLLECT       = true;
const DEBUG_TRACE_ALLOCATIONS    = true;
const DEBUG_TRACE_DEALLOCATIONS  = true;
const DEBUG_LOG_NUM_COLLECTIONS  = true;
var debug_num_collections: usize = 0;

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

        if (DEBUG_LOG_NUM_COLLECTIONS)
            std.debug.print("::: No. GC Collections: {}\n", .{debug_num_collections});
    }

    pub fn copyString(this: *This, s: []const u8) ![]const u8 {
        const allocated = try this.allocator.dupe(u8, s);

        if (DEBUG_TRACE_ALLOCATIONS)
            std.debug.print("::: ALLOCATING \"{s}\"\n", .{allocated});

        try this.strings.append(this.allocator, GCValue([]const u8){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateList(this: *This) !*ArrayListUnmanaged(Value) {
        const list = try this.allocator.create(ArrayListUnmanaged(Value));

        if (DEBUG_TRACE_ALLOCATIONS)
            std.debug.print("::: ALLOCATING LIST\n", .{});

        try this.lists.append(this.allocator, GCValue(*ArrayListUnmanaged(Value)){ .marked = false, .value = list });
        return list;
    }

    pub fn allocateClosure(this: *This, closure: Closure) !*Closure {
        const allocated = try this.allocator.create(Closure);
        allocated.* = closure;

        if (DEBUG_TRACE_ALLOCATIONS)
            std.debug.print("::: ALLOCATING {}\n", .{allocated});

        try this.closures.append(this.allocator, GCValue(*Closure){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateStruct(this: *This, struct_: Struct) !*Struct {
        const allocated = try this.allocator.create(Struct);
        allocated.* = struct_;

        if (DEBUG_TRACE_ALLOCATIONS)
            std.debug.print("::: ALLOCATING {}\n", .{allocated});

        try this.structs.append(this.allocator, GCValue(*Struct){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateInstance(this: *This, instance: Instance) !*Instance {
        const allocated = try this.allocator.create(Instance);
        allocated.* = instance;

        if (DEBUG_TRACE_ALLOCATIONS)
            std.debug.print("::: ALLOCATING {}\n", .{allocated});

        try this.instances.append(this.allocator, GCValue(*Instance){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn collectGarbage(this: *This, scopes: *BucketArrayUnmanaged(SCOPE_BUCKET_SIZE, Scope)) void {
        if (!DEBUG_ALWAYS_COLLECT and this.notEnoughGarbage()) return;

        if (DEBUG_LOG_NUM_COLLECTIONS)
            debug_num_collections += 1;

        this.resetValuesForCollection();

        var it = scopes.buckets.first;
        var i: usize = 0;
        outer: while (it) |bucket| {
            for (bucket.data) |scope| {
                if (i >= scopes.len) break :outer;
                this.markVariables(scope.variables.values());
                i += 1;
            }
            it = bucket.next;
        }

        this.clearUnmarkedValues();
    }

    fn notEnoughGarbage(_: *This) bool {
        return false;
    }

    fn resetValuesForCollection(this: *This) void {
        resetValuesForCollectionInList([]const u8, &this.strings);
        resetValuesForCollectionInList(*ArrayListUnmanaged(Value), &this.lists);
        resetValuesForCollectionInList(*Closure, &this.closures);
        resetValuesForCollectionInList(*Struct, &this.structs);
        resetValuesForCollectionInList(*Instance, &this.instances);
    }

    fn resetValuesForCollectionInList(comptime T: type, list: *ArrayListUnmanaged(GCValue(T))) void {
        for (list.items) |*value| {
            value.marked = false;
        }
    }

    fn markVariables(this: *This, variables: []Value) void {
        for (variables) |variable| {
            this.markVariable(variable);
        }
    }

    fn markVariable(this: *This, variable: Value) void {
        switch (variable) {
            .None, .Bool, .Int, .Num => {},
            .Str => |value| {
                for (this.strings.items) |*string| {
                    if (&value[0] == &string.value[0]) {
                        string.marked = true;
                    }
                }
            },
            .List => |value| {
                for (this.lists.items) |*list| {
                    if (value == list.value) {
                        if (!list.marked)
                            this.markVariables(list.value.items);
                        list.marked = true;
                    }
                }
            },
            .Closure => |value| {
                this.markClosure(value);
            },
            .Struct => |value| {
                this.markStruct(value);
            },
            .Instance => |value| {
                for (this.instances.items) |*instance| {
                    if (value == instance.value) {
                        if (!instance.marked) {
                            this.markStruct(instance.value._struct);
                            this.markVariables(instance.value.fields.values());
                        }
                        instance.marked = true;
                    }
                }
            },
        }
    }

    fn markClosure(this: *This, closure: *Closure) void {
        for (this.closures.items) |*c| {
            if (closure == c.value) {
                if (!c.marked)
                    this.markVariables(c.value.closed_values.values());
                c.marked = true;
            }
        }
    }

    fn markStruct(this: *This, struct_: *Struct) void {
        for (this.structs.items) |*s| {
            if (struct_ == s.value) {
                if (!s.marked) {
                    for (s.value.methods.values()) |method| {
                        this.markClosure(method);
                    }
                }
                s.marked = true;
            }
        }
    }

    fn clearUnmarkedValues(this: *This) void {
        this.clearUnmarkedStrings(&this.strings);
        this.clearUnmarkedValuesInList(*ArrayListUnmanaged(Value), &this.lists);
        this.clearUnmarkedValuesInList(*Closure, &this.closures);
        this.clearUnmarkedValuesInList(*Struct, &this.structs);
        this.clearUnmarkedValuesInList(*Instance, &this.instances);
    }

    fn clearUnmarkedStrings(this: *This, strings: *ArrayListUnmanaged(GCValue([]const u8))) void {
        var i: usize = 0;
        while (i < strings.items.len) {
            if (!strings.items[i].marked) {
                const value = strings.swapRemove(i);

                if (DEBUG_TRACE_DEALLOCATIONS)
                    std.debug.print("::: FREEING \"{s}\"\n", .{value.value});

                this.allocator.free(value.value);
                continue;
            }
            i += 1;
        }
    }

    fn clearUnmarkedValuesInList(this: *This, comptime T: type, values: *ArrayListUnmanaged(GCValue(T))) void {
        var i: usize = 0;
        while (i < values.items.len) {
            if (!values.items[i].marked) {
                const value = values.swapRemove(i);

                if (DEBUG_TRACE_DEALLOCATIONS)
                    std.debug.print("::: FREEING \"{}\"\n", .{value.value});

                this.allocator.destroy(value.value);
                continue;
            }
            i += 1;
        }
    }
};

fn GCValue(comptime T: type) type {
    return struct {
        marked: bool,
        value: T,
    };
}
