const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const evaluator = @import("evaluator.zig");
const SCOPE_BUCKET_SIZE = evaluator.SCOPE_BUCKET_SIZE;
const Scope = evaluator.Scope;

const val = @import("value.zig");
const Value = val.Value;
const List = val.List;
const Closure = val.Closure;
const Struct = val.Struct;
const Instance = val.Instance;

const err = @import("error.zig");
const todo = err.todo;

const BucketArrayUnmanaged = @import("bucket_array.zig").BucketArrayUnmanaged;

const DEBUG_ALWAYS_COLLECT         = false;
const DEBUG_TRACE_ALLOCATIONS      = false;
const DEBUG_TRACE_DEALLOCATIONS    = false;
const DEBUG_LOG_NUM_COLLECTIONS    = false;
var debug_num_collections: usize   = 0;
var debug_num_allocations: usize   = 0;
var debug_num_deallocations: usize = 0;

fn debugReportGcInfo() void {
    if (DEBUG_LOG_NUM_COLLECTIONS)
        std.debug.print("::: No. GC Collections: {}\n", .{debug_num_collections});
    if (DEBUG_TRACE_ALLOCATIONS)
        std.debug.print("::: No. Allocations:    {}\n", .{debug_num_allocations});
    if (DEBUG_TRACE_DEALLOCATIONS)
        std.debug.print("::: No. Deallocations:  {}\n", .{debug_num_deallocations});
}

pub const GarbageCollector = struct {
    allocator: Allocator,

    strings: ArrayListUnmanaged(GCValue([]const u8)),
    lists: ArrayListUnmanaged(GCValue(*List)),
    closures: ArrayListUnmanaged(GCValue(*Closure)),
    structs: ArrayListUnmanaged(GCValue(*Struct)),
    instances: ArrayListUnmanaged(GCValue(*Instance)),

    const This = @This();
    const COLLECTION_THRESHOLD: usize = 128;

    pub fn init(allocator: Allocator) This {
        return This{
            .allocator = allocator,
            .strings = ArrayListUnmanaged(GCValue([]const u8)){},
            .lists = ArrayListUnmanaged(GCValue(*List)){},
            .closures = ArrayListUnmanaged(GCValue(*Closure)){},
            .structs = ArrayListUnmanaged(GCValue(*Struct)){},
            .instances = ArrayListUnmanaged(GCValue(*Instance)){},
        };
    }

    pub fn deinit(this: *This) void {
        debugReportGcInfo();

        for (this.strings.items) |string| {
            this.allocator.free(string.value);
        }
        this.strings.deinit(this.allocator);

        for (this.lists.items) |list| {
            list.value.deinit(this.allocator);
            this.allocator.destroy(list.value);
        }
        this.lists.deinit(this.allocator);

        for (this.closures.items) |closure| {
            closure.value.deinit(this.allocator);
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

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING \"{s}\"\n", .{allocated});
            debug_num_allocations += 1;
        }

        try this.strings.append(this.allocator, GCValue([]const u8){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateList(this: *This) !*List {
        const list = try this.allocator.create(List);

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING LIST\n", .{});
            debug_num_allocations += 1;
        }

        try this.lists.append(this.allocator, GCValue(*List){ .marked = false, .value = list });
        return list;
    }

    pub fn allocateClosure(this: *This, closure: Closure) !*Closure {
        const allocated = try this.allocator.create(Closure);
        allocated.* = closure;

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING {}\n", .{allocated});
            debug_num_allocations += 1;
        }

        try this.closures.append(this.allocator, GCValue(*Closure){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateStruct(this: *This, struct_: Struct) !*Struct {
        const allocated = try this.allocator.create(Struct);
        allocated.* = struct_;

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING {}\n", .{allocated});
            debug_num_allocations += 1;
        }

        try this.structs.append(this.allocator, GCValue(*Struct){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateInstance(this: *This, instance: Instance) !*Instance {
        const allocated = try this.allocator.create(Instance);
        allocated.* = instance;

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING {}\n", .{allocated});
            debug_num_allocations += 1;
        }

        try this.instances.append(this.allocator, GCValue(*Instance){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn collectGarbage(this: *This, scopes: *BucketArrayUnmanaged(SCOPE_BUCKET_SIZE, Scope)) void {
        if (!DEBUG_ALWAYS_COLLECT and this.notEnoughGarbage()) return;

        if (DEBUG_LOG_NUM_COLLECTIONS)
            debug_num_collections += 1;

        this.resetValuesForCollection();

        var it = scopes.iterator();
        while (it.next()) |scope| {
            this.markVariables(scope.variables.values());
        }

        this.freeUnmarkedValues();
    }

    fn notEnoughGarbage(this: *This) bool {
        // @INCOMPLETE:
        // This should probably use the actual amount of memory used
        // to determine if a collection is reasonable.
        //

        var num_live_allocations =
            this.strings.items.len +
            this.lists.items.len +
            this.closures.items.len +
            this.structs.items.len +
            this.instances.items.len;

        if (DEBUG_LOG_NUM_COLLECTIONS)
            std.debug.print("::: num_live_allocations = {}\n", .{num_live_allocations});

        return num_live_allocations < COLLECTION_THRESHOLD;
    }

    fn resetValuesForCollection(this: *This) void {
        resetValuesForCollectionInList([]const u8, &this.strings);
        resetValuesForCollectionInList(*List, &this.lists);
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
            .None, .Bool, .Char, .Int, .Num, .Range => {},
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

    fn freeUnmarkedValues(this: *This) void {
        this.freeUnmarkedStrings(&this.strings);
        this.freeUnmarkedValuesInList(*List, &this.lists);
        this.freeUnmarkedValuesInList(*Closure, &this.closures);
        this.freeUnmarkedValuesInList(*Struct, &this.structs);
        this.freeUnmarkedValuesInList(*Instance, &this.instances);
    }

    fn freeUnmarkedStrings(this: *This, strings: *ArrayListUnmanaged(GCValue([]const u8))) void {
        var i: usize = 0;
        while (i < strings.items.len) {
            if (!strings.items[i].marked) {
                const value = strings.swapRemove(i);

                if (DEBUG_TRACE_DEALLOCATIONS) {
                    std.debug.print("::: FREEING \"{s}\"\n", .{value.value});
                    debug_num_deallocations += 1;
                }

                this.allocator.free(value.value);
                continue;
            }
            i += 1;
        }
    }

    fn freeUnmarkedValuesInList(
        this: *This,
        comptime T: type,
        values: *ArrayListUnmanaged(GCValue(T)),
    ) void {
        var i: usize = 0;
        while (i < values.items.len) {
            if (!values.items[i].marked) {
                const value = values.swapRemove(i);

                if (DEBUG_TRACE_DEALLOCATIONS) {
                    std.debug.print("::: FREEING {}\n", .{T});
                    debug_num_deallocations += 1;
                }

                value.value.deinit(this.allocator);
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
