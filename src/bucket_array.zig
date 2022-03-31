const std = @import("std");
const Allocator = std.mem.Allocator;
const SinglyLinkedList = std.SinglyLinkedList;

pub fn BucketArray(comptime N: usize, comptime T: type) type {
    return struct {
        allocator: Allocator,
        buckets: List,
        write_index: usize,

        const This = @This();
        const List = SinglyLinkedList(Bucket);
        const Bucket = [N]T;

        pub fn init(allocator: Allocator) This {
            return This{
                .allocator = allocator,
                .buckets = List{ .first = null },
                .write_index = 0,
            };
        }

        pub fn deinit(this: *This) void {
            var it = this.buckets.first;
            while (it) |bucket| {
                this.allocator.destroy(bucket);
                it = bucket.next;
            }
        }

        pub fn push(this: *This, item: T) !void {
            if (this.buckets.first == null) {
                this.buckets.first = try this.allocator.create(List.Node);
            } else if (this.write_index == N) {
                const node = try this.allocator.create(List.Node);
                this.buckets.prepend(node);
                this.write_index = 0;
            }

            const ptr = &this.buckets.first.?.data[this.write_index];
            ptr.* = item;

            this.write_index += 1;
        }

        pub fn pop(this: *This) void {
            if (this.write_index == 0) {
                const node = this.buckets.popFirst() orelse return;
                this.buckets.first = node.next;
                this.write_index = N;
                this.allocator.destroy(node);
            } else {
                this.write_index -= 1;
            }
        }

        pub fn top(this: *This) ?*T {
            if (this.write_index == 0) {
                const node = this.buckets.first orelse return null;
                const next = node.next orelse return null;
                return &next.data[0];
            } else {
                const node = this.buckets.first orelse return null;
                return &node.data[this.write_index - 1];
            }
        }
    };
}
