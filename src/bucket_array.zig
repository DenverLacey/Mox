const std = @import("std");
const Allocator = std.mem.Allocator;
const SinglyLinkedList = std.SinglyLinkedList;

pub fn BucketArrayUnmanaged(comptime N: usize, comptime T: type) type {
    return struct {
        buckets: List = List{ .first = null },
        write_index: usize = 0,
        len: usize = 0,

        const This = @This();
        const List = SinglyLinkedList(Bucket);
        const Bucket = [N]T;

        pub fn init() This {
            return This{};
        }

        pub fn deinit(this: *This, allocator: Allocator) void {
            var it = this.buckets.first;
            while (it) |bucket| {
                allocator.destroy(bucket);
                it = bucket.next;
            }
        }

        pub fn push(this: *This, allocator: Allocator, item: T) !void {
            if (this.buckets.first == null) {
                this.buckets.first = try allocator.create(List.Node);
            } else if (this.write_index == N) {
                const node = try allocator.create(List.Node);
                this.buckets.prepend(node);
                this.write_index = 0;
            }

            const ptr = &this.buckets.first.?.data[this.write_index];
            ptr.* = item;

            this.write_index += 1;
            this.len += 1;
        }

        pub fn pop(this: *This, allocator: Allocator) void {
            if (this.write_index == 0) {
                const node = this.buckets.popFirst() orelse return;
                this.buckets.first = node.next;
                this.write_index = N;
                allocator.destroy(node);
            } else {
                this.write_index -= 1;
            }

            this.len -= 1;
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

pub fn BucketArray(comptime N: usize, comptime T: type) type {
    return struct {
        allocator: Allocator,
        inner: BucketArrayUnmanaged(N, T),

        const This = @This();
        const List = SinglyLinkedList(Bucket);
        const Bucket = [N]T;

        pub fn init(allocator: Allocator) This {
            return This{
                .allocator = allocator,
                .inner = BucketArrayUnmanaged(N, T).init(),
            };
        }

        pub fn deinit(this: *This) void {
            this.inner.deinit(this.allocator);
        }

        pub fn push(this: *This, item: T) !void {
            return this.inner.push(this.allocator, item);
        }

        pub fn pop(this: *This) void {
            return this.inner.pop(this.allocator);
        }

        pub fn top(this: *This) ?*T {
            return this.inner.top();
        }
    };
}
