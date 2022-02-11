const std = @import("std");
const print = std.debug.print;
const c_allocator = std.heap.c_allocator;

pub fn inspect(x: anytype) []const u8 {
    return xinspect(x) catch unreachable;
}
pub fn xinspect(x: anytype) ![]const u8 {
    const Type = @TypeOf(x);
    switch (Type) {
        []const u8, ?[]const u8 => {
            return std.fmt.allocPrint(c_allocator, "{s}", .{x});
        },
        else => {}
    }
    const type_info = @typeInfo(Type);
    switch (type_info) {
        .Struct => |struct_info| {
            if (struct_info.fields.len == 1)
                return std.fmt.allocPrint(c_allocator, "{}", x);
        },
        .Pointer => |pointer_info| {
            const child_info = @typeInfo(pointer_info.child);
            switch (child_info) {
                .Array => |array_info| {
                    if (array_info.child == u8)
                        return std.fmt.allocPrint(c_allocator, "{s}", .{x.*});
                },
                else => {}
            }
        },
        else => {}
    }
    return std.fmt.allocPrint(c_allocator, "{}", .{x});
}
pub fn pp(x: anytype) void {
    const string = inspect(x);
    defer c_allocator.free(string);
    print("{s}\n", .{string});
}
pub fn p(x: anytype) void {
    const string = inspect(x);
    defer c_allocator.free(string);
    // print("{s}\n", .{string});
}
pub fn xprint(comptime fmt: []const u8, x: anytype) void {
    const string = std.fmt.allocPrint(c_allocator, fmt, x) catch unreachable;
    defer c_allocator.free(string);
    // print("{s}\n", .{string});
}

pub fn main() void {
    pp("foo");
    var bar: []const u8 = "bar";
    pp(bar);
}
