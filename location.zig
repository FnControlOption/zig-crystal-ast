const Location = @This();
const std = @import("std");

filename: ?[]const u8,
line_number: usize,
column_number: usize,

pub fn new(filename: ?[]const u8, line_number: usize, column_number: usize) Location {
    return .{
        .filename = filename,
        .line_number = line_number,
        .column_number = column_number,
    };
}

pub fn dirname(loc: Location) ?[]const u8 {
    if (loc.filename) |filename| {
        if (std.fs.path.dirname(filename)) |result| {
            return result;
        }
        return "/"; // TODO: this is how Crystal's File.dirname works, but is it necessary?
    }
    return null;
}

pub fn isBetween(loc: Location, min: ?Location, max: ?Location) bool {
    if (min) |a| {
        if (max) |b| {
            return a.compare(.lte, loc) and loc.compare(.lte, b);
        }
    }
    return false;
}

pub fn format(loc: Location, comptime fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
    _ = opt;
    _ = fmt;
    if (loc.filename) |filename| {
        try writer.print("{s}", .{filename});
    }
    try writer.print(":{}:{}", .{ loc.line_number, loc.column_number });
}

pub fn order(a: Location, b: Location) ?std.math.Order {
    if (a.filename) |file_a| {
        if (b.filename) |file_b| {
            if (std.mem.eql(u8, file_a, file_b)) {
                return switch (std.math.order(a.line_number, b.line_number)) {
                    .eq => std.math.order(a.column_number, b.column_number),
                    .lt, .gt => |result| result,
                };
            }
        }
    }
    return null;
}

pub fn compare(a: Location, op: std.math.CompareOperator, b: Location) bool {
    if (a.order(b)) |ord| {
        return ord.compare(op);
    }
    return switch (op) {
        .lt, .lte, .eq, .gte, .gt => false,
        .neq => true,
    };
}

pub fn main() void {
    const p = @import("std").debug.print;
    p("{?s}\n", .{std.fs.path.dirname("/")});
    p("{?s}\n", .{Location.new("/", 0, 0).dirname()});
    p("{}\n", .{Location.new("/", 0, 0)});
    const loc1 = Location.new("foo", 0, 0);
    const loc2 = Location.new("foo", 0, 1);
    const loc3 = Location.new("foo", 0, 2);
    p("{?}\n", .{loc1.order(loc2)});
    p("{}\n", .{loc1.compare(.lt, loc2)});
    p("{}\n", .{loc1.compare(.lte, loc2)});
    p("{}\n", .{loc1.compare(.gt, loc2)});
    p("{}\n", .{loc2.isBetween(loc2, loc3)});
}
