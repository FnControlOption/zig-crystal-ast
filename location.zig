//! A location of an `ASTNode`, including its filename, line number and column number.

const std = @import("std");

const Location = @This();
const VirtualFile = @import("virtual_file.zig");

line_number: i32,
column_number: i32,
filename: ?Filename,

pub const Filename = union(enum) {
    string: []const u8,
    virtual_file: *VirtualFile,

    pub fn format(self: Filename, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .string => |string| {
                try writer.print("{s}", .{string});
            },
            .virtual_file => |virtual_file| {
                try writer.print("{}", .{virtual_file});
            },
        }
    }
};

pub fn init(filename: ?Filename, line_number: i32, column_number: i32) Location {
    return .{
        .filename = filename,
        .line_number = line_number,
        .column_number = column_number,
    };
}

/// Returns the directory name of this location's filename. If
/// the filename is a VirtualFile, this is invoked on its expanded
/// location.
pub fn dirname(self: Location) ?[]const u8 {
    if (self.original_filename()) |filename| {
        return std.fs.path.dirname(filename);
    } else {
        return null;
    }
}

/// Returns the Location whose filename is a string, not a VirtualFile,
/// traversing virtual file expanded locations.
pub fn expanded_location(self: Location) ?Location {
    if (self.filename) |filename| {
        switch (filename) {
            .string => {
                return self;
            },
            .virtual_file => |virtual_file| {
                if (virtual_file.expanded_location) |location| {
                    return location.expanded_location();
                }
            },
        }
    }
    return null;
}

/// Returns the Location whose filename is a string, not a VirtualFile,
/// traversing virtual file expanded locations leading to the original user source code
pub fn macro_location(self: Location) ?Location {
    if (self.filename) |filename| {
        switch (filename) {
            .string => {
                return self;
            },
            .virtual_file => |virtual_file| {
                if (virtual_file.macro_location) |location| {
                    return location.macro_location();
                }
            },
        }
    }
    return null;
}

/// Returns the filename of the `expanded_location`
pub fn original_filename(self: Location) ?[]const u8 {
    if (self.expanded_location()) |location| {
        if (location.filename) |filename| {
            switch (filename) {
                .string => |string| return string,
                .virtual_file => {},
            }
        }
    }
    return null;
}

pub fn isBetween(self: Location, min: ?Location, max: ?Location) bool {
    if (min) |minimum| {
        if (max) |maximum| {
            return minimum.order(self).compare(.lte) and self.order(maximum).compare(.lte);
        }
    }
    return false;
}

pub fn format(self: Location, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    try writer.print("{}:{}:{}", .{self.filename, self.line_number, self.column_number});
}

pub fn order(a: Location, b: Location) std.math.Order {
    // TODO: check if filename is the same?
    if (a.line_number == b.line_number) {
        return std.math.order(a.column_number, b.column_number);
    } else {
        return std.math.order(a.line_number, b.line_number);
    }
}

const print = std.debug.print;
const c_allocator = std.heap.c_allocator;

const ast = @import("ast.zig");
const utils = @import("utils.zig");
const inspect = utils.inspect;
const pp = utils.pp;
const p = utils.p;
const xprint = utils.xprint;

pub fn main() !void {
    const macro1 = try ast.Macro.create(c_allocator, "foo", try c_allocator.alloc(*ast.Arg, 0), null);
    p(try VirtualFile.create(c_allocator, macro1, null, "bar", null));

    p(Location.init(null, 0, 0));
    p(Location.init(Filename { .string = "foo" }, 0, 0));
    p(Location.init(Filename { .string = "foo/bar" }, 0, 0));
    p(Location.init(Filename { .virtual_file = try VirtualFile.create(c_allocator, macro1, null, "bar", Location.init(Filename { .string = "fizz" }, 0, 0)) }, 0, 0));
    p(Location.init(Filename { .virtual_file = try VirtualFile.create(c_allocator, macro1, null, "bar", Location.init(Filename { .string = "fizz/buzz" }, 0, 0)) }, 0, 0));

    p(Location.init(null, 0, 0).dirname());
    p(Location.init(Filename { .string = "foo" }, 0, 0).dirname());
    p(Location.init(Filename { .string = "foo/bar" }, 0, 0).dirname());
    p(Location.init(Filename { .virtual_file = try VirtualFile.create(c_allocator, macro1, null, "bar", Location.init(Filename { .string = "fizz" }, 0, 0)) }, 0, 0).dirname());
    p(Location.init(Filename { .virtual_file = try VirtualFile.create(c_allocator, macro1, null, "bar", Location.init(Filename { .string = "fizz/buzz" }, 0, 0)) }, 0, 0).dirname());

    p(Location.init(null, 1, 1).order(Location.init(null, 1, 0)));
    p(Location.init(null, 1, 1).order(Location.init(null, 1, 1)));
    p(Location.init(null, 1, 1).order(Location.init(null, 1, 2)));

    p(Location.init(null, 1, 1).order(Location.init(null, 0, 1)));
    p(Location.init(null, 1, 1).order(Location.init(null, 1, 1)));
    p(Location.init(null, 1, 1).order(Location.init(null, 2, 1)));

    p(Location.init(null, 1, 1).isBetween(Location.init(null, 1, 0), Location.init(null, 1, 2)));
    p(Location.init(null, 1, 1).isBetween(Location.init(null, 0, 1), Location.init(null, 2, 1)));
    p(Location.init(null, 1, 1).isBetween(Location.init(null, 1, 1), Location.init(null, 1, 2)));
    p(Location.init(null, 1, 1).isBetween(Location.init(null, 1, 2), Location.init(null, 1, 2)));
    p(Location.init(null, 1, 1).isBetween(Location.init(null, 1, 1), Location.init(null, 2, 1)));
    p(Location.init(null, 1, 1).isBetween(Location.init(null, 2, 1), Location.init(null, 2, 1)));
}
