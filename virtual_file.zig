//! A VirtualFile is used as a Location's filename when
//! expanding a macro. It contains the macro expanded source
//! code so the user can debug it as if there was a file in the
//! filesystem with those contents.

const ast = @import("ast.zig");
const std = @import("std");

const Location = @import("location.zig");
const VirtualFile = @This();

/// The macro that produced this VirtualFile
macro: *ast.Macro,
macro_location: ?Location,

/// The expanded source code of the macro
source: []const u8,

/// The location where the macro was expanded (where the macro was invoked).
expanded_location: ?Location,

pub fn create(allocator: std.mem.Allocator, macro: *ast.Macro, macro_location: ?Location, source: []const u8, expanded_location: ?Location) !*VirtualFile {
    var instance = try allocator.create(VirtualFile);
    instance.* = .{
        .macro = macro,
        .macro_location = macro_location,
        .source = source,
        .expanded_location = expanded_location,
    };
    return instance;
}

pub fn format(self: VirtualFile, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    try writer.print("expanded macro: {s}", .{self.macro.name});
}
