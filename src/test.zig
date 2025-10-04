const std = @import("std");
const def = @import("def.zig");
const implib = @import("implib.zig");

test "foo" {
    try check(@embedFile("foo.def"), @embedFile("foo.lib"));
}

test "longnames" {
    try check(@embedFile("foo-longname.def"), @embedFile("foo-longname.lib"));
}

fn check(input: [:0]const u8, expected: []const u8) !void {
    const machine_type: std.coff.IMAGE.FILE.MACHINE = .AMD64;

    var diagnostics: def.Diagnostics = undefined;
    var module_def = def.parse(std.testing.allocator, input, machine_type, .mingw, &diagnostics) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        error.ParseError => {
            std.debug.print("{}: {} {s}\n", .{ diagnostics.err, diagnostics.token, diagnostics.token.slice(input) });
            return err;
        },
    };
    defer module_def.deinit();

    module_def.fixupForImportLibraryGeneration(machine_type);

    const members = try implib.getMembers(std.testing.allocator, module_def, machine_type);
    defer members.deinit();

    var alloc_writer: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer alloc_writer.deinit();
    try implib.writeCoffArchive(std.testing.allocator, &alloc_writer.writer, members);

    try std.testing.expectEqualSlices(u8, expected, (alloc_writer.written()));
}
