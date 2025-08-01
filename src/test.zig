const std = @import("std");
const def = @import("def.zig");
const implib = @import("implib.zig");

test "foo" {
    const input = @embedFile("foo.def");

    var diagnostics: def.Diagnostics = undefined;
    const module_def = def.parse(std.testing.allocator, input, &diagnostics) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        error.ParseError => {
            std.debug.print("{}: {} {s}\n", .{ diagnostics.err, diagnostics.token, diagnostics.token.slice(input) });
            return err;
        },
    };
    defer module_def.deinit();

    const members = try implib.getMembers(std.testing.allocator, module_def, .X64);
    defer members.deinit();

    var alloc_writer: std.io.Writer.Allocating = .init(std.testing.allocator);
    defer alloc_writer.deinit();
    try implib.writeCoffArchive(&alloc_writer.writer, members);

    // try std.fs.cwd().writeFile(.{
    //     .sub_path = "foo.zig.lib",
    //     .data = alloc_writer.getWritten(),
    // });

    const expected = @embedFile("foo.lib");
    try std.testing.expectEqualSlices(u8, expected[428..], (alloc_writer.getWritten())[8..]);
}
