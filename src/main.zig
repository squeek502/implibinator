const std = @import("std");
const implibinator = @import("implibinator");
const def = implibinator.def;
const implib = implibinator.implib;

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.debug.print("usage: implibinator <def path> <lib path>", .{});
        std.process.exit(1);
    }

    const def_path = args[1];
    const input = try std.fs.cwd().readFileAllocOptions(def_path, allocator, .unlimited, .of(u8), 0);
    defer allocator.free(input);

    var diagnostics: def.Diagnostics = undefined;
    const module_def = def.parse(allocator, input, &diagnostics) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        error.ParseError => {
            std.debug.print("{}: {} {s}\n", .{ diagnostics.err, diagnostics.token, diagnostics.token.slice(input) });
            return err;
        },
    };
    defer module_def.deinit();

    const members = try implib.getMembers(allocator, module_def, .X64);
    defer members.deinit();

    var alloc_writer: std.Io.Writer.Allocating = .init(allocator);
    defer alloc_writer.deinit();
    try implib.writeCoffArchive(allocator, &alloc_writer.writer, members);

    try std.fs.cwd().writeFile(.{
        .sub_path = args[2],
        .data = alloc_writer.written(),
    });
}
