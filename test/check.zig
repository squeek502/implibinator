const std = @import("std");
const implibinator = @import("implibinator");
const def = implibinator.def;
const implib = implibinator.implib;

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("expected root dir for def files as first argument", .{});
        std.process.exit(1);
    }

    const root_dir_path = args[1];
    var root_dir = try std.fs.cwd().openDir(root_dir_path, .{ .iterate = true });
    defer root_dir.close();

    var walker = try root_dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        const is_def = std.mem.eql(u8, std.fs.path.extension(entry.basename), ".def");
        if (!is_def) continue;

        const input = try entry.dir.readFileAllocOptions(allocator, entry.basename, std.math.maxInt(usize), null, .of(u8), 0);
        defer allocator.free(input);

        const stem = entry.basename[0 .. std.mem.indexOfScalar(u8, entry.basename, '.') orelse entry.basename.len];
        const lib_basename = try std.mem.concat(allocator, u8, &.{ stem, ".lib" });
        defer allocator.free(lib_basename);
        const expected_output = try entry.dir.readFileAlloc(allocator, lib_basename, std.math.maxInt(usize));
        defer allocator.free(expected_output);

        std.debug.print("{s}\n", .{entry.path});
        try check(allocator, input, expected_output);
    }
}

fn check(allocator: std.mem.Allocator, input: [:0]const u8, expected_output: []const u8) !void {
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

    var alloc_writer: std.io.Writer.Allocating = .init(allocator);
    defer alloc_writer.deinit();
    try implib.writeCoffArchive(allocator, &alloc_writer.writer, members);

    try std.testing.expectEqualSlices(u8, expected_output, alloc_writer.written());
}
