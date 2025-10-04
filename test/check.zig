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

    const machine_type = machine_type: {
        var machine_type: std.coff.IMAGE.FILE.MACHINE = .AMD64;
        if (args.len >= 3) {
            machine_type = std.meta.stringToEnum(std.coff.IMAGE.FILE.MACHINE, args[2]) orelse {
                std.debug.print("unknown or unsupported machine type: {s}\n", .{args[2]});
                std.process.exit(1);
            };
        }
        break :machine_type machine_type;
    };

    const root_dir_path = args[1];
    var root_dir = try std.fs.cwd().openDir(root_dir_path, .{ .iterate = true });
    defer root_dir.close();

    var walker = try root_dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        const is_def = std.mem.eql(u8, std.fs.path.extension(entry.basename), ".def");
        if (!is_def) continue;

        const input = try entry.dir.readFileAllocOptions(entry.basename, allocator, .unlimited, .of(u8), 0);
        defer allocator.free(input);

        const stem = entry.basename[0 .. std.mem.indexOfScalar(u8, entry.basename, '.') orelse entry.basename.len];
        const lib_basename = try std.mem.concat(allocator, u8, &.{ stem, ".lib" });
        defer allocator.free(lib_basename);
        const expected_output = entry.dir.readFileAlloc(lib_basename, allocator, .unlimited) catch |err| switch (err) {
            error.FileNotFound => {
                std.debug.print("no matching .lib for {s}, skipping\n", .{entry.path});
                continue;
            },
            else => |e| return e,
        };
        defer allocator.free(expected_output);

        std.debug.print("{s}\n", .{entry.path});
        try check(allocator, input, expected_output, machine_type);
    }
}

fn check(allocator: std.mem.Allocator, input: [:0]const u8, expected_output: []const u8, machine_type: std.coff.IMAGE.FILE.MACHINE) !void {
    var diagnostics: def.Diagnostics = undefined;
    var module_def = def.parse(allocator, input, machine_type, .mingw, &diagnostics) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        error.ParseError => {
            std.debug.print("{}: {} {s}\n", .{ diagnostics.err, diagnostics.token, diagnostics.token.slice(input) });
            return err;
        },
    };
    defer module_def.deinit();

    module_def.fixupForImportLibraryGeneration(machine_type);

    const members = try implib.getMembers(allocator, module_def, machine_type);
    defer members.deinit();

    var alloc_writer: std.Io.Writer.Allocating = .init(allocator);
    defer alloc_writer.deinit();
    try implib.writeCoffArchive(allocator, &alloc_writer.writer, members);

    try std.testing.expectEqualSlices(u8, expected_output, alloc_writer.written());
}
