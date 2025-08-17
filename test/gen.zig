const std = @import("std");

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("expected root dir for def files as first argument", .{});
        std.process.exit(1);
    }

    const root_dir_path = args[1];
    const target = if (args.len > 2) args[2] else "x86_64-windows-gnu";

    var root_dir = try std.fs.cwd().openDir(root_dir_path, .{ .iterate = true });
    defer root_dir.close();

    var walker = try root_dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        const is_def_or_def_in = std.mem.eql(u8, std.fs.path.extension(entry.basename), ".def") or (std.mem.eql(u8, std.fs.path.extension(entry.basename), ".in") and std.mem.eql(u8, std.fs.path.extension(std.fs.path.stem(entry.basename)), ".def"));
        if (!is_def_or_def_in) continue;
        if (std.mem.indexOf(u8, entry.path, "def-include") != null) continue;

        const full_path = try std.fs.path.join(allocator, &.{ root_dir_path, entry.path });
        defer allocator.free(full_path);
        std.debug.print("{s}\n", .{entry.path});
        gen(allocator, full_path, std.fs.path.dirname(entry.path) orelse ".", target) catch |err| switch (err) {
            error.ConversionFailed => {
                std.debug.print(" failed, skipping {s}\n", .{full_path});
            },
            else => |e| return e,
        };
    }
}

fn gen(allocator: std.mem.Allocator, full_path: []const u8, out_sub_dir_path: []const u8, target: []const u8) !void {
    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var argv: std.ArrayList([]const u8) = .empty;
    try argv.appendSlice(arena, &.{
        "zig4",
        "implib",
        "-outdir",
        try std.fs.path.join(arena, &.{ target, out_sub_dir_path }),
        "-target",
        target,
        full_path,
    });
    const result = try std.process.Child.run(.{
        .allocator = arena,
        .argv = argv.items,
    });

    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("{s}\n", .{try std.mem.join(arena, " ", argv.items)});
        return error.ConversionFailed;
    }
}
