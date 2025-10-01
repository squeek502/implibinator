const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const use_llvm: ?bool = b.option(bool, "llvm", "Force llvm backend") orelse null;

    const mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const main_exe = b.addExecutable(.{
        .name = "implibinator",
        .use_llvm = use_llvm,
        .use_lld = use_llvm,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    main_exe.root_module.addImport("implibinator", mod);
    b.installArtifact(main_exe);

    {
        const check_exe = b.addExecutable(.{
            .name = "check",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/check.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        check_exe.root_module.addImport("implibinator", mod);

        const install_check = b.addInstallArtifact(check_exe, .{});
        const check_step = b.step("check", "Build and install check tool");
        check_step.dependOn(&install_check.step);
    }

    {
        const gen_exe = b.addExecutable(.{
            .name = "gen",
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/gen.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        gen_exe.root_module.addImport("implibinator", mod);

        const install_gen = b.addInstallArtifact(gen_exe, .{});
        const gen_step = b.step("gen", "Build and install gen tool");
        gen_step.dependOn(&install_gen.step);
    }

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_tests.step);
}
