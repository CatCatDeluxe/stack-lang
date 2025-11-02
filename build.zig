const std = @import("std");

fn modules(b: *std.Build, target: std.Build.ResolvedTarget, opt: std.builtin.OptimizeMode) *std.Build.Step.Compile {
	// We will also create a module for our other entry point, 'main.zig'.
	const exe_mod = b.createModule(.{
		// `root_source_file` is the Zig "entry point" of the module. If a module
		// only contains e.g. external object files, you can make this `null`.
		// In this case the main source file is merely a path, however, in more
		// complicated build scripts, this could be a generated file.
		.root_source_file = b.path("src/main.zig"),
		.target = target,
		.optimize = opt,
	});

	// This creates another `std.Build.Step.Compile`, but this one builds an executable
	// rather than a static library.
	const exe = b.addExecutable(.{
		.name = "stack_lang",
		.root_module = exe_mod,
	});

	b.installArtifact(exe);

	return exe;
}

pub fn build(b: *std.Build) void {
	const target = b.standardTargetOptions(.{});
	const optimize = b.standardOptimizeOption(.{});

	const exe = modules(b, target, optimize);

	const flags_dep = b.dependency("flags", .{
		.target = target,
		.optimize = optimize,
	});
	exe.root_module.addImport("flags", flags_dep.module("flags"));

	const run_cmd = b.addRunArtifact(exe);
	run_cmd.step.dependOn(b.getInstallStep());
	if (b.args) |args| run_cmd.addArgs(args);
	const run_step = b.step("run", "Run the app");
	run_step.dependOn(&run_cmd.step);

	const exe_unit_tests = b.addTest(.{
		.root_module = exe.root_module,
	});
	const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

	// Similar to creating the run step earlier, this exposes a `test` step to
	// the `zig build --help` menu, providing a way for the user to request
	// running the unit tests.
	const test_step = b.step("test", "Run unit tests");
	test_step.dependOn(&run_exe_unit_tests.step);
}
