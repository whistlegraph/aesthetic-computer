#!/usr/bin/env node
// memory/install-hooks.mjs
// Sets core.hooksPath to .githooks and ensures hook scripts are executable.

import { execFileSync } from "child_process";
import { chmod } from "fs/promises";
import { join } from "path";

function runGit(args, cwd) {
  return execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
}

async function ensureExecutable(path) {
  try {
    await chmod(path, 0o755);
  } catch {
    // Ignore missing hook files.
  }
}

async function main() {
  const repoRoot = runGit(["rev-parse", "--show-toplevel"], process.cwd());

  runGit(["config", "core.hooksPath", ".githooks"], repoRoot);

  await ensureExecutable(join(repoRoot, ".githooks", "pre-commit"));
  await ensureExecutable(join(repoRoot, ".githooks", "post-commit"));

  console.log("git hooks installed: core.hooksPath=.githooks");
}

main().catch((error) => {
  console.error(`install-hooks: ${error.message}`);
  process.exit(1);
});
