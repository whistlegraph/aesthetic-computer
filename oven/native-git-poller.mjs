// native-git-poller.mjs — polls git for fedac/native/ changes, auto-triggers OTA builds
//
// Runs inside the oven server. Every POLL_INTERVAL_MS (default 60s), fetches
// origin/main from the configured native checkout remote and checks if any fedac/native/ paths changed since the last
// successful build. If so, pulls and triggers startNativeBuild().
//
// Requires a git clone at GIT_REPO_DIR (default /opt/oven/native-git/).
// deploy.sh sets this up on first deploy.

import { execFile } from "child_process";
import { promises as fs } from "fs";
import path from "path";

const POLL_INTERVAL_MS = parseInt(process.env.NATIVE_POLL_INTERVAL_MS || "0", 10);
const GIT_REPO_DIR = process.env.NATIVE_GIT_DIR || "/opt/oven/native-git";
const BRANCH = process.env.NATIVE_GIT_BRANCH || "main";
const HASH_FILE = path.join(GIT_REPO_DIR, ".last-built-hash");

let polling = false;
let timer = null;
let startBuildFn = null; // set via startPoller()
let logFn = (level, icon, msg) => console.log(`[native-git-poller] ${msg}`);

function git(args, cwd = GIT_REPO_DIR) {
  return new Promise((resolve, reject) => {
    execFile("git", args, { cwd, timeout: 30_000 }, (err, stdout, stderr) => {
      if (err) {
        err.stderr = stderr;
        return reject(err);
      }
      resolve(stdout.trim());
    });
  });
}

async function readLastBuiltHash() {
  try {
    return (await fs.readFile(HASH_FILE, "utf8")).trim();
  } catch {
    return null;
  }
}

async function writeLastBuiltHash(hash) {
  await fs.writeFile(HASH_FILE, hash + "\n", "utf8");
}

async function poll() {
  if (polling) return;
  polling = true;

  try {
    // Fetch latest from origin
    await git(["fetch", "origin", BRANCH, "--quiet"]);

    const remoteHead = await git(["rev-parse", `origin/${BRANCH}`]);
    const lastBuilt = await readLastBuiltHash();

    if (remoteHead === lastBuilt) {
      // No new commits
      polling = false;
      return;
    }

    // Check which files changed
    let changedPaths = "";
    if (lastBuilt) {
      try {
        const diffOutput = await git([
          "diff",
          "--name-only",
          lastBuilt,
          remoteHead,
        ]);
        changedPaths = diffOutput;
      } catch {
        // lastBuilt hash might not exist (force push, etc) — treat as full build
        changedPaths = "fedac/native/src/force-rebuild";
      }
    } else {
      // First run — treat as full build
      changedPaths = "fedac/native/src/force-rebuild";
    }

    // Filter to fedac/native/ and fedac/nixos/ paths
    const allPaths = changedPaths.split("\n");
    const nativePaths = allPaths.filter((p) => p.startsWith("fedac/native/"));
    const nixosPaths = allPaths.filter((p) => p.startsWith("fedac/nixos/"));

    if (nativePaths.length === 0 && nixosPaths.length === 0) {
      // Changes exist but not in fedac/native/ or fedac/nixos/ — update hash, skip build
      logFn("info", "⏭️", `New commits (${remoteHead.slice(0, 8)}) but no fedac/ build changes — skipping build`);
      await writeLastBuiltHash(remoteHead);
      polling = false;
      return;
    }

    // Determine variant based on which directories changed
    let variant = "c";
    if (nixosPaths.length > 0 && nativePaths.length === 0) variant = "nix";
    else if (nixosPaths.length > 0 && nativePaths.length > 0) variant = "all";

    // Hard-sync to origin so stale local edits/conflicts cannot leak into OTA builds.
    await git(["checkout", "-f", BRANCH, "--quiet"]);
    await git(["reset", "--hard", `origin/${BRANCH}`, "--quiet"]);
    await git(["clean", "-fdq"]);

    const relevantPaths = [...nativePaths, ...nixosPaths];
    logFn(
      "info",
      "🔨",
      `Build changes detected (${remoteHead.slice(0, 8)}): ${relevantPaths.length} file(s), variant=${variant} — triggering OTA build`
    );

    // Trigger build
    const job = await startBuildFn({
      ref: remoteHead,
      changed_paths: relevantPaths.join(","),
      variant,
    });

    logFn(
      "info",
      "🚀",
      `OTA build ${job.id} started (flags: ${job.flags.join(" ") || "full"})`
    );

    // Update hash after successfully starting (not completing) the build
    await writeLastBuiltHash(remoteHead);
  } catch (err) {
    if (err?.code === "NATIVE_BUILD_BUSY") {
      // Build already running — skip, will retry next poll
      logFn("info", "⏳", "Build already running — will retry next poll");
    } else {
      logFn(
        "error",
        "❌",
        `Git poll error: ${err.message}${err.stderr ? " | " + err.stderr.trim() : ""}`
      );
    }
  } finally {
    polling = false;
  }
}

// ── Public API ──────────────────────────────────────────────────────────────

export function startPoller({ startNativeBuild, addServerLog, nativeDir }) {
  startBuildFn = startNativeBuild;
  if (addServerLog) logFn = addServerLog;

  // Override NATIVE_DIR in the builder's env so it uses our git checkout
  if (nativeDir !== false) {
    process.env.NATIVE_DIR = path.join(GIT_REPO_DIR, "fedac", "native");
  }

  // Check that GIT_REPO_DIR exists before starting
  if (POLL_INTERVAL_MS <= 0) {
    logFn("info", "🛑", "Native git poller disabled (NATIVE_POLL_INTERVAL_MS=0). Use manual POST /native-build to trigger.");
    return;
  }

  fs.access(GIT_REPO_DIR)
    .then(() => {
      logFn(
        "info",
        "👁️",
        `Native git poller started (every ${POLL_INTERVAL_MS / 1000}s, repo: ${GIT_REPO_DIR})`
      );
      // First poll after a short delay to let the server settle
      setTimeout(poll, 5000);
      timer = setInterval(poll, POLL_INTERVAL_MS);
    })
    .catch(() => {
      logFn(
        "error",
        "⚠️",
        `Native git poller disabled — repo dir not found: ${GIT_REPO_DIR}. Run: git clone --branch main https://tangled.org/aesthetic.computer/core.git ${GIT_REPO_DIR}`
      );
    });
}

export function stopPoller() {
  if (timer) {
    clearInterval(timer);
    timer = null;
    logFn("info", "🛑", "Native git poller stopped");
  }
}

export function getPollerStatus() {
  return {
    running: timer !== null,
    intervalMs: POLL_INTERVAL_MS,
    repoDir: GIT_REPO_DIR,
    branch: BRANCH,
  };
}
