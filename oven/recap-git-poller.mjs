// recap-git-poller.mjs — polls git for recap/audience/*.mjs changes,
// auto-triggers recap mp4 builds via startRecapBuild.
//
// Runs inside the oven server. Every POLL_INTERVAL_MS (default 90s),
// fetches origin/main and checks if any audience config changed since
// the last successful build. If so, pulls and triggers startRecapBuild
// for each changed audience (one at a time — recap-builder serializes).
//
// Shares the git clone at GIT_REPO_DIR with native-git-poller and
// papers-git-poller. Uses .last-recap-built-hash to track state.

import { execFile } from "child_process";
import { promises as fs } from "fs";
import path from "path";

const POLL_INTERVAL_MS = parseInt(process.env.RECAP_POLL_INTERVAL_MS || "90000", 10);
const GIT_REPO_DIR = process.env.NATIVE_GIT_DIR || "/opt/oven/native-git";
const BRANCH = process.env.NATIVE_GIT_BRANCH || "main";
const HASH_FILE = path.join(GIT_REPO_DIR, ".last-recap-built-hash");

// Paths whose changes trigger a recap rebuild. We watch only audience
// configs — changes to bin/* or pipeline.fish would force rebuilds of
// every audience and aren't worth the cost (~$5 per cut).
const TRIGGER_PREFIX = "recap/audience/";
const SOURCE_EXTS = [".mjs"];

let polling = false;
let timer = null;
let startBuildFn = null;
let logFn = (level, icon, msg) => console.log(`[recap-git-poller] ${msg}`);

function git(args, cwd = GIT_REPO_DIR) {
  return new Promise((resolve, reject) => {
    execFile("git", args, { cwd, timeout: 30_000 }, (err, stdout, stderr) => {
      if (err) { err.stderr = stderr; return reject(err); }
      resolve(stdout.trim());
    });
  });
}

async function readLastBuiltHash() {
  try { return (await fs.readFile(HASH_FILE, "utf8")).trim(); } catch { return null; }
}
async function writeLastBuiltHash(hash) {
  await fs.writeFile(HASH_FILE, hash + "\n", "utf8");
}

function isTriggerPath(filePath) {
  if (!filePath.startsWith(TRIGGER_PREFIX)) return false;
  const ext = filePath.slice(filePath.lastIndexOf(".")).toLowerCase();
  return SOURCE_EXTS.includes(ext);
}

// "recap/audience/jeffrey-73h-2026-05-02.mjs" → "jeffrey-73h-2026-05-02"
function audienceNameFromPath(filePath) {
  const base = path.basename(filePath);
  return base.replace(/\.mjs$/, "");
}

async function poll() {
  if (polling) return;
  polling = true;

  try {
    await git(["fetch", "origin", BRANCH, "--quiet"]);
    const remoteHead = await git(["rev-parse", `origin/${BRANCH}`]);
    const lastBuilt = await readLastBuiltHash();
    if (remoteHead === lastBuilt) { polling = false; return; }

    let changedPaths = "";
    if (lastBuilt) {
      try {
        changedPaths = await git(["diff", "--name-only", lastBuilt, remoteHead]);
      } catch {
        // lastBuilt missing — only build if there are explicit recap audience
        // changes in the remote head's tree (avoid a full force-build sweep
        // through every audience, which would cost real money).
        changedPaths = "";
      }
    }

    const audienceChanges = changedPaths
      .split("\n")
      .filter(isTriggerPath)
      .map(audienceNameFromPath);

    if (audienceChanges.length === 0) {
      logFn("info", "⏭️", `New commits (${remoteHead.slice(0, 8)}) but no recap/audience/ changes — skipping`);
      await writeLastBuiltHash(remoteHead);
      polling = false;
      return;
    }

    // Pull so the build runs against the latest tree
    await git(["checkout", BRANCH, "--quiet"]);
    await git(["merge", `origin/${BRANCH}`, "--ff-only", "--quiet"]);

    // Trigger one build per changed audience, serialized by recap-builder
    // (the start function rejects with RECAP_BUILD_BUSY if one is running).
    const unique = [...new Set(audienceChanges)];
    logFn("info", "🎬", `Recap changes detected (${remoteHead.slice(0, 8)}): ${unique.join(", ")}`);

    let queued = 0;
    for (const audience of unique) {
      try {
        const job = await startBuildFn({ audience, ref: remoteHead });
        logFn("info", "🚀", `Recap build ${job.id} started for ${audience}`);
        queued++;
        // If the builder is busy with the previous, retry on next poll
        break;
      } catch (err) {
        if (err?.code === "RECAP_BUILD_BUSY") {
          logFn("info", "⏳", `Recap builder busy — will retry next poll for ${audience}`);
          break;
        }
        logFn("error", "❌", `Failed to start recap build for ${audience}: ${err.message}`);
      }
    }

    // Only mark hash as built once nothing remains queued (so the next poll
    // continues serializing through any backlog).
    if (queued === unique.length) {
      await writeLastBuiltHash(remoteHead);
    }
  } catch (err) {
    if (err?.code === "RECAP_BUILD_BUSY") {
      logFn("info", "⏳", "Recap build already running — will retry next poll");
    } else {
      logFn("error", "❌", `Recap git poll error: ${err.message}${err.stderr ? " | " + err.stderr.trim() : ""}`);
    }
  } finally {
    polling = false;
  }
}

// ── Public API ─────────────────────────────────────────────────────────

export function startPoller({ startRecapBuild, addServerLog }) {
  startBuildFn = startRecapBuild;
  if (addServerLog) logFn = addServerLog;

  fs.access(GIT_REPO_DIR)
    .then(() => {
      logFn("info", "🎬", `Recap git poller started (every ${POLL_INTERVAL_MS / 1000}s, repo: ${GIT_REPO_DIR})`);
      // Stagger: native uses 5s, papers uses 15s, recap uses 25s
      setTimeout(poll, 25000);
      timer = setInterval(poll, POLL_INTERVAL_MS);
    })
    .catch(() => {
      logFn("error", "⚠️", `Recap git poller disabled — repo dir not found: ${GIT_REPO_DIR}`);
    });
}

export function stopPoller() {
  if (timer) {
    clearInterval(timer);
    timer = null;
    logFn("info", "🛑", "Recap git poller stopped");
  }
}

export function getPollerStatus() {
  return {
    running: timer !== null,
    intervalMs: POLL_INTERVAL_MS,
    repoDir: GIT_REPO_DIR,
    branch: BRANCH,
    triggerPrefix: TRIGGER_PREFIX,
  };
}
