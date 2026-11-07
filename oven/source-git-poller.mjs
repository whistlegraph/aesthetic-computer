// source-git-poller.mjs — keeps the oven level with origin/main on its own.
//
// Two things on this box used to move only when someone ran oven/deploy.sh
// from their laptop: the runtime copy the bundler packs (ac-source/, a code-only
// export of system/public/aesthetic.computer) and the oven's own code. A deploy
// of the site from the knot left both behind, so a $code packed or previewed
// here could run an older viewer than production (2026-11-07: the object
// viewer's #bg pragma was live on aesthetic.computer and absent from the oven's
// turntables).
//
// Every SOURCE_POLL_INTERVAL_MS (default 60s) this fetches origin/main in the
// shared clone at GIT_REPO_DIR (the papers poller's) and diffs against the last
// synced commit, kept under .git/oven-state/ like the other markers.
//
//   system/public/aesthetic.computer/** changed → export .mjs .js .json .lisp
//     (deploy.sh's filter) into ac-source/ by swapping in a fresh directory,
//     then tell the bundler the source revision moved (its core cache keys on it).
//   oven/** changed (and OVEN_SELF_UPDATE is not 0) → lay the new oven files over
//     /opt/oven (an archive never carries node_modules, .env, ac-source,
//     native-git, state, secrets or private-products, so those stay), run
//     `npm ci --omit=dev` if package-lock.json moved, check server.mjs parses,
//     and exit 0 once no job is running: systemd's Restart=always brings the
//     new code up. The version /status reports then comes from here.
//
// SOURCE_POLL_INTERVAL_MS=0 turns the whole poller off.
import { execFile } from "child_process";
import { promises as fs } from "fs";
import path from "path";
import { fileURLToPath } from "url";

const POLL_INTERVAL_MS = parseInt(process.env.SOURCE_POLL_INTERVAL_MS || "60000", 10);
const GIT_REPO_DIR = process.env.NATIVE_GIT_DIR || "/opt/oven/native-git";
const BRANCH = process.env.NATIVE_GIT_BRANCH || "main";
const OVEN_DIR = path.dirname(fileURLToPath(import.meta.url));
const AC_SOURCE_DIR = process.env.AC_SOURCE_DIR || path.join(OVEN_DIR, "ac-source");
const SELF_UPDATE = process.env.OVEN_SELF_UPDATE !== "0";
const STATE_DIR = path.join(GIT_REPO_DIR, ".git", "oven-state");
const HASH_FILE = path.join(STATE_DIR, "source-last-synced-hash");

const RUNTIME_PREFIX = "system/public/aesthetic.computer/";
const RUNTIME_EXTENSIONS = [".mjs", ".js", ".json", ".lisp"]; // what deploy.sh ships
const OVEN_PREFIX = "oven/";

let polling = false;
let timer = null;
let logFn = (level, icon, msg) => console.log(`[source-git-poller] ${msg}`);
let isBusy = () => false;
let onRuntimeSynced = () => {};
let exiting = false;

const health = {
  enabled: true,
  disabledReason: null,
  startedAt: null,
  lastPollAt: null,
  lastPollOk: null,
  lastError: null,
  consecutiveErrors: 0,
  syncedHash: null,
  runtimeSyncedAt: null,
  ovenUpdatedAt: null,
  restartPending: false,
};

function git(args, cwd = GIT_REPO_DIR) {
  return new Promise((resolve, reject) => {
    execFile("git", args, { cwd, timeout: 60_000, maxBuffer: 16 << 20 }, (err, stdout, stderr) => {
      if (err) { err.stderr = stderr; return reject(err); }
      resolve(stdout.trim());
    });
  });
}

function sh(script, opts = {}) {
  return new Promise((resolve, reject) => {
    execFile("sh", ["-c", script], { timeout: 600_000, maxBuffer: 16 << 20, ...opts }, (err, stdout, stderr) => {
      if (err) { err.stderr = stderr; return reject(err); }
      resolve(stdout.trim());
    });
  });
}

async function readSyncedHash() {
  try { return (await fs.readFile(HASH_FILE, "utf8")).trim(); } catch { return null; }
}
async function writeSyncedHash(hash) {
  await fs.mkdir(STATE_DIR, { recursive: true });
  await fs.writeFile(HASH_FILE, hash + "\n", "utf8");
  health.syncedHash = hash;
}

const isRuntimeChange = (p) => p.startsWith(RUNTIME_PREFIX) && RUNTIME_EXTENSIONS.includes(path.extname(p).toLowerCase());
const isOvenChange = (p) => p.startsWith(OVEN_PREFIX) && !p.endsWith(".md");

// The runtime export: a code-only tree of the public runtime at `hash`, built
// beside ac-source/ and swapped in whole, so a request never reads half a sync.
// `tar -m` stamps the files with now: the bundler watches disk.mjs's mtime.
async function exportRuntime(hash) {
  const fresh = AC_SOURCE_DIR + ".next", old = AC_SOURCE_DIR + ".prev";
  await fs.rm(fresh, { recursive: true, force: true });
  await fs.mkdir(fresh, { recursive: true });
  const globs = RUNTIME_EXTENSIONS.map((e) => `'*${e}'`).join(" ");
  // --wildcards-match-slash: a member is "system/public/aesthetic.computer/lib/disk.mjs",
  // and without it `*.mjs` cannot cross the slashes (the first run extracted nothing).
  await sh(`git -C '${GIT_REPO_DIR}' archive ${hash} '${RUNTIME_PREFIX}' | tar -x -m --strip-components=3 --wildcards --wildcards-match-slash ${globs} -C '${fresh}'`);
  await fs.access(path.join(fresh, "lib", "disk.mjs")); // a runtime without its core is not a runtime
  await fs.rm(old, { recursive: true, force: true });
  try { await fs.rename(AC_SOURCE_DIR, old); } catch {}
  await fs.rename(fresh, AC_SOURCE_DIR);
  await fs.rm(old, { recursive: true, force: true });
}

// The oven's own files at `hash`, laid over this directory. Nothing outside the
// archive is touched, so local state and secrets survive; files deleted upstream
// linger until the next deploy.sh, which rsyncs with --delete.
async function updateOven(hash, changed) {
  await sh(`git -C '${GIT_REPO_DIR}' archive ${hash} '${OVEN_PREFIX}' | tar -x -m --strip-components=1 -C '${OVEN_DIR}'`);
  if (changed.includes("oven/package-lock.json") || changed.includes("oven/package.json")) {
    logFn("info", "📦", "package-lock.json moved — npm ci --omit=dev");
    await sh("npm ci --omit=dev --no-audit --no-fund", { cwd: OVEN_DIR });
  }
  await sh(`node --check '${path.join(OVEN_DIR, "server.mjs")}' && node --check '${path.join(OVEN_DIR, "bundler.mjs")}'`);
}

// Exit once no job is running; systemd restarts the service on the new code.
function restartWhenIdle(hash) {
  if (exiting) return;
  exiting = true;
  health.restartPending = true;
  const tick = () => {
    if (isBusy()) { setTimeout(tick, 5000); return; }
    logFn("info", "🔁", `oven code at ${hash.slice(0, 10)} is on disk — exiting for systemd to restart`);
    setTimeout(() => process.exit(0), 300);
  };
  setTimeout(tick, 1000);
}

async function poll() {
  if (polling || exiting) return;
  polling = true;
  health.lastPollAt = new Date().toISOString();
  try {
    await git(["fetch", "origin", BRANCH, "--quiet"]);
    const remoteHead = await git(["rev-parse", `origin/${BRANCH}`]);
    const synced = await readSyncedHash();
    health.syncedHash = synced;
    health.lastPollOk = true;
    health.lastError = null;
    health.consecutiveErrors = 0;
    if (remoteHead === synced) { polling = false; return; }

    // Without a marker (first run after this poller lands), or after a force
    // push, everything counts as changed: one full sync, then diffs.
    let changed = null;
    if (synced) {
      try { changed = (await git(["diff", "--name-only", synced, remoteHead])).split("\n").filter(Boolean); } catch { changed = null; }
    }
    const runtime = changed ? changed.some(isRuntimeChange) : true;
    const oven = changed ? changed.some(isOvenChange) : true;

    // Each lane fails on its own: a broken runtime export must not keep the
    // oven's own code (including a fix for that export) from landing.
    const failures = [];
    let ovenLanded = false;
    if (runtime) {
      try {
        await exportRuntime(remoteHead);
        health.runtimeSyncedAt = new Date().toISOString();
        onRuntimeSynced(remoteHead);
        logFn("info", "📥", `ac-source now at ${remoteHead.slice(0, 10)} (${changed ? changed.filter(isRuntimeChange).length : "all"} runtime file(s))`);
      } catch (err) { failures.push(`runtime: ${(err.stderr || err.message || String(err)).slice(0, 200)}`); }
    }
    if (oven) {
      if (!SELF_UPDATE) {
        logFn("info", "⏭️", `oven/ changed at ${remoteHead.slice(0, 10)} but OVEN_SELF_UPDATE=0 — run oven/deploy.sh`);
      } else {
        try {
          await updateOven(remoteHead, changed || []);
          health.ovenUpdatedAt = new Date().toISOString();
          ovenLanded = true;
          logFn("info", "🧰", `oven code at ${remoteHead.slice(0, 10)} (${changed ? changed.filter(isOvenChange).length : "all"} file(s))`);
        } catch (err) { failures.push(`oven: ${(err.stderr || err.message || String(err)).slice(0, 200)}`); }
      }
    }
    if (!runtime && !oven) logFn("info", "⏭️", `new commits (${remoteHead.slice(0, 10)}) touch neither the runtime nor oven/`);
    if (failures.length) throw new Error(failures.join(" | ")); // marker stays: the next tick retries
    await writeSyncedHash(remoteHead);
    if (ovenLanded) restartWhenIdle(remoteHead);
  } catch (err) {
    health.lastPollOk = false;
    health.lastError = (err.stderr || err.message || String(err)).slice(0, 300);
    health.consecutiveErrors += 1;
    logFn("error", "⚠️", `poll failed: ${health.lastError}`);
  } finally {
    polling = false;
  }
}

// options: addServerLog(level, icon, msg); isBusy() → true while a job runs;
// onRuntimeSynced(hash) → the bundler's setSourceRevision.
export function startPoller({ addServerLog, isBusy: busy, onRuntimeSynced: synced } = {}) {
  if (addServerLog) logFn = addServerLog;
  if (busy) isBusy = busy;
  if (synced) onRuntimeSynced = synced;
  if (!POLL_INTERVAL_MS) {
    health.enabled = false;
    health.disabledReason = "SOURCE_POLL_INTERVAL_MS=0";
    logFn("info", "⏸️", "source poller off (SOURCE_POLL_INTERVAL_MS=0)");
    return;
  }
  health.startedAt = new Date().toISOString();
  logFn("info", "🛰️", `source poller: origin/${BRANCH} every ${POLL_INTERVAL_MS / 1000}s → ac-source${SELF_UPDATE ? " + oven self-update" : ""}`);
  setTimeout(poll, 15_000); // let the server finish booting first
  timer = setInterval(poll, POLL_INTERVAL_MS);
}

export function stopPoller() {
  if (timer) { clearInterval(timer); timer = null; }
}

export function getPollerStatus() {
  return { ...health, intervalMs: POLL_INTERVAL_MS, selfUpdate: SELF_UPDATE, repo: GIT_REPO_DIR, acSource: AC_SOURCE_DIR };
}
