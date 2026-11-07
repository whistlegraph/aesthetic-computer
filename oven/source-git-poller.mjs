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
const RUNTIME_HASH_FILE = path.join(STATE_DIR, "source-last-synced-hash");
const OVEN_HASH_FILE = path.join(STATE_DIR, "oven-last-synced-hash");

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
  syncedHash: null, // the runtime lane's commit
  ovenHash: null, // the oven lane's commit
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

async function readHash(file) {
  try { return (await fs.readFile(file, "utf8")).trim(); } catch { return null; }
}
async function writeHash(file, hash) {
  await fs.mkdir(STATE_DIR, { recursive: true });
  await fs.writeFile(file, hash + "\n", "utf8");
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
  // A plain extract, then prune: GNU tar 1.35 selects nothing when member
  // patterns are combined with --strip-components (verified on jasellite).
  const keep = RUNTIME_EXTENSIONS.map((e) => `-name '*${e}'`).join(" -o ");
  await sh(`git -C '${GIT_REPO_DIR}' archive ${hash} '${RUNTIME_PREFIX}' | tar -x -m --strip-components=3 -C '${fresh}' && find '${fresh}' -type f ! \( ${keep} \) -delete && find '${fresh}' -type d -empty -delete`);
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
    const runtimeSynced = await readHash(RUNTIME_HASH_FILE), ovenSynced = await readHash(OVEN_HASH_FILE);
    health.syncedHash = runtimeSynced;
    health.ovenHash = ovenSynced;
    health.lastPollOk = true;
    health.lastError = null;
    health.consecutiveErrors = 0;
    if (remoteHead === runtimeSynced && remoteHead === ovenSynced) { polling = false; return; }

    // Each lane keeps its own marker and fails on its own: a broken runtime
    // export must not keep the oven's code (including a fix for it) from
    // landing, and a landed oven update restarts once, not every tick.
    // Without a marker (first run, force push) everything counts as changed.
    const changedSince = async (since) => {
      if (!since) return null;
      try { return (await git(["diff", "--name-only", since, remoteHead])).split("\n").filter(Boolean); } catch { return null; }
    };
    const failures = [];
    let ovenLanded = false;

    if (remoteHead !== ovenSynced && SELF_UPDATE) {
      const changed = await changedSince(ovenSynced);
      if (!changed || changed.some(isOvenChange)) {
        try {
          await updateOven(remoteHead, changed || []);
          health.ovenUpdatedAt = new Date().toISOString();
          ovenLanded = true;
          logFn("info", "🧰", `oven code at ${remoteHead.slice(0, 10)} (${changed ? changed.filter(isOvenChange).length : "all"} file(s))`);
        } catch (err) { failures.push(`oven: ${(err.stderr || err.message || String(err)).slice(0, 200)}`); }
      }
      if (!failures.length) { await writeHash(OVEN_HASH_FILE, remoteHead); health.ovenHash = remoteHead; }
    } else if (remoteHead !== ovenSynced) {
      logFn("info", "⏭️", `oven/ may have changed at ${remoteHead.slice(0, 10)} but OVEN_SELF_UPDATE=0 — run oven/deploy.sh`);
      await writeHash(OVEN_HASH_FILE, remoteHead); health.ovenHash = remoteHead;
    }

    if (remoteHead !== runtimeSynced) {
      const changed = await changedSince(runtimeSynced);
      if (!changed || changed.some(isRuntimeChange)) {
        try {
          await exportRuntime(remoteHead);
          health.runtimeSyncedAt = new Date().toISOString();
          onRuntimeSynced(remoteHead);
          logFn("info", "📥", `ac-source now at ${remoteHead.slice(0, 10)} (${changed ? changed.filter(isRuntimeChange).length : "all"} runtime file(s))`);
          await writeHash(RUNTIME_HASH_FILE, remoteHead); health.syncedHash = remoteHead;
        } catch (err) { failures.push(`runtime: ${(err.stderr || err.message || String(err)).slice(0, 200)}`); }
      } else {
        await writeHash(RUNTIME_HASH_FILE, remoteHead); health.syncedHash = remoteHead;
      }
    }

    if (ovenLanded) restartWhenIdle(remoteHead); // the new code may carry the runtime lane's fix
    if (failures.length) throw new Error(failures.join(" | ")); // its marker stays: the next tick retries
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
