// papers-builder.mjs — Auto-build all AC paper PDFs from LaTeX sources
//
// Triggered via POST /papers-build or by papers-git-poller.mjs when
// papers/ paths change on main. Runs `node papers/cli.mjs publish`
// (xelatex 3-pass + deploy + index update + verify).

import { promises as fs } from "fs";
import { mkdirSync, readFileSync, writeFileSync } from "fs";
import path from "path";
import { randomUUID } from "crypto";
import { spawn, execFile } from "child_process";

const MAX_RECENT_JOBS = 20;
const MAX_LOG_LINES = 2000;

// Job history is kept on disk as well as in memory. Held only in memory, the
// entire build record vanished on every oven restart, so `recent: []` was
// ambiguous between "never built" and "restarted since the last build" —
// exactly the question you need answered when the mill has gone quiet.
// Summaries only; logs stay in memory and are lost on restart as before.
const STATE_DIR = process.env.OVEN_STATE_DIR || "/opt/oven/state";
const HISTORY_FILE = path.join(STATE_DIR, "papers-build-history.json");

// papers/cli.mjs lives inside the git clone at /opt/oven/native-git/papers/
const GIT_REPO_DIR =
  process.env.NATIVE_GIT_DIR || "/opt/oven/native-git";

const jobs = new Map();
const jobOrder = [];
let activeJobId = null;

// Restarted-in jobs: summaries rehydrated from HISTORY_FILE, oldest last.
// They carry no logs and no live process — they exist so `recent` can still
// answer "when did this mill last turn, and did it work?".
let history = [];

function loadHistory() {
  try {
    const parsed = JSON.parse(readFileSync(HISTORY_FILE, "utf8"));
    if (Array.isArray(parsed)) history = parsed.slice(0, MAX_RECENT_JOBS);
  } catch {
    history = []; // absent or corrupt — start clean, never fatal
  }
}

function saveHistory(snapshot) {
  history = [snapshot, ...history.filter((h) => h.id !== snapshot.id)].slice(
    0,
    MAX_RECENT_JOBS,
  );
  try {
    mkdirSync(STATE_DIR, { recursive: true });
    writeFileSync(HISTORY_FILE, JSON.stringify(history, null, 2), "utf8");
  } catch {
    // A read-only or missing state dir must never take the build down.
  }
}

loadHistory();

function nowISO() {
  return new Date().toISOString();
}

function stripAnsi(s) {
  return String(s || "").replace(/\u001b\[[0-9;]*m/g, "");
}

// Estimate total papers from PAPER_MAP in cli.mjs (17 papers × 4 langs = 68 builds)
const ESTIMATED_BUILDS = 68;
let buildCount = 0;

function addLogLine(job, stream, line) {
  const clean = stripAnsi(line).replace(/\r/g, "").trimEnd();
  if (!clean) return;
  job.logs.push({ ts: nowISO(), stream, line: clean });
  if (job.logs.length > MAX_LOG_LINES)
    job.logs.splice(0, job.logs.length - MAX_LOG_LINES);
  job.updatedAt = nowISO();

  // Parse progress from cli.mjs publish output
  if (clean.match(/^\s+BUILD /)) {
    buildCount++;
    job.stage = "build";
    job.percent = Math.min(80, Math.round((buildCount / ESTIMATED_BUILDS) * 80));
  } else if (clean.match(/^\s+DEPLOY /)) {
    job.stage = "deploy";
    job.percent = Math.max(job.percent, 85);
  } else if (clean.includes("INDEX updated")) {
    job.stage = "index";
    job.percent = 90;
  } else if (clean.includes("VERIFY")) {
    job.stage = "verify";
    job.percent = 95;
  } else if (clean.includes("Publish complete")) {
    job.stage = "done";
    job.percent = 100;
  }
}

function makeSnapshot(job, opts = {}) {
  const { includeLogs = false, tail = 200 } = opts;
  const snap = {
    id: job.id,
    ref: job.ref,
    status: job.status,
    stage: job.stage,
    percent: job.percent,
    createdAt: job.createdAt,
    startedAt: job.startedAt,
    updatedAt: job.updatedAt,
    finishedAt: job.finishedAt,
    exitCode: job.exitCode,
    error: job.error,
    publishError: job.publishError || null,
    logCount: job.logs.length,
    elapsedMs: job.startedAt
      ? (job.finishedAt ? Date.parse(job.finishedAt) : Date.now()) -
        Date.parse(job.startedAt)
      : 0,
  };
  if (includeLogs) {
    const start = Math.max(0, job.logs.length - Math.max(0, tail));
    snap.logs = job.logs.slice(start);
  }
  return snap;
}

function wireStream(job, proc, streamName) {
  let pending = "";
  const s = streamName === "stdout" ? proc.stdout : proc.stderr;
  s.on("data", (chunk) => {
    pending += chunk.toString();
    let idx;
    while ((idx = pending.indexOf("\n")) >= 0) {
      addLogLine(job, streamName, pending.slice(0, idx));
      pending = pending.slice(idx + 1);
    }
  });
  s.on("end", () => {
    if (pending) addLogLine(job, streamName, pending);
  });
}

function git(args, cwd = GIT_REPO_DIR) {
  return new Promise((resolve, reject) => {
    execFile("git", args, { cwd, timeout: 60_000 }, (err, stdout, stderr) => {
      if (err) {
        err.stderr = stderr;
        return reject(err);
      }
      resolve(stdout.trim());
    });
  });
}

// rsync the built PDFs + platter HTML directly to lith (papers.aesthetic.computer
// origin), bypassing git for binary artifacts. Then commit the tiny text-only
// source-of-truth files (metadata.json + BUILDLOG.md) so the lith cache-purge
// webhook still fires.
//
// See oven/SETUP-LITH-RSYNC.md for the one-time SSH key provisioning.
async function publishToLith(job) {
  const SITE_DIR = path.join(GIT_REPO_DIR, "system", "public", "papers.aesthetic.computer");
  const LITH_HOST = process.env.LITH_PAPERS_HOST || "root@lith.aesthetic.computer";
  const LITH_DEST = process.env.LITH_PAPERS_DEST || "/opt/ac/system/public/papers.aesthetic.computer/";
  const SSH_KEY = process.env.LITH_SSH_KEY || "/root/.ssh/oven-to-lith";

  addLogLine(job, "stdout", `  RSYNC: pushing PDFs + platter to ${LITH_HOST}...`);
  job.stage = "rsync";
  job.percent = 94;

  try {
    await new Promise((resolve, reject) => {
      execFile(
        "rsync",
        [
          "-av",
          "--delete-after",
          "--include=*/",
          "--include=*.pdf",
          "--include=*.html",
          "--exclude=*",
          "-e", `ssh -i ${SSH_KEY} -o StrictHostKeyChecking=accept-new -o UserKnownHostsFile=/root/.ssh/oven-known-hosts`,
          SITE_DIR + "/",
          `${LITH_HOST}:${LITH_DEST}`,
        ],
        { timeout: 300_000 },
        (err, stdout, stderr) => {
          if (err) {
            err.stderr = stderr;
            return reject(err);
          }
          for (const line of stdout.split("\n").slice(-8)) {
            if (line.trim()) addLogLine(job, "stdout", "  RSYNC: " + line);
          }
          resolve();
        },
      );
    });
  } catch (rsyncErr) {
    addLogLine(job, "stderr", `  RSYNC FAILED: ${rsyncErr.message}${rsyncErr.stderr ? " | " + rsyncErr.stderr.trim() : ""}`);
    throw rsyncErr;
  }

  // Still commit metadata.json + BUILDLOG.md so the lith webhook fires and
  // purges its caches. These are text files — no binary churn.
  job.stage = "git-push";
  job.percent = 97;

  await git(["config", "user.email", "oven@aesthetic.computer"]);
  await git(["config", "user.name", "Oven (aesthetic.computer)"]);

  await git(["add", "papers/metadata.json", "papers/BUILDLOG.md"]).catch(() => {});

  const status = await git(["diff", "--cached", "--name-only"]);
  if (!status) {
    addLogLine(job, "stdout", "  GIT: no metadata changes — rsync only");
    return;
  }

  const msg = `[papers] oven auto-build: metadata + buildlog`;
  await git(["commit", "-m", msg]);

  // Discard byproduct (xelatex .log/.toc updates, dirty intermediate PDFs,
  // .last-papers-built-hash) so pull --rebase can run cleanly.
  try {
    await git(["checkout", "--", "."]);
  } catch (cleanupErr) {
    addLogLine(job, "stderr", `  GIT: pre-rebase cleanup note: ${cleanupErr.message || cleanupErr}`);
  }

  try {
    await git(["pull", "--rebase", "origin", "main"]);
  } catch (pullErr) {
    try { await git(["rebase", "--abort"]); } catch {}
    throw pullErr;
  }

  job.percent = 99;
  await git(["push", "origin", "main"]);
  addLogLine(job, "stdout", `  GIT: pushed — ${msg}`);
}

async function runPapersJob(job) {
  try {
    job.status = "running";
    job.startedAt = nowISO();
    job.percent = 0;
    buildCount = 0;

    // Ensure deploy directory is writable (PDFs committed by others may have restrictive perms)
    const deployDir = path.join(GIT_REPO_DIR, "system", "public", "papers.aesthetic.computer");
    try {
      await new Promise((res) => execFile("chmod", ["-R", "u+w", deployDir], () => res()));
    } catch {}

    // Single phase: node papers/cli.mjs publish
    const cliPath = path.join(GIT_REPO_DIR, "papers", "cli.mjs");
    job.stage = "build";
    job.updatedAt = nowISO();

    await new Promise((resolve, reject) => {
      const proc = spawn("node", [cliPath, "publish"], {
        cwd: GIT_REPO_DIR,
        env: {
          ...process.env,
          TERM: "dumb",
          CLICOLOR: "0",
          FORCE_COLOR: "0",
        },
        stdio: ["ignore", "pipe", "pipe"],
      });
      job.process = proc;
      job.pid = proc.pid;
      wireStream(job, proc, "stdout");
      wireStream(job, proc, "stderr");
      proc.on("error", reject);
      proc.on("close", (code) => {
        job.process = null;
        if (code !== 0) reject(new Error(`papers publish failed (exit ${code})`));
        else resolve();
      });
    });

    // rsync built PDFs to lith; commit metadata so cache-purge webhook fires
    try {
      await publishToLith(job);
    } catch (pushErr) {
      // A build whose PDFs never reach papers.aesthetic.computer is not a
      // success, however clean the xelatex runs were — reporting it as one is
      // how a broken papermill looks healthy from the outside. The PDFs *did*
      // build, so say exactly that and let the status endpoint carry it.
      const detail = `${pushErr.message}${pushErr.stderr ? " | " + pushErr.stderr.trim() : ""}`;
      addLogLine(job, "stderr", `  PUBLISH FAILED: ${detail}`);
      job.publishError = detail;
      job.status = "failed";
      job.stage = "publish-failed";
      job.percent = 100;
      job.error = `PDFs built, but publishing to lith failed: ${detail}`;
      job.finishedAt = nowISO();
      return;
    }

    job.status = "success";
    job.stage = "done";
    job.percent = 100;
    job.finishedAt = nowISO();
  } catch (err) {
    job.finishedAt = nowISO();
    job.status = job.status === "cancelled" ? "cancelled" : "failed";
    job.stage = job.status;
    job.error = err.message || String(err);
  } finally {
    if (activeJobId === job.id) activeJobId = null;
    saveHistory(makeSnapshot(job));
  }
}

export async function startPapersBuild(options = {}) {
  if (activeJobId) {
    const err = new Error(`Papers build already running: ${activeJobId}`);
    err.code = "PAPERS_BUILD_BUSY";
    err.activeJobId = activeJobId;
    throw err;
  }

  const id = randomUUID().slice(0, 10);
  const job = {
    id,
    ref: options.ref || "unknown",
    status: "queued",
    stage: "queued",
    percent: 0,
    createdAt: nowISO(),
    startedAt: null,
    updatedAt: nowISO(),
    finishedAt: null,
    pid: null,
    process: null,
    exitCode: null,
    error: null,
    logs: [],
  };

  jobs.set(id, job);
  jobOrder.unshift(id);
  while (jobOrder.length > MAX_RECENT_JOBS) {
    const old = jobOrder.pop();
    if (old !== activeJobId) jobs.delete(old);
  }
  activeJobId = id;
  runPapersJob(job).catch(() => {});
  return makeSnapshot(job);
}

export function getPapersBuild(jobId, opts = {}) {
  const job = jobs.get(jobId);
  return job ? makeSnapshot(job, opts) : null;
}

export function getPapersBuildsSummary() {
  const live = jobOrder
    .map((id) => jobs.get(id))
    .filter(Boolean)
    .map((j) => makeSnapshot(j));
  const liveIds = new Set(live.map((j) => j.id));
  return {
    activeJobId,
    active: activeJobId ? makeSnapshot(jobs.get(activeJobId)) : null,
    // This process's jobs first, then whatever earlier runs left on disk.
    recent: [...live, ...history.filter((h) => !liveIds.has(h.id))].slice(
      0,
      MAX_RECENT_JOBS,
    ),
    lastFinished: [...live, ...history].find((j) => j.finishedAt) || null,
  };
}

export function cancelPapersBuild(jobId) {
  const job = jobs.get(jobId);
  if (!job) return { ok: false, error: "not found" };
  if (job.status !== "running" || !job.process)
    return { ok: false, error: "not running" };
  try {
    job.process.kill("SIGTERM");
    job.status = "cancelled";
    return { ok: true };
  } catch (err) {
    return { ok: false, error: err.message };
  }
}
