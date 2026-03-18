// papers-builder.mjs — Auto-build all AC paper PDFs from LaTeX sources
//
// Triggered via POST /papers-build or by papers-git-poller.mjs when
// papers/ paths change on main. Runs `node papers/cli.mjs publish`
// (xelatex 3-pass + deploy + index update + verify).

import { promises as fs } from "fs";
import path from "path";
import { randomUUID } from "crypto";
import { spawn } from "child_process";

const MAX_RECENT_JOBS = 10;
const MAX_LOG_LINES = 2000;

// papers/cli.mjs lives inside the git clone at /opt/oven/native-git/papers/
const GIT_REPO_DIR =
  process.env.NATIVE_GIT_DIR || "/opt/oven/native-git";

const jobs = new Map();
const jobOrder = [];
let activeJobId = null;

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

async function runPapersJob(job) {
  try {
    job.status = "running";
    job.startedAt = nowISO();
    job.percent = 0;
    buildCount = 0;

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
  return {
    activeJobId,
    active: activeJobId ? makeSnapshot(jobs.get(activeJobId)) : null,
    recent: jobOrder
      .map((id) => jobs.get(id))
      .filter(Boolean)
      .map((j) => makeSnapshot(j)),
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
