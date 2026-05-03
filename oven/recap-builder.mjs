// recap-builder.mjs — Auto-build a recap mp4 from an audience config.
//
// Triggered via POST /recap-build (with `audience` in body) or by
// recap-git-poller.mjs when recap/audience/*.mjs changes on main.
// Runs `fish recap/pipeline.fish <audience>` inside the git clone at
// GIT_REPO_DIR (shared with papers-builder + native-builder).
//
// Mirrors papers-builder.mjs structure: queue + active job + log capture
// + SSE streaming + cancel. The pipeline emits "▸ N/8 <stage>" markers
// which we parse into percent.
//
// On success, the resulting mp4 is copied to RECAP_OUT_DIR/<audience>.mp4
// (default /opt/oven/recap-out/) and exposed via GET /recap-build/<id>/mp4.
// We do NOT commit mp4s back to git (~30 MB each — would bloat history).
// Future: upload to DO Spaces and commit a small manifest entry.

import { promises as fs } from "fs";
import path from "path";
import { randomUUID } from "crypto";
import { spawn } from "child_process";

const MAX_RECENT_JOBS = 10;
const MAX_LOG_LINES = 4000;

const GIT_REPO_DIR =
  process.env.NATIVE_GIT_DIR || "/opt/oven/native-git";
const RECAP_OUT_DIR =
  process.env.RECAP_OUT_DIR || "/opt/oven/recap-out";

// Pipeline stages, in order. Each "▸ N/8 <stage>" line bumps to the next
// stage. Percent is interpolated within each stage.
// Keys must match (as prefix in either direction) the first word after the
// "▸ N/8 " marker emitted by recap/pipeline.fish. So for "▸ 2/8 transcribe
// + align" the key must be "transcribe" (or a shorter prefix of it). For
// "▸ 3/8 jeffrey-photos ..." the key is "jeffrey" (since
// "jeffrey-photos".startsWith("jeffrey")).
const STAGES = [
  { key: "tts",        label: "tts",        weight: 5 },
  { key: "transcribe", label: "transcribe", weight: 5 },
  { key: "jeffrey",    label: "photos",     weight: 55 }, // dominant — ~3min × 13
  { key: "chat",       label: "chat-fetch", weight: 1 },
  { key: "screen",     label: "screenshots", weight: 1 },
  { key: "scout",      label: "scout",      weight: 2 },
  { key: "slides",     label: "slides",     weight: 12 },
  { key: "subtitles",  label: "subtitles",  weight: 4 },
  { key: "waltz",      label: "waltz",      weight: 5 },
  { key: "compose",    label: "compose",    weight: 10 },
];
const TOTAL_WEIGHT = STAGES.reduce((s, x) => s + x.weight, 0);

const jobs = new Map();
const jobOrder = [];
let activeJobId = null;

function nowISO() { return new Date().toISOString(); }
function stripAnsi(s) { return String(s || "").replace(/\[[0-9;]*m/g, ""); }

function addLogLine(job, stream, line) {
  const clean = stripAnsi(line).replace(/\r/g, "").trimEnd();
  if (!clean) return;
  job.logs.push({ ts: nowISO(), stream, line: clean });
  if (job.logs.length > MAX_LOG_LINES)
    job.logs.splice(0, job.logs.length - MAX_LOG_LINES);
  job.updatedAt = nowISO();

  // Stage transitions: pipeline.fish prints "▸ <step>/8 <name>" or
  // "▸ <step>.<sub>/8 <name>" between phases. Match the leading number.
  const stageMatch = clean.match(/▸\s+\d+(?:\.\d+)?\/\d+\s+([a-z][\w-]*)/i);
  if (stageMatch) {
    const name = stageMatch[1].toLowerCase();
    const idx = STAGES.findIndex((s) => name.startsWith(s.key) || s.key.startsWith(name));
    if (idx >= 0) {
      job.stageIdx = idx;
      job.stage = STAGES[idx].label;
      // Percent up to the start of this stage
      const cumWeight = STAGES.slice(0, idx).reduce((s, x) => s + x.weight, 0);
      job.percent = Math.min(99, Math.round((cumWeight / TOTAL_WEIGHT) * 100));
    }
  }

  // Per-stage progress hints (best-effort)
  if (job.stage === "photos" && /^\s*✓\s+\S+\.png/.test(clean)) {
    // Each photo done bumps a small amount within the photos band
    job.photosDone = (job.photosDone || 0) + 1;
  } else if (job.stage === "slides" && /^\s*✓\s+\d{2}_/.test(clean)) {
    job.slidesDone = (job.slidesDone || 0) + 1;
  }

  // Final success marker from pipeline.fish
  if (clean.includes("━━━ done ·")) {
    job.stage = "done";
    job.percent = 100;
  }
}

function makeSnapshot(job, opts = {}) {
  const { includeLogs = false, tail = 200 } = opts;
  const snap = {
    id: job.id,
    audience: job.audience,
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
    mp4Path: job.mp4Path,
    mp4Bytes: job.mp4Bytes,
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
  s.on("end", () => { if (pending) addLogLine(job, streamName, pending); });
}

async function copyOutputMp4(job) {
  const src = path.join(GIT_REPO_DIR, "recap", "out", "recap.mp4");
  await fs.mkdir(RECAP_OUT_DIR, { recursive: true });
  const dst = path.join(RECAP_OUT_DIR, `${job.audience}.mp4`);
  await fs.copyFile(src, dst);
  const stat = await fs.stat(dst);
  job.mp4Path = dst;
  job.mp4Bytes = stat.size;
  addLogLine(job, "stdout", `  OUT: copied recap.mp4 → ${dst} (${(stat.size / 1024 / 1024).toFixed(1)} MB)`);
}

async function runRecapJob(job) {
  try {
    job.status = "running";
    job.startedAt = nowISO();
    job.percent = 0;
    job.stage = "starting";
    job.stageIdx = -1;

    const cwd = path.join(GIT_REPO_DIR, "recap");
    addLogLine(job, "stdout", `▸ recap pipeline · audience=${job.audience}`);

    await new Promise((resolve, reject) => {
      const proc = spawn("fish", ["./pipeline.fish", job.audience], {
        cwd,
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
        job.exitCode = code;
        if (code !== 0) reject(new Error(`recap pipeline failed (exit ${code})`));
        else resolve();
      });
    });

    // Pipeline succeeded — copy the mp4 out of the git clone
    try {
      await copyOutputMp4(job);
    } catch (copyErr) {
      addLogLine(job, "stderr", `  OUT: copy failed: ${copyErr.message}`);
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
  }
}

export async function startRecapBuild(options = {}) {
  if (!options.audience || !/^[\w.-]+$/.test(options.audience)) {
    const err = new Error(`recap-build: missing or invalid audience name '${options.audience}'`);
    err.code = "RECAP_BUILD_BAD_AUDIENCE";
    throw err;
  }
  if (activeJobId) {
    const err = new Error(`Recap build already running: ${activeJobId}`);
    err.code = "RECAP_BUILD_BUSY";
    err.activeJobId = activeJobId;
    throw err;
  }

  const id = randomUUID().slice(0, 10);
  const job = {
    id,
    audience: options.audience,
    ref: options.ref || "unknown",
    status: "queued",
    stage: "queued",
    stageIdx: -1,
    percent: 0,
    createdAt: nowISO(),
    startedAt: null,
    updatedAt: nowISO(),
    finishedAt: null,
    pid: null,
    process: null,
    exitCode: null,
    error: null,
    mp4Path: null,
    mp4Bytes: null,
    logs: [],
  };

  jobs.set(id, job);
  jobOrder.unshift(id);
  while (jobOrder.length > MAX_RECENT_JOBS) {
    const old = jobOrder.pop();
    if (old !== activeJobId) jobs.delete(old);
  }
  activeJobId = id;
  runRecapJob(job).catch(() => {});
  return makeSnapshot(job);
}

export function getRecapBuild(jobId, opts = {}) {
  const job = jobs.get(jobId);
  return job ? makeSnapshot(job, opts) : null;
}

export function getRecapBuildsSummary() {
  return {
    activeJobId,
    active: activeJobId ? makeSnapshot(jobs.get(activeJobId)) : null,
    recent: jobOrder
      .map((id) => jobs.get(id))
      .filter(Boolean)
      .map((j) => makeSnapshot(j)),
  };
}

export function cancelRecapBuild(jobId) {
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

// Returns the on-disk mp4 path for a successful job, or null.
export function getRecapMp4Path(jobId) {
  const job = jobs.get(jobId);
  if (!job || job.status !== "success" || !job.mp4Path) return null;
  return job.mp4Path;
}
