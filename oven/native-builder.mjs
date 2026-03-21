// native-builder.mjs — FedAC Native kernel OTA builds for oven
//
// Triggered via POST /native-build after commits to fedac/native/ on main.
// Runs build-and-flash.sh (no --flash) then upload-release.sh.
// Models os-base-build.mjs.

import { promises as fs } from "fs";
import path from "path";
import { randomUUID } from "crypto";
import { spawn } from "child_process";

const MAX_RECENT_JOBS = 10;
const MAX_LOG_LINES = 2000;

// fedac/native/ lives in the native-git repo on oven (polled by native-git-poller).
const NATIVE_DIR =
  process.env.NATIVE_DIR || "/opt/oven/native-git/fedac/native";

// Kernel build cache: symlinked from fedac/native/build so kernel object
// files survive rsync --delete between commits (5-10x faster warm builds).
const CACHE_DIR =
  process.env.NATIVE_CACHE_DIR || "/opt/oven/native-cache";

const jobs = new Map();
const jobOrder = [];
let activeJobId = null;

function nowISO() {
  return new Date().toISOString();
}

function stripAnsi(s) {
  return String(s || "").replace(/\u001b\[[0-9;]*m/g, "");
}

function addLogLine(job, stream, line) {
  const clean = stripAnsi(line).replace(/\r/g, "").trimEnd();
  if (!clean) return;
  job.logs.push({ ts: nowISO(), stream, line: clean });
  if (job.logs.length > MAX_LOG_LINES)
    job.logs.splice(0, job.logs.length - MAX_LOG_LINES);
  job.updatedAt = nowISO();

  // Parse progress hints from build-and-flash.sh + upload-release.sh output
  if (clean.includes("[build]") || clean.includes("[ac-os]")) {
    if (clean.match(/Building kernel|bzImage|vmlinuz/i)) {
      job.stage = "kernel";
      job.percent = Math.max(job.percent, 55);
    } else if (clean.match(/initramfs|cpio|lz4|Repacking|Copying firmware/i)) {
      job.stage = "initramfs";
      job.percent = Math.max(job.percent, 30);
    } else if (clean.match(/Building binary|Built:|ac-native|gcc|musl/i)) {
      job.stage = "binary";
      job.percent = Math.max(job.percent, 10);
    }
  }
  if (clean.match(/SMOKE TEST|smoke_test|qemu/i)) {
    job.stage = "smoke-test";
    job.percent = Math.max(job.percent, 80);
  }
  if (clean.match(/Uploading|uploaded:/i)) {
    job.stage = "upload";
    job.percent = Math.max(job.percent, 90);
  }
  if (clean.includes("Release published")) {
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
    flags: job.flags,
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

// Determine build-and-flash.sh flags based on which paths changed.
// Never skip binary: AC_BUILD_NAME and AC_GIT_HASH are compiled into the
// binary via CFLAGS and change on every commit. The Makefile's CFLAGS
// signature check (`.cflags` md5) handles incremental rebuilds efficiently —
// only object files are recompiled when flags change, not the full kernel.
// Skipping the binary causes version string mismatch (device shows stale name).
function buildFlagsFor(changedPaths = "") {
  return [];
}

// Symlink fedac/native/build → CACHE_DIR so kernel object files survive
// the rsync --delete that happens on every deploy/push sync.
async function setupBuildCache() {
  await fs.mkdir(CACHE_DIR, { recursive: true });
  const buildLink = path.join(NATIVE_DIR, "build");
  let stat;
  try {
    stat = await fs.lstat(buildLink);
  } catch {
    await fs.symlink(CACHE_DIR, buildLink);
    return;
  }
  if (stat.isSymbolicLink()) return;
  if (stat.isDirectory()) {
    try {
      await fs.rename(buildLink, CACHE_DIR);
    } catch {
      await fs.rm(buildLink, { recursive: true, force: true });
    }
  }
  await fs.symlink(CACHE_DIR, buildLink);
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

async function runPhase(job, label, cmd, args, cwd, extraEnv = {}) {
  job.stage = label;
  job.updatedAt = nowISO();
  return new Promise((resolve, reject) => {
    const proc = spawn(cmd, args, {
      cwd,
      env: {
        ...process.env,
        TERM: "dumb",
        CLICOLOR: "0",
        FORCE_COLOR: "0",
        ...extraEnv,
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
      if (code !== 0) reject(new Error(`${label} failed (exit ${code})`));
      else resolve();
    });
  });
}

async function runBuildJob(job) {
  try {
    await setupBuildCache();

    job.status = "running";
    job.startedAt = nowISO();
    job.percent = 0;

    // Phase 1: build vmlinuz (no --flash)
    const buildScript = path.join(NATIVE_DIR, "scripts/build-and-flash.sh");
    await runPhase(job, "build", "bash", [buildScript, ...job.flags], NATIVE_DIR);

    job.percent = 80;

    // Phase 2: QEMU smoke test (boot kernel, check serial for success/panic)
    const acOs = path.join(NATIVE_DIR, "ac-os");
    try {
      await runPhase(job, "smoke-test", "bash", [acOs, "test"], NATIVE_DIR);
      addLogLine(job, "stdout", "  SMOKE TEST: passed");
    } catch (smokeErr) {
      addLogLine(job, "stderr", `  SMOKE TEST: failed — ${smokeErr.message}`);
      // Non-fatal for now — log warning but continue upload
      // TODO: make this fatal once QEMU + virtio-gpu is reliable
    }

    job.percent = 85;

    // Phase 3: upload vmlinuz to DO Spaces CDN
    const vmlinuz = path.join(CACHE_DIR, "vmlinuz");
    const uploadScript = path.join(NATIVE_DIR, "scripts/upload-release.sh");
    await runPhase(job, "upload", "bash", [uploadScript, vmlinuz], NATIVE_DIR, {
      DO_SPACES_KEY: process.env.DO_SPACES_KEY || process.env.ART_SPACES_KEY || "",
      DO_SPACES_SECRET:
        process.env.DO_SPACES_SECRET || process.env.ART_SPACES_SECRET || "",
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

export async function startNativeBuild(options = {}) {
  if (activeJobId) {
    const err = new Error(`Native build already running: ${activeJobId}`);
    err.code = "NATIVE_BUILD_BUSY";
    err.activeJobId = activeJobId;
    throw err;
  }

  const id = randomUUID().slice(0, 10);
  const flags = buildFlagsFor(options.changed_paths || "");
  const job = {
    id,
    ref: options.ref || "unknown",
    flags,
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
  runBuildJob(job).catch(() => {});
  return makeSnapshot(job);
}

export function getNativeBuild(jobId, opts = {}) {
  const job = jobs.get(jobId);
  return job ? makeSnapshot(job, opts) : null;
}

export function getNativeBuildsSummary() {
  return {
    activeJobId,
    active: activeJobId ? makeSnapshot(jobs.get(activeJobId)) : null,
    recent: jobOrder
      .map((id) => jobs.get(id))
      .filter(Boolean)
      .map((j) => makeSnapshot(j)),
  };
}

export function cancelNativeBuild(jobId) {
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
