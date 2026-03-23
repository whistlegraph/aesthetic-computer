// native-builder.mjs — FedAC Native kernel OTA builds for oven
//
// Triggered via POST /native-build after commits to fedac/native/ on main.
// Runs build-and-flash.sh (no --flash) then upload-release.sh.
// Models os-base-build.mjs.

import { promises as fs } from "fs";
import path from "path";
import { randomUUID } from "crypto";
import { spawn } from "child_process";

function runSync(cmd, args, cwd) {
  return new Promise((resolve) => {
    const proc = spawn(cmd, args, { cwd, stdio: ['ignore', 'pipe', 'ignore'] });
    let out = '';
    proc.stdout.on('data', d => out += d);
    proc.on('close', () => resolve(out.trim()));
    proc.on('error', () => resolve(''));
  });
}

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

let progressCallback = null;
let lastProgressBroadcast = 0;

export function onNativeBuildProgress(cb) {
  progressCallback = cb;
}

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

  // Broadcast every log line with the last few lines attached
  if (progressCallback) {
    const now = Date.now();
    // Full snapshot every 2s, lightweight log-only message in between
    if (now - lastProgressBroadcast > 2000) {
      lastProgressBroadcast = now;
      const snap = makeSnapshot(job);
      snap.recentLines = job.logs.slice(-8).map(l => l.line);
      progressCallback(snap);
    } else {
      progressCallback({ id: job.id, line: clean, stage: job.stage, percent: job.percent });
    }
  }

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
    buildName: job.buildName || null,
    commitMsg: job.commitMsg || null,
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
    job.status = "running";
    job.startedAt = nowISO();
    job.percent = 0;

    const repoDir = path.resolve(NATIVE_DIR, "../..");

    // Resolve ref from git HEAD if manual trigger didn't provide one
    if (!job.ref || job.ref === "unknown") {
      const headRef = await runSync("git", ["rev-parse", "HEAD"], repoDir);
      if (headRef) job.ref = headRef;
    }

    const buildName = await runSync("bash", ["scripts/build-name.sh"], NATIVE_DIR) || `oven-${job.ref.slice(0, 7)}`;
    const commitMsg = await runSync("git", ["log", "-1", "--format=%s", job.ref], repoDir) || "";
    job.buildName = buildName;
    job.commitMsg = commitMsg;
    const vmlinuzOut = `/tmp/oven-vmlinuz-${job.id}`;

    // Pre-build: prune stopped containers and dangling images to avoid disk-full failures
    addLogLine(job, "stdout", "Pre-build: Pruning Docker artifacts...");
    try {
      await runPhase(job, "prune", "docker", [
        "system", "prune", "-af", "--filter", "until=1h",
      ], repoDir);
    } catch { addLogLine(job, "stdout", "  Prune skipped (non-fatal)"); }

    // Phase 1: Docker image build (cached layers = fast)
    addLogLine(job, "stdout", "Phase 1: Building Docker image...");
    await runPhase(job, "docker-build", "docker", [
      "build", "-t", "ac-os-builder",
      "-f", path.join(repoDir, "fedac/native/Dockerfile.builder"),
      repoDir,
    ], repoDir);

    job.percent = 30;

    // Phase 2: Docker run → compile binary + initramfs + kernel
    addLogLine(job, "stdout", "Phase 2: Compiling kernel in Docker...");
    const cidFile = `/tmp/oven-cid-${job.id}`;
    await runPhase(job, "build", "bash", ["-c", [
      `CID=$(docker create -e AC_BUILD_NAME=${buildName} ac-os-builder)`,
      `echo $CID > ${cidFile}`,
      `docker start -a $CID`,
    ].join(" && ")], repoDir);

    job.percent = 75;

    // Phase 3: Extract vmlinuz + ISO from container
    addLogLine(job, "stdout", "Phase 3: Extracting kernel + ISO...");
    const cid = (await fs.readFile(cidFile, "utf8")).trim();
    const isoOut = `/tmp/oven-iso-${job.id}`;
    await runPhase(job, "extract", "bash", ["-c",
      `docker cp ${cid}:/tmp/ac-build/vmlinuz ${vmlinuzOut} && docker cp ${cid}:/tmp/ac-build/ac-os.iso ${isoOut} 2>/dev/null; docker rm ${cid} >/dev/null`
    ], repoDir);

    job.percent = 80;

    // Phase 4: Upload vmlinuz + ISO to DO Spaces CDN
    addLogLine(job, "stdout", "Phase 4: Uploading to CDN...");
    const uploadScript = path.join(NATIVE_DIR, "scripts/upload-release.sh");
    const uploadEnv = {
      DO_SPACES_KEY: process.env.DO_SPACES_KEY || process.env.ART_SPACES_KEY || "",
      DO_SPACES_SECRET:
        process.env.DO_SPACES_SECRET || process.env.ART_SPACES_SECRET || "",
    };
    await runPhase(job, "upload", "bash", [uploadScript, vmlinuzOut], NATIVE_DIR, uploadEnv);

    // Upload ISO if it was generated
    try {
      await fs.access(isoOut);
      addLogLine(job, "stdout", "  Uploading ISO...");
      await runPhase(job, "upload-iso", "bash", ["-c",
        `${uploadScript} --iso ${isoOut}`
      ], NATIVE_DIR, uploadEnv);
    } catch { addLogLine(job, "stdout", "  No ISO generated — skipping"); }

    // Cleanup C build
    try { await fs.unlink(vmlinuzOut); } catch {}
    try { await fs.unlink(isoOut); } catch {}
    try { await fs.unlink(cidFile); } catch {}

    job.percent = 85;

    // Phase 5: Build CL variant (non-blocking — failure doesn't fail the job)
    const clChanged = (job.changedPaths || "").includes("fedac/native/cl/");
    if (clChanged) {
      try {
        addLogLine(job, "stdout", "Phase 5: Building Common Lisp variant...");
        const clCidFile = `/tmp/oven-cl-cid-${job.id}`;
        const clVmlinuzOut = `/tmp/oven-cl-vmlinuz-${job.id}`;
        const clBuildName = `cl-${job.ref.slice(0, 7)}`;

        await runPhase(job, "cl-build", "bash", ["-c", [
          `CID=$(docker create -e AC_BUILD_NAME=${clBuildName} -e AC_BUILD_LISP=1 ac-os-builder)`,
          `echo $CID > ${clCidFile}`,
          `docker start -a $CID`,
        ].join(" && ")], repoDir);

        const clCid = (await fs.readFile(clCidFile, "utf8")).trim();
        await runPhase(job, "cl-extract", "bash", ["-c",
          `docker cp ${clCid}:/tmp/ac-build/vmlinuz ${clVmlinuzOut} && docker rm ${clCid} >/dev/null`
        ], repoDir);

        // Upload CL variant to separate OTA channel
        addLogLine(job, "stdout", "Uploading CL variant to CDN...");
        const uploadScript = path.join(NATIVE_DIR, "scripts/upload-release.sh");
        await runPhase(job, "cl-upload", "bash", [uploadScript, clVmlinuzOut], NATIVE_DIR, {
          DO_SPACES_KEY: process.env.DO_SPACES_KEY || process.env.ART_SPACES_KEY || "",
          DO_SPACES_SECRET: process.env.DO_SPACES_SECRET || process.env.ART_SPACES_SECRET || "",
          OTA_CHANNEL: "cl",  // uploads to os/cl-native-notepat-latest.vmlinuz
        });

        try { await fs.unlink(clVmlinuzOut); } catch {}
        try { await fs.unlink(clCidFile); } catch {}
        addLogLine(job, "stdout", "CL variant uploaded successfully");
      } catch (clErr) {
        addLogLine(job, "stderr", `CL build failed (non-fatal): ${clErr.message}`);
      }
    }

    job.status = "success";
    job.stage = "done";
    job.percent = 100;
    job.finishedAt = nowISO();
    if (progressCallback) progressCallback(makeSnapshot(job));
  } catch (err) {
    job.finishedAt = nowISO();
    job.status = job.status === "cancelled" ? "cancelled" : "failed";
    job.stage = job.status;
    job.error = err.message || String(err);
    if (progressCallback) progressCallback(makeSnapshot(job));
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
    changedPaths: options.changed_paths || "",
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
