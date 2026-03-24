// native-builder.mjs — FedAC Native kernel OTA builds for oven
//
// Triggered via POST /native-build after commits to fedac/native/ on main.
// Runs build-and-flash.sh (no --flash) then upload-release.sh.
// Models os-base-build.mjs.

import { promises as fs } from "fs";
import path from "path";
import { randomUUID } from "crypto";
import { spawn } from "child_process";
import { MongoClient } from "mongodb";

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
const NATIVE_BUILD_COLLECTION =
  process.env.NATIVE_BUILD_COLLECTION || "oven-native-builds";

// fedac/native/ lives in the native-git repo on oven (polled by native-git-poller).
const NATIVE_DIR =
  process.env.NATIVE_DIR || "/opt/oven/native-git/fedac/native";
const NATIVE_BRANCH = process.env.NATIVE_GIT_BRANCH || "main";

// Kernel build cache: symlinked from fedac/native/build so kernel object
// files survive rsync --delete between commits (5-10x faster warm builds).
const CACHE_DIR =
  process.env.NATIVE_CACHE_DIR || "/opt/oven/native-cache";

const jobs = new Map();
const jobOrder = [];
let activeJobId = null;

let progressCallback = null;
let lastProgressBroadcast = 0;
let nativeBuildMongoClient = null;
let nativeBuildMongoDb = null;

export function onNativeBuildProgress(cb) {
  progressCallback = cb;
}

function nowISO() {
  return new Date().toISOString();
}

function toDateOrNull(v) {
  if (!v) return null;
  const d = new Date(v);
  if (Number.isNaN(d.getTime())) return null;
  return d;
}

async function getNativeBuildMongo() {
  if (nativeBuildMongoDb) return nativeBuildMongoDb;
  const uri = process.env.MONGODB_CONNECTION_STRING;
  const dbName = process.env.MONGODB_NAME;
  if (!uri || !dbName) return null;
  try {
    nativeBuildMongoClient = await MongoClient.connect(uri);
    nativeBuildMongoDb = nativeBuildMongoClient.db(dbName);
    await nativeBuildMongoDb
      .collection(NATIVE_BUILD_COLLECTION)
      .createIndex({ when: -1 });
    await nativeBuildMongoDb
      .collection(NATIVE_BUILD_COLLECTION)
      .createIndex({ buildName: 1, when: -1 });
    return nativeBuildMongoDb;
  } catch (err) {
    console.error("[native-builder] MongoDB connect failed:", err.message);
    return null;
  }
}

async function persistNativeBuildRecord(job) {
  try {
    const db = await getNativeBuildMongo();
    if (!db) return;
    const startedAt = toDateOrNull(job.startedAt);
    const finishedAt = toDateOrNull(job.finishedAt);
    const durationMs =
      startedAt && finishedAt ? Math.max(0, finishedAt - startedAt) : null;
    const record = {
      jobId: job.id,
      buildName: job.buildName || null,
      ref: job.ref || null,
      gitHash: job.ref && job.ref !== "unknown" ? String(job.ref).slice(0, 40) : null,
      status: job.status || "unknown",
      stage: job.stage || null,
      percent: Number.isFinite(job.percent) ? job.percent : null,
      error: job.error || null,
      exitCode: Number.isFinite(job.exitCode) ? job.exitCode : null,
      commitMsg: job.commitMsg || null,
      flags: Array.isArray(job.flags) ? job.flags : [],
      changedPaths: job.changedPaths || "",
      createdAt: toDateOrNull(job.createdAt),
      startedAt,
      updatedAt: toDateOrNull(job.updatedAt),
      finishedAt,
      durationMs,
      logCount: Array.isArray(job.logs) ? job.logs.length : 0,
      logTail: Array.isArray(job.logs)
        ? job.logs.slice(-120).map((l) => l.line)
        : [],
      source: "oven-native-builder",
      when: new Date(),
    };
    await db.collection(NATIVE_BUILD_COLLECTION).insertOne(record);
  } catch (err) {
    console.error("[native-builder] Failed to persist build record:", err.message);
  }
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
      job.exitCode = code;
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

    // Preflight: hard-sync build repo and refuse conflicted/dirty native trees.
    addLogLine(job, "stdout", "Preflight: syncing native git checkout...");
    await runPhase(job, "preflight-sync", "bash", ["-lc", [
      "set -euo pipefail",
      `git fetch origin ${NATIVE_BRANCH} --quiet || true`,
      `git checkout -f ${NATIVE_BRANCH} --quiet || true`,
      `if git rev-parse --verify origin/${NATIVE_BRANCH} >/dev/null 2>&1; then`,
      `  git reset --hard origin/${NATIVE_BRANCH} --quiet`,
      "fi",
      "git clean -fdq -- fedac/native",
    ].join("\n")], repoDir);

    const syncedRef = await runSync("git", ["rev-parse", "HEAD"], repoDir);
    if (syncedRef) job.ref = syncedRef;

    const trackedDirty = await runSync(
      "git",
      ["status", "--porcelain", "--untracked-files=no", "--", "fedac/native"],
      repoDir,
    );
    if (trackedDirty) {
      throw new Error(
        `Refusing native build: fedac/native tree is dirty after sync:\n${trackedDirty}`,
      );
    }

    const unresolved = await runSync(
      "git",
      ["diff", "--name-only", "--diff-filter=U", "--", "fedac/native"],
      repoDir,
    );
    if (unresolved) {
      throw new Error(
        `Refusing native build: unresolved merge conflict(s): ${unresolved}`,
      );
    }

    const conflictMarkers = await runSync(
      "bash",
      [
        "-lc",
        "grep -nE '^(<<<<<<<|=======|>>>>>>>)|Updated upstream|Stashed changes' fedac/native/initramfs/init 2>/dev/null || true",
      ],
      repoDir,
    );
    if (conflictMarkers) {
      throw new Error(
        `Refusing native build: conflict markers detected in initramfs/init:\n${conflictMarkers}`,
      );
    }

    // Parse-check init script explicitly so syntax issues fail before kernel compile/upload.
    await runPhase(
      job,
      "preflight-init",
      "bash",
      ["-lc", "set -euo pipefail\nsh -n fedac/native/initramfs/init"],
      repoDir,
    );

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
      await runPhase(job, "prune", "bash", ["-c",
        "docker container prune -f && docker builder prune -af --filter until=30m",
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

    // Determine variant: "c" (default), "cl", or "both"
    const variant = job.variant || "c";
    const buildC = variant === "c" || variant === "both";
    const buildCL = variant === "cl" || variant === "both";
    const uploadScript = path.join(NATIVE_DIR, "scripts/upload-release.sh");
    const uploadEnv = {
      DO_SPACES_KEY: process.env.DO_SPACES_KEY || process.env.ART_SPACES_KEY || "",
      DO_SPACES_SECRET: process.env.DO_SPACES_SECRET || process.env.ART_SPACES_SECRET || "",
      ALLOW_DIRTY_UPLOAD: "1",
      AC_BUILD_NAME: buildName,
    };

    // ── C variant ──
    if (buildC) {
      addLogLine(job, "stdout", "Phase 2: Compiling C kernel in Docker...");
      const cidFile = `/tmp/oven-cid-${job.id}`;
      await runPhase(job, "build", "bash", ["-c", [
        `CID=$(docker create -e AC_BUILD_NAME=${buildName} ac-os-builder)`,
        `echo $CID > ${cidFile}`,
        `docker start -a $CID`,
      ].join(" && ")], repoDir);

      job.percent = 75;

      addLogLine(job, "stdout", "Phase 3: Extracting C kernel + ISO...");
      const cid = (await fs.readFile(cidFile, "utf8")).trim();
      const isoOut = `/tmp/oven-iso-${job.id}`;
      await runPhase(job, "extract", "bash", ["-c",
        `docker cp ${cid}:/tmp/ac-build/vmlinuz ${vmlinuzOut} && docker cp ${cid}:/tmp/ac-build/ac-os.iso ${isoOut} 2>/dev/null; docker rm ${cid} >/dev/null`
      ], repoDir);

      job.percent = 80;

      addLogLine(job, "stdout", "Phase 4: Uploading C variant to CDN...");
      const uploadDir = `/tmp/oven-upload-${job.id}`;
      const vmlinuzUpload = `${uploadDir}/vmlinuz`;
      const isoUpload = `${uploadDir}/ac-os.iso`;
      await fs.mkdir(uploadDir, { recursive: true });
      await fs.rename(vmlinuzOut, vmlinuzUpload);
      try { await fs.rename(isoOut, isoUpload); } catch {}
      await runPhase(job, "upload", "bash", [uploadScript, vmlinuzUpload], NATIVE_DIR, uploadEnv);

      try { await fs.rm(uploadDir, { recursive: true }); } catch {}
      try { await fs.unlink(cidFile); } catch {}
      addLogLine(job, "stdout", "C variant uploaded successfully");
    }

    job.percent = buildCL ? 50 : 90;

    // ── CL variant ──
    if (buildCL) {
      addLogLine(job, "stdout", `Phase ${buildC ? 5 : 2}: Compiling CL kernel in Docker...`);
      const clCidFile = `/tmp/oven-cl-cid-${job.id}`;
      const clVmlinuzOut = `/tmp/oven-cl-vmlinuz-${job.id}`;

      await runPhase(job, "cl-build", "bash", ["-c", [
        `CID=$(docker create -e AC_BUILD_NAME=${buildName} -e AC_BUILD_VARIANT=cl -e AC_BUILD_LISP=1 ac-os-builder)`,
        `echo $CID > ${clCidFile}`,
        `docker start -a $CID`,
      ].join(" && ")], repoDir);

      job.percent = buildC ? 85 : 75;

      addLogLine(job, "stdout", "Extracting CL kernel...");
      const clCid = (await fs.readFile(clCidFile, "utf8")).trim();
      const clIsoOut = `/tmp/oven-cl-iso-${job.id}`;
      await runPhase(job, "cl-extract", "bash", ["-c",
        `docker cp ${clCid}:/tmp/ac-build/vmlinuz ${clVmlinuzOut} && docker cp ${clCid}:/tmp/ac-build/ac-os.iso ${clIsoOut} 2>/dev/null; docker rm ${clCid} >/dev/null`
      ], repoDir);

      job.percent = buildC ? 90 : 80;

      addLogLine(job, "stdout", "Uploading CL variant to CDN...");
      const clUploadDir = `/tmp/oven-cl-upload-${job.id}`;
      const clVmlinuzUpload = `${clUploadDir}/vmlinuz`;
      const clIsoUpload = `${clUploadDir}/ac-os.iso`;
      await fs.mkdir(clUploadDir, { recursive: true });
      await fs.rename(clVmlinuzOut, clVmlinuzUpload);
      try { await fs.rename(clIsoOut, clIsoUpload); } catch {}
      await runPhase(job, "cl-upload", "bash", [uploadScript, clVmlinuzUpload], NATIVE_DIR, {
        ...uploadEnv,
        OTA_CHANNEL: "cl",
      });

      try { await fs.rm(clUploadDir, { recursive: true }); } catch {}
      try { await fs.unlink(clCidFile); } catch {}
      addLogLine(job, "stdout", "CL variant uploaded successfully");
    }

    job.status = "success";
    job.stage = "done";
    job.percent = 100;
    job.error = null;
    job.finishedAt = nowISO();
    if (progressCallback) progressCallback(makeSnapshot(job));
  } catch (err) {
    job.finishedAt = nowISO();
    job.status = job.status === "cancelled" ? "cancelled" : "failed";
    job.stage = job.status;
    job.error = err.message || String(err);
    if (progressCallback) progressCallback(makeSnapshot(job));
  } finally {
    await persistNativeBuildRecord(job);
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
    variant: options.variant || "c",
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
