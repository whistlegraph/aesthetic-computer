// os-base-build.mjs — background FedOS base image builds for oven
//
// Runs fedac/scripts/make-kiosk-piece-usb.sh asynchronously, tracks progress,
// and uploads artifacts to Spaces so /os can use fresh base images.

import { promises as fs } from "fs";
import fsSync from "fs";
import path from "path";
import { randomUUID } from "crypto";
import { spawn } from "child_process";
import { S3Client, PutObjectCommand } from "@aws-sdk/client-s3";

const MAX_RECENT_JOBS = 20;
const MAX_LOG_LINES = 1500;
const STEP_COUNT = 6;

const BUILD_SCRIPTS = {
  fedora: process.env.OS_BASE_BUILD_SCRIPT ||
    path.resolve(process.cwd(), "fedac/scripts/make-kiosk-piece-usb.sh"),
  alpine: process.env.OS_ALPINE_BUILD_SCRIPT ||
    path.resolve(process.cwd(), "fedac/scripts/make-alpine-kiosk.sh"),
};
const DEFAULT_BUILD_SCRIPT = BUILD_SCRIPTS.fedora;
const DEFAULT_BUILD_CWD =
  process.env.OS_BASE_BUILD_CWD || path.resolve(process.cwd());
const DEFAULT_WORK_BASE = process.env.OS_BASE_WORK_BASE || "/tmp";
const DEFAULT_IMAGE_SIZE_GB = parsePositiveInt(process.env.OS_BASE_IMAGE_SIZE_GB, 4);
const IMAGE_SIZE_DEFAULTS = { fedora: 4, alpine: 1 };
const KEEP_LOCAL_ARTIFACTS = process.env.OS_BASE_KEEP_ARTIFACTS === "1";

const SPACES_REGION = process.env.OS_SPACES_REGION || "us-east-1";
const SPACES_ENDPOINT =
  process.env.OS_SPACES_ENDPOINT ||
  process.env.ART_SPACES_ENDPOINT ||
  "https://sfo3.digitaloceanspaces.com";
const SPACES_BUCKET = process.env.OS_SPACES_BUCKET || "assets-aesthetic-computer";
const SPACES_CDN_BASE = (
  process.env.OS_SPACES_CDN_BASE ||
  "https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com"
).replace(/\/+$/, "");
const SPACES_PREFIX = (process.env.OS_SPACES_PREFIX || "os").replace(/^\/+|\/+$/g, "");

const jobs = new Map();
const jobOrder = [];
let activeJobId = null;

function parsePositiveInt(value, fallback) {
  const parsed = parseInt(value, 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

function nowISO() {
  return new Date().toISOString();
}

function clampPercent(value) {
  if (!Number.isFinite(value)) return null;
  return Math.max(0, Math.min(100, Math.round(value)));
}

function stripAnsi(value) {
  return String(value || "").replace(/\u001b\[[0-9;]*m/g, "");
}

function formatBytes(bytes) {
  if (!Number.isFinite(bytes) || bytes < 0) return null;
  if (bytes < 1024) return `${bytes}B`;
  const units = ["KB", "MB", "GB", "TB"];
  let size = bytes;
  let idx = -1;
  while (size >= 1024 && idx < units.length - 1) {
    size /= 1024;
    idx += 1;
  }
  return `${size.toFixed(size >= 100 ? 0 : size >= 10 ? 1 : 2)}${units[idx]}`;
}

function makeSnapshot(job, options = {}) {
  const { includeLogs = false, tail = 200 } = options;
  const snapshot = {
    id: job.id,
    flavor: job.flavor,
    status: job.status,
    stage: job.stage,
    message: job.message,
    percent: job.percent,
    createdAt: job.createdAt,
    startedAt: job.startedAt,
    updatedAt: job.updatedAt,
    finishedAt: job.finishedAt,
    elapsedMs: job.startedAt
      ? (job.finishedAt ? Date.parse(job.finishedAt) : Date.now()) - Date.parse(job.startedAt)
      : 0,
    pid: job.pid,
    step: job.step,
    command: job.command,
    output: { ...job.output },
    metrics: { ...job.metrics },
    upload: { ...job.upload },
    exitCode: job.exitCode,
    signal: job.signal,
    error: job.error,
    logCount: job.logs.length,
  };

  if (includeLogs) {
    const start = Math.max(0, job.logs.length - Math.max(0, tail));
    snapshot.logs = job.logs.slice(start);
  }

  return snapshot;
}

function setStage(job, stage, message, percent = null) {
  job.stage = stage;
  if (message) job.message = message;
  if (percent != null) job.percent = clampPercent(percent);
  job.updatedAt = nowISO();
}

function addLogLine(job, stream, line) {
  const clean = stripAnsi(line).replace(/\r/g, "").trimEnd();
  if (!clean) return;
  job.logs.push({ ts: nowISO(), stream, line: clean });
  if (job.logs.length > MAX_LOG_LINES) {
    job.logs.splice(0, job.logs.length - MAX_LOG_LINES);
  }
  job.updatedAt = nowISO();

  const stepMatch = clean.match(/\[(\d+)\/(\d+)\]\s*(.+)$/);
  if (stepMatch) {
    const stepNum = parseInt(stepMatch[1], 10);
    const total = parseInt(stepMatch[2], 10);
    const message = stepMatch[3] || `Step ${stepNum}/${total}`;
    const denom = total > 0 ? total : STEP_COUNT;
    const percent = ((stepNum - 1) / denom) * 100;
    job.step = stepNum;
    setStage(job, `step-${stepNum}`, message, percent);
    return;
  }

  const workDirMatch = clean.match(/^Work dir:\s*(.+)$/);
  if (workDirMatch) {
    job.workDir = workDirMatch[1];
    job.metrics.workDir = job.workDir;
    return;
  }

  if (clean.includes("Extracting rootfs")) {
    setStage(job, "step-2", "Extracting rootfs from base ISO...", 22);
    return;
  }

  if (clean.includes("Fallback extraction")) {
    setStage(job, "step-2", "Fallback rootfs extraction in progress...", 22);
    return;
  }

  if (clean.includes("Building disk image")) {
    setStage(job, "step-5", "Building disk image...", 72);
    return;
  }

  if (clean.includes("Disk image ready")) {
    setStage(job, "step-5", "Disk image built", 88);
    return;
  }

  if (clean.includes("Manifest written")) {
    setStage(job, "step-6", "Manifest generated", 93);
    return;
  }

  if (clean.includes("FedAC Kiosk Build Ready")) {
    setStage(job, "step-6", "Build complete, preparing upload...", 95);
  }
}

function trimJobHistory() {
  while (jobOrder.length > MAX_RECENT_JOBS) {
    const staleId = jobOrder[jobOrder.length - 1];
    if (staleId === activeJobId) break;
    jobs.delete(staleId);
    jobOrder.pop();
  }
}

async function statSize(filePath) {
  try {
    const stat = await fs.stat(filePath);
    return stat.size;
  } catch {
    return null;
  }
}

async function duSizeBytes(targetPath) {
  return new Promise((resolve) => {
    const proc = spawn("du", ["-s", targetPath]);
    let output = "";
    proc.stdout.on("data", (chunk) => {
      output += chunk.toString();
    });
    proc.on("error", () => resolve(null));
    proc.on("close", (code) => {
      if (code !== 0) return resolve(null);
      const parts = output.trim().split(/\s+/);
      const kb = parseInt(parts[0], 10);
      if (!Number.isFinite(kb)) return resolve(null);
      resolve(kb * 1024);
    });
  });
}

function startMetricsSampler(job) {
  const timer = setInterval(async () => {
    if (!job.workDir) return;
    const rootfsPath = path.join(job.workDir, "rootfs");
    const [rootfsBytes, imageBytes] = await Promise.all([
      duSizeBytes(rootfsPath),
      statSize(job.output.imagePath),
    ]);

    if (Number.isFinite(rootfsBytes)) {
      job.metrics.rootfsBytes = rootfsBytes;
      job.metrics.rootfsHuman = formatBytes(rootfsBytes);
      if (job.stage === "step-2" && job.metrics.rootfsHuman) {
        setStage(job, "step-2", `Extracting rootfs... ${job.metrics.rootfsHuman}`, job.percent);
      }
    }

    if (Number.isFinite(imageBytes)) {
      job.metrics.imageBytes = imageBytes;
      job.metrics.imageHuman = formatBytes(imageBytes);
    }

    job.updatedAt = nowISO();
  }, 15000);

  return timer;
}

function getUploadCredentials() {
  const accessKeyId = process.env.OS_SPACES_KEY || process.env.ART_SPACES_KEY;
  const secretAccessKey = process.env.OS_SPACES_SECRET || process.env.ART_SPACES_SECRET;
  if (!accessKeyId || !secretAccessKey) {
    throw new Error(
      "Spaces credentials missing: set OS_SPACES_KEY/OS_SPACES_SECRET or ART_SPACES_KEY/ART_SPACES_SECRET",
    );
  }

  return { accessKeyId, secretAccessKey };
}

function buildObjectKey(prefix, filename) {
  return prefix ? `${prefix}/${filename}` : filename;
}

async function uploadArtifacts(job) {
  const creds = getUploadCredentials();
  const client = new S3Client({
    region: SPACES_REGION,
    endpoint: SPACES_ENDPOINT,
    credentials: creds,
  });

  const imageKey = buildObjectKey(job.upload.prefix, job.upload.imageName);
  const manifestKey = buildObjectKey(job.upload.prefix, job.upload.manifestName);

  const imageStat = await fs.stat(job.output.imagePath);
  setStage(
    job,
    "upload",
    `Uploading ${formatBytes(imageStat.size)} base image to Spaces...`,
    96,
  );

  await client.send(
    new PutObjectCommand({
      Bucket: job.upload.bucket,
      Key: imageKey,
      Body: fsSync.createReadStream(job.output.imagePath),
      ContentType: "application/octet-stream",
      ACL: "public-read",
      CacheControl: "no-cache",
    }),
  );

  setStage(job, "upload", "Uploading base manifest...", 98);
  const manifestBuffer = await fs.readFile(job.output.manifestPath);
  await client.send(
    new PutObjectCommand({
      Bucket: job.upload.bucket,
      Key: manifestKey,
      Body: manifestBuffer,
      ContentType: "application/json",
      ACL: "public-read",
      CacheControl: "no-cache",
    }),
  );

  job.upload.imageKey = imageKey;
  job.upload.manifestKey = manifestKey;
  job.upload.imageUrl = `${job.upload.cdnBase}/${imageKey}`;
  job.upload.manifestUrl = `${job.upload.cdnBase}/${manifestKey}`;
}

async function cleanupLocalArtifacts(job) {
  if (KEEP_LOCAL_ARTIFACTS) return;
  try {
    await fs.unlink(job.output.imagePath);
  } catch {
    // ignore
  }
  try {
    await fs.unlink(job.output.manifestPath);
  } catch {
    // ignore
  }
}

function createJob(options = {}) {
  const id = randomUUID().slice(0, 10);
  const createdAt = nowISO();
  const flavor = options.flavor || "alpine";
  const defaultSize = IMAGE_SIZE_DEFAULTS[flavor] || DEFAULT_IMAGE_SIZE_GB;
  const imageSizeGB = parsePositiveInt(options.imageSizeGB, defaultSize);
  const imageName = options.imageName || `${flavor}-base-latest.img`;
  const manifestName = options.manifestName || `${flavor}-base-manifest.json`;
  const uploadPrefix = (options.uploadPrefix || SPACES_PREFIX).replace(/^\/+|\/+$/g, "");
  const outputBase = path.join(DEFAULT_WORK_BASE, `${flavor}-base-${id}`);

  return {
    id,
    flavor,
    status: "queued",
    stage: "queued",
    message: "Queued",
    percent: 0,
    step: 0,
    createdAt,
    startedAt: null,
    updatedAt: createdAt,
    finishedAt: null,
    pid: null,
    command: null,
    workDir: null,
    output: {
      imagePath: `${outputBase}.img`,
      manifestPath: `${outputBase}-manifest.json`,
      imageSizeGB,
    },
    upload: {
      enabled: options.publish !== false,
      bucket: options.bucket || SPACES_BUCKET,
      endpoint: options.endpoint || SPACES_ENDPOINT,
      cdnBase: (options.cdnBase || SPACES_CDN_BASE).replace(/\/+$/, ""),
      prefix: uploadPrefix,
      imageName,
      manifestName,
      imageKey: null,
      manifestKey: null,
      imageUrl: null,
      manifestUrl: null,
    },
    metrics: {
      workDir: null,
      rootfsBytes: null,
      rootfsHuman: null,
      imageBytes: null,
      imageHuman: null,
    },
    logs: [],
    process: null,
    exitCode: null,
    signal: null,
    error: null,
  };
}

async function runBuildJob(job, options = {}, hooks = {}) {
  const flavor = job.flavor || "alpine";
  const scriptPath = options.scriptPath || BUILD_SCRIPTS[flavor] || DEFAULT_BUILD_SCRIPT;
  const cwd = options.cwd || DEFAULT_BUILD_CWD;

  try {
    await fs.access(scriptPath, fsSync.constants.R_OK);
  } catch {
    throw new Error(`Build script not found: ${scriptPath}`);
  }

  const args = [
    scriptPath,
    "__base__",
    "--base-image",
    "--image",
    job.output.imagePath,
    "--image-size",
    String(job.output.imageSizeGB),
    "--yes",
    "--no-eject",
  ];

  if (options.workBase || DEFAULT_WORK_BASE) {
    args.push("--work-base", options.workBase || DEFAULT_WORK_BASE);
  }

  try {
  // The build script requires root. The oven user has sudoers rules:
  //   oven ALL=(root) NOPASSWD: /usr/bin/bash /opt/oven/fedac/scripts/make-kiosk-piece-usb.sh *
  //   oven ALL=(root) NOPASSWD: /usr/bin/bash /opt/oven/fedac/scripts/make-alpine-kiosk.sh *
  // So we spawn via sudo when not already root.
  const needsSudo = process.getuid?.() !== 0;
  const spawnCmd = needsSudo ? "sudo" : "bash";
  const spawnArgs = needsSudo ? ["/usr/bin/bash", ...args] : args;

  job.command = `${needsSudo ? "sudo " : ""}bash ${args.join(" ")}`;
  job.status = "running";
  job.startedAt = nowISO();
  setStage(job, "starting", "Starting base image build...", 1);

  const proc = spawn(spawnCmd, spawnArgs, {
    cwd,
    env: {
      ...process.env,
      TERM: "dumb",
      CLICOLOR: "0",
      FORCE_COLOR: "0",
    },
  });

  job.process = proc;
  job.pid = proc.pid || null;
  hooks.onStart?.(makeSnapshot(job));
  setStage(job, "step-1", "Running build steps...", 5);

  const metricsTimer = startMetricsSampler(job);

  const wireStream = (stream, streamName) => {
    let pending = "";
    stream.on("data", (chunk) => {
      pending += chunk.toString();
      let idx = pending.indexOf("\n");
      while (idx >= 0) {
        const line = pending.slice(0, idx);
        pending = pending.slice(idx + 1);
        addLogLine(job, streamName, line);
        idx = pending.indexOf("\n");
      }
    });
    stream.on("end", () => {
      if (pending.length > 0) {
        addLogLine(job, streamName, pending);
      }
    });
  };

  wireStream(proc.stdout, "stdout");
  wireStream(proc.stderr, "stderr");

  await new Promise((resolve, reject) => {
    proc.on("error", reject);
    proc.on("close", (code, signal) => {
      job.exitCode = code;
      job.signal = signal;
      resolve();
    });
  });

  clearInterval(metricsTimer);
  job.process = null;
  if (job.exitCode !== 0) {
    throw new Error(`Build failed (exit ${job.exitCode}${job.signal ? `, signal ${job.signal}` : ""})`);
  }

  setStage(job, "verify", "Verifying build artifacts...", 94);
  await fs.access(job.output.imagePath, fsSync.constants.R_OK);
  await fs.access(job.output.manifestPath, fsSync.constants.R_OK);
  const imageSize = await statSize(job.output.imagePath);
  if (Number.isFinite(imageSize)) {
    job.metrics.imageBytes = imageSize;
    job.metrics.imageHuman = formatBytes(imageSize);
  }

  if (job.upload.enabled) {
    await uploadArtifacts(job);
    hooks.onUploadComplete?.(makeSnapshot(job));
  } else {
    setStage(job, "done", "Build complete (upload disabled)", 100);
  }

  job.status = "success";
  job.finishedAt = nowISO();
  setStage(job, "done", "Base image build complete", 100);
  hooks.onSuccess?.(makeSnapshot(job));

  await cleanupLocalArtifacts(job);
} catch (error) {
  job.finishedAt = nowISO();
  if (job.status === "cancelled") {
    job.error = "Cancelled";
    setStage(job, "cancelled", "Build cancelled", job.percent);
    hooks.onError?.(makeSnapshot(job), error);
    return;
  }
  job.status = "failed";
  job.error = error.message || String(error);
  setStage(job, "failed", job.error, job.percent);
  hooks.onError?.(makeSnapshot(job), error);
}
}

export async function startOSBaseBuild(options = {}, hooks = {}) {
  if (activeJobId) {
    const error = new Error(`OS base build already running: ${activeJobId}`);
    error.code = "OS_BASE_BUSY";
    error.activeJobId = activeJobId;
    throw error;
  }

  const job = createJob(options);
  jobs.set(job.id, job);
  jobOrder.unshift(job.id);
  trimJobHistory();
  activeJobId = job.id;

  runBuildJob(job, options, hooks)
    .catch((err) => {
      if (!job.error) job.error = err.message || String(err);
      if (job.status !== "cancelled") {
        job.status = "failed";
      }
      job.finishedAt = nowISO();
      setStage(job, job.status === "cancelled" ? "cancelled" : "failed", job.error, job.percent);
    })
    .finally(() => {
      if (activeJobId === job.id) activeJobId = null;
      hooks.onSettled?.(makeSnapshot(job));
    });

  return makeSnapshot(job);
}

export function getOSBaseBuild(jobId, options = {}) {
  const job = jobs.get(jobId);
  if (!job) return null;
  return makeSnapshot(job, options);
}

export function getOSBaseBuildsSummary() {
  return {
    activeJobId,
    active: activeJobId ? makeSnapshot(jobs.get(activeJobId)) : null,
    recent: jobOrder.map((id) => jobs.get(id)).filter(Boolean).map((job) => makeSnapshot(job)),
  };
}

export function cancelOSBaseBuild(jobId) {
  const job = jobs.get(jobId);
  if (!job) return { ok: false, error: "job not found" };
  if (job.status !== "running" || !job.process) {
    return { ok: false, error: "job is not running" };
  }

  try {
    job.process.kill("SIGTERM");
    setStage(job, "cancelled", "Cancellation requested", job.percent);
    job.status = "cancelled";
    return { ok: true };
  } catch (error) {
    return { ok: false, error: error.message || String(error) };
  }
}
