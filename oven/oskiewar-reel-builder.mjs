// Remote Oskiewar Reel jobs for Oven.
//
// A job checks out an immutable commit from origin/main, runs the latest bot
// fight through the fixed-step Replay Oven, and exposes review artifacts. It
// never holds Instagram credentials and can never publish.

import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import { promises as fs } from "node:fs";
import path from "node:path";

const GIT_REPO_DIR = process.env.NATIVE_GIT_DIR || "/opt/oven/native-git";
const WORK_ROOT = process.env.OSKIEWAR_REEL_WORK_DIR || "/tmp/oven-oskiewar-reels";
const MAX_RECENT = 20;
const MAX_LOGS = 4000;
const jobs = new Map();
const order = [];
let activeJobId = null;

const now = () => new Date().toISOString();
const run = (command, args, options = {}) => new Promise((resolve, reject) => {
  const child = spawn(command, args, { stdio: ["ignore", "pipe", "pipe"], ...options });
  let stdout = "", stderr = "";
  child.stdout.on("data", (chunk) => { stdout += chunk; options.onLine?.("stdout", chunk); });
  child.stderr.on("data", (chunk) => { stderr += chunk; options.onLine?.("stderr", chunk); });
  child.on("error", reject);
  child.on("close", (code) => code === 0 ? resolve(stdout.trim())
    : reject(new Error((stderr || stdout || `${command} exited ${code}`).trim())));
  options.onProcess?.(child);
});

function log(job, stream, chunk) {
  for (const raw of String(chunk).split(/\r?\n/)) {
    const line = raw.trimEnd();
    if (!line) continue;
    job.logs.push({ ts: now(), stream, line });
    if (job.logs.length > MAX_LOGS) job.logs.splice(0, job.logs.length - MAX_LOGS);
    const progress = line.match(/offline replay (\d+)\/(\d+) exact frames/);
    if (progress) {
      job.stage = "offline-render";
      job.percent = 25 + Math.round(Number(progress[1]) / Number(progress[2]) * 60);
    } else if (line.includes("audio tee")) { job.stage = "bot-fight"; job.percent = 15; }
    else if (line.includes("meta spec")) { job.stage = "verify"; job.percent = 92; }
    job.updatedAt = now();
  }
}

function snapshot(job, includeLogs = false) {
  return {
    id: job.id, day: job.day, index: job.index, ref: job.ref,
    resolvedRef: job.resolvedRef, status: job.status, stage: job.stage,
    percent: job.percent, createdAt: job.createdAt, startedAt: job.startedAt,
    updatedAt: job.updatedAt, finishedAt: job.finishedAt, error: job.error,
    reelId: job.reelId, files: job.files, ...(includeLogs ? { logs: job.logs } : {}),
  };
}

async function findResult(queue) {
  const entries = await fs.readdir(queue, { withFileTypes: true });
  for (const entry of entries) {
    if (!entry.isDirectory()) continue;
    const sidecar = path.join(queue, entry.name, "reel.json");
    try {
      const record = JSON.parse(await fs.readFile(sidecar, "utf8"));
      return { record, dir: path.dirname(sidecar) };
    } catch {}
  }
  throw new Error("Reel factory finished without a reel.json artifact");
}

async function execute(job) {
  const root = path.join(WORK_ROOT, job.id);
  const source = path.join(root, "source");
  const queue = path.join(root, "queue");
  try {
    job.status = "running"; job.stage = "checkout"; job.percent = 2;
    job.startedAt = now(); job.updatedAt = job.startedAt;
    await fs.mkdir(root, { recursive: true });
    await run("git", ["fetch", "origin", "main", "--quiet"], { cwd: GIT_REPO_DIR });
    job.resolvedRef = await run("git", ["rev-parse", `${job.ref}^{commit}`], { cwd: GIT_REPO_DIR });
    await run("git", ["worktree", "add", "--detach", source, job.resolvedRef],
      { cwd: GIT_REPO_DIR });
    job.stage = "bot-fight"; job.percent = 8;
    await run(process.execPath, ["xbox/live/marketing/reel.mjs",
      "--day", job.day, "--index", String(job.index), "--slots-per-day", "3",
      "--no-replays", "--out", queue, "--theme", job.theme], {
      cwd: source, env: { ...process.env, TERM: "dumb", FORCE_COLOR: "0" },
      onLine: (stream, chunk) => log(job, stream, chunk),
      onProcess: (process) => { job.process = process; },
    });
    job.process = null;
    const { record, dir } = await findResult(queue);
    if (!record.meta?.ok || !record.sync?.ok || !record.motion?.ok)
      throw new Error("remote artifact failed media, sync, or fixed-step motion gate");
    if (record.sourceCommit !== job.resolvedRef)
      throw new Error(`artifact commit ${record.sourceCommit} does not match job ${job.resolvedRef}`);
    job.reelId = record.id;
    job.files = { reel: path.join(dir, "reel.mp4"), cover: path.join(dir, "cover.jpg"),
      thumbnail: path.join(dir, "thumbnail-10-percent.jpg"), sidecar: path.join(dir, "reel.json") };
    job.status = "success"; job.stage = "done"; job.percent = 100;
  } catch (error) {
    job.status = job.status === "cancelled" ? "cancelled" : "failed";
    job.stage = job.status; job.error = error.message || String(error);
  } finally {
    job.process = null; job.finishedAt = now(); job.updatedAt = job.finishedAt;
    await run("git", ["worktree", "remove", "--force", source], { cwd: GIT_REPO_DIR })
      .catch(() => {});
    if (activeJobId === job.id) activeJobId = null;
  }
}

export function startOskiewarReel(options = {}) {
  const day = String(options.day || new Date().toISOString().slice(0, 10));
  const index = Number(options.index ?? 0);
  const ref = String(options.ref || "origin/main");
  const theme = options.theme === "dark" ? "dark" : "light";
  if (!/^\d{4}-\d{2}-\d{2}$/.test(day)) throw Object.assign(new Error("invalid day"), { code: "BAD_REEL_JOB" });
  if (!Number.isInteger(index) || index < 0 || index > 2)
    throw Object.assign(new Error("index must be 0, 1, or 2"), { code: "BAD_REEL_JOB" });
  if (!/^[A-Za-z0-9_./-]+$/.test(ref)) throw Object.assign(new Error("invalid git ref"), { code: "BAD_REEL_JOB" });
  if (activeJobId) throw Object.assign(new Error(`Oskiewar Reel job already running: ${activeJobId}`),
    { code: "REEL_JOB_BUSY", activeJobId });
  const id = randomUUID().slice(0, 10);
  const job = { id, day, index, ref, theme, resolvedRef: null, status: "queued",
    stage: "queued", percent: 0, createdAt: now(), startedAt: null,
    updatedAt: now(), finishedAt: null, error: null, reelId: null,
    files: null, logs: [], process: null };
  jobs.set(id, job); order.unshift(id); activeJobId = id;
  while (order.length > MAX_RECENT) jobs.delete(order.pop());
  execute(job).catch(() => {});
  return snapshot(job);
}

export const getOskiewarReel = (id, includeLogs = false) =>
  jobs.has(id) ? snapshot(jobs.get(id), includeLogs) : null;
export const getOskiewarReels = () => ({ activeJobId,
  active: activeJobId ? snapshot(jobs.get(activeJobId)) : null,
  recent: order.map((id) => snapshot(jobs.get(id))) });
export function getOskiewarReelFile(id, name) {
  const job = jobs.get(id);
  return job?.status === "success" && job.files?.[name] || null;
}
export function cancelOskiewarReel(id) {
  const job = jobs.get(id);
  if (!job?.process || job.status !== "running") return { ok: false, error: "not running" };
  job.status = "cancelled"; job.process.kill("SIGTERM");
  return { ok: true };
}
