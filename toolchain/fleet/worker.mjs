#!/usr/bin/env node
// AC fleet worker — a deliberately narrow, tailnet-only compute endpoint.
//
// v1 accepts one typed workload: ffmpeg. It never accepts a shell command.
// Inputs are content-addressed uploads, arguments may reference only named
// inputs and one declared output, and every job runs in its own process group.

import { createHash, randomBytes, timingSafeEqual } from "node:crypto";
import { createReadStream, createWriteStream, existsSync, mkdirSync, readFileSync,
  readdirSync, renameSync, rmSync, statfsSync, statSync, writeFileSync } from "node:fs";
import { createServer } from "node:http";
import { homedir, hostname, loadavg, freemem, totalmem, cpus } from "node:os";
import { basename, dirname, join } from "node:path";
import { spawn, spawnSync } from "node:child_process";

const HOME = homedir();
const PORT = positiveInt(process.env.AC_FLEET_WORKER_PORT, 5263);
const BIND = process.env.AC_FLEET_WORKER_BIND || "127.0.0.1";
const ROLE = process.env.AC_FLEET_WORKER_ROLE || "balanced";
const HOST_NAME = process.env.AC_FLEET_WORKER_NAME || hostname().split(".")[0];
const ROOT = process.env.AC_FLEET_WORKER_STATE || join(HOME, ".local", "share", "ac-fleet-worker");
const TOKEN_FILE = process.env.AC_FLEET_WORKER_TOKEN_FILE || join(HOME, ".config", "ac-fleet-worker", "token");
const GUARD_LATEST = process.env.AC_FLEET_GUARD_LATEST || join(HOME, ".local", "share", "slab", "performance", "latest.txt");
const GUARD_PRESSURE = process.env.AC_FLEET_GUARD_PRESSURE || join(HOME, ".local", "share", "slab", "performance", "pressure-active");
const MISSION_FILE = process.env.AC_FLEET_MISSION_FILE || join(HOME, ".local", "share", "desktop-badge", "mission.json");
const MAX_BODY = positiveInt(process.env.AC_FLEET_WORKER_MAX_BODY, 1_048_576);
const MAX_INPUT = positiveInt(process.env.AC_FLEET_WORKER_MAX_INPUT, 50 * 1024 * 1024 * 1024);
const MAX_JOBS = positiveInt(process.env.AC_FLEET_WORKER_CONCURRENCY, 1);
const FFMPEG = process.env.AC_FLEET_WORKER_FFMPEG || "/opt/homebrew/bin/ffmpeg";
const INPUT_DIR = join(ROOT, "inputs");
const JOB_DIR = join(ROOT, "jobs");
const LOG_DIR = join(ROOT, "logs");
const JOB_ID = /^[a-z0-9][a-z0-9-]{5,80}$/;
const SHA256 = /^[a-f0-9]{64}$/;
const SAFE_NAME = /^[A-Za-z0-9][A-Za-z0-9._-]{0,119}$/;

for (const dir of [ROOT, INPUT_DIR, JOB_DIR, LOG_DIR]) mkdirSync(dir, { recursive: true });
if (!existsSync(TOKEN_FILE)) {
  console.error(`fleet worker token is missing: ${TOKEN_FILE}`);
  process.exit(78);
}
const TOKEN = readFileSync(TOKEN_FILE, "utf8").trim();
if (TOKEN.length < 32) {
  console.error("fleet worker token must contain at least 32 characters");
  process.exit(78);
}
if (!existsSync(FFMPEG)) {
  console.error(`ffmpeg is missing: ${FFMPEG}`);
  process.exit(69);
}

const jobs = new Map();
const queue = [];
const active = new Map();
loadJobs();

function positiveInt(value, fallback) {
  const n = Number(value);
  return Number.isFinite(n) && n > 0 ? Math.floor(n) : fallback;
}

function now() { return new Date().toISOString(); }
function inputPath(sha) { return join(INPUT_DIR, sha); }
function jobPath(id) { return join(JOB_DIR, `${id}.json`); }
function jobWorkDir(id) { return join(JOB_DIR, id); }

function publicJob(job) {
  return {
    id: job.id, type: job.type, host: HOST_NAME, status: job.status,
    stage: job.stage, progress: job.progress, createdAt: job.createdAt,
    startedAt: job.startedAt || null, updatedAt: job.updatedAt,
    finishedAt: job.finishedAt || null, exitCode: job.exitCode ?? null,
    error: job.error || null, output: job.output,
    outputBytes: job.outputBytes || null, outputSha256: job.outputSha256 || null,
    origin: job.origin || null,
  };
}

function persist(job) {
  job.updatedAt = now();
  writeFileSync(jobPath(job.id), JSON.stringify(job, null, 2) + "\n", { mode: 0o600 });
}

function loadJobs() {
  for (const file of readdirSync(JOB_DIR)) {
    if (!file.endsWith(".json")) continue;
    try {
      const job = JSON.parse(readFileSync(join(JOB_DIR, file), "utf8"));
      if (!JOB_ID.test(job.id)) continue;
      if (["queued", "running"].includes(job.status)) {
        job.status = "failed";
        job.stage = "interrupted";
        job.error = "worker restarted before the job completed";
        job.finishedAt = now();
        persist(job);
      }
      jobs.set(job.id, job);
    } catch {}
  }
}

function parseGuard() {
  const values = {};
  try {
    for (const line of readFileSync(GUARD_LATEST, "utf8").split("\n")) {
      for (const match of line.matchAll(/(?:^|\s)([A-Za-z0-9_]+)=([^\s;]+)/g)) {
        values[match[1]] = match[2];
      }
    }
  } catch {}
  return values;
}

function capacity() {
  const guard = parseGuard();
  const cores = cpus().length || 1;
  const load1 = Number(guard.load1 ?? loadavg()[0]) || 0;
  const freePct = Number(guard.free_pct ?? macMemoryFreePct() ?? (freemem() / Math.max(1, totalmem()) * 100)) || 0;
  const cpuPct = Math.max(0, Math.min(100, load1 / cores * 100));
  const pressure = existsSync(GUARD_PRESSURE) || guard.pressure === "1";
  let diskUsedPct = 0;
  try {
    const disk = statfsSync(ROOT);
    const blocks = Number(disk.blocks) || 0;
    const available = Number(disk.bavail) || 0;
    diskUsedPct = blocks > 0 ? Math.max(0, Math.min(100, (blocks - available) / blocks * 100)) : 0;
  } catch {}
  const roleThreshold = ROLE === "interactive"
    ? { free: 32, load: 0.72 }
    : ROLE === "light" ? { free: 24, load: 0.95 }
    : { free: 18, load: 1.20 };
  const headroom = !pressure && freePct >= roleThreshold.free && load1 <= cores * roleThreshold.load;
  return {
    cpuPct, load1, cores, memoryUsedPct: Math.max(0, Math.min(100, 100 - freePct)),
    memoryFreePct: freePct, memoryTotalGB: totalmem() / 1_073_741_824,
    diskUsedPct,
    pressure, pressureReason: guard.reason || (pressure ? "guard" : "none"),
    headroom, accepting: headroom && active.size < MAX_JOBS,
  };
}

function macMemoryFreePct() {
  if (process.platform !== "darwin") return null;
  const result = spawnSync("/usr/bin/memory_pressure", [], { encoding: "utf8", timeout: 1500 });
  const match = result.stdout?.match(/System-wide memory free percentage:\s*([0-9.]+)%/);
  return match ? Number(match[1]) : null;
}

function health() {
  const cap = capacity();
  return {
    ok: true, service: "ac-fleet-worker", version: 1,
    host: HOST_NAME, role: ROLE, bind: BIND,
    capabilities: ["ffmpeg-render"], concurrency: MAX_JOBS,
    active: [...active.keys()], queued: queue.length,
    ...cap, checkedAt: now(),
  };
}

function authorized(req) {
  const value = req.headers.authorization || "";
  const supplied = value.startsWith("Bearer ") ? value.slice(7) : "";
  if (supplied.length !== TOKEN.length) return false;
  const a = Buffer.from(supplied), b = Buffer.from(TOKEN);
  return a.length === b.length && timingSafeEqual(a, b);
}

function json(res, status, value) {
  const body = JSON.stringify(value) + "\n";
  res.writeHead(status, { "content-type": "application/json", "content-length": Buffer.byteLength(body), "cache-control": "no-store" });
  res.end(body);
}

async function readJson(req) {
  const chunks = [];
  let size = 0;
  for await (const chunk of req) {
    size += chunk.length;
    if (size > MAX_BODY) throw new Error("request body is too large");
    chunks.push(chunk);
  }
  try { return JSON.parse(Buffer.concat(chunks).toString("utf8")); }
  catch { throw new Error("request body is not valid JSON"); }
}

function cleanName(value, label) {
  const name = String(value || "");
  if (!SAFE_NAME.test(name) || basename(name) !== name) throw new Error(`${label} has an unsafe name`);
  return name;
}

function validateSpec(spec) {
  if (!spec || spec.type !== "ffmpeg") throw new Error("only type=ffmpeg is supported");
  if (!Array.isArray(spec.inputs) || spec.inputs.length < 1 || spec.inputs.length > 16) throw new Error("inputs must contain 1-16 items");
  const names = new Set();
  const inputs = spec.inputs.map((item) => {
    const name = cleanName(item?.name, "input");
    const sha256 = String(item?.sha256 || "").toLowerCase();
    if (names.has(name)) throw new Error(`duplicate input name: ${name}`);
    if (!SHA256.test(sha256)) throw new Error(`invalid sha256 for ${name}`);
    if (!existsSync(inputPath(sha256))) throw new Error(`input is not uploaded: ${name}`);
    names.add(name);
    return { name, sha256 };
  });
  const output = cleanName(spec.output, "output");
  if (!Array.isArray(spec.args) || spec.args.length < 2 || spec.args.length > 160) throw new Error("args must contain 2-160 tokens");
  let sawInput = false, sawOutput = false;
  const args = spec.args.map((raw) => {
    const token = String(raw);
    if (!token || token.length > 4096 || /[\0\r\n]/.test(token)) throw new Error("ffmpeg argument contains invalid characters");
    if (token === "@output") { sawOutput = true; return token; }
    if (token.startsWith("@input:")) {
      const name = token.slice(7);
      if (!names.has(name)) throw new Error(`argument references an unknown input: ${name}`);
      sawInput = true;
      return token;
    }
    if (token.includes("..") || token.startsWith("/") || /(?:^|[=,])(https?|tcp|udp|file|concat):/i.test(token)) {
      throw new Error(`ffmpeg argument is outside the typed sandbox: ${token.slice(0, 120)}`);
    }
    if (["-filter_script", "-filter_complex_script", "-attach", "-dump_attachment"].includes(token)) {
      throw new Error(`ffmpeg option is not allowed: ${token}`);
    }
    return token;
  });
  if (!sawInput) throw new Error("args must reference at least one @input:name");
  if (!sawOutput || args.filter((x) => x === "@output").length !== 1) throw new Error("args must reference @output exactly once");
  const expectedDurationSeconds = Number(spec.expectedDurationSeconds);
  return {
    type: "ffmpeg", inputs, output, args,
    expectedDurationSeconds: Number.isFinite(expectedDurationSeconds) && expectedDurationSeconds > 0 ? expectedDurationSeconds : null,
    origin: spec.origin && typeof spec.origin === "object" ? {
      host: String(spec.origin.host || "").slice(0, 80),
      handle: String(spec.origin.handle || "").slice(0, 160),
    } : null,
  };
}

function createJob(spec) {
  const id = `ffmpeg-${Date.now().toString(36)}-${randomBytes(4).toString("hex")}`;
  const job = { id, ...spec, status: "queued", stage: "queued", progress: 0, createdAt: now(), updatedAt: now() };
  mkdirSync(jobWorkDir(id), { recursive: true });
  persist(job);
  jobs.set(id, job);
  queue.push(id);
  pump();
  return job;
}

function resolvedArgs(job) {
  const work = jobWorkDir(job.id);
  const paths = new Map(job.inputs.map((item) => [item.name, inputPath(item.sha256)]));
  return job.args.map((token) => token === "@output"
    ? join(work, job.output)
    : token.startsWith("@input:") ? paths.get(token.slice(7)) : token);
}

function writeMission(job, detail) {
  let current = null;
  try { current = JSON.parse(readFileSync(MISSION_FILE, "utf8")); } catch {}
  if (current?.agent && current.agent !== "fleet-worker") return;
  mkdirSync(dirname(MISSION_FILE), { recursive: true });
  const status = job.status === "success" ? "done" : job.status === "failed" ? "pending" : "in_progress";
  const percent = Number.isFinite(job.progress) ? ` · ${Math.round(job.progress * 100)}%` : "";
  const doc = {
    mission: `fleet ${job.type} on ${HOST_NAME}`,
    emoji: "🧱", agent: "fleet-worker", updatedAt: now(),
    bounded: !!job.expectedDurationSeconds, progress: job.progress,
    items: [{ text: `${detail}${percent}`, status }],
  };
  writeFileSync(MISSION_FILE, JSON.stringify(doc, null, 2) + "\n", { mode: 0o600 });
}

function clearMissionIfOwned() {
  setTimeout(() => {
    try {
      const current = JSON.parse(readFileSync(MISSION_FILE, "utf8"));
      if (current?.agent === "fleet-worker") rmSync(MISSION_FILE, { force: true });
    } catch {}
  }, 30_000).unref();
}

function pump() {
  while (active.size < MAX_JOBS && queue.length) {
    if (!capacity().headroom) return;
    const id = queue.shift();
    const job = jobs.get(id);
    if (job?.status === "queued") runJob(job);
  }
}

function runJob(job) {
  job.status = "running";
  job.stage = "ffmpeg";
  job.startedAt = now();
  persist(job);
  writeMission(job, job.output);
  const logPath = join(LOG_DIR, `${job.id}.log`);
  const log = createWriteStream(logPath, { flags: "a", mode: 0o600 });
  const ffmpegArgs = ["-nostdin", "-y", "-hide_banner", "-progress", "pipe:1", "-nostats", ...resolvedArgs(job)];
  const launcher = existsSync("/usr/sbin/taskpolicy") ? "/usr/sbin/taskpolicy" : FFMPEG;
  const args = launcher === FFMPEG ? ffmpegArgs : ["-c", "utility", "/usr/bin/nice", "-n", "10", FFMPEG, ...ffmpegArgs];
  const proc = spawn(launcher, args, {
    cwd: jobWorkDir(job.id), detached: true, stdio: ["ignore", "pipe", "pipe"],
    env: { ...process.env, PATH: "/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin" },
  });
  active.set(job.id, proc);
  let progressBuffer = "";
  proc.stdout.on("data", (chunk) => {
    log.write(chunk);
    progressBuffer += chunk.toString("utf8");
    const lines = progressBuffer.split("\n");
    progressBuffer = lines.pop() || "";
    for (const line of lines) {
      const [key, raw] = line.trim().split("=", 2);
      if ((key === "out_time_us" || key === "out_time_ms") && job.expectedDurationSeconds) {
        const seconds = Number(raw) / 1_000_000;
        if (Number.isFinite(seconds)) job.progress = Math.max(job.progress, Math.min(0.99, seconds / job.expectedDurationSeconds));
      }
      if (key === "progress") {
        if (raw === "end") job.progress = 1;
        persist(job);
        writeMission(job, job.output);
      }
    }
  });
  proc.stderr.on("data", (chunk) => log.write(chunk));
  proc.on("error", (error) => { void finishJob(job, null, error); });
  proc.on("close", (code) => { void finishJob(job, code, null); });
}

async function finishJob(job, code, spawnError) {
  if (!active.has(job.id)) return;
  active.delete(job.id);
  job.exitCode = Number.isInteger(code) ? code : null;
  job.finishedAt = now();
  const output = join(jobWorkDir(job.id), job.output);
  if (!spawnError && code === 0 && existsSync(output) && statSync(output).isFile()) {
    job.status = "success";
    job.stage = "done";
    job.progress = 1;
    job.outputBytes = statSync(output).size;
    job.outputSha256 = await shaFile(output);
  } else {
    job.status = job.status === "cancelled" ? "cancelled" : "failed";
    job.stage = job.status;
    job.error = spawnError?.message || `ffmpeg exited with code ${code}`;
  }
  persist(job);
  writeMission(job, job.output);
  clearMissionIfOwned();
  pump();
}

function shaFile(path) {
  return new Promise((resolveHash, rejectHash) => {
    const hash = createHash("sha256");
    const stream = createReadStream(path);
    stream.on("data", (chunk) => hash.update(chunk));
    stream.on("error", rejectHash);
    stream.on("end", () => resolveHash(hash.digest("hex")));
  });
}

function cancelJob(job) {
  if (job.status === "queued") {
    const index = queue.indexOf(job.id);
    if (index >= 0) queue.splice(index, 1);
    job.status = "cancelled"; job.stage = "cancelled"; job.finishedAt = now(); persist(job);
    return true;
  }
  const proc = active.get(job.id);
  if (!proc) return false;
  job.status = "cancelled";
  try { process.kill(-proc.pid, "SIGTERM"); } catch { try { proc.kill("SIGTERM"); } catch {} }
  setTimeout(() => { try { process.kill(-proc.pid, "SIGKILL"); } catch {} }, 5000).unref();
  return true;
}

async function receiveInput(req, res, sha) {
  if (!SHA256.test(sha)) return json(res, 400, { error: "invalid sha256" });
  const length = Number(req.headers["content-length"] || 0);
  if (!Number.isFinite(length) || length < 1 || length > MAX_INPUT) return json(res, 413, { error: "invalid input size" });
  const final = inputPath(sha);
  if (existsSync(final) && statSync(final).size === length) return json(res, 200, { ok: true, cached: true, sha256: sha, bytes: length });
  const temp = `${final}.${process.pid}.part`;
  const hash = createHash("sha256");
  const out = createWriteStream(temp, { mode: 0o600 });
  let bytes = 0;
  try {
    for await (const chunk of req) {
      bytes += chunk.length;
      if (bytes > MAX_INPUT || bytes > length) throw new Error("input exceeded declared size");
      hash.update(chunk);
      if (!out.write(chunk)) await new Promise((resolveDrain) => out.once("drain", resolveDrain));
    }
    await new Promise((resolveClose, rejectClose) => out.end((error) => error ? rejectClose(error) : resolveClose()));
    const actual = hash.digest("hex");
    if (bytes !== length || actual !== sha) throw new Error("input size or sha256 mismatch");
    renameSync(temp, final);
    return json(res, 201, { ok: true, cached: false, sha256: sha, bytes });
  } catch (error) {
    out.destroy(); rmSync(temp, { force: true });
    return json(res, 400, { error: error.message });
  }
}

const server = createServer(async (req, res) => {
  const url = new URL(req.url || "/", `http://${req.headers.host || "localhost"}`);
  if (req.method === "GET" && url.pathname === "/health") return json(res, 200, health());
  if (!authorized(req)) return json(res, 401, { error: "unauthorized" });
  try {
    const inputMatch = url.pathname.match(/^\/v1\/inputs\/([a-f0-9]{64})$/);
    if (req.method === "PUT" && inputMatch) return receiveInput(req, res, inputMatch[1]);
    if (req.method === "POST" && url.pathname === "/v1/jobs") {
      if (!capacity().headroom) return json(res, 503, { error: "worker is under pressure", health: health() });
      const spec = validateSpec(await readJson(req));
      return json(res, 202, { job: publicJob(createJob(spec)) });
    }
    if (req.method === "GET" && url.pathname === "/v1/jobs") {
      return json(res, 200, { jobs: [...jobs.values()].slice(-30).reverse().map(publicJob) });
    }
    const jobMatch = url.pathname.match(/^\/v1\/jobs\/([a-z0-9-]+)$/);
    if (jobMatch && req.method === "GET") {
      const job = jobs.get(jobMatch[1]);
      return job ? json(res, 200, { job: publicJob(job) }) : json(res, 404, { error: "job not found" });
    }
    if (jobMatch && req.method === "DELETE") {
      const job = jobs.get(jobMatch[1]);
      return job ? json(res, cancelJob(job) ? 202 : 409, { job: publicJob(job) }) : json(res, 404, { error: "job not found" });
    }
    const artifactMatch = url.pathname.match(/^\/v1\/jobs\/([a-z0-9-]+)\/artifact$/);
    if (artifactMatch && req.method === "GET") {
      const job = jobs.get(artifactMatch[1]);
      if (!job) return json(res, 404, { error: "job not found" });
      if (job.status !== "success") return json(res, 409, { error: "artifact is not ready" });
      const path = join(jobWorkDir(job.id), job.output);
      res.writeHead(200, {
        "content-type": "application/octet-stream", "content-length": statSync(path).size,
        "x-content-sha256": job.outputSha256, "content-disposition": `attachment; filename="${job.output}"`,
      });
      return createReadStream(path).pipe(res);
    }
    return json(res, 404, { error: "not found" });
  } catch (error) {
    return json(res, 400, { error: error.message || String(error) });
  }
});

server.listen(PORT, BIND, () => {
  console.log(`ac-fleet-worker listening on http://${BIND}:${PORT} as ${ROLE}`);
});

setInterval(pump, 5000).unref();

for (const signal of ["SIGINT", "SIGTERM"]) {
  process.on(signal, () => {
    server.close();
    for (const proc of active.values()) {
      try { process.kill(-proc.pid, "SIGTERM"); } catch {}
    }
    process.exit(0);
  });
}
