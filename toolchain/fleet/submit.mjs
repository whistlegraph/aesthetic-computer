#!/usr/bin/env node
// Submit a typed FFmpeg mission to the least-loaded AC fleet worker.

import { createHash } from "node:crypto";
import { createReadStream, createWriteStream, existsSync, readFileSync, renameSync, rmSync, statSync } from "node:fs";
import { homedir, hostname } from "node:os";
import { dirname, resolve } from "node:path";
import { mkdir } from "node:fs/promises";

const HOME = homedir();
const PORT = Number(process.env.AC_FLEET_WORKER_PORT || 5263);
const TOKEN_FILE = process.env.AC_FLEET_WORKER_TOKEN_FILE || `${HOME}/.config/ac-fleet-worker/token`;
const REGISTRY = process.env.FLEET_MACHINES || `${HOME}/aesthetic-computer-vault/machines.normalized.json`;

function usage(message) {
  if (message) console.error(`error: ${message}\n`);
  console.error("usage: node toolchain/fleet/submit.mjs ffmpeg [--host NAME] --input NAME=PATH [--input ...] --output PATH [--duration SECONDS] [--detach] -- ARGS");
  console.error("args use @input:NAME and @output placeholders; example:");
  console.error("  ... ffmpeg --input src=in.mov --output out.mp4 -- -i @input:src -c:v libx264 @output");
  process.exit(message ? 2 : 0);
}

function parse(argv) {
  if (argv.shift() !== "ffmpeg") usage("only the ffmpeg adapter exists in v1");
  const spec = { host: "", inputs: [], output: "", duration: null, detach: false, args: [] };
  while (argv.length) {
    const arg = argv.shift();
    if (arg === "--") { spec.args = argv.splice(0); break; }
    if (arg === "--host") spec.host = argv.shift() || "";
    else if (arg === "--input") spec.inputs.push(argv.shift() || "");
    else if (arg === "--output") spec.output = argv.shift() || "";
    else if (arg === "--duration") spec.duration = Number(argv.shift());
    else if (arg === "--detach") spec.detach = true;
    else if (arg === "--help" || arg === "-h") usage();
    else usage(`unknown option: ${arg}`);
  }
  if (!spec.output || !spec.inputs.length || !spec.args.length) usage("input, output, and ffmpeg args are required");
  spec.inputs = spec.inputs.map((entry) => {
    const eq = entry.indexOf("=");
    if (eq < 1) usage(`input must be NAME=PATH: ${entry}`);
    const name = entry.slice(0, eq), path = resolve(entry.slice(eq + 1));
    if (!existsSync(path) || !statSync(path).isFile()) usage(`input file is missing: ${path}`);
    return { name, path };
  });
  spec.output = resolve(spec.output);
  return spec;
}

function registryMachines() {
  try {
    const doc = JSON.parse(readFileSync(REGISTRY, "utf8"));
    return doc.machines || doc;
  } catch { return {}; }
}

function candidateList(wanted) {
  const machines = registryMachines();
  const defaults = wanted ? [wanted] : ["poorslice", "chicken", "panda", "blueberry", "neo"];
  return defaults.map((name) => {
    const m = machines[name] || Object.values(machines).find((x) =>
      x?.name === name || x?.tailscale?.name === name || x?.ssh?.alias === name) || {};
    const ip = m.tailscale?.ip || (name === hostname().split(".")[0] ? "127.0.0.1" : name);
    const role = name === "poorslice" ? "heavy"
      : name === "blueberry" ? "light"
      : name === "neo" ? "interactive" : "balanced";
    return { name, ip, role };
  });
}

async function workerHealth(candidate) {
  try {
    const res = await fetch(`http://${candidate.ip}:${PORT}/health`, { signal: AbortSignal.timeout(1800) });
    if (!res.ok) return null;
    const value = await res.json();
    return { ...candidate, health: value };
  } catch { return null; }
}

async function chooseWorker(wanted) {
  const probed = await Promise.all(candidateList(wanted).map(workerHealth));
  const online = probed.filter(Boolean);
  if (!online.length) throw new Error(wanted ? `${wanted} worker is unreachable` : "no fleet workers are reachable");
  if (wanted) {
    if (!online[0].health.headroom) throw new Error(`${wanted} is under pressure and refused new work`);
    return online[0];
  }
  const priority = { poorslice: 100, chicken: 82, panda: 80, blueberry: 58, neo: 22 };
  const usable = online.filter((x) => x.health.accepting);
  if (!usable.length) throw new Error(`all reachable workers are busy or under pressure: ${online.map((x) => x.name).join(", ")}`);
  usable.sort((a, b) => score(b) - score(a));
  return usable[0];
  function score(x) {
    return (priority[x.name] || 40) + x.health.memoryFreePct * 0.35 - x.health.cpuPct * 0.25 - x.health.queued * 30;
  }
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

async function authFetch(url, token, options = {}) {
  return fetch(url, { ...options, headers: { authorization: `Bearer ${token}`, ...(options.headers || {}) } });
}

async function upload(worker, token, input) {
  const sha256 = await shaFile(input.path);
  const bytes = statSync(input.path).size;
  process.stderr.write(`upload ${input.name} (${(bytes / 1_048_576).toFixed(1)} MB) → ${worker.name}\n`);
  const res = await authFetch(`http://${worker.ip}:${PORT}/v1/inputs/${sha256}`, token, {
    method: "PUT", headers: { "content-length": String(bytes), "content-type": "application/octet-stream" },
    body: createReadStream(input.path), duplex: "half",
  });
  const value = await res.json();
  if (!res.ok) throw new Error(value.error || `upload failed with HTTP ${res.status}`);
  return { name: input.name, sha256 };
}

async function submit(worker, token, spec, inputs) {
  const outputName = spec.output.split("/").pop();
  const body = {
    type: "ffmpeg", inputs, output: outputName, args: spec.args,
    expectedDurationSeconds: Number.isFinite(spec.duration) && spec.duration > 0 ? spec.duration : undefined,
    origin: { host: hostname().split(".")[0], handle: process.env.AC_PROX_HANDLE || "" },
  };
  const res = await authFetch(`http://${worker.ip}:${PORT}/v1/jobs`, token, {
    method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify(body),
  });
  const value = await res.json();
  if (!res.ok) throw new Error(value.error || `submit failed with HTTP ${res.status}`);
  return value.job;
}

async function waitForJob(worker, token, job) {
  let last = "";
  while (true) {
    await new Promise((r) => setTimeout(r, 1500));
    const res = await authFetch(`http://${worker.ip}:${PORT}/v1/jobs/${job.id}`, token);
    const value = await res.json();
    if (!res.ok) throw new Error(value.error || `status failed with HTTP ${res.status}`);
    job = value.job;
    const line = `${job.stage} ${Math.round((job.progress || 0) * 100)}%`;
    if (line !== last) { process.stderr.write(`${worker.name}: ${line}\n`); last = line; }
    if (["success", "failed", "cancelled"].includes(job.status)) return job;
  }
}

async function download(worker, token, job, output) {
  await mkdir(dirname(output), { recursive: true });
  const temp = `${output}.${process.pid}.part`;
  const res = await authFetch(`http://${worker.ip}:${PORT}/v1/jobs/${job.id}/artifact`, token);
  if (!res.ok) {
    const value = await res.json();
    throw new Error(value.error || `artifact failed with HTTP ${res.status}`);
  }
  const out = createWriteStream(temp, { mode: 0o600 });
  const hash = createHash("sha256");
  try {
    for await (const chunk of res.body) { hash.update(chunk); if (!out.write(chunk)) await new Promise((r) => out.once("drain", r)); }
    await new Promise((r, reject) => out.end((e) => e ? reject(e) : r()));
    const actual = hash.digest("hex");
    const expected = res.headers.get("x-content-sha256") || job.outputSha256;
    if (!expected || actual !== expected) throw new Error("downloaded artifact sha256 did not match");
    renameSync(temp, output);
  } catch (error) { out.destroy(); rmSync(temp, { force: true }); throw error; }
}

async function main() {
  const spec = parse(process.argv.slice(2));
  if (!existsSync(TOKEN_FILE)) throw new Error(`worker token is missing: ${TOKEN_FILE}`);
  const token = readFileSync(TOKEN_FILE, "utf8").trim();
  const worker = await chooseWorker(spec.host);
  process.stderr.write(`selected ${worker.name}: CPU ${worker.health.cpuPct.toFixed(0)}% · RAM ${worker.health.memoryUsedPct.toFixed(0)}% · ${worker.health.role}\n`);
  const inputs = [];
  for (const input of spec.inputs) inputs.push(await upload(worker, token, input));
  let job = await submit(worker, token, spec, inputs);
  process.stderr.write(`mission ${job.id} accepted by ${worker.name}\n`);
  if (spec.detach) { console.log(JSON.stringify({ worker: worker.name, ip: worker.ip, job }, null, 2)); return; }
  job = await waitForJob(worker, token, job);
  if (job.status !== "success") throw new Error(job.error || `mission ${job.status}`);
  await download(worker, token, job, spec.output);
  console.log(`${spec.output}\t${job.outputSha256}\t${worker.name}:${job.id}`);
}

main().catch((error) => { console.error(`fleet submit: ${error.message || error}`); process.exit(1); });
