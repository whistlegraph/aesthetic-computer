#!/usr/bin/env node
// Linux/headless Prox host. It serves the same compact ledger shape as the
// Slab menubar on the machine's tailnet address, while job control is limited
// to named systemd user units. No endpoint accepts shell text, executables,
// paths, prompts, or environment variables.

import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { createServer } from "node:http";
import { mkdir, readFile, readdir, rename, writeFile } from "node:fs/promises";
import { homedir, hostname } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

const pexec = promisify(execFile);
const JOBS = Object.freeze({ mediascholar: "mediascholar.service" });

export function workerConfig(env = process.env) {
  const ledgerDir = resolve(env.PROX_WORKER_LEDGER_DIR || join(homedir(), ".config", "slab", "ledger"));
  return {
    host: String(env.PROX_WORKER_HOST || hostname().split(".")[0]).toLowerCase(),
    bind: env.PROX_WORKER_BIND || "",
    port: Number(env.PROX_WORKER_PORT) || 5252,
    ledgerDir,
    advertiseDir: resolve(env.PROX_WORKER_ADVERTISE_DIR || join(ledgerDir, "advertise")),
    localFile: resolve(env.PROX_WORKER_LOCAL_FILE || join(ledgerDir, "local.json")),
    refreshMs: Number(env.PROX_WORKER_REFRESH_MS) || 3000,
    systemctl: env.PROX_WORKER_SYSTEMCTL || "systemctl",
  };
}

async function readJson(path) {
  try { return JSON.parse(await readFile(path, "utf8")); }
  catch { return null; }
}

async function atomicJson(path, value) {
  await mkdir(dirname(path), { recursive: true });
  const temp = `${path}.${process.pid}.tmp`;
  await writeFile(temp, `${JSON.stringify(value, null, 2)}\n`, { mode: 0o600 });
  await rename(temp, path);
}

async function tailnetIp(config) {
  if (config.bind) return config.bind;
  try {
    const { stdout } = await pexec("tailscale", ["ip", "-4"], { timeout: 3000 });
    return stdout.trim().split(/\s+/)[0] || "";
  } catch { return ""; }
}

function cleanEntry(value, config) {
  if (!value || typeof value !== "object") return null;
  const id = String(value.id || "").slice(0, 180);
  const name = String(value.name || "").toLowerCase()
    .replace(/[^a-z0-9-]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 80);
  if (!id || !name) return null;
  const status = new Set(["working", "awaiting", "complete", "rendering", "blank", "interrupted"])
    .has(value.status) ? value.status : "complete";
  return {
    id,
    host: config.host,
    name,
    subject: String(value.subject || "headless agent").replace(/\s+/g, " ").slice(0, 180),
    status,
    kind: "agent",
    seed: String(value.seed || createHash("sha256").update(id).digest("hex")).slice(0, 128),
    cwd: String(value.cwd || "").slice(0, 1000),
    updated: Number(value.updated) || Date.now(),
    started: Number(value.started) || undefined,
    agentType: String(value.agentType || name).slice(0, 40),
    artifact: value.artifact ? String(value.artifact).slice(0, 1000) : undefined,
    runId: value.runId ? String(value.runId).slice(0, 180) : undefined,
  };
}

export async function buildLedger(config, ip) {
  await mkdir(config.advertiseDir, { recursive: true });
  const entries = [];
  for (const name of await readdir(config.advertiseDir).catch(() => [])) {
    if (!name.endsWith(".json")) continue;
    const entry = cleanEntry(await readJson(join(config.advertiseDir, name)), config);
    if (entry) entries.push(entry);
  }
  entries.sort((a, b) => b.updated - a.updated);
  return { host: config.host, ip, updatedAt: Date.now(), entries };
}

async function readBody(req, max = 16 * 1024) {
  let text = "";
  for await (const chunk of req) {
    text += chunk;
    if (Buffer.byteLength(text) > max) throw new Error("request too large");
  }
  return text ? JSON.parse(text) : {};
}

async function systemctl(config, args) {
  const { stdout, stderr } = await pexec(config.systemctl, ["--user", ...args], {
    timeout: 15_000,
    maxBuffer: 1024 * 1024,
  });
  return { stdout: stdout.trim(), stderr: stderr.trim() };
}

async function jobAction(config, body) {
  const job = String(body.job || "").toLowerCase();
  const action = String(body.action || "status").toLowerCase();
  const unit = JOBS[job];
  if (!unit) return { status: 400, body: { ok: false, error: "job is not allowlisted" } };
  if (!new Set(["start", "status", "cancel"]).has(action)) {
    return { status: 400, body: { ok: false, error: "action must be start, status, or cancel" } };
  }
  try {
    if (action === "start") {
      await systemctl(config, ["start", "--no-block", unit]);
      return { status: 202, body: { ok: true, host: config.host, job, action, unit, state: "queued" } };
    }
    if (action === "cancel") {
      await systemctl(config, ["stop", unit]);
      return { status: 200, body: { ok: true, host: config.host, job, action, unit, state: "stopped" } };
    }
    const { stdout } = await systemctl(config, [
      "show", unit, "--property=ActiveState,SubState,Result,ExecMainStatus,MemoryCurrent,CPUUsageNSec",
    ]);
    const properties = Object.fromEntries(stdout.split("\n").filter(Boolean).map((line) => {
      const at = line.indexOf("=");
      return at < 0 ? [line, ""] : [line.slice(0, at), line.slice(at + 1)];
    }));
    return { status: 200, body: { ok: true, host: config.host, job, action, unit, properties } };
  } catch (error) {
    return { status: 503, body: { ok: false, error: String(error.stderr || error.message).trim().slice(-500) } };
  }
}

function respond(res, status, body) {
  const data = Buffer.from(JSON.stringify(body));
  res.writeHead(status, {
    "content-type": "application/json",
    "content-length": String(data.length),
    "cache-control": "no-store",
  });
  res.end(data);
}

export function createWorkerServer(config, ip) {
  return createServer(async (req, res) => {
    try {
      const url = new URL(req.url || "/", `http://${ip}:${config.port}`);
      if (req.method === "POST" && url.pathname === "/job") {
        const result = await jobAction(config, await readBody(req));
        respond(res, result.status, result.body);
        return;
      }
      if (req.method === "POST" && url.pathname === "/poke") {
        await readBody(req).catch(() => ({}));
        respond(res, 200, { ok: true });
        return;
      }
      if (req.method !== "GET") {
        respond(res, 405, { ok: false, error: "method not allowed" });
        return;
      }
      const ledger = await buildLedger(config, ip);
      await atomicJson(config.localFile, ledger);
      respond(res, 200, ledger);
    } catch (error) {
      respond(res, 400, { ok: false, error: error.message });
    }
  });
}

async function main() {
  const config = workerConfig();
  const ip = await tailnetIp(config);
  if (!ip) throw new Error("no Tailscale IPv4 address; set PROX_WORKER_BIND only for local testing");
  const ledger = await buildLedger(config, ip);
  await atomicJson(config.localFile, ledger);
  if (process.argv.includes("--check")) {
    console.log(JSON.stringify({ host: config.host, ip, port: config.port, jobs: Object.keys(JOBS), ledger }, null, 2));
    return;
  }
  const server = createWorkerServer(config, ip);
  server.listen(config.port, ip, () => console.error(`headless Prox ${config.host} on http://${ip}:${config.port}`));
  const timer = setInterval(async () => {
    await atomicJson(config.localFile, await buildLedger(config, ip)).catch(() => {});
  }, config.refreshMs);
  timer.unref();
}

const isMain = process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isMain) main().catch((error) => {
  console.error(`prox-worker: ${error.message}`);
  process.exit(1);
});
