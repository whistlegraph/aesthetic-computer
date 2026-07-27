#!/usr/bin/env node
// Keep exactly one local Netlify development stack alive.
//
// Netlify starts Caddy as a child. If Netlify exits without reaping it, Caddy
// is adopted by launchd and a naive restart loop accumulates another server on
// :8111 each pass. This supervisor gives each run its own process group, tears
// that group down before retrying, and removes only validated AC media-server
// Caddy orphans from this directory.

import { createHash } from "node:crypto";
import { closeSync, existsSync, openSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn, spawnSync } from "node:child_process";

const systemDir = dirname(dirname(fileURLToPath(import.meta.url)));
const ports = [8880, 8888, 8889, 8080, 8000, 8111, 3333, 3000, 3001];
const explicitContext = process.argv.find((arg) => arg.startsWith("--context="))?.split("=", 2)[1];
const inContainer = existsSync("/.dockerenv") || process.env.CODESPACES === "true"
  || Boolean(process.env.CODESPACE_NAME) || Boolean(process.env.REMOTE_CONTAINERS);
const context = explicitContext && explicitContext !== "auto"
  ? explicitContext : (inContainer ? "codespace" : "local");
const openBrowser = process.argv.includes("--open");
const lockId = createHash("sha256").update(systemDir).digest("hex").slice(0, 12);
const lockPath = join(tmpdir(), `ac-netlify-${process.getuid?.() ?? "user"}-${lockId}.lock`);

let child = null;
let shuttingDown = false;
let lockHeld = false;

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const log = (message) => console.log(`[site-supervisor] ${message}`);

function pidAlive(pid) {
  if (!Number.isInteger(pid) || pid <= 1) return false;
  try { process.kill(pid, 0); return true; } catch (error) { return error?.code === "EPERM"; }
}

function acquireLock() {
  for (let attempt = 0; attempt < 2; attempt += 1) {
    try {
      const fd = openSync(lockPath, "wx", 0o600);
      writeFileSync(fd, `${process.pid}\n`);
      closeSync(fd);
      lockHeld = true;
      return;
    } catch (error) {
      if (error?.code !== "EEXIST") throw error;
      const owner = Number.parseInt(readFileSync(lockPath, "utf8").trim(), 10);
      if (pidAlive(owner)) {
        throw new Error(`another site supervisor is already running (pid ${owner})`);
      }
      rmSync(lockPath, { force: true });
    }
  }
  throw new Error(`could not acquire ${lockPath}`);
}

function releaseLock() {
  if (!lockHeld) return;
  try {
    const owner = Number.parseInt(readFileSync(lockPath, "utf8").trim(), 10);
    if (owner === process.pid) rmSync(lockPath, { force: true });
  } catch {}
  lockHeld = false;
}

function output(command, args) {
  const result = spawnSync(command, args, { encoding: "utf8", stdio: ["ignore", "pipe", "ignore"] });
  return result.status === 0 ? result.stdout.trim() : "";
}

function validatedCaddyPids() {
  const raw = output("pgrep", ["-x", "caddy"]);
  if (!raw) return [];
  return raw.split(/\s+/).map(Number).filter((pid) => {
    const command = output("ps", ["-p", String(pid), "-o", "command="]);
    if (!/(^|\/)caddy run --config Caddyfile(?:\s|$)/.test(command)) return false;
    const cwd = output("lsof", ["-a", "-p", String(pid), "-d", "cwd", "-Fn"])
      .split("\n").find((line) => line.startsWith("n"))?.slice(1);
    return cwd === systemDir;
  });
}

async function terminatePids(pids, label) {
  const live = [...new Set(pids)].filter(pidAlive);
  if (!live.length) return;
  log(`stopping ${live.length} ${label}`);
  for (const pid of live) {
    try { process.kill(pid, "SIGTERM"); } catch {}
  }
  for (let attempt = 0; attempt < 20 && live.some(pidAlive); attempt += 1) await sleep(100);
  for (const pid of live.filter(pidAlive)) {
    try { process.kill(pid, "SIGKILL"); } catch {}
  }
}

async function cleanupValidatedCaddy() {
  await terminatePids(validatedCaddyPids(), "orphaned AC Caddy process(es)");
}

async function stopChildGroup() {
  if (!child?.pid) return;
  const pid = child.pid;
  try { process.kill(-pid, "SIGTERM"); } catch {}
  for (let attempt = 0; attempt < 20 && pidAlive(pid); attempt += 1) await sleep(100);
  try { process.kill(-pid, "SIGKILL"); } catch {}
  child = null;
}

async function shutdown(signal, exitCode = 0) {
  if (shuttingDown) return;
  shuttingDown = true;
  log(`received ${signal}; cleaning development children`);
  await stopChildGroup();
  await cleanupValidatedCaddy();
  releaseLock();
  process.exit(exitCode);
}

for (const signal of ["SIGINT", "SIGTERM", "SIGHUP"]) {
  process.on(signal, () => void shutdown(signal, 0));
}
process.on("exit", releaseLock);

async function runNetlify() {
  const startedAt = Date.now();
  const args = ["dev", "--context", context];
  if (openBrowser) args.push("-o");
  child = spawn("netlify", args, {
    cwd: systemDir,
    detached: true,
    env: process.env,
    stdio: "inherit",
  });
  const exit = await new Promise((resolve) => {
    child.once("error", (error) => resolve({ code: 127, signal: null, error }));
    child.once("exit", (code, signal) => resolve({ code, signal }));
  });
  await stopChildGroup();
  await cleanupValidatedCaddy();
  return { ...exit, runtimeMs: Date.now() - startedAt };
}

async function main() {
  acquireLock();
  log(`context=${context}; ports=${ports.join(",")}; pid=${process.pid}`);
  await cleanupValidatedCaddy();

  if (context === "codespace" && process.env.NETLIFY_SITE_ID) {
    const link = spawnSync("netlify", ["link", "--id", process.env.NETLIFY_SITE_ID], {
      cwd: systemDir, stdio: "inherit", env: process.env,
    });
    if (link.status !== 0) throw new Error(`netlify link failed with status ${link.status}`);
  }

  const recentFailures = [];
  let attempt = 0;
  while (!shuttingDown) {
    log(`starting Netlify dev (attempt ${attempt + 1})`);
    const result = await runNetlify();
    if (shuttingDown) break;
    if (result.error) console.error(`[site-supervisor] ${result.error.message}`);

    const now = Date.now();
    if (result.runtimeMs >= 5 * 60_000) attempt = 0;
    else attempt += 1;
    recentFailures.push(now);
    while (recentFailures[0] < now - 5 * 60_000) recentFailures.shift();
    if (recentFailures.length >= 8) {
      throw new Error("Netlify exited 8 times in five minutes; refusing a runaway restart loop");
    }

    const delaySeconds = Math.min(30, 2 ** Math.min(attempt, 5));
    log(`Netlify exited code=${result.code ?? "-"} signal=${result.signal ?? "-"}; retrying in ${delaySeconds}s`);
    await sleep(delaySeconds * 1000);
  }
}

main().catch(async (error) => {
  console.error(`[site-supervisor] fatal: ${error.message}`);
  await stopChildGroup();
  await cleanupValidatedCaddy();
  releaseLock();
  process.exit(1);
});
