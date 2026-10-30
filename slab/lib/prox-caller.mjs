// Who is calling a shared slab MCP daemon (prox, dm). The daemon's own
// process.env says nothing about the caller. In order: the session id a
// harness forwards as a header; the process env, but only when the server is
// a per-session stdio child; and, for Claude Code (which forwards nothing),
// the process that owns the caller's end of the loopback socket — its pid, or
// an ancestor's, is in a session marker written by the slab claude hooks.

import { readFile, readdir } from "node:fs/promises";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { join } from "node:path";
import { homedir } from "node:os";

const pexec = promisify(execFile);
const SLAB_HOME = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab");
const MARKER_DIRS = [join(SLAB_HOME, "state", "active-prompts"), join(SLAB_HOME, "state", "awaiting-prompts")];
const LOCAL_LEDGER = join(homedir(), ".config", "slab", "ledger", "local.json");

const isLoopback = (a) => ["127.0.0.1", "::1", "::ffff:127.0.0.1"].includes(a);

async function readJson(path) {
  try {
    return JSON.parse(await readFile(path, "utf8"));
  } catch {
    return null;
  }
}

async function markerPids() {
  const pids = new Map();
  for (const dir of MARKER_DIRS) {
    for (const name of await readdir(dir).catch(() => [])) {
      const m = await readJson(join(dir, name));
      const pid = Number(m?.agent_pid || m?.claude_pid || 0);
      if (pid > 0) pids.set(pid, name);
    }
  }
  return pids;
}

async function sessionByPeerPort(port) {
  const { stdout } = await pexec("lsof", ["-nP", "-a", `-iTCP:${port}`, "-sTCP:ESTABLISHED", "-Fp"], { timeout: 1500 }).catch(() => ({ stdout: "" }));
  const owners = stdout.split("\n").filter((l) => l.startsWith("p")).map((l) => Number(l.slice(1))).filter((p) => p > 0 && p !== process.pid);
  if (!owners.length) return null;
  const markers = await markerPids();
  for (const owner of owners) {
    let pid = owner;
    for (let hop = 0; hop < 8 && pid > 1; hop++) {
      if (markers.has(pid)) return markers.get(pid);
      const { stdout: ppid } = await pexec("ps", ["-o", "ppid=", "-p", String(pid)], { timeout: 1000 }).catch(() => ({ stdout: "" }));
      pid = Number(ppid.trim()) || 0;
    }
  }
  return null;
}

export async function callerSessionId(context) {
  const header = context?.headers?.["x-slab-prompt-session-id"];
  if (typeof header === "string" && header) return header;
  if (!context) return process.env.AGENT_SESSION_ID || process.env.CLAUDE_SESSION_ID || process.env.SLAB_PROMPT_SESSION_ID || null;
  if (isLoopback(context.remoteAddress) && context.remotePort) return sessionByPeerPort(context.remotePort);
  return null;
}

// The caller's `host:name` from this machine's ledger, or null when the caller
// can't be told or isn't a rock here.
export async function callerRock(context) {
  const id = await callerSessionId(context);
  if (!id) return null;
  const ledger = await readJson(LOCAL_LEDGER);
  const rock = ledger?.entries?.find((e) => e.id === id);
  return rock ? `${rock.host || ledger.host}:${rock.name}` : null;
}
