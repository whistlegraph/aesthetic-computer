#!/usr/bin/env node
// codex-session-watch.mjs — drive slab session state from a Codex rollout log.
//
// Codex has no per-turn hooks like Claude Code, but it streams every session
// to ~/.codex/sessions/YYYY/MM/DD/rollout-<ts>-<uuid>.jsonl with clean turn
// boundaries. We tail that file and flip the SAME state files the Claude hooks
// use, so the menubar reducer stays agent-agnostic:
//   task_started  → working  (rewrite active marker, drop awaiting, touch running-tools)
//   task_complete → complete (write awaiting "turn complete", drop running-tools)
//
// Usage: node codex-session-watch.mjs <sessionId> <beginEpochSec> <wrapperPid> <tty> <cwd>
// Launched (and killed) by codex-slab.sh. Exits when the wrapper pid dies.

import { readFile, writeFile, stat, unlink, utimes } from "node:fs/promises";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { join } from "node:path";
import { homedir } from "node:os";

const [sid, _beginArg, wrapperArg, tty = "", cwd = ""] = process.argv.slice(2);
if (!sid) process.exit(1);
const wrapperPid = Number(wrapperArg) || 0;

const SLAB_HOME = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab");
const ACTIVE = join(SLAB_HOME, "state", "active-prompts", sid);
const AWAITING = join(SLAB_HOME, "state", "awaiting-prompts", sid);
const RUNNING = join(SLAB_HOME, "state", "running-tools", sid);
const OPEN_IMAGES = join(SLAB_HOME, "state", "open-images");
const SESSIONS = join(process.env.CODEX_HOME || join(homedir(), ".codex"), "sessions");
const execFileAsync = promisify(execFile);

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const nowISO = () => new Date().toISOString().replace(/\.\d+Z$/, "Z");
const wrapperAlive = () => {
  if (!wrapperPid) return true;
  try { process.kill(wrapperPid, 0); return true; }
  catch (error) {
    // Sandboxed launch contexts may forbid signalling an otherwise-live
    // sibling. EPERM proves the PID exists; ESRCH is the actual dead case.
    return error?.code === "EPERM";
  }
};

// Find the rollout file for THIS Codex process. Concurrent windows often share
// cwd and start within the same second, so "newest file" cross-wires their
// rocks. The wrapper and Codex are parent/child; Codex keeps its own rollout
// open for writing, giving us an exact, resume-safe association via lsof.
async function findRollout() {
  for (let i = 0; i < 40 && wrapperAlive(); i++) {
    try {
      // BSD/macOS ps has no Linux `-P <parent>` selector. Read its compact
      // PID/PPID table and select the wrapper's children ourselves.
      const { stdout: processes } = await execFileAsync(
        "/bin/ps", ["-axo", "pid=,ppid="]);
      const pairs = processes.split("\n").map((line) => line.trim().split(/\s+/))
        .filter((parts) => parts.length >= 2)
        .map(([pid, ppid]) => ({ pid: Number(pid), ppid: Number(ppid) }))
        .filter(({ pid, ppid }) => Number.isFinite(pid) && Number.isFinite(ppid));
      // npm's current Codex package launches a Node shim, which then launches
      // the native Rust binary that owns the rollout. Walk the whole process
      // subtree instead of inspecting only the wrapper's direct child.
      const descendants = new Set([wrapperPid]);
      let grew = true;
      while (grew) {
        grew = false;
        for (const { pid, ppid } of pairs) {
          if (descendants.has(ppid) && !descendants.has(pid)) {
            descendants.add(pid);
            grew = true;
          }
        }
      }
      const pids = [...descendants]
        .filter((pid) => pid !== wrapperPid && pid !== process.pid)
        .map(String);
      for (const pid of pids) {
        const { stdout } = await execFileAsync("/usr/sbin/lsof", ["-Fn", "-p", pid]);
        const rollout = stdout.split("\n")
          .filter((line) => line.startsWith("n"))
          .map((line) => line.slice(1))
          .find((p) => p.startsWith(SESSIONS + "/")
            && p.includes("/rollout-") && p.endsWith(".jsonl"));
        if (rollout) return rollout;
      }
    } catch {
      // Codex may not have opened its rollout yet; retry below.
    }
    await sleep(500);
  }
  return null;
}

// Read the marker, merge fields, write it back (atomic-ish).
async function updateMarker(patch) {
  // Keep enough launch metadata here to reconstruct a marker if an external
  // janitor removes it while Codex is still alive. Without this baseline the
  // next rollout event recreated only `{state, updated}`, losing the tty and
  // agent type that Slab needs to theme the terminal.
  let obj = {
    session_id: sid,
    cwd,
    subject: "codex session",
    summary: "codex",
    tty,
    agent_pid: wrapperPid,
    agent_type: "codex",
    state: "blank",
  };
  try { Object.assign(obj, JSON.parse(await readFile(ACTIVE, "utf8"))); } catch {}
  Object.assign(obj, patch, { updated: nowISO() });
  try { await writeFile(ACTIVE, JSON.stringify(obj)); } catch {}
}
const rm = async (p) => { try { await unlink(p); } catch {} };
const touch = async (p) => {
  const t = new Date();
  try { await writeFile(p, "", { flag: "a" }); await utimes(p, t, t); } catch {}
};

// Pull readable text out of a rollout content array (user prompt → subject).
function textOf(payload) {
  const c = payload?.content;
  if (!Array.isArray(c)) return "";
  return c.map((x) => x?.text || x?.input_text || "").join(" ").replace(/\s+/g, " ").trim();
}
function summarize(s) {
  const words = s.split(" ").filter(Boolean).slice(0, 7).join(" ");
  return words.length > 48 ? words.slice(0, 45) + "…" : (s.split(" ").length > 7 ? words + "…" : words);
}

async function onTurnStart(subject) {
  await rm(AWAITING);
  await touch(RUNNING);
  const patch = { state: "working" };
  if (subject) { patch.subject = subject.slice(0, 140); patch.summary = summarize(subject); }
  await updateMarker(patch);
}
async function onTurnComplete() {
  try { await writeFile(AWAITING, "turn complete\n"); } catch {}
  await rm(RUNNING);
}
async function onAwaiting(msg) {
  try { await writeFile(AWAITING, (msg || "needs approval") + "\n"); } catch {}
  await rm(RUNNING);
}

async function onVisualArtifact(path) {
  // ImageGroupPreview consumes this request on the menubar's next tick.
  // The newest generated artifact becomes the current review wall.
  try { await writeFile(OPEN_IMAGES, path + "\n"); } catch {}
  await onAwaiting("visual artifact ready for review");
}

function handleLine(line, ctx) {
  // Image generation returns a saved host path beside an inline bitmap. Match
  // only tool response items so quoted history cannot reopen stale artifacts.
  const isImageToolOutput = line.includes('"type":"response_item"')
    && line.includes('"role":"tool"');
  const generated = isImageToolOutput
    ? line.match(/Generated images are saved[^\n]*? as (\/[^\s]+\.(?:png|jpe?g|webp))/i)
    : null;
  if (generated) ctx.pending.push(() => onVisualArtifact(generated[1]));
  let obj;
  try { obj = JSON.parse(line); } catch { return; }
  const type = obj.type;
  const payload = obj.payload || obj;
  if (type === "response_item" && (payload.role === "user")) {
    const t = textOf(payload);
    if (t) {
      ctx.lastUser = t;
      // Current Codex writes task_started before the user response items. If
      // we only sample lastUser at task_started, a fresh rock stays named
      // simply "codex" for the entire first turn. Update the visible subject
      // when the actual user message arrives; later user items in the same
      // rollout naturally win over injected context blocks.
      ctx.pending.push(() => updateMarker({
        subject: t.slice(0, 140),
        summary: summarize(t),
      }));
    }
    return;
  }
  if (type === "event_msg") {
    const pt = payload.type || "";
    if (pt === "task_started" || pt === "user_turn") ctx.pending.push(() => onTurnStart(ctx.lastUser));
    else if (pt === "task_complete" || pt === "turn_complete") ctx.pending.push(() => onTurnComplete());
    else if (pt.includes("approval") || pt.includes("elicitation")) ctx.pending.push(() => onAwaiting("codex needs approval"));
  }
}

async function main() {
  const file = await findRollout();
  if (!file) process.exit(0);
  // Replay once from the beginning so a resumed Codex window immediately
  // inherits its real last state (usually complete) instead of sitting blank
  // or aging into interrupted until the user submits another prompt. After
  // that first pass `offset` makes this an ordinary incremental tail.
  let offset = 0;
  const ctx = { lastUser: "", pending: [] };
  while (wrapperAlive()) {
    let size = offset;
    try { size = (await stat(file)).size; } catch { break; }
    if (size > offset) {
      let chunk = Buffer.alloc(0);
      try { chunk = await readFile(file); } catch { chunk = Buffer.alloc(0); }
      // Offsets from stat are bytes, not JavaScript UTF-16 character counts.
      // Slice the Buffer first so emoji/non-ASCII output can never skew the
      // tail boundary or cause an already-seen completion to be replayed.
      const tail = chunk.subarray(offset).toString("utf8");
      offset = chunk.length;
      for (const line of tail.split("\n")) if (line.trim()) handleLine(line, ctx);
      // Apply transitions in order; last one wins the visible state.
      for (const fn of ctx.pending) await fn();
      ctx.pending = [];
    }
    await sleep(600);
  }
}

main().catch((error) => {
  console.error(`codex-session-watch: ${error?.stack || error}`);
  process.exit(1);
});
