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
// Usage: node codex-session-watch.mjs <sessionId> <beginEpochSec> <wrapperPid>
// Launched (and killed) by codex-slab.sh. Exits when the wrapper pid dies.

import { readdir, readFile, writeFile, stat, unlink, utimes } from "node:fs/promises";
import { join } from "node:path";
import { homedir } from "node:os";

const [sid, beginArg, wrapperArg] = process.argv.slice(2);
if (!sid) process.exit(1);
const beginSec = Number(beginArg) || Math.floor(Date.now() / 1000);
const wrapperPid = Number(wrapperArg) || 0;

const SLAB_HOME = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab");
const ACTIVE = join(SLAB_HOME, "state", "active-prompts", sid);
const AWAITING = join(SLAB_HOME, "state", "awaiting-prompts", sid);
const RUNNING = join(SLAB_HOME, "state", "running-tools", sid);
const SESSIONS = join(process.env.CODEX_HOME || join(homedir(), ".codex"), "sessions");

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const nowISO = () => new Date().toISOString().replace(/\.\d+Z$/, "Z");
const wrapperAlive = () => {
  if (!wrapperPid) return true;
  try { process.kill(wrapperPid, 0); return true; } catch { return false; }
};

async function walk(dir, out = []) {
  let entries;
  try { entries = await readdir(dir, { withFileTypes: true }); } catch { return out; }
  for (const e of entries) {
    const p = join(dir, e.name);
    if (e.isDirectory()) await walk(p, out);
    else if (e.isFile() && e.name.startsWith("rollout-") && e.name.endsWith(".jsonl")) out.push(p);
  }
  return out;
}

// Find the rollout file for THIS codex session: the newest one created at/after
// the wrapper's start. Poll until it appears (codex writes it on session start).
async function findRollout() {
  for (let i = 0; i < 40 && wrapperAlive(); i++) {
    const files = await walk(SESSIONS);
    let best = null, bestM = 0;
    for (const f of files) {
      let m;
      try { m = (await stat(f)).mtimeMs; } catch { continue; }
      if (m >= (beginSec - 3) * 1000 && m > bestM) { best = f; bestM = m; }
    }
    if (best) return best;
    await sleep(500);
  }
  return null;
}

// Read the marker, merge fields, write it back (atomic-ish).
async function updateMarker(patch) {
  let obj = {};
  try { obj = JSON.parse(await readFile(ACTIVE, "utf8")); } catch {}
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

function handleLine(line, ctx) {
  let obj;
  try { obj = JSON.parse(line); } catch { return; }
  const type = obj.type;
  const payload = obj.payload || obj;
  if (type === "response_item" && (payload.role === "user")) {
    const t = textOf(payload);
    if (t) ctx.lastUser = t;
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
  // Start tailing from EOF — historical turns already happened; we only care
  // about live transitions from here on. (A fresh `co` launch has an empty log.)
  let offset = 0;
  try { offset = (await stat(file)).size; } catch {}
  const ctx = { lastUser: "", pending: [] };
  while (wrapperAlive()) {
    let size = offset;
    try { size = (await stat(file)).size; } catch { break; }
    if (size > offset) {
      let chunk = "";
      try { chunk = await readFile(file, { encoding: "utf8" }); } catch { chunk = ""; }
      // Re-read whole file (rollouts are small) and process only new tail.
      const tail = chunk.slice(offset);
      offset = chunk.length;
      for (const line of tail.split("\n")) if (line.trim()) handleLine(line, ctx);
      // Apply transitions in order; last one wins the visible state.
      for (const fn of ctx.pending) await fn();
      ctx.pending = [];
    }
    await sleep(600);
  }
}

main().catch(() => process.exit(0));
