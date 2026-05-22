// render-progress.mjs — progress heartbeats for long /pop renders.
//
// Each slow /pop render (audio = render.mjs, illy = gen-illy.mjs, video
// = preview-score.mjs) writes a tiny JSON heartbeat into
// ~/.ac-pop-renders/ so an external watcher — the Slab menubar — can
// show a live, temporary progress bar per render and drop it the moment
// the render ends.
//
// Lifecycle:  begin({type,label}) → update(pct) → end()
//
// Heartbeat file ~/.ac-pop-renders/<id>.json:
//   { id, type, label, pct, pid, startedAt, updatedAt }
//   type ∈ "audio" | "illy" | "video"
//   pct  ∈ 0..100, or null for an indeterminate render
//
// The reader (`pop renders` CLI, and the Swift menubar) treats a file
// as stale — and sweeps it — when its pid is dead or updatedAt is older
// than STALE_MS, so a crashed render never leaves a ghost bar behind.

import { mkdirSync, writeFileSync, rmSync, readdirSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

export const RENDERS_DIR = join(homedir(), ".ac-pop-renders");
const STALE_MS = 120_000;

function ensureDir() {
  try { mkdirSync(RENDERS_DIR, { recursive: true }); } catch { /* ignore */ }
}

// ── writer side (the render process) ─────────────────────────────────
let _id = null, _file = null, _type = null, _label = null;
let _started = 0, _lastWrite = 0;

export function begin({ type, label } = {}) {
  ensureDir();
  _type = type || "render";
  _label = label || _type;
  _started = Date.now();
  _id = `${_type}-${process.pid}-${_started.toString(36)}`;
  _file = join(RENDERS_DIR, `${_id}.json`);
  _write(0);
  // best-effort sweep if the process exits or is interrupted
  const done = () => { try { rmSync(_file, { force: true }); } catch { /* ignore */ } };
  process.on("exit", done);
  process.on("SIGINT", () => { done(); process.exit(130); });
  process.on("SIGTERM", () => { done(); process.exit(143); });
  return _id;
}

export function update(pct) {
  if (!_file) return;
  // throttle to ~3 writes/s — the menubar only polls every 2 s
  if (pct != null && pct < 100 && Date.now() - _lastWrite < 300) return;
  _write(pct);
}

export function end() {
  if (!_file) return;
  try { rmSync(_file, { force: true }); } catch { /* ignore */ }
  _file = _id = null;
}

function _write(pct) {
  if (!_file) return;
  _lastWrite = Date.now();
  const rec = {
    id: _id, type: _type, label: _label,
    pct: pct == null ? null : Math.max(0, Math.min(100, Math.round(pct))),
    pid: process.pid, startedAt: _started, updatedAt: _lastWrite,
  };
  try { writeFileSync(_file, JSON.stringify(rec)); } catch { /* ignore */ }
}

// ── reader side (`pop renders` CLI; mirrors the Swift menubar) ────────
function _alive(pid) {
  if (!pid) return false;
  try { process.kill(pid, 0); return true; }
  catch (e) { return e.code === "EPERM"; }   // alive but not ours
}

export function list() {
  ensureDir();
  const out = [];
  let names = [];
  try { names = readdirSync(RENDERS_DIR); } catch { return out; }
  for (const n of names) {
    if (!n.endsWith(".json")) continue;
    const f = join(RENDERS_DIR, n);
    let rec;
    try { rec = JSON.parse(readFileSync(f, "utf8")); } catch { continue; }
    const stale = Date.now() - (rec.updatedAt || 0) > STALE_MS || !_alive(rec.pid);
    if (stale) { try { rmSync(f, { force: true }); } catch { /* ignore */ } continue; }
    out.push(rec);
  }
  return out.sort((a, b) => (a.startedAt || 0) - (b.startedAt || 0));
}
