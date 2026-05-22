#!/usr/bin/env node
// renders.mjs — list the /pop renders running right now.
//
//   node pop/bin/renders.mjs        # one-shot list
//   node pop/bin/renders.mjs --watch  # refresh every 2 s
//
// Reads the heartbeat files written by pop/lib/render-progress.mjs —
// the same source the Slab menubar progress bars poll.

import { list } from "../lib/render-progress.mjs";

const WATCH = process.argv.includes("--watch");

function bar(pct) {
  if (pct == null) return "····················";
  const n = Math.round(pct / 5);
  return "▓".repeat(n) + "░".repeat(20 - n);
}

function render() {
  const rs = list();
  if (WATCH) process.stdout.write("\x1b[2J\x1b[H");
  if (!rs.length) { console.log("no /pop renders running"); return; }
  for (const r of rs) {
    const pct = r.pct == null ? "  ··" : `${String(r.pct).padStart(3)}%`;
    const age = Math.round((Date.now() - (r.startedAt || Date.now())) / 1000);
    console.log(`${(r.type || "?").padEnd(6)} ${bar(r.pct)} ${pct}  ${r.label}  (${age}s)`);
  }
}

render();
if (WATCH) setInterval(render, 2000);
