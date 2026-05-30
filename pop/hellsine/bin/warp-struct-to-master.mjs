#!/usr/bin/env node
// pop/hellsine/bin/warp-struct-to-master.mjs — apply the post-bake tempo
// warp to hellsine.struct.json so its event timings line up with the
// finalized hellsine-MASTER.wav timeline (the master gets atempo=1.06
// from 110.77s onward, then truncates at 162.0s — see bake.mjs).
//
// The engine writes struct.json in ENGINE time (no tempo bump).
// Visualizers consume struct.json against the MASTER wav timeline.
// This script reads the engine struct in-place, walks every event time
// + section boundary, applies the warp, and writes the file back.
//
// Idempotent IF and only if no previous run was warped (a flag is added
// so re-running is a no-op). Re-running after a fresh engine emit is
// always safe.
//
// Usage:
//   node pop/hellsine/bin/warp-struct-to-master.mjs

import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const STRUCT_PATH = resolve(HERE, "../hellsine.struct.json");

// Mirror bake.mjs.
const TEMPO_CUT = 110.77;             // s · climax.startSec — the final drop
const TEMPO_MUL = 1.06;
const TRUNCATE  = 162.0;              // s · master is truncated here

function warpT(tEngine) {
  if (tEngine <= TEMPO_CUT) return tEngine;
  return TEMPO_CUT + (tEngine - TEMPO_CUT) / TEMPO_MUL;
}

function round4(x) { return +x.toFixed(4); }

const s = JSON.parse(readFileSync(STRUCT_PATH, "utf8"));
if (s.masterWarpApplied) {
  console.log("✓ struct already warped to master timing — no-op");
  process.exit(0);
}

let warpedEvents = 0;
let truncatedEvents = 0;
for (const lane of Object.keys(s.events || {})) {
  const evs = s.events[lane];
  const out = [];
  for (const ev of evs) {
    const w = warpT(ev.t);
    if (w >= TRUNCATE) { truncatedEvents++; continue; }
    if (w !== ev.t) warpedEvents++;
    out.push({ ...ev, t: round4(w) });
  }
  s.events[lane] = out;
  if (s.counts) s.counts[lane] = out.length;
}

// Warp section boundaries too — keeps sectionForTime() honest in the
// player + visualizer.
if (Array.isArray(s.sections)) {
  for (const sec of s.sections) {
    if (typeof sec.startSec === "number") sec.startSec = round4(warpT(sec.startSec));
    if (typeof sec.endSec   === "number") sec.endSec   = round4(Math.min(TRUNCATE, warpT(sec.endSec)));
  }
}

s.totalSec = TRUNCATE;
s.masterWarpApplied = { tempoCut: TEMPO_CUT, tempoMul: TEMPO_MUL, truncate: TRUNCATE };

writeFileSync(STRUCT_PATH, JSON.stringify(s, null, 2) + "\n");
console.log(`✓ warped ${warpedEvents} events past ${TEMPO_CUT}s by /${TEMPO_MUL}`);
console.log(`  truncated ${truncatedEvents} events past ${TRUNCATE}s`);
console.log(`  totalSec → ${TRUNCATE}`);
console.log(`  ${STRUCT_PATH.replace(process.env.HOME, "~")}`);
