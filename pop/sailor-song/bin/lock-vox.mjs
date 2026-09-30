#!/usr/bin/env node
// lock-vox.mjs — pull her sung onsets onto her own strum grid.
//
// The lane FOLLOWS her guitar, so the grid is hers (measures.json). Her
// voice floats a little off it: note onsets sit a median 41 ms from the
// nearest 8th (p90 105 ms, slightly late). This builds one smooth time map
// — every tuned-note onset (vox-notes.json) moved STRENGTH of the way to
// its nearest 8th, never more than MAX_MS — and runs the SAME map through
// rubberband (R3, formant-preserving, -M timemap) on the lead, the halo and
// every harmony stem, so the stack stays one voice. Timing only; pitch is
// untouched.
//
//   node pop/sailor-song/bin/lock-vox.mjs [--strength 0.8] [--max-ms 100]
//     → src/vox/locked/*.wav  (render.mjs prefers these when present)

import { readFileSync, writeFileSync, mkdirSync, readdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const VOX = resolve(LANE, "src/vox");
const OUT = resolve(VOX, "locked");
mkdirSync(OUT, { recursive: true });
const SR = 48_000;

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (a.startsWith("--")) flags[a.slice(2)] = process.argv[++i];
}
const STRENGTH = Number(flags.strength ?? 0.6);          // v6.1: was 0.8 — keep more of her placement (ANALYSIS.md §2)
const MAX = Number(flags["max-ms"] ?? 70) / 1000;         // v6.1: was 100
// v6.1: no note may be squeezed or stretched past these local ratios. Before
// this clamp adjacent anchors moving opposite ways compressed a held note at
// 79.5 s to 0.20 of its length and 21 more by 30–40% ("true is really short").
const RATIO_MIN = Number(flags["ratio-min"] ?? 0.85), RATIO_MAX = Number(flags["ratio-max"] ?? 1.15);
const MIN_GAP = 0.04;

const M = JSON.parse(readFileSync(resolve(LANE, "measures.json"), "utf8"));
const V = JSON.parse(readFileSync(resolve(LANE, "vox-notes.json"), "utf8")).notes;
const beats = M.bars.flatMap((b) => b.beats.slice(0, -1));
const g8 = [];
for (let i = 0; i < beats.length - 1; i++) g8.push(beats[i], (beats[i] + beats[i + 1]) / 2);
const near = (t) => g8.reduce((p, c) => (Math.abs(c - t) < Math.abs(p - t) ? c : p));

// anchors: (source time → locked time), monotonic, spaced
const anchors = [[0, 0]];
let moved = [];
for (const v of V) {
  const d = near(v.t) - v.t;
  const shift = Math.max(-MAX, Math.min(MAX, d * STRENGTH));
  const src = v.t; let dst = v.t + shift;
  const [ps, pd] = anchors.at(-1);
  dst = Math.max(pd + RATIO_MIN * (src - ps), Math.min(pd + RATIO_MAX * (src - ps), dst));   // local ratio clamp
  if (src - ps < MIN_GAP || dst - pd < MIN_GAP) continue;   // keep it monotonic + smooth
  anchors.push([src, dst]);
  moved.push(Math.abs(d) * 1000);
}
const dur = 179.14;
{ const [ps, pd] = anchors.at(-1); anchors.push([dur, Math.max(pd + RATIO_MIN * (dur - ps), Math.min(pd + RATIO_MAX * (dur - ps), dur))]); }
{ const r = anchors.slice(1).map(([s, d], i) => (d - anchors[i][1]) / (s - anchors[i][0]));
  console.log(`local ratio ${Math.min(...r).toFixed(3)}–${Math.max(...r).toFixed(3)} (clamped to ${RATIO_MIN}–${RATIO_MAX})`); }
const mapPath = resolve(OUT, "timemap.txt");
writeFileSync(mapPath, anchors.map(([s, d]) => `${Math.round(s * SR)} ${Math.round(d * SR)}`).join("\n") + "\n");

// before/after check: onset distance to the 8th grid
const after = V.map((v) => {
  let k = anchors.findIndex(([s]) => s > v.t); if (k <= 0) k = 1;
  const [s0, d0] = anchors[k - 1], [s1, d1] = anchors[k];
  const t = d0 + ((v.t - s0) / (s1 - s0)) * (d1 - d0);
  return Math.abs(t - near(t)) * 1000;
}).sort((a, b) => a - b);
const before = V.map((v) => Math.abs(v.t - near(v.t)) * 1000).sort((a, b) => a - b);
const med = (a) => a[a.length >> 1].toFixed(0), p90 = (a) => a[Math.floor(0.9 * (a.length - 1))].toFixed(0);
console.log(`onsets vs 8th grid: median ${med(before)} → ${med(after)} ms, p90 ${p90(before)} → ${p90(after)} ms (${anchors.length - 2} anchors)`);

const stems = readdirSync(VOX).filter((f) => /^(vocals-aesthetivox|vocals-halo|harm-.*)\.wav$/.test(f));
for (const f of stems) {
  const r = spawnSync("rubberband", ["-3", "-F", "-q", "-D", String(dur), "-M", mapPath, resolve(VOX, f), resolve(OUT, f)], { encoding: "utf8" });
  if (r.status !== 0) { console.error(r.stderr); process.exit(1); }
  console.log(`  locked ${f}`);
}
