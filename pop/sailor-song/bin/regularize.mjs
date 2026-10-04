#!/usr/bin/env node
// regularize.mjs — damp her tempo curve once the floor arrives.
//
// Her clock swings 113 → 130 BPM (ANALYSIS.md §3). In the bedroom opening
// that rubato is the performance and is left alone. From FLOOR_BAR on, the
// dance floor wants a steadier pulse: each beat's local BPM is smoothed over
// ±8 beats and pulled KEEP of the way from the median — so a third of her
// sway remains, and the stretch ratio never leaves ±6%, which rubberband R3
// handles on a voice without artefacts. One time map (beat → beat) is run
// through rubberband on every stem the engine plays (the locked lead, halo
// and harmonies, her guitar, the replay guitars), and the same map produces
// measures.reg.json for the chart. Timing only; pitch is untouched.
//
//   node pop/sailor-song/bin/regularize.mjs [--keep 0.3] [--floor-bar 24] [--jobs 4]
//     → src/vox/reg/*.wav + src/vox/reg/timemap.txt + measures.reg.json

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const VOX = resolve(LANE, "src/vox");
const OUT = resolve(VOX, "reg");
mkdirSync(OUT, { recursive: true });
const SR = 48_000;

const flags = {};
for (let i = 2; i < process.argv.length; i++) { const a = process.argv[i]; if (a.startsWith("--")) flags[a.slice(2)] = process.argv[++i]; }
const KEEP = Number(flags.keep ?? 0.3);
const FLOOR_BAR = Number(flags["floor-bar"] ?? 24);
const JOBS = Number(flags.jobs ?? 4);
const WIN = 8;
// v19: the song kicks off at verse 2 — every bar that plays after verse 1 in the record
// (source bars 28 on: verse 2, both choruses, break, bridge, outro) runs LIFT faster
const LIFT = Number(flags.lift ?? 1.05), LIFT_BAR = Number(flags["lift-bar"] ?? 28);

const M = JSON.parse(readFileSync(resolve(LANE, "measures.json"), "utf8"));
const bars = M.bars;
// the full beat list: every bar's beats without its closing downbeat, plus the final end
const src = bars.flatMap((b) => b.beats.slice(0, -1));
src.push(bars.at(-1).beats.at(-1));
const barOfBeat = []; bars.forEach((b) => { for (let j = 0; j < b.beats.length - 1; j++) barOfBeat.push(b.n); }); barOfBeat.push(bars.at(-1).n);

const ibi = src.slice(1).map((t, i) => t - src[i]);
const bpm = ibi.map((d) => 60 / d);
const sorted = [...bpm].sort((a, b) => a - b), median = sorted[sorted.length >> 1];
const smooth = bpm.map((_, i) => { const a = Math.max(0, i - WIN), z = Math.min(bpm.length, i + WIN + 1); return bpm.slice(a, z).reduce((s, v) => s + v, 0) / (z - a); });
// how much of her sway to keep per beat: all of it before the floor, KEEP after, eased over 4 bars before FLOOR_BAR
const keepAt = (i) => { const n = barOfBeat[i]; if (n >= FLOOR_BAR) return KEEP; if (n < FLOOR_BAR - 4) return 1; const x = (n - (FLOOR_BAR - 4)) / 4; return 1 + (KEEP - 1) * (0.5 - 0.5 * Math.cos(Math.PI * x)); };
// v32: the lift EASES in over the four bars before LIFT_BAR (it was a 5.6 % step on the downbeat of 28 — "kiss me on
// the mouth" was squeezed 11 %), and no beat is ever squeezed more than SQUEEZE (6 %) against how she played it
const SQUEEZE = Number(flags.squeeze ?? 1.06);
const liftAt = (i) => { const n = barOfBeat[i]; if (n >= LIFT_BAR) return LIFT; if (n < LIFT_BAR - 4) return 1; const x = (n - (LIFT_BAR - 4)) / 4; return 1 + (LIFT - 1) * (0.5 - 0.5 * Math.cos(Math.PI * x)); };
let target = bpm.map((v, i) => { const k = keepAt(i); const t = (k >= 1 ? v : median + k * (smooth[i] - median)) * liftAt(i); return Math.min(t, v * SQUEEZE); });
// v53: FIT — a bar she stretched to five beats (27: "to me? … oh won't you") is compressed to four, so the chorus downbeat lands
// on "kiss" and the band's "1" is hers. Each of the bar's intervals is scaled by the same factor; the squeeze cap does not apply.
const FIT = String(flags["fit-bars"] ?? "").split(",").filter(Boolean).map(Number);   // v54: off by default — "there was never a problem with the 'oh won't you' timing"
for (const fb of FIT) { const idx = ibi.map((_, i) => i).filter((i) => barOfBeat[i] === fb); if (idx.length <= 4) continue;
  const raw = idx.reduce((a, i) => a + ibi[i], 0), nb = idx.length;
  const near = ibi.map((_, i) => i).filter((i) => (barOfBeat[i] === fb - 1 || barOfBeat[i] === fb + 1)); const beatT = near.reduce((a, i) => a + 60 / target[i], 0) / near.length;
  const want = 4 * beatT; for (const i of idx) target[i] = bpm[i] * (raw / want);
  console.log(`  fit bar ${fb}: ${nb} beats ${raw.toFixed(2)} s → 4 beats ${want.toFixed(2)} s (×${(want / raw).toFixed(3)})`); }
const dst = [src[0]];
for (let i = 0; i < target.length; i++) dst.push(dst[i] + 60 / target[i]);
const ratio = ibi.map((d, i) => (60 / target[i]) / d);
const dur = 179.14;
const total = dst.at(-1) + (dur - src.at(-1));

// the map, in samples; identity before the first beat and after the last
const pairs = [[0, 0], ...src.map((s, i) => [s, dst[i]]), [dur, total]];
const mapPath = resolve(OUT, "timemap.txt");
writeFileSync(mapPath, pairs.map(([s, d]) => `${Math.round(s * SR)} ${Math.round(d * SR)}`).join("\n") + "\n");
const warp = (t) => { let k = pairs.findIndex(([s]) => s > t); if (k <= 0) k = 1; const [s0, d0] = pairs[k - 1], [s1, d1] = pairs[k]; return d0 + ((t - s0) / (s1 - s0)) * (d1 - d0); };

// measures.reg.json: the same bars on the new clock
const reg = { ...M, regularized: { keep: KEEP, floorBar: FLOOR_BAR, medianBpm: Math.round(median * 10) / 10, totalSec: Math.round(total * 1000) / 1000 },
  beats: { ...M.beats, snapped: M.beats.snapped.map(warp) },
  bars: bars.map((b) => { const beats = b.beats.map(warp); const t = beats[0], d = beats.at(-1) - t; return { ...b, t: +t.toFixed(3), dur: +d.toFixed(3), beats: beats.map((x) => +x.toFixed(3)), bpm: +(60 * (beats.length - 1) / d).toFixed(1) }; }),
  strumList: M.strumList.map((s) => ({ ...s, t: +warp(s.t).toFixed(3) })) };
writeFileSync(resolve(LANE, "measures.reg.json"), JSON.stringify(reg, null, 1));

const sec = (a, z) => { const r = reg.bars.filter((b) => b.n >= a && b.n <= z).map((b) => b.bpm).sort((x, y) => x - y); return r[r.length >> 1]; };
const was = (a, z) => { const r = bars.filter((b) => b.n >= a && b.n <= z).map((b) => b.bpm).sort((x, y) => x - y); return r[r.length >> 1]; };
console.log(`median ${median.toFixed(1)} BPM · keep ${KEEP} from bar ${FLOOR_BAR} · stretch ratio ${Math.min(...ratio).toFixed(3)}–${Math.max(...ratio).toFixed(3)} · ${dur.toFixed(1)} → ${total.toFixed(1)} s`);
for (const [n, a, z] of [["intro", 1, 10], ["verse1", 11, 27], ["chorus1", 28, 43], ["verse2", 44, 51], ["chorus2", 52, 67], ["break", 68, 71], ["bridge", 72, 80], ["outro", 81, 84]]) console.log(`  ${n.padEnd(8)} ${was(a, z)} → ${sec(a, z)}`);

// stems through the same map
// v17: the voice stems are taken UNLOCKED (src/vox/*.wav) — the lock's stretch put a
// pre-echo on hard consonants ("k-kiss"); her placement stays as sung. The lock map is
// written as identity so every downstream tool composes the same way.
const stems = ["vocals-natural.wav", "vocals-aesthetivox.wav", "vocals-halo.wav", "harm-up3.wav", "harm-down3.wav", "harm-up5.wav", "harm-down6.wav", "harm-down8.wav", "harm-up8.wav", "guitar-48k.wav", "replay-acoustic.wav", "replay-electric.wav"].filter((f) => existsSync(resolve(VOX, f)));
mkdirSync(resolve(VOX, "locked"), { recursive: true });
writeFileSync(resolve(VOX, "locked/timemap.txt"), `0 0\n${Math.round(dur * SR)} ${Math.round(dur * SR)}\n`);
const queue = [...stems];
let running = 0, failed = false;
await new Promise((done) => {
  const next = () => {
    if (failed) return done();
    if (!queue.length) { if (!running) done(); return; }
    const f = queue.shift(); running++;
    const out = resolve(OUT, f.split("/").pop());
    const p = spawn("rubberband", ["-3", "-F", "-q", "-D", total.toFixed(4), "-M", mapPath, resolve(VOX, f), out], { stdio: ["ignore", "ignore", "pipe"] });
    let err = ""; p.stderr.on("data", (d) => (err += d));
    p.on("close", (code) => { running--; if (code !== 0) { console.error(`rubberband failed on ${f}\n${err}`); failed = true; } else console.log(`  reg ${f}`); next(); });
  };
  for (let i = 0; i < JOBS; i++) next();
});
if (failed) process.exit(1);
console.log(`✓ ${OUT}`);
