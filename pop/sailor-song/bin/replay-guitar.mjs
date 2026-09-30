#!/usr/bin/env node
// replay-guitar.mjs — re-play her guitar part on pop/guitar/c/strum, bar
// for bar, so the arrangement can blend, double or replace her take.
//
// Everything comes from her: bar starts + lengths (measures.json, anchored
// on her strum motif), the chord she held, and her hand — X..XX..X in
// 8ths → "D..DD..U". Voicings are her actual capo-4 shapes, sounding:
//   Em shape  022000 → G#m  44 51 56 59 63 68
//   Cmaj7     x32000 + open low string → Emaj7/G#  44 52 56 59 63 68
//   G shape   320003 → B    47 51 54 59 63 71
// Each bar renders at its own tempo (a bar is whatever length she played)
// and overlap-adds at her downbeat; a short tail lets strings ring into
// the next bar's choke.
//
//   node pop/sailor-song/bin/replay-guitar.mjs
//     → src/vox/replay-acoustic.wav, src/vox/replay-electric.wav (48 k stereo)

import { readFileSync, writeFileSync, mkdirSync, rmSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const STRUM = resolve(LANE, "../guitar/c/strum");
const TMP = resolve(LANE, "src/replay-tmp");
const SR = 48_000;
mkdirSync(TMP, { recursive: true });

const M = JSON.parse(readFileSync(resolve(LANE, "measures.json"), "utf8"));
const TOTAL = 179.2;
const VOICING = {
  "G#m": "44,51,56,59,63,68",
  Emaj7: "44,52,56,59,63,68",
  B: "47,51,54,59,63,71",
};

function readF32(path) {
  const b = readFileSync(path);
  let p = 12, data;
  while (p < b.length) {
    const id = b.toString("ascii", p, p + 4), sz = b.readUInt32LE(p + 4);
    if (id === "data") data = b.subarray(p + 8, p + 8 + sz);
    p += 8 + sz + (sz & 1);
  }
  const n = data.length / 8, L = new Float32Array(n), R = new Float32Array(n);
  for (let i = 0; i < n; i++) { L[i] = data.readFloatLE(i * 8); R[i] = data.readFloatLE(i * 8 + 4); }
  return { L, R };
}
function writeF32(path, L, R) {
  const n = L.length, b = Buffer.alloc(44 + n * 8);
  b.write("RIFF", 0); b.writeUInt32LE(36 + n * 8, 4); b.write("WAVEfmt ", 8);
  b.writeUInt32LE(16, 16); b.writeUInt16LE(3, 20); b.writeUInt16LE(2, 22);
  b.writeUInt32LE(SR, 24); b.writeUInt32LE(SR * 8, 28); b.writeUInt16LE(8, 32);
  b.writeUInt16LE(32, 34); b.write("data", 36); b.writeUInt32LE(n * 8, 40);
  for (let i = 0; i < n; i++) { b.writeFloatLE(L[i], 44 + i * 8); b.writeFloatLE(R[i], 48 + i * 8); }
  writeFileSync(path, b);
}

for (const kind of ["acoustic", "electric"]) {
  const n = Math.ceil(TOTAL * SR), L = new Float32Array(n), R = new Float32Array(n);
  for (const bar of M.bars) {
    const nbeat = bar.beats.length - 1;
    // one 4/4 "bar" in strum's clock = this bar's real length
    const bpm = (60 * 4) / bar.dur;
    const pattern = nbeat === 4 ? "D..DD..U" : ("D..DD..U" + "D.").slice(0, 2 * nbeat).padEnd(2 * nbeat, ".");
    const out = resolve(TMP, `${kind}-${bar.n}.wav`);
    const args = ["--chord", VOICING[bar.chord] || VOICING["G#m"], "--pattern", pattern,
      "--bpm", bpm.toFixed(3), "--bars", "1", "--tail", "0.5", "--human", "0.25",
      "--force", "0.7", "--seed", String(bar.n), "--out", out];
    if (kind === "electric") args.push("--electric", "--drive", "0.25");
    const r = spawnSync(STRUM, args, { encoding: "utf8" });
    if (r.status !== 0) { console.error(r.stderr); process.exit(1); }
    const w = readF32(out), s0 = Math.floor(bar.t * SR);
    // strum peak-normalizes each render to 0.9; bring every bar to one level
    for (let i = 0; i < w.L.length && s0 + i < n; i++) { L[s0 + i] += w.L[i] * 0.5; R[s0 + i] += w.R[i] * 0.5; }
  }
  const dst = resolve(LANE, `src/vox/replay-${kind}.wav`);
  writeF32(dst, L, R);
  console.log(`✓ ${kind}: ${M.bars.length} bars → ${dst}`);
}
rmSync(TMP, { recursive: true, force: true });
