#!/usr/bin/env node
// gen-click.mjs — the imab click track and singing guide.
//
// Click: 2 bars of count-in, then 64 bars. Beat 1 accented, a brighter
// door tick opens every 8-bar phrase. Guide: the same click with the
// imab hook (xylophone) and bass roots looping, to practice and cut
// takes against. Score source of truth: pop/imab/imab.np.
//
//   node pop/imab/bin/gen-click.mjs [--bpm 124]
//
// Writes out/imab-click-<bpm>.{wav,mp3} and out/imab-guide-<bpm>.{wav,mp3}
// (juke-sync carries out/*.mp3 to the room library).

import { mixEventMarimba } from "../../marimba/synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
mkdirSync(OUT, { recursive: true });
const SR = 48_000;
const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 124;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const COUNT_BARS = 2, BODY_BARS = 64;
const TOTAL = (COUNT_BARS + BODY_BARS) * BAR + 2.0;
const NS = Math.ceil(TOTAL * SR);

// ── the hook, straight from imab.np (bar, beat, midi, durBeats) ───────
// The MEASURED melody (imab.np — corpus-derived, 35 takes): tonic chant
// on A3, octave flare on "-PING", 4th→3rd→2nd walk through "costume".
const HOOK = [
  [1, 1, 57, 0.5], [1, 1.75, 57, 0.25], [1, 2, 57, 1], [1, 3, 57, 1], [1, 4, 57, 1],
  [2, 1.5, 57, 0.5], [2, 2.5, 69, 1], [2, 3.5, 57, 0.5], [2, 4, 57, 0.5], [2, 4.5, 57, 0.5],
  [3, 2.5, 62, 0.5], [3, 4, 62, 0.5], [3, 4.5, 61, 0.5],
  [4, 1, 59, 1.5], [4, 3, 59, 0.5], [4, 3.5, 59, 0.5], [4, 4, 57, 0.5], [4, 4.5, 57, 0.5],
  [5, 1, 56, 0.5], [5, 1.5, 57, 1.5],
];
const ROOTS = [45, 45, 50, 50, 45, 45, 45, 45]; // A | A | D | D | A…  (imab.np)

// ── tick voice ────────────────────────────────────────────────────────
function tick(buf, t0, freq, gain) {
  const n = Math.floor(0.030 * SR), a = Math.floor(t0 * SR);
  for (let i = 0; i < n && a + i < buf.length; i++) {
    const t = i / SR;
    const s = Math.tanh(1.6 * Math.sin(2 * Math.PI * freq * t) * Math.exp(-t / 0.005));
    buf[a + i] += s * gain;
  }
}
function clickBed() {
  const buf = new Float32Array(NS);
  for (let b = 0; b < COUNT_BARS * 4; b++)
    tick(buf, b * BEAT, 2200, b % 4 === 0 ? 0.65 : 0.5);
  for (let bar = 0; bar < BODY_BARS; bar++) {
    const t0 = (COUNT_BARS + bar) * BAR;
    tick(buf, t0, bar % 8 === 0 ? 2600 : 1700, bar % 8 === 0 ? 0.7 : 0.55);
    for (let beat = 1; beat < 4; beat++) tick(buf, t0 + beat * BEAT, 1100, 0.32);
  }
  return buf;
}

// ── renders ───────────────────────────────────────────────────────────
function encode(mono, name) {
  const st = new Float32Array(NS * 2);
  for (let i = 0; i < NS; i++) { st[2 * i] = mono[i]; st[2 * i + 1] = mono[i]; }
  const raw = resolve(OUT, `.${name}.raw`);
  writeFileSync(raw, Buffer.from(st.buffer));
  for (const [ext, args] of [["wav", ["-c:a", "pcm_s16le"]], ["mp3", ["-c:a", "libmp3lame", "-q:a", "2"]]]) {
    const dest = resolve(OUT, `${name}.${ext}`);
    const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", raw,
      "-metadata", `title=${name}`, "-metadata", "artist=Whistlegraph Dot Org",
      ...args, dest], { stdio: "inherit" });
    if (ff.status !== 0) process.exit(1);
    console.log(`✓ ${dest}`);
  }
  unlinkSync(raw);
}

const click = clickBed();
encode(click, `imab-click-${BPM}`);

const guide = Float32Array.from(click);
for (let cyc = 0; cyc < BODY_BARS / 8; cyc++) {
  const base = COUNT_BARS + cyc * 8;
  for (const [bar, beat, midi, durB] of HOOK)
    mixEventMarimba({ startSec: (base + bar - 1) * BAR + (beat - 1) * BEAT, midi,
      durSec: durB * BEAT, gain: 0.9, preset: "xylophone", decayMul: 1.1 }, guide, { sampleRate: SR });
  for (let bar = 0; bar < 8; bar++)
    mixEventMarimba({ startSec: (base + bar) * BAR, midi: ROOTS[bar],
      durSec: 2 * BEAT, gain: 0.7, preset: "bass", decayMul: 0.8 }, guide, { sampleRate: SR });
}
let pk = 0; for (let i = 0; i < NS; i++) pk = Math.max(pk, Math.abs(guide[i]));
if (pk > 0.9) for (let i = 0; i < NS; i++) guide[i] *= 0.9 / pk;
encode(guide, `imab-guide-${BPM}`);
