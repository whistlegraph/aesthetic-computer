#!/usr/bin/env node
// make-boot-chime.mjs — re-synthesize the AC-native boot/startup chime
// that opens trancenwaltz (recap/bin/trance.mjs fireBootMelody, which
// itself matches audio.c:audio_ready_melody). Three ascending triangle
// "beeps" — C5 → E5 → G5 — with a 60 ms gap between each, attack 3 ms,
// decay 0.6×dur. This is the "square beeps / startup chime" @jeffrey
// wants at the very top of the Amazing Grace vowels track, BEFORE any
// voice. Pure synthesis, no samples.
//
// Writes: pop/big-pictures/out/boot-chime.wav  (mono 48k, ~0.6 s)
//
// Usage:
//   node pop/big-pictures/bin/make-boot-chime.mjs [--gain 0.9] [--out path]

import { execSync } from "node:child_process";
import { writeFileSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");

const argv = process.argv.slice(2);
function flag(name, def) {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : def;
}
const GAIN = parseFloat(flag("gain", "0.9"));
const OUT  = flag("out", resolve(POP, "big-pictures/out/boot-chime.wav"));
mkdirSync(dirname(OUT), { recursive: true });

const SR = 48000;
// Matches fireBootMelody: triangle C5/E5/G5, gaps 60 ms.
const NOTES = [
  { tone: 523.25, dur: 0.15, vol: 0.70 }, // C5
  { tone: 659.25, dur: 0.15, vol: 0.70 }, // E5
  { tone: 783.99, dur: 0.20, vol: 0.80 }, // G5
];
const GAP = 0.060;

const totalSec = NOTES.reduce((s, n) => s + n.dur + GAP, 0) + 0.10;
const N = Math.ceil(totalSec * SR);
const buf = new Float32Array(N);

// bandlimited-ish triangle via the standard 2/pi*asin(sin) shape — keeps
// it soft (few harmonics), which is exactly the "beep" character.
function triangle(phase) {
  return (2 / Math.PI) * Math.asin(Math.sin(2 * Math.PI * phase));
}

let cursor = 0; // seconds
for (const n of NOTES) {
  const start = Math.round(cursor * SR);
  const len = Math.round(n.dur * SR);
  const atk = Math.round(0.003 * SR);
  const dec = n.dur * 0.6;
  for (let i = 0; i < len; i++) {
    const t = i / SR;
    const phase = n.tone * t;
    let env;
    if (i < atk) env = i / atk;
    else env = Math.exp(-(t - 0.003) / dec);
    const s = triangle(phase) * env * n.vol * GAIN;
    const idx = start + i;
    if (idx < N) buf[idx] += s;
  }
  cursor += n.dur + GAP;
}

// write a temp raw f32 → let ffmpeg wrap to 16-bit pcm wav (mono 48k)
const raw = Buffer.alloc(N * 4);
for (let i = 0; i < N; i++) raw.writeFloatLE(Math.max(-1, Math.min(1, buf[i])), i * 4);
const TMP = "/tmp/boot-chime.f32";
writeFileSync(TMP, raw);
execSync(
  `ffmpeg -y -loglevel error -f f32le -ar ${SR} -ac 1 -i "${TMP}" ` +
  `-ar ${SR} -ac 1 -c:a pcm_s16le "${OUT}"`
);
const dur = parseFloat(execSync(
  `ffprobe -v error -show_entries format=duration -of csv=p=0 "${OUT}"`
).toString().trim());
console.log(`✓ ${OUT.replace(POP + "/", "pop/")} (${dur.toFixed(2)}s) — C5·E5·G5 boot chime`);
