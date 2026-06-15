#!/usr/bin/env node
// cornerfive.mjs — a /pop remix of the whistlegraph "five in the corner x2".
//
// The whistled TikTok (src/whistle.wav, analyzed in src/whistle.analysis.json)
// is the topline — the "vocal" — and everything under it is composed
// bottom-up from AC instruments: a downtempo boom-bap bed in B major /
// G# minor at 73.2 BPM, chosen so four bars (13.11s) land exactly on the
// length of one whistle pass, for seamless looping.
//
// Bed instruments (all synthesized here, no samples but the whistle):
//   • kick   — pitched sine drop + click
//   • clap   — layered noise bursts on the 2 & 4 backbeat
//   • hat    — high-passed noise, 8ths with offbeat accent
//   • bass   — lowpassed saw, syncopated roots G#–E–B–F#
//   • pad    — detuned-saw triads (vi–IV–I–V), slow attack
//   • bells  — sine arpeggio sparkle in the back half
// The whistle gets a high-pass, slap delay, and a Schroeder reverb, and
// is sidechain-ducked under the kick so it breathes with the groove.
//
// Usage:  node bin/cornerfive.mjs   (writes out/cornerfive.mp3)

import { readWavMono } from "../../lib/wav.mjs";
import { spawnSync } from "node:child_process";
import { writeFileSync, readFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const SRC_SYNCED = resolve(ROOT, "src/whistle-synced.wav"); // rubberband grid+key
const SRC_NOTES  = resolve(ROOT, "src/whistle-synced.notes.json"); // the melody
const SRC_RAW    = resolve(ROOT, "src/whistle.wav");        // the real recording
const OUT_WAV = resolve(ROOT, "out/cornerfive.wav");
const OUT_MP3 = resolve(ROOT, "out/cornerfive.mp3");

// Default to the SYNCED + tuned whistle (rubberband, pitch-preserving).
// Pass --raw to use the untouched original recording instead.
const WANT_RAW = process.argv.includes("--raw");

const SR  = 44100;
const BPM = 73.2;
const BEAT = 60 / BPM;          // 0.8197 s
const BAR  = 4 * BEAT;          // 3.2787 s
const STEP = BAR / 16;          // 16th note
const TARGET_SEC = 84;          // 1:24
const BARS = Math.ceil(TARGET_SEC / BAR);
const N = Math.ceil(TARGET_SEC * SR);
const mix = new Float32Array(N);

// ── tiny DSP toolkit ───────────────────────────────────────────────────
const TAU = Math.PI * 2;
let seed = 0x5f3759df;
function rnd() { seed ^= seed << 13; seed ^= seed >>> 17; seed ^= seed << 5; return ((seed >>> 0) / 0xffffffff) * 2 - 1; }
const clamp = (x, a, b) => (x < a ? a : x > b ? b : x);
const softclip = (x) => Math.tanh(x);
function add(buf, pos, val) { const i = pos | 0; if (i >= 0 && i < buf.length) buf[i] += val; }

// add a windowed voice rendered by fn(t) -> sample, with AD envelope
function voice(buf, startSec, durSec, attack, release, gain, fn) {
  const s0 = Math.floor(startSec * SR);
  const len = Math.floor(durSec * SR);
  const aN = Math.max(1, Math.floor(attack * SR));
  const rN = Math.max(1, Math.floor(release * SR));
  for (let k = 0; k < len; k++) {
    const t = k / SR;
    let env = 1;
    if (k < aN) env = k / aN;
    else if (k > len - rN) env = Math.max(0, (len - k) / rN);
    add(buf, s0 + k, fn(t) * env * gain);
  }
}

const noteHz = (m) => 440 * Math.pow(2, (m - 69) / 12);

// ── kick ─────────────────────────────────────────────────────────────────
function kick(buf, at, gain = 1) {
  voice(buf, at, 0.40, 0.001, 0.34, gain, (t) => {
    const f = 165 * Math.exp(-t * 38) + 48;             // snappy drop → deep sub
    const click = Math.exp(-t * 1300) * rnd() * 0.7;    // beater click
    const body = Math.sin(TAU * f * t + 7 * Math.exp(-t * 55));
    return softclip(body * 1.5) + click;                // saturated punch
  });
}

// ── sine bell — pure sine + soft octave, bell decay (the "sister" line) ──
function bell(buf, at, midi, gain = 0.3) {
  const f = noteHz(midi);
  voice(buf, at, 1.3, 0.003, 1.25, gain, (t) => {
    const e = Math.exp(-t * 3.0);
    return (Math.sin(TAU * f * t) + 0.3 * Math.sin(TAU * 2 * f * t) * Math.exp(-t * 6)) * e;
  });
  // gentle sub an octave down for a little low-end body (not boomy)
  voice(buf, at, 0.9, 0.003, 0.85, gain * 0.35, (t) => Math.sin(TAU * (f / 2) * t) * Math.exp(-t * 4));
}

// ── kick across the whole track ─────────────────────────────────────────
const KICK = [1,0,0,0, 1,0,0,0, 1,0,1,0, 1,0,0,0]; // four-on-floor + push on "and of 3"
const kickTimes = [];
for (let bar = 0; bar < BARS; bar++) {
  for (let s = 0; s < 16; s++) {
    if (!KICK[s]) continue;
    const at = bar * BAR + s * STEP;
    if (at >= TARGET_SEC) break;
    kick(mix, at, 1.0); kickTimes.push(at);
  }
}

// ── the whistle "vocal": tuned → time-snapped → up-front reverb ─────────
const useSynced = !WANT_RAW && existsSync(SRC_SYNCED);
console.log(useSynced
  ? "→ loading SYNCED whistle (rubberband grid-sync + autotuned to its own melody)…"
  : "→ loading ORIGINAL whistle (true to the recording)…");
const { samples: wSrc, sampleRate: wSr } = readWavMono(useSynced ? SRC_SYNCED : SRC_RAW);
// resample to SR if needed (linear)
let wSamples = wSrc;
if (wSr !== SR) {
  const ratio = SR / wSr, out = new Float32Array(Math.floor(wSrc.length * ratio));
  for (let i = 0; i < out.length; i++) {
    const x = i / ratio, i0 = x | 0, f = x - i0;
    out[i] = (wSrc[i0] || 0) * (1 - f) + (wSrc[i0 + 1] || 0) * f;
  }
  wSamples = out;
}

// The tuned whistle is already time-warped onto the grid + pitch-snapped by
// bin/tune-whistle.mjs — one continuous stream, no slicing here (that's what
// kept it buttery instead of choppy). Just place it as-is.
const w = wSamples;

// normalize + high-pass (rumble out) + high-shelf air (clarity / presence)
let peak = 0; for (const v of w) peak = Math.max(peak, Math.abs(v));
const wg = peak > 0 ? 0.9 / peak : 1;
const wet = new Float32Array(w.length);
{
  let prev = 0, hp = 0, lp = 0;
  const a = Math.exp(-TAU * 250 / SR);
  for (let i = 0; i < w.length; i++) {
    const x = w[i] * wg;
    hp = a * (hp + x - prev); prev = x;   // one-pole high-pass ~250 Hz
    lp += 0.25 * (hp - lp);               // split lows/highs
    const high = hp - lp;                 // upper band
    wet[i] = hp + 0.7 * high;             // +high-shelf → clearer, more present
  }
}
// Schroeder reverb (4 combs + 2 allpass), summed wet+dry
function reverb(input, mixWet = 0.32) {
  const combs = [1116, 1188, 1277, 1356].map((d) => Math.floor(d * SR / 44100));
  const fb = 0.78;
  const allp = [556, 441].map((d) => Math.floor(d * SR / 44100));
  const cb = combs.map((d) => new Float32Array(d));
  const cbi = combs.map(() => 0);
  const ab = allp.map((d) => new Float32Array(d));
  const abi = allp.map(() => 0);
  const out = new Float32Array(input.length + SR); // tail
  for (let i = 0; i < out.length; i++) {
    const x = i < input.length ? input[i] : 0;
    let acc = 0;
    for (let c = 0; c < cb.length; c++) {
      const buf = cb[c]; const idx = cbi[c];
      const y = buf[idx]; buf[idx] = x + y * fb; cbi[c] = (idx + 1) % buf.length; acc += y;
    }
    acc /= cb.length;
    for (let a = 0; a < ab.length; a++) {
      const buf = ab[a]; const idx = abi[a];
      const y = buf[idx]; const z = -0.5 * acc + y; buf[idx] = acc + 0.5 * y;
      abi[a] = (idx + 1) % buf.length; acc = z;
    }
    out[i] = (i < input.length ? input[i] : 0) * (1 - mixWet) + acc * mixWet;
  }
  return out;
}
const whistle = reverb(wet, 0.10);  // nearly dry → clearest
// slap delay (one tap, ~1/8 note) — very subtle
const slap = Math.floor(STEP * 2 * SR);
for (let i = whistle.length - 1; i >= slap; i--) whistle[i] += whistle[i - slap] * 0.09;

// sidechain envelope: the VOCAL ducks under every kick, so the whistle
// pumps with the beat. 1 = open, dips to 0.4 on each kick then recovers.
const duck = new Float32Array(N).fill(1);
const dDur = Math.floor(0.18 * SR);
for (const kt of kickTimes) {
  const s0 = Math.floor(kt * SR);
  for (let k = 0; k < dDur; k++) {
    const i = s0 + k; if (i >= N) break;
    const d = 0.40 + 0.60 * (k / dDur);  // dip to -8dB then recover
    if (d < duck[i]) duck[i] = d;
  }
}

// loop the whistle through the WHOLE track, ducked under the kick; the sine
// bells follow the same melody, an octave-spanning "sister" line.
const loopLen = w.length / SR;             // one synced pass (~13.1s)
const melody = useSynced && existsSync(SRC_NOTES)
  ? JSON.parse(readFileSync(SRC_NOTES, "utf8")).notes : [];
for (let t0 = 0; t0 < TARGET_SEC; t0 += loopLen) {
  const off = Math.floor(t0 * SR);
  for (let i = 0; i < whistle.length; i++) {
    const di = off + i; if (di >= N) break;
    add(mix, di, whistle[i] * 1.2 * duck[di]);
  }
  for (const nt of melody) {               // sister bells track the vocal
    const at = t0 + nt.startSec;
    if (at >= TARGET_SEC) break;
    bell(mix, at, nt.midi, 0.26);
  }
}

// ── master: soft-clip, normalize, write WAV ─────────────────────────────
let mpeak = 0; for (let i = 0; i < N; i++) { mix[i] = softclip(mix[i] * 1.1); mpeak = Math.max(mpeak, Math.abs(mix[i])); }
const norm = mpeak > 0 ? 0.97 / mpeak : 1;
// gentle fade in/out
const fadeN = Math.floor(0.5 * SR);
function writeWav(path, data) {
  const n = data.length, buf = Buffer.alloc(44 + n * 2);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * 2, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 2, 28);
  buf.writeUInt16LE(2, 32); buf.writeUInt16LE(16, 34); buf.write("data", 36); buf.writeUInt32LE(n * 2, 40);
  for (let i = 0; i < n; i++) {
    let v = data[i] * norm;
    if (i < fadeN) v *= i / fadeN;
    if (i > n - fadeN) v *= (n - i) / fadeN;
    buf.writeInt16LE(Math.round(clamp(v, -1, 1) * 32767), 44 + i * 2);
  }
  writeFileSync(path, buf);
}
writeWav(OUT_WAV, mix);
console.log(`✓ wav → ${OUT_WAV.replace(process.env.HOME, "~")} (${(N / SR).toFixed(1)}s)`);

// ── encode mp3 (loudnorm to streaming target) ───────────────────────────
console.log("→ ffmpeg → mp3…");
const r = spawnSync("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y", "-i", OUT_WAV,
  "-af", "loudnorm=I=-11:TP=-1.0:LRA=11",
  "-c:a", "libmp3lame", "-b:a", "320k", OUT_MP3,
], { stdio: "inherit" });
if (r.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ mp3 → ${OUT_MP3.replace(process.env.HOME, "~")}`);
