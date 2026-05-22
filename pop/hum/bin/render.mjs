#!/usr/bin/env node
// render.mjs — the "hum" lane: a tiny singable melody, rendered.
//
// A testbed for the RFA voice-take workflow. The melody + words live in
// voice-takes/manifest.json. This renderer plays the tune as a clear
// sustained sine lead over a soft chord pad, and — for every note that
// has a recorded take (voice-takes/<id>.wav, captured by
// `node pop/bin/rfa.mjs --track hum`) — mixes @jeffrey's actual voice
// in on top, ducking the synth lead under it. So the track is always
// playable and becomes "real" one sung note at a time.
//
// Usage: node pop/hum/bin/render.mjs [--play]
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const PLAY = process.argv.includes("--play");
const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const TAKES = resolve(LANE, "voice-takes");
const OUTDIR = resolve(LANE, "out");
const SR = 48_000;
const TAU = Math.PI * 2;
const m2f = (m) => 440 * Math.pow(2, (m - 69) / 12);

const manifest = JSON.parse(readFileSync(resolve(TAKES, "manifest.json"), "utf8"));
const SPB = 60 / manifest.bpm;
const notes = manifest.notes;

// ── chord bed — D natural minor, one triad per phrase ─────────────────
const CHORDS = [
  { beat: 0,  name: "Dm", midis: [50, 53, 57] },
  { beat: 8,  name: "Bb", midis: [46, 50, 53] },
  { beat: 18, name: "F",  midis: [53, 57, 60] },
  { beat: 28, name: "Dm", midis: [50, 53, 57] },
];
const TOTAL_BEATS = 44;                        // 40 melody + 4 tail
const N = Math.ceil((TOTAL_BEATS * SPB + 1.0) * SR);
const buf = new Float32Array(N);

function add(t, dur, fn) {
  let i = Math.max(0, Math.floor(t * SR));
  const e = Math.min(N, Math.ceil((t + dur) * SR));
  for (; i < e; i++) buf[i] += fn(i / SR - t);
}

// soft sustained chord pad — background
function pad(t, dur, midi, gain) {
  const f = m2f(midi);
  add(t, dur, (lt) => {
    let env = Math.min(1, lt / 0.4);
    if (lt > dur - 0.6) env *= Math.max(0, (dur - lt) / 0.6);
    const x = Math.sin(TAU * f * lt) + 0.3 * Math.sin(TAU * f * 2 * lt);
    return Math.tanh(x * 0.6) * env * gain;
  });
}

// clear sustained melody lead — the tune to sing along to
function lead(t, dur, midi, gain) {
  const f = m2f(midi);
  add(t, dur, (lt) => {
    let env = Math.min(1, lt / 0.03);
    if (lt > dur - 0.12) env *= Math.max(0, (dur - lt) / 0.12);
    const vib = 1 + Math.sin(TAU * 5 * lt) * 0.004 * Math.min(1, lt / 0.2);
    const x = Math.sin(TAU * f * vib * lt) + 0.25 * Math.sin(TAU * f * 2 * lt)
            + 0.1 * Math.sin(TAU * f * 3 * lt);
    return Math.tanh(x * 0.7) * env * gain;
  });
}

// soft beat click — keeps time
function click(t, gain) {
  add(t, 0.05, (lt) => Math.sin(TAU * 2000 * lt) * Math.exp(-lt / 0.012) * gain);
}

// ── wav loader — mono float @ SR, silence-trimmed, peak-normalized ────
function loadWav(path) {
  const b = readFileSync(path);
  let p = 12, fmt = null, dOff = 0, dLen = 0;
  while (p + 8 <= b.length) {
    const id = b.toString("ascii", p, p + 4);
    const sz = b.readUInt32LE(p + 4);
    if (id === "fmt ") fmt = { format: b.readUInt16LE(p + 8), ch: b.readUInt16LE(p + 10),
      sr: b.readUInt32LE(p + 12), bits: b.readUInt16LE(p + 22) };
    else if (id === "data") { dOff = p + 8; dLen = sz; }
    p += 8 + sz + (sz & 1);
  }
  if (!fmt || !dOff) throw new Error(`bad WAV: ${path}`);
  const fb = (fmt.bits / 8) * fmt.ch, frames = Math.floor(dLen / fb);
  let mono = new Float32Array(frames);
  for (let i = 0; i < frames; i++) {
    let acc = 0;
    for (let c = 0; c < fmt.ch; c++) {
      const o = dOff + i * fb + c * (fmt.bits / 8);
      if (fmt.format === 3 && fmt.bits === 32) acc += b.readFloatLE(o);
      else if (fmt.bits === 16) acc += b.readInt16LE(o) / 32768;
      else if (fmt.bits === 24) acc += (b.readUInt8(o) | (b.readUInt8(o + 1) << 8) | (b.readInt8(o + 2) << 16)) / 8388608;
      else if (fmt.bits === 32) acc += b.readInt32LE(o) / 2147483648;
    }
    mono[i] = acc / fmt.ch;
  }
  if (fmt.sr !== SR) {
    const outN = Math.round(frames * SR / fmt.sr), rs = new Float32Array(outN);
    for (let i = 0; i < outN; i++) {
      const x = i * fmt.sr / SR, i0 = Math.floor(x), fr = x - i0;
      rs[i] = (mono[i0] || 0) + ((mono[i0 + 1] || 0) - (mono[i0] || 0)) * fr;
    }
    mono = rs;
  }
  let a = 0, e = mono.length; const TH = 0.02;
  while (a < e && Math.abs(mono[a]) < TH) a++;
  while (e > a && Math.abs(mono[e - 1]) < TH) e--;
  mono = mono.subarray(a, e);
  let pk = 0; for (let i = 0; i < mono.length; i++) pk = Math.max(pk, Math.abs(mono[i]));
  if (pk > 0) for (let i = 0; i < mono.length; i++) mono[i] /= pk;
  return mono;
}

function placeSample(t, samp, gain) {
  const start = Math.floor(t * SR), fadeN = Math.floor(0.015 * SR);
  for (let k = 0; k < samp.length; k++) {
    const di = start + k;
    if (di < 0 || di >= N) continue;
    let s = samp[k];
    if (k < fadeN) s *= k / fadeN;
    if (samp.length - k < fadeN) s *= (samp.length - k) / fadeN;
    buf[di] += s * gain;
  }
}

// ── render ────────────────────────────────────────────────────────────
for (let c = 0; c < CHORDS.length; c++) {
  const ch = CHORDS[c];
  const endBeat = c + 1 < CHORDS.length ? CHORDS[c + 1].beat : TOTAL_BEATS;
  for (const m of ch.midis) pad(ch.beat * SPB, (endBeat - ch.beat) * SPB, m, 0.05);
}
for (let b = 0; b < TOTAL_BEATS; b++) click(b * SPB, b % 4 === 0 ? 0.06 : 0.035);

let takeCount = 0;
for (const n of notes) {
  const takePath = resolve(TAKES, `${n.id}.wav`);
  const hasTake = existsSync(takePath);
  lead(n.startSec, n.durSec, n.midi, hasTake ? 0.10 : 0.22);   // duck lead under a voice take
  if (hasTake) { placeSample(n.startSec, loadWav(takePath), 0.95); takeCount++; }
}

// ── master + write ────────────────────────────────────────────────────
let peak = 0;
for (let i = 0; i < N; i++) peak = Math.max(peak, Math.abs(buf[i]));
const g = peak > 0 ? 0.9 / peak : 1;
mkdirSync(OUTDIR, { recursive: true });
const wavPath = resolve(OUTDIR, "hum.wav");
const w = Buffer.alloc(44 + N * 2);
w.write("RIFF", 0); w.writeUInt32LE(36 + N * 2, 4); w.write("WAVE", 8);
w.write("fmt ", 12); w.writeUInt32LE(16, 16); w.writeUInt16LE(1, 20);
w.writeUInt16LE(1, 22); w.writeUInt32LE(SR, 24); w.writeUInt32LE(SR * 2, 28);
w.writeUInt16LE(2, 32); w.writeUInt16LE(16, 34);
w.write("data", 36); w.writeUInt32LE(N * 2, 40);
for (let i = 0; i < N; i++) {
  const s = Math.tanh(buf[i] * g * 1.02);
  w.writeInt16LE(Math.max(-32768, Math.min(32767, Math.round(s * 32767))), 44 + i * 2);
}
writeFileSync(wavPath, w);
const mp3Path = resolve(OUTDIR, "hum.mp3");
spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wavPath,
  "-af", "loudnorm=I=-16:TP=-1.5", "-b:a", "320k", mp3Path], { stdio: "ignore" });

console.log(`hum · ${manifest.bpm} BPM · ${notes.length} notes · ` +
  `${takeCount}/${notes.length} sung · ${(N / SR).toFixed(1)}s`);
console.log(`→ ${wavPath}`);
if (takeCount === 0)
  console.log(`  (no voice takes yet — synth lead only. sing: ` +
    `node pop/bin/rfa.mjs --track hum)`);
if (PLAY) spawnSync("afplay", [wavPath], { stdio: "ignore" });
