#!/usr/bin/env node
// alt.mjs — three ALTERNATE mixes of cornerfive, each made by a DIFFERENT
// method, all from the same synced whistle + melody as cornerfive.mjs.
//
//   dub       — SPACE: dotted-8th tape-delay throws + big plate reverb on
//               the whistle, half-time deep sub kick, sparse. nocturnal.
//   footwork  — CHOP:  a 16th-note rhythmic gate + ratchet retriggers chop
//               the vocal, over a fast syncopated juke kick + claps.
//   musicbox  — HARMONY: every whistle note is harmonized into stacked
//               fifths + octaves on a glassy FM bell, with a soft bass root.
//
// Usage:  node bin/alt.mjs <dub|footwork|musicbox>   (writes out/cornerfive-<mix>.mp3)
//         node bin/alt.mjs all                        (renders all three)

import { readWavMono } from "../../lib/wav.mjs";
import { spawnSync } from "node:child_process";
import { writeFileSync, readFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const SRC_SYNCED = resolve(ROOT, "src/whistle-synced.wav");
const SRC_NOTES  = resolve(ROOT, "src/whistle-synced.notes.json");

const SR = 44100;
const BPM = 73.2;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const STEP = BAR / 16;
const TARGET_SEC = 84;
const BARS = Math.ceil(TARGET_SEC / BAR);
const N = Math.ceil(TARGET_SEC * SR);

const TAU = Math.PI * 2;
let seed = 0x5f3759df;
function rnd() { seed ^= seed << 13; seed ^= seed >>> 17; seed ^= seed << 5; return ((seed >>> 0) / 0xffffffff) * 2 - 1; }
const clamp = (x, a, b) => (x < a ? a : x > b ? b : x);
const softclip = (x) => Math.tanh(x);
function add(buf, pos, val) { const i = pos | 0; if (i >= 0 && i < buf.length) buf[i] += val; }
const noteHz = (m) => 440 * Math.pow(2, (m - 69) / 12);

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

// ── drums ────────────────────────────────────────────────────────────────
function kick(buf, at, gain, deep) {
  const drop = deep ? 30 : 38, base = deep ? 42 : 48, dur = deep ? 0.55 : 0.40;
  voice(buf, at, dur, 0.001, dur * 0.85, gain, (t) => {
    const f = 165 * Math.exp(-t * drop) + base;
    const click = Math.exp(-t * 1300) * rnd() * 0.7;
    const body = Math.sin(TAU * f * t + 7 * Math.exp(-t * 55));
    return softclip(body * 1.5) + click;
  });
}
function clap(buf, at, gain) {
  for (let r = 0; r < 3; r++) {
    voice(buf, at + r * 0.011, 0.13, 0.001, 0.12, gain * (1 - r * 0.2), (t) => {
      const env = Math.exp(-t * 55);
      return rnd() * env;
    });
  }
}

// ── bells ──────────────────────────────────────────────────────────────────
function bell(buf, at, midi, gain, decay = 3.0, dur = 1.3) {
  const f = noteHz(midi);
  voice(buf, at, dur, 0.003, dur * 0.95, gain, (t) => {
    const e = Math.exp(-t * decay);
    return (Math.sin(TAU * f * t) + 0.3 * Math.sin(TAU * 2 * f * t) * Math.exp(-t * 6)) * e;
  });
  voice(buf, at, dur * 0.7, 0.003, dur * 0.65, gain * 0.35, (t) => Math.sin(TAU * (f / 2) * t) * Math.exp(-t * 4));
}
// glassy FM bell — music-box timbre (carrier + quick metallic partials)
function fmBell(buf, at, midi, gain) {
  const f = noteHz(midi);
  voice(buf, at, 1.6, 0.002, 1.5, gain, (t) => {
    const idx = 2.4 * Math.exp(-t * 7);
    const mod = idx * Math.sin(TAU * f * 3.0 * t);
    const e = Math.exp(-t * 4.2);
    return (Math.sin(TAU * f * t + mod) + 0.18 * Math.sin(TAU * 4 * f * t) * Math.exp(-t * 12)) * e;
  });
}
function bass(buf, at, midi, gain) {
  const f = noteHz(midi);
  voice(buf, at, 0.85, 0.004, 0.7, gain, (t) => softclip(Math.sin(TAU * f * t) * 1.4) * Math.exp(-t * 2.4));
}

// ── reverb (Schroeder) ─────────────────────────────────────────────────────
function reverb(input, mixWet) {
  const combs = [1116, 1188, 1277, 1356, 1422, 1491].map((d) => Math.floor(d * SR / 44100));
  const fb = 0.82;
  const allp = [556, 441, 341].map((d) => Math.floor(d * SR / 44100));
  const cb = combs.map((d) => new Float32Array(d)); const cbi = combs.map(() => 0);
  const ab = allp.map((d) => new Float32Array(d)); const abi = allp.map(() => 0);
  const out = new Float32Array(input.length + 2 * SR);
  for (let i = 0; i < out.length; i++) {
    const x = i < input.length ? input[i] : 0;
    let acc = 0;
    for (let c = 0; c < cb.length; c++) { const b = cb[c], idx = cbi[c]; const y = b[idx]; b[idx] = x + y * fb; cbi[c] = (idx + 1) % b.length; acc += y; }
    acc /= cb.length;
    for (let a = 0; a < ab.length; a++) { const b = ab[a], idx = abi[a]; const y = b[idx]; const z = -0.5 * acc + y; b[idx] = acc + 0.5 * y; abi[a] = (idx + 1) % b.length; acc = z; }
    out[i] = (i < input.length ? input[i] : 0) * (1 - mixWet) + acc * mixWet;
  }
  return out;
}

// ── shared whistle prep (normalize + high-pass + air) ─────────────────────
function prepWhistle() {
  const { samples: wSrc, sampleRate: wSr } = readWavMono(SRC_SYNCED);
  let w = wSrc;
  if (wSr !== SR) {
    const ratio = SR / wSr, out = new Float32Array(Math.floor(wSrc.length * ratio));
    for (let i = 0; i < out.length; i++) { const x = i / ratio, i0 = x | 0, f = x - i0; out[i] = (wSrc[i0] || 0) * (1 - f) + (wSrc[i0 + 1] || 0) * f; }
    w = out;
  }
  let peak = 0; for (const v of w) peak = Math.max(peak, Math.abs(v));
  const wg = peak > 0 ? 0.9 / peak : 1;
  const wet = new Float32Array(w.length);
  let prev = 0, hp = 0, lp = 0;
  const a = Math.exp(-TAU * 250 / SR);
  for (let i = 0; i < w.length; i++) {
    const x = w[i] * wg;
    hp = a * (hp + x - prev); prev = x;
    lp += 0.25 * (hp - lp);
    wet[i] = hp + 0.7 * (hp - lp);
  }
  return wet;
}

// ── per-kick sidechain duck ────────────────────────────────────────────────
function makeDuck(kickTimes, depth, durSec) {
  const duck = new Float32Array(N).fill(1);
  const dDur = Math.floor(durSec * SR);
  for (const kt of kickTimes) {
    const s0 = Math.floor(kt * SR);
    for (let k = 0; k < dDur; k++) { const i = s0 + k; if (i >= N) break; const d = depth + (1 - depth) * (k / dDur); if (d < duck[i]) duck[i] = d; }
  }
  return duck;
}

// ── render one mix ─────────────────────────────────────────────────────────
function render(mix) {
  const out = new Float32Array(N);
  const wet = prepWhistle();
  const loopLen = wet.length / SR;
  const melody = existsSync(SRC_NOTES) ? JSON.parse(readFileSync(SRC_NOTES, "utf8")).notes : [];

  // drums + sidechain per mix
  const kickTimes = [];
  let PAT, kgain, deep, claps = [];
  if (mix === "dub")      { PAT = [1,0,0,0, 0,0,0,0, 1,0,0,0, 0,0,1,0]; kgain = 1.05; deep = true; }
  else if (mix === "footwork") { PAT = [1,0,1,0, 0,1,0,0, 1,0,0,1, 0,1,0,0]; kgain = 0.95; deep = false; claps = [4, 12]; }
  else                    { PAT = [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0]; kgain = 0.72; deep = false; } // musicbox
  for (let bar = 0; bar < BARS; bar++) {
    for (let s = 0; s < 16; s++) {
      const at = bar * BAR + s * STEP; if (at >= TARGET_SEC) break;
      if (PAT[s]) { kick(out, at, kgain, deep); kickTimes.push(at); }
      if (claps.includes(s)) clap(out, at, 0.5);
    }
  }
  const duck = makeDuck(kickTimes, mix === "footwork" ? 0.45 : 0.4, 0.18);

  // whistle treatment per mix
  let whistle;
  if (mix === "dub") {
    whistle = reverb(wet, 0.42);                       // big plate
    const dly = Math.floor(STEP * 3 * SR);             // dotted-8th throw
    for (let i = dly; i < whistle.length; i++) whistle[i] += whistle[i - dly] * 0.45;
    for (let i = dly; i < whistle.length; i++) whistle[i] += whistle[i - dly] * 0.18; // second tap
  } else if (mix === "footwork") {
    whistle = reverb(wet, 0.10);
    // 16th-note rhythmic GATE (chop) with short fades, some steps dropped
    const G = [1,1,0,1, 1,0,1,1, 1,1,0,1, 0,1,1,0];
    const stepN = Math.floor(STEP * SR), fade = Math.floor(0.004 * SR);
    const gated = new Float32Array(whistle.length);
    for (let i = 0; i < whistle.length; i++) {
      const stepIdx = Math.floor(i / stepN) % 16;
      let g = G[stepIdx];
      const into = i % stepN;
      if (into < fade) g *= into / fade;
      else if (into > stepN - fade) g *= (stepN - into) / fade;
      gated[i] = whistle[i] * g;
    }
    whistle = gated;
  } else {
    whistle = reverb(wet, 0.12);                       // musicbox: clear, present
  }
  const slap = Math.floor(STEP * 2 * SR);
  for (let i = whistle.length - 1; i >= slap; i--) whistle[i] += whistle[i - slap] * (mix === "dub" ? 0.06 : 0.09);

  // place whistle + harmonic layer through whole track
  for (let t0 = 0; t0 < TARGET_SEC; t0 += loopLen) {
    const off = Math.floor(t0 * SR);
    const wgain = mix === "dub" ? 1.0 : 1.2;
    for (let i = 0; i < whistle.length; i++) { const di = off + i; if (di >= N) break; add(out, di, whistle[i] * wgain * duck[di]); }

    for (let n = 0; n < melody.length; n++) {
      const nt = melody[n]; const at = t0 + nt.startSec; if (at >= TARGET_SEC) break;
      if (mix === "musicbox") {
        fmBell(out, at, nt.midi, 0.22);                // root
        fmBell(out, at, nt.midi + 7, 0.13);            // stacked fifth
        fmBell(out, at, nt.midi + 12, 0.10);           // octave shimmer
        if (n % 2 === 0) bass(out, at, nt.midi - 12, 0.30); // soft bass root
      } else if (mix === "footwork") {
        bell(out, at, nt.midi, 0.22, 4.5, 0.7);        // snappier
        // ratchet: retrigger the bell as a fast triplet on every 3rd note
        if (n % 3 === 0) { bell(out, at + STEP / 3, nt.midi, 0.14, 6, 0.4); bell(out, at + 2 * STEP / 3, nt.midi, 0.10, 6, 0.4); }
      } else { // dub — deep, sparse sister, reverbed feel via long decay
        bell(out, at, nt.midi - 12, 0.20, 1.6, 2.2);
      }
    }
  }

  // master
  let mpeak = 0; for (let i = 0; i < N; i++) { out[i] = softclip(out[i] * 1.08); mpeak = Math.max(mpeak, Math.abs(out[i])); }
  const norm = mpeak > 0 ? 0.97 / mpeak : 1;
  const fadeN = Math.floor(0.5 * SR);
  const buf = Buffer.alloc(44 + N * 2);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + N * 2, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 2, 28);
  buf.writeUInt16LE(2, 32); buf.writeUInt16LE(16, 34); buf.write("data", 36); buf.writeUInt32LE(N * 2, 40);
  for (let i = 0; i < N; i++) {
    let v = out[i] * norm;
    if (i < fadeN) v *= i / fadeN;
    if (i > N - fadeN) v *= (N - i) / fadeN;
    buf.writeInt16LE(Math.round(clamp(v, -1, 1) * 32767), 44 + i * 2);
  }
  const wavPath = resolve(ROOT, `out/cornerfive-${mix}.wav`);
  const mp3Path = resolve(ROOT, `out/cornerfive-${mix}.mp3`);
  writeFileSync(wavPath, buf);
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wavPath,
    "-af", "loudnorm=I=-11:TP=-1.0:LRA=11", "-c:a", "libmp3lame", "-b:a", "320k", mp3Path], { stdio: "inherit" });
  if (r.status !== 0) { console.error(`✗ ffmpeg failed for ${mix}`); process.exit(1); }
  console.log(`✓ ${mix} → ${mp3Path.replace(process.env.HOME, "~")}`);
}

const which = process.argv[2] || "all";
const mixes = which === "all" ? ["dub", "footwork", "musicbox"] : [which];
for (const m of mixes) render(m);
