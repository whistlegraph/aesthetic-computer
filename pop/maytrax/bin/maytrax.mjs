#!/usr/bin/env node
// maytrax.mjs — the prodigy-shaped, juno-reactor-flavored big-beat
// techno bed. breakbeat-driven (not 4-on-floor) with a distorted 909
// kick layered under the break. hoover stabs from the hippyhayzard
// synth library, orchestral hit accents loaded from
// pop/hellsine/samples/, inline 303 acid squelch on the second drop.
//
// Bottom-up posture (pop/SCORE.md): every voice is synthesized here
// except the orchestral one-shots (brass / flugelhorn / bell), which
// are already in the AC sample palette.
//
// Form (16 bars × 2 drops):
//   intro (4 bars)  — break alone, no hoover, no stabs
//   drop  (8 bars)  — full kit: break + 909 + hoover stabs + brass hits
//   break (4 bars)  — kick out, hoover dropouts, 303 squelch enters
//   drop  (8 bars)  — full kit + 303 + flugelhorn accent on bar 1
//   outro (4 bars)  — break decays, bell on last bar
//
// Usage:
//   node pop/maytrax/bin/maytrax.mjs                        # → out/maytrax.mp3
//   node pop/maytrax/bin/maytrax.mjs --bpm 140 --out ~/m.mp3

import { writeFileSync, mkdirSync, readFileSync, existsSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { mixEventHoover } from "../../hippyhayzard/synths/hoover.mjs";

const SR = 48_000;
const HERE = dirname(fileURLToPath(import.meta.url));

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const BPM = Number(flags.bpm ?? 140);
const beat = 60 / BPM;
const bar = beat * 4;
const sx = beat / 4; // 16th note

// ── utility ────────────────────────────────────────────────────────────
function add(buf, idx, v) { if (idx >= 0 && idx < buf.length) buf[idx] += v; }
const TAU = Math.PI * 2;

// ── load a WAV one-shot (16-bit or 32-bit float PCM, mono or stereo) ──
function loadWav(path) {
  if (!existsSync(path)) return null;
  const b = readFileSync(path);
  // riff/fmt parser, minimal
  let p = 12;
  let fmt = null, dataOff = 0, dataLen = 0;
  while (p < b.length - 8) {
    const id = b.toString("ascii", p, p + 4);
    const sz = b.readUInt32LE(p + 4);
    if (id === "fmt ") {
      fmt = {
        format: b.readUInt16LE(p + 8),
        channels: b.readUInt16LE(p + 10),
        sampleRate: b.readUInt32LE(p + 12),
        bitsPerSample: b.readUInt16LE(p + 22),
      };
    } else if (id === "data") {
      dataOff = p + 8;
      dataLen = sz;
      break;
    }
    p += 8 + sz;
  }
  if (!fmt) return null;
  const { channels, bitsPerSample, sampleRate, format } = fmt;
  const bytesPerSamp = bitsPerSample / 8;
  const totalSamps = dataLen / bytesPerSamp / channels;
  const out = new Float32Array(totalSamps);
  for (let i = 0; i < totalSamps; i++) {
    let v = 0;
    if (format === 3 && bitsPerSample === 32) {
      v = b.readFloatLE(dataOff + i * channels * 4);
    } else if (format === 1 && bitsPerSample === 16) {
      v = b.readInt16LE(dataOff + i * channels * 2) / 32768;
    } else if (format === 1 && bitsPerSample === 24) {
      const o = dataOff + i * channels * 3;
      let s = b[o] | (b[o + 1] << 8) | (b[o + 2] << 16);
      if (s & 0x800000) s |= ~0xffffff;
      v = s / 8388608;
    }
    out[i] = v;
  }
  return { samples: out, sampleRate };
}

function playSample(buf, smp, startSec, gain = 1, rate = 1) {
  if (!smp) return;
  const s0 = Math.floor(startSec * SR);
  const src = smp.samples;
  const srcRate = smp.sampleRate;
  const step = (srcRate / SR) * rate;
  for (let i = 0; i < buf.length - s0; i++) {
    const p = i * step;
    const j = Math.floor(p);
    if (j >= src.length - 1) break;
    const f = p - j;
    const v = src[j] * (1 - f) + src[j + 1] * f;
    add(buf, s0 + i, v * gain);
  }
}

// ── drum synthesis ─────────────────────────────────────────────────────
// the prodigy 909-kick: pitch-enveloped sine driven hard through tanh.
function kick909(buf, startSec, g = 1.0) {
  const dur = 0.22, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const f = 52 + (165 - 52) * Math.exp(-t * 50);
    ph += (TAU * f) / SR;
    const body = Math.sin(ph) * Math.exp(-t * 11);
    const click = i < SR * 0.003 ? (Math.random() * 2 - 1) * 0.9 * (1 - i / (SR * 0.003)) : 0;
    add(buf, s0 + i, Math.tanh((body + click) * 2.2) * g);
  }
}

// chopped breakbeat snare (high noise burst + body sine).
function snareBrk(buf, startSec, g = 0.8) {
  const dur = 0.18, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let prev = 0, ph = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev * 0.62; prev = nz;
    ph += (TAU * 195) / SR;
    const body = Math.sin(ph) * Math.exp(-t * 28) * 0.55;
    const tail = hp * Math.exp(-t * 18);
    add(buf, s0 + i, (body + tail) * g);
  }
}

// closed hi-hat (high-pass noise burst).
function hat(buf, startSec, g = 0.18) {
  const dur = 0.045, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev * 0.78; prev = nz;
    add(buf, s0 + i, hp * Math.exp(-t * 90) * g);
  }
}

// open hat (longer decay).
function openHat(buf, startSec, g = 0.22) {
  const dur = 0.22, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev * 0.82; prev = nz;
    add(buf, s0 + i, hp * Math.exp(-t * 14) * g);
  }
}

// the funky-drummer-shaped break, one bar (16th-grid). K=kick, S=snare,
// h=hat, H=openhat, .=rest. amen-adjacent: kick on 1+&3, snare on 2+4
// plus a syncopated ghost; hats running 16ths with opens on the offs.
function breakBar(buf, drm, t0, energy = 1) {
  //               1   .   .   .   2   .   .   .   3   .   .   .   4   .   .   .
  const kP = ["K", ".", ".", "K", ".", ".", ".", ".", ".", ".", "K", ".", ".", ".", ".", "."];
  const sP = [".", ".", ".", ".", "S", ".", ".", "s", ".", ".", ".", ".", "S", ".", ".", "."];
  const hP = ["h", "h", "h", "H", "h", "h", "h", "h", "h", "H", "h", "h", "h", "h", "H", "h"];
  for (let e = 0; e < 16; e++) {
    const t = t0 + e * sx;
    if (kP[e] === "K") kick909(drm, t, 1.05 * energy);
    if (sP[e] === "S") snareBrk(drm, t, 0.78 * energy);
    if (sP[e] === "s") snareBrk(drm, t, 0.34 * energy); // ghost
    if (hP[e] === "h") hat(buf, t, 0.16 * energy);
    if (hP[e] === "H") openHat(buf, t, 0.20 * energy);
  }
}

// ── 303 acid line ──────────────────────────────────────────────────────
// saw oscillator into a resonant SVF lowpass, with an envelope that
// modulates the cutoff (the squelch). short notes, accent slides.
function acid303(buf, t0, midi, durSec, opts = {}) {
  const { gain = 0.28, accent = false, slideFrom = null } = opts;
  const n = Math.floor(durSec * SR);
  const s0 = Math.floor(t0 * SR);
  const f0 = 440 * Math.pow(2, (midi - 69) / 12);
  const fStart = slideFrom !== null
    ? 440 * Math.pow(2, (slideFrom - 69) / 12)
    : f0;
  // SVF state
  let lp = 0, bp = 0;
  const q = accent ? 0.08 : 0.14; // lower = more resonance
  const envDecay = accent ? 9 : 18;
  const cutMin = 320;
  const cutMax = accent ? 4800 : 2600;
  let saw = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const slideT = Math.min(1, t / 0.045); // 45ms glide
    const freq = fStart + (f0 - fStart) * slideT;
    saw += freq / SR;
    saw -= Math.floor(saw);
    const osc = saw * 2 - 1; // -1..1
    const env = Math.exp(-t * envDecay);
    const cutoff = cutMin + (cutMax - cutMin) * env;
    const fc = Math.tan(Math.PI * Math.min(0.45, cutoff / SR));
    const r = q;
    // one-pole SVF (Chamberlin)
    const hp = osc - lp - r * bp;
    bp += fc * hp;
    lp += fc * bp;
    const amp = Math.exp(-t * (accent ? 5 : 8)) * gain;
    add(buf, s0 + i, Math.tanh(lp * 1.2) * amp);
  }
}

// ── arrange ───────────────────────────────────────────────────────────
const totalBars = 4 + 8 + 4 + 8 + 4; // 28 bars
const totalSec = totalBars * bar + 0.5;
const N = Math.floor(totalSec * SR);
const bus = new Float32Array(N); // hats / synths / hits
const drm = new Float32Array(N); // kicks + snares (for sidechain duck)
const kickTimes = [];

// load orchestral hits from hellsine/samples/
const SAMPLES = resolve(HERE, "../../hellsine/samples");
const brass = loadWav(resolve(SAMPLES, "brass-strong-e2.wav"));
const flugel = loadWav(resolve(SAMPLES, "flugelhorn-asharp.wav"));
const bell = loadWav(resolve(SAMPLES, "church-bell.wav"));

// F minor stab pattern (Fm — F Ab C Eb): root motion F → Eb → Db → C
const FM_ROOTS = [53, 51, 49, 48]; // F3 Eb3 Db3 C3

let cursor = 0;

// section 1: intro (4 bars) — break alone
for (let b = 0; b < 4; b++) {
  const t0 = (cursor + b) * bar;
  breakBar(bus, drm, t0, 0.85);
  // log kicks for duck
  kickTimes.push(t0, t0 + 3 * sx, t0 + 10 * sx);
}
cursor += 4;

// section 2: drop 1 (8 bars) — full kit + hoover stabs + brass hit on bar 1/5
for (let b = 0; b < 8; b++) {
  const t0 = (cursor + b) * bar;
  breakBar(bus, drm, t0, 1.0);
  kickTimes.push(t0, t0 + 3 * sx, t0 + 10 * sx);
  // hoover stab on beat 3 of every bar
  const stabMidi = FM_ROOTS[b % FM_ROOTS.length];
  mixEventHoover(
    { startSec: t0 + beat * 2, midi: stabMidi, durSec: beat * 1.5, gain: 0.42 },
    bus, { sampleRate: SR, preset: "stab" },
  );
  // brass hit on bar 1 and 5 of the drop
  if ((b === 0 || b === 4) && brass) {
    playSample(bus, brass, t0, 0.45, 1.0);
  }
}
cursor += 8;

// section 3: break (4 bars) — kick out, 303 acid enters
for (let b = 0; b < 4; b++) {
  const t0 = (cursor + b) * bar;
  // sparse hats only
  for (let e = 0; e < 16; e++) {
    if (e % 4 === 2) openHat(bus, t0 + e * sx, 0.14);
  }
  // snare on 2 and 4 to keep the pulse
  snareBrk(drm, t0 + beat, 0.5);
  snareBrk(drm, t0 + beat * 3, 0.5);
  // 303 squelch line — 16th-note F minor pentatonic walk
  const pent = [53, 56, 58, 60, 63, 60, 58, 56]; // F Ab Bb C Eb C Bb Ab
  for (let e = 0; e < 16; e++) {
    const midi = pent[e % pent.length] + (b % 2 === 0 ? 0 : -12);
    const slide = e > 0 && e % 4 === 0 ? pent[(e - 1) % pent.length] : null;
    const accent = e % 4 === 0;
    acid303(bus, t0 + e * sx, midi, sx * 0.9, {
      gain: 0.26, accent, slideFrom: slide,
    });
  }
}
cursor += 4;

// section 4: drop 2 (8 bars) — full kit + 303 + hoover + flugelhorn on bar 1
for (let b = 0; b < 8; b++) {
  const t0 = (cursor + b) * bar;
  breakBar(bus, drm, t0, 1.0);
  kickTimes.push(t0, t0 + 3 * sx, t0 + 10 * sx);
  const stabMidi = FM_ROOTS[b % FM_ROOTS.length];
  mixEventHoover(
    { startSec: t0 + beat * 2, midi: stabMidi, durSec: beat * 1.5, gain: 0.46 },
    bus, { sampleRate: SR, preset: "stab" },
  );
  // 303 continues — keeps the squelch under the drop
  const pent = [53, 56, 58, 60, 63, 60, 58, 56];
  for (let e = 0; e < 16; e++) {
    if (e % 2 === 0) {
      const midi = pent[e % pent.length] - 12;
      acid303(bus, t0 + e * sx, midi, sx * 0.85, {
        gain: 0.18, accent: e % 4 === 0,
      });
    }
  }
  if (b === 0 && flugel) playSample(bus, flugel, t0, 0.50, 1.0);
}
cursor += 8;

// section 5: outro (4 bars) — break decays, bell on last bar
for (let b = 0; b < 4; b++) {
  const t0 = (cursor + b) * bar;
  breakBar(bus, drm, t0, 0.85 - b * 0.15);
  kickTimes.push(t0, t0 + 3 * sx, t0 + 10 * sx);
  if (b === 3 && bell) playSample(bus, bell, t0 + beat, 0.55, 1.0);
}

// ── sidechain duck (the pump) + master ─────────────────────────────────
const duck = new Float32Array(N).fill(1);
for (const kt of kickTimes) {
  const s0 = Math.floor(kt * SR), len = Math.floor(0.14 * SR);
  for (let i = 0; i < len && s0 + i < N; i++) {
    const w = i / len;
    const d = 0.55 + 0.45 * w; // dip to .55, recover
    if (d < duck[s0 + i]) duck[s0 + i] = d;
  }
}

const mix = new Float32Array(N);
for (let i = 0; i < N; i++) {
  const s = bus[i] * duck[i] + drm[i];
  mix[i] = Math.tanh(s * 1.08);
}
let peak = 0;
for (let i = 0; i < N; i++) { const a = Math.abs(mix[i]); if (a > peak) peak = a; }
if (peak > 0) { const n = 0.85 / peak; for (let i = 0; i < N; i++) mix[i] *= n; }

const outPath = expandHome(flags.out) || resolve(HERE, "../out/maytrax.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(N * 4);
for (let i = 0; i < N; i++) b.writeFloatLE(mix[i], i * 4);
writeFileSync(rawPath, b);
console.log(
  `→ maytrax · ${BPM} BPM · ${totalBars} bars · ${totalSec.toFixed(1)}s · ` +
  `intro→drop→break→drop→outro`,
);
const ff = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
  "-c:a", "libmp3lame", "-q:a", "2", outPath,
], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath}`);
