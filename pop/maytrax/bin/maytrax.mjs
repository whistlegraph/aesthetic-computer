#!/usr/bin/env node
// maytrax.mjs — full-length (2:30) big-beat techno bed in the prodigy /
// juno-reactor / matrix-soundtrack lineage. Stereo render with pop
// master chain (highpass → glue comp → air → alimiter) so it sits with
// the rest of the pixsies catalog (marimbaba, helpabeach, trancepenta).
//
// Bottom-up: every voice synthesized here except the orchestral one-
// shots (brass / flugel / bell) loaded from pop/hellsine/samples/.
// Fully instrumental — no vocals.
//
// Form (88 bars at 140 BPM ≈ 150.9 s):
//   intro    (8 bars)  — sub pulse + sparse hats + dark pad swell
//   build A  (4 bars)  — pad rises, snare roll, white-noise riser
//   drop 1   (16 bars) — break + 909 + sub + soft hoover stabs + brass fanfare
//   break A  (8 bars)  — kick out, harmonic 303 acid + flugel fanfare
//   drop 2   (16 bars) — full kit + 303 + hoover + bell chime on bar 9
//   break B  (8 bars)  — atmospheric: pad + bell + reverse swell
//   drop 3   (16 bars) — biggest: lush hoover triad + brass+flugel fanfares
//   outro    (12 bars) — break decays, sub fades, final bell + pad wash
//
// Usage:
//   node pop/maytrax/bin/maytrax.mjs                          # → out/maytrax.mp3
//   node pop/maytrax/bin/maytrax.mjs --bpm 140 --out ~/m.mp3
//   node pop/maytrax/bin/maytrax.mjs --wav ~/maytrax-MASTER.wav   # DistroKid 44.1k/16-bit

import { writeFileSync, mkdirSync, readFileSync, existsSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { mixEventHoover as _hooverImpl } from "../../hippyhayzard/synths/hoover.mjs";

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

const BPM = Number(flags.bpm ?? 160); // classic jungle
const beat = 60 / BPM;
const bar = beat * 4;
const sx = beat / 4; // 16th note

// ── utility ────────────────────────────────────────────────────────────
function add(buf, idx, v) { if (idx >= 0 && idx < buf.length) buf[idx] += v; }
function addStereo(L, R, idx, l, r) {
  if (idx >= 0 && idx < L.length) { L[idx] += l; R[idx] += r; }
}
const TAU = Math.PI * 2;

// ── load a WAV one-shot (16-bit PCM, 24-bit PCM, or 32-bit float; mono/stereo)
function loadWav(path) {
  if (!existsSync(path)) return null;
  const b = readFileSync(path);
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
  const { channels, bitsPerSample, format } = fmt;
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
  // trim leading silence so chops start on transient (esp. for vocal stabs)
  let lead = 0;
  while (lead < out.length && Math.abs(out[lead]) < 0.005) lead++;
  return { samples: out.subarray(lead), sampleRate: fmt.sampleRate };
}

// maxDurSec truncates the played sample (with a short release fade) so the
// whole track can be tightened up — short decays = punchy.
function playSampleStereo(L, R, smp, startSec, gain = 1, rate = 1, panSpread = 0, maxDurSec = Infinity) {
  if (!smp) return;
  const s0 = Math.floor(startSec * SR);
  const src = smp.samples;
  const step = (smp.sampleRate / SR) * rate;
  const lg = gain * (1 - panSpread * 0.5);
  const rg = gain * (1 + panSpread * 0.5);
  // light haas widening when panSpread > 0
  const haasDelay = panSpread > 0 ? Math.floor(0.012 * SR) : 0;
  const outMax = maxDurSec === Infinity ? Infinity : Math.floor(maxDurSec * SR);
  const relN = Math.floor(0.02 * SR);
  for (let i = 0; i < L.length - s0; i++) {
    const p = i * step;
    const j = Math.floor(p);
    if (j >= src.length - 1 || i >= outMax) break;
    const f = p - j;
    let v = src[j] * (1 - f) + src[j + 1] * f;
    if (outMax !== Infinity && i > outMax - relN) v *= (outMax - i) / relN;
    addStereo(L, R, s0 + i, v * lg, 0);
    if (haasDelay > 0) addStereo(L, R, s0 + i + haasDelay, 0, v * rg);
    else addStereo(L, R, s0 + i, 0, v * rg);
  }
}

// chiptune square-wave voice — for the pirate-shanty opener. Slightly
// detuned + a little pulse-width wobble so it reads as a playful 8-bit lead.
function squareNote(L, R, midi, startSec, durSec, g = 0.2, pan = 0) {
  const f = 440 * Math.pow(2, (midi - 69) / 12);
  const n = Math.floor(durSec * SR), s0 = Math.floor(startSec * SR);
  const lg = g * (1 - pan * 0.5), rg = g * (1 + pan * 0.5);
  const atk = Math.floor(0.004 * SR), rel = Math.floor(0.04 * SR);
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const pw = 0.5 + 0.06 * Math.sin(TAU * 5 * t);          // pulse-width wobble
    const ph = (f * t) % 1, ph2 = (f * 1.003 * t) % 1;       // tiny detune
    let s = (ph < pw ? 1 : -1) * 0.6 + (ph2 < 0.5 ? 1 : -1) * 0.4;
    let env = 1;
    if (i < atk) env = i / atk; else if (i > n - rel) env = Math.max(0, (n - i) / rel);
    const v = s * env;
    addStereo(L, R, s0 + i, v * lg, v * rg);
  }
}

// ── SAMPLE INSTRUMENT ENGINE ─────────────────────────────────────────
// Jungle mode (default; --no-samples disables): every voice is a Freesound
// one-shot from kit.json. Drums play at native pitch; pitchable roles
// (sub/reese/lead/pad/choir/brass) are resampled per-note over the F-minor
// data, with the root auto-detected (f0) so transposition lands in tune.
// The synth bodies below stay as a fallback when a role is missing.
const JUNGLE = !flags["no-samples"];
// global decay scaler — shorter = tighter, punchier track. --decay 0.7 etc.
const DEC = Number(flags.decay ?? 0.65); // tight, poppy, minimal-techno
const LAZY = Number(flags.lazy ?? 0.45);  // subtle laid-back drag + swing
const HUMAN = Number(flags.human ?? 0.4); // gentle micro-timing jitter
const hz = () => (Math.random() * 2 - 1) * 0.004 * HUMAN; // ±~1.6 ms jitter (tight)
const KIT_PATH = resolve(HERE, "..", "kit.json");
const KIT = existsSync(KIT_PATH) ? JSON.parse(readFileSync(KIT_PATH, "utf8")) : {};
const NN = { C: 0, D: 2, E: 4, F: 5, G: 7, A: 9, B: 11 };
const noteToMidi = (s) => { const m = /^([A-G])([#b]?)(-?\d)$/.exec(s || "C4"); return m ? NN[m[1]] + (m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0) + (parseInt(m[3], 10) + 1) * 12 : 60; };
function detectF0(s, sr) {
  let pk = 0, pidx = 0; for (let i = 0; i < s.length; i++) { const a = Math.abs(s[i]); if (a > pk) { pk = a; pidx = i; } }
  const w = Math.min(8192, s.length), st = Math.max(0, Math.min(s.length - w, pidx - (w >> 1)));
  const minLag = Math.floor(sr / 1200), maxLag = Math.floor(sr / 45);
  let best = -1, bestLag = 0;
  for (let lag = minLag; lag <= maxLag; lag++) { let sum = 0; for (let i = 0; i + lag < w; i++) sum += s[st + i] * s[st + i + lag]; if (sum > best) { best = sum; bestLag = lag; } }
  return bestLag > 0 ? sr / bestLag : 0;
}
const SMP = {};
if (JUNGLE) for (const role of Object.keys(KIT)) {
  const wv = loadWav(KIT[role].path); if (!wv) continue;
  let rootMidi = noteToMidi(KIT[role].root || "C4");
  if (KIT[role].pitched) { const f = detectF0(wv.samples, wv.sampleRate); if (f > 30 && f < 2000) rootMidi = Math.round(69 + 12 * Math.log2(f / 440)); }
  SMP[role] = { w: wv, rootMidi };
}
if (JUNGLE) console.log(`• jungle sample voices: ${Object.keys(SMP).join(", ")}`);
// in jungle mode the synth hoover lead is silenced — a sampled flute lead
// (added in the sampled-instrument pass) carries the main voice instead.
const mixEventHoover = JUNGLE ? () => {} : _hooverImpl;
// mono sample player (native pitch unless `rate` given); linear resample,
// optionally truncated to maxDurSec with a short release fade so rapidly
// re-triggered tonal samples (sub/reese) don't pile into mud.
function smpMono(buf, role, startSec, gain, rate = 1, maxDurSec = Infinity) {
  const e = SMP[role]; if (!e) return false;
  const src = e.w.samples, step = (e.w.sampleRate / SR) * rate, s0 = Math.floor(startSec * SR);
  const outMax = maxDurSec === Infinity ? Infinity : Math.floor(maxDurSec * SR);
  const relN = Math.floor(0.02 * SR);
  let i = 0;
  for (let p = 0; p < src.length - 1 && i < outMax; i++, p += step) {
    const idx = p | 0, fr = p - idx; let v = (src[idx] * (1 - fr) + src[idx + 1] * fr) * gain;
    if (outMax !== Infinity && i > outMax - relN) v *= (outMax - i) / relN;
    add(buf, s0 + i, v);
  }
  return true;
}
// pitched mono player — resample a tonal role to a MIDI note.
function smpNote(buf, role, note, startSec, gain, maxDurSec = Infinity) { const e = SMP[role]; return e ? smpMono(buf, role, startSec, gain, Math.pow(2, (note - e.rootMidi) / 12), maxDurSec) : false; }

// ── drum synthesis (sample-backed in jungle mode) ──────────────────────
// the prodigy 909-kick: pitch-enveloped sine driven hard through tanh,
// with an extra sub layer underneath for weight.
function kick909(buf, startSec, g = 1.0) {
  if (smpMono(buf, "kick", startSec, g * 0.9, 1, 0.20 * DEC)) {
    // deep, matrix-cinematic sub-boom under the sampled kick — a pitch-
    // dropping sine that settles at ~40 Hz for weight you feel in the chest.
    const n = Math.floor(0.20 * SR), s0 = Math.floor(startSec * SR);
    let ph = 0;
    for (let i = 0; i < n; i++) {
      const t = i / SR, f = 110 * Math.exp(-t * 26) + 40;
      ph += (TAU * f) / SR;
      add(buf, s0 + i, Math.tanh(Math.sin(ph) * 1.3) * Math.exp(-t * 6.5) * 0.72 * g);
    }
    return;
  }
  const dur = 0.28, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let ph = 0, sub = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const f = 52 + (165 - 52) * Math.exp(-t * 50);
    ph += (TAU * f) / SR;
    const body = Math.sin(ph) * Math.exp(-t * 11);
    // sub layer — 38 Hz pure sine decaying slower (the rumble)
    sub += (TAU * 38) / SR;
    const subLayer = Math.sin(sub) * Math.exp(-t * 5) * 0.55;
    const click = i < SR * 0.003 ? (Math.random() * 2 - 1) * 0.9 * (1 - i / (SR * 0.003)) : 0;
    add(buf, s0 + i, Math.tanh((body + click) * 2.2) * g + subLayer * g * 0.7);
  }
}

// chopped breakbeat snare (high noise burst + body sine).
function snareBrk(buf, startSec, g = 0.8) {
  if (smpMono(buf, "snare", startSec, g * 0.95, 1, 0.18 * DEC)) return;
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
  if (smpMono(buf, "hat", startSec, g * 1.4, 1, 0.05 * DEC)) return;
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
  if (smpMono(buf, "ohat", startSec, g * 1.3, 1, 0.13 * DEC)) return;
  const dur = 0.22, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev * 0.82; prev = nz;
    add(buf, s0 + i, hp * Math.exp(-t * 14) * g);
  }
}

// shaker / maraca — a two-pole high-passed noise burst with a soft 3 ms
// attack and a longer decay than the closed hat, so it reads as an airy
// "shh" rather than a "tk". The backbone of the sunk-in perc layer.
function shaker(buf, startSec, g = 0.10, decay = 52) {
  if (smpMono(buf, "shaker", startSec, g * 1.6, 1, 0.06 * DEC)) return;
  const dur = 0.09, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let prev = 0, prev2 = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev * 0.86; prev = nz;
    const hp2 = hp - prev2 * 0.4; prev2 = hp;
    const att = Math.min(1, t / 0.003);
    add(buf, s0 + i, hp2 * att * Math.exp(-t * decay) * g);
  }
}

// helper: render a mono hat once and place it into stereo with a tight
// pan (default 0 = dead center). Avoids the decorrelated-noise blowout
// you get from calling openHat(busL,...) + openHat(busR,...) separately.
function tightHatStereo(busL, busR, startSec, kind = "open", g = 0.22, pan = 0) {
  const dur = kind === "open" ? 0.22 : 0.045;
  const tmp = new Float32Array(Math.floor(dur * SR) + 4);
  if (kind === "open") openHat(tmp, 0, g);
  else hat(tmp, 0, g);
  const s0 = Math.floor(startSec * SR);
  const lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
  for (let i = 0; i < tmp.length; i++) {
    const dst = s0 + i;
    if (dst < 0 || dst >= busL.length) continue;
    const v = tmp[i];
    busL[dst] += v * lg;
    busR[dst] += v * rg;
  }
}

// funky-drummer-shaped break, one bar (16th-grid). K=kick, S=snare,
// h=hat, H=openhat, .=rest.
function breakBar(busL, busR, drm, t0, energy = 1, hatPan = 0.35) {
  // four chopped-break variations, cycled per bar so the break keeps moving
  const KV = [
    ["K", ".", ".", "K", ".", ".", ".", ".", ".", ".", "K", ".", ".", ".", ".", "."],
    ["K", ".", ".", ".", ".", ".", "K", ".", ".", "K", ".", ".", ".", ".", "K", "."],
    ["K", ".", ".", "K", ".", ".", ".", "K", ".", ".", "K", ".", ".", ".", ".", "."],
    ["K", ".", "K", ".", ".", ".", ".", ".", "K", ".", ".", ".", "K", ".", ".", "."],
  ];
  const SV = [
    [".", ".", ".", ".", "S", ".", ".", "s", ".", ".", ".", ".", "S", ".", ".", "."],
    [".", ".", ".", ".", "S", ".", ".", ".", ".", "s", ".", ".", "S", ".", ".", "s"],
    [".", ".", "s", ".", "S", ".", ".", ".", ".", ".", "s", ".", "S", ".", ".", "."],
    [".", ".", ".", "s", "S", ".", ".", "s", ".", ".", ".", "s", "S", ".", "s", "."],
  ];
  // change pattern per PHRASE (every 4 bars), not every bar, so it stays
  // regular and groovy instead of shuffling around every beat.
  const vi = (Math.floor(Math.round(t0 / bar) / 4) % KV.length + KV.length) % KV.length;
  const kP = KV[vi], sP = SV[vi];
  const hP = ["h", "h", "h", "H", "h", "h", "h", "h", "h", "H", "h", "h", "h", "h", "H", "h"];
  const swing = (e) => (e % 2 === 1 ? 0.020 * LAZY : 0); // lazy swing on the offbeats
  const drag = 0.010 * LAZY;                              // whole-kit laid-back drag
  for (let e = 0; e < 16; e++) {
    const t = t0 + e * sx;
    // kick stays tightest; snare drags laziest behind the beat; humanized
    if (kP[e] === "K") kick909(drm, t + drag * 0.4 + hz() * 0.5, 1.05 * energy);
    if (sP[e] === "S") snareBrk(drm, t + drag + swing(e) + hz(), (0.7 + 0.16 * Math.random()) * energy);
    if (sP[e] === "s") snareBrk(drm, t + drag + swing(e) + hz(), (0.3 + 0.1 * Math.random()) * energy);
    // hats: ping-pong slightly so the kit breathes in stereo
    const pan = (e % 2 === 0 ? -1 : 1) * hatPan, th = t + drag + swing(e) + hz();
    const tmp = new Float32Array(busL.length);
    if (hP[e] === "h") hat(tmp, th, (0.10 + 0.04 * Math.random()) * energy);
    if (hP[e] === "H") openHat(tmp, th, (0.13 + 0.04 * Math.random()) * energy);
    // mix tmp into stereo with pan
    const s0 = Math.floor(t * SR);
    const end = Math.min(busL.length, s0 + Math.floor(0.25 * SR));
    const lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (let i = s0; i < end; i++) {
      const v = tmp[i];
      if (v) { busL[i] += v * lg; busR[i] += v * rg; }
    }
  }
}

// ── sunk-in perc layer ───────────────────────────────────────────────
// A busy swung-16th shaker groove, ghosted low and sent to the reverb
// bus so it sits *behind* the kit rather than on top. Accented shakers
// on the &s, open-hat lifts on the & of 2 and & of 4 (also reverb-fed).
// energy scales gain; revSend controls how "sunk" the layer reads.
// Uses small reusable scratch buffers — no per-hit full-length allocs.
function percGroove(bL, bR, rL, rR, t0, energy = 1, opts = {}) {
  const { swing = 0.018, base = 0.052, revSend = 0.38 } = opts;
  const shLen = Math.floor(0.09 * SR) + 4;
  const ohLen = Math.floor(0.22 * SR) + 4;
  const tmp = new Float32Array(ohLen);
  for (let e = 0; e < 16; e++) {
    // swing the odd 16ths slightly late so the groove breathes
    const t = t0 + e * sx + (e % 2 === 1 ? swing : 0);
    const s0 = Math.floor(t * SR);
    const accent = e % 4 === 2;            // the &s
    const jitter = 0.75 + 0.25 * (((e * 7) % 5) / 4);
    const g = (accent ? base * 1.5 : base) * energy * jitter;
    const pan = (e % 2 === 0 ? -1 : 1) * 0.28;
    tmp.fill(0, 0, shLen);
    shaker(tmp, 0, g);
    const lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (let i = 0; i < shLen; i++) {
      const v = tmp[i]; if (!v) continue;
      const dst = s0 + i; if (dst < 0 || dst >= bL.length) continue;
      bL[dst] += v * lg; bR[dst] += v * rg;
      rL[dst] += v * lg * revSend; rR[dst] += v * rg * revSend;
    }
    // open-hat lift on the & of 2 and & of 4 — sunk via heavier rev send
    if (e === 6 || e === 14) {
      tmp.fill(0, 0, ohLen);
      openHat(tmp, 0, 0.085 * energy);
      const op = (e === 6 ? -1 : 1) * 0.22;
      const olg = 1 - op * 0.5, org = 1 + op * 0.5;
      for (let i = 0; i < ohLen; i++) {
        const v = tmp[i]; if (!v) continue;
        const dst = s0 + i; if (dst < 0 || dst >= bL.length) continue;
        bL[dst] += v * olg; bR[dst] += v * org;
        rL[dst] += v * olg * 0.5; rR[dst] += v * org * 0.5;
      }
    }
  }
}

// ── sub-bass voice ─────────────────────────────────────────────────────
// the deep undercarriage — pure sine with a soft attack so it sits
// under the kick (sidechain ducks it on each kick).
function subBass(buf, startSec, midi, durSec, g = 0.42) {
  if (smpNote(buf, "sub", midi, startSec, g * 1.1, durSec)) return;
  const f = 440 * Math.pow(2, (midi - 69) / 12);
  const n = Math.floor(durSec * SR);
  const s0 = Math.floor(startSec * SR);
  const att = Math.floor(0.012 * SR);
  const rel = Math.floor(0.08 * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    ph += (TAU * f) / SR;
    let env = 1;
    if (i < att) env = i / att;
    else if (i > n - rel) env = Math.max(0, (n - i) / rel);
    // gentle 2nd-harmonic for warmth, then tanh-clipped
    const s = Math.sin(ph) + Math.sin(ph * 2) * 0.12;
    add(buf, s0 + i, Math.tanh(s * 1.05) * env * g);
  }
}

// ── dark pad ──────────────────────────────────────────────────────────
// detuned-saws into a dark resonant lowpass; stereo by rendering twice
// with mirrored detune. Sits as the matrix-noir undertone.
function darkPad(L, R, startSec, midiArr, durSec, g = 0.18) {
  if (JUNGLE && SMP.pad) {
    const e = SMP.pad;
    midiArr.forEach((m, k) => playSampleStereo(L, R, e.w, startSec, g * 0.6, Math.pow(2, (m - e.rootMidi) / 12), (k / Math.max(1, midiArr.length - 1) - 0.5) * 0.6));
    return;
  }
  const n = Math.floor(durSec * SR);
  const s0 = Math.floor(startSec * SR);
  const baseF = midiArr.map((m) => 440 * Math.pow(2, (m - 69) / 12));
  // 4 voices per side, detuned ±cents
  const detL = [-12, -4, +3, +9];
  const detR = [-9, -3, +4, +12];
  const phL = new Array(midiArr.length * detL.length).fill(0);
  const phR = new Array(midiArr.length * detR.length).fill(0);
  let lpL = 0, bpL = 0, lpR = 0, bpR = 0;
  const cutoff = 900;       // dark
  const q = 0.6;
  const fc = Math.tan(Math.PI * Math.min(0.45, cutoff / SR));
  const att = Math.floor(0.35 * SR);
  const rel = Math.floor(0.45 * SR);
  for (let i = 0; i < n; i++) {
    let sumL = 0, sumR = 0;
    for (let m = 0; m < midiArr.length; m++) {
      for (let v = 0; v < detL.length; v++) {
        const idxL = m * detL.length + v;
        const idxR = m * detR.length + v;
        const fL = baseF[m] * Math.pow(2, detL[v] / 1200);
        const fR = baseF[m] * Math.pow(2, detR[v] / 1200);
        phL[idxL] += fL / SR; if (phL[idxL] >= 1) phL[idxL] -= 1;
        phR[idxR] += fR / SR; if (phR[idxR] >= 1) phR[idxR] -= 1;
        sumL += phL[idxL] * 2 - 1;
        sumR += phR[idxR] * 2 - 1;
      }
    }
    const norm = 1 / (midiArr.length * detL.length);
    sumL *= norm; sumR *= norm;
    // SVF lowpass per side
    const hpL = sumL - lpL - q * bpL;
    bpL += fc * hpL; lpL += fc * bpL;
    const hpR = sumR - lpR - q * bpR;
    bpR += fc * hpR; lpR += fc * bpR;
    let env = 1;
    if (i < att) env = i / att;
    else if (i > n - rel) env = Math.max(0, (n - i) / rel);
    addStereo(L, R, s0 + i, Math.tanh(lpL * 1.1) * env * g,
                            Math.tanh(lpR * 1.1) * env * g);
  }
}

// ── 303 acid line ──────────────────────────────────────────────────────
function acid303(buf, t0, midi, durSec, opts = {}) {
  const { gain = 0.28, accent = false, slideFrom = null } = opts;
  // jungle: the reese bass is the mid-bass squelch (the genre signature)
  if (smpNote(buf, "reese", midi, t0, gain * (accent ? 1.3 : 1.0), durSec)) return;
  const n = Math.floor(durSec * SR);
  const s0 = Math.floor(t0 * SR);
  const f0 = 440 * Math.pow(2, (midi - 69) / 12);
  const fStart = slideFrom !== null
    ? 440 * Math.pow(2, (slideFrom - 69) / 12)
    : f0;
  let lp = 0, bp = 0;
  const q = accent ? 0.08 : 0.14;
  const envDecay = accent ? 9 : 18;
  const cutMin = 320;
  const cutMax = accent ? 4800 : 2600;
  let saw = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const slideT = Math.min(1, t / 0.045);
    const freq = fStart + (f0 - fStart) * slideT;
    saw += freq / SR;
    saw -= Math.floor(saw);
    const osc = saw * 2 - 1;
    const env = Math.exp(-t * envDecay);
    const cutoff = cutMin + (cutMax - cutMin) * env;
    const fc = Math.tan(Math.PI * Math.min(0.45, cutoff / SR));
    const r = q;
    const hp = osc - lp - r * bp;
    bp += fc * hp;
    lp += fc * bp;
    const amp = Math.exp(-t * (accent ? 5 : 8)) * gain;
    add(buf, s0 + i, Math.tanh(lp * 1.2) * amp);
  }
}

// ── riser (uplifting noise sweep + pitch-rising sine) ─────────────────
function riser(L, R, t0, durSec, g = 0.32) {
  const n = Math.floor(durSec * SR);
  const s0 = Math.floor(t0 * SR);
  let prev = 0, ph = 0;
  for (let i = 0; i < n; i++) {
    const w = i / n;
    // noise band-pass that opens from 800 → 8000 Hz
    const cutoff = 800 + (8000 - 800) * w;
    const a = Math.exp(-1 / (SR / cutoff));
    const nz = Math.random() * 2 - 1;
    const filt = nz - prev * a; prev = nz;
    // pitch sweep sine 80 → 1200 Hz
    const f = 80 + (1200 - 80) * w * w;
    ph += (TAU * f) / SR;
    const sine = Math.sin(ph) * 0.5;
    const env = w * w; // accelerating crescendo
    const v = (filt * 0.6 + sine * 0.4) * env * g;
    // pan from center → out (sweep widens)
    const spread = w * 0.6;
    addStereo(L, R, s0 + i, v * (1 - spread), v * (1 + spread));
  }
}

// reverse-cymbal style swell (white noise + rising lowpass cutoff)
function reverseSwell(L, R, t0, durSec, g = 0.28) {
  const n = Math.floor(durSec * SR);
  const s0 = Math.floor(t0 * SR);
  let lpL = 0, lpR = 0;
  for (let i = 0; i < n; i++) {
    const w = i / n;
    const cutoff = 200 + (6000 - 200) * w;
    const a = 1 - Math.exp(-TAU * cutoff / SR);
    const nzL = Math.random() * 2 - 1;
    const nzR = Math.random() * 2 - 1;
    lpL += a * (nzL - lpL);
    lpR += a * (nzR - lpR);
    const env = w * w;
    addStereo(L, R, s0 + i, lpL * env * g, lpR * env * g);
  }
}

// snare roll (16th-note ramping snares — the build-up classic)
function snareRoll(buf, t0, bars, baseGain = 0.4) {
  const total = bars * 16;
  for (let e = 0; e < total; e++) {
    const t = t0 + e * sx;
    const w = e / total;
    snareBrk(buf, t, baseGain + 0.5 * w);
  }
}

// ── arrange ───────────────────────────────────────────────────────────
// section sizes (in bars)
const SECTION_BARS = {
  intro: 8,
  buildA: 4,
  drop1: 16,
  breakA: 8,
  drop2: 16,
  breakB: 8,
  drop3: 16,
  outro: 12,
};
const totalBars = Object.values(SECTION_BARS).reduce((s, v) => s + v, 0); // 88
const totalSec = totalBars * bar + 4.0; // +tail for reverb/strings/healing ring-out
const N = Math.floor(totalSec * SR);

// stereo buses — bus is everything ducked by sidechain; drm is the
// dry kicks+snares which bypass the duck (they're the punch). revL/revR
// is the reverb send (brass/flugel fanfares + bells route here).
const busL = new Float32Array(N);
const busR = new Float32Array(N);
const drm  = new Float32Array(N);
const revL = new Float32Array(N);
const revR = new Float32Array(N);
// vocal bus — NOT ducked by the sidechain, summed clean after the pump so
// jeffrey + the apple say layer cut through the dense jungle mix.
const vocL = new Float32Array(N);
const vocR = new Float32Array(N);
const kickTimes = [];

// ── fanfare helpers ──────────────────────────────────────────────────
// Multi-note "ta-da-DAAAH" pattern using a brass/flugel one-shot
// pitched up to outline the F minor triad (F-Ab-C). Hits dry on
// busL/busR + sends to the reverb bus for a hall tail.
function brassFanfare(L, R, rL, rR, smp, t0, gain) {
  if (JUNGLE) return; // cheesy orchestra-hit fanfares replaced by animal stabs
  // Brass sample = E2; rates: F=+1st, Ab=+4st, C=+8st
  const rF  = Math.pow(2, 1 / 12);
  const rAb = Math.pow(2, 4 / 12);
  const rC  = Math.pow(2, 8 / 12);
  const hits = [
    { t: t0,            rate: rF,  g: gain * 0.55, pan: 0.2 },
    { t: t0 + sx * 2,   rate: rAb, g: gain * 0.65, pan: -0.2 },
    { t: t0 + sx * 4,   rate: rC,  g: gain * 1.00, pan: 0.4 },
  ];
  for (const h of hits) {
    playSampleStereo(L,  R,  smp, h.t, h.g,        h.rate, h.pan);
    playSampleStereo(rL, rR, smp, h.t, h.g * 0.78, h.rate, 0);
  }
}

// Flugel fanfare — uses the flugelhorn-asharp sample (A#2). Plays the
// same triad ascent but at a slightly different timing for variation.
function flugelFanfare(L, R, rL, rR, smp, t0, gain) {
  if (JUNGLE) return; // replaced by animal stabs in jungle mode
  // Flugel = A#2. Triad ascent over F minor: F-Ab-C means relative to A#:
  // F = -4st, Ab = -1st, C = +3st. To keep direction ascending, use
  // Ab-C-F (= -1, +3, +8 from A#).
  const rAb = Math.pow(2, -1 / 12);
  const rC  = Math.pow(2,  3 / 12);
  const rF  = Math.pow(2,  8 / 12);
  const hits = [
    { t: t0,            rate: rAb, g: gain * 0.55, pan: -0.2 },
    { t: t0 + sx * 2,   rate: rC,  g: gain * 0.65, pan: 0.2 },
    { t: t0 + sx * 4,   rate: rF,  g: gain * 1.00, pan: -0.35 },
  ];
  for (const h of hits) {
    playSampleStereo(L,  R,  smp, h.t, h.g,        h.rate, h.pan);
    playSampleStereo(rL, rR, smp, h.t, h.g * 0.78, h.rate, 0);
  }
}


// load orchestral hits + jeffrey vocal stabs from hellsine/samples/
const SAMPLES = resolve(HERE, "../../hellsine/samples");
const brass    = loadWav(resolve(SAMPLES, "brass-strong-e2.wav"));
const flugel   = loadWav(resolve(SAMPLES, "flugelhorn-asharp.wav"));
const bell     = loadWav(resolve(SAMPLES, "church-bell.wav"));

// Softer, more harmonic hoover voicing — less buzz, less drive, more
// sub warmth, smoother detune. Used for every stab in the track.
const HOOVER_SOFT = {
  drive: 1.45,
  pulseGain: 0.22,
  q: 2.6,
  cutoff: 5.4,
  subGain: 0.46,
  voices: 7,
  detune: 14,
  pwmRate: 1.6,
  whoop: [-3, 0.07],
};

// F minor harmony: stab roots (one per bar), pad voicing (Fm = F Ab C)
const FM_ROOTS = [53, 51, 49, 48]; // F3, Eb3, Db3, C3 (matrix descent)
const FM_PAD   = [41, 53, 56, 60]; // F2 F3 Ab3 C4 (open voicing)
const EB_PAD   = [39, 51, 54, 58]; // Eb2 Eb3 Gb3 Bb3
const DB_PAD   = [37, 49, 52, 56]; // Db2 Db3 E3  Ab3
const C_PAD    = [36, 48, 51, 55]; // C2  C3  Eb3 G3

let cursor = 0;

// ── INTRO (8 bars) — sub pulse + sparse hats + pad swell ──────────────
{
  const t0 = cursor * bar;
  // pad chord — Fm held for 4 bars then drops to Eb
  darkPad(busL, busR, t0, FM_PAD, bar * 4 - 0.05, 0.16);
  darkPad(busL, busR, t0 + bar * 4, EB_PAD, bar * 4 - 0.05, 0.18);
  // sub pulse — quarter notes on root, half volume so it just throbs
  for (let b = 0; b < 8; b++) {
    const root = b < 4 ? 29 : 27; // F1 then Eb1
    for (let q = 0; q < 4; q++) {
      subBass(busL, t0 + (b * 4 + q) * beat, root, beat * 0.9, 0.18);
      // duplicate to R via L for mono sub
      subBass(busR, t0 + (b * 4 + q) * beat, root, beat * 0.9, 0.18);
    }
  }
  // sparse hats — open on the off-beat, every other bar. Tight (mostly
  // centered, very slight pan) so the intro doesn't feel washy.
  for (let b = 0; b < 8; b++) {
    if (b % 2 === 1) {
      for (let e = 0; e < 16; e++) {
        if (e === 6 || e === 14) {
          tightHatStereo(busL, busR, t0 + b * bar + e * sx, "open", 0.12, e === 6 ? -0.08 : 0.08);
        }
      }
    }
  }
  // an opening brass fanfare pp — sets the tone (intro statement)
  if (brass) brassFanfare(busL, busR, revL, revR, brass, t0 + bar * 4, 0.20);
}
cursor += SECTION_BARS.intro;

// ── BUILD A (4 bars) — pad rises, snare roll, riser ───────────────────
{
  const t0 = cursor * bar;
  darkPad(busL, busR, t0, DB_PAD, bar * 2 - 0.05, 0.22);
  darkPad(busL, busR, t0 + bar * 2, C_PAD, bar * 2 - 0.05, 0.26);
  // sub continues — busier
  for (let b = 0; b < 4; b++) {
    const root = b < 2 ? 25 : 24; // Db1 then C1
    for (let q = 0; q < 4; q++) {
      subBass(busL, t0 + (b * 4 + q) * beat, root, beat * 0.9, 0.22);
      subBass(busR, t0 + (b * 4 + q) * beat, root, beat * 0.9, 0.22);
    }
  }
  // snare roll across the last 2 bars
  snareRoll(drm, t0 + bar * 2, 2, 0.32);
  // riser on the last bar
  riser(busL, busR, t0 + bar * 3, bar - 0.02, 0.34);
  // reverse swell across the whole build (4 bars)
  reverseSwell(busL, busR, t0, bar * 4 - 0.02, 0.20);
}
cursor += SECTION_BARS.buildA;

// ── DROP 1 (16 bars) — full kit + sub + hoover stabs + vocal "we!" ───
{
  for (let b = 0; b < 16; b++) {
    const t0 = (cursor + b) * bar;
    breakBar(busL, busR, drm, t0, 1.0);
    percGroove(busL, busR, revL, revR, t0, 0.85);
    kickTimes.push(t0, t0 + 3 * sx, t0 + 10 * sx);
    // sub follows the root (one per beat)
    const root = 29 + (FM_ROOTS[b % 4] - FM_ROOTS[0]); // mirror the stab walk
    for (let q = 0; q < 4; q++) {
      subBass(busL, t0 + q * beat, root, beat * 0.9, 0.30);
      subBass(busR, t0 + q * beat, root, beat * 0.9, 0.30);
    }
    // hoover stab on beat 3 — softer, more harmonic. Render root + 5th
    // above as separate voices so the lead has built-in harmony.
    const stabMidi = FM_ROOTS[b % FM_ROOTS.length];
    for (const off of [0, 7]) {
      const stabBuf = new Float32Array(N);
      mixEventHoover(
        { startSec: t0 + beat * 2, midi: stabMidi + off, durSec: beat * 1.5, gain: off === 0 ? 0.32 : 0.20 },
        stabBuf, { sampleRate: SR, preset: "stab", params: HOOVER_SOFT },
      );
      const s0 = Math.floor((t0 + beat * 2) * SR);
      const haas = Math.floor(0.009 * SR);
      const endX = Math.min(N, s0 + Math.floor(beat * 2 * SR));
      // root sits left/center, 5th leans right
      const lg = off === 0 ? 1.0 : 0.6;
      const rg = off === 0 ? 0.75 : 1.0;
      for (let i = s0; i < endX; i++) {
        const v = stabBuf[i];
        if (v) {
          busL[i] += v * lg;
          if (i + haas < N) busR[i + haas] += v * rg;
        }
      }
    }
    // acid 303 enters here — quieter, single notes pulsing under the kit.
    // The squelch begins early so the build → drop1 lift has continuity.
    {
      const pent = [53, 56, 58, 60, 63, 60, 58, 56];
      for (let e = 0; e < 16; e++) {
        if (e % 2 === 0) {
          const midi = pent[e % pent.length] - 12;
          const tmp = new Float32Array(N);
          acid303(tmp, t0 + e * sx, midi, sx * 0.85, {
            gain: 0.14, accent: e % 4 === 0,
          });
          // harmony — a 5th above on the accent notes only
          const tmpH = new Float32Array(N);
          if (e % 4 === 0) {
            acid303(tmpH, t0 + e * sx, midi + 7, sx * 0.85, {
              gain: 0.09, accent: true,
            });
          }
          const sx0 = Math.floor((t0 + e * sx) * SR);
          const h = Math.floor(0.006 * SR);
          const ex = Math.min(N, sx0 + Math.floor(sx * SR * 1.2));
          for (let i = sx0; i < ex; i++) {
            const v = tmp[i], vh = tmpH[i];
            if (v || vh) {
              busL[i] += v + vh * 0.6;
              if (i + h < N) busR[i + h] += v + vh;
            }
          }
        }
      }
    }
    // brass fanfare on bar 1, 5, 9, 13 — three-note orchestral statement
    if (b % 4 === 0 && brass) {
      brassFanfare(busL, busR, revL, revR, brass, t0, 0.40);
    }
  }
}
cursor += SECTION_BARS.drop1;

// ── BREAK A (8 bars) — kick out, 303 acid + flugelhorn ────────────────
{
  for (let b = 0; b < 8; b++) {
    const t0 = (cursor + b) * bar;
    // sunk-in shaker groove keeps the pulse alive while the kick is out
    percGroove(busL, busR, revL, revR, t0, 0.55, { base: 0.045, revSend: 0.5 });
    // sparse hats — tight, slight pan only
    for (let e = 0; e < 16; e++) {
      if (e % 4 === 2) {
        tightHatStereo(busL, busR, t0 + e * sx, "open", 0.14, (e % 8 === 2 ? -0.1 : 0.1));
      }
    }
    // snare on 2 and 4 keeps pulse
    snareBrk(drm, t0 + beat, 0.5);
    snareBrk(drm, t0 + beat * 3, 0.5);
    // 303 squelch — 16th F minor pentatonic walk, with 5th-above harmony
    // on the accent beats (the matrix-tinge thirds/fifths).
    const pent = [53, 56, 58, 60, 63, 60, 58, 56];
    for (let e = 0; e < 16; e++) {
      const midi = pent[e % pent.length] + (b % 2 === 0 ? 0 : -12);
      const slide = e > 0 && e % 4 === 0 ? pent[(e - 1) % pent.length] : null;
      const accent = e % 4 === 0;
      const tmp = new Float32Array(N);
      acid303(tmp, t0 + e * sx, midi, sx * 0.9, {
        gain: 0.26, accent, slideFrom: slide,
      });
      // harmonic voice — 5th up on accents, 3rd up otherwise (softer)
      const tmpH = new Float32Array(N);
      const harmInt = accent ? 7 : 3;
      acid303(tmpH, t0 + e * sx, midi + harmInt, sx * 0.9, {
        gain: accent ? 0.16 : 0.08, accent: accent,
      });
      const s0 = Math.floor((t0 + e * sx) * SR);
      const haas = Math.floor(0.005 * SR);
      const endX = Math.min(N, s0 + Math.floor(sx * SR * 1.2));
      for (let i = s0; i < endX; i++) {
        const v = tmp[i], vh = tmpH[i];
        if (v || vh) {
          busL[i] += v + vh * 0.55;
          if (i + haas < N) busR[i + haas] += v + vh;
        }
      }
    }
    // pad continues in break — dark
    if (b === 0) darkPad(busL, busR, t0, FM_PAD, bar * 4 - 0.05, 0.16);
    if (b === 4) darkPad(busL, busR, t0, EB_PAD, bar * 4 - 0.05, 0.16);
    // flugelhorn fanfare at the start of bar 5
    if (b === 4 && flugel) {
      flugelFanfare(busL, busR, revL, revR, flugel, t0, 0.46);
    }
  }
  // riser across the last 2 bars to set up drop 2
  riser(busL, busR, (cursor + 6) * bar, bar * 2 - 0.05, 0.38);
}
cursor += SECTION_BARS.breakA;

// ── DROP 2 (16 bars) — full kit + 303 under + hoover + vocal "you!" ──
{
  for (let b = 0; b < 16; b++) {
    const t0 = (cursor + b) * bar;
    breakBar(busL, busR, drm, t0, 1.0);
    percGroove(busL, busR, revL, revR, t0, 1.0);
    kickTimes.push(t0, t0 + 3 * sx, t0 + 10 * sx);
    // sub
    const root = 29 + (FM_ROOTS[b % 4] - FM_ROOTS[0]);
    for (let q = 0; q < 4; q++) {
      subBass(busL, t0 + q * beat, root, beat * 0.9, 0.32);
      subBass(busR, t0 + q * beat, root, beat * 0.9, 0.32);
    }
    // hoover stab — soft, harmonic (root + 5th above)
    const stabMidi = FM_ROOTS[b % FM_ROOTS.length];
    for (const off of [0, 7]) {
      const stabBuf = new Float32Array(N);
      mixEventHoover(
        { startSec: t0 + beat * 2, midi: stabMidi + off, durSec: beat * 1.5, gain: off === 0 ? 0.36 : 0.22 },
        stabBuf, { sampleRate: SR, preset: "stab", params: HOOVER_SOFT },
      );
      const s0 = Math.floor((t0 + beat * 2) * SR);
      const haas = Math.floor(0.009 * SR);
      const endX = Math.min(N, s0 + Math.floor(beat * 2 * SR));
      const lg = off === 0 ? 1.0 : 0.6;
      const rg = off === 0 ? 0.75 : 1.0;
      for (let i = s0; i < endX; i++) {
        const v = stabBuf[i];
        if (v) {
          busL[i] += v * lg;
          if (i + haas < N) busR[i + haas] += v * rg;
        }
      }
    }
    // 303 continues — squelch under, with harmonic interval on accents
    const pent = [53, 56, 58, 60, 63, 60, 58, 56];
    for (let e = 0; e < 16; e++) {
      if (e % 2 === 0) {
        const midi = pent[e % pent.length] - 12;
        const tmp = new Float32Array(N);
        acid303(tmp, t0 + e * sx, midi, sx * 0.85, {
          gain: 0.18, accent: e % 4 === 0,
        });
        const tmpH = new Float32Array(N);
        const harmInt = e % 4 === 0 ? 7 : 3;
        acid303(tmpH, t0 + e * sx, midi + harmInt, sx * 0.85, {
          gain: e % 4 === 0 ? 0.10 : 0.06,
        });
        const sx0 = Math.floor((t0 + e * sx) * SR);
        const h = Math.floor(0.006 * SR);
        const ex = Math.min(N, sx0 + Math.floor(sx * SR * 1.2));
        for (let i = sx0; i < ex; i++) {
          const v = tmp[i], vh = tmpH[i];
          if (v || vh) {
            busL[i] += v + vh * 0.55;
            if (i + h < N) busR[i + h] += v + vh;
          }
        }
      }
    }
    // brass fanfare on bar 1
    if (b === 0 && brass) {
      brassFanfare(busL, busR, revL, revR, brass, t0, 0.40);
    }
    // flugel fanfare on bar 5
    if (b === 4 && flugel) {
      flugelFanfare(busL, busR, revL, revR, flugel, t0, 0.40);
    }
    // bell on bar 9 — also routed to reverb for the hall peal
    if (b === 8 && bell) {
      playSampleStereo(busL, busR, bell, t0, 0.30, 1.0, 0.5);
      playSampleStereo(revL, revR, bell, t0, 0.20, 1.0, 0);
    }
  }
}
cursor += SECTION_BARS.drop2;

// ── BREAK B (8 bars) — atmospheric: pad + bell + swell ────────────────
{
  const t0 = cursor * bar;
  darkPad(busL, busR, t0, DB_PAD, bar * 4 - 0.05, 0.22);
  darkPad(busL, busR, t0 + bar * 4, C_PAD, bar * 4 - 0.05, 0.22);
  // distant shaker haze — very sunk, builds slightly over the 8 bars
  for (let b = 0; b < 8; b++) {
    percGroove(busL, busR, revL, revR, t0 + b * bar, 0.35 + b * 0.04,
               { base: 0.04, revSend: 0.6, swing: 0.022 });
  }
  // sub drops in once per bar — half-time pulse
  for (let b = 0; b < 8; b++) {
    const root = b < 4 ? 25 : 24;
    subBass(busL, t0 + b * bar, root, bar * 0.85, 0.26);
    subBass(busR, t0 + b * bar, root, bar * 0.85, 0.26);
  }
  // bell on bar 1 + 5 — with reverb send for hall sustain
  if (bell) {
    playSampleStereo(busL, busR, bell, t0, 0.32, 1.0, 0.55);
    playSampleStereo(revL, revR, bell, t0, 0.24, 1.0, 0);
    playSampleStereo(busL, busR, bell, t0 + bar * 4, 0.28, 1.05, 0.55);
    playSampleStereo(revL, revR, bell, t0 + bar * 4, 0.22, 1.05, 0);
  }
  // reverse swell building under the last 4 bars
  reverseSwell(busL, busR, t0 + bar * 4, bar * 4 - 0.05, 0.30);
  // snare roll across last 2 bars
  snareRoll(drm, t0 + bar * 6, 2, 0.30);
  // riser final bar
  riser(busL, busR, t0 + bar * 7, bar - 0.02, 0.40);
}
cursor += SECTION_BARS.breakB;

// ── DROP 3 (16 bars) — biggest: everything stacks ─────────────────────
{
  for (let b = 0; b < 16; b++) {
    const t0 = (cursor + b) * bar;
    breakBar(busL, busR, drm, t0, 1.05);
    percGroove(busL, busR, revL, revR, t0, 1.15, { base: 0.06, revSend: 0.42 });
    kickTimes.push(t0, t0 + 3 * sx, t0 + 10 * sx);
    // sub — louder
    const root = 29 + (FM_ROOTS[b % 4] - FM_ROOTS[0]);
    for (let q = 0; q < 4; q++) {
      subBass(busL, t0 + q * beat, root, beat * 0.9, 0.36);
      subBass(busR, t0 + q * beat, root, beat * 0.9, 0.36);
    }
    // Soft hoover stab — root + 5th + octave (lush triad, no buzz).
    const stabMidi = FM_ROOTS[b % FM_ROOTS.length];
    const stabVoices = [
      { off: 0,  gL: 1.00, gR: 0.72, gain: 0.40 },
      { off: 7,  gL: 0.62, gR: 1.00, gain: 0.24 },
      { off: 12, gL: 0.55, gR: 0.55, gain: 0.18 },
    ];
    for (const sv of stabVoices) {
      const stabBuf = new Float32Array(N);
      mixEventHoover(
        { startSec: t0 + beat * 2, midi: stabMidi + sv.off, durSec: beat * 1.5, gain: sv.gain },
        stabBuf, { sampleRate: SR, preset: "stab", params: HOOVER_SOFT },
      );
      const s0 = Math.floor((t0 + beat * 2) * SR);
      const haas = Math.floor(0.009 * SR);
      const endX = Math.min(N, s0 + Math.floor(beat * 2 * SR));
      for (let i = s0; i < endX; i++) {
        const v = stabBuf[i];
        if (v) {
          busL[i] += v * sv.gL;
          if (i + haas < N) busR[i + haas] += v * sv.gR;
        }
      }
    }
    // 303 riding under, with a 5th-above harmony on the accent
    if (b % 2 === 0) {
      const pent = [53, 56, 58, 60, 63];
      const midi = pent[b % pent.length] - 12;
      const tmp = new Float32Array(N);
      acid303(tmp, t0 + beat * 2.5, midi, beat * 0.5, {
        gain: 0.22, accent: true,
      });
      const tmpH = new Float32Array(N);
      acid303(tmpH, t0 + beat * 2.5, midi + 7, beat * 0.5, {
        gain: 0.12, accent: true,
      });
      const sx0 = Math.floor((t0 + beat * 2.5) * SR);
      const h = Math.floor(0.006 * SR);
      const ex = Math.min(N, sx0 + Math.floor(beat * SR));
      for (let i = sx0; i < ex; i++) {
        const v = tmp[i], vh = tmpH[i];
        if (v || vh) {
          busL[i] += v + vh * 0.55;
          if (i + h < N) busR[i + h] += v + vh;
        }
      }
    }
    // brass fanfare on bar 1, 9 + flugel fanfare on bar 5, 13 (with reverb)
    if ((b === 0 || b === 8) && brass) {
      brassFanfare(busL, busR, revL, revR, brass, t0, 0.45);
    }
    if ((b === 4 || b === 12) && flugel) {
      flugelFanfare(busL, busR, revL, revR, flugel, t0, 0.40);
    }
  }
}
cursor += SECTION_BARS.drop3;

// ── OUTRO (12 bars) — break decays, sub fades, bell + pad wash ───────
{
  for (let b = 0; b < 12; b++) {
    const t0 = (cursor + b) * bar;
    // break energy ramps from 1.0 → 0.2 over 12 bars
    const energy = Math.max(0.15, 1.0 - b * 0.075);
    breakBar(busL, busR, drm, t0, energy);
    percGroove(busL, busR, revL, revR, t0, energy, { revSend: 0.45 });
    kickTimes.push(t0, t0 + 3 * sx, t0 + 10 * sx);
    // sub fades same curve
    const root = b < 6 ? 29 : 24;
    for (let q = 0; q < 4; q++) {
      subBass(busL, t0 + q * beat, root, beat * 0.9, 0.30 * energy);
      subBass(busR, t0 + q * beat, root, beat * 0.9, 0.30 * energy);
    }
  }
  // pad wash through the whole outro
  const t0 = cursor * bar;
  darkPad(busL, busR, t0, FM_PAD, bar * 6 - 0.05, 0.20);
  darkPad(busL, busR, t0 + bar * 6, C_PAD, bar * 6 - 0.05, 0.18);
  // final bell on bar 11 (the close) — long reverb tail
  if (bell) {
    playSampleStereo(busL, busR, bell, t0 + bar * 10, 0.45, 0.95, 0.6);
    playSampleStereo(revL, revR, bell, t0 + bar * 10, 0.45, 0.95, 0);
  }
}

// ── SAMPLED-INSTRUMENT LAYER — Freesound one-shots played over the
// F-minor note data ("sampled insts over MIDI"). Loads pop/maytrax/kit.json
// (fetch-kit.mjs): taiko under the kicks, gong on drop downbeats, the iconic
// rave ORCHESTRA HIT pitched to the FM_ROOTS stab walk, and a choir "aah"
// pad on the breaks + final drop. Additive into the buses; toggle with
// --no-samples. Pitched roles transpose by resampling from their kit `root`.
if (!flags["no-samples"]) {
  const KIT_PATH = resolve(HERE, "..", "kit.json");
  const kit = existsSync(KIT_PATH) ? JSON.parse(readFileSync(KIT_PATH, "utf8")) : {};
  const SMP = {};
  for (const role of Object.keys(kit)) { const s = loadWav(kit[role].path); if (s) SMP[role] = { ...kit[role], smp: s }; }
  // reversed variants (genre-confusing risers): reverse-wolf + reverse-animal
  const reverseSamples = (sm) => { const o = new Float32Array(sm.samples.length); for (let i = 0; i < o.length; i++) o[i] = sm.samples[sm.samples.length - 1 - i]; return { samples: o, sampleRate: sm.sampleRate }; };
  if (SMP.wolf) SMP.wolf_rev = { ...SMP.wolf, smp: reverseSamples(SMP.wolf.smp) };
  if (SMP.animal) SMP.animal_rev = { ...SMP.animal, smp: reverseSamples(SMP.animal.smp) };
  const have = Object.keys(SMP);
  if (have.length) {
    console.log(`• sampled insts: ${have.join(", ")}`);
    const NN = { C: 0, D: 2, E: 4, F: 5, G: 7, A: 9, B: 11 };
    const noteToMidi = (s) => { const m = /^([A-G])([#b]?)(-?\d)$/.exec(s || "C4"); if (!m) return 60; return NN[m[1]] + (m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0) + (parseInt(m[3], 10) + 1) * 12; };
    const rateFor = (role, note) => Math.pow(2, (note - noteToMidi(SMP[role]?.root || "C4")) / 12);
    // section start bars (derived from SECTION_BARS so it stays in sync)
    const order = ["intro", "buildA", "drop1", "breakA", "drop2", "breakB", "drop3", "outro"];
    const startBar = {}; { let c = 0; for (const k of order) { startBar[k] = c; c += SECTION_BARS[k]; } }
    const secOfBar = []; for (const k of order) for (let i = 0; i < SECTION_BARS[k]; i++) secOfBar.push(k);

    // ── FAINT AMBIENCE BED — rainstorm + jungle + crickets (meadow). Loud-ish
    // and lonely at the very start, then ducked to a faint bed under the
    // track, and back up at the tail so it loops into the quiet opening.
    {
      const amb = (role, g, t) => SMP[role] && playSampleStereo(busL, busR, SMP[role].smp, t, g, 1, (Math.random() * 2 - 1) * 0.3);
      // opener — present (the track emerges from a rainy jungle meadow)
      amb("rain", 0.22, 0); amb("jungle", 0.16, 0); amb("cricket", 0.12, 0.5);
      // faint bed re-triggered across the track so it never disappears
      for (let t = 8; t < totalBars * bar; t += 18) { amb("rain", 0.05, t); if (Math.random() < 0.5) amb("cricket", 0.04, t + 4); }
      // tail — bring the meadow back for the loop point
      amb("rain", 0.2, (totalBars - 10) * bar); amb("cricket", 0.14, (totalBars - 9) * bar); amb("jungle", 0.12, (totalBars - 8) * bar);
    }

    // ── PERC + HATS THROUGHOUT — cowbell, toms, clicks, closed/open hats run
    // across the WHOLE track (humanized + lazy), sparser in quiet sections so
    // the groove keeps ticking even between the drops.
    {
      const lag = 0.010 * LAZY;
      for (let bAbs = 0; bAbs < totalBars; bAbs++) {
        const sec = secOfBar[bAbs] || "drop1";
        const inDrop = sec.startsWith("drop"), quiet = sec === "intro" || sec === "outro";
        const dens = inDrop ? 1 : sec.startsWith("break") ? 0.55 : quiet ? 0.3 : 0.7;
        const sw = (s) => (s % 2 ? 0.020 * LAZY : 0);
        for (let s = 0; s < 16; s++) {
          const t = bAbs * bar + s * sx + lag + sw(s) + hz();
          // steady closed hats on the offbeats (regular groove), light accent
          if (SMP.hat && s % 2 === 1)
            playSampleStereo(busL, busR, SMP.hat.smp, t, (s % 4 === 3 ? 0.075 : 0.05) * dens, 1, 0.2, 0.05 * DEC);
          // sparse clicks on a fixed grid (no random scatter)
          if (SMP.click && inDrop && s % 8 === 5)
            playSampleStereo(busL, busR, SMP.click.smp, t, 0.05 * dens, 1, 0.4, 0.04 * DEC);
        }
        if (SMP.ohat && Math.random() < 0.7 * dens)
          playSampleStereo(busL, busR, SMP.ohat.smp, bAbs * bar + 6 * sx + lag + hz(), 0.07 * dens, 1, 0.25, 0.12 * DEC);
        if (SMP.cowbell && inDrop) for (const s of [7, 14]) if (Math.random() < 0.22 * dens)
          playSampleStereo(busL, busR, SMP.cowbell.smp, bAbs * bar + s * sx + lag + hz(), 0.07 * dens, 1, (Math.random() * 2 - 1) * 0.4, 0.14 * DEC);
        if (SMP.tom && bAbs % 4 === 3 && !quiet) // descending tom fill at phrase ends
          [10, 12, 14].forEach((s, k) => playSampleStereo(busL, busR, SMP.tom.smp, bAbs * bar + s * sx + lag + hz(), 0.2 * dens, Math.pow(2, -k / 6), (k - 1) * 0.3, 0.6 * DEC));
      }
    }
    const drops = [["drop1", 16], ["drop2", 16], ["drop3", 16]];
    const breaks = [["breakA", 8], ["breakB", 8]];

    for (const [name, bars] of drops) {
      const s0 = startBar[name];
      // gong swell on the very first downbeat of the drop (wet, tightened)
      if (SMP.gong) { const t = s0 * bar; playSampleStereo(busL, busR, SMP.gong.smp, t, 0.34, 1, 0.5, 1.2 * DEC); playSampleStereo(revL, revR, SMP.gong.smp, t, 0.30, 1, 0, 1.2 * DEC); }
      for (let b = 0; b < bars; b++) {
        const t0 = (s0 + b) * bar, root = FM_ROOTS[b % 4];
        // taiko reinforcing beats 1 & 3 (the thump), native pitch, tight
        if (SMP.taiko) { playSampleStereo(busL, busR, SMP.taiko.smp, t0, 0.30, 1, 0.15, 0.35 * DEC); playSampleStereo(busL, busR, SMP.taiko.smp, t0 + 2 * beat, 0.24, 1, -0.15, 0.35 * DEC); }
        // FLUTEY BLEEP-BLOPS — the RIFF, but processed: harmonized flute
        // chord-blips pitched up high + tight, mixed LOW (hidden under the
        // jeffrey vocals). A faint reversed-animal blip adds genre-confusing
        // texture, and a reversed-wolf howl swells UP into each phrase start.
        const phraseStart = b % 4 === 0;
        if (SMP.lead) {
          const steps = name === "drop3" ? [0, 3, 6, 10, 13] : [0, 6, 10];
          const BLEEP = [[12, 0.5], [15, 0.3], [19, 0.4], [24, 0.2]]; // Fm chord, up high
          for (const st of steps) {
            const t = t0 + st * sx, g = (st === 0 ? 0.22 : 0.15); // hidden in the mix
            for (const [semi, hg] of BLEEP)
              playSampleStereo(busL, busR, SMP.lead.smp, t, g * hg, rateFor("lead", root + semi), (Math.random() * 2 - 1) * 0.55, 0.10 * DEC);
            playSampleStereo(revL, revR, SMP.lead.smp, t, g * 0.3, rateFor("lead", root + 12), 0, 0.13 * DEC);
            // faint reversed-animal blip (texture/confusion)
            if (SMP.animal_rev && Math.random() < 0.5)
              playSampleStereo(busL, busR, SMP.animal_rev.smp, t, 0.1, rateFor("animal", root + 12), (Math.random() * 2 - 1) * 0.6, 0.12 * DEC);
          }
        }
        // REVERSE WOLF HOWL — pitched to the lead, swelling UP so it lands on
        // the phrase-start downbeat (a haunting genre-bending riser).
        if (SMP.wolf_rev && phraseStart) {
          const hold = name === "drop3" ? 2.6 : 2.0, lead = root + 12;
          playSampleStereo(busL, busR, SMP.wolf_rev.smp, t0 - hold, 0.32, rateFor("wolf_rev", lead), 0.2, hold);
          playSampleStereo(revL, revR, SMP.wolf_rev.smp, t0 - hold, 0.2, rateFor("wolf_rev", lead), 0, hold);
        }
        // ANIMAL ROAR is BACK — the reverse-wolf swells up, then a big low
        // jaguar roar LANDS on the phrase downbeat, plus the odd forward
        // roar-stab for the rock factor (flutey blips stay as texture).
        if (SMP.animal) {
          if (phraseStart) {
            // pitched 2 octaves down → a long, deep, stretched-out roar
            const rl = rateFor("animal", root - 24), hold = name === "drop3" ? 5.5 : 4.5;
            playSampleStereo(busL, busR, SMP.animal.smp, t0, 0.54, rl, 0.2, hold);
            playSampleStereo(revL, revR, SMP.animal.smp, t0, 0.24, rl, 0, hold);
            playSampleStereo(busL, busR, SMP.animal.smp, t0, 0.34, rateFor("animal", root), -0.2, 0.9 * DEC); // mid body
          } else if (Math.random() < 0.45) {
            playSampleStereo(busL, busR, SMP.animal.smp, t0 + 6 * sx, 0.42, rateFor("animal", root), (Math.random() * 2 - 1) * 0.4, 0.5 * DEC);
          }
        }
      }
      // choir pad sustained through the biggest drop
      if (SMP.choir && name === "drop3") {
        for (let b = 0; b < bars; b += 2) {
          const t0 = (s0 + b) * bar;
          for (const n of [FM_PAD[1], FM_PAD[2], FM_PAD[3]]) {
            playSampleStereo(busL, busR, SMP.choir.smp, t0, 0.07, rateFor("choir", n), 0.4);
            playSampleStereo(revL, revR, SMP.choir.smp, t0, 0.05, rateFor("choir", n), 0);
          }
        }
      }
    }
    // choir "aah" pad over the breaks (the matrix vowel wash)
    if (SMP.choir) for (const [name, bars] of breaks) {
      const s0 = startBar[name];
      for (let b = 0; b < bars; b += 2) {
        const t0 = (s0 + b) * bar;
        for (const n of [FM_PAD[1], FM_PAD[2], FM_PAD[3]]) {
          playSampleStereo(busL, busR, SMP.choir.smp, t0, 0.08, rateFor("choir", n), 0.45);
          playSampleStereo(revL, revR, SMP.choir.smp, t0, 0.06, rateFor("choir", n), 0);
        }
      }
    }

    // ── PIRATE-SHANTY OPENER — the whole thing opens with a jaunty chiptune
    // square-wave melody in F minor (genre bait-and-switch before the jungle
    // hits). 4-bar phrase, played twice across the 8-bar intro.
    {
      const s0 = startBar["intro"];
      const SHANTY = [
        [0, 65, 0.5], [0.5, 65, 0.5], [1, 68, 0.5], [1.5, 72, 0.5], [2, 72, 1], [3, 70, 1],
        [4, 68, 0.5], [4.5, 68, 0.5], [5, 67, 0.5], [5.5, 65, 0.5], [6, 63, 1.5], [7.5, 65, 0.5],
        [8, 65, 0.5], [8.5, 68, 0.5], [9, 72, 0.5], [9.5, 75, 0.5], [10, 73, 1], [11, 72, 1],
        [12, 68, 0.5], [12.5, 67, 0.5], [13, 65, 0.5], [13.5, 63, 0.5], [14, 65, 2],
      ];
      // lead with the FLUTE carrying the shanty — but SLOWER (timing ×2,
      // spanning the whole intro) and an octave LOWER, with reverb + space so
      // it's a slow, low, atmospheric opener. Square = faint chiptune ghost.
      for (const [bt, midi, dur] of SHANTY) {
        const t = (s0 * bar) + bt * 2 * beat + 0.012 * LAZY + hz();
        const m = midi - 12, ring = Math.max(1.1, dur * 2 * beat);
        if (SMP.lead) {
          playSampleStereo(busL, busR, SMP.lead.smp, t, 0.3, rateFor("lead", m), 0.25, ring * 0.95);
          playSampleStereo(revL, revR, SMP.lead.smp, t, 0.26, rateFor("lead", m), 0, ring); // wash/space
        }
        squareNote(busL, busR, m, t, dur * 2 * beat * 0.9, 0.05, (Math.random() * 2 - 1) * 0.25); // faint ghost
      }
      // a low flute drone under it grounds the key (matches the strings/pads)
      if (SMP.lead) for (const bo of [0, 4]) {
        const t = (s0 + bo) * bar;
        playSampleStereo(busL, busR, SMP.lead.smp, t, 0.16, rateFor("lead", 41), 0.1, 3.0); // F2 pad-ish
        playSampleStereo(revL, revR, SMP.lead.smp, t, 0.12, rateFor("lead", 41), 0, 3.0);
      }
    }

    // ── JOHN WILLIAMS STRINGS — long, deep, cinematic Fm swells. They live
    // on the ducked music bus, so they're side-chained to the kick (pump).
    if (SMP.strings) {
      const strSegs = [["intro", 8], ["breakA", 8], ["drop2", 16], ["breakB", 8], ["drop3", 16], ["outro", 12]];
      const chord = [41, 48, 56]; // F2 C3 Ab3 — deep Fm spread
      for (const [name, bars] of strSegs) {
        const s0 = startBar[name];
        for (let b = 0; b < bars; b += 4) {
          const t0 = (s0 + b) * bar, g = name.startsWith("drop") ? 0.13 : 0.16;
          for (const n of chord) {
            playSampleStereo(busL, busR, SMP.strings.smp, t0, g, rateFor("strings", n), 0.5, 6.0);
            playSampleStereo(revL, revR, SMP.strings.smp, t0, g * 0.5, rateFor("strings", n), 0, 6.0);
          }
        }
      }
    }

    // ── SAMPLED FLUTE LEAD — the MAIN VOICE. A jazzy F-minor melody played
    // on the Freesound flute, harmonized into octave + fifth + minor-third
    // stacks (humanized with a little timing/pitch jitter) so it sings like
    // a small flute section over the drops.
    if (SMP.lead) {
      // 4-bar phrase: [barInPhrase, beatOffset, midi] in F4–Eb5 (Fm-ish)
      const LEAD = [
        [0, 0, 65], [0, 2, 72],        // F4 … C5
        [1, 0, 75], [1, 1, 72], [1, 2, 68], // Eb5 C5 Ab4
        [2, 0, 65], [2, 2, 68],        // F4 Ab4
        [3, 0, 72],                    // C5 (hold)
      ];
      const HARM = [[0, 0.5], [12, 0.34], [7, 0.26], [3, 0.2]]; // unison + 8ve + 5th + m3
      const flute = (note, t, g, pan) => {
        for (const [semi, hg] of HARM) {
          const jt = (Math.random() * 2 - 1) * 0.012, jp = (Math.random() * 2 - 1) * 0.02; // humanize
          playSampleStereo(busL, busR, SMP.lead.smp, t + jt, g * hg, rateFor("lead", note + semi) * (1 + jp / 12 * 0.05), pan, 0.6 * DEC);
          playSampleStereo(revL, revR, SMP.lead.smp, t + jt, g * hg * 0.3, rateFor("lead", note + semi), 0, 0.6 * DEC);
        }
      };
      for (const [name] of drops) {
        const s0 = startBar[name], gain = name === "drop3" ? 0.34 : 0.28;
        for (let phr = 0; phr < 4; phr++) for (const [bo, beatO, midi] of LEAD)
          flute(midi, (s0 + phr * 4 + bo) * bar + beatO * beat, gain, (Math.random() * 2 - 1) * 0.3);
      }
    }
  }
}

// ── JEFFREY-PVC SHOUTS — slammed on the drop downbeats. Loads
// shouts.json (gen-shouts.mjs); ElevenLabs clips run quiet so each is
// peak-normalized, then thrown dry on the buses with a short reverb send.
// Toggle with --no-vocals.
if (!flags["no-vocals"]) {
  const SH_PATH = resolve(HERE, "..", "shouts.json");
  if (existsSync(SH_PATH)) {
    const sh = JSON.parse(readFileSync(SH_PATH, "utf8"));
    // load + peak-normalize a clip; `variant` picks jeffrey (.wav) or the
    // Apple-say sung layer (-say-sung.wav).
    const loadShout = (key, variant = "jeffrey") => {
      const e = sh[key]; if (!e) return null;
      const path = variant === "apple" ? e.path.replace(/\.wav$/, "-say-sung.wav") : e.path;
      if (!existsSync(path)) return null;
      const w = loadWav(path); if (!w) return null;
      let pk = 0; for (let i = 0; i < w.samples.length; i++) pk = Math.max(pk, Math.abs(w.samples[i]));
      const g = pk > 0 ? 0.97 / pk : 1, out = new Float32Array(w.samples.length);
      for (let i = 0; i < w.samples.length; i++) out[i] = w.samples[i] * g;
      return { samples: out, sampleRate: w.sampleRate };
    };
    const keyOf = { wake: "wake_up", real: "it_s_real", hold: "hold_on", let: "let_go", rabbit: "follow_the_white_rabbit", now: "now" };
    const J = {}, A = {};
    for (const [k, slug] of Object.entries(keyOf)) { J[k] = loadShout(slug, "jeffrey"); A[k] = loadShout(slug, "apple"); }
    console.log(`• shouts: jeffrey ${Object.values(J).filter(Boolean).length}/6 + apple ${Object.values(A).filter(Boolean).length}/6 → clean vocal bus`);
    const order = ["intro", "buildA", "drop1", "breakA", "drop2", "breakB", "drop3", "outro"];
    const startBar = {}; { let c = 0; for (const k of order) { startBar[k] = c; c += SECTION_BARS[k]; } }
    // route to the UN-DUCKED vocal bus, loud + present. jeffrey leads centre;
    // the Apple-say twin sits a touch quieter, panned + an octave context.
    // jeffrey leads LOUD + centre; the apple twin is a quieter side colour.
    const sing = (k, bAbs, g = 1.35, pan = 0) => {
      const t = bAbs * bar;
      if (J[k]) { playSampleStereo(vocL, vocR, J[k], t, g, 1, pan); playSampleStereo(revL, revR, J[k], t, g * 0.18, 1, 0); }
      if (A[k]) { playSampleStereo(vocL, vocR, A[k], t, g * 0.32, 1, -pan - 0.2); playSampleStereo(revL, revR, A[k], t, g * 0.14, 1, 0); }
    };
    // every 2 bars now (more frequent words), cycling the phrase
    for (const name of ["drop1", "drop3"]) {
      const s0 = startBar[name];
      ["wake", "real", "hold", "let", "wake", "real", "hold", "let"].forEach((k, i) => sing(k, s0 + i * 2, 1.35, i % 2 ? 0.12 : -0.12));
    }
    { const s0 = startBar["drop2"];
      ["rabbit", "now", "rabbit", "now", "rabbit", "now", "rabbit", "now"].forEach((k, i) => sing(k, s0 + i * 2, 1.35, i % 2 ? 0.12 : -0.12));
    }
  }
}

// ── reverb (Schroeder: 6 parallel feedback combs → 2 allpass diffusers)
// Process the reverb send and add the wet to the bus. Hall-ish: ~2 s
// decay, low-passed for a darker tail (warm not splashy).
function processReverb(srcL, srcR, dstL, dstR, decay = 0.78, wet = 0.55, damp = 0.42) {
  const combDelays = [
    Math.floor(0.0297 * SR), Math.floor(0.0371 * SR),
    Math.floor(0.0411 * SR), Math.floor(0.0437 * SR),
    Math.floor(0.0497 * SR), Math.floor(0.0581 * SR),
  ];
  const apDelays = [Math.floor(0.005 * SR), Math.floor(0.0017 * SR)];
  const apFb = 0.5;
  const cBufL = combDelays.map((d) => new Float32Array(d));
  const cBufR = combDelays.map((d) => new Float32Array(d));
  const cIdxL = combDelays.map(() => 0);
  const cIdxR = combDelays.map(() => 0);
  const cLPL  = combDelays.map(() => 0);  // per-comb damping LP state
  const cLPR  = combDelays.map(() => 0);
  const aBufL = apDelays.map((d) => new Float32Array(d));
  const aBufR = apDelays.map((d) => new Float32Array(d));
  const aIdxL = apDelays.map(() => 0);
  const aIdxR = apDelays.map(() => 0);
  for (let i = 0; i < srcL.length; i++) {
    const inL = srcL[i], inR = srcR[i];
    if (inL === 0 && inR === 0
        && cBufL.every((b, k) => Math.abs(b[cIdxL[k]]) < 1e-7)) {
      // when both input and most-recent comb tail are silent, skip
      // (cheap shortcut for long quiet stretches)
    }
    let cL = 0, cR = 0;
    for (let k = 0; k < combDelays.length; k++) {
      const dL = cBufL[k][cIdxL[k]];
      const dR = cBufR[k][cIdxR[k]];
      cL += dL;
      cR += dR;
      // one-pole LP inside the feedback loop = darker tail
      cLPL[k] = dL * (1 - damp) + cLPL[k] * damp;
      cLPR[k] = dR * (1 - damp) + cLPR[k] * damp;
      cBufL[k][cIdxL[k]] = inL + cLPL[k] * decay;
      cBufR[k][cIdxR[k]] = inR + cLPR[k] * decay;
      cIdxL[k] = (cIdxL[k] + 1) % combDelays[k];
      cIdxR[k] = (cIdxR[k] + 1) % combDelays[k];
    }
    cL *= 1 / combDelays.length; cR *= 1 / combDelays.length;
    for (let k = 0; k < apDelays.length; k++) {
      const dL = aBufL[k][aIdxL[k]];
      const dR = aBufR[k][aIdxR[k]];
      const oL = -apFb * cL + dL;
      const oR = -apFb * cR + dR;
      aBufL[k][aIdxL[k]] = cL + apFb * oL;
      aBufR[k][aIdxR[k]] = cR + apFb * oR;
      aIdxL[k] = (aIdxL[k] + 1) % apDelays[k];
      aIdxR[k] = (aIdxR[k] + 1) % apDelays[k];
      cL = oL; cR = oR;
    }
    dstL[i] += cL * wet;
    dstR[i] += cR * wet;
  }
}
processReverb(revL, revR, busL, busR, 0.80, 0.55, 0.42);

// ── sidechain duck (the pump) ─────────────────────────────────────────
const duck = new Float32Array(N).fill(1);
for (const kt of kickTimes) {
  const s0 = Math.floor(kt * SR), len = Math.floor(0.14 * SR);
  for (let i = 0; i < len && s0 + i < N; i++) {
    const w = i / len;
    const d = 0.55 + 0.45 * w;
    if (d < duck[s0 + i]) duck[s0 + i] = d;
  }
}

// ── pre-master sum: bus (ducked) + drm (dry) → stereo float ──────────
let outL = new Float32Array(N);
let outR = new Float32Array(N);
let outN = N; // grows when the tape-stop ending stretches the tail
// ── INDUSTRIAL FILTER SWEEP + CRUNCH (skrillex-leaning) ───────────────
// A resonant state-variable lowpass on the MUSIC bus whose cutoff sweeps
// with the arrangement (rising through the intro, a dramatic down-up sweep
// across the breaks) and WOBBLES on the drops (LFO-modulated cutoff — the
// dubstep wobble). Before the filter the music is crunched (drive + soft
// fold + mild bitcrush) for grit. Vocals are summed AFTER, clean, so they
// stay present over the wobble. Tune with --crunch / --wobble / --wobhz.
const CRUNCH = Number(flags.crunch ?? 1);
const WOB = Number(flags.wobble ?? 0.4); // subtle — just a gentle breathing
const wobHz = Number(flags.wobhz ?? (BPM / 60) / 2); // half-note wobble
const Q = 0.8, qc = 1 / Q, fcMax = SR * 0.16; // gentle resonance (not peaky)
const segOrder = ["intro", "buildA", "drop1", "breakA", "drop2", "breakB", "drop3", "outro"];
const segs = []; { let c = 0; for (const k of segOrder) { segs.push({ k, s: c * bar, e: (c + SECTION_BARS[k]) * bar }); c += SECTION_BARS[k]; } }
// opening swell — keep the intro + build QUIET, ramp up into the first drop
const drop1T = (SECTION_BARS.intro + SECTION_BARS.buildA) * bar;
function crunch(x) {
  let y = Math.tanh(x * (1.5 + 1.6 * CRUNCH));   // drive
  y = y - 0.14 * CRUNCH * y * y * y;             // asymmetric-ish fold
  const lv = 56; return Math.round(y * lv) / lv; // mild bitcrush grit
}
let lowL = 0, bandL = 0, lowR = 0, bandR = 0, si = 0;
for (let i = 0; i < N; i++) {
  const t = i / SR;
  while (si < segs.length - 1 && t >= segs[si].e) si++;
  const sg = segs[si], p = (t - sg.s) / Math.max(1e-6, sg.e - sg.s);
  let cut;
  if (sg.k.startsWith("drop")) {                 // GENTLE breathing — stays open
    const lfo = 0.5 + 0.5 * Math.sin(TAU * wobHz * t);
    cut = 13000 - WOB * 6000 * lfo;              // ~13k → ~10.6k at WOB 0.4 (subtle)
  } else if (sg.k.startsWith("break")) {         // dramatic down-up sweep
    cut = 9000 - 8500 * Math.sin(Math.PI * p);
  } else if (sg.k === "intro") {
    cut = 6000;                                  // open — the shanty rings clear
  } else if (sg.k === "buildA") {
    cut = 1200 * Math.pow(24000 / 1200, p);      // rising build into drop 1
  } else { cut = 12000 - 9500 * p; }             // outro close
  cut = Math.max(220, Math.min(fcMax, cut));
  const f = 2 * Math.sin(Math.PI * cut / SR);
  let mL = crunch((busL[i] * duck[i] + drm[i] * 0.96) * 1.05);
  let mR = crunch((busR[i] * duck[i] + drm[i] * 0.96) * 1.05);
  const hL = mL - lowL - qc * bandL; bandL += f * hL; lowL += f * bandL;
  const hR = mR - lowR - qc * bandR; bandR += f * hR; lowR += f * bandR;
  // vocals: clean, post-filter, LOUD, lightly side-chained to the kick so
  // they pump/breathe with the beat without losing audibility.
  // opening swell — starts near-silent and fades in very slowly, staying
  // quiet for most of the intro before swelling up into the first drop.
  const swell = t < drop1T ? 0.04 + 0.96 * Math.pow(t / drop1T, 2.6) : 1;
  const vduck = 0.74 + 0.26 * duck[i];
  outL[i] = lowL * swell + vocL[i] * 1.5 * vduck;
  outR[i] = lowR * swell + vocR[i] * 1.5 * vduck;
}

// ── SPACEY HEALING DRONE — evolving Solfeggio-ish tones (F minor) that
// slowly GLIDE in pitch, drift in pan, and shimmer in amplitude — clean,
// post-filter, a deep-space healing bed. Phase accumulators per voice so
// the pitch glides are smooth.
{
  const durS = N / SR;
  const H = [
    { f: 174.61, pr: 0.013, pd: 2.0, panr: 0.05, ampr: 0.06 }, // F3 (Solfeggio 174)
    { f: 261.63, pr: 0.017, pd: 1.5, panr: 0.07, ampr: 0.04 }, // C4
    { f: 392.00, pr: 0.011, pd: 3.0, panr: 0.09, ampr: 0.05 }, // G4
    { f: 523.25, pr: 0.019, pd: 2.0, panr: 0.06, ampr: 0.07 }, // C5 (~528)
  ];
  const ph = H.map(() => 0);
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    const fade = Math.min(1, t / 6) * Math.min(1, (durS - t) / 6);
    for (let k = 0; k < H.length; k++) {
      const h = H[k];
      const f = h.f * Math.pow(2, (h.pd / 12) * Math.sin(TAU * h.pr * t + k));   // slow pitch glide
      ph[k] += TAU * f / SR;
      const amp = 0.019 * (0.5 + 0.5 * Math.sin(TAU * h.ampr * t + k * 1.7)) * fade;
      const pan = Math.sin(TAU * h.panr * t + k * 2.1);                          // wide evolving pan
      const v = Math.sin(ph[k]) * amp;
      outL[i] += v * (0.5 - pan * 0.5);
      outR[i] += v * (0.5 + pan * 0.5);
    }
  }
}

// ── SINE "TINGE" DROPLETS — cute high descending sine pings scattered
// through the track (clean, post-filter), random pan, quick decay.
{
  const dropF = [523.25, 587.33, 659.25, 783.99, 880.0]; // C D E G A, high + sweet
  for (let b = 4; b < totalBars; b++) {
    if (Math.random() < 0.55) continue;               // sparse
    const t0 = b * bar + Math.random() * bar;
    const f0 = dropF[Math.floor(Math.random() * dropF.length)];
    const dur = 0.4, n = Math.floor(dur * SR), s0 = Math.floor(t0 * SR), pan = Math.random() * 2 - 1;
    let p = 0;
    for (let i = 0; i < n; i++) {
      const t = i / SR, f = f0 * Math.pow(2, -1.6 * t);  // pitch drops
      p += TAU * f / SR;
      const v = Math.sin(p) * Math.exp(-t * 7) * 0.085;
      if (s0 + i < N) { outL[s0 + i] += v * (0.5 - pan * 0.5); outR[s0 + i] += v * (0.5 + pan * 0.5); }
    }
  }
}

// ── PAYPHONE INTRO — the first ~6s come through a tinny telephone (narrow
// 400–3000 Hz band, quiet, a touch of crush), then ALL the harmonics fade
// in full over ~1.5s. The track "picks up off the line".
{
  const PHONE = 6.0, XF = 1.6, endI = Math.floor((PHONE + XF) * SR);
  let s1L = 0, s2L = 0, s1R = 0, s2R = 0;
  const aLo = 1 - Math.exp(-2 * Math.PI * 400 / SR);   // HP corner
  const aHi = 1 - Math.exp(-2 * Math.PI * 3000 / SR);  // LP corner
  for (let i = 0; i < endI && i < N; i++) {
    const t = i / SR;
    const xL = outL[i], xR = outR[i];
    s1L += aLo * (xL - s1L); let bL = xL - s1L; s2L += aHi * (bL - s2L); bL = s2L;
    s1R += aLo * (xR - s1R); let bR = xR - s1R; s2R += aHi * (bR - s2R); bR = s2R;
    const crush = (x) => Math.round(x * 28) / 28;        // gritty phone-line bitcrush
    const wetL = crush(bL) * 0.42, wetR = crush(bR) * 0.42;
    const mix = t < PHONE ? 0 : (t - PHONE) / XF;        // 0 = full phone, 1 = full dry
    outL[i] = wetL * (1 - mix) + xL * mix;
    outR[i] = wetR * (1 - mix) + xR * mix;
  }
}

// normalize headroom so the master chain has room to work
let peak = 0;
for (let i = 0; i < N; i++) {
  const a = Math.max(Math.abs(outL[i]), Math.abs(outR[i]));
  if (a > peak) peak = a;
}
if (peak > 0) {
  const n = 0.78 / peak;
  for (let i = 0; i < N; i++) { outL[i] *= n; outR[i] *= n; }
}

// ── TAPE-STOP ENDING — over the last ~24s the BPM eases DOWN from 160 to
// ~80 (rate 0.5) and just keeps humming there, pitch sagging — not a full
// death-to-zero, more a "settling into a slow heartbeat". Resamples the tail.
{
  const TAPE = 24.0;
  const ts0 = Math.max(0, Math.floor((totalBars * bar - TAPE) * SR));
  const srcLen = N - ts0;
  const tailL = [], tailR = [];
  let pos = 0;
  while (ts0 + pos < N - 1) {
    const prog = pos / srcLen;
    const rate = Math.max(0.5, 1 - prog * prog * 0.5); // eases to ~80 BPM, keeps humming
    const sp = ts0 + pos, j = Math.floor(sp), fr = sp - j;
    tailL.push(outL[j] * (1 - fr) + outL[j + 1] * fr);
    tailR.push(outR[j] * (1 - fr) + outR[j + 1] * fr);
    pos += rate;
  }
  outN = ts0 + tailL.length;
  const nL = new Float32Array(outN), nR = new Float32Array(outN);
  nL.set(outL.subarray(0, ts0)); nR.set(outR.subarray(0, ts0));
  for (let i = 0; i < tailL.length; i++) { nL[ts0 + i] = tailL[i]; nR[ts0 + i] = tailR[i]; }
  outL = nL; outR = nR;
}

// ── trim trailing silence, keep a generous tail ──────────────────────
let tailEnd = outN - 1;
const TAIL_THRESH = 0.0003;
while (tailEnd > 0 && Math.abs(outL[tailEnd]) < TAIL_THRESH && Math.abs(outR[tailEnd]) < TAIL_THRESH) tailEnd--;
const trimN = Math.min(outN, tailEnd + Math.floor(1.0 * SR));

// ── PHONE-WORLD LOOP — over the last ~6s the track morphs BACK into the
// quiet telephone band (matching the intro), so it loops seamlessly from
// the dying tail straight into the payphone opening.
{
  const PH = 6.0, ph0 = Math.max(0, trimN - Math.floor(PH * SR));
  let s1L = 0, s2L = 0, s1R = 0, s2R = 0;
  const aLo = 1 - Math.exp(-2 * Math.PI * 400 / SR), aHi = 1 - Math.exp(-2 * Math.PI * 3000 / SR);
  for (let i = ph0; i < trimN; i++) {
    const xL = outL[i], xR = outR[i];
    s1L += aLo * (xL - s1L); let bL = xL - s1L; s2L += aHi * (bL - s2L); bL = s2L;
    s1R += aLo * (xR - s1R); let bR = xR - s1R; s2R += aHi * (bR - s2R); bR = s2R;
    const cr = (x) => Math.round(x * 28) / 28;
    const mix = (i - ph0) / (trimN - ph0); // 0 = dry, 1 = full phone
    outL[i] = xL * (1 - mix) + cr(bL) * 0.42 * mix;
    outR[i] = xR * (1 - mix) + cr(bR) * 0.42 * mix;
  }
}
// gentle fade on the last 1.2s so the loop seam doesn't click
const fadeN = Math.min(trimN, Math.floor(1.2 * SR));
for (let i = 0; i < fadeN; i++) {
  const g = Math.cos((Math.PI / 2) * (i / fadeN));
  const idx = trimN - fadeN + i;
  outL[idx] *= g; outR[idx] *= g;
}

// ── write interleaved stereo f32 raw ─────────────────────────────────
const outPath = expandHome(flags.out) || resolve(HERE, "../out/maytrax.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const rawBuf = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  rawBuf.writeFloatLE(outL[i], i * 8);
  rawBuf.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, rawBuf);

console.log(
  `→ maytrax · ${BPM} BPM · ${totalBars} bars · ${(trimN / SR).toFixed(1)}s · ` +
  `intro→buildA→drop1→breakA→drop2→breakB→drop3→outro · stereo`,
);

// ── pop master chain ─────────────────────────────────────────────────
// A fuller, more "finished" pop master than the old 4-stage shape:
//
//   1. highpass 28        — strip infrasonic rumble
//   2. tonal balance EQ   — sub weight @60, de-mud @300, presence @3.5k,
//                           air shelf @11k (the modern smiley curve)
//   3. glue compression   — slow-ish 2:1 to fuse the kit + bed
//   4. asoftclip (tanh)   — harmonic saturation = density + perceived loud
//   5. stereotools        — gentle side lift for width (mono-safe ~1.12)
//   6. alimiter           — driven brickwall for the loud, glossy ceiling
//
// A final loudnorm stage per target sets the delivery loudness: hot pop
// level for the mp3 preview, streaming-safe -14 LUFS for the DistroKid wav.
// Tonal + dynamics core — everything *except* loudness + the final
// ceiling. Loudness is set once (loudnorm), then a true-peak brickwall
// runs dead last so nothing downstream can push peaks back over.
const MASTER_CORE = [
  "highpass=f=28",
  "equalizer=f=60:t=q:w=0.7:g=1.6",       // sub weight
  "equalizer=f=300:t=q:w=1.1:g=-1.6",     // de-mud the low-mids
  "equalizer=f=3500:t=q:w=1.2:g=1.8",     // presence / clarity
  "treble=g=1.6:f=12000",                 // air shelf (gentle — keeps mp3 clean)
  "acompressor=threshold=-18dB:ratio=2.2:attack=18:release=180:makeup=2.6:knee=6",
  "asoftclip=type=tanh:threshold=0.96",   // light density / glue
  "stereotools=slev=1.10",                // subtle, mono-safe width
].join(",");

// mp3 preview — loudnorm sets a hot modern pop loudness, then the
// limiter holds the ceiling at ~-1.2 dBFS so LAME's lossy inter-sample
// overshoot still lands under 0 dBTP. Verified via ebur128 (NOT
// loudnorm's print, which mis-reports on dense brickwalled signal):
// post-encode ≈ -8.8 LUFS / -0.8 dBTP.
const MASTER_MP3 =
  `${MASTER_CORE},loudnorm=I=-9.5:TP=-1.5:LRA=9,` +
  `alimiter=limit=0.89:attack=3:release=55`;

const ff = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER_MP3,
  "-c:a", "libmp3lame", "-b:a", "320k", outPath,
], { stdio: "inherit" });
if (ff.status !== 0) {
  try { unlinkSync(rawPath); } catch {}
  console.error("✗ ffmpeg failed");
  process.exit(1);
}
console.log(`✓ ${outPath} (pop-mastered · 320k · ${(trimN / SR).toFixed(1)} s · stereo)`);

// ── optional DistroKid WAV master ────────────────────────────────────
const wavOut = expandHome(flags.wav);
if (wavOut) {
  mkdirSync(dirname(wavOut), { recursive: true });
  const wff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
    "-af", `${MASTER_CORE},loudnorm=I=-14:TP=-1.5:LRA=11,alimiter=limit=0.95:attack=3:release=55`,
    "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", wavOut,
  ], { stdio: "inherit" });
  if (wff.status !== 0) {
    try { unlinkSync(rawPath); } catch {}
    console.error("✗ wav master failed");
    process.exit(1);
  }
  console.log(`✓ ${wavOut} (DistroKid master · 44.1 kHz / 16-bit · -14 LUFS)`);
}
try { unlinkSync(rawPath); } catch {}
