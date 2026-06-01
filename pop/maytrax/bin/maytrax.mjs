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

function playSampleStereo(L, R, smp, startSec, gain = 1, rate = 1, panSpread = 0) {
  if (!smp) return;
  const s0 = Math.floor(startSec * SR);
  const src = smp.samples;
  const step = (smp.sampleRate / SR) * rate;
  const lg = gain * (1 - panSpread * 0.5);
  const rg = gain * (1 + panSpread * 0.5);
  // light haas widening when panSpread > 0
  const haasDelay = panSpread > 0 ? Math.floor(0.012 * SR) : 0;
  for (let i = 0; i < L.length - s0; i++) {
    const p = i * step;
    const j = Math.floor(p);
    if (j >= src.length - 1) break;
    const f = p - j;
    const v = src[j] * (1 - f) + src[j + 1] * f;
    addStereo(L, R, s0 + i, v * lg, 0);
    if (haasDelay > 0) addStereo(L, R, s0 + i + haasDelay, 0, v * rg);
    else addStereo(L, R, s0 + i, 0, v * rg);
  }
}

// ── drum synthesis ─────────────────────────────────────────────────────
// the prodigy 909-kick: pitch-enveloped sine driven hard through tanh,
// with an extra sub layer underneath for weight.
function kick909(buf, startSec, g = 1.0) {
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

// shaker / maraca — a two-pole high-passed noise burst with a soft 3 ms
// attack and a longer decay than the closed hat, so it reads as an airy
// "shh" rather than a "tk". The backbone of the sunk-in perc layer.
function shaker(buf, startSec, g = 0.10, decay = 52) {
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
  const kP = ["K", ".", ".", "K", ".", ".", ".", ".", ".", ".", "K", ".", ".", ".", ".", "."];
  const sP = [".", ".", ".", ".", "S", ".", ".", "s", ".", ".", ".", ".", "S", ".", ".", "."];
  const hP = ["h", "h", "h", "H", "h", "h", "h", "h", "h", "H", "h", "h", "h", "h", "H", "h"];
  for (let e = 0; e < 16; e++) {
    const t = t0 + e * sx;
    if (kP[e] === "K") kick909(drm, t, 1.05 * energy);
    if (sP[e] === "S") snareBrk(drm, t, 0.78 * energy);
    if (sP[e] === "s") snareBrk(drm, t, 0.34 * energy);
    // hats: ping-pong slightly so the kit breathes in stereo
    const pan = (e % 2 === 0 ? -1 : 1) * hatPan;
    const tmp = new Float32Array(busL.length);
    if (hP[e] === "h") hat(tmp, t, 0.115 * energy);
    if (hP[e] === "H") openHat(tmp, t, 0.145 * energy);
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
const totalSec = totalBars * bar + 1.5; // +tail for reverb-ish decays
const N = Math.floor(totalSec * SR);

// stereo buses — bus is everything ducked by sidechain; drm is the
// dry kicks+snares which bypass the duck (they're the punch). revL/revR
// is the reverb send (brass/flugel fanfares + bells route here).
const busL = new Float32Array(N);
const busR = new Float32Array(N);
const drm  = new Float32Array(N);
const revL = new Float32Array(N);
const revR = new Float32Array(N);
const kickTimes = [];

// ── fanfare helpers ──────────────────────────────────────────────────
// Multi-note "ta-da-DAAAH" pattern using a brass/flugel one-shot
// pitched up to outline the F minor triad (F-Ab-C). Hits dry on
// busL/busR + sends to the reverb bus for a hall tail.
function brassFanfare(L, R, rL, rR, smp, t0, gain) {
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
const outL = new Float32Array(N);
const outR = new Float32Array(N);
for (let i = 0; i < N; i++) {
  outL[i] = Math.tanh((busL[i] * duck[i] + drm[i] * 0.96) * 1.05);
  outR[i] = Math.tanh((busR[i] * duck[i] + drm[i] * 0.96) * 1.05);
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

// ── trim trailing silence (keep 0.6s tail for the bell) ──────────────
let tailEnd = N - 1;
const TAIL_THRESH = 0.0005;
while (tailEnd > 0 && Math.abs(outL[tailEnd]) < TAIL_THRESH && Math.abs(outR[tailEnd]) < TAIL_THRESH) tailEnd--;
const trimN = Math.min(N, tailEnd + Math.floor(0.6 * SR));

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
