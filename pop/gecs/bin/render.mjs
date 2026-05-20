#!/usr/bin/env node
// gecs/bin/render.mjs — the gecs bed builder, v2. Same node-only / no-
// Web-Audio posture as pop/hippyhayzard/bin/render.mjs: every sample
// synthesized here from AC instruments. No Suno. Bottom-up (pop/SCORE.md).
//
// Lane: 100-gecs / hyperpop crossed with TRAP swing. The arrangement
// mirrors pop/gecs/gecs.np and clocks ~84s at 165 BPM:
//
//   intro 4 · v1 12 · ch1 8 · v2 8 · ch2 8 · break 6 · ch3 8 · outro 4
//   = 58 bars × 1.454s ≈ 84.4s
//
// Design notes vs v1 (the "general MIDI" critique):
//   • STEREO bus + a real algorithmic plate reverb send. Voices live in
//     space, not stacked dry on the same point.
//   • Trap drum patterns: kick on 1 + the 'and of 3', snare on 3, hi-
//     hats 1/16 with strong (~26%) swing + occasional 1/32 rolls. No
//     more 4-on-the-floor in the verses.
//   • SPARSER LAYERING: each section drops one or two voices so the
//     space breathes. Verse = just trap kit + screech-skrill + a single
//     stab. Chorus = supersaw chord stack + hoover honk + reverb tail.
//     Break = ska upstrokes alone over half-time trap kit.
//   • Slight timing humanization (±6ms jitter) so it's not gridlocked.
//   • Filter sweeps on chorus supersaw (time-varying lowpass) instead
//     of static-bright stacks.
//   • Bit-crush stays on the master + kick, but lighter than v1 so the
//     reverb tails actually read.
//
// Usage:
//   node pop/gecs/bin/render.mjs                         # → out/gecs.mp3
//   node pop/gecs/bin/render.mjs --bpm 168 --out ~/g.mp3

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { mixEventSupersaw } from "../../dance/synths/supersaw.mjs";
import { mixEventSkrill } from "../../dance/synths/skrill.mjs";
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

const BPM = Number(flags.bpm ?? 165);
const beat = 60 / BPM;
const bar = beat * 4;
const sx = beat / 4;

// Deterministic per-event jitter (humanization, ±jitMs around the grid).
function jit(seedStr, jitMs = 6) {
  let s = 2166136261 >>> 0;
  for (let i = 0; i < seedStr.length; i++) { s ^= seedStr.charCodeAt(i); s = Math.imul(s, 16777619); }
  s = (s >>> 0) || 1;
  s ^= s << 13; s >>>= 0; s ^= s >>> 17; s >>>= 0; s ^= s << 5; s >>>= 0;
  return ((s / 0xffffffff) * 2 - 1) * (jitMs / 1000);
}

// ── inline drums ─────────────────────────────────────────────────────
function addStereo(L, R, idx, vL, vR) {
  if (idx >= 0 && idx < L.length) { L[idx] += vL; R[idx] += vR; }
}
function crush(x, levels = 12) {
  const s = x >= 0 ? 1 : -1;
  const q = Math.round(Math.abs(x) * levels) / levels;
  return s * Math.min(1, q);
}

// 808-style trap kick — long sub tail (~0.35s) + crushed click on top.
function trapKick(L, R, startSec, g = 1.0, crushLvls = 14) {
  const dur = 0.36, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const f = 42 + (140 - 42) * Math.exp(-t * 22);   // smoother slide
    ph += (2 * Math.PI * f) / SR;
    const body = Math.sin(ph) * Math.exp(-t * 6);    // long sub tail
    const click = i < SR * 0.003 ? (Math.random() * 2 - 1) * 0.8 * (1 - i / (SR * 0.003)) : 0;
    const v = crush((body + click * 0.6) * g, crushLvls);
    addStereo(L, R, s0 + i, v, v);                   // mono center
  }
}

// Sharp trap snare — short, snappy, pan slightly right
function trapSnare(L, R, startSec, g = 0.85) {
  const dur = 0.11, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let prev = 0, ph = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    ph += (2 * Math.PI * 195) / SR;
    const tone = Math.sin(ph) * Math.exp(-t * 38) * 0.5;
    const v = crush((hp * Math.exp(-t * 30) + tone) * g, 10);
    addStereo(L, R, s0 + i, v * 0.92, v * 1.08);     // hint right
  }
}

// Stacked clap layered with snare (gecs hit)
function trapClap(L, R, startSec, g = 0.7) {
  for (let k = 0; k < 4; k++) {
    const t0 = startSec + k * 0.009;
    const dur = 0.06, n = Math.floor(dur * SR), s0 = Math.floor(t0 * SR);
    let prev = 0;
    for (let i = 0; i < n; i++) {
      const nz = Math.random() * 2 - 1;
      const hp = nz - prev; prev = nz;
      const v = crush(hp * Math.exp(-(i / SR) * 50) * g * (k === 3 ? 1.3 : 0.55), 10);
      addStereo(L, R, s0 + i, v * 1.05, v * 0.95);   // hint left
    }
  }
}

// Hi-hat: closed, short, can be panned across the stereo field
function hat(L, R, startSec, g = 0.32, len = 0.022, pan = 0) {
  const n = Math.floor(len * SR), s0 = Math.floor(startSec * SR);
  let prev = 0;
  const pL = Math.cos((pan + 1) * Math.PI / 4);      // equal-power pan
  const pR = Math.sin((pan + 1) * Math.PI / 4);
  for (let i = 0; i < n; i++) {
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    const v = hp * Math.exp(-(i / SR) * 260) * g;
    addStereo(L, R, s0 + i, v * pL * 1.414, v * pR * 1.414);
  }
}

// Open hat — same color, longer tail
function openHat(L, R, startSec, g = 0.22, pan = 0) {
  hat(L, R, startSec, g, 0.18, pan);
}

function riser(L, R, startSec, durSec, g = 0.55) {
  const n = Math.floor(durSec * SR), s0 = Math.floor(startSec * SR);
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const w = i / n;
    const nz = Math.random() * 2 - 1;
    const a = 0.02 + 0.5 * w;
    lp += a * (nz - lp);
    const v = (nz - lp) * Math.pow(w, 1.4) * g;
    // riser sweeps L→R as it climbs
    const pan = w * 2 - 1;
    const pL = Math.cos((pan + 1) * Math.PI / 4);
    const pR = Math.sin((pan + 1) * Math.PI / 4);
    addStereo(L, R, s0 + i, v * pL * 1.414, v * pR * 1.414);
  }
}

// ── algorithmic plate reverb (Schroeder-style: 4 comb + 2 allpass) ───
// Process a mono send bus into a stereo wet bus. Cheap but musical —
// gives us the "deep space" the v1 bed was missing.
function reverbToStereo(send, wetL, wetR, opts = {}) {
  const room = opts.room ?? 0.78;        // 0..1 — feedback amount
  const damp = opts.damp ?? 0.35;        // 0..1 — HF damping per pass
  const wet = opts.wet ?? 0.42;
  // four parallel comb filters w/ prime-ish delay (in samples @ 48k)
  const combDelL = [1557, 1617, 1491, 1422];
  const combDelR = [1580, 1640, 1514, 1445];
  const combL = combDelL.map((d) => ({ buf: new Float32Array(d), idx: 0, lp: 0 }));
  const combR = combDelR.map((d) => ({ buf: new Float32Array(d), idx: 0, lp: 0 }));
  // two series allpass per channel
  const apDel = [225, 556];
  const apL = apDel.map((d) => ({ buf: new Float32Array(d), idx: 0 }));
  const apR = apDel.map((d) => ({ buf: new Float32Array(d), idx: 0 }));
  const apGain = 0.5;

  for (let i = 0; i < send.length; i++) {
    const x = send[i];
    let sumL = 0, sumR = 0;
    for (let c = 0; c < combL.length; c++) {
      const cL = combL[c], cR = combR[c];
      // L
      const outL = cL.buf[cL.idx];
      cL.lp = outL * (1 - damp) + cL.lp * damp;
      cL.buf[cL.idx] = x + cL.lp * room;
      cL.idx = (cL.idx + 1) % cL.buf.length;
      sumL += outL;
      // R
      const outR = cR.buf[cR.idx];
      cR.lp = outR * (1 - damp) + cR.lp * damp;
      cR.buf[cR.idx] = x + cR.lp * room;
      cR.idx = (cR.idx + 1) % cR.buf.length;
      sumR += outR;
    }
    let yL = sumL * 0.25, yR = sumR * 0.25;
    for (const ap of apL) {
      const d = ap.buf[ap.idx];
      const in_ = yL + d * apGain;
      ap.buf[ap.idx] = in_;
      yL = d - in_ * apGain;
      ap.idx = (ap.idx + 1) % ap.buf.length;
    }
    for (const ap of apR) {
      const d = ap.buf[ap.idx];
      const in_ = yR + d * apGain;
      ap.buf[ap.idx] = in_;
      yR = d - in_ * apGain;
      ap.idx = (ap.idx + 1) % ap.buf.length;
    }
    wetL[i] += yL * wet;
    wetR[i] += yR * wet;
  }
}

// Mix a mono synth buffer into the stereo bus with a pan + send level.
// pan in [-1, 1]. sendLevel goes to the reverb input bus.
function placeStereo(mono, busL, busR, send, pan = 0, sendLevel = 0.3) {
  const pL = Math.cos((pan + 1) * Math.PI / 4);
  const pR = Math.sin((pan + 1) * Math.PI / 4);
  for (let i = 0; i < mono.length; i++) {
    const v = mono[i];
    busL[i] += v * pL * 1.414;
    busR[i] += v * pR * 1.414;
    send[i] += v * sendLevel;
  }
}

// ── chord progressions (F#m verse · A major chorus) ──────────────────
const VERSE_PROG = [
  { bass: 42, voicing: [54, 57, 61] },   // F#m
  { bass: 38, voicing: [54, 57, 62] },   // D
  { bass: 40, voicing: [52, 56, 59] },   // E
  { bass: 35, voicing: [54, 57, 59] },   // Bm
];
const CHORUS_PROG = [
  { bass: 45, voicing: [57, 61, 64] },   // A
  { bass: 38, voicing: [57, 62, 66] },   // D
  { bass: 42, voicing: [57, 61, 66] },   // F#m
  { bass: 40, voicing: [56, 59, 64] },   // E
];

// CHORUS MELODY — hand-transcribed from gecs.np (the chorus block). The
// instruments DOUBLE this line (the user feedback: "harmonizes on the
// instruments"). Format: [startBeat, midi, durBeats]. Spans 8 bars
// (32 beats) — same shape as the sung chorus.
//
//   "ker-nel pa-nic ker-nel pa-nic   blue   light   scream"
//   "ker-nel pa-nic ker-nel pa-nic   wake up from the   dream"
//   "ev-ry-thing is   o-kay     ev-ry-thing is   fine"
//   "ker-nel pa-nic   on the   nine-ty   ninth   time"
const CHORUS_MELODY = [
  // bars 1-2 — blue light scream
  [0,    69, 0.5], [0.5,  69, 0.5], [1,    73, 0.5], [1.5,  73, 0.5],
  [2,    69, 0.5], [2.5,  69, 0.5], [3,    73, 0.5], [3.5,  73, 0.5],
  [4,    76, 1.0], [5,    73, 1.0], [6,    69, 2.0],
  // bars 3-4 — wake up from the dream
  [8,    69, 0.5], [8.5,  69, 0.5], [9,    73, 0.5], [9.5,  73, 0.5],
  [10,   69, 0.5], [10.5, 69, 0.5], [11,   73, 0.5], [11.5, 73, 0.5],
  [12,   76, 0.5], [12.5, 76, 0.5], [13,   73, 0.5], [13.5, 73, 0.5],
  [14,   69, 2.0],
  // bars 5-6 — everything is okay / everything is fine
  [16,   73, 0.5], [16.5, 73, 0.5], [17,   69, 0.5], [17.5, 69, 0.5],
  [18,   76, 0.5], [18.5, 76, 1.5],
  [20,   73, 0.5], [20.5, 73, 0.5], [21,   69, 0.5], [21.5, 69, 0.5],
  [22,   78, 2.0],
  // bars 7-8 — kernel panic on the ninety ninth time
  [24,   69, 0.5], [24.5, 69, 0.5], [25,   73, 0.5], [25.5, 73, 0.5],
  [26,   69, 0.5], [26.5, 69, 0.5], [27,   76, 0.5], [27.5, 76, 0.5],
  [28,   73, 1.0], [29,   69, 3.0],
];

// Diatonic 3rd-above (A major scale) — counter-line for the harmony layer.
const AMAJ = [0, 2, 4, 5, 7, 9, 11];   // semitone offsets from A
function third_above(midi) {
  const semi = midi - 69;
  const octs = Math.floor(semi / 12);
  const pc = ((semi % 12) + 12) % 12;
  let idx = AMAJ.indexOf(pc);
  if (idx < 0) { idx = 0; for (let k = 0; k < AMAJ.length; k++) if (AMAJ[k] <= pc) idx = k; }
  const pos = octs * 7 + idx + 2;        // up a diatonic third
  const nOct = Math.floor(pos / 7);
  const nIdx = ((pos % 7) + 7) % 7;
  return 69 + nOct * 12 + AMAJ[nIdx];
}

// ── section renderers ────────────────────────────────────────────────
// Each section gets its own mono synth scratch buffer that we pan +
// send into reverb. Drums are rendered straight into the stereo bus.

function renderIntro(busL, busR, send, drmL, drmR, t0, bars, N) {
  const scratch = new Float32Array(N);
  for (let b = 0; b < bars; b++) {
    const bt = t0 + b * bar;
    // sub drone holds the key — pan slight L, wet on send
    mixEventSkrill(
      { startSec: bt, midi: 30, durSec: bar * 0.95, gain: 0.30, preset: "sub", lfo: "1/2" },
      scratch, { sampleRate: SR, bpm: BPM },
    );
    // single hoover whoop on beat 1 of bar 2 + bar 4 — ambient, distant
    if (b === 1 || b === 3) {
      const tmp = new Float32Array(N);
      mixEventHoover(
        { startSec: bt, midi: 57, durSec: bar * 0.8, gain: 0.24 },
        tmp, { sampleRate: SR, preset: "whoop" },
      );
      placeStereo(tmp, busL, busR, send, b === 1 ? -0.6 : 0.6, 0.7);
    }
    // sparse trap hats — only the 'and' of 2 and 4
    hat(drmL, drmR, bt + beat * 1.5, 0.15, 0.025, -0.4);
    hat(drmL, drmR, bt + beat * 3.5, 0.15, 0.025, 0.4);
  }
  placeStereo(scratch, busL, busR, send, 0, 0.5);
  // big riser across the last 2 bars (sweeps L→R) + pickup kick
  riser(drmL, drmR, t0 + (bars - 2) * bar, 2 * bar, 0.55);
  trapKick(drmL, drmR, t0 + bars * bar - sx, 0.9);
}

function renderVerse(busL, busR, send, drmL, drmR, kt, t0, bars, prog, sectionName) {
  const screech = new Float32Array(N);
  const subB = new Float32Array(N);
  const stab = new Float32Array(N);
  for (let b = 0; b < bars; b++) {
    const bt = t0 + b * bar;
    const ch = prog[b % prog.length];
    // screech-skrill on 1/16 (the gecs wobble) — narrower than chorus
    mixEventSkrill(
      { startSec: bt, midi: ch.bass + 12, durSec: bar * 0.95, gain: 0.34,
        preset: "screech", lfo: "1/16" },
      screech, { sampleRate: SR, bpm: BPM },
    );
    // 808 sub track — only on beat 1 and 3.5 (the trap kick spots)
    mixEventSkrill(
      { startSec: bt, midi: ch.bass - 12, durSec: beat * 1.4, gain: 0.36, preset: "sub" },
      subB, { sampleRate: SR, bpm: BPM },
    );
    mixEventSkrill(
      { startSec: bt + beat * 2.5, midi: ch.bass - 12, durSec: beat * 1.2, gain: 0.32, preset: "sub" },
      subB, { sampleRate: SR, bpm: BPM },
    );
    // a single supersaw stab on the 'and' of 4 (lead-in to next bar)
    for (const m of ch.voicing) {
      mixEventSupersaw(
        { startSec: bt + beat * 3.5, midi: m, durSec: beat * 0.4, gain: 0.18 },
        stab, { sampleRate: SR, preset: "stab" },
      );
    }
    // TRAP drum pattern, humanized:
    //   beat 1     — kick (the big 808 hit)
    //   beat 3     — snare + clap
    //   beat 3.5   — kick (the trap stutter)
    //   16th hats with strong swing + 1/32 roll on the last 8th of bar 4 of every 4-bar phrase
    const seedB = `${sectionName}:${b}:`;
    trapKick(drmL, drmR, bt + jit(seedB + "k1"), 0.95);
    kt.push(bt);
    trapSnare(drmL, drmR, bt + beat * 2 + jit(seedB + "s3"), 0.72);
    trapClap(drmL, drmR, bt + beat * 2 + jit(seedB + "c3"), 0.40);
    trapKick(drmL, drmR, bt + beat * 2.5 + jit(seedB + "k35"), 0.70);
    kt.push(bt + beat * 2.5);
    // 16th hats (strong swing — off-16ths delayed 26% of sx)
    for (let e = 0; e < 16; e++) {
      const sw = e % 2 === 1 ? sx * 0.26 : 0;
      const ht = bt + e * sx + sw + jit(seedB + "h" + e, 3);
      const pan = (e % 4 === 0) ? 0 : ((e % 4 === 2) ? 0.3 : (e % 2 ? -0.3 : 0.15));
      hat(drmL, drmR, ht, e % 4 === 0 ? 0.24 : e % 2 ? 0.14 : 0.18, 0.022, pan);
    }
    // 1/32 roll on the last 8th of bar 4 in each 4-bar phrase (trap tic)
    if (b % 4 === 3) {
      for (let r = 0; r < 4; r++) {
        hat(drmL, drmR, bt + beat * 3.5 + r * (sx / 2), 0.18 - r * 0.03, 0.018, r * 0.4 - 0.6);
      }
    }
    // open hat ghost on the 'and of 4' for breath
    if (b % 2 === 1) openHat(drmL, drmR, bt + beat * 3.5, 0.14, 0.4);
  }
  // Place voices in space with reverb sends
  placeStereo(screech, busL, busR, send, -0.45, 0.32);   // screech sits left
  placeStereo(subB,    busL, busR, send,  0.0,  0.10);   // sub centered, dry
  placeStereo(stab,    busL, busR, send,  0.55, 0.45);   // stab right, wet
}

function renderChorus(busL, busR, send, drmL, drmR, kt, t0, bars, prog, isFinal, sectionName) {
  const supL = new Float32Array(N);   // supersaw chord pad (panned wide)
  const supR = new Float32Array(N);
  const lead = new Float32Array(N);   // SUPERSAW LEAD doubling the vocal melody
  const leadOct = new Float32Array(N); // octave-up doubling of the lead
  const harm = new Float32Array(N);   // diatonic-3rd-above harmony counter-line
  const honkMel = new Float32Array(N); // hoover playing the melody too (the gecs honk wall)
  const subB = new Float32Array(N);   // 808 sub

  for (let b = 0; b < bars; b++) {
    const bt = t0 + b * bar;
    const ch = prog[b % prog.length];
    // Lighter chord pad now — the lead carries melody, pad is bedding
    ch.voicing.forEach((m, vi) => {
      const target = vi === 0 ? supL : vi === 2 ? supR : (vi % 2 ? supR : supL);
      mixEventSupersaw(
        { startSec: bt, midi: m, durSec: bar * 0.96, gain: isFinal ? 0.14 : 0.12 },
        target, { sampleRate: SR, preset: "lead" },
      );
    });
    // 808 sub on beat 1 and beat 3.5 (trap kick spots)
    mixEventSkrill(
      { startSec: bt, midi: ch.bass - 12, durSec: beat * 2.2, gain: 0.40, preset: "sub" },
      subB, { sampleRate: SR, bpm: BPM },
    );
    mixEventSkrill(
      { startSec: bt + beat * 2.5, midi: ch.bass - 12, durSec: beat * 1.5, gain: 0.36, preset: "sub" },
      subB, { sampleRate: SR, bpm: BPM },
    );
    // CHORUS LEAD — supersaw + hoover + harmonized 3rd play the same
    // melody the vocal sings. Notes for this bar come from CHORUS_MELODY
    // (filter to events that start within this bar's [b*4, (b+1)*4) beats).
    const barStartBeats = b * 4;
    const barEndBeats = (b + 1) * 4;
    for (const [sb, m, db] of CHORUS_MELODY) {
      if (sb < barStartBeats || sb >= barEndBeats) continue;
      const tNote = bt + (sb - barStartBeats) * beat;
      const dNote = db * beat;
      // primary lead (supersaw, the vocal-doubled hyperpop wall)
      mixEventSupersaw(
        { startSec: tNote, midi: m, durSec: dNote * 0.95, gain: isFinal ? 0.46 : 0.40 },
        lead, { sampleRate: SR, preset: "lead" },
      );
      // octave-up doubling (the chipmunk shimmer that locks with vocal)
      mixEventSupersaw(
        { startSec: tNote, midi: m + 12, durSec: dNote * 0.6, gain: 0.18 },
        leadOct, { sampleRate: SR, preset: "stab" },
      );
      // diatonic 3rd above — gecs-style harmonized counter-line
      mixEventSupersaw(
        { startSec: tNote, midi: third_above(m), durSec: dNote * 0.95, gain: isFinal ? 0.28 : 0.22 },
        harm, { sampleRate: SR, preset: "lead" },
      );
      // hoover honk doubling the melody an octave down (only on
      // sustained notes ≥ 1 beat — keeps the wall feel without mud)
      if (db >= 1) {
        mixEventHoover(
          { startSec: tNote, midi: m - 12, durSec: dNote * 0.85, gain: isFinal ? 0.40 : 0.34 },
          honkMel, { sampleRate: SR, preset: "stab" },
        );
      }
    }
    // TRAP chorus kit — same skeleton as verse but harder, with double-time hat fills on bar 4
    const seedB = `${sectionName}:${b}:`;
    trapKick(drmL, drmR, bt + jit(seedB + "k1"), isFinal ? 1.05 : 0.98);
    kt.push(bt);
    trapSnare(drmL, drmR, bt + beat * 2 + jit(seedB + "s3"), 0.78);
    trapClap(drmL, drmR, bt + beat * 2 + jit(seedB + "c3"), 0.52);
    trapKick(drmL, drmR, bt + beat * 2.5 + jit(seedB + "k35"), 0.75);
    kt.push(bt + beat * 2.5);
    for (let e = 0; e < 16; e++) {
      const sw = e % 2 === 1 ? sx * 0.26 : 0;
      const ht = bt + e * sx + sw + jit(seedB + "h" + e, 3);
      const pan = (e % 4 === 0) ? 0 : ((e % 4 === 2) ? 0.35 : (e % 2 ? -0.35 : 0.15));
      hat(drmL, drmR, ht, e % 4 === 0 ? 0.26 : e % 2 ? 0.15 : 0.20, 0.022, pan);
    }
    // 1/32 hat roll on the last beat of bar 4 in every 4-bar phrase
    if (b % 4 === 3) {
      for (let r = 0; r < 8; r++) {
        hat(drmL, drmR, bt + beat * 3 + r * (sx / 2), 0.22 - r * 0.020, 0.018, r * 0.25 - 0.85);
      }
    }
  }

  // Filter sweep on the chord pad (time-varying lowpass via 1-pole) —
  // opens across each 4-bar phrase, snaps back. Kills the static-GM feel.
  function sweepLP(buf, baseAlpha = 0.06, peakAlpha = 0.45, phraseBars = 4) {
    const phraseSamp = Math.floor(phraseBars * bar * SR);
    let lp = 0;
    for (let i = 0; i < buf.length; i++) {
      const w = (i % phraseSamp) / phraseSamp;
      const a = baseAlpha + (peakAlpha - baseAlpha) * w;
      lp += a * (buf[i] - lp);
      buf[i] = lp;
    }
  }
  sweepLP(supL); sweepLP(supR);
  sweepLP(leadOct, 0.12, 0.60);

  // Place voices in stereo space.
  // Chord pad — kept wide L/R as background bedding.
  for (let i = 0; i < N; i++) {
    busL[i] += supL[i] * 1.0; busR[i] += supR[i] * 1.0;
    send[i] += (supL[i] + supR[i]) * 0.20;
  }
  // Lead doubles vocal — center (where the vocal sits), modest reverb
  placeStereo(lead,    busL, busR, send,  0.00, 0.30);
  placeStereo(leadOct, busL, busR, send,  0.10, 0.50);   // octave shimmer slightly right
  placeStereo(harm,    busL, busR, send, -0.40, 0.45);   // 3rd-above harmony to the left
  placeStereo(honkMel, busL, busR, send,  0.40, 0.55);   // hoover melody-double on the right
  placeStereo(subB,    busL, busR, send,  0.00, 0.06);
}

function renderBreak(busL, busR, send, drmL, drmR, kt, t0, bars, sectionName) {
  // ska/skank flip: half-time trap kit + off-beat upstroke supersaw chords.
  // first 4 bars stripped (no bass), last 2 bars riser → ch3.
  const ch = { bass: 42, voicing: [54, 57, 61] };
  const upstroke = new Float32Array(N);
  const subB = new Float32Array(N);

  for (let b = 0; b < bars; b++) {
    const bt = t0 + b * bar;
    if (b >= 4) {
      mixEventSkrill(
        { startSec: bt, midi: ch.bass - 12, durSec: bar * 0.95, gain: 0.34, preset: "sub" },
        subB, { sampleRate: SR, bpm: BPM },
      );
    }
    // off-beat ska upstrokes — supersaw stab on every 'and'
    for (let q = 0; q < 4; q++) {
      const stabT = bt + q * beat + beat * 0.5 + jit(`${sectionName}:${b}:up${q}`, 4);
      for (const m of ch.voicing) {
        mixEventSupersaw(
          { startSec: stabT, midi: m + 12, durSec: beat * 0.3, gain: 0.16 },
          upstroke, { sampleRate: SR, preset: "stab" },
        );
      }
    }
    // half-time TRAP kit (much sparser than verse/chorus)
    const seedB = `${sectionName}:${b}:`;
    trapKick(drmL, drmR, bt + jit(seedB + "k1"), 0.92);
    kt.push(bt);
    trapSnare(drmL, drmR, bt + beat * 2 + jit(seedB + "s3"), 0.78);
    trapClap(drmL, drmR, bt + beat * 2 + jit(seedB + "c3"), 0.52);
    // sparse open hat on each beat (let it ring — that's the "space")
    for (let q = 0; q < 4; q++) {
      openHat(drmL, drmR, bt + q * beat + jit(seedB + "oh" + q, 4), 0.16, q % 2 ? 0.4 : -0.4);
    }
  }
  // riser through the last 2 bars (sweeps L→R)
  riser(drmL, drmR, t0 + (bars - 2) * bar, 2 * bar, 0.62);

  // Place upstrokes hard-stereo bouncing — quintessential ska feel
  // (alternating L/R per upstroke would need per-event placement;
  //  here we just split mono → L+R wide and drench in reverb)
  for (let i = 0; i < N; i++) {
    const v = upstroke[i];
    // alternate L/R via a 1Hz-ish toggle (~one full L↔R per 2 bars)
    const pan = Math.sin(2 * Math.PI * (i / SR) * (BPM / 60) * 0.5);
    const pL = Math.cos((pan + 1) * Math.PI / 4);
    const pR = Math.sin((pan + 1) * Math.PI / 4);
    busL[i] += v * pL * 1.414;
    busR[i] += v * pR * 1.414;
    send[i] += v * 0.55;
  }
  placeStereo(subB, busL, busR, send, 0, 0.10);
}

function renderOutro(busL, busR, send, drmL, drmR, t0, bars) {
  // Unresolved tail: A major pad + a single low hoover, fade. Drenched
  // in reverb — "the song ends but the room keeps ringing".
  const pad = new Float32Array(N);
  const whoop = new Float32Array(N);
  for (const m of [57, 61, 64]) {
    mixEventSupersaw(
      { startSec: t0, midi: m, durSec: bars * bar, gain: 0.14 },
      pad, { sampleRate: SR, preset: "pad" },
    );
  }
  mixEventHoover(
    { startSec: t0, midi: 45, durSec: bars * bar, gain: 0.24 },
    whoop, { sampleRate: SR, preset: "whoop" },
  );
  placeStereo(pad,   busL, busR, send, -0.3, 0.75);
  placeStereo(whoop, busL, busR, send,  0.3, 0.65);
  // one trap kick on the 1
  trapKick(drmL, drmR, t0, 0.95);
  // sparse hats fading out
  for (let b = 0; b < bars; b++) {
    for (let q = 0; q < 4; q++) {
      const g = 0.10 * (1 - b / bars);
      hat(drmL, drmR, t0 + b * bar + q * beat + beat * 0.5, g, 0.025, q % 2 ? -0.4 : 0.4);
    }
  }
}

// ── arrangement ─────────────────────────────────────────────────────
const SECTIONS = [
  { name: "intro",  bars: 4, kind: "intro"   },
  { name: "v1",     bars: 12, kind: "verse",  prog: VERSE_PROG },
  { name: "ch1",    bars: 8, kind: "chorus", prog: CHORUS_PROG },
  { name: "v2",     bars: 8, kind: "verse",  prog: VERSE_PROG },
  { name: "ch2",    bars: 8, kind: "chorus", prog: CHORUS_PROG },
  { name: "break",  bars: 6, kind: "break"   },
  { name: "ch3",    bars: 8, kind: "chorus", prog: CHORUS_PROG, final: true },
  { name: "outro",  bars: 4, kind: "outro"   },
];

const totalBars = SECTIONS.reduce((a, s) => a + s.bars, 0);
const totalSec = totalBars * bar;
const N = Math.ceil(totalSec * SR);

const busL = new Float32Array(N), busR = new Float32Array(N);
const drmL = new Float32Array(N), drmR = new Float32Array(N);
const send = new Float32Array(N);                // reverb input bus
const wetL = new Float32Array(N), wetR = new Float32Array(N);
const kickTimes = [];

// Compute section start times in seconds — needed for meta-FX placement.
const sectionStarts = {};
{
  let bb = 0;
  for (const sec of SECTIONS) {
    sectionStarts[sec.name] = { startSec: bb * bar, bars: sec.bars, endSec: (bb + sec.bars) * bar };
    bb += sec.bars;
  }
}

let cursor = 0;
for (const sec of SECTIONS) {
  const t0 = cursor * bar;
  if (sec.kind === "intro")  renderIntro(busL, busR, send, drmL, drmR, t0, sec.bars, N);
  if (sec.kind === "verse")  renderVerse(busL, busR, send, drmL, drmR, kickTimes, t0, sec.bars, sec.prog, sec.name);
  if (sec.kind === "chorus") renderChorus(busL, busR, send, drmL, drmR, kickTimes, t0, sec.bars, sec.prog, !!sec.final, sec.name);
  if (sec.kind === "break")  renderBreak(busL, busR, send, drmL, drmR, kickTimes, t0, sec.bars, sec.name);
  if (sec.kind === "outro")  renderOutro(busL, busR, send, drmL, drmR, t0, sec.bars);
  cursor += sec.bars;
}

// ── META-FX layer: section transitions ─────────────────────────────
// These run AFTER all sections are rendered. They modify the already-
// rendered buffers in place (tape stops, stutter chops) and add new
// signal at transition points (reverse swells).

// (1) Reverse cymbal swell — 1 bar before each chorus (ch1 + ch2).
// We render a noise crescendo forward then time-reverse it in place.
function reverseCymbalSwell(L, R, sendBus, startSec, durSec, g = 0.45) {
  const n = Math.floor(durSec * SR);
  const s0 = Math.floor(startSec * SR);
  // forward swell: pink-ish noise w/ exponential growth + filter opening
  const tmpL = new Float32Array(n), tmpR = new Float32Array(n);
  let lpL = 0, lpR = 0;
  for (let i = 0; i < n; i++) {
    const w = i / n;
    const a = 0.05 + 0.55 * w;
    const nzL = Math.random() * 2 - 1;
    const nzR = Math.random() * 2 - 1;
    lpL += a * (nzL - lpL);
    lpR += a * (nzR - lpR);
    const env = Math.pow(w, 1.4);
    tmpL[i] = (nzL - lpL) * env * g;
    tmpR[i] = (nzR - lpR) * env * g;
  }
  // reverse in place
  for (let i = 0; i < n; i++) {
    if (s0 + i < N) {
      L[s0 + i] += tmpL[n - 1 - i];
      R[s0 + i] += tmpR[n - 1 - i];
      sendBus[s0 + i] += (tmpL[n - 1 - i] + tmpR[n - 1 - i]) * 0.5;  // drench
    }
  }
}
reverseCymbalSwell(drmL, drmR, send, sectionStarts.ch1.startSec - bar, bar, 0.50);
reverseCymbalSwell(drmL, drmR, send, sectionStarts.ch2.startSec - bar, bar, 0.45);

// (2) Stutter chop on the last beat of v2 — replace last 4 sixteenths
// with 4 copies of the first sixteenth (the classic "1/16 chop"), then
// the final 1/32 gets repeated twice + silence. Operates on bus + drums.
function stutterChop(L, R, drmL_, drmR_, startSec, beatsToChop = 1) {
  const sliceBeats = beatsToChop / 4;        // 4 sixteenths
  const sliceSec = sliceBeats * beat;
  const sliceN = Math.floor(sliceSec * SR);
  const s0 = Math.floor(startSec * SR);
  // grab the FIRST slice (the "ka" at startSec) from each buffer
  const cap = (buf) => {
    const out = new Float32Array(sliceN);
    for (let i = 0; i < sliceN && s0 + i < N; i++) out[i] = buf[s0 + i];
    return out;
  };
  const sL = cap(L), sR = cap(R), sDL = cap(drmL_), sDR = cap(drmR_);
  // overwrite the next 3 slices with that same captured slice (chop ×4)
  for (let k = 1; k < 4; k++) {
    const off = s0 + k * sliceN;
    for (let i = 0; i < sliceN && off + i < N; i++) {
      // taper amplitude across repeats so it doesn't sound like a stuck CD
      const g = 1.0 - k * 0.15;
      L[off + i]    = sL[i] * g;
      R[off + i]    = sR[i] * g;
      drmL_[off + i] = sDL[i] * g;
      drmR_[off + i] = sDR[i] * g;
    }
  }
}
stutterChop(busL, busR, drmL, drmR, sectionStarts.v2.endSec - beat * 1, 1);

// (3) Tape stop into break — last 0.4s of ch2. Slow pitch-down via
// progressively-stretched read of the source, then fade.
function tapeStop(L, R, drmL_, drmR_, sendBus, startSec, durSec = 0.45) {
  const n = Math.floor(durSec * SR);
  const s0 = Math.floor(startSec * SR);
  // Capture the source region first (so we can overwrite it).
  const capLen = n;
  const srcL  = new Float32Array(capLen);
  const srcR  = new Float32Array(capLen);
  const srcDL = new Float32Array(capLen);
  const srcDR = new Float32Array(capLen);
  for (let i = 0; i < capLen; i++) {
    if (s0 + i < N) {
      srcL[i]  = L[s0 + i];
      srcR[i]  = R[s0 + i];
      srcDL[i] = drmL_[s0 + i];
      srcDR[i] = drmR_[s0 + i];
    }
  }
  // Read pointer starts at 0, advances at rate = 1.0 → 0.0 (quadratic).
  let readPos = 0;
  for (let i = 0; i < n; i++) {
    const w = i / n;
    const rate = 1.0 - Math.pow(w, 1.6);            // 1 → 0
    const env = Math.pow(1 - w, 1.4);               // amplitude fade
    const idx = Math.floor(readPos);
    const frac = readPos - idx;
    const lerp = (b) => {
      if (idx >= capLen - 1) return 0;
      return b[idx] * (1 - frac) + b[idx + 1] * frac;
    };
    if (s0 + i < N) {
      L[s0 + i]    = lerp(srcL)  * env;
      R[s0 + i]    = lerp(srcR)  * env;
      drmL_[s0 + i] = lerp(srcDL) * env;
      drmR_[s0 + i] = lerp(srcDR) * env;
      sendBus[s0 + i] *= env;                       // also fade send to silence
    }
    readPos += rate;
  }
}
tapeStop(busL, busR, drmL, drmR, send, sectionStarts.break.startSec - 0.45, 0.45);

// ── reverb (deep plate, the missing space) ──────────────────────────
// Reverb wet level is automated per section (more wet in break + outro,
// less wet in chorus where the dry hooks need to punch through).
reverbToStereo(send, wetL, wetR, { room: 0.80, damp: 0.32, wet: 0.45 });

// Apply per-section reverb wet automation (gain on the wet buffers).
function wetEnv(sec) {
  switch (sec.kind) {
    case "intro":  return 1.35;
    case "verse":  return 0.85;
    case "chorus": return 0.75;
    case "break":  return 1.55;        // BIG space in the break
    case "outro":  return 1.50;
    default: return 1.0;
  }
}
for (const sec of SECTIONS) {
  const { startSec, endSec } = sectionStarts[sec.name];
  const g = wetEnv(sec);
  const s0 = Math.floor(startSec * SR), s1 = Math.min(N, Math.floor(endSec * SR));
  // smooth crossfade over 50ms at boundaries to avoid clicks
  const xfN = Math.floor(0.05 * SR);
  for (let i = s0; i < s1; i++) {
    let mul = g;
    if (i - s0 < xfN) {
      const w = (i - s0) / xfN;
      mul = 1.0 * (1 - w) + g * w;
    } else if (s1 - i < xfN) {
      const w = (s1 - i) / xfN;
      mul = g * w + 1.0 * (1 - w);
    }
    wetL[i] *= mul;
    wetR[i] *= mul;
  }
}

// ── sidechain (heavy pump on the synth bus, NOT the reverb) ─────────
// Duck depth automated per section: deeper in chorus (the pump), lighter
// in verse, OFF in break (so the ska upstrokes breathe).
function duckDepth(secKind) {
  switch (secKind) {
    case "verse":  return 0.55;        // dip to .55 → recover
    case "chorus": return 0.32;        // dip to .32 → recover (hard pump)
    case "break":  return 0.85;        // light, almost off
    case "intro":  return 0.70;
    case "outro":  return 0.65;
  }
  return 0.45;
}
function kickSecKind(t) {
  for (const sec of SECTIONS) {
    const s = sectionStarts[sec.name];
    if (t >= s.startSec && t < s.endSec) return sec.kind;
  }
  return "verse";
}
const duck = new Float32Array(N).fill(1);
for (const kt of kickTimes) {
  const dipFloor = duckDepth(kickSecKind(kt));
  const s0 = Math.floor(kt * SR), len = Math.floor(0.20 * SR);
  for (let i = 0; i < len && s0 + i < N; i++) {
    const w = i / len;
    const d = dipFloor + (1.0 - dipFloor) * w;
    if (d < duck[s0 + i]) duck[s0 + i] = d;
  }
}

// ── MASTER FX ENVELOPE CHAIN ────────────────────────────────────────
// Time-varying processing on the (synth bus + wet reverb + drums) mix
// BEFORE saturation/crush. The envelope morphs across sections to give
// the song meta-level shape — kills the static GM feel.
//
//   - lowpass alpha (a=1 → bypass, a=0 → DC). Drops between sections to
//     create a momentary "muffle" then snaps open.
//   - highpass alpha (the 1-pole HP). Rises in the break — opens up the
//     low end for the kick to thump, also that "filter ramp" gecs trick.
//   - saturation drive — gentle in verse, hot in chorus (the wall).
//   - bitcrush level — harsher in chorus + final chorus.
//
// All envelopes are stored as keyframes [time, value] and lerp'd per-sample.

const songEnd = totalSec;
function env(keyframes) {
  return (t) => {
    // binary-search would be cleaner but keyframes are short
    let prev = keyframes[0], next = keyframes[keyframes.length - 1];
    for (let i = 0; i < keyframes.length - 1; i++) {
      if (t >= keyframes[i][0] && t <= keyframes[i + 1][0]) {
        prev = keyframes[i]; next = keyframes[i + 1]; break;
      }
    }
    if (t <= prev[0]) return prev[1];
    if (t >= next[0]) return next[1];
    const w = (t - prev[0]) / (next[0] - prev[0]);
    return prev[1] * (1 - w) + next[1] * w;
  };
}

const T = sectionStarts;
// LP alpha — 1.0 = wide open. Dip to 0.35 at section boundaries for the
// "filter close" gecs effect, then snap back open across the first bar.
const lpEnv = env([
  [0,                            1.00],
  [T.v1.startSec,                0.50],
  [T.v1.startSec + bar,          1.00],
  [T.ch1.startSec - 0.15,        0.35],
  [T.ch1.startSec + bar * 0.5,   1.00],
  [T.v2.startSec - 0.15,         0.45],
  [T.v2.startSec + bar * 0.5,    1.00],
  [T.ch2.startSec - 0.15,        0.35],
  [T.ch2.startSec + bar * 0.5,   1.00],
  [T.break.startSec - 0.05,      0.25],     // tape stop already pulls it down
  [T.break.startSec + bar,       0.85],
  [T.ch3.startSec - 0.15,        0.30],
  [T.ch3.startSec + bar * 0.3,   1.00],     // CH3 SLAM OPEN
  [T.outro.startSec,             1.00],
  [songEnd,                      0.55],
]);
// HP alpha — 0 = bypass; rises = more low-cut. Climb during break for
// the "filter ramp" thinning, then drop back at ch3.
const hpEnv = env([
  [0,                            0.0],
  [T.break.startSec,             0.05],
  [T.break.startSec + bar * 3,   0.35],     // peak thin in middle of break
  [T.break.endSec - bar,         0.10],
  [T.ch3.startSec,               0.0],
  [songEnd,                      0.0],
]);
// Saturation drive — gentle (1.0) verse, hotter (1.45) chorus, off in break.
const driveEnv = env([
  [0,                          1.00],
  [T.v1.startSec,              1.05],
  [T.ch1.startSec,             1.35],
  [T.ch1.endSec,               1.10],
  [T.v2.startSec,              1.10],
  [T.ch2.startSec,             1.40],
  [T.ch2.endSec,               1.40],
  [T.break.startSec,           0.90],       // break breathes
  [T.break.endSec,             1.30],
  [T.ch3.startSec,             1.55],       // final chorus hottest
  [T.outro.startSec,           1.10],
  [songEnd,                    0.90],
]);
// Bitcrush levels — fewer levels = crunchier. 256 ≈ transparent.
const crushEnv = env([
  [0,                          256],
  [T.v1.startSec,              160],
  [T.ch1.startSec,              96],
  [T.v2.startSec,              160],
  [T.ch2.startSec,              80],
  [T.break.startSec,           256],        // clean for the break
  [T.ch3.startSec,              64],        // CH3 cranks the crush
  [T.outro.startSec,           160],
  [songEnd,                    256],
]);

// Apply master FX in-place — 1-pole LP + 1-pole HP + drive + crush.
const masterL = new Float32Array(N), masterR = new Float32Array(N);
{
  let lpStateL = 0, lpStateR = 0;
  let hpStateL = 0, hpStateR = 0, hpInPrevL = 0, hpInPrevR = 0;
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    const a = lpEnv(t);            // 1-pole LP coefficient
    const h = hpEnv(t);            // HP amount (0=off)
    const drv = driveEnv(t);
    const lvls = crushEnv(t);

    let xL = busL[i] * duck[i] + wetL[i] * 0.85 + drmL[i];
    let xR = busR[i] * duck[i] + wetR[i] * 0.85 + drmR[i];

    // LP
    lpStateL += a * (xL - lpStateL);
    lpStateR += a * (xR - lpStateR);
    let yL = lpStateL, yR = lpStateR;

    // HP (1-pole HP = x - x_lp; blend by h)
    if (h > 0.001) {
      // simple HP: y[n] = (1-h)*(y[n-1] + x[n] - x[n-1])
      const k = 1 - h;
      hpStateL = k * (hpStateL + yL - hpInPrevL);
      hpStateR = k * (hpStateR + yR - hpInPrevR);
      hpInPrevL = yL; hpInPrevR = yR;
      yL = hpStateL; yR = hpStateR;
    } else {
      hpInPrevL = yL; hpInPrevR = yR;
      hpStateL = yL; hpStateR = yR;
    }

    // Drive + crush
    masterL[i] = crush(Math.tanh(yL * drv), lvls);
    masterR[i] = crush(Math.tanh(yR * drv), lvls);
  }
}

// ── master out ───────────────────────────────────────────────────────
const mix = new Float32Array(N * 2);
let peak = 0;
for (let i = 0; i < N; i++) {
  mix[i * 2]     = masterL[i];
  mix[i * 2 + 1] = masterR[i];
  if (Math.abs(masterL[i]) > peak) peak = Math.abs(masterL[i]);
  if (Math.abs(masterR[i]) > peak) peak = Math.abs(masterR[i]);
}
if (peak > 0) { const nrm = 0.82 / peak; for (let i = 0; i < mix.length; i++) mix[i] *= nrm; }

const outPath = expandHome(flags.out) || resolve(HERE, "../out/gecs.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(mix.length * 4);
for (let i = 0; i < mix.length; i++) b.writeFloatLE(mix[i], i * 4);
writeFileSync(rawPath, b);
console.log(
  `→ gecs [bed v3 · trap+space+meta-fx+harmony] · ${BPM} BPM · ${totalBars} bars · ${(totalBars * bar).toFixed(1)}s · ` +
  `stereo + reverb + envelopes · intro → v1 → ch1 → v2 → ch2 → BREAK(ska) → ch3 → outro`,
);
const ff = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-c:a", "libmp3lame", "-q:a", "2", outPath,
], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath}`);
