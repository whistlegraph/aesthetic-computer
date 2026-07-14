// wandro, 26.07.13
// Pendulum-wave visual (wexo) driven by just-intonation swing ratios and a
// layered pluck/bell/sub voice (drolo) — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `wandro 0.5`, the tap/XY "pump", audio polling). This file only says what
// makes wandro wandro: a fixed melodic SCORE laid out spatially across a row
// of hanging pendulums — bob LENGTH traces the melody's contour (short string
// = high note, long string = low note, so the row's silhouette IS the tune) —
// while each bob's SWING PERIOD is that note's just-intonation ratio over the
// root. Every pendulum plucks its own fixed note the instant it swings through
// bottom-center: physics decides the rhythm, the row's shape decides the
// melody. Consonant intervals (simple ratios) drift back into a single
// sweeping line often; dissonant ones race ahead and rarely come home.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score (inherited from drolo) -------------------------------------------
const SCORE = [
  "d3", "f3", "g3", "a3", "c4", "a3", "g3", "f3",
  "d3", "f3", "a3", "c4", "d4", "c4", "a3", "g3",
];
const BASS = ["d1", "d1", "a1", "a1", "f1", "f1", "c2", "c2"]; // half-time root
const N = SCORE.length;

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const ROOT = notePitch("d3");
const SCORE_PITCHES = SCORE.map(notePitch);
const PITCH_MIN = Math.min(...SCORE_PITCHES);
const PITCH_MAX = Math.max(...SCORE_PITCHES);
const PN = SCORE_PITCHES.map((p) => (p - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN));
const HUE = PN.map((pn) => 20 + pn * 260); // low note=warm, high note=cool

// interval-over-root (semitones) → small integer swing-ratio a:b. Consonances
// (5th, 4th, octave) get simple ratios and re-lock often; wider leaps get
// denser ratios and drift out of the row fastest.
const JUST = {
  0: [1, 1], 3: [6, 5], 4: [5, 4], 5: [4, 3], 7: [3, 2],
  9: [5, 3], 10: [9, 5], 12: [2, 1], 14: [9, 4], 15: [12, 5],
  16: [5, 2], 17: [8, 3], 19: [3, 1],
};
function ratioFor(pitch) {
  const semis = ((pitch - ROOT) % 24 + 24) % 24;
  const r = JUST[semis];
  if (r) return r;
  return [1 + (semis % 5), 3];
}
const RATIOS = SCORE_PITCHES.map((p) => { const [a, b] = ratioFor(p); return a / b; });
const SWING_SPEED = 0.5; // full swings per beat at ratio 1:1

// --- wandro-specific state (engine owns pump/bursts/rhythm) -----------------
let prevSin = new Array(N).fill(0); // last frame's sin(phase) per bob — the zero-crossing detector
let glow = new Array(N).fill(0);    // per-bob pluck flash 0..1
let push = new Array(N).fill(0);    // per-bob tap "wider swing" boost 0..1
let alignFlash = 0;                 // whole-row re-alignment bloom
let wasAligned = false;             // edge-detect the alignment threshold

function beatsNow(simMs, beatSeconds) {
  const b = simMs / (beatSeconds * 1000);
  return Number.isFinite(b) ? b : 0;
}

const CONFIG = {
  bpm: 96,
  steps: N,
  drawBursts: false, // wandro draws its own tap flashes via push/glow, like wexo
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.26, feedback: 0.5 });
    },

    // UTC beat grid carries the harmonic floor under the physically-plucked
    // melody: a half-time bass root (drolo) plus a light hat tick.
    onBeat({ idx, synth }) {
      const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
      voices.sub(synth, BASS[bi], { beats: 1.6, decay: 0.5, volume: 0.4 });
      voices.hat(synth, { tone: 6800, beats: 0.08, volume: 0.09 });
    },

    // Physically-driven plucks: each pendulum strikes its own fixed SCORE
    // note the instant it crosses bottom-center (sin(phase) changes sign) —
    // rhythm comes from the swing, not the beat grid.
    onSim({ simMs, beatSeconds, sound }) {
      const synth = sound?.synth;
      const beats = beatsNow(simMs, beatSeconds);
      let alignSum = 0;

      for (let i = 0; i < N; i++) {
        const phase = RATIOS[i] * SWING_SPEED * beats * Math.PI * 2;
        const sinVal = Math.sin(phase);
        alignSum += Math.cos(phase);

        if (prevSin[i] !== 0 && Math.sign(sinVal) !== 0 && Math.sign(sinVal) !== Math.sign(prevSin[i])) {
          glow[i] = 1;
          if (synth) {
            const note = SCORE[i];
            const pan = (i / (N - 1)) * 2 - 1;
            const vol = 0.22 + push[i] * 0.2;
            voices.pluck(synth, note, { beats: 0.5, decay: 0.4, volume: vol, pan });
            voices.bell(synth, note, { beats: 0.9, volume: 0.22, pan });
            synth({ tone: note, type: "sine", beats: 0.7, attack: 0.004, decay: 0.6, volume: 0.1, pan: pan * 0.5 });
          }
        }
        prevSin[i] = sinVal;
      }

      // Re-alignment: when most bobs swing back in phase, the row snaps
      // into a single sweeping line — a consonant instant made audible too.
      const align = alignSum / N;
      const aligned = align > 0.92;
      if (aligned && !wasAligned && synth) {
        alignFlash = 1;
        voices.sub(synth, "d2", { beats: 2.4, decay: 0.55, volume: 0.4, attack: 0.06 });
      }
      wasAligned = aligned;

      alignFlash *= 0.9;
      for (let i = 0; i < N; i++) {
        glow[i] *= 0.86;
        push[i] *= 0.985;
      }
    },

    // Tap = strike whichever pendulum sits under the finger, widen its swing.
    onTap({ x, y, synth }) {
      const i = Math.max(0, Math.min(N - 1, Math.floor(x * N)));
      push[i] = 1;
      glow[i] = 1;
      const note = SCORE[i];
      const pan = x * 2 - 1;
      voices.pluck(synth, note, { beats: 0.6, volume: 0.4, pan });
      voices.bell(synth, note, { beats: 0.7, volume: 0.35 * (1 - y), pan });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { simMs, beatSeconds, pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      const bass = band("subBass");
      const energy = Math.min(2, bass * 1.1 + amp * 0.5 + pump * 0.4);
      const veil = 46 - Math.min(24, energy * 16 + pump * 8);
      ink(6, 6, 16, veil).box(0, 0, w, h);

      const beats = beatsNow(simMs, beatSeconds);
      const railY = h * 0.16;
      const marginX = w * 0.08;
      const spanX = w - marginX * 2;
      const maxLen = h * 0.66;
      const minLen = h * 0.32;
      const baseAmp = 0.42;

      ink(40, 48, 70, 200).line(marginX, railY, w - marginX, railY, 1);

      if (alignFlash > 0.02) {
        const a = Math.round(60 * alignFlash);
        ink(160, 200, 255, a).box(0, railY - 4, w, h - railY);
      }

      const rings = Math.max(1, Math.round(3 * quality));

      for (let i = 0; i < N; i++) {
        const anchorX = marginX + (N === 1 ? spanX / 2 : (i / (N - 1)) * spanX);
        const pn = PN[i]; // 0=low note (long bob), 1=high note (short bob)
        const len = maxLen - pn * (maxLen - minLen); // bob length traces the melody
        const swingAmp = baseAmp * (1 + push[i] * 0.7) * (1 + pump * 0.2);
        const phase = RATIOS[i] * SWING_SPEED * beats * Math.PI * 2;
        const angle = swingAmp * Math.sin(phase);
        const bx = anchorX + Math.sin(angle) * len;
        const by = railY + Math.cos(angle) * len;

        const g = glow[i];
        const light = 45 + g * 40 + push[i] * 15;
        const [r0, g0, b0] = num.hslToRgb(HUE[i] % 360, 85, Math.min(95, light));

        ink(r0, g0, b0, 90 + g * 120).line(anchorX, railY, bx, by, 1);
        ink(80, 90, 120, 200).circle(anchorX, railY, 2, true);

        const rad = 4 + (1 - pn) * 5 + g * 6 + push[i] * 3; // longer (lower) bob = heavier
        if (g > 0.05) {
          for (let ri = rings; ri > 0; ri--) {
            const rr = (rad + 4) * (ri / rings) * (1 + g * 0.6);
            ink(r0, g0, b0, Math.round(70 * (ri / rings) * g)).circle(bx, by, rr, true);
          }
        }
        ink(r0, g0, b0, 235).circle(bx, by, rad, true);
        if (g > 0.3) ink(255, 255, 255, Math.round(200 * g)).circle(bx, by, rad * 0.5, true);
      }

      const midlineA = 30 + Math.round(amp * 60);
      ink(60, 80, 140, midlineA).line(marginX, railY + maxLen * 0.5, w - marginX, railY + maxLen * 0.5, 1);

      if (energy > 1.2 || pump > 1.3) blur?.(1);
    },
  },
};

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before the engine
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
