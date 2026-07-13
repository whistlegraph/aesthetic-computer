// wexo, 26.07.12
// Pendulum wave × pentatonic pluck — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `wexo 0.5`, the tap/XY "pump", audio polling). This file only describes what
// makes wexo wexo: a row of N hanging bobs of gradually increasing length that
// swing side-to-side at slightly different periods.
//
// ALLEGORY — the classic mesmerizing pendulum wave. All bobs start aligned,
// drift into a travelling-wave then a snake, then periodically RE-ALIGN. Each
// pendulum PLUCKS its note the instant it swings through BOTTOM-center (pitch by
// index/length), so the visual sweep = a cascading pentatonic arpeggio; the
// re-alignment moment = a chord (they pluck together) + a bass swell. The phases
// are derived from the UTC clock (simMs / beatSeconds) so it's beat-synced,
// loops, and two instances opened anywhere match.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Physics ----------------------------------------------------------------
// N pendulums. Over one CYCLE the fastest completes (BASE + N-1) swings and the
// slowest completes BASE swings, so they all re-phase at cycle boundaries — the
// signature pendulum-wave re-alignment. We drive the whole cycle from UTC time.
const N = 15; // pendulum count
const BASE_SWINGS = 20; // half-swings the slowest bob makes per cycle
const CYCLE_BEATS = 16; // one full re-alignment cycle spans this many beats

// Pentatonic scale (A minor pentatonic across octaves), one note per pendulum,
// LOW (long, slow) bob → high (short, fast) bob. Longest pendulum = lowest note.
const PENTA = ["a2", "c3", "d3", "e3", "g3", "a3", "c4", "d4", "e4", "g4", "a4", "c5", "d5", "e5", "g5"];
const BASS = ["a1", "e1", "f1", "c2"]; // swell root cycling per re-alignment

// Each pendulum swings as sin(phase). It crosses BOTTOM-center (angle 0) each
// time sin() passes through zero — i.e. twice per full oscillation. We count
// zero-crossings to fire a pluck exactly at bottom-center.
// swings[i] = number of half-oscillations pendulum i makes over one CYCLE.
const SWINGS = [];
for (let i = 0; i < N; i++) SWINGS.push(BASE_SWINGS + i);

// --- wexo-specific state (engine owns pump/bursts/rhythm) -------------------
let prevZero = new Array(N).fill(0); // last zero-crossing index seen per bob
let glow = new Array(N).fill(0); // per-bob pluck flash 0..1
let push = new Array(N).fill(0); // per-bob tap "wider swing" boost 0..1
let alignFlash = 0; // whole-row re-alignment bloom
let prevCycle = -1; // last completed cycle index (for chord/bass at boundary)

// --- helpers ----------------------------------------------------------------
// UTC-driven cycle phase 0..1 (one pendulum-wave cycle). simMs / beat length.
function cyclePhase(simMs, beatSeconds) {
  const beats = simMs / (beatSeconds * 1000);
  return beats / CYCLE_BEATS; // grows without bound; frac = position in cycle
}
// Pendulum i angle at cycle-phase p (radians of the swing, -1..1 as sin).
function swingSin(i, p) {
  return Math.sin(SWINGS[i] * p * Math.PI * 2);
}
// Pluck each pendulum's note by index — mapped straight into PENTA.
function pluckOf(i) {
  return PENTA[i % PENTA.length];
}

const CONFIG = {
  bpm: 90,
  steps: CYCLE_BEATS,
  drawBursts: false, // wexo draws its own tap flashes in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.24, feedback: 0.5 });
    },

    // We DON'T rely on onBeat for the plucks — the allegory is that each bob
    // plucks at BOTTOM-center, which happens off the beat grid. We detect those
    // zero-crossings in onSim (below) and strike directly. onBeat only handles
    // the whole-cycle chord + bass swell at re-alignment.
    onBeat({ idx, synth }) {
      // Cycle boundary → re-alignment. Every CYCLE_BEATS beats a cycle wraps.
      const cyc = Math.floor(idx / CYCLE_BEATS);
      if (cyc !== prevCycle) {
        prevCycle = cyc;
        alignFlash = 1;
        // BASS SWELL under the alignment.
        const bass = BASS[((cyc % BASS.length) + BASS.length) % BASS.length];
        voices.sub(synth, bass, { beats: 3.2, decay: 0.6, volume: 0.55, attack: 0.08 });
      }
    },

    onSim({ simMs, beatSeconds, sound }) {
      const synth = sound?.synth;
      const p = cyclePhase(simMs, beatSeconds);

      // Detect BOTTOM-center crossings per pendulum and pluck the note there.
      // Zero-crossings of sin(SWINGS[i]*p*2π) land at integer multiples of
      // 0.5/SWINGS[i] in p. We count them and fire on each new one.
      for (let i = 0; i < N; i++) {
        const crossings = Math.floor(SWINGS[i] * p * 2); // 2 per oscillation
        if (crossings !== prevZero[i]) {
          prevZero[i] = crossings;
          glow[i] = 1;
          if (synth) {
            const note = pluckOf(i);
            const pan = (i / (N - 1)) * 2 - 1; // low=left, high=right
            const vol = 0.32 + push[i] * 0.28; // pushed bobs pluck louder
            voices.pluck(synth, note, { beats: 0.7, decay: 0.5, volume: vol, pan });
            // Bright bell accent on the top (short/fast) pendulums.
            if (i > N * 0.7) voices.bell(synth, note, { beats: 0.4, volume: 0.12, pan });
          }
        }
      }

      // Decays.
      alignFlash *= 0.9;
      for (let i = 0; i < N; i++) {
        glow[i] *= 0.86;
        push[i] *= 0.985; // pushes ease out slowly (the bob keeps swinging wide)
      }
    },

    // Tap = give a pendulum a PUSH (wider swing + glow) and pluck it. X → which
    // pendulum, Y → pitch shift within the pentatonic run.
    onTap({ x, y, synth }) {
      const i = Math.max(0, Math.min(N - 1, Math.floor(x * N)));
      push[i] = 1;
      glow[i] = 1;
      const pIdx = Math.max(0, Math.min(PENTA.length - 1, Math.floor((1 - y) * PENTA.length)));
      const note = PENTA[pIdx];
      const pan = (i / (N - 1)) * 2 - 1;
      voices.pluck(synth, note, { beats: 0.8, volume: 0.6, pan });
      voices.bell(synth, note, { beats: 0.4, volume: 0.3 * (1 - y), pan });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { simMs, beatSeconds, amp, quality } = s;
      const { width: w, height: h } = screen;

      // Dark veil (cheap fade — elegant physics-toy backdrop).
      ink(6, 8, 16, 255).box(0, 0, w, h);

      const p = cyclePhase(simMs, beatSeconds);

      // Layout: bobs hang from a pivot rail near the top, spread across width.
      const railY = h * 0.16;
      const marginX = w * 0.08;
      const spanX = w - marginX * 2;
      const maxLen = h * 0.66; // longest pendulum reaches near the bottom
      const minLen = h * 0.34;
      // Swing amplitude (max horizontal sway of a bob), in radians of pivot arc.
      const baseAmp = 0.42;

      // Pivot rail.
      ink(40, 48, 70, 200).line(marginX, railY, w - marginX, railY, 1);

      // Re-alignment bloom: a soft horizontal glow across the whole rail.
      if (alignFlash > 0.02) {
        const a = Math.round(60 * alignFlash);
        ink(120, 160, 255, a).box(0, railY - 4, w, h - railY);
      }

      // Draw each pendulum: string from pivot to bob, then the bob circle.
      for (let i = 0; i < N; i++) {
        const pivotX = marginX + (N === 1 ? spanX / 2 : (i / (N - 1)) * spanX);
        // Longest at the LEFT (lowest note), shortest at the right.
        const t = i / (N - 1);
        const len = maxLen - t * (maxLen - minLen);
        const swingAmp = baseAmp * (1 + push[i] * 0.7); // pushed bobs swing wider
        const angle = swingAmp * swingSin(i, p); // horizontal swing angle
        const bx = pivotX + Math.sin(angle) * len;
        const by = railY + Math.cos(angle) * len;

        // Hue by pendulum index (pitch) — low=warm, high=cool.
        const hue = 20 + t * 260;
        const g = glow[i];
        const light = 45 + g * 40 + push[i] * 15;
        const [r0, g0, b0] = num.hslToRgb(hue % 360, 85, Math.min(95, light));

        // String.
        ink(r0, g0, b0, 90 + g * 120).line(pivotX, railY, bx, by, 1);
        // Pivot dot.
        ink(80, 90, 120, 200).circle(pivotX, railY, 2, true);

        // Bob — size by length (longer = heavier), brighter when plucking.
        const rad = 4 + (1 - t) * 5 + g * 6 + push[i] * 3;
        // Bottom-center proximity → extra bright halo (the pluck moment).
        const nearBottom = 1 - Math.min(1, Math.abs(Math.sin(angle)) * 3);
        const halo = Math.max(g, nearBottom * 0.5);
        if (halo > 0.05) {
          ink(255, 255, 255, Math.round(120 * halo)).circle(bx, by, rad + 4 + halo * 6, true);
        }
        ink(r0, g0, b0, 235).circle(bx, by, rad, true);
        // Inner sparkle on pluck.
        if (g > 0.3) ink(255, 255, 255, Math.round(200 * g)).circle(bx, by, rad * 0.5, true);
      }

      // A faint "bottom-center" reference line where the plucks happen — makes
      // the travelling wave read as it sweeps across.
      const midlineA = 30 + Math.round(amp * 60);
      ink(60, 80, 140, midlineA).line(marginX, railY + maxLen * 0.5, w - marginX, railY + maxLen * 0.5, 1);
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
