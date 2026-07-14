// vexen, 26.07.14
// Pendulum wave × pentatonic pluck, RUN BACKWARDS — wexo's row of bobs drifts
// apart and BUILDS toward a bright realignment chord; vexen starts there.
// Thin wrapper over lib/pads.mjs (UTC-clock beat grid, `params[0]` rate
// override e.g. `vexen 0.5`, tap/XY pump, audio polling). This file only
// describes what makes vexen vexen: every pendulum is struck TOGETHER at each
// cycle boundary — full chord, full swing, full brightness — and then DECAYS.
// Short, high-pitched bobs bleed energy fastest and freeze silent at
// bottom-center; long, low bobs keep ringing longest. The dense chord thins to
// a single low note, then true stillness, until the next strike resets it. A
// tap REVIVES one bob — injects energy back against the decay — the only thing
// in the piece that fights entropy.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Physics ----------------------------------------------------------------
// N pendulums, same rail-and-bob apparatus as wexo. Where wexo let amplitude
// stay constant forever and only drifted PHASE, vexen holds phase simple and
// decays AMPLITUDE — each cycle is a strike followed by a damped fall to rest.
const N = 15; // pendulum count
const BASE_SWINGS = 20; // half-swings the slowest bob makes per cycle
const CYCLE_BEATS = 16; // one strike→silence cycle spans this many beats

// Pentatonic scale (A minor pentatonic across octaves), one note per pendulum,
// LOW (long, slow) bob → high (short, fast) bob — same physical pairing as
// wexo (longer pendulum = lower note is just true, not part of the mutation).
const PENTA = ["a2", "c3", "d3", "e3", "g3", "a3", "c4", "d4", "e4", "g4", "a4", "c5", "d5", "e5", "g5"];
const BASS = ["a1", "e1", "f1", "c2"]; // strike root cycling per cycle

const SWINGS = [];
for (let i = 0; i < N; i++) SWINGS.push(BASE_SWINGS + i);

// Per-bob damping: HIGHER, shorter pendulums lose energy fastest, so the
// audible range narrows DOWNWARD over a cycle — a falling arpeggio made of
// range, not just note order. The lowest bob is still faintly ringing when
// the next strike lands; the highest is dead within the first beat or two.
const ALPHA = [];
for (let i = 0; i < N; i++) ALPHA.push(0.05 + (i / (N - 1)) * 0.6);
const ENV_FLOOR = 0.06; // below this, a bob is treated as silent/still

// --- vexen-specific state (engine owns pump/bursts/rhythm) ------------------
let prevZero = new Array(N).fill(0); // last zero-crossing index seen per bob
let glow = new Array(N).fill(0); // per-bob pluck flash 0..1
let boost = new Array(N).fill(0); // per-bob tap "revive" energy 0..1, fights decay
let strikeFlash = 0; // whole-row strike bloom (front-loaded, not a climax)
let hush = 0; // 0..1, rises as the row quiets — darkens the veil
let prevCycle = -1; // last cycle index seen (for the strike trigger)

// --- helpers ----------------------------------------------------------------
// Beats elapsed WITHIN the current cycle (sawtooth 0..CYCLE_BEATS).
function cycleBeatsElapsed(simMs, beatSeconds) {
  const beats = simMs / (beatSeconds * 1000);
  return beats % CYCLE_BEATS;
}
function cycleIndex(simMs, beatSeconds) {
  const beats = simMs / (beatSeconds * 1000);
  return Math.floor(beats / CYCLE_BEATS);
}
function swingSin(i, tBeats) {
  const p = tBeats / CYCLE_BEATS;
  return Math.sin(SWINGS[i] * p * Math.PI * 2);
}
// Natural decay envelope for bob i at tBeats since the last strike, 0..1.
function envelopeOf(i, tBeats) {
  return Math.exp(-ALPHA[i] * tBeats);
}
function pluckOf(i) {
  return PENTA[i % PENTA.length];
}

const CONFIG = {
  bpm: 90,
  steps: CYCLE_BEATS,
  drawBursts: false, // vexen draws its own strike/pluck flashes in onPaint
  hooks: {
    onBoot({ sound }) {
      // A longer tail than wexo's — things fading into reverberant quiet.
      sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.65 });
    },

    // Cycle boundary = THE STRIKE (inverted from wexo, where the boundary was
    // the climax). Everything gets hit here, then falls silent across the
    // cycle in onSim.
    onBeat({ idx, synth }) {
      const cyc = Math.floor(idx / CYCLE_BEATS);
      if (cyc !== prevCycle) {
        prevCycle = cyc;
        strikeFlash = 1;
        glow.fill(1);
        prevZero.fill(0);
        const bass = BASS[((cyc % BASS.length) + BASS.length) % BASS.length];
        voices.sub(synth, bass, { beats: 3.6, decay: 0.7, volume: 0.55, attack: 0.02 });
        // A sparse chord (not all 15 — dense-at-once, not dense-forever) so
        // the strike reads as one impact, not a wall of noise.
        for (let k = 0; k < N; k += 3) {
          const pan = (k / (N - 1)) * 2 - 1;
          voices.pluck(synth, pluckOf(k), { beats: 0.9, decay: 0.55, volume: 0.34, pan });
        }
      }
    },

    onSim({ simMs, beatSeconds, sound }) {
      const synth = sound?.synth;
      const tBeats = cycleBeatsElapsed(simMs, beatSeconds);

      let envSum = 0;
      for (let i = 0; i < N; i++) {
        const env = Math.min(1, envelopeOf(i, tBeats) + boost[i]);
        envSum += env;

        const crossings = Math.floor(SWINGS[i] * (tBeats / CYCLE_BEATS) * 2);
        if (crossings !== prevZero[i]) {
          prevZero[i] = crossings;
          if (env > ENV_FLOOR) {
            glow[i] = 1;
            if (synth) {
              const note = pluckOf(i);
              const pan = (i / (N - 1)) * 2 - 1;
              const vol = 0.1 + env * 0.5; // dying bobs pluck quieter and quieter
              voices.pluck(synth, note, { beats: 0.6, decay: 0.45, volume: vol, pan });
              if (i > N * 0.7 && env > 0.35) voices.bell(synth, note, { beats: 0.3, volume: 0.1 * env, pan });
            }
          }
          // env <= ENV_FLOOR: crossing happens but stays silent — the bob is
          // effectively at rest even though the math still ticks it over.
        }
      }

      hush = 1 - Math.min(1, envSum / N); // rises as the row quiets down

      strikeFlash *= 0.88;
      for (let i = 0; i < N; i++) {
        glow[i] *= 0.86;
        boost[i] *= 0.96; // revived energy fades back into the decay curve
      }
    },

    // Tap = REVIVE a bob against the decay (the only thing here that resists
    // entropy). X → which pendulum, Y → pitch shift within the pentatonic run.
    onTap({ x, y, synth }) {
      const i = Math.max(0, Math.min(N - 1, Math.floor(x * N)));
      boost[i] = 1;
      glow[i] = 1;
      const pIdx = Math.max(0, Math.min(PENTA.length - 1, Math.floor((1 - y) * PENTA.length)));
      const note = PENTA[pIdx];
      const pan = (i / (N - 1)) * 2 - 1;
      voices.pluck(synth, note, { beats: 0.8, volume: 0.6, pan });
      voices.bell(synth, note, { beats: 0.4, volume: 0.3 * (1 - y), pan });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { simMs, beatSeconds, quality } = s;
      const { width: w, height: h } = screen;

      // Veil darkens with hush — the row visibly quiets, not just goes silent.
      const veilDark = Math.round(6 + hush * 10);
      ink(veilDark, veilDark + 2, veilDark + 8, 255).box(0, 0, w, h);

      const tBeats = cycleBeatsElapsed(simMs, beatSeconds);

      const railY = h * 0.16;
      const marginX = w * 0.08;
      const spanX = w - marginX * 2;
      const maxLen = h * 0.66;
      const minLen = h * 0.34;
      const baseAmp = 0.42;
      const rich = quality >= 0.6; // skip halos/sparkle under load

      ink(40, 48, 70, 200).line(marginX, railY, w - marginX, railY, 1);

      // Strike bloom — cool/icy, and front-loaded (fires AT the strike, then
      // fades), the inverse of wexo's warm bloom that arrived at the climax.
      if (strikeFlash > 0.02) {
        const a = Math.round(70 * strikeFlash);
        ink(180, 210, 255, a).box(0, railY - 4, w, h - railY);
      }

      for (let i = 0; i < N; i++) {
        const pivotX = marginX + (N === 1 ? spanX / 2 : (i / (N - 1)) * spanX);
        const t = i / (N - 1);
        const len = maxLen - t * (maxLen - minLen);
        const env = Math.min(1, envelopeOf(i, tBeats) + boost[i]);
        const angle = baseAmp * env * swingSin(i, tBeats);
        const bx = pivotX + Math.sin(angle) * len;
        const by = railY + Math.cos(angle) * len;

        const hue = 20 + t * 260;
        const g = glow[i];
        const light = 18 + env * 55 + g * 30;
        const [r0, g0, b0] = num.hslToRgb(hue % 360, 80, Math.min(95, light));

        ink(r0, g0, b0, Math.round(50 + env * 90 + g * 90)).line(pivotX, railY, bx, by, 1);
        ink(80, 90, 120, 200).circle(pivotX, railY, 2, true);

        // Bob shrinks as it dies — visual stillness, not just dim color.
        const rad = (2 + (1 - t) * 5) * (0.45 + env * 0.65) + g * 5;
        if (rich && g > 0.05) {
          ink(255, 255, 255, Math.round(110 * g)).circle(bx, by, rad + 4 + g * 5, true);
        }
        ink(r0, g0, b0, Math.round(120 + env * 115)).circle(bx, by, rad, true);
        if (rich && g > 0.3) ink(255, 255, 255, Math.round(180 * g)).circle(bx, by, rad * 0.5, true);
      }

      // Bottom-center reference — fades with hush, same as the row does.
      const midlineA = Math.round(30 * (1 - hush));
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
