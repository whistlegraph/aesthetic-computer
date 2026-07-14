// welto, 26.07.14
// Pendulum wave you can GRAB — a thin wrapper over lib/pads.mjs (the shared
// pad engine: UTC-clock beat grid, `params[0]` rate override e.g. `welto 0.5`,
// the tap/XY "pump", audio polling). Child of wexo: same N-bob pendulum row,
// same pentatonic-by-position tuning, same re-alignment cycle — but wexo's
// onTap only nudged a bob (a small `push`); here the tap/drag IS the piece.
//
// ALLEGORY — you reach into the wave and hold one bob out by hand, like
// pulling back a slingshot. While your finger is down it OWNS that bob: the
// bob stops obeying physics and instead follows your finger's position inside
// its own lane (drag left/right for how far you pull it), while its vertical
// position scrubs a live pentatonic note under your touch — a bowed, glissy
// voice instead of a discrete pluck. Neighboring bobs feel the tug too (a
// coupled-pendulum ripple: real hanging bobs share a rail, so a hard yank on
// one visibly jostles the ones beside it). Let go, and the natural cycle
// reclaims the bob — the harder you pulled, the louder the snap and the wider
// it swings on its way back into the pack.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Physics ----------------------------------------------------------------
const N = 15; // pendulum count
const BASE_SWINGS = 20; // half-swings the slowest bob makes per cycle
const CYCLE_BEATS = 16; // one full re-alignment cycle spans this many beats
const BASE_AMP = 0.42; // natural swing amplitude (radians of pivot arc)
const MAX_PULL = BASE_AMP * 2.2; // how far a full-tension grab can drag a bob — past what physics alone would ever reach, so puppeteering reads as an OVERRIDE

const PENTA = ["a2", "c3", "d3", "e3", "g3", "a3", "c4", "d4", "e4", "g4", "a4", "c5", "d5", "e5", "g5"];
const BASS = ["a1", "e1", "f1", "c2"]; // swell root cycling per re-alignment

const SWINGS = [];
for (let i = 0; i < N; i++) SWINGS.push(BASE_SWINGS + i);

// --- welto-specific state (engine owns pump/bursts/rhythm) ------------------
let prevZero = new Array(N).fill(0); // last zero-crossing index seen per bob
let glow = new Array(N).fill(0); // per-bob pluck flash 0..1
let push = new Array(N).fill(0); // per-bob "swing wider" boost 0..1 (ripple + release kick)
let curAngle = new Array(N).fill(0); // this frame's rendered angle (natural, or blended with a grab)
let holdAmt = new Array(N).fill(0); // 0..1 — how much a bob is currently under finger control
let holdWasActive = new Array(N).fill(false); // so we can detect the RELEASE edge, not just presence
let capturedAngle = new Array(N).fill(0); // the angle the finger is holding this bob at
let tensionAtCapture = new Array(N).fill(0); // 0..1 how far off-center the grab was — drives the snap
let noteAtCapture = new Array(N).fill(PENTA[0]); // last scrubbed note — what plays on release
let alignFlash = 0;
let prevCycle = -1;
let lastTapX = 0.5, lastTapY = 0.5; // last finger position, normalized — for drawing the tether
let lastScrubIdx = -1, lastScrubY = -1; // gates the scrub voice so a still finger doesn't spam notes

function cyclePhase(simMs, beatSeconds) {
  const beats = simMs / (beatSeconds * 1000);
  return beats / CYCLE_BEATS;
}
function swingSin(i, p) {
  return Math.sin(SWINGS[i] * p * Math.PI * 2);
}
function pluckOf(i) {
  return PENTA[i % PENTA.length];
}

const CONFIG = {
  bpm: 90,
  steps: CYCLE_BEATS,
  drawBursts: false, // welto draws its own grab tether + flashes in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.24, feedback: 0.5 });
    },

    onBeat({ idx, synth }) {
      const cyc = Math.floor(idx / CYCLE_BEATS);
      if (cyc !== prevCycle) {
        prevCycle = cyc;
        alignFlash = 1;
        const bass = BASS[((cyc % BASS.length) + BASS.length) % BASS.length];
        voices.sub(synth, bass, { beats: 3.2, decay: 0.6, volume: 0.55, attack: 0.08 });
      }
    },

    onSim({ simMs, beatSeconds, sound }) {
      const synth = sound?.synth;
      const p = cyclePhase(simMs, beatSeconds);

      for (let i = 0; i < N; i++) {
        const swingAmp = BASE_AMP * (1 + push[i] * 0.7);
        const natural = swingAmp * swingSin(i, p);
        const held = holdAmt[i];
        curAngle[i] = held > 0.001 ? natural * (1 - held) + capturedAngle[i] * held : natural;

        // Bottom-center pluck detection — but while a finger OWNS this bob
        // (held > 0.3) we swallow the crossings instead of auto-plucking, so
        // the manual scrub voice is the only thing you hear from it.
        const crossings = Math.floor(SWINGS[i] * p * 2);
        if (held > 0.3) {
          prevZero[i] = crossings;
        } else if (crossings !== prevZero[i]) {
          prevZero[i] = crossings;
          glow[i] = 1;
          if (synth) {
            const note = pluckOf(i);
            const pan = (i / (N - 1)) * 2 - 1;
            const vol = 0.32 + push[i] * 0.28;
            voices.pluck(synth, note, { beats: 0.7, decay: 0.5, volume: vol, pan });
            if (i > N * 0.7) voices.bell(synth, note, { beats: 0.4, volume: 0.12, pan });
          }
        }

        // RELEASE — the finger let go. The harder it was pulled, the louder
        // the snap and the wider it keeps swinging on the way back in.
        if (holdWasActive[i] && held < 0.15) {
          holdWasActive[i] = false;
          glow[i] = 1;
          const t = tensionAtCapture[i];
          push[i] = Math.max(push[i], 0.55 + t * 0.45);
          if (synth) {
            const pan = (i / (N - 1)) * 2 - 1;
            const note = noteAtCapture[i];
            voices.pluck(synth, note, { beats: 1.0, decay: 0.6, volume: 0.4 + t * 0.5, pan });
            if (t > 0.5) voices.bell(synth, note, { beats: 0.5, volume: 0.2 + t * 0.3, pan });
            if (t > 0.6) voices.sub(synth, BASS[i % BASS.length], { beats: 1.4, decay: 0.5, volume: 0.2 + t * 0.3 });
          }
        }

        holdAmt[i] *= 0.88;
      }

      alignFlash *= 0.9;
      for (let i = 0; i < N; i++) {
        glow[i] *= 0.86;
        push[i] *= 0.985;
      }
    },

    // Tap/drag GRABS the nearest bob: x picks which one AND (via how far
    // across that bob's own lane the finger sits) how far to pull it — the
    // same axis the bob itself swings on, so the pull reads as physical. y
    // scrubs the pentatonic note live, like bowing a string under the finger.
    onTap({ x, y, synth }) {
      const slot = x * N;
      const i = Math.max(0, Math.min(N - 1, Math.floor(slot)));
      const frac = slot - i; // 0..1 position inside this bob's lane
      const tension = Math.abs(frac - 0.5) * 2; // 0 center .. 1 either edge
      const pull = (frac - 0.5) * 2 * MAX_PULL;

      capturedAngle[i] = pull;
      tensionAtCapture[i] = tension;
      holdAmt[i] = 1;
      holdWasActive[i] = true;
      glow[i] = Math.max(glow[i], 0.6);
      lastTapX = x;
      lastTapY = y;

      // Ripple — coupled bobs feel the tug, softer and shorter than a grab.
      if (i > 0) {
        push[i - 1] = Math.max(push[i - 1], tension * 0.5);
        glow[i - 1] = Math.max(glow[i - 1], tension * 0.4);
      }
      if (i < N - 1) {
        push[i + 1] = Math.max(push[i + 1], tension * 0.5);
        glow[i + 1] = Math.max(glow[i + 1], tension * 0.4);
      }

      const pIdx = Math.max(0, Math.min(PENTA.length - 1, Math.floor((1 - y) * PENTA.length)));
      const note = PENTA[pIdx];
      noteAtCapture[i] = note;
      const moved = i !== lastScrubIdx || Math.abs(y - lastScrubY) > 0.025;
      if (moved && synth) {
        const pan = (i / (N - 1)) * 2 - 1;
        voices.pluck(synth, note, { beats: 0.5, decay: 0.35, volume: 0.22 + tension * 0.35, pan });
        lastScrubIdx = i;
        lastScrubY = y;
      }
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { amp, quality } = s;
      const { width: w, height: h } = screen;

      ink(6, 8, 16, 255).box(0, 0, w, h);

      const railY = h * 0.16;
      const marginX = w * 0.08;
      const spanX = w - marginX * 2;
      const maxLen = h * 0.66;
      const minLen = h * 0.34;

      ink(40, 48, 70, 200).line(marginX, railY, w - marginX, railY, 1);

      if (alignFlash > 0.02) {
        const a = Math.round(60 * alignFlash);
        ink(120, 160, 255, a).box(0, railY - 4, w, h - railY);
      }

      let heldIdx = -1, heldAmtMax = 0.2;
      for (let i = 0; i < N; i++) if (holdAmt[i] > heldAmtMax) { heldAmtMax = holdAmt[i]; heldIdx = i; }

      for (let i = 0; i < N; i++) {
        const pivotX = marginX + (N === 1 ? spanX / 2 : (i / (N - 1)) * spanX);
        const t = i / (N - 1);
        const len = maxLen - t * (maxLen - minLen);
        const angle = curAngle[i];
        const bx = pivotX + Math.sin(angle) * len;
        const by = railY + Math.cos(angle) * len;

        const hue = 20 + t * 260;
        const g = glow[i];
        const held = holdAmt[i];
        const light = 45 + g * 40 + push[i] * 15 + held * 20;
        const [r0, g0, b0] = num.hslToRgb(hue % 360, 85, Math.min(95, light));

        // A held bob's string runs hotter and thicker — you can feel the tension.
        const strWidth = held > 0.3 ? 2 : 1;
        ink(r0, g0, b0, 90 + g * 120 + held * 80).line(pivotX, railY, bx, by, strWidth);
        ink(80, 90, 120, 200).circle(pivotX, railY, 2, true);

        const rad = 4 + (1 - t) * 5 + g * 6 + push[i] * 3 + held * 4;
        const nearBottom = 1 - Math.min(1, Math.abs(Math.sin(angle)) * 3);
        const halo = Math.max(g, nearBottom * 0.5, held * 0.7);
        if (halo > 0.05 && quality > 0.4) {
          ink(255, 255, 255, Math.round(120 * halo)).circle(bx, by, rad + 4 + halo * 6, true);
        }
        ink(r0, g0, b0, 235).circle(bx, by, rad, true);
        if (g > 0.3 && quality > 0.5) ink(255, 255, 255, Math.round(200 * g)).circle(bx, by, rad * 0.5, true);
      }

      // Tether — the finger is still holding one bob out of true; draw the pull.
      if (heldIdx >= 0 && quality > 0.35) {
        const pivotX = marginX + (N === 1 ? spanX / 2 : (heldIdx / (N - 1)) * spanX);
        const t = heldIdx / (N - 1);
        const len = maxLen - t * (maxLen - minLen);
        const angle = curAngle[heldIdx];
        const bx = pivotX + Math.sin(angle) * len;
        const by = railY + Math.cos(angle) * len;
        const fx = lastTapX * w, fy = lastTapY * h;
        ink(255, 220, 140, Math.round(160 * heldAmtMax)).line(bx, by, fx, fy, 1);
        ink(255, 220, 140, Math.round(200 * heldAmtMax)).circle(fx, fy, 3 + heldAmtMax * 3, true);
      }

      const midlineA = 30 + Math.round(amp * 60);
      ink(60, 80, 140, midlineA).line(marginX, railY + maxLen * 0.5, w - marginX, railY + maxLen * 0.5, 1);
    },
  },
};

function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
