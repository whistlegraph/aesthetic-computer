// dravo, 26.07.14
// Pendulum wave whose bobs pluck themselves — drolo's row of just-intonation
// swinging pendulums, but instead of a beat-quantized melody stepping through
// them, each pendulum sounds its own note the instant it swings through
// bottom-center (wexo's zero-crossing pluck), and the row's OWN alignment —
// the moment most bobs swing back into phase — is what fires the bass swell,
// not a fixed cycle count. The visual algebra IS the score: a fifth's swing
// ratio (3:2) brings it back through bottom-center in step with the root
// every three swings, and every one of those returns is an audible near-
// consonance; a wide leap's dense ratio (12:5, 5:2…) plucks fast and rarely
// lines up with anything.
//
// ALLEGORY: drolo's pendulum-wave demo, but the bobs are wind chimes — each
// one rings when it passes true-bottom, not on a metronome. The drift into a
// braid and the spontaneous re-alignment are exactly drolo's; what's new is
// that alignment is *heard*, because it's the trigger, not decoration laid
// on top of a fixed beat.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ----------------------------------------------------------------
// D minor pentatonic climb + fall; each step's interval-over-root sets that
// pendulum's swing-frequency ratio, so a fifth swings 3:2 against the root
// and a wide leap swings a dense, fast, quickly-drifting ratio.
const SCORE = [
  "d3", "f3", "g3", "a3", "c4", "a3", "g3", "f3",
  "d3", "f3", "a3", "c4", "d4", "c4", "a3", "g3",
];
const BASS = ["d1", "a1", "f1", "c2", "d1", "a1", "g1", "c2"]; // swell roots, cycled on re-alignment

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
const pitchNorm = (p) => (p - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// interval-over-root (semitones) → small integer swing-ratio a:b. Consonances
// (5th, 4th, octave) get simple ratios and stay near-locked; wider leaps get
// denser ratios and visibly race ahead, drifting out of the row fastest.
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
const PN = SCORE_PITCHES.map(pitchNorm);
const HUE = PN.map((pn) => 260 - pn * 200); // gold (low) → violet (high)

const SWING_SPEED = 0.5; // full swings per beat at ratio 1:1

// --- dravo-specific state (engine owns pump/bursts/rhythm) ----------------
let prevCross = new Array(SCORE.length).fill(0); // last zero-crossing count seen per bob
let glow = new Array(SCORE.length).fill(0);      // per-bob pluck flash 0..1 (wexo's mechanism)
let kicks = new Array(SCORE.length).fill(0);     // per-bob tap perturbation (drolo's mechanism)
let alignFlash = 0; // whole-row re-alignment bloom
let bassKick = 0;   // decaying flash from the last alignment-triggered swell
let wasAligned = false; // hysteresis so the swell fires once per pass-through, not every frame
let bassIdx = 0;

const CONFIG = {
  bpm: 104,
  steps: SCORE.length,
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.27, feedback: 0.5 });
    },

    // Discrete beat grid keeps only a felt tick under the continuous pluck
    // mechanism below — everything melodic now fires off bottom-center.
    onBeat({ synth }) {
      voices.hat(synth, { tone: 6800, beats: 0.08, volume: 0.1 });
    },

    onSim({ simMs, beatSeconds, sound }) {
      const synth = sound?.synth;
      const t = simMs / (beatSeconds * 1000); // continuous UTC beat-time, same grid drolo paints from

      let alignSum = 0;
      for (let i = 0; i < SCORE.length; i++) {
        const phase = t * RATIOS[i] * SWING_SPEED * Math.PI * 2;
        const crossings = Math.floor(phase / Math.PI); // bottom-center = every zero of sin(phase)
        if (crossings !== prevCross[i]) {
          prevCross[i] = crossings;
          glow[i] = 1;
          if (synth) {
            const note = SCORE[i];
            const pan = (i / (SCORE.length - 1)) * 2 - 1;
            voices.pluck(synth, note, { beats: 0.6, decay: 0.45, volume: 0.26 + kicks[i] * 0.22, pan });
            if (PN[i] > 0.6) voices.bell(synth, note, { beats: 0.4, volume: 0.14, pan }); // high, fast bobs ring brighter, wind-chime style
          }
        }
        alignSum += Math.cos((0.55 + 0.3 * PN[i]) * Math.sin(phase));
      }

      const align = alignSum / SCORE.length;
      if (align > 0.92 && !wasAligned) {
        wasAligned = true;
        alignFlash = 1;
        bassKick = 1;
        if (synth) {
          voices.sub(synth, BASS[bassIdx], { beats: 3.2, decay: 0.6, volume: 0.55, attack: 0.08 });
          bassIdx = (bassIdx + 1) % BASS.length;
        }
      } else if (align < 0.85) {
        wasAligned = false;
      }

      alignFlash *= 0.9;
      bassKick *= 0.9;
      for (let i = 0; i < SCORE.length; i++) { glow[i] *= 0.86; kicks[i] *= 0.97; }
    },

    // Tap = flick that pendulum (and nudge its neighbors) + an immediate
    // pluck/bell (engine already bumped pump + drew the burst).
    onTap({ x, y, synth }) {
      const idx = Math.max(0, Math.min(SCORE.length - 1, Math.floor(x * SCORE.length)));
      kicks[idx] = Math.min(2.2, kicks[idx] + 1.3);
      if (kicks[idx - 1] !== undefined) kicks[idx - 1] = Math.min(2.2, kicks[idx - 1] + 0.5);
      if (kicks[idx + 1] !== undefined) kicks[idx + 1] = Math.min(2.2, kicks[idx + 1] + 0.5);
      glow[idx] = 1;

      const note = SCORE[idx];
      const pan = x * 2 - 1;
      voices.pluck(synth, note, { beats: 0.6, volume: 0.5, pan });
      voices.bell(synth, note, { beats: 0.4, volume: 0.3 * (1 - y), pan });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { simMs, beatSeconds, pump, bursts, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      const t = simMs / (beatSeconds * 1000);
      const bass = band("subBass");
      const energy = Math.min(2, bassKick * 0.7 + bass * 1.1 + amp * 0.5 + pump * 0.4);

      // Phosphor veil: prior swings persist and fade (motion trails for free).
      const veil = 46 - Math.min(24, energy * 16 + pump * 8);
      ink(6, 4, 16, veil).box(0, 0, w, h);

      const rodY = h * 0.14;
      const baseL = h * 0.6;
      const spacing = w / (SCORE.length + 1);

      // ADAPTIVE QUALITY: any bob mid-glow always draws; the rest of the row
      // strides down, so a killed frame loses evenly-sampled bobs, not detail.
      const stride = quality >= 1 ? 1 : Math.max(1, Math.round(1 / quality));
      const rings = Math.max(1, Math.round(3 * quality));

      let alignSum = 0, tiltSum = 0;
      for (let i = 0; i < SCORE.length; i++) {
        const g = glow[i];
        if (g < 0.05 && i % stride !== 0) continue;
        const pn = PN[i];
        const phase = t * RATIOS[i] * SWING_SPEED * Math.PI * 2;
        const ampI = (0.55 + 0.3 * pn) * (1 + kicks[i] * 0.9) * (1 + pump * 0.25);
        const angle = ampI * Math.sin(phase);
        const L = baseL * (1 - 0.18 * pn);
        const anchorX = spacing * (i + 1);
        const bx = anchorX + Math.sin(angle) * L;
        const by = rodY + Math.cos(angle) * L;
        alignSum += Math.cos(angle); tiltSum += angle;

        const [r0, g0, b0] = num.hslToRgb(((HUE[i] % 360) + 360) % 360, 90, 50 + pn * 22);
        ink(r0, g0, b0, 55 + g * 90).line(anchorX, rodY, bx, by);

        for (let ri = rings; ri > 0; ri--) {
          const rr = (3 + g * 7) * (ri / rings) * (1 + g * 0.6);
          ink(r0, g0, b0, (30 + g * 60) * (ri / rings)).circle(bx, by, rr, true);
        }
        ink(255, 255, 255, 90 + g * 140).circle(bx, by, 2.4 + g * 3.5, true);
        ink(r0, g0, b0, 140).circle(anchorX, rodY, 2.5, true);
      }

      // The rod itself sways with the row's net lean — a felt sense of weight.
      const tilt = (tiltSum / SCORE.length) * 6;
      ink(90, 70, 130, 90).line(0, rodY - tilt, w, rodY + tilt);

      // Re-alignment shimmer: the same metric that fired the bass swell.
      const align = alignSum / SCORE.length;
      if (align > 0.9) {
        const rays = Math.max(4, Math.round(10 * quality));
        const glowAmt = (align - 0.9) * 10;
        for (let i = 0; i < rays; i++) {
          const a = (i / rays) * Math.PI * 2 + t * 0.3;
          const rx = w / 2 + Math.cos(a) * w * 0.4;
          const ry = rodY + Math.sin(a) * h * 0.25;
          ink(255, 235, 200, 60 * glowAmt).line(w / 2, rodY, rx, ry);
        }
        ink(255, 245, 220, 120 * glowAmt).circle(w / 2, rodY, 14 + glowAmt * 20, true);
      }

      // Bass bloom — the gravity well the whole row hangs from.
      if (bassKick > 0.02 || bass > 0.05) {
        const bloom = Math.max(bassKick, bass * 1.2);
        const bR = Math.min(w, h) * (0.06 + bloom * 0.18);
        for (let i = 3; i > 0; i--)
          ink(60, 30, 150, 14 * bloom).circle(w / 2, h * 0.55, bR * (i / 3), true);
      }

      if (energy > 1.1 || pump > 1.3) blur?.(1);

      // Grounding pulse at the base — the beat you feel under the swing.
      const heartR = 3 + bassKick * 8 + bass * 10 + pump * 6;
      ink(120, 90, 255, 80 + bassKick * 90).circle(w / 2, h * 0.92, heartR * 1.7, true);
      ink(255, 255, 255, 150 + bassKick * 80).circle(w / 2, h * 0.92, heartR, true);

      // Tap bursts (engine-tracked).
      for (const b of bursts) {
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 95, 62);
        ink(r0, g0, b0, 170 * b.life).circle(b.x, b.y, b.r, false, 2 + b.life * 3);
        ink(255, 255, 255, 120 * b.life).circle(b.x, b.y, 3 + b.life * 10, true);
      }
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
