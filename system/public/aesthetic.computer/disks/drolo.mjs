// drolo, 26.07.13
// Pendulum-wave pad — a thin wrapper over lib/pads.mjs (the shared pad engine:
// UTC-clock beat grid, `params[0]` rate override e.g. `drolo 0.5`, the tap/XY
// "pump", audio polling). This file only says what makes drolo drolo: its
// scale-step SCORE and a row of swinging pendulums whose SWING RATIOS are the
// notes' just-intonation intervals over the root.
//
// ALLEGORY: the classic physics "pendulum wave" demo — a row of pendulums
// released together, each tuned to a slightly different period, that drift
// out of phase and then spontaneously re-align — except here every period
// ratio IS a musical interval (5:4, 3:2, 2:1…), so the moments the row snaps
// back into a single sweeping line are literal consonance made visible, and
// the moments it dissolves into a chaotic braid are the dissonant intervals
// working themselves out. The current melody step glows and rings a bell;
// everything else is the harmony it's embedded in, still swinging.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score --------------------------------------------------------------
// D minor pentatonic climb + fall; each step's interval-over-root sets that
// pendulum's swing-frequency ratio, so a fifth swings 3:2 against the root
// and a wide leap swings a dense, fast, quickly-drifting ratio.
const SCORE = [
  "d3", "f3", "g3", "a3", "c4", "a3", "g3", "f3",
  "d3", "f3", "a3", "c4", "d4", "c4", "a3", "g3",
];
const BASS = ["d1", "d1", "a1", "a1", "f1", "f1", "c2", "c2"]; // half-time root

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

// Precomputed once — 16 pendulums, cheap, avoids per-frame recompute.
const RATIOS = SCORE_PITCHES.map((p) => { const [a, b] = ratioFor(p); return a / b; });
const PN = SCORE_PITCHES.map(pitchNorm);
const HUE = PN.map((pn) => 260 - pn * 200); // gold (low) → violet (high)

const SWING_SPEED = 0.5; // full swings per beat at ratio 1:1

// --- drolo-specific visual state (engine owns pump/bursts/rhythm) -------
let lastIdx = 0;       // most recent UTC beat index (continuous, from onBeat)
let activeIdx = 0;     // which pendulum the melody is currently on
let flashLife = 0;     // decaying glow on the active pendulum
let bassKick = 0;      // per-bass-note flash
const kicks = new Array(SCORE.length).fill(0); // per-pendulum tap perturbation

const CONFIG = {
  bpm: 104,
  steps: SCORE.length,
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.5 });
    },

    // A new UTC beat crossed — advance the melody, ring its pendulum.
    onBeat({ idx, synth }) {
      lastIdx = idx;
      const step = ((idx % SCORE.length) + SCORE.length) % SCORE.length;
      activeIdx = step;
      flashLife = 1;
      const note = SCORE[step];
      const pan = Math.sin((step / SCORE.length) * Math.PI * 2) * 0.7;

      voices.bell(synth, note, { beats: 1.2, volume: 0.3, pan });
      voices.pluck(synth, note, { beats: 0.5, decay: 0.4, volume: 0.22, pan });
      synth({ tone: note, type: "sine", beats: 0.9, attack: 0.004, decay: 0.75, volume: 0.12, pan: pan * 0.5 });

      if (step % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.6, decay: 0.5, volume: 0.45 });
        bassKick = 1;
      }
      voices.hat(synth, { tone: 6800, beats: 0.08, volume: 0.1 });
    },

    onSim() {
      flashLife *= 0.9;
      bassKick *= 0.9;
      for (let i = 0; i < kicks.length; i++) kicks[i] *= 0.9;
    },

    // Tap = flick that pendulum (and nudge its neighbors) + a bell/pluck boost
    // (engine already bumped pump + drew the burst). X picks which pendulum.
    onTap({ x, y, synth }) {
      const idx = Math.max(0, Math.min(SCORE.length - 1, Math.floor(x * SCORE.length)));
      kicks[idx] = Math.min(2.2, kicks[idx] + 1.3);
      if (kicks[idx - 1] !== undefined) kicks[idx - 1] = Math.min(2.2, kicks[idx - 1] + 0.5);
      if (kicks[idx + 1] !== undefined) kicks[idx + 1] = Math.min(2.2, kicks[idx + 1] + 0.5);
      activeIdx = idx;
      flashLife = 1;

      const note = SCORE[idx];
      const pan = x * 2 - 1;
      voices.bell(synth, note, { beats: 0.9, volume: 0.4, pan });
      voices.pluck(synth, note, { beats: 0.5, volume: 0.3, pan });
      synth({ tone: note, type: "triangle", beats: 0.6, attack: 0.003, decay: 0.6, volume: 0.18 * (1 - y), pan });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, beatProgress, bursts, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      // Continuous beat-time — every pendulum's phase is this times its own
      // ratio, so the whole row is deterministic from the same beat grid.
      const t = lastIdx + beatProgress;
      const bass = band("subBass");
      const energy = Math.min(2, bassKick * 0.7 + bass * 1.1 + amp * 0.5 + pump * 0.4);

      // Phosphor veil: prior swings persist and fade (motion trails for free).
      const veil = 46 - Math.min(24, energy * 16 + pump * 8);
      ink(6, 4, 16, veil).box(0, 0, w, h);

      const rodY = h * 0.14;
      const baseL = h * 0.6;
      const spacing = w / (SCORE.length + 1);

      // ADAPTIVE QUALITY: at full quality draw every pendulum; below that,
      // stride through the row (still always drawing the active one) so the
      // count scales down instead of per-pendulum detail degrading unevenly.
      const stride = quality >= 1 ? 1 : Math.max(1, Math.round(1 / quality));
      const rings = Math.max(1, Math.round(3 * quality));

      let alignSum = 0, alignCount = 0, tiltSum = 0;
      for (let i = 0; i < SCORE.length; i++) {
        if (i !== activeIdx && i % stride !== 0) continue;
        const pn = PN[i];
        const phase = t * RATIOS[i] * SWING_SPEED * Math.PI * 2;
        const isActive = i === activeIdx;
        const ampI = (0.55 + 0.3 * pn) * (1 + kicks[i] * 0.9) * (1 + pump * 0.25);
        const angle = ampI * Math.sin(phase);
        const L = baseL * (1 - 0.18 * pn);
        const anchorX = spacing * (i + 1);
        const bx = anchorX + Math.sin(angle) * L;
        const by = rodY + Math.cos(angle) * L;
        alignSum += Math.cos(angle); alignCount++;
        tiltSum += angle;

        const [r0, g0, b0] = num.hslToRgb(((HUE[i] % 360) + 360) % 360, 90, 50 + pn * 22);
        ink(r0, g0, b0, (isActive ? 90 + flashLife * 100 : 55)).line(anchorX, rodY, bx, by);

        for (let ri = rings; ri > 0; ri--) {
          const rr = (3 + (isActive ? 6 : 2)) * (ri / rings) * (1 + (isActive ? flashLife : 0) * 0.6);
          ink(r0, g0, b0, (isActive ? 70 : 30) * (ri / rings)).circle(bx, by, rr, true);
        }
        ink(255, 255, 255, isActive ? 180 + flashLife * 60 : 90).circle(bx, by, isActive ? 4 + flashLife * 3 : 2.4, true);
        ink(r0, g0, b0, 140).circle(anchorX, rodY, 2.5, true);
      }

      // The rod itself sways with the row's net lean — a felt sense of weight.
      const tilt = alignCount ? (tiltSum / alignCount) * 6 : 0;
      ink(90, 70, 130, 90).line(0, rodY - tilt, w, rodY + tilt);

      // Re-alignment shimmer: when most pendulums swing back in phase, the
      // row reads as one line — a consonant instant made visible.
      const align = alignCount ? alignSum / alignCount : 0;
      if (align > 0.9) {
        const rays = Math.max(4, Math.round(10 * quality));
        const glow = (align - 0.9) * 10;
        for (let i = 0; i < rays; i++) {
          const a = (i / rays) * Math.PI * 2 + t * 0.3;
          const rx = w / 2 + Math.cos(a) * w * 0.4;
          const ry = rodY + Math.sin(a) * h * 0.25;
          ink(255, 235, 200, 60 * glow).line(w / 2, rodY, rx, ry);
        }
        ink(255, 245, 220, 120 * glow).circle(w / 2, rodY, 14 + glow * 20, true);
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
