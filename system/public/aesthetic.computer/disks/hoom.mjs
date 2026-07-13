// hoom, 26.07.12
// Radial polyrhythm clock — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `hoom 0.5`, the
// tap/XY "pump", audio polling). This file only describes what makes hoom hoom:
// concentric rings, each carrying N evenly-spaced peg-dots (3, 4, 5, 6, 7…) and
// a single HAND that sweeps at that ring's own period. When a hand crosses a peg
// it STRIKES — the peg flashes and a percussion hit sounds (per-ring pitch +
// timbre). So you SEE the polyrhythm: rings strike at different rates and only
// occasionally align. Clockwork, cross-meter, hypnotic.
//
// The hand angles are DERIVED from the UTC beat phase (ctx.simMs / beatSeconds),
// so strikes land on the grid and two instances opened anywhere stay in lock.
// Strikes are detected + sounded directly in onSim (per-ring last-crossed peg),
// not just on the coarse onBeat — that per-ring crossing IS the polyrhythm.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Rings ------------------------------------------------------------------
// Each ring: peg count, how many beats it takes its hand to sweep once around
// (period), a voice, a base note, and a hue. Different periods vs different peg
// counts = the cross-meter. Inner rings are fast/high, outer rings slow/low.
const RINGS = [
  { pegs: 3, period: 3, voice: "sub",   note: "a1", hue: 275 }, // outer, slow, deep
  { pegs: 4, period: 4, voice: "pluck", note: "e2", hue: 210 },
  { pegs: 5, period: 2, voice: "block", note: "a2", hue: 150 },
  { pegs: 6, period: 3, voice: "bell",  note: "e3", hue: 45  },
  { pegs: 7, period: 1, voice: "hat",   note: "a3", hue: 12  }, // inner, fast, bright
];
const STEPS = 12; // grid length for onBeat (LCM-ish of periods, keeps loop tidy)

// A short square-wave "woodblock" tick — dry, pitched, percussive.
function woodblock(synth, note, o = {}) {
  synth({
    tone: note,
    type: "square",
    beats: o.beats ?? 0.14,
    attack: 0.001,
    decay: 0.22,
    volume: o.volume ?? 0.3,
    pan: o.pan ?? 0,
  });
}

function strike(synth, ring, o = {}) {
  const note = o.note ?? ring.note;
  switch (ring.voice) {
    case "sub":   voices.sub(synth, note, { beats: 1.2, decay: 0.5, volume: 0.5, pan: o.pan }); break;
    case "pluck": voices.pluck(synth, note, { beats: 0.6, decay: 0.5, volume: 0.42, pan: o.pan }); break;
    case "block": woodblock(synth, note, { beats: 0.14, volume: 0.34, pan: o.pan }); break;
    case "bell":  voices.bell(synth, note, { beats: 0.7, volume: 0.24, pan: o.pan }); break;
    case "hat":   voices.hat(synth, { tone: 7000, beats: 0.1, volume: 0.16, pan: o.pan }); break;
  }
}

// --- hoom-specific state (engine owns pump/bursts/rhythm) --------------------
// Per ring: which peg the hand last passed (to detect crossings) + a flash-decay
// per peg. Plus the hand angles for paint.
let flashes = []; // per ring: Float array of per-peg brightness 0..1
let handAngles = []; // per ring: current hand angle (radians)
let lastPeg = []; // per ring: index of last peg the hand crossed
let ringPulse = 0; // whole-clock pulse on any strike
let taps = []; // { angle, r, hue, life } tap sparks

function ensureState() {
  if (flashes.length === RINGS.length) return;
  flashes = RINGS.map((r) => new Array(r.pegs).fill(0));
  handAngles = RINGS.map(() => 0);
  lastPeg = RINGS.map(() => -1);
}

const CONFIG = {
  bpm: 96,
  steps: STEPS,
  drawBursts: false, // hoom draws its own tap sparks
  hooks: {
    onBoot({ sound }) {
      ensureState();
      sound.room?.set?.({ enabled: true, mix: 0.22, feedback: 0.5 });
    },

    // onBeat only stamps the whole-clock pulse; the ACTUAL strikes are detected
    // per-ring in onSim (that's the polyrhythm — rings fire off the beat grid).
    onBeat() {
      ringPulse = Math.min(1.5, ringPulse + 0.35);
    },

    onSim({ simMs, beatSeconds, sound }) {
      ensureState();
      // onSim's ctx doesn't include `synth` — pull it from the api's sound.
      const synth = sound?.synth;
      ringPulse *= 0.92;

      // Global beat phase (float beats since epoch). Hand angle = phase/period.
      const beats = simMs / (beatSeconds * 1000);

      for (let ri = 0; ri < RINGS.length; ri++) {
        const ring = RINGS[ri];
        // Fraction of this ring's revolution, 0..1. Outer rings sweep clockwise,
        // alternate direction per ring for visual interest.
        const dir = ri % 2 === 0 ? 1 : -1;
        const frac = ((beats / ring.period) % 1 + 1) % 1;
        const ang = dir * frac * Math.PI * 2 - Math.PI / 2; // start at top
        handAngles[ri] = ang;

        // Which peg sector is the hand in right now? Pegs are evenly spaced.
        const pegFrac = dir > 0 ? frac : (1 - frac) % 1;
        const peg = Math.floor(pegFrac * ring.pegs) % ring.pegs;

        if (peg !== lastPeg[ri]) {
          // Only fire once we have a prior peg (skip the boot spurious cross).
          if (lastPeg[ri] !== -1 && synth) {
            const pan = Math.cos((peg / ring.pegs) * Math.PI * 2) * 0.6;
            strike(synth, ring, { pan });
          }
          lastPeg[ri] = peg;
          if (flashes[ri]) flashes[ri][peg] = 1;
          ringPulse = Math.min(1.6, ringPulse + 0.25);
        }

        // decay peg flashes
        const f = flashes[ri];
        for (let p = 0; p < f.length; p++) f[p] *= 0.86;
      }

      for (const t of taps) { t.r += 5; t.life -= 0.04; }
      taps = taps.filter((t) => t.life > 0);
    },

    // Tap = spin a ring + strike it. X → which ring, Y → pitch (higher tap =
    // higher note). Also spawns a spark at the tapped angle.
    onTap({ x, y, ex, ey, synth, screen }) {
      ensureState();
      const ri = Math.min(RINGS.length - 1, Math.floor(x * RINGS.length));
      const ring = RINGS[ri];
      const semis = Math.round((1 - y) * 24) - 12; // -12..+12 from base
      const note = transpose(ring.note, semis);
      const pan = x * 2 - 1;
      strike(synth, ring, { note, pan });
      // flash the whole ring
      if (flashes[ri]) for (let p = 0; p < flashes[ri].length; p++) flashes[ri][p] = 1;
      ringPulse = 1.5;
      taps.push({
        angle: Math.atan2(ey - screen.height / 2, ex - screen.width / 2),
        r: 0,
        hue: ring.hue,
        life: 1,
      });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      ensureState();

      const bass = band("subBass");
      const energy = Math.min(2, ringPulse * 0.5 + bass * 1.0 + amp * 0.5 + pump * 0.4);

      // Dark clock face wash.
      ink(6, 6, 12, 255).box(0, 0, w, h);

      const maxR = Math.min(w, h) * 0.44;
      const ringGap = maxR / (RINGS.length + 0.6);
      const innerR = ringGap * 0.8;

      // Detail scales with quality: pegs are always drawn (few), but the smooth
      // ring outline segment count drops when quality is low.
      const seg = quality < 0.6 ? 24 : 48;

      for (let ri = 0; ri < RINGS.length; ri++) {
        const ring = RINGS[ri];
        const R = innerR + ri * ringGap;
        const [rr, rg, rb] = num.hslToRgb(ring.hue, 55, 30);

        // Ring track (faint circle outline via segments).
        let px = null, py = null;
        for (let i = 0; i <= seg; i++) {
          const a = (i / seg) * Math.PI * 2;
          const x = cx + Math.cos(a) * R, y = cy + Math.sin(a) * R;
          if (px !== null) ink(rr, rg, rb, 120).line(px, py, x, y, 1);
          px = x; py = y;
        }

        // Pegs — evenly spaced dots, flash when struck.
        const f = flashes[ri];
        for (let p = 0; p < ring.pegs; p++) {
          const a = (p / ring.pegs) * Math.PI * 2 - Math.PI / 2;
          const px2 = cx + Math.cos(a) * R, py2 = cy + Math.sin(a) * R;
          const fl = f ? f[p] : 0;
          const [pr, pg, pb] = num.hslToRgb(ring.hue, 90, 45 + fl * 45);
          const size = 3 + fl * 9 + energy * 1.2;
          // flash halo
          if (fl > 0.05) {
            const [hr, hg, hb] = num.hslToRgb(ring.hue, 95, 70);
            ink(hr, hg, hb, 120 * fl).circle(px2, py2, size + 6 + fl * 10, true);
          }
          ink(pr, pg, pb, 220).circle(px2, py2, size, true);
          if (fl > 0.3) ink(255, 255, 255, 200 * fl).circle(px2, py2, 2 + fl * 3, true);
        }

        // HAND — a spoke from center to the ring, glowing at its tip.
        const ha = handAngles[ri];
        const hx = cx + Math.cos(ha) * R, hy = cy + Math.sin(ha) * R;
        const [lr, lg, lb] = num.hslToRgb(ring.hue, 85, 60);
        ink(lr, lg, lb, 210).line(cx, cy, hx, hy, 2);
        ink(255, 255, 255, 230).circle(hx, hy, 3 + energy, true);
        ink(lr, lg, lb, 130).circle(hx, hy, 6 + energy * 2, true);
      }

      // Tap sparks — expanding rings, hue by ring.
      for (const t of taps) {
        const [tr, tg, tb] = num.hslToRgb(t.hue, 95, 60);
        ink(tr, tg, tb, 200 * t.life).circle(cx, cy, t.r, false, 2);
      }

      // Central hub — pulses on every strike.
      const hubR = 5 + ringPulse * 9 + bass * 10 + pump * 6;
      ink(200, 200, 255, 90 + ringPulse * 90).circle(cx, cy, hubR * 1.6, true);
      ink(255, 255, 255, 180 + ringPulse * 70).circle(cx, cy, hubR, true);
    },
  },
};

// --- helpers ----------------------------------------------------------------
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
const SEMI_NOTE = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"];
function transpose(note, semis) {
  const m = /^([a-g])(#?)(\d)$/.exec(note);
  if (!m) return note;
  let idx = NOTE_SEMI[m[1]] + (m[2] ? 1 : 0);
  let midi = (parseInt(m[3], 10) + 1) * 12 + idx + semis;
  midi = Math.max(12, Math.min(108, midi));
  const oct = Math.floor(midi / 12) - 1;
  return SEMI_NOTE[((midi % 12) + 12) % 12] + oct;
}

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before the engine
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
