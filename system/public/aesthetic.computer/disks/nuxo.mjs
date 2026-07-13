// nuxo, 26.07.12
// String-art harp pad — a thin wrapper over lib/pads.mjs (the shared pad engine:
// UTC-clock beat grid, `params[0]` rate override e.g. `nuxo 0.5`, tap/XY "pump",
// audio polling). This file only describes what makes nuxo nuxo: its pentatonic
// harp score, its richened pluck/bell/sub voices, and its nail-and-thread paint.
// ALLEGORY: pegs ring the border; every arpeggio note strings a NEW colored
// thread between two pegs (pitch → peg-pair offset). The accumulating straight
// lines envelope a curve (cardioid/astroid). Bass = global thread TENSION
// (brightness/hue shift); the beat makes threads shimmer. Threads accumulate
// then fade at the loop so it seams. The visual IS the score.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// Pentatonic (A minor pent: a c d e g), elegant, harp-plucked.
const ARP = [
  "a3", "c4", "e4", "g4", "a4", "e4", "c4", "d4",
  "a3", "d4", "g4", "a4", "c5", "a4", "g4", "e4",
];
const BASS = ["a1", "a1", "f1", "f1", "c2", "c2", "e1", "e1"]; // half-time root
const PEGS = 36; // nails evenly spaced around the border ring

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const ARP_PITCHES = ARP.map(notePitch);
const PITCH_MIN = Math.min(...ARP_PITCHES);
const PITCH_MAX = Math.max(...ARP_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- nuxo-specific visual state (engine owns pump/bursts/rhythm) -------------
let threads = []; // { a, b, hue, life, bright } — one straight thread per note
let tension = 0; // global bass tension: brightens/shifts every thread
let shimmer = 0; // whole-web shimmer, kicked each beat
const BASE_THREADS = 44; // full-quality live thread cap (scaled by ctx.quality)

const CONFIG = {
  bpm: 120,
  steps: ARP.length,
  drawBursts: false, // nuxo strings its own tap thread + glow in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.55 });
    },

    // A new UTC beat crossed — pluck the harp + string the matching thread.
    onBeat({ idx, synth }) {
      const s = ((idx % ARP.length) + ARP.length) % ARP.length;
      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / ARP.length) * Math.PI * 2) * 0.7;

      // Harp: Karplus–Strong pluck, the string being strung.
      voices.pluck(synth, note, { beats: 1.0, decay: 0.6, volume: 0.5, pan });

      // ALLEGORY: string a thread between two pegs. Peg A walks the ring by step;
      // peg B is offset by pitch, so higher notes reach farther across — the set
      // of chords carves the enveloping curve.
      const a = s * Math.round(PEGS / ARP.length);
      const b = (a + 5 + Math.round(pn * (PEGS - 12))) % PEGS;
      threads.push({ a, b, hue: 30 + pn * 280, life: 1, bright: 0.4 + pn * 0.6 });

      // Shimmer bell an octave feel on the high notes.
      if (pn > 0.6) voices.bell(synth, note, { beats: 0.5, volume: 0.16, pan: -pan });

      // Sub-bass root on the half-beat = the tension you feel + SEE.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.6, decay: 0.5, volume: 0.5 });
        tension = 1;
      }

      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.13 });
      shimmer = 1;
    },

    onSim({ quality }) {
      tension *= 0.9;
      shimmer *= 0.88;
      // Fade threads SLOWLY so a full loop's worth accumulates into a visible
      // envelope curve; fade FASTER when over the quality-scaled cap so the
      // oldest clear out and the web loops cleanly.
      const cap = Math.max(10, Math.round(BASE_THREADS * (quality ?? 1)));
      const fade = threads.length > cap ? 0.03 : 0.0016;
      for (const t of threads) t.life -= fade;
      threads = threads.filter((t) => t.life > 0);
      if (threads.length > cap) threads = threads.slice(-cap);
    },

    // Tap = string a bright thread from the NEAREST peg to the tap point + a harp
    // pluck (engine already bumped pump). X→pan/hue, Y→pitch.
    onTap({ x, y, ex, ey, synth, screen }) {
      // Find nearest peg to the tap, string a thread from it to the tap point.
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;
      const rx = w * 0.44, ry = h * 0.44;
      let best = 0, bestD = Infinity;
      for (let i = 0; i < PEGS; i++) {
        const ang = (i / PEGS) * Math.PI * 2 - Math.PI / 2;
        const px = cx + Math.cos(ang) * rx, py = cy + Math.sin(ang) * ry;
        const d = (px - ex) * (px - ex) + (py - ey) * (py - ey);
        if (d < bestD) { bestD = d; best = i; }
      }
      threads.push({ a: best, tap: [ex, ey], hue: x * 360, life: 1.3, bright: 1 });

      const note = ["a", "c", "d", "e", "g"][Math.floor(x * 5)] + (3 + Math.floor((1 - y) * 3));
      voices.pluck(synth, note, { beats: 0.7, volume: 0.55, pan: x * 2 - 1 });
      voices.bell(synth, note, { beats: 0.35, volume: 0.3 * (1 - y), pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;
      const rx = w * 0.44, ry = h * 0.44;

      const bass = band("subBass");
      const energy = Math.min(2, tension * 0.7 + bass * 1.1 + amp * 0.6 + pump * 0.5);

      // Peg positions around the elliptical border ring.
      const peg = (i) => {
        const ang = (i / PEGS) * Math.PI * 2 - Math.PI / 2;
        return [cx + Math.cos(ang) * rx, cy + Math.sin(ang) * ry];
      };

      // Dark ground.
      ink(6, 5, 14).box(0, 0, w, h);

      // The nails: faint dots around the border, brighter with tension.
      const nailA = 40 + tension * 90 + shimmer * 60;
      for (let i = 0; i < PEGS; i++) {
        const [px, py] = peg(i);
        ink(120, 130, 200, nailA).circle(px, py, 1.5 + shimmer * 1.5, true);
      }

      // The threads: straight lines between pegs. Bass tension brightens + shifts
      // hue; beat shimmer flickers alpha. Their accumulation envelopes a curve.
      for (const t of threads) {
        const [ax, ay] = peg(t.a);
        const [bx, by] = t.tap ? t.tap : peg(t.b);
        const hue = ((t.hue + tension * 40) % 360 + 360) % 360;
        const light = 52 + t.bright * 18 + tension * 12;
        const [r, g, b] = num.hslToRgb(hue, 95, Math.min(90, light));
        const a = (60 + 150 * t.life) * (0.75 + shimmer * 0.25);
        ink(r, g, b, a).line(ax, ay, bx, by);
        // Bright core over-stroke for the freshest threads → crisp line art.
        if (t.life > 0.6) {
          const [r2, g2, b2] = num.hslToRgb(hue, 60, 88);
          ink(r2, g2, b2, 90 * t.life).line(ax, ay, bx, by);
        }
        // Glinting endpoints where the thread meets the nail.
        ink(255, 255, 255, 160 * t.life).circle(ax, ay, 1.5 + t.bright, true);
        ink(255, 255, 255, 130 * t.life).circle(bx, by, 1.5, true);
      }

      // Central tension bloom — the whole web pulls toward here on the bass.
      if (tension > 0.02 || bass > 0.04) {
        const bloom = Math.max(tension, bass * 1.1);
        const bR = Math.min(w, h) * (0.02 + bloom * 0.08);
        ink(150, 90, 210, 30 * bloom).circle(cx, cy, bR, true);
      }

      // Tap glow handled inline via the tap threads above; add a soft flash on pump.
      if (pump > 0.05) {
        const [r, g, b] = num.hslToRgb((pump * 60) % 360, 90, 65);
        ink(r, g, b, 40 * Math.min(1, pump)).circle(cx, cy, Math.min(w, h) * 0.5, false, 2);
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
