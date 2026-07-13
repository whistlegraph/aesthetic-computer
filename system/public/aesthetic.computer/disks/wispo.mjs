// wispo, 26.07.12
// Lissajous / harmonograph pad — a thin wrapper over lib/pads.mjs (the shared
// pad engine: UTC-clock beat grid, `params[0]` rate override e.g. `wispo 0.5`,
// the tap/XY "pump", audio polling). This file only says what makes wispo wispo:
// its FM-bell score and its phosphor-curve paint.
//
// ALLEGORY: every audible note DRAWS a glowing Lissajous figure whose x/y
// frequency-ratio IS the note's interval over the root — so the curve's SHAPE
// literally is the harmony you hear. Bass = a slow global rotation + scale of
// the whole field; the beat = a fresh curve tracing on; neon phosphor trails
// (a translucent veil) let curves persist and fade. The visual IS the score.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A minor pentatonic climb + fall; each step's interval-over-root sets the
// Lissajous frequency ratio, so consonant intervals draw simple closed loops
// and wider intervals draw denser knots.
const SCORE = [
  "a2", "c3", "e3", "g3", "a3", "g3", "e3", "d3",
  "a2", "c3", "e3", "a3", "b3", "a3", "g3", "e3",
];
const BASS = ["a1", "a1", "f1", "f1", "c2", "c2", "e1", "e1"]; // half-time root

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const ROOT = notePitch("a2");
const SCORE_PITCHES = SCORE.map(notePitch);
const PITCH_MIN = Math.min(...SCORE_PITCHES);
const PITCH_MAX = Math.max(...SCORE_PITCHES);
const pitchNorm = (p) => (p - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// interval-over-root (semitones) → small integer freq ratio a:b for Lissajous.
// Map the interval to the nearest simple just-ratio so consonances draw simple
// curves. Fall back to (1 + semis mod 5) : 3 for anything exotic.
const JUST = {
  0: [1, 1], 3: [6, 5], 4: [5, 4], 5: [4, 3], 7: [3, 2],
  9: [5, 3], 10: [9, 5], 12: [2, 1], 14: [9, 4], 15: [12, 5],
  16: [5, 2], 17: [8, 3], 19: [3, 1],
};
function lissRatio(pitch) {
  const semis = ((pitch - ROOT) % 24 + 24) % 24;
  const r = JUST[semis];
  if (r) return r;
  return [1 + (semis % 5), 3];
}

// --- wispo-specific visual state (engine owns pump/bursts/rhythm) -----------
// Each curve: { a, b, delta, hue, life, big, bright, spin, tap } — one per note.
let curves = [];
let fieldRot = 0; // slow global rotation, driven by bass
let fieldScale = 1; // slow global scale breath, driven by bass
let bassKick = 0; // per-bass-note flash

const CONFIG = {
  bpm: 96,
  steps: SCORE.length,
  drawBursts: false, // wispo draws its own tap glow in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.34, feedback: 0.62 });
    },

    // A new UTC beat crossed — fire the FM-bell + trace its Lissajous curve.
    onBeat({ idx, synth }) {
      const s = ((idx % SCORE.length) + SCORE.length) % SCORE.length;
      const note = SCORE[s];
      const pitch = notePitch(note);
      const pn = pitchNorm(pitch); // 0 low .. 1 high
      const pan = Math.sin((s / SCORE.length) * Math.PI * 2) * 0.7;
      const [a, b] = lissRatio(pitch);

      // FM-ish bell: layered bell (sine+triangle) + a detuned pair of sines a
      // few cents apart for shimmer beating + a soft harp attack transient.
      voices.bell(synth, note, { beats: 1.4, volume: 0.32, pan });
      synth({ tone: note, type: "sine", beats: 1.1, attack: 0.004, decay: 0.85, volume: 0.14, pan: pan * 0.6 });
      synth({ tone: note, type: "triangle", beats: 0.7, attack: 0.002, decay: 0.6, volume: 0.1, pan: -pan * 0.6 });
      voices.pluck(synth, note, { beats: 0.5, decay: 0.4, volume: 0.16, pan });

      // ALLEGORY: this note traces its Lissajous figure — ratio a:b = interval,
      // hue ∝ pitch, size ∝ pitch (low = wide field, high = tight knot).
      curves.push({
        a, b,
        delta: (s / SCORE.length) * Math.PI, // phase → orientation
        hue: 180 + pn * 200,
        life: 1,
        big: 1 - pn * 0.55,
        bright: 0.4 + pn * 0.6,
        spin: (s % 2 === 0 ? 1 : -1) * (0.15 + pn * 0.2),
        tap: 0,
      });

      // Sub-bass root on the half-beat — the downbeat you feel AND see (it
      // rotates + scales the whole field).
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.8, decay: 0.55, volume: 0.5 });
        bassKick = 1;
      }

      voices.hat(synth, { tone: 7200, beats: 0.1, volume: 0.13 });
    },

    onSim() {
      bassKick *= 0.9;
      fieldRot += 0.004 + bassKick * 0.02; // bass drives the slow global spin
      fieldScale = 0.94 + 0.1 * Math.sin(fieldRot * 0.7) + bassKick * 0.08;
      for (const c of curves) {
        c.life -= 0.011;
        c.delta += c.spin * 0.02;
        c.tap *= 0.92;
      }
      curves = curves.filter((c) => c.life > 0);
      if (curves.length > 20) curves = curves.slice(-20);
    },

    // Tap = a bespoke bright Lissajous knot at the tap point + an FM-bell boost
    // (engine already bumped pump + pushed the burst; X→ratio/pitch, Y→size).
    onTap({ x, y, ex, ey, synth, screen }) {
      const a = 2 + Math.floor(x * 5); // X picks the x-frequency
      const b = 2 + Math.floor((1 - y) * 4); // Y picks the y-frequency
      curves.push({
        a, b,
        delta: Math.atan2(ey - screen.height / 2, ex - screen.width / 2),
        hue: x * 360,
        life: 1.3,
        big: 0.4 + (1 - y) * 0.5,
        bright: 1,
        spin: (x < 0.5 ? -1 : 1) * 0.35,
        tap: 1, // marks it as a tap knot → drawn centered on the tap point
        px: ex,
        py: ey,
      });
      const note = ["a", "c", "d", "e", "g"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 4));
      voices.bell(synth, note, { beats: 0.9, volume: 0.4, pan: x * 2 - 1 });
      synth({ tone: note, type: "sine", beats: 0.7, attack: 0.003, decay: 0.7, volume: 0.2 * (1 - y), pan: x * 2 - 1 });
      voices.pluck(synth, note, { beats: 0.4, volume: 0.28, pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, beatProgress, bursts, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      const bass = band("subBass");
      const energy = Math.min(2, bassKick * 0.7 + bass * 1.2 + amp * 0.6 + pump * 0.5);

      // Phosphor veil: translucent dark box → prior curves persist and fade
      // (neon trails). Lighter veil when high-energy so trails linger a touch.
      const veil = 40 - Math.min(22, energy * 16 + pump * 8);
      ink(4, 3, 14, veil).box(0, 0, w, h);

      // BASS = a faint central bloom you can see swell with the sub.
      if (bassKick > 0.02 || bass > 0.05) {
        const bloom = Math.max(bassKick, bass * 1.2);
        const bR = Math.min(w, h) * (0.08 + bloom * 0.22);
        for (let i = 3; i > 0; i--)
          ink(60, 30, 150, 14 * bloom).circle(cx, cy, bR * (i / 3), true);
      }

      // ADAPTIVE QUALITY: scale BOTH the number of curves drawn AND the points
      // per curve by ctx.quality so we hold 60fps. At quality=1 → full detail.
      const q = quality;
      const maxCurves = Math.max(3, Math.round(14 * q));
      const basePts = Math.max(28, Math.round(180 * q));
      const R = Math.min(w, h) * 0.42 * fieldScale;

      const drawn = curves.slice(-maxCurves);
      for (const c of drawn) {
        // tap knots sit on the tap point; score curves fill the rotating field.
        const ox = c.tap ? c.px : cx;
        const oy = c.tap ? c.py : cy;
        const scale = (c.tap ? Math.min(w, h) * 0.16 : R) * (0.55 + c.big * 0.6) *
          (0.9 + energy * 0.18);
        // point count scales with life (fresh curves get more detail) + quality.
        const pts = Math.max(20, Math.round(basePts * (0.5 + c.life * 0.5)));
        const [r0, g0, b0] = num.hslToRgb(
          ((c.hue % 360) + 360) % 360,
          100,
          52 + c.bright * 20,
        );
        const alpha = 60 + 150 * c.life + c.tap * 40;
        const rot = c.tap ? c.delta : fieldRot + c.delta;
        const cr = Math.cos(rot), sr = Math.sin(rot);

        let px = null, py = null;
        for (let i = 0; i <= pts; i++) {
          const t = (i / pts) * Math.PI * 2;
          // Lissajous: x = sin(a·t + δ), y = sin(b·t) — a:b is the interval.
          let lx = Math.sin(c.a * t + c.delta) * scale;
          let ly = Math.sin(c.b * t) * scale;
          // rotate the whole figure (field spin for score, orient for taps).
          const rx = lx * cr - ly * sr;
          const ry = lx * sr + ly * cr;
          const x = ox + rx, y = oy + ry;
          if (px !== null) ink(r0, g0, b0, alpha).line(px, py, x, y);
          px = x; py = y;
        }
        // bright leading dot at the trace head (a glowing phosphor tip).
        const th = beatProgress * Math.PI * 2 * c.a + c.delta;
        const hx = ox + (Math.sin(c.a * th + c.delta) * scale) * cr -
          (Math.sin(c.b * th) * scale) * sr;
        const hy = oy + (Math.sin(c.a * th + c.delta) * scale) * sr +
          (Math.sin(c.b * th) * scale) * cr;
        ink(255, 255, 255, 200 * c.life).circle(hx, hy, 2 + c.bright * 3 + c.tap * 3, true);
      }

      // TAP GLOW (engine-tracked bursts): soft expanding halo at each tap.
      for (const b of bursts) {
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 95, 62);
        ink(r0, g0, b0, 170 * b.life).circle(b.x, b.y, b.r, false, 2 + b.life * 3);
        ink(255, 255, 255, 120 * b.life).circle(b.x, b.y, 3 + b.life * 10, true);
      }

      if (energy > 1.1 || pump > 1.3) blur?.(1);

      // Center heartbeat — the pulse of the field.
      const heartR = 3 + bassKick * 8 + bass * 10 + pump * 6;
      ink(120, 90, 255, 80 + bassKick * 90).circle(cx, cy, heartR * 1.7, true);
      ink(255, 255, 255, 150 + bassKick * 80).circle(cx, cy, heartR, true);
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
