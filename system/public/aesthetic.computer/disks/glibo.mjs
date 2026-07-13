// glibo, 26.07.12
// Wave-interference ripple tank × marimba/soft-mallet — a thin wrapper over
// lib/pads.mjs (the shared pad engine: UTC-clock beat grid, `params[0]` rate
// override e.g. `glibo 0.5`, the tap/XY "pump", audio polling). This file only
// describes what makes glibo glibo: its score, its soft-mallet voices, and its
// per-pixel interference paint.
//
// ALLEGORY: each note DROPS a ripple SOURCE onto a still pond — pitch → x
// position AND wavelength (high notes = tight fast ripples on the right, low =
// long slow swells on the left). Concentric waves spread and INTERFERE, so a
// chord is a beautiful moiré where its sources overlap: you SEE the harmony.
// Bass = one big slow swell source in the middle. The beat is the ripple
// EMISSION. Interference is summed per-pixel across a handful of live sources,
// strided by ctx.quality to hold 60fps.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A gentle marimba figure in F pentatonic (warm, non-clashing → the moiré stays
// pretty). Chord steps stack two sources at once so interference reads.
const MEL = [
  "f3", "a3", "c4", "a3", "d4", "c4", "a3", "f3",
  "g3", "bb3", "d4", "bb3", "e4", "d4", "bb3", "g3",
];
const CHORD_STEPS = new Set([2, 4, 10, 12]); // steps that stack a 2nd source
const BASS = ["f2", "f2", "bb1", "bb1", "c2", "c2", "g1", "g1"]; // half-time swell

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(b?)(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[3], 10) + 1) * 12 + NOTE_SEMI[m[1]] + (m[2] ? -1 : 0);
}
const MEL_PITCHES = MEL.map(notePitch);
const PITCH_MIN = Math.min(...MEL_PITCHES);
const PITCH_MAX = Math.max(...MEL_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- glibo-specific pond state (engine owns pump/bursts/rhythm) --------------
// Each source: { x, y (0..1), k (wavenumber, from wavelength), age (seconds),
//   life (0..1 for color/kill), amp, hue }. Concentric wave = amp * sin(k*r -
//   speed*age) * envelope(r, age) — summed per pixel for interference moiré.
let sources = [];
const SPEED = 4.2; // ripple travel speed (radians of phase per second-ish)
let bassSwell = 0; // central slow swell strength

const MAX_SOURCES = 7; // hard cap on summed sources per pixel (perf)

function dropSource(xn, yn, pn, amp, hue) {
  sources.push({
    x: xn,
    y: yn,
    // wavelength ∝ pitch: high notes → short wavelength → high wavenumber.
    k: 0.05 + pn * 0.16,
    age: 0,
    life: 1,
    amp,
    hue,
  });
  if (sources.length > MAX_SOURCES) sources = sources.slice(-MAX_SOURCES);
}

const CONFIG = {
  bpm: 96, // marimba lilt
  steps: MEL.length,
  drawBursts: false, // glibo draws its own ripple burst rings in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.5 });
    },

    // A new UTC beat crossed — strike the mallet + drop the ripple source.
    onBeat({ idx, synth }) {
      const s = ((idx % MEL.length) + MEL.length) % MEL.length;
      const note = MEL[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const xn = 0.12 + pn * 0.76; // pitch → x position (low left, high right)
      const pan = xn * 2 - 1;

      // Warm soft-mallet marimba: bell body + a short-decay pluck attack.
      voices.bell(synth, note, { beats: 0.7, decay: 0.55, volume: 0.24, pan });
      voices.pluck(synth, note, {
        beats: 0.35,
        decay: 0.3,
        volume: 0.18,
        pan,
      });

      // ALLEGORY: this note drops a ripple source (x ∝ pitch, k ∝ wavelength).
      dropSource(xn, 0.32 + pn * 0.36, pn, 1, 20 + pn * 300);

      // Chord step: stack a 5th-above source right away → interference moiré.
      if (CHORD_STEPS.has(s)) {
        voices.bell(synth, note, { beats: 0.6, volume: 0.14, pan: -pan });
        dropSource(xn * 0.5 + 0.25, 0.5, Math.min(1, pn + 0.25), 0.8, 60 + pn * 260);
      }

      // Sub swell root on the half-beat — a big slow source you feel + SEE.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.8, decay: 0.5, volume: 0.42 });
        dropSource(0.5, 0.5, 0.0, 1.3, 200); // big long-wavelength swell
        bassSwell = 1;
      }

      voices.hat(synth, { tone: 6000, beats: 0.1, volume: 0.1 });
    },

    onSim() {
      bassSwell *= 0.92;
      const dt = 1 / 60;
      for (const src of sources) {
        src.age += dt;
        src.life -= 0.01; // ~1.6s visible life
      }
      sources = sources.filter((src) => src.life > 0);
    },

    // Tap = drop a ripple source + strike a mallet at the tap point.
    onTap({ x, y, synth }) {
      const pn = 1 - y; // higher tap = higher pitch
      const note = ["f", "g", "a", "c", "d"][Math.floor(x * 5) % 5] + (2 + Math.floor(pn * 3));
      voices.bell(synth, note, { beats: 0.7, decay: 0.5, volume: 0.4, pan: x * 2 - 1 });
      voices.pluck(synth, note, { beats: 0.4, decay: 0.3, volume: 0.3, pan: x * 2 - 1 });
      dropSource(x, y, pn, 1.5, x * 360); // strong bright source at the tap
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      const bass = band("subBass");
      const energy = Math.min(2, bassSwell * 0.5 + bass * 1.0 + amp * 0.5 + pump * 0.5);

      // —— DIRECT PER-PIXEL INTERFERENCE (native screen.pixels, no draw calls) ——
      // Cost is a handful of sin/sqrt per pixel (strided), written straight into
      // the RGBA byte buffer — the same fast path molten uses. The summed field
      // is a PURE FUNCTION of the live sources: each is a concentric wave
      // sin(k·r − speed·age) radially damped; overlaps INTERFERE → moiré = chord.
      const pix = screen.pixels;
      const stride = quality < 0.6 ? 3 : quality < 0.85 ? 2 : 1;
      const src = sources.slice(-MAX_SOURCES);
      const n = src.length;

      // Precompute source pixel centers + rolling phase (speed·age) + amp.
      const sx = new Float32Array(n);
      const sy = new Float32Array(n);
      const sk = new Float32Array(n);
      const sph = new Float32Array(n);
      const sa = new Float32Array(n);
      const shue = new Float32Array(n);
      for (let i = 0; i < n; i++) {
        sx[i] = src[i].x * w;
        sy[i] = src[i].y * h;
        sk[i] = src[i].k;
        sph[i] = SPEED * src[i].age;
        sa[i] = src[i].amp * src[i].life;
        shue[i] = ((src[i].hue % 360) + 360) % 360;
      }
      const globalAmp = 1 + energy * 0.3;

      for (let y = 0; y < h; y += stride) {
        const rowBase = y * w;
        for (let x = 0; x < w; x += stride) {
          // Deep-water base color for every cell (still pond).
          let cr = 4, cg = 10, cb = 26;
          if (n > 0) {
            let sum = 0;
            let dom = 0;
            let domW = -1;
            for (let i = 0; i < n; i++) {
              const dx = x - sx[i];
              const dy = y - sy[i];
              const r = Math.sqrt(dx * dx + dy * dy) + 0.001;
              const damp = sa[i] / (1 + r * 0.006);
              sum += Math.sin(sk[i] * r - sph[i]) * damp;
              if (damp > domW) {
                domW = damp;
                dom = i;
              }
            }
            let lum = 0.5 + sum * globalAmp * 0.5;
            if (!(lum >= 0)) lum = 0; // NaN guard
            else if (lum > 1) lum = 1;
            if (lum >= 0.06) {
              const [r0, g0, b0] = num.hslToRgb(shue[dom], 55 + lum * 35, 18 + lum * 62);
              cr = r0; cg = g0; cb = b0;
            }
          }
          // Fill the stride×stride block from this computed pixel.
          const yEnd = y + stride < h ? y + stride : h;
          const xEnd = x + stride < w ? x + stride : w;
          for (let by = y; by < yEnd; by++) {
            const bBase = by * w;
            for (let bx = x; bx < xEnd; bx++) {
              const di = (bBase + bx) * 4;
              pix[di] = cr;
              pix[di + 1] = cg;
              pix[di + 2] = cb;
              pix[di + 3] = 255;
            }
          }
        }
      }

      // Ripple emission ring at each fresh source (the "drop" splash).
      for (const so of src) {
        if (so.age > 0.9) continue;
        const rr = so.age * SPEED * (10 / Math.max(0.05, so.k));
        const hue = ((so.hue % 360) + 360) % 360;
        const [rc, gc, bc] = num.hslToRgb(hue, 90, 62);
        const a = Math.round(160 * (1 - so.age / 0.9) * so.life);
        ink(rc, gc, bc, a).circle(so.x * w, so.y * h, rr, false, 2);
        ink(255, 255, 255, a).circle(so.x * w, so.y * h, 3 + so.amp * 3, true);
      }

      // Central bass swell glow.
      if (bassSwell > 0.02 || bass > 0.05) {
        const bloom = Math.max(bassSwell, bass * 1.1);
        const bR = Math.min(w, h) * (0.06 + bloom * 0.22);
        for (let i = 3; i > 0; i--)
          ink(60, 120, 220, 14 * bloom).circle(w / 2, h / 2, bR * (i / 3), true);
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
