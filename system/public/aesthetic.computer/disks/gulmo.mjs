// gulmo, 26.07.12
// Terrain-heightfield scanlines × deep sub techno — a thin wrapper over
// lib/pads.mjs (the shared pad engine: UTC-clock beat grid, `params[0]` rate
// override e.g. `gulmo 0.5`, the tap/XY "pump", audio polling). This file only
// describes what makes gulmo gulmo: its dark techno score, its richened deep
// voices, and its neon-terrain paint.
//
// ALLEGORY — the LIVE AUDIO carves the mountains. Rows of ridge scanlines are
// drawn in perspective (nearer rows bigger); each row samples the frequency
// bands + waveform amplitude to set its ridge heights, so you literally SEE the
// sound as a scrolling 3D terrain. BASS = the tallest ridges + a valley glow
// pooled in the low ground; the four-on-floor BEAT = a fresh ridge row emitted
// at the horizon that scrolls toward you. Read the terrain and you read the
// track. Retro-wireframe neon (Tron/synthwave) look; vector lines only, and the
// row + point counts scale by ctx.quality to hold 60fps.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// 16 steps @ 130 BPM. Four-on-floor sub kick; a filtered minor stab on the
// backbeat/syncopations; offbeat hats; a dark held pad chord underneath.
const STEPS = 16;
const KICK = "c1"; // deep sub thump root — four on the floor
// Filtered stab notes across the bar (C minor: c eb g bb), 0 = rest.
const STAB = [0, 0, "g2", 0, 0, "eb2", 0, "c3", 0, 0, "bb2", 0, "g2", 0, 0, "eb2"];
const PAD_CHORD = ["c2", "eb2", "g2", "bb2"]; // dark Cm7 bed

// --- gulmo-specific visual state (engine owns pump/bursts/rhythm) -----------
// The terrain is a CONTINUOUS scrolling field of ridge rows: rows always fill
// the screen (a new one spawns at the horizon whenever the field scrolls up),
// so you never lose the mountain range. Each row carries a scroll offset `off`
// (0 = far horizon … 1 = nearest, then it recycles). Beats INJECT energy +
// color pulses that ride forward with the terrain, and emit a fresh bright
// "beat ridge" so the four-on-floor is visible marching toward you.
let rows = []; // { seed, energy, hue, peakX, peakAmt } — index i = distance
let scroll = 0; // continuous forward scroll (fractional rows)
let seedHead = 0; // running seed counter for freshly recycled rows
let bassGlow = 0; // valley glow, kicked by each sub kick
let beatFlash = 0; // horizon flash on the four-on-floor
let scanPhase = 0; // slow scroll phase for point sampling
let beatPulse = 0; // how many rows back the last beat's bright ridge is

const BASE_ROWS = 30; // ridge rows at full quality
const BASE_PTS = 44; // points per ridge line at full quality

// Deterministic value-noise-ish ridge shape so a row is stable as it scrolls
// but each row differs. Cheap hash → smooth-ish 1D field.
function ridgeAt(seed, t) {
  // Layered sines with seed-shifted phases — a fake fractal ridge.
  const a = Math.sin(t * 6.283 + seed * 1.7) * 0.5;
  const b = Math.sin(t * 12.566 + seed * 3.1) * 0.28;
  const c = Math.sin(t * 25.13 + seed * 5.9) * 0.14;
  return Math.abs(a + b + c); // ridged (mountains rise from valley floor)
}

const CONFIG = {
  bpm: 130,
  steps: STEPS,
  drawBursts: false, // gulmo draws its own tap ridge-pulse in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.24, feedback: 0.5 });
      // Pre-fill the terrain field so mountains are present from frame one.
      rows = [];
      scroll = 0;
      seedHead = 0;
      beatPulse = 999;
      for (let i = 0; i < 44; i++)
        rows.push({ seed: (seedHead++ % 997) * 0.618, energy: 0.15, hue: 200, peakX: -1, peakAmt: 0 });
    },

    // A new UTC beat crossed — play the score + light up the terrain.
    onBeat({ idx, synth }) {
      const s = ((idx % STEPS) + STEPS) % STEPS;

      // ALLEGORY: the beat lights the horizon row bright and marks it as THE beat
      // ridge, which then scrolls toward you row-by-row (beatPulse counts up as
      // the terrain advances). The whole field is carved by the live bands each
      // frame in onPaint, so you read the whole track as a moving mountain range.
      if (rows.length) {
        const head = rows[0];
        head.energy = 1;
        head.hue = 190 + (s / STEPS) * 120; // cyan→magenta band sweep
      }
      beatPulse = 0;
      beatFlash = 1;

      // Four-on-floor deep SUB kick — short thump you feel + see as valley glow.
      if (s % 4 === 0) {
        voices.sub(synth, KICK, { beats: 0.5, attack: 0.004, decay: 0.32, volume: 0.62 });
        bassGlow = 1;
      }

      // Filtered minor STAB on the syncopations — the melodic ridge lift.
      const stab = STAB[s];
      if (stab) {
        const pan = Math.sin((s / STEPS) * Math.PI * 2) * 0.5;
        voices.pluck(synth, stab, { beats: 0.5, decay: 0.4, volume: 0.34, pan });
        voices.bell(synth, stab, { beats: 0.4, volume: 0.12, pan: -pan });
      }

      // Offbeat HATS — the eighth-note shimmer between kicks.
      if (s % 2 === 1) voices.hat(synth, { tone: 9000, beats: 0.08, volume: 0.13 });

      // Dark PAD bed retriggered at the top of each bar (short, so it breathes).
      if (s === 0) {
        for (const t of PAD_CHORD)
          voices.flute(synth, t, { beats: 3.2, attack: 0.4, decay: 1.4, volume: 0.07 });
      }
    },

    onSim() {
      bassGlow *= 0.9;
      beatFlash *= 0.85;
      scanPhase += 0.006;
      // Continuous scroll: the whole field advances toward the viewer. When it
      // has moved a full row, RECYCLE — pop the nearest row (index 0 = closest)
      // and unshift a fresh one at the horizon, so the range never empties.
      scroll += 0.12; // rows-per-sim scroll speed (dense, smooth march)
      while (scroll >= 1) {
        scroll -= 1;
        rows.pop(); // drop the nearest (it scrolled past the viewer)
        rows.unshift({
          seed: (seedHead++ % 997) * 0.618, // decorrelated fresh horizon ridge
          energy: 0.15,
          hue: 200,
          peakX: -1,
          peakAmt: 0,
        });
        beatPulse++; // the last beat ridge is now one row closer
      }
      for (const r of rows) {
        r.energy *= 0.99;
        r.peakAmt *= 0.95;
      }
    },

    // Tap = raise a peak / send a ridge pulse from the tap x + a sub boom.
    onTap({ x, y, synth }) {
      // Push a peak into a swath of rows at the tapped x — a mountain rises and
      // then rides the terrain toward the viewer (a scrolling ridge pulse).
      const n = rows.length;
      for (let i = 0; i < n; i++) {
        const near = i / n; // 0 = nearest .. 1 = horizon
        if (near > 0.1 && near < 0.75) {
          const r = rows[i];
          r.peakX = x;
          r.peakAmt = Math.max(r.peakAmt, 0.7 + (1 - y) * 0.7);
          r.energy = Math.min(1, r.energy + 0.5);
        }
      }
      bassGlow = 1;
      beatFlash = Math.max(beatFlash, 0.7);
      // Sub boom pitched by X, plus a stab for the strike.
      const kick = ["c1", "d1", "eb1", "g1", "c2"][Math.floor(x * 5)];
      voices.sub(synth, kick, { beats: 0.6, attack: 0.004, decay: 0.4, volume: 0.7 });
      const stab = ["c3", "eb3", "g3", "bb3", "c4"][Math.floor(x * 5)];
      voices.pluck(synth, stab, { beats: 0.4, volume: 0.4, pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, beatProgress, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      const sub = band("subBass");
      const low = band("lowMid");
      const mid = band("mid");
      const air = band("air");
      const energy = Math.min(2, sub * 1.3 + amp * 0.7 + pump * 0.5 + beatFlash * 0.4);

      // --- Neon night backdrop (dark, low-alpha veil = subtle trails) --------
      ink(4, 2, 14, 255).box(0, 0, w, h);

      // Horizon line sits high; terrain fills below it toward the viewer.
      const horizonY = h * 0.34;
      // Valley-glow gradient pooled at the low ground (BASS pools in the valley).
      const glow = Math.max(bassGlow, sub * 1.2);
      if (glow > 0.02) {
        const bands = 5;
        for (let i = 0; i < bands; i++) {
          const yy = h - (i / bands) * (h - horizonY);
          const a = Math.round(30 * glow * (1 - i / bands));
          if (a > 1) ink(120, 20, 90, a).box(0, yy - 12, w, 24);
        }
      }
      // Horizon beat-flash bar.
      const hf = Math.round(60 + beatFlash * 140);
      ink(80, 220, 255, hf).box(0, horizonY - 1, w, 2);
      // Sun/afterglow disc on the horizon (synthwave motif), pulsing with bass.
      const sunR = Math.min(w, h) * (0.11 + glow * 0.05);
      ink(255, 60, 140, 40 + Math.round(glow * 60)).circle(w / 2, horizonY, sunR, true);

      // --- Terrain rows: nearer = bigger, brighter, wider --------------------
      // Scale row + point counts by quality to hold 60fps (vector lines only).
      // rows[] index i: 0 = nearest (bottom), higher = toward horizon. The
      // fractional `scroll` slides the whole grid smoothly between recycles.
      const maxRows = Math.max(10, Math.round(BASE_ROWS * quality));
      const pts = Math.max(16, Math.round(BASE_PTS * quality));
      const rowCount = Math.min(maxRows, rows.length);

      // Draw HORIZON→NEAR (high index first) so nearer ridges paint over far.
      for (let i = rowCount - 1; i >= 0; i--) {
        const r = rows[i];
        // Distance param d: 0 = at viewer, 1 = at horizon. Add fractional scroll.
        const d = Math.min(1, (i - scroll) / maxRows);
        if (d < 0) continue;
        const near = 1 - d; // 1 near .. 0 far
        // Perspective: horizon rows bunch up top, near rows spread to the bottom.
        const persp = Math.pow(near, 1.7);
        const baseY = horizonY + persp * (h - horizonY);
        // Ridge height budget grows toward the viewer (near mountains are tall).
        const hgt = (h - horizonY) * (0.12 + persp * 0.75);

        // Audio carves this row: its own energy + the LIVE bands, so the whole
        // range breathes with the current sound (bands = mountain heights).
        const carve = 0.45 + r.energy * 0.6 + low * 0.7 + mid * 0.45 + sub * 0.6;

        // Is this the beat ridge? (the row the last downbeat lit, now scrolled
        // `beatPulse` rows closer) — draw it extra bright/thick.
        const isBeat = i === rowCount - 1 - beatPulse && beatPulse < rowCount;

        // Neon color: hue from row, lightness from nearness + energy.
        const light = 48 + persp * 30 + r.energy * 26 + energy * 6;
        const [cr, cg, cb] = num.hslToRgb(
          (((r.hue + mid * 40) % 360) + 360) % 360,
          100,
          Math.min(90, light),
        );
        const alpha = Math.round(70 + persp * 165 + r.energy * 60);
        const thick = isBeat ? 2 : 1;

        let px = null, py = null;
        for (let j = 0; j <= pts; j++) {
          const t = j / pts;
          const x = t * w;
          // Ridge silhouette from the fake-fractal field. Sample by seed so each
          // row's mountains are stable as the grid scrolls under it.
          let ridge = ridgeAt(r.seed, t) * hgt * carve;
          // Tapped peak: a gaussian mountain rises at peakX and rides forward.
          if (r.peakAmt > 0.01) {
            const dx = t - r.peakX;
            ridge += Math.exp(-(dx * dx) * 50) * hgt * r.peakAmt * 1.5;
          }
          // Waveform ripple riding the crest (air band = fine sparkle).
          ridge += Math.sin(t * 40 + scanPhase * 6 + i) * hgt * air * 0.14;
          const y = baseY - ridge;
          if (px !== null) {
            ink(cr, cg, cb, alpha).line(px, py, x, y, thick);
            if (isBeat) ink(255, 255, 255, 120).line(px, py, x, y, 1);
          }
          px = x; py = y;
        }
      }

      // --- Tap pump flash on the near ground ---------------------------------
      if (pump > 0.4) {
        const a = Math.round(Math.min(120, pump * 50));
        ink(255, 120, 220, a).box(0, h - 40, w, 40);
      }

      // Bass bloom on strong low frequency → soft blur for a glow bleed.
      if (glow > 0.6 || beatFlash > 0.6) blur?.(1);

      // Foreground scan sparkle (air) — tiny neon dust on the near terrain.
      if (air > 0.05) {
        const n = Math.round(air * 20 * quality);
        for (let i = 0; i < n; i++) {
          const sx = (Math.sin(i * 12.9 + scanPhase * 10) * 0.5 + 0.5) * w;
          const sy = horizonY + (Math.sin(i * 7.3 + scanPhase * 3) * 0.5 + 0.5) * (h - horizonY);
          ink(180, 255, 255, 120).circle(sx, sy, 1, true);
        }
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
