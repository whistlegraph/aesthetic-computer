// vex, 26.07.12
// Pixel-sort / datamosh glitch pad — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `vex 0.5`, the tap/XY "pump", audio polling). This file only describes what
// makes vex vex: its glitch-techno score, its industrial voices, and its
// datamosh paint that CORRUPTS a flowing base field IN TIME with the music.
//
// ALLEGORY — the visual IS the score:
//   • BEAT  → a PIXEL-SORT sweep: a band of screen rows gets its columns sorted
//             by brightness, smearing the field into glitch streaks.
//   • NOTE  → an RGB CHANNEL-SHIFT (chromatic aberration): pitch → shift amount
//             + hue; the R/B planes tear sideways for a frame.
//   • BASS  → the glitch INTENSITY: louder sub = more corrupted bands + wider
//             sorts + heavier channel tearing.
// A base flowing color field is drawn to screen.pixels (via a hue LUT), then
// bands of it are sorted / torn / block-corrupted. Cost is bounded by
// ctx.quality: STRIDE the base field + LIMIT sorted rows per frame.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// 16-step glitch-techno grid @ 128 BPM (classic techno). A detuned minor stab
// riff sits over a four-on-the-floor sub kick + chopped glitch clicks.
const STAB = [
  "a2", null, "a2", "c3", null, "e3", null, "c3",
  "a2", null, "g2", null, "b2", "d3", null, "e3",
];
const BASS = ["a1", "a1", "f1", "e1"]; // slow root movement under the kick

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n || "");
  if (!m) return 36;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const PITCHES = STAB.filter(Boolean).map(notePitch);
const PITCH_MIN = Math.min(...PITCHES);
const PITCH_MAX = Math.max(...PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- vex-specific glitch state (engine owns pump/bursts/rhythm) --------------
// Each sort sweep is a horizontal band that briefly sorts its rows' columns by
// brightness (the datamosh smear). Each shift is an RGB tear at a Y band.
let sorts = []; // { y0, h, life } — active pixel-sort bands
let shifts = []; // { y, h, amt, hue, life } — RGB channel tears
let glitchInt = 0; // bass-driven global glitch intensity 0..~1.3
let flow = 0; // base-field flow phase (advances in sim)
let kickFlash = 0; // whole-screen kick flash

// Reusable scratch buffers for the pixel-sort (avoid per-row allocations that
// would thrash GC at 60fps). Sized lazily to the widest possible span.
let scratchLuma = null; // Float32Array of luminance
let scratchCol = null; // Uint32Array of packed RGB

// Precomputed hue LUT for the base field — num.hslToRgb is expensive per pixel,
// so we bake 256 hues once (built lazily in onPaint when num is available).
let HUE_LUT = null;
function buildLUT(num) {
  const lut = new Uint8Array(256 * 3);
  for (let i = 0; i < 256; i++) {
    const [r, g, b] = num.hslToRgb((i / 256) * 360, 85, 45);
    lut[i * 3] = r;
    lut[i * 3 + 1] = g;
    lut[i * 3 + 2] = b;
  }
  return lut;
}

const CONFIG = {
  // NOTE: pads.mjs fires onBeat once per STEP and beatSeconds = 60/bpm is the
  // per-STEP duration — so `bpm` here is STEPS-per-minute, not musical BPM. For
  // driving 16th-note glitch techno we want ~8 steps/sec: bpm 480 → 0.125s/step
  // → a 128-BPM feel with 16th-note subdivision (16 steps = 2s bar).
  bpm: 480,
  steps: STAB.length,
  drawBursts: false, // vex draws its own glitch-block tap feedback
  hooks: {
    onBoot({ sound }) {
      // A short slap for the industrial space — not too washed.
      sound.room?.set?.({ enabled: true, mix: 0.18, feedback: 0.4 });
    },

    // A new UTC beat crossed — fire the glitch-techno score + spawn corruption.
    onBeat({ idx, synth, screen }) {
      const s = ((idx % STAB.length) + STAB.length) % STAB.length;
      const h = screen?.height || 256;

      // FOUR-ON-THE-FLOOR sub kick — every other 16th (the driving pulse).
      const onBeat4 = s % 2 === 0;
      if (onBeat4) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 0.42, decay: 0.42, volume: 0.6 });
        // Hard click transient stacked on the kick — punchy techno attack.
        synth({ type: "noise-white", tone: 2400, beats: 0.03, attack: 0.001, decay: 0.05, volume: 0.28 });
        kickFlash = 1;
        // BASS = glitch intensity: kick corrupts a wide band + sort sweep.
        glitchInt = Math.min(1.3, glitchInt + 0.9);
        const y0 = Math.floor(Math.random() * h * 0.85);
        sorts.push({ y0, h: Math.floor(h * (0.1 + Math.random() * 0.14)), life: 1 });
      }

      // OFF-beat chopped glitch clicks — granular stutter hats between kicks.
      if (!onBeat4) {
        voices.hat(synth, { tone: 9000, beats: 0.05, volume: 0.16, pan: (s / 16) * 2 - 1 });
        if (s % 4 === 3) {
          // A rapid stutter cut — short bright noise burst (the databend click).
          synth({ type: "noise-white", tone: 6000 + Math.random() * 4000, beats: 0.025, attack: 0.001, decay: 0.04, volume: 0.2, pan: Math.random() * 2 - 1 });
        }
      }

      // STAB — detuned minor pluck; each NOTE fires an RGB channel-shift.
      const note = STAB[s];
      if (note) {
        const pn = pitchNorm(note); // 0 low .. 1 high
        const pan = Math.sin((s / STAB.length) * Math.PI * 2) * 0.6;
        // Detuned stab: two slightly-detuned plucks = a gritty industrial chord.
        voices.pluck(synth, note, { beats: 0.32, decay: 0.4, volume: 0.34, pan });
        synth({ tone: note, type: "sawtooth", beats: 0.28, attack: 0.003, decay: 0.35, volume: 0.14, pan: -pan });
        // ALLEGORY: this note tears the RGB planes — pitch→shift amt + hue.
        shifts.push({
          y: Math.floor(Math.random() * h),
          h: Math.floor(h * (0.08 + pn * 0.16)),
          amt: 4 + pn * 22,
          hue: pn,
          life: 1,
        });
      }
    },

    onSim() {
      flow += 0.02;
      glitchInt *= 0.92;
      kickFlash *= 0.82;
      // Slow decays so at least one sort/shift band is almost always live — the
      // corruption should read continuously, pulsing HARDER on each beat/note.
      for (const so of sorts) so.life -= 0.05;
      for (const sh of shifts) sh.life -= 0.06;
      sorts = sorts.filter((so) => so.life > 0);
      shifts = shifts.filter((sh) => sh.life > 0);
      if (sorts.length > 10) sorts = sorts.slice(-10);
      if (shifts.length > 14) shifts = shifts.slice(-14);
    },

    // Tap = a glitch BURST at the tapped band: a big sort sweep + RGB tear +
    // a stab. X → shift amount / hue, Y → pitch (engine already bumped pump).
    onTap({ x, y, ex, ey, synth, screen }) {
      const h = screen?.height || 256;
      const band = Math.floor(h * (0.12 + Math.abs(0.5 - x) * 0.2));
      sorts.push({ y0: Math.max(0, Math.floor(ey - band / 2)), h: band, life: 1.3 });
      shifts.push({ y: Math.floor(ey), h: band, amt: 8 + x * 30, hue: x, life: 1.3 });
      glitchInt = Math.min(1.3, glitchInt + 0.7);
      kickFlash = 1;
      // Stab: X picks the scale degree, Y picks the octave (higher tap=higher).
      const note = ["a", "c", "d", "e", "g"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 3));
      voices.pluck(synth, note, { beats: 0.4, decay: 0.4, volume: 0.5, pan: x * 2 - 1 });
      synth({ type: "noise-white", tone: 3000 + x * 5000, beats: 0.04, attack: 0.001, decay: 0.06, volume: 0.24 });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, step, beatProgress, band, amp, quality } = s;
      const { width: w, height: h } = screen;
      const pix = screen.pixels;
      if (!pix) return;

      if (!HUE_LUT) HUE_LUT = buildLUT(num);
      const lut = HUE_LUT;

      const bass = band("subBass");
      // Bass drives global glitch intensity (allegory: bass = corruption).
      const intensity = Math.min(1.5, glitchInt + bass * 1.4 + pump * 0.4);

      // ADAPTIVE QUALITY — the base field is per-pixel and the pixel-sort is a
      // per-row cost, so we scale BOTH: stride the field write, and cap how many
      // rows we actually sort per frame. quality<0.55→stride 3, <0.8→2, else 1.
      const stride = quality < 0.55 ? 3 : quality < 0.8 ? 2 : 1;
      // Sorted rows are the heavy part; hard-cap the per-frame budget by quality.
      const rowBudget = quality < 0.55 ? 40 : quality < 0.8 ? 90 : 180;

      // —— 1) BASE FLOWING COLOR FIELD (written straight to screen.pixels) ——
      // A traveling plasma of hue bands: cheap sines → a hue index → LUT color.
      // Strided at low quality (block-fill), full-res at quality 1.
      const t = flow;
      const jitter = intensity * 30; // bass warps the field vertically
      for (let y = 0; y < h; y += stride) {
        const fy = y / h;
        // Warp Y by a slow sine + bass jitter (the field itself is unstable).
        const wy = fy + Math.sin(fy * 6.0 + t * 1.3) * 0.04;
        const rowBase = y * w;
        const yEnd = y + stride < h ? y + stride : h;
        for (let x = 0; x < w; x += stride) {
          const fx = x / w;
          // Diagonal traveling hue bands + a second slower layer = flowing field.
          const v =
            Math.sin((fx * 5.0 + wy * 3.0) * Math.PI + t) * 0.5 +
            Math.sin((fx * 2.0 - wy * 4.0) * Math.PI - t * 0.7) * 0.5;
          let hi = ((v * 0.5 + 0.5) * 256 + jitter) | 0;
          hi = ((hi % 256) + 256) & 255;
          const li = hi * 3;
          const cr = lut[li], cg = lut[li + 1], cb = lut[li + 2];
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

      // —— 2) BEAT = PIXEL-SORT SWEEPS (datamosh smear) ——
      // For each active sort band, sort the pixels of each COLUMN by brightness
      // WITHIN the horizontal band → the classic pixel-sort VERTICAL streak
      // (bright pixels pulled into smears perpendicular to the field's bands).
      // Sorting columns (not rows) is what reads as datamosh here, because the
      // field's diagonal bands turn into torn vertical streaks. Column count is
      // capped by `colBudget` (from quality) so this stays cheap.
      let colsLeft = rowBudget * 3; // columns are cheaper than rows here
      const colStride = stride; // skip columns at low quality
      // Lazily size the scratch buffers to full height (reused across columns).
      if (!scratchLuma || scratchLuma.length < h) {
        scratchLuma = new Float32Array(h);
        scratchCol = new Uint32Array(h);
      }
      const lum = scratchLuma, col = scratchCol;
      for (const so of sorts) {
        if (colsLeft <= 0) break;
        const yTop = Math.max(0, so.y0);
        const yBot = Math.min(h, so.y0 + so.h);
        const len = yBot - yTop;
        if (len <= 1) continue;
        for (let x = 0; x < w && colsLeft > 0; x += colStride) {
          colsLeft--;
          // Gather luminance + packed color down the column into REUSED scratch
          // buffers, then insertion-sort ascending by luma and write it back.
          for (let i = 0; i < len; i++) {
            const di = ((yTop + i) * w + x) * 4;
            const r = pix[di], g = pix[di + 1], b = pix[di + 2];
            lum[i] = r * 0.299 + g * 0.587 + b * 0.114;
            col[i] = (r << 16) | (g << 8) | b;
          }
          for (let i = 1; i < len; i++) {
            const kl = lum[i], kc = col[i];
            let j = i - 1;
            while (j >= 0 && lum[j] > kl) {
              lum[j + 1] = lum[j];
              col[j + 1] = col[j];
              j--;
            }
            lum[j + 1] = kl;
            col[j + 1] = kc;
          }
          for (let i = 0; i < len; i++) {
            const c = col[i];
            const di = ((yTop + i) * w + x) * 4;
            pix[di] = (c >> 16) & 255;
            pix[di + 1] = (c >> 8) & 255;
            pix[di + 2] = c & 255;
          }
        }
      }

      // —— 3) NOTE = RGB CHANNEL-SHIFT (chromatic aberration tear) ——
      // For each shift band, tear the R plane left and the B plane right by
      // `amt` px — a per-note chromatic-aberration glitch. Bass widens the tear.
      for (const sh of shifts) {
        const amt = Math.round((sh.amt + intensity * 10) * sh.life);
        if (amt < 1) continue;
        const yTop = Math.max(0, sh.y - (sh.h >> 1));
        const yBot = Math.min(h, sh.y + (sh.h >> 1));
        for (let y = yTop; y < yBot; y += stride) {
          const rowBase = y * w;
          // Shift R plane +amt, B plane -amt (read from strided source, cheap).
          for (let x = 0; x < w; x++) {
            const sr = x + amt < w ? x + amt : w - 1;
            const sb = x - amt >= 0 ? x - amt : 0;
            const di = (rowBase + x) * 4;
            pix[di] = pix[(rowBase + sr) * 4]; // R tears right
            pix[di + 2] = pix[(rowBase + sb) * 4 + 2]; // B tears left
          }
        }
      }

      // —— 4) NATIVE-RES OVERLAYS (cheap vector) ——
      // Kick flash: a bright scanline sweep on the downbeat.
      if (kickFlash > 0.04) {
        const a = Math.round(kickFlash * 60);
        ink(255, 255, 255, a).box(0, 0, w, h);
      }

      // Scan-glitch bars: bright/dark edges frame each active sort band so the
      // BEAT reads even when the sorted streaks blend into the field.
      for (const so of sorts) {
        ink(255, 255, 255, 200 * so.life).box(0, so.y0, w, 2);
        ink(0, 0, 0, 160 * so.life).box(0, so.y0 + so.h, w, 2);
      }

      // Channel-shift hue tint markers on the tear bands (readable note→hue).
      for (const sh of shifts) {
        const [r0, g0, b0] = num.hslToRgb((sh.hue * 300 + 20) % 360, 100, 60);
        ink(r0, g0, b0, 90 * sh.life).box(0, sh.y - 1, w, 2);
      }

      // Tap glitch blocks: corrupted rectangles at bursts (readable tap feedback).
      for (const b of s.bursts) {
        const sz = 8 + b.r * 0.3;
        const [r0, g0, b0] = num.hslToRgb((b.hue % 360 + 360) % 360, 95, 60);
        ink(r0, g0, b0, 180 * b.life).box(b.x - sz / 2, b.y - sz / 2, sz, sz);
        ink(255, 255, 255, 200 * b.life).box(b.x - sz / 2, b.y - sz / 2, sz, 2);
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
