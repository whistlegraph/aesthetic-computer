// velsor, 26.07.13
// Datamosh pad scored by just-intonation dissonance — a thin wrapper over
// lib/pads.mjs (the shared pad engine: UTC-clock beat grid, `params[0]` rate
// override e.g. `velsor 0.5`, the tap/XY "pump", audio polling). velsor
// inherits its EYE from vex (a flowing hue field that gets pixel-sorted and
// RGB-torn) and its EAR from drolo (bell/pluck voices ringing a D-minor
// pentatonic score, each note's interval-over-root read as a just-intonation
// ratio). This file only says what makes velsor velsor: the ratio IS the
// glitch amount.
//
// ALLEGORY — where drolo made an interval's consonance visible as pendulums
// snapping back into one line, velsor makes it visible as a WOUND IN THE
// FIELD. A unison or fifth (simple ratio, e.g. 1:1, 3:2) barely disturbs the
// flowing color field — a thin, quiet channel-shift. A wide dissonant leap
// (a dense ratio like 9:4) rips the field open — a wide RGB tear and a heavy
// pixel-sort sweep. The bass root still grounds the row (every other step,
// like drolo's half-time bass) and the sort bands are gated to those hits, so
// the datamosh reads as a HEARTBEAT with dissonance riding on top of it. When
// a run of recent notes has been consonant, gold rays burn through the
// corruption — drolo's "realignment shimmer" reborn as light breaking through
// the glitch instead of a row of pendulums locking phase.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score (drolo's D minor pentatonic climb + fall) -------------------
const SCORE = [
  "d3", "f3", "g3", "a3", "c4", "a3", "g3", "f3",
  "d3", "f3", "a3", "c4", "d4", "c4", "a3", "g3",
];
const BASS = ["d1", "d1", "a1", "a1", "f1", "f1", "c2", "c2"]; // half-time root

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n || "");
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const ROOT = notePitch("d3");

// interval-over-root (semitones) → small integer swing-ratio a:b, same table
// drolo used to tune pendulum speed. Here a*b becomes the DISSONANCE that
// drives how hard the field tears — simple ratios (1:1, 3:2) barely disturb
// it, dense ratios (9:4, 12:5) rip it open.
const JUST = {
  0: [1, 1], 3: [6, 5], 4: [5, 4], 5: [4, 3], 7: [3, 2],
  9: [5, 3], 10: [9, 5], 12: [2, 1], 14: [9, 4], 15: [12, 5],
  16: [5, 2], 17: [8, 3], 19: [3, 1],
};
function ratioFor(pitch) {
  const semis = (((pitch - ROOT) % 24) + 24) % 24;
  return JUST[semis] || [1 + (semis % 5), 3];
}
function dissonanceFor(pitch) {
  const [a, b] = ratioFor(pitch);
  return Math.min(1, (a * b - 1) / 30); // 1:1→0 .. 9:4(36)/12:5(60)→~1
}

// --- velsor-specific glitch state (engine owns pump/bursts/rhythm) ------
// Sort bands ride the bass (grounding heartbeat); shift bands ride every
// melody note (the dissonance tear). `hue` on each is its dissonance 0..1,
// reused to color the overlay (gold=consonant → violet=dissonant).
let sorts = [];  // { y0, h, life, hue }
let shifts = []; // { y, h, amt, hue, life }
let glitchInt = 0;
let flow = 0;
let kickFlash = 0;

// Reusable scratch buffers for the column pixel-sort (no per-column alloc).
let scratchLuma = null;
let scratchCol = null;

// Precomputed hue LUT for the base field (built lazily once `num` exists).
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
  bpm: 104, // drolo's pace — the ear sets the tempo, the eye rides it
  steps: SCORE.length,
  drawBursts: false, // velsor draws its own glitch-block tap feedback
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.5 });
    },

    // A new UTC beat crossed — ring the score's next pendulum-note, tear the
    // field by its dissonance, and (on the half-time root) ground it with a
    // sort sweep.
    onBeat({ idx, synth, screen }) {
      const step = ((idx % SCORE.length) + SCORE.length) % SCORE.length;
      const h = screen?.height || 256;
      const note = SCORE[step];
      const pan = Math.sin((step / SCORE.length) * Math.PI * 2) * 0.7;

      voices.bell(synth, note, { beats: 1.2, volume: 0.3, pan });
      voices.pluck(synth, note, { beats: 0.5, decay: 0.4, volume: 0.22, pan });
      synth({ tone: note, type: "sine", beats: 0.9, attack: 0.004, decay: 0.75, volume: 0.12, pan: pan * 0.5 });
      voices.hat(synth, { tone: 6800, beats: 0.08, volume: 0.1, pan: pan * 0.5 });

      // ALLEGORY: this note's just-intonation ratio tears the RGB planes —
      // consonant = thin quiet tear, dissonant = wide loud rip.
      const dis = dissonanceFor(notePitch(note));
      shifts.push({
        y: Math.floor(Math.random() * h),
        h: Math.floor(h * (0.08 + dis * 0.22)),
        amt: 3 + dis * 26,
        hue: dis,
        life: 1,
      });

      if (step % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.6, decay: 0.5, volume: 0.45 });
        glitchInt = Math.min(1.2, glitchInt + 0.8);
        kickFlash = 1;
        const y0 = Math.floor(Math.random() * h * 0.85);
        sorts.push({ y0, h: Math.floor(h * (0.12 + dis * 0.16)), life: 1, hue: dis });
      }
    },

    onSim() {
      flow += 0.015;
      glitchInt *= 0.94;
      kickFlash *= 0.85;
      for (const so of sorts) so.life -= 0.035;
      for (const sh of shifts) sh.life -= 0.045;
      sorts = sorts.filter((so) => so.life > 0);
      shifts = shifts.filter((sh) => sh.life > 0);
      if (sorts.length > 6) sorts = sorts.slice(-6);
      if (shifts.length > 8) shifts = shifts.slice(-8);
    },

    // Tap = pick the nearest scale step (like drolo's flick) and tear the
    // field there by ITS dissonance — engine already bumped pump.
    onTap({ x, y, ey, synth, screen }) {
      const h = screen?.height || 256;
      const idx = Math.max(0, Math.min(SCORE.length - 1, Math.floor(x * SCORE.length)));
      const note = SCORE[idx];
      const pan = x * 2 - 1;
      voices.bell(synth, note, { beats: 0.9, volume: 0.42, pan });
      voices.pluck(synth, note, { beats: 0.5, volume: 0.3, pan });
      synth({ tone: note, type: "triangle", beats: 0.6, attack: 0.003, decay: 0.6, volume: 0.18 * (1 - y), pan });

      const dis = dissonanceFor(notePitch(note));
      const band = Math.floor(h * (0.1 + dis * 0.2));
      sorts.push({ y0: Math.max(0, Math.floor(ey - band / 2)), h: band, life: 1.3, hue: dis });
      shifts.push({ y: Math.floor(ey), h: band, amt: 6 + dis * 30, hue: dis, life: 1.3 });
      glitchInt = Math.min(1.2, glitchInt + 0.6);
      kickFlash = 1;
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, band, quality } = s;
      const { width: w, height: h } = screen;
      const pix = screen.pixels;
      if (!pix) return;

      if (!HUE_LUT) HUE_LUT = buildLUT(num);
      const lut = HUE_LUT;

      const bass = band("subBass");
      const intensity = Math.min(1.5, glitchInt + bass * 1.4 + pump * 0.4);

      // ADAPTIVE QUALITY — stride the per-pixel field write, and cap sorted
      // columns per frame, same budget shape as vex.
      const stride = quality < 0.55 ? 3 : quality < 0.8 ? 2 : 1;
      const rowBudget = quality < 0.55 ? 40 : quality < 0.8 ? 90 : 180;

      // —— 1) BASE FLOWING COLOR FIELD ——
      const t = flow;
      const jitter = intensity * 30;
      for (let y = 0; y < h; y += stride) {
        const fy = y / h;
        const wy = fy + Math.sin(fy * 6.0 + t * 1.3) * 0.04;
        const yEnd = y + stride < h ? y + stride : h;
        for (let x = 0; x < w; x += stride) {
          const fx = x / w;
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

      // —— 2) BASS = PIXEL-SORT SWEEPS (the grounding heartbeat) ——
      let colsLeft = rowBudget * 3;
      const colStride = stride;
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

      // —— 3) NOTE = DISSONANCE TEAR (RGB channel-shift) ——
      for (const sh of shifts) {
        const amt = Math.round((sh.amt + intensity * 8) * sh.life);
        if (amt < 1) continue;
        const yTop = Math.max(0, sh.y - (sh.h >> 1));
        const yBot = Math.min(h, sh.y + (sh.h >> 1));
        for (let y = yTop; y < yBot; y += stride) {
          const rowBase = y * w;
          for (let x = 0; x < w; x++) {
            const sr = x + amt < w ? x + amt : w - 1;
            const sb = x - amt >= 0 ? x - amt : 0;
            const di = (rowBase + x) * 4;
            pix[di] = pix[(rowBase + sr) * 4];
            pix[di + 2] = pix[(rowBase + sb) * 4 + 2];
          }
        }
      }

      // —— 4) NATIVE-RES OVERLAYS ——
      if (kickFlash > 0.04) {
        ink(255, 255, 255, Math.round(kickFlash * 60)).box(0, 0, w, h);
      }

      // Sort edges + shift markers colored by dissonance: gold (consonant)
      // → violet (dissonant), the same gold-low/violet-high instinct drolo
      // used for pitch, now reading the interval instead.
      for (const so of sorts) {
        const [r0, g0, b0] = num.hslToRgb(40 + so.hue * 240, 90, 55);
        ink(r0, g0, b0, 200 * so.life).box(0, so.y0, w, 2);
        ink(0, 0, 0, 140 * so.life).box(0, so.y0 + so.h, w, 2);
      }
      for (const sh of shifts) {
        const [r0, g0, b0] = num.hslToRgb(40 + sh.hue * 240, 100, 60);
        ink(r0, g0, b0, 90 * sh.life).box(0, sh.y - 1, w, 2);
      }

      // Realignment shimmer, reborn: when recent notes have skewed
      // consonant, gold rays burn through the corruption from center.
      if (shifts.length) {
        let dsum = 0;
        for (const sh of shifts) dsum += sh.hue;
        const avgDis = dsum / shifts.length;
        if (avgDis < 0.3) {
          const glow = (0.3 - avgDis) / 0.3;
          const rays = Math.max(4, Math.round(8 * quality));
          for (let i = 0; i < rays; i++) {
            const a = (i / rays) * Math.PI * 2 + flow * 0.4;
            const rx = w / 2 + Math.cos(a) * w * 0.35;
            const ry = h / 2 + Math.sin(a) * h * 0.25;
            ink(255, 225, 160, 50 * glow).line(w / 2, h / 2, rx, ry);
          }
          ink(255, 240, 200, 90 * glow).circle(w / 2, h / 2, 10 + glow * 16, true);
        }
      }

      // Tap glitch blocks (readable tap feedback, dissonance-colored).
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
