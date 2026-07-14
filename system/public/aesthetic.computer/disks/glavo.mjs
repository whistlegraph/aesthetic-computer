// glavo, 26.07.14
// A rising-bubble bath seen through vex's datamosh eye — a thin wrapper over
// lib/pads.mjs (the shared pad engine: UTC-clock beat grid, `params[0]` rate
// override e.g. `glavo 0.5`, the tap/XY "pump", audio polling). This file only
// describes what makes glavo glavo: it keeps tavo's watery score and physical
// bubble voice verbatim (the ear), and replaces its soft translucent circles
// with vex's pixel-sort / RGB-tear machinery (the eye) — so every bubble is now
// a smear of corrupted water rising through the tank instead of a drawn glyph.
//
// ALLEGORY — the visual IS the (bubble) score:
//   • NOTE → a bubble is BORN as a band of the water that gets column-sorted by
//     brightness (vex's pixel-sort) — the ascending smear you'd see if a real
//     bubble dragged the fluid up with it. Size ∝ pitch, same as tavo: a fat
//     low note = a tall, slow sort-band; a thin high note = a slim, quick one.
//   • The bubble's SOUND (AC's physical `type:"bubble"` synth, radius-matched)
//     is doubled by an RGB channel-shift TEAR at the band (vex's chromatic
//     aberration) — hue ∝ pitch exactly as tavo colored its bubbles (warm/low
//     .. cool/high), so the tear reads as the bubble's own shimmer, not noise.
//   • BASS (tavo's half-time CURRENT) → global glitch intensity + rise speed:
//     a fat sub note widens every tear and drags every band upward faster —
//     the same "current pushes the shoal" idea, now expressed as glitch depth.
//   • POP (band reaches the surface) → a bright ring-flash + a plinked
//     bell/pluck release, tavo's pop-plink, deferred the same way tavo did it
//     (sim can't hold a synth handle, so pops queue a plink played next paint).
// Base field + column-sort + row-tear all stay vex's exact techniques, just
// driven by tavo's melody instead of glitch-techno, and recolored to the tank.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score (verbatim from tavo — this is the ear we inherit) ----------------
const MELODY = [
  "a3", "e4", "c4", "a4",
  "e3", "b3", "e4", "g4",
  "f3", "c4", "a3", "f4",
  "c3", "g3", "d4", "e4",
];
const CURRENT = ["a1", "a1", "e2", "e2", "f1", "f1", "c2", "c2"]; // half-time bass current

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n || "");
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const PITCHES = MELODY.map(notePitch);
const PMIN = Math.min(...PITCHES);
const PMAX = Math.max(...PITCHES);
const pitchNorm = (n) => (notePitch(n) - PMIN) / Math.max(1, PMAX - PMIN); // 0 low .. 1 high

// map pitch → the physical bubble synth's radius, same curve tavo used, so the
// SOUND's size and the TEAR's size agree.
function bubbleRadius(pn) {
  return 8 + (1 - pn) * 46; // 8 (tiny/high) .. 54 (fat/low)
}

// --- glavo-specific state (vex's eye, retargeted at tavo's bubbles) ---------
// One bubble = one rising sort-band + its co-located RGB tear. Fusing vex's
// `sorts`/`shifts` into a single object per note is what makes them "belong
// together" here: the tear always sits on the band that's smearing it.
let bubbles = []; // { yf, hf, amt, hue, pn, life, popped, popLife, pan, note }
let current = 0; // eased bass level → rise speed + glitch intensity (tavo's current)
let surge = 0; // whole-tank flash on the half-beat current pulse
let surface = 0; // shimmer line, kicked by pops

const MAX_BUBBLES = 12; // combined sort+tear cost is heavier than a drawn circle

let scratchLuma = null; // reused column-sort scratch (vex)
let scratchCol = null;
let lastSynth = null; // stashed so sim-time pops can plink (tavo's deferred pattern)
const pendingPlinks = [];

let HUE_LUT = null; // watery blue→teal LUT (vex's technique, tavo's palette)
function buildLUT(num) {
  const lut = new Uint8Array(256 * 3);
  for (let i = 0; i < 256; i++) {
    const [r, g, b] = num.hslToRgb(185 + (i / 256) * 45, 65, 10 + (i / 256) * 14);
    lut[i * 3] = r;
    lut[i * 3 + 1] = g;
    lut[i * 3 + 2] = b;
  }
  return lut;
}

let flow = 0; // base-field flow phase (advances in sim)

const CONFIG = {
  bpm: 96,
  steps: MELODY.length,
  drawBursts: false, // glavo draws its own tap tears, not the engine's default burst
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.55 }); // tavo's watery tank verb
      bubbles = [];
      current = 0;
      surge = 0;
      surface = 0;
    },

    onBeat({ idx, synth, sound, pump }) {
      const s = ((idx % MELODY.length) + MELODY.length) % MELODY.length;
      const note = MELODY[s];
      const pn = pitchNorm(note);
      const radius = bubbleRadius(pn);
      const boost = 1 + Math.min(1, pump * 0.3);
      const pan = Math.sin((s / MELODY.length) * Math.PI * 2) * 0.7;

      // The bloop (tavo's physical bubble voice) + a bell doubling the pitch.
      sound?.bubble?.({ radius, rise: 0.1 + pn * 0.5, volume: 0.9 * boost, pan });
      voices.bell(synth, note, { beats: 0.6, volume: 0.14 * boost, pan });

      // ALLEGORY: birth the bubble as a rising sort-band + co-located tear.
      // hf/amt/hue follow tavo's pitch mapping; the mechanics are vex's.
      if (bubbles.length >= MAX_BUBBLES) bubbles.shift();
      bubbles.push({
        yf: 0.94 + Math.random() * 0.03,
        hf: 0.05 + (1 - pn) * 0.15,
        amt: 4 + pn * 22,
        hue: 190 + (1 - pn) * 130, // warm/low .. cool/high, tavo's tank palette
        pn,
        life: 1,
        popped: false,
        popLife: 0,
        pan,
        note,
      });

      // BASS current on the half-beat — tavo's push, now also vex's glitch depth.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % CURRENT.length) + CURRENT.length) % CURRENT.length;
        voices.sub(synth, CURRENT[bi], { beats: 1.8, decay: 0.5, volume: 0.5 * boost });
        current = Math.min(1.6, current + 0.7);
        surge = 1;
      }

      voices.hat(synth, { tone: 6500, beats: 0.1, volume: 0.1 * boost }); // waterline tick

      lastSynth = synth; // so sim-time pops can plink with no tap in hand
    },

    onSim({ pump, band }) {
      flow += 0.02;
      current *= 0.96;
      if (!Number.isFinite(current)) current = 0;
      current = Math.max(current, band("subBass") * 1.2);
      surge *= 0.85;
      surface *= 0.9;
      if (surface < 0.001) surface = 0;

      const rise = 0.0016 + current * 0.006 + pump * 0.0015;

      for (let i = bubbles.length - 1; i >= 0; i--) {
        const b = bubbles[i];
        if (!Number.isFinite(b.yf)) { bubbles.splice(i, 1); continue; }
        if (b.popped) {
          b.popLife -= 0.12;
          if (b.popLife <= 0) bubbles.splice(i, 1);
          continue;
        }
        // Higher-pitched (smaller) bubbles rise faster — tavo's buoyancy rule.
        b.yf -= rise * (0.7 + b.pn * 0.9);
        b.life = Math.min(1, b.life + 0.08);
        if (b.yf <= 0.08) {
          b.popped = true;
          b.popLife = 0.6;
          surface = Math.min(1.4, surface + 0.5);
          pendingPlinks.push({ note: b.note, pan: b.pan });
        }
      }
    },

    // Tap = blow a bubble at the tap point (tavo), rendered as a fresh tear/band
    // right there (vex). X → hue-ish pan, Y → size/pitch (top = small/high).
    onTap({ x, y, synth, sound, isDraw, pump }) {
      const pn = Math.max(0, Math.min(1, 1 - y));
      const scale = ["a", "c", "d", "e", "g"];
      const deg = Math.floor(pn * (scale.length - 1));
      const oct = 2 + Math.floor(pn * 3);
      const note = scale[Math.max(0, Math.min(scale.length - 1, deg))] + oct;
      const radius = 12 + (1 - pn) * 52;
      const boost = 1 + Math.min(1, pump * 0.3);
      const pan = x * 2 - 1;

      sound?.bubble?.({ radius, rise: 0.05 + pn * 0.5, volume: (isDraw ? 0.7 : 1.0) * boost, pan });
      voices.bell(synth, note, { beats: 0.4, volume: 0.16 * boost, pan });

      if (bubbles.length >= MAX_BUBBLES) bubbles.shift();
      bubbles.push({
        yf: Math.min(0.95, y),
        hf: 0.06 + (1 - pn) * 0.16,
        amt: 8 + x * 26,
        hue: x * 360,
        pn,
        life: isDraw ? 0.6 : 1,
        popped: false,
        popLife: 0,
        pan,
        note,
      });
      current = Math.min(1.6, current + (1 - y) * 0.25);
      lastSynth = synth;
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, quality, amp, band } = s;
      const { width: w, height: h } = screen;
      const pix = screen.pixels;
      if (!pix) return;

      if (!HUE_LUT) HUE_LUT = buildLUT(num);
      const lut = HUE_LUT;

      const bass = band("subBass");
      const intensity = Math.min(1.5, current + bass * 1.4 + pump * 0.4);

      // ADAPTIVE QUALITY (vex's rule): stride the base field, cap sorted columns.
      const stride = quality < 0.55 ? 3 : quality < 0.8 ? 2 : 1;
      const colBudget = quality < 0.55 ? 40 : quality < 0.8 ? 90 : 180;

      // —— 1) BASE FLOWING WATER FIELD ——
      const t = flow;
      const jitter = intensity * 24;
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

      // —— 2) EACH BUBBLE = a column-sorted band (vex's pixel-sort), the smear
      // its rise drags through the water — bounded by colBudget per frame.
      let colsLeft = colBudget * 3;
      if (!scratchLuma || scratchLuma.length < h) {
        scratchLuma = new Float32Array(h);
        scratchCol = new Uint32Array(h);
      }
      const lum = scratchLuma, col = scratchCol;
      for (const b of bubbles) {
        if (colsLeft <= 0) break;
        const cy = b.yf * h;
        const bandH = b.hf * h;
        const yTop = Math.max(0, Math.floor(cy - bandH / 2));
        const yBot = Math.min(h, Math.floor(cy + bandH / 2));
        const len = yBot - yTop;
        if (len <= 1) continue;
        for (let x = 0; x < w && colsLeft > 0; x += stride) {
          colsLeft--;
          for (let i = 0; i < len; i++) {
            const di = ((yTop + i) * w + x) * 4;
            const r = pix[di], g = pix[di + 1], bl = pix[di + 2];
            lum[i] = r * 0.299 + g * 0.587 + bl * 0.114;
            col[i] = (r << 16) | (g << 8) | bl;
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

      // —— 3) EACH BUBBLE'S SOUND doubles as an RGB channel-shift tear on the
      // same band — the note's own shimmer, hue-matched to tavo's pitch color.
      for (const b of bubbles) {
        const amt = Math.round((b.amt + intensity * 8) * b.life);
        if (amt < 1) continue;
        const cy = b.yf * h;
        const bandH = b.hf * h;
        const yTop = Math.max(0, Math.floor(cy - bandH / 2));
        const yBot = Math.min(h, Math.floor(cy + bandH / 2));
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
      // Current surge = a whole-tank flash on the half-beat bass push.
      if (surge > 0.04) ink(220, 245, 255, Math.round(surge * 50)).box(0, 0, w, h);

      // Band edges tinted by the bubble's own hue — reads the note without
      // vex's stark white/black framing.
      for (const b of bubbles) {
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 80, 62);
        const cy = b.yf * h;
        const bandH = b.hf * h;
        ink(r0, g0, b0, Math.round(170 * b.life)).box(0, Math.floor(cy - bandH / 2), w, 2);
        ink(0, 20, 30, Math.round(120 * b.life)).box(0, Math.floor(cy + bandH / 2), w, 2);
      }

      // Pop = a bright expanding ring where the band met the surface.
      for (const b of bubbles) {
        if (!b.popped) continue;
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 85, 72);
        const ry = b.yf * h;
        const rad = (1 - b.popLife) * Math.min(w, h) * 0.14 + 6;
        ink(r0, g0, b0, Math.round(200 * b.popLife)).circle(w / 2, ry, rad, false, 2);
      }

      // Surface shimmer near the top, kicked by recent pops.
      if (surface > 0.02) {
        ink(180, 230, 255, Math.round(120 * surface)).box(0, Math.floor(h * 0.06), w, 2);
      }

      // Blur on strong pushes only when cheap (matches tavo's dreamy bloom).
      if ((surface > 0.5 || pump > 1.3 || amp > 0.5) && quality >= 0.85) api.blur?.(1);

      // Deferred pop plinks — a bell + pluck release, tavo's pattern.
      if (pendingPlinks.length && lastSynth) {
        for (const pl of pendingPlinks) {
          voices.bell(lastSynth, pl.note, { beats: 0.35, volume: 0.14, pan: pl.pan });
          voices.pluck(lastSynth, pl.note, { beats: 0.3, decay: 0.5, volume: 0.12, pan: pl.pan });
        }
        pendingPlinks.length = 0;
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
