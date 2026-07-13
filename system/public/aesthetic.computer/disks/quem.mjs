// quem, 26.07.12
// Dendritic crystal/coral GROWTH pad — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `quem 0.5`, the tap/XY "pump", audio polling). This file only describes what
// makes quem quem: its phasing score, its frosty voices, and its accretion paint.
//
// ALLEGORY — DLA / coral / crystal growth × minimal Steve-Reich PHASING. Two
// short melodic loops of DIFFERENT length (6 vs 8 steps) drift out of phase; each
// fires a growth spurt on its own color. Every note ACCRETES a segment onto a
// live branch (pitch → hue + branch angle); the two loops become two interweaving
// growth colors that slowly shift against each other. Bass = growth RATE (energy
// that lengthens segments + sprouts side-branches). Structures grow outward from
// seed points then slowly fade/reset — so the pad loops as a living dendrite.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score: TWO phasing loops of different length --------------------------
// Loop A (6 steps) on voices.pluck (marimba/harp), Loop B (8 steps) on
// voices.bell. 6 vs 8 → they realign every 24 beats (lcm) → slow Reich phase.
const LOOP_A = ["c4", "e4", "g4", "e4", "a4", "g4"]; // 6-step marimba figure
const LOOP_B = ["c3", "d3", "g3", "e3", "a3", "b3", "g3", "e3"]; // 8-step figure
const BASS = ["c2", "c2", "a1", "a1", "f1", "f1", "g1", "g1"]; // growth-rate root

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const ALL = [...LOOP_A, ...LOOP_B].map(notePitch);
const PMIN = Math.min(...ALL);
const PMAX = Math.max(...ALL);
const pitchNorm = (n) => (notePitch(n) - PMIN) / Math.max(1, PMAX - PMIN);

// --- quem-specific visual state (the engine owns pump/bursts/rhythm) --------
// A crystal = a seed + a growing bag of segments. Each segment is appended once
// (on a note) and never recomputed — we hold 60fps by only ever DRAWING, never
// regrowing, and by capping the live segment count with ctx.quality.
let crystals = []; // { x, y, tips:[{x,y,ang}], segs:[{x0,y0,x1,y1,hue,life,w}], loop, life }
let growRate = 0; // bass-driven growth energy, decays
let flash = 0; // whole-field pulse each beat
const MAX_SEGS = 520; // hard cap (scaled by quality); oldest segs drop off

// Seed a crystal at (nx,ny) in normalized coords, tagged to a phasing loop.
function seed(nx, ny, loop, w, h) {
  const x = nx * w;
  const y = ny * h;
  const tips = [];
  const n = 3; // initial dendrite arms
  for (let i = 0; i < n; i++) {
    tips.push({ x, y, ang: (i / n) * Math.PI * 2 + Math.random() * 0.6 });
  }
  crystals.push({ x, y, tips, segs: [], loop, life: 1 });
  if (crystals.length > 6) crystals = crystals.slice(-6);
}

// Grow every crystal of a given loop by one accretion step (hue from pitch).
// Appends ONE segment per tip — cheap, incremental (never recompute the tree).
function growLoop(loop, hue, len, w, h) {
  for (const c of crystals) {
    if (c.loop !== loop) continue;
    for (const t of c.tips) {
      // dendritic wander: turn a little, occasionally branch
      t.ang += (Math.random() - 0.5) * 0.9;
      const x1 = t.x + Math.cos(t.ang) * len;
      const y1 = t.y + Math.sin(t.ang) * len;
      c.segs.push({ x0: t.x, y0: t.y, x1, y1, hue, life: 1, w: 1.4 + growRate * 1.4 });
      t.x = x1;
      t.y = y1;
      // side-branch when growth energy is high (bass = growth rate)
      if (growRate > 0.35 && Math.random() < 0.25 && c.tips.length < 10) {
        c.tips.push({ x: x1, y: y1, ang: t.ang + (Math.random() < 0.5 ? 0.7 : -0.7) });
      }
    }
  }
}

const CONFIG = {
  bpm: 96, // marimba-friendly; two loops phase against the grid
  steps: 24, // lcm(6,8) — one full phase cycle before both loops realign
  drawBursts: false, // quem draws its own frosty tap seeds in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.55 });
    },

    // A new UTC beat crossed — advance BOTH phasing loops on their own periods.
    onBeat({ idx, synth, screen }) {
      const w = screen?.width || 256;
      const h = screen?.height || 256;

      // Ensure we always have a seed of each color to grow into.
      if (!crystals.some((c) => c.loop === "A")) seed(0.35, 0.55, "A", w, h);
      if (!crystals.some((c) => c.loop === "B")) seed(0.65, 0.5, "B", w, h);

      // Growth spurt length scales with bass energy (growth rate).
      const baseLen = Math.min(w, h) * (0.03 + growRate * 0.045);

      // LOOP A — 6-step marimba (cool frost hues 170..210).
      const ia = ((idx % LOOP_A.length) + LOOP_A.length) % LOOP_A.length;
      const na = LOOP_A[ia];
      const pna = pitchNorm(na);
      const pan = Math.sin((ia / LOOP_A.length) * Math.PI * 2) * 0.6;
      voices.pluck(synth, na, { beats: 0.7, decay: 0.5, volume: 0.42, pan });
      growLoop("A", 170 + pna * 40, baseLen * (0.7 + pna * 0.6), w, h);

      // LOOP B — 8-step bell (warm accent hues 280..330), out of phase with A.
      const ib = ((idx % LOOP_B.length) + LOOP_B.length) % LOOP_B.length;
      const nb = LOOP_B[ib];
      const pnb = pitchNorm(nb);
      voices.bell(synth, nb, { beats: 0.8, volume: 0.16, pan: -pan });
      growLoop("B", 280 + pnb * 50, baseLen * (0.7 + pnb * 0.6), w, h);

      // Sub-bass root every other beat = the growth-rate pulse you feel + SEE.
      if (idx % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.6, decay: 0.5, volume: 0.5 });
        growRate = 1;
      }
      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.12 });
      flash = 1;

      // Full phase cycle complete → let the dendrites fade and reseed (loop).
      if (step === 0) for (const c of crystals) c.life = Math.min(c.life, 0.6);
    },

    onSim({ quality }) {
      growRate *= 0.9;
      flash *= 0.88;
      // Age segments + crystals; drop dead ones. Cap segment count by quality so
      // low-power frames render fewer strokes (holds 60fps).
      const cap = Math.round(MAX_SEGS * (0.4 + 0.6 * quality));
      let total = 0;
      for (const c of crystals) {
        c.life -= 0.0016;
        for (const s of c.segs) s.life -= 0.0016; // slow: dendrites accrete before fading
        c.segs = c.segs.filter((s) => s.life > 0);
        total += c.segs.length;
      }
      crystals = crystals.filter((c) => c.life > 0 && c.segs.length > 0);
      // Global trim: shed the oldest segments across all crystals when over cap.
      while (total > cap && crystals.length) {
        let oldest = crystals[0];
        for (const c of crystals) if (c.segs.length > oldest.segs.length) oldest = c;
        oldest.segs.shift();
        total--;
      }
    },

    // Tap = seed a NEW growth crystal at the tap point + a pluck. X→hue/loop,
    // Y→pitch (engine already bumped pump). Frosty seed flash drawn in onPaint.
    onTap({ x, y, ex, ey, synth, screen }) {
      const w = screen?.width || 256;
      const h = screen?.height || 256;
      const loop = x < 0.5 ? "A" : "B";
      seed(ex / w, ey / h, loop, w, h);
      growRate = 1; // taps energize growth
      flash = 1;
      const scale = ["c", "d", "e", "g", "a"];
      const note = scale[Math.floor(x * 5) % 5] + (3 + Math.floor((1 - y) * 3));
      voices.pluck(synth, note, { beats: 0.6, volume: 0.55, pan: x * 2 - 1 });
      voices.bell(synth, note, { beats: 0.4, volume: 0.28 * (1 - y), pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, band, amp } = s;
      const { width: w, height: h } = screen;

      const bass = band("subBass");
      const energy = Math.min(2, flash * 0.5 + bass * 1.1 + amp * 0.5 + pump * 0.4);

      // Dark frosty ground — faint veil each frame so growth accretes (trails).
      ink(4, 8, 14, 22).box(0, 0, w, h);

      // DRAW each crystal's accreted segments (never recomputed — cheap redraw).
      // Frost core (bright) + soft halo. Loop-A cool, loop-B warm hues interweave.
      for (const c of crystals) {
        for (const seg of c.segs) {
          const life = seg.life * c.life;
          const [r, g, b] = num.hslToRgb(
            ((seg.hue % 360) + 360) % 360,
            75,
            55 + life * 25,
          );
          const a = 110 + 145 * life;
          ink(r, g, b, a).line(seg.x0, seg.y0, seg.x1, seg.y1, seg.w + energy * 0.4);
          // frost tip glint on the freshest segments
          if (life > 0.75) ink(230, 245, 255, 150 * life).circle(seg.x1, seg.y1, 1 + life, true);
        }
      }

      // Seed glow at each crystal origin (where growth radiates from).
      for (const c of crystals) {
        const hue = c.loop === "A" ? 190 : 300;
        const [r, g, b] = num.hslToRgb(hue, 80, 60);
        const gr = 3 + c.life * 5 + growRate * 4;
        ink(r, g, b, 60 * c.life).circle(c.x, c.y, gr * 1.8, true);
        ink(235, 245, 255, 150 * c.life).circle(c.x, c.y, gr * 0.6, true);
      }

      if (flash > 0.6 || pump > 1.3) blur?.(1);
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
