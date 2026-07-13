// sork, 26.07.12
// Isometric voxel-city pad — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `sork 0.5`, the
// tap/XY "pump", audio polling, adaptive quality). This file only describes what
// makes sork sork: its arcade-blip score, its 8-bit voices, and its iso paint.
//
// ALLEGORY — the melody BUILDS a little 3D city. Each note STACKS a voxel on a
// tower (pitch → which tower column + how tall/what hue), drawn as an iso cube
// (top + 2 shaded side faces) back-to-front so towers overlap correctly. The
// bass = a ripple wave that LIFTS/drops rows of towers. The beat = a bright
// highlight sweep crossing the grid. Read the growing city and you read the tune.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Grid ------------------------------------------------------------------
const COLS = 5; // iso city footprint (COLS × ROWS towers)
const ROWS = 5;
const NTOWERS = COLS * ROWS;
const MAXH = 6; // tallest a tower can grow before it topples back to 1

// --- Score -----------------------------------------------------------------
// A bright arcade arpeggio in A-minor pentatonic; each step targets a tower.
const ARP = [
  "a3", "c4", "e4", "g4", "a4", "e4", "d4", "c4",
  "a3", "e4", "g4", "b4", "a4", "g4", "e4", "d4",
];
const BASS = ["a1", "a1", "f1", "f1", "c2", "c2", "e1", "e1"]; // half-time root

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

// --- 8-bit arcade voices (raw synth — bright, short, instant) ----------------
// A hard square/triangle blip: fast attack, fast decay. This is the melody.
const blip = (synth, tone, o = {}) =>
  synth({
    tone,
    type: o.type ?? "square",
    beats: o.beats ?? 0.16,
    attack: o.attack ?? 0.001,
    decay: o.decay ?? 0.4,
    volume: o.volume ?? 0.3,
    pan: o.pan ?? 0,
  });
// A coin-y bell: a quick low square blip then an octave-up ring (Mario-pickup
// "bling" — two stacked square tones, the second higher & held a touch longer).
const octUp = (n) => {
  const m = /^([a-g])(\d)$/.exec(n);
  return m ? `${m[1]}${Math.min(9, parseInt(m[2], 10) + 1)}` : n;
};
const coin = (synth, tone, o = {}) => {
  const v = o.volume ?? 0.2;
  const pan = o.pan ?? 0;
  blip(synth, tone, { type: "square", beats: 0.06, decay: 0.2, volume: v, pan });
  blip(synth, octUp(tone), { type: "square", beats: 0.16, decay: 0.28, volume: v, pan });
};

// --- sork-specific state (engine owns pump/bursts/rhythm) ------------------
let heights = new Array(NTOWERS).fill(1); // voxel count per tower
let hues = new Array(NTOWERS).fill(180); // per-tower hue (last note that hit it)
let pop = new Array(NTOWERS).fill(0); // per-tower "just stacked" bounce, 0..1
let ripple = 0; // bass ripple phase (advances each bass note)
let rippleAmp = 0; // decaying bass ripple lift
let sweep = -1; // beat highlight sweep position across grid diagonal
let sweepLife = 0; // decaying sweep brightness

const CONFIG = {
  bpm: 140,
  steps: ARP.length,
  drawBursts: false, // sork draws its own tap feedback in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.18, feedback: 0.4 });
    },

    // A new UTC beat crossed — fire the blip + stack a voxel on its tower.
    onBeat({ idx, synth }) {
      const s = ((idx % ARP.length) + ARP.length) % ARP.length;
      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      // Pitch picks the tower column; step-phase picks the row → snakes across city.
      const col = Math.floor(pn * (COLS - 0.001));
      const row = ((Math.floor(s / 2) % ROWS) + ROWS) % ROWS;
      const t = row * COLS + col;
      const pan = (col / (COLS - 1)) * 1.4 - 0.7;

      // STACK a voxel on that tower (topple back to 1 when it overflows).
      heights[t] = heights[t] >= MAXH ? 1 : heights[t] + 1;
      hues[t] = 20 + pn * 300;
      pop[t] = 1;

      // Arcade blip: bright short square (raw synth), fast attack/decay.
      blip(synth, note, { type: "square", beats: 0.16, volume: 0.34, pan });
      // Triangle harmonic sparkle a touch quieter for body.
      blip(synth, note, { type: "triangle", beats: 0.1, volume: 0.14, pan });

      // Coin-y bell on the tallest / high-pitch accents.
      if (pn > 0.6 || heights[t] >= MAXH - 1)
        coin(synth, note, { volume: 0.2, pan: -pan });

      // Sub-bass root on the half-beat → a ripple that lifts a whole row.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.2, decay: 0.5, volume: 0.5 });
        ripple += 1;
        rippleAmp = 1;
      }

      voices.hat(synth, { tone: 9000, beats: 0.08, volume: 0.12 });

      // Beat highlight sweep starts at the far corner, crosses the diagonal.
      sweep = 0;
      sweepLife = 1;
    },

    onSim() {
      rippleAmp *= 0.92;
      sweepLife *= 0.9;
      if (sweep >= 0) {
        sweep += 0.14;
        if (sweep > COLS + ROWS) sweep = -1;
      }
      for (let i = 0; i < NTOWERS; i++) pop[i] *= 0.85;
    },

    // Tap = stack a voxel on the tapped tower + a blip. X → column, Y → height/pitch.
    onTap({ x, y, synth }) {
      const col = Math.max(0, Math.min(COLS - 1, Math.floor(x * COLS)));
      const row = Math.max(0, Math.min(ROWS - 1, Math.floor(y * ROWS)));
      const t = row * COLS + col;
      const bump = 1 + Math.floor((1 - y) * 3); // higher tap → taller stack
      heights[t] = Math.min(MAXH, heights[t] + bump);
      const pn = 1 - y;
      hues[t] = 20 + pn * 300;
      pop[t] = 1.4;
      const note = ["a", "c", "d", "e", "g"][col % 5] + (3 + Math.floor(pn * 2));
      blip(synth, note, { type: "square", beats: 0.2, volume: 0.4, pan: x * 2 - 1 });
      coin(synth, note, { volume: 0.28, pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      // Sky wash (dark candy night). Cheap flat fill each frame.
      ink(14, 12, 30, 255).box(0, 0, w, h);

      const bass = band("subBass");
      const energy = Math.min(1.6, rippleAmp * 0.7 + bass * 1.1 + amp * 0.5 + pump * 0.4);

      // Adaptive footprint: scale how many towers we DRAW by quality (iso cubes
      // are cheap but bounded — always draw at least a 3×3 core).
      const drawCols = Math.max(3, Math.round(COLS * quality));
      const drawRows = Math.max(3, Math.round(ROWS * quality));

      // Iso projection: cell (col,row) → screen. Tile "radius" scales to screen.
      const tile = Math.min(w, h) * 0.11;
      const tw = tile; // half tile width  (x screen offset per +col / +row)
      const th = tile * 0.5; // half tile height (y screen offset per +col / +row)
      const vox = tile * 0.62; // voxel height in screen px
      const cx = w / 2;
      const cy = h / 2 - drawRows * th * 0.5 + tile * 0.5;

      const iso = (col, row) => [
        cx + (col - row) * tw,
        cy + (col + row) * th,
      ];

      // BEAT SWEEP: a diagonal band (col+row ≈ sweep) glowing bright.
      const sweepPos = sweep >= 0 ? sweep : -99;

      // Draw towers BACK-TO-FRONT: farthest (small col+row) first so nearer
      // towers overlap correctly. Painter's order = ascending (col+row), then col.
      const order = [];
      for (let row = 0; row < drawRows; row++)
        for (let col = 0; col < drawCols; col++) order.push([col, row]);
      order.sort((a, b) => (a[0] + a[1]) - (b[0] + b[1]) || a[0] - b[0]);

      for (const [col, row] of order) {
        const t = row * COLS + col;
        // Bass ripple lifts rows in a travelling sine wave (gentle, so the
        // city stays legible while the wave visibly rolls through).
        const lift =
          rippleAmp * th * 1.4 *
          Math.sin(ripple * 0.9 + (col + row) * 0.7);
        const [bx, by0] = iso(col, row);
        const by = by0 + lift;

        const height = heights[t];
        const bounce = pop[t] * vox * 0.4; // fresh-stack squash-and-pop
        const baseHue = hues[t];

        // Sweep proximity (0..1) — brightens the row the highlight is on.
        const sd = Math.abs((col + row) - sweepPos);
        const sweepGlow = sweepLife * Math.max(0, 1 - sd * 0.8);

        // Draw the WHOLE tower as one column (3 iso faces = 6 triangles),
        // regardless of height — bounded cost. Filled quads route through the
        // slow scanline fill, so we split each face into two triangles (the
        // engine fast-paths 3-point shapes via fillTri).
        const towerH = height * vox + bounce; // total column height in px
        const topY = by - towerH; // top-face center y (higher = smaller y)
        const light = 46 + height * 3 + sweepGlow * 34 + energy * 7;
        const hue = ((baseHue % 360) + 360) % 360;

        // TOP diamond corners.
        const tN = [bx, topY - th];
        const tE = [bx + tw, topY];
        const tS = [bx, topY + th];
        const tW = [bx - tw, topY];
        // BASE (ground) corners east/south/west (walls run top→base).
        const gE = [bx + tw, by];
        const gS = [bx, by + th];
        const gW = [bx - tw, by];

        // LEFT wall (west→south) — darkest. Two tris.
        const [lr, lg, lb] = num.hslToRgb(hue, 68, Math.max(6, light - 24));
        ink(lr, lg, lb, 255).shape([tW, tS, gS]);
        ink(lr, lg, lb, 255).shape([tW, gS, gW]);
        // RIGHT wall (south→east) — mid. Two tris.
        const [rr, rg, rb] = num.hslToRgb(hue, 72, Math.max(9, light - 11));
        ink(rr, rg, rb, 255).shape([tS, tE, gE]);
        ink(rr, rg, rb, 255).shape([tS, gE, gS]);
        // TOP face — brightest. Two tris.
        const [tr, tg, tb] = num.hslToRgb(hue, 80, Math.min(92, light + 16));
        ink(tr, tg, tb, 255).shape([tN, tE, tS]);
        ink(tr, tg, tb, 255).shape([tN, tS, tW]);

        // Blocky 8-bit read: cheap level seams on the two visible walls (lines
        // only — no fills). Only when reasonably close / lifted.
        if (height > 1 && (col + row) >= drawCols + drawRows - 8) {
          const [sr, sg, sb] = num.hslToRgb(hue, 60, Math.max(4, light - 34));
          for (let lv = 1; lv < height; lv++) {
            const yy = by - lv * vox;
            ink(sr, sg, sb, 150).line(bx - tw, yy, bx, yy + th); // west→south
            ink(sr, sg, sb, 150).line(bx, yy + th, bx + tw, yy); // south→east
          }
        }

        // Bright top outline for the front two edges (sweep highlights it).
        const oa = 50 + sweepGlow * 150;
        ink(255, 255, 255, oa).line(tW[0], tW[1], tS[0], tS[1]);
        ink(255, 255, 255, oa).line(tS[0], tS[1], tE[0], tE[1]);

        // Coin sparkle atop a freshly-popped or maxed tower.
        if (pop[t] > 0.4 || height >= MAXH) {
          const py = topY - vox * 0.5;
          const [cr, cg, cb] = num.hslToRgb((baseHue + 60) % 360, 90, 70);
          ink(cr, cg, cb, 190 * Math.max(pop[t], height >= MAXH ? 0.5 : 0))
            .circle(bx, py, 3 + pop[t] * 4, true);
        }
      }

      // Bass ripple ground-glow hint at the front edge.
      if (rippleAmp > 0.05) {
        const gy = cy + (drawCols + drawRows) * th + tile * 0.4;
        ink(120, 60, 220, 40 * rippleAmp).box(0, gy, w, 4);
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
