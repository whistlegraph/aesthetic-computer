// plimo, 26.07.12
// Voronoi / cellular-shatter pad — a thin wrapper over lib/pads.mjs (the shared
// pad engine: UTC-clock beat grid, `params[0]` rate override e.g. `plimo 0.5`,
// the tap/XY "pump", audio polling, adaptive quality). This file only describes
// what makes plimo plimo: its score, its glassy FM voices, and its stained-glass
// Voronoi paint.
//
// ALLEGORY: the plane is a mosaic of Voronoi cells (nearest of N sites per
// pixel). Every audible note LIGHTS / CRACKS a cell — pitch → cell hue, high
// notes → small bright cells. The beat RE-SEEDS the sites so the mosaic breathes
// and drifts. BASS = the crack lines (cell borders) glowing. Tap SHATTERS a new
// bright cell at the touch point + a glassy stab. The visual IS the score.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// Glassy D-minor-ish stab pattern; high steps = small bright cells.
const ARP = [
  "d3", "a3", "d4", "f4", "a4", "f4", "d4", "a3",
  "c4", "g4", "c5", "e5", "g5", "e5", "c5", "g4",
];
const BASS = ["d2", "d2", "bb1", "bb1", "g1", "g1", "a1", "a1"]; // half-time root
const N_SITES = 10; // Voronoi site count — keep small so per-pixel scan is cheap

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(b?)(\d)$/.exec(n);
  if (!m) return 48;
  const flat = m[2] === "b" ? -1 : 0;
  return (parseInt(m[3], 10) + 1) * 12 + NOTE_SEMI[m[1]] + flat;
}
const ARP_PITCHES = ARP.map(notePitch);
const PITCH_MIN = Math.min(...ARP_PITCHES);
const PITCH_MAX = Math.max(...ARP_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- plimo-specific visual state (engine owns pump/bursts/rhythm) ------------
// Each site is a Voronoi seed with a live cell color/energy. Notes light a cell,
// beats nudge the seeds (re-seed drift). A tap can spawn an extra transient cell.
let sites = []; // { x, y, tx, ty, hue, lit, weight } — normalized 0..1 coords
let crackGlow = 0; // bass-driven border glow, 0..1
let beatFlash = 0; // whole-mosaic re-seed flash, kicked each beat
let seeded = false;

function seedSites() {
  sites = [];
  for (let i = 0; i < N_SITES; i++) {
    const x = Math.random(), y = Math.random();
    sites.push({ x, y, tx: x, ty: y, hue: (i / N_SITES) * 360, lit: 0.2, weight: 1 });
  }
  seeded = true;
}

const CONFIG = {
  bpm: 120,
  steps: ARP.length,
  drawBursts: false, // plimo shatters its own cells on tap in onPaint/onTap
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.55 });
      seedSites();
    },

    // A new UTC beat crossed — fire the score + light the matching cell.
    onBeat({ idx, synth }) {
      if (!seeded) seedSites();
      const s = ((idx % ARP.length) + ARP.length) % ARP.length;
      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / ARP.length) * Math.PI * 2) * 0.7;

      // Glassy FM stab: bell + flute + detuned sine layer.
      voices.bell(synth, note, { beats: 0.7, volume: 0.34, pan });
      voices.flute(synth, note, { beats: 0.5, volume: 0.14, pan: -pan * 0.5 });
      synth({ tone: note, type: "sine", beats: 0.4, attack: 0.003, decay: 0.7,
        volume: 0.16, pan: pan * 0.4 });
      synth({ tone: note, type: "sine", beats: 0.4, attack: 0.003, decay: 0.7,
        volume: 0.11, pan: -pan * 0.4 }); // detuned-feel doubling

      // ALLEGORY: this note lights a cell — pitch → hue, high notes → small &
      // bright (small weight = smaller Voronoi cell).
      const site = sites[s % sites.length];
      if (site) {
        site.hue = 200 + pn * 200; // cyan→violet glass palette
        site.lit = 1;
        site.weight = 1.35 - pn * 0.7; // high notes = tighter cell
        // nudge toward a fresh drift target (the mosaic breathes)
        site.tx = Math.min(1, Math.max(0, site.x + (Math.random() - 0.5) * 0.18));
        site.ty = Math.min(1, Math.max(0, site.y + (Math.random() - 0.5) * 0.18));
      }

      // Sub-bass root on the half-beat — the downbeat you feel + SEE as cracks.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.6, decay: 0.5, volume: 0.5 });
        crackGlow = 1;
      }

      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.13 });
      beatFlash = 1;
    },

    onSim() {
      crackGlow *= 0.9;
      beatFlash *= 0.9;
      for (const st of sites) {
        st.lit *= 0.955;
        // drift each seed toward its target (re-seed animation)
        st.x += (st.tx - st.x) * 0.06;
        st.y += (st.ty - st.y) * 0.06;
        st.weight += (1 - st.weight) * 0.02; // relax back toward even
      }
      // expire transient tap-spawned cells (weight tagged via .transient)
      for (const st of sites) if (st.transient) st.transientLife -= 0.02;
      sites = sites.filter((st) => !st.transient || st.transientLife > 0);
    },

    // Tap = SHATTER a new bright cell at the touch point + a glassy stab.
    onTap({ x, y, synth }) {
      const note = ["d", "f", "a", "c", "e"][Math.floor(x * 5)] +
        (3 + Math.floor((1 - y) * 3));
      voices.bell(synth, note, { beats: 0.5, volume: 0.4, pan: x * 2 - 1 });
      voices.flute(synth, note, { beats: 0.4, volume: 0.18, pan: 1 - x * 2 });
      synth({ tone: note, type: "sine", beats: 0.35, attack: 0.002, decay: 0.6,
        volume: 0.2, pan: x * 2 - 1 });

      // A fresh, tight, very bright cell cracks open right where you touched.
      sites.push({
        x, y, tx: x, ty: y,
        hue: 40 + x * 300, lit: 1.4, weight: 0.55,
        transient: true, transientLife: 1.2,
      });
      if (sites.length > N_SITES + 8) sites.shift();
      crackGlow = Math.min(1, crackGlow + 0.5);
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, beatProgress, band, amp, quality } = s;
      const { width: w, height: h } = screen;
      if (!seeded || sites.length === 0) seedSites();

      const bass = band("subBass");
      const air = band("air");
      const energy = Math.min(2, beatFlash * 0.5 + bass * 1.2 + amp * 0.6 + pump * 0.5);

      // Dark glass backdrop veil (slight trail).
      ink(4, 6, 14, 200).box(0, 0, w, h);

      // --- Voronoi mosaic (nearest weighted site per pixel), STRIDED by quality.
      // Immediate-mode per-block fill is the cost driver, so we work on a fixed
      // ~COLS-wide grid (block size scales with screen) and STRIDE that grid by
      // quality — block-filled so the whole scan stays inside the 60fps budget.
      const COLS = quality < 0.55 ? 40 : quality < 0.8 ? 55 : 72;
      const stride = Math.max(1, Math.round(w / COLS));
      const n = sites.length;
      // Precompute pixel-space site coords + palette once per frame.
      const sx = new Float32Array(n), sy = new Float32Array(n), sw = new Float32Array(n);
      const cr = new Uint8Array(n), cg = new Uint8Array(n), cb = new Uint8Array(n);
      const lit = new Float32Array(n);
      for (let i = 0; i < n; i++) {
        const st = sites[i];
        sx[i] = st.x * w; sy[i] = st.y * h;
        sw[i] = st.weight > 0.05 ? st.weight : 0.05;
        const L = 22 + Math.min(1, st.lit) * 46; // lit cells glow bright
        const [r, g, b] = num.hslToRgb(((st.hue % 360) + 360) % 360, 85, L);
        cr[i] = r; cg[i] = g; cb[i] = b; lit[i] = st.lit;
      }

      // Scan block-grid; coalesce contiguous same-color blocks per row into ONE
      // box (Voronoi cells are contiguous → long runs → far fewer draw calls,
      // which is what actually holds 60fps in immediate mode).
      const shimmerAmt = air * 40;
      const glow = crackGlow * (0.6 + bass);
      for (let py = 0; py < h; py += stride) {
        let runR = -1, runG = 0, runB = 0, runX = 0;
        for (let px = 0; px < w; px += stride) {
          let best = 0, bestD = Infinity, second = Infinity;
          for (let i = 0; i < n; i++) {
            const dx = px - sx[i], dy = py - sy[i];
            const d = (dx * dx + dy * dy) / (sw[i] * sw[i]);
            if (d < bestD) { second = bestD; bestD = d; best = i; }
            else if (d < second) second = d;
          }
          const rim = bestD / (second + 1); // ~1 at borders, 0 at centers
          let r, g, b;
          if (rim > 0.78) { // dark leaded crack line, glows with bass
            r = 8 + 240 * glow; g = 8 + 120 * glow; b = 30 + 255 * glow;
          } else {
            const s2 = lit[best] * shimmerAmt;
            r = Math.min(255, cr[best] + s2);
            g = Math.min(255, cg[best] + s2);
            b = Math.min(255, cb[best] + s2);
          }
          r |= 0; g |= 0; b |= 0;
          if (r !== runR || g !== runG || b !== runB) {
            if (runR >= 0) ink(runR, runG, runB, 235).box(runX, py, px - runX, stride);
            runR = r; runG = g; runB = b; runX = px;
          }
        }
        if (runR >= 0) ink(runR, runG, runB, 235).box(runX, py, w - runX, stride);
      }

      // Site cores: tiny bright specks at each seed (the light source of a cell).
      for (let i = 0; i < n; i++) {
        const L = lit[i];
        if (L > 0.06) {
          ink(255, 255, 255, Math.min(255, 120 * L)).circle(sx[i], sy[i], 1 + L * 3, true);
        }
      }

      if (beatFlash > 0.6 || pump > 1.3) blur?.(1);

      // A faint central glass heart pulse tying the mosaic together.
      const cx = w / 2, cy = h / 2;
      const heartR = 3 + beatFlash * 8 + bass * 10 + pump * 6;
      ink(120, 200, 255, 70 + beatFlash * 70).circle(cx, cy, heartR * 1.6, true);
      ink(255, 255, 255, 120 + beatFlash * 70).circle(cx, cy, heartR, true);
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
