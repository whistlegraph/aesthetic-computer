// rullo, 26.07.12
// A honeycomb gamelan pad — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `rullo 0.5`, the
// tap/XY "pump", audio polling). rullo is a FLAT hex-grid tessellation: a
// pointy-top honeycomb of cells. Every audible note LIGHTS one hex bright
// (pitch → cell + hue) and the light RIPPLES outward to its neighbors as a
// hex-distance wave (a little cellular automaton of glow). The BEAT is a
// hexagonal wave that sweeps across the comb; the BASS makes the whole grid
// breathe/glow. Distinct from prism (a radial kaleidoscope) — this is a jewel
// honeycomb lighting up, cell by cell.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score (Pelog-ish pentatonic gamelan) -----------------------------------
// A five-note pelog set across octaves — shimmery, slightly Eastern.
const SCALE = ["d3", "e3", "g3", "a3", "b3", "d4", "e4", "g4"];
const BASS = ["d2", "d2", "a1", "a1", "g1", "g1", "a1", "a1"]; // half-time root
const STEPS = SCALE.length;

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const SCALE_PITCHES = SCALE.map(notePitch);
const PITCH_MIN = Math.min(...SCALE_PITCHES);
const PITCH_MAX = Math.max(...SCALE_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- Hex grid geometry (pointy-top, axial coords) ---------------------------
// The comb is a set of axial cells (q,r). We keep a small fixed roster and map
// each score step to one cell; hex-distance from that cell drives the ripple.
// Axial → pixel conversion happens in onPaint against the live screen size.
const HEX = []; // { q, r } — the comb cells
const HEX_RADIUS = 3; // hex "radius" in cells (fills a hexagon-shaped comb)
for (let q = -HEX_RADIUS; q <= HEX_RADIUS; q++) {
  const r1 = Math.max(-HEX_RADIUS, -q - HEX_RADIUS);
  const r2 = Math.min(HEX_RADIUS, -q + HEX_RADIUS);
  for (let r = r1; r <= r2; r++) HEX.push({ q, r });
}
// Cube-coordinate hex distance between two axial cells.
function hexDist(a, b) {
  const ax = a.q, az = a.r, ay = -ax - az;
  const bx = b.q, bz = b.r, by = -bx - bz;
  return (Math.abs(ax - bx) + Math.abs(ay - by) + Math.abs(az - bz)) / 2;
}
// Map a score step (0..STEPS-1) to a comb cell, spread around the ring.
function stepCell(s) {
  return HEX[(s * 5 + 2) % HEX.length]; // *5 scatters neighbors apart
}

// --- rullo-specific visual state (engine owns pump/bursts/rhythm) -----------
let cellLight = new Array(HEX.length).fill(0); // per-hex brightness (decays)
let cellHue = new Array(HEX.length).fill(200); // per-hex hue
let ripples = []; // { cx, cy(axial), hue, age, life } — expanding hex waves
let gridGlow = 0; // whole-comb bass breathing
let beatWave = 0; // hex sweep phase, kicked each beat

// Light a cell + seed a ripple that will wash its neighbors.
function strike(cellIndex, hue, power) {
  cellLight[cellIndex] = Math.min(1.4, cellLight[cellIndex] + power);
  cellHue[cellIndex] = hue;
  const c = HEX[cellIndex];
  ripples.push({ q: c.q, r: c.r, hue, age: 0, life: 1 });
  if (ripples.length > 14) ripples = ripples.slice(-14);
}

const CONFIG = {
  bpm: 108,
  steps: STEPS,
  drawBursts: false, // rullo draws its own hex tap glow in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.34, feedback: 0.62 });
    },

    // A new UTC beat crossed — fire the gamelan + light the matching hex.
    onBeat({ idx, synth }) {
      const s = ((idx % STEPS) + STEPS) % STEPS;
      const note = SCALE[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / STEPS) * Math.PI * 2) * 0.6;
      const hue = 30 + pn * 260; // amber → violet up the scale

      // Gamelan bell: layered + slightly detuned for the shimmering metallophone
      // beat (two bell voices a hair apart = inharmonic gong sparkle).
      voices.bell(synth, note, { beats: 1.1, volume: 0.34, pan });
      voices.bell(synth, note, { beats: 0.7, volume: 0.14, pan: -pan * 0.5 });
      // Soft pluck doubles the attack — the mallet hitting the key.
      voices.pluck(synth, note, { beats: 0.5, decay: 0.4, volume: 0.22, pan });

      // ALLEGORY: this note lights its hex cell; ripple washes the neighbors.
      strike(HEX.indexOf(stepCell(s)), hue, 1);

      // Sub-bass root on the half-beat — the whole comb breathes/glows.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.7, decay: 0.5, volume: 0.5 });
        gridGlow = 1;
      }

      voices.hat(synth, { tone: 7200, beats: 0.1, volume: 0.12 });
      beatWave = 1; // kick the sweeping hex wave
    },

    onSim() {
      gridGlow *= 0.9;
      beatWave *= 0.92;
      for (let i = 0; i < cellLight.length; i++) cellLight[i] *= 0.9;
      for (const rp of ripples) {
        rp.age += 0.6; // expands ~0.6 cells per sim tick
        rp.life -= 0.03;
      }
      ripples = ripples.filter((rp) => rp.life > 0 && rp.age < HEX_RADIUS * 2 + 2);
    },

    // Tap = light the nearest hex + ripple + a bell strike. X→hue, Y→pitch.
    onTap({ x, y, synth }) {
      const note = SCALE[Math.min(STEPS - 1, Math.floor((1 - y) * STEPS))];
      const hue = x * 360;
      // Pick the comb cell closest to the tap (in axial-ish screen order).
      const idx = Math.min(HEX.length - 1, Math.floor(x * HEX.length));
      strike(idx, hue, 1.3);
      voices.bell(synth, note, { beats: 1.2, volume: 0.4, pan: x * 2 - 1 });
      voices.pluck(synth, note, { beats: 0.5, volume: 0.3, pan: x * 2 - 1 });
      gridGlow = Math.min(1, gridGlow + 0.5);
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, beatProgress, bursts, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      const bass = band("subBass");
      const glow = Math.max(gridGlow, bass * 1.2);
      const energy = Math.min(2, beatWave * 0.5 + bass * 1.0 + amp * 0.6 + pump * 0.5);

      // Hex cell pixel radius: fill the screen with the comb, breathe on bass.
      const fit = Math.min(w, h) / ((HEX_RADIUS * 2 + 2) * 1.8);
      const cell = fit * (1 + glow * 0.06); // pointy-top circumradius
      const sqrt3 = 1.7320508;
      // Axial → pixel (pointy-top layout).
      const px = (q, r) => cx + cell * sqrt3 * (q + r / 2);
      const py = (q, r) => cy + cell * 1.5 * r;

      // Dark honeycomb backdrop (soft veil, keeps glow trails without full clear).
      ink(6, 5, 14, 165).box(0, 0, w, h);

      // Adaptive detail: at low quality collapse the multi-ring glows to single
      // circles + drop the thin cell outline (READ ctx.quality → hold framerate).
      const rich = quality > 0.6;

      // Whole-grid bass glow: a broad wash behind the comb (the comb breathes).
      if (glow > 0.02) {
        const gR = Math.min(w, h) * (0.35 + glow * 0.28);
        if (rich)
          for (let i = 3; i > 0; i--)
            ink(90, 40, 150, 22 * glow).circle(cx, cy, gR * (i / 3), true);
        else ink(90, 40, 150, 40 * glow).circle(cx, cy, gR * 0.7, true);
      }

      // Draw each hex cell. Base = dim comb wall; add ripple wash + note light.
      const beatSweepAngle = (beatProgress + (1 - beatWave)) * Math.PI * 2;
      for (let i = 0; i < HEX.length; i++) {
        const c = HEX[i];
        const x = px(c.q, c.r), y = py(c.q, c.r);

        // Ripple contribution: sum washes from active ripples at this cell's
        // hex-distance (a ring of glow expands outward from each strike).
        let ripLight = 0, ripHue = cellHue[i];
        for (const rp of ripples) {
          const d = hexDist(c, { q: rp.q, r: rp.r });
          const front = Math.abs(d - rp.age);
          if (front < 0.9) {
            const contrib = (1 - front / 0.9) * rp.life * 0.8;
            if (contrib > ripLight) { ripLight = contrib; ripHue = rp.hue; }
          }
        }

        // Beat wave: a hex-sweep highlight crossing the comb.
        const cellAng = Math.atan2(y - cy, x - cx);
        const sweep = beatWave * Math.max(0, Math.cos(cellAng - beatSweepAngle)) * 0.35;

        const light = Math.min(1.6, cellLight[i] + ripLight + sweep);
        const lit = light > 0.02;
        const hue = lit ? (cellLight[i] > ripLight ? cellHue[i] : ripHue) : 220;
        // Dim comb wall when dark, jewel-bright when struck.
        const sat = lit ? 70 + light * 30 : 45;
        const lum = 16 + light * 60;
        const [r0, g0, b0] = num.hslToRgb(((hue % 360) + 360) % 360, Math.min(100, sat), Math.min(94, lum));

        // Pointy-top hexagon vertices.
        const verts = [];
        for (let k = 0; k < 6; k++) {
          const a = (Math.PI / 180) * (60 * k - 90);
          verts.push([x + Math.cos(a) * cell * 0.92, y + Math.sin(a) * cell * 0.92]);
        }
        const alpha = 160 + light * 95;
        ink(r0, g0, b0, alpha).shape(verts);
        // Bright jewel core + mallet dot when lit — the cell glows.
        if (light > 0.12) {
          const [rc, gc, bc] = num.hslToRgb(((hue % 360) + 360) % 360, 100, 82);
          const coreR = 3 + light * cell * 0.42;
          if (rich) {
            ink(255, 255, 255, 40 + light * 110).line(verts[0][0], verts[0][1], verts[3][0], verts[3][1], 1);
            for (let g = 2; g >= 0; g--)
              ink(rc, gc, bc, (70 + light * 140) * (1 - g * 0.3)).circle(x, y, coreR * (g / 2 + 0.5), true);
          } else {
            // Cheap single-circle jewel core when the host is throttled.
            ink(rc, gc, bc, Math.min(255, 120 + light * 120)).circle(x, y, coreR, true);
          }
        }
      }

      // Tap glow: expanding bright hexes at each engine burst, hue by X.
      for (const b of bursts) {
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 95, 62);
        ink(r0, g0, b0, 180 * b.life).circle(b.x, b.y, b.r, false, 2 + b.life * 3);
        ink(255, 255, 255, 120 * b.life).circle(b.x, b.y, b.r * 0.5, false, 2);
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
