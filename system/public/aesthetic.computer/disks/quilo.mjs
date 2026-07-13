// quilo, 26.07.12
// Matrix glyph-rain × chiptune arps — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `quilo 0.5`, the tap/XY "pump", audio polling). This file only describes what
// makes quilo quilo: its chiptune score, its 8-bit voices, and its cascade.
// ALLEGORY: columns of falling green glyphs. Every arp note LIGHTS A COLUMN —
// pitch → column x (low = left, high = right); high notes fall faster & brighter.
// The bass drives the whole rain's SPEED + DENSITY. The beat flashes a sweep.
// A viewer reads the arp directly as the lit columns cascading down.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score (chiptune, D-minor pentatonic-ish arp) ---------------------------
const ARP = [
  "d3", "a3", "d4", "f4", "a4", "f4", "d4", "a3",
  "c3", "g3", "c4", "e4", "g4", "e4", "c4", "g3",
];
const BASS = ["d2", "d2", "c2", "c2", "a1", "a1", "f1", "f1"]; // half-time root
const TYPES = ["square", "triangle", "square", "sawtooth"]; // pulse-ish rotation

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

// Glyph alphabet — katakana-ish + ASCII for a legible cascade.
const GLYPHS = "アカサタナabcdef0123456789+*=<>#$%".split("");

// --- quilo-specific visual state (engine owns pump/bursts/rhythm) ------------
// Each column is a single falling STREAK: a head position + a short glyph tail.
// This keeps the per-frame `write` count bounded (~COLS × TAIL) so it holds
// 60fps — the cascade legibility comes from the head being white-hot.
const COLS = 20; // logical column count
let columns = []; // per-column streaks
let rainSpeed = 1; // global fall speed, driven by bass
let flashSweep = 0; // beat flash sweep 0..1
let flashY = 0;

// Tiny deterministic hash for frame-seeded glyph choice (no Math.random flake).
function hash(a, b) {
  let h = (a * 374761393 + b * 668265263) >>> 0;
  h = (h ^ (h >>> 13)) * 1274126177;
  return (h >>> 0) / 4294967296;
}
let frame = 0;
const TAIL = 13; // glyphs trailing each streak head

function ensureColumns() {
  if (columns.length === COLS) return;
  columns = [];
  for (let c = 0; c < COLS; c++) {
    const glyphs = [];
    for (let i = 0; i < TAIL; i++) glyphs.push(Math.floor(hash(c, i + 11) * GLYPHS.length));
    columns.push({
      head: hash(c, 3) * 2 - 1, // normalized -1..1 head position
      glyphs,
      lit: 0, // brightness pulse when this column's arp note fires
      litHue: 120,
      speed: 0.7 + hash(c, 5) * 0.6,
    });
  }
}

const CONFIG = {
  bpm: 128,
  steps: ARP.length,
  drawBursts: false, // quilo draws its own column highlights
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.14, feedback: 0.35 });
      ensureColumns();
    },

    // A new UTC beat crossed — fire the chiptune arp + light the matching column.
    onBeat({ idx, synth }) {
      ensureColumns();
      const s = ((idx % ARP.length) + ARP.length) % ARP.length;
      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const col = Math.min(COLS - 1, Math.floor(pn * (COLS - 1) + 0.5)); // pitch → x
      const pan = (col / (COLS - 1)) * 2 - 1;

      // 8-bit chiptune arp: raw square/triangle/pulse, hard attack, short decay.
      const type = TYPES[s % TYPES.length];
      synth({ tone: note, type, beats: 0.42, attack: 0.001, decay: 0.55, volume: 0.34, pan });
      // Octave-up sparkle blip on the high half of the arp.
      if (pn > 0.5)
        synth({ tone: note, type: "square", beats: 0.18, attack: 0.001, decay: 0.4, volume: 0.12 * pn, pan: -pan });

      // ALLEGORY: light the pitch's column bright; high notes = faster + brighter.
      const cc = columns[col];
      cc.lit = 1;
      cc.litHue = 110 + pn * 40; // green → chartreuse for highs
      cc.speed = 0.9 + pn * 1.5; // high notes fall faster
      cc.head = -1.1; // relaunch its streak from the top

      // Sub-bass root on the half-beat — drives the whole rain's speed + density.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.3, decay: 0.5, volume: 0.42 });
        rainSpeed = 2.1; // bass surges the rain
      }

      // Beat flash sweep + chiptune hat.
      voices.hat(synth, { tone: 9000, beats: 0.08, volume: 0.12 });
      flashSweep = 1;
      flashY = 0;
    },

    onSim() {
      frame++;
      rainSpeed += (1 - rainSpeed) * 0.06; // relax toward baseline between bass hits
      flashSweep *= 0.9;
      const fall = 0.02 * rainSpeed;
      for (const c of columns) {
        c.lit *= 0.9;
        c.head += fall * c.speed;
        if (c.head > 1.3) {
          c.head = -1.15;
          // Reshuffle a couple glyphs so the streak reads as living text.
          const k = frame % TAIL;
          c.glyphs[k] = Math.floor(hash(c.speed * 1000 + frame, k) * GLYPHS.length);
        }
      }
    },

    // Tap = ignite a bright column burst at tap x + an arcade blip.
    onTap({ x, y, synth }) {
      ensureColumns();
      const col = Math.min(COLS - 1, Math.max(0, Math.floor(x * COLS)));
      const cc = columns[col];
      cc.lit = 1.7; // over-bright ignite
      cc.litHue = 90 + (1 - y) * 80; // green→yellow toward top
      cc.speed = 1.6 + (1 - y) * 1.6;
      cc.head = -1.1;
      // Light the two neighbours too, for a visible splash.
      for (const d of [-1, 1]) {
        const nb = columns[col + d];
        if (nb) { nb.lit = Math.max(nb.lit, 0.9); nb.litHue = cc.litHue; }
      }
      // Arcade blip: rising square + saw tick + hat.
      const note = ["c", "e", "g", "a", "c"][Math.floor(x * 5)] + (3 + Math.floor((1 - y) * 3));
      synth({ tone: note, type: "square", beats: 0.22, attack: 0.001, decay: 0.5, volume: 0.42, pan: x * 2 - 1 });
      synth({ tone: note, type: "sawtooth", beats: 0.12, attack: 0.001, decay: 0.3, volume: 0.18, pan: x * 2 - 1 });
      voices.hat(synth, { tone: 7000, beats: 0.06, volume: 0.14 });
    },

    onPaint(api, s) {
      const { ink, screen, num, write } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      ensureColumns();

      // Trailing veil (near-black with a green tint) → glyph trails linger.
      ink(0, 8, 3, 240).box(0, 0, w, h);

      const bass = band("subBass") + band("lowMid") * 0.5;
      const energy = Math.min(1.5, bass * 1.4 + amp * 0.5 + pump * 0.4);

      const colW = w / COLS;
      const glyphSize = Math.max(1, Math.round(Math.min(3, colW / 6)));
      const cellH = Math.max(6, glyphSize * 7);
      // ADAPTIVE QUALITY: tail length drawn scales with quality (fewer glyphs
      // per streak when the engine asks us to cut cost).
      const tailN = Math.max(4, Math.round(TAIL * quality));

      for (let ci = 0; ci < COLS; ci++) {
        const c = columns[ci];
        const cx = ci * colW + colW * 0.5;
        const lit = c.lit;
        const headPix = ((c.head + 1.2) / 2.4) * (h + cellH) - cellH;

        // Lit column highlight bar (the ARP note reads as a bright column).
        if (lit > 0.08) {
          const [r, g, b] = num.hslToRgb(((c.litHue % 360) + 360) % 360, 90, 55);
          ink(r, g, b, Math.min(150, Math.round(85 * lit))).box(ci * colW, 0, Math.max(1, colW - 1), h);
          ink(255, 255, 255, Math.min(110, Math.round(70 * lit))).box(Math.round(cx) - 1, 0, 2, h);
        }

        // The streak: white-hot head, fading green glyph tail upward.
        for (let t = 0; t < tailN; t++) {
          const py = headPix - t * cellH;
          if (py < -cellH || py > h) continue;
          const trail = 1 - t / tailN;
          const glyph = GLYPHS[c.glyphs[(t + ((frame >> 2) % TAIL)) % TAIL]];
          const gx = cx - glyphSize * 2;
          if (t === 0) {
            // White-hot cascade leader.
            ink(220, 255, 230, 255).write(glyph, { x: gx, y: py, size: glyphSize });
          } else {
            const light = Math.min(90, 40 + trail * 30 + lit * 20 + energy * 10);
            const [r, g, b] = num.hslToRgb(
              ((c.litHue - trail * 8) % 360 + 360) % 360,
              Math.min(100, 75 + lit * 20),
              Math.min(100, light),
            );
            const a = Math.max(0, Math.min(255, Math.round((70 + trail * 170) * (0.55 + lit * 0.5))));
            ink(r, g, b, a).write(glyph, { x: gx, y: py, size: glyphSize });
          }
        }
      }

      // BEAT FLASH SWEEP — a bright horizontal band travels top→bottom.
      if (flashSweep > 0.06) {
        flashY += h * 0.06 * (0.6 + rainSpeed * 0.4);
        if (flashY > h) flashY = 0;
        const bandH = Math.max(2, Math.round(h * 0.05));
        ink(120, 255, 160, Math.min(140, Math.round(120 * flashSweep))).box(0, flashY, w, bandH);
        ink(255, 255, 255, Math.min(90, Math.round(70 * flashSweep))).box(0, flashY, w, 2);
      }

      // Bass floor glow — density surges pulse the bottom edge.
      if (rainSpeed > 1.2 || bass > 0.1) {
        const gl = Math.min(1, (rainSpeed - 1) * 0.8 + bass);
        const gh = Math.round(h * 0.08 * gl);
        if (gh > 0) ink(40, 200, 90, Math.round(45 * gl)).box(0, h - gh, w, gh);
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
