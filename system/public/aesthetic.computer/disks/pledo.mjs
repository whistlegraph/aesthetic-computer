// pledo, 26.07.12
// Dot-matrix / LED-grid step SEQUENCER × house piano + four-on-the-floor — a
// thin wrapper over lib/pads.mjs (the shared pad engine: UTC-clock beat grid,
// `params[0]` rate override e.g. `pledo 0.25`, tap/XY "pump", audio polling).
// ALLEGORY: the glowing LED grid IS the sequencer. A playhead COLUMN sweeps
// left→right locked to the beat (ctx.step = which column; ctx.beatProgress =
// smooth sub-column glide). Lit cells in each column ARE the notes scheduled
// there — rows = pitches (top = high, bottom = low). The bottom row is the kick
// LED strip pulsing four-on-the-floor. You READ the sequence right off the grid.
// Chunky retro LEDs (rounded dots + bloom). Vector grid = cheap → holds 60fps.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Grid dimensions --------------------------------------------------------
const STEPS = 16; // columns (one 4/4 bar of 16th notes)
const ROWS = 8; // pitch rows (top row = high, bottom row = kick strip)

// --- Score: a classic-house A-minor pattern read as grid rows ---------------
// Rows are indexed 0 (top / highest) .. ROWS-1 (bottom / kick). Each row maps
// to a pitch; the sequencer PATTERN below lights cells and those exact notes
// fire. Piano rows (0..ROWS-2) hold a Rhodes/piano stab voicing on the off-beat
// "and" of each beat (the house piano bounce); the bottom row is the kick.
const ROW_NOTES = [
  "e5", // 0 top  — shimmer
  "c5", // 1
  "a4", // 2
  "g4", // 3
  "e4", // 4
  "c4", // 5
  "a3", // 6  — chord bass
  "a1", // 7 bottom = KICK strip (pitch only used for the sub thump)
];
const KICK_ROW = ROWS - 1;

// PATTERN[col] = array of row indices lit in that column.
// Kick (row 7) on every downbeat → four-on-the-floor (cols 0,4,8,12).
// Piano stabs land on the classic house off-beat "&" (cols 2,6,10,14) as an
// Am chord voicing, with a small ii/IV colour move across the bar so it grooves.
const PATTERN = Array.from({ length: STEPS }, () => []);
// four-on-the-floor kick
for (let c = 0; c < STEPS; c += 4) PATTERN[c].push(KICK_ROW);
// house piano off-beat stabs (rows spelling the chord), colour-shifted per hit
const STAB_COLS = [2, 6, 10, 14];
const CHORD_A = [2, 4, 5, 6]; // a4 e4 c4 a3  (Am)
const CHORD_F = [1, 3, 5, 6]; // c5 g4 c4 a3  (F add)
const CHORD_G = [0, 3, 4, 6]; // e5 g4 e4 a3  (G-ish)
const CHORD_E = [2, 3, 4, 5]; // a4 g4 e4 c4  (Em/Am7)
const STAB_CHORDS = [CHORD_A, CHORD_F, CHORD_G, CHORD_E];
STAB_COLS.forEach((c, i) => PATTERN[c].push(...STAB_CHORDS[i]));

// Row → pitch class → hue (top rows warm/bright, low rows cool/deep).
const rowHue = (r) => 20 + (1 - r / (ROWS - 1)) * 280; // 20 (low) .. 300 (high)

// --- pledo-specific visual state (engine owns pump/bursts/rhythm) -----------
// cells[col][row] = glow 0..1. onBeat lights a column's pattern cells; taps
// light individual cells; glow decays each sim so lit notes shimmer + fade.
const cells = Array.from({ length: STEPS }, () => new Float32Array(ROWS));
let kickFlash = 0; // whole-bottom-strip flash on each kick
let barFlash = 0; // faint frame flash on the downbeat

const CONFIG = {
  bpm: 124, // classic house tempo
  steps: STEPS,
  drawBursts: false, // pledo draws its own tap glow in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.22, feedback: 0.5 });
    },

    // A new UTC beat crossed → this is the playhead landing on column `s`.
    // Fire exactly the notes lit in that column, and light those same cells so
    // the grid always shows the truth of what's sounding.
    onBeat({ idx, synth }) {
      const s = ((idx % STEPS) + STEPS) % STEPS;
      const lit = PATTERN[s];
      if (!lit.length) return;

      const isKick = lit.includes(KICK_ROW);
      if (isKick) {
        // Four-on-the-floor: warm sub kick + the bottom LED strip flashes.
        voices.sub(synth, ROW_NOTES[KICK_ROW], { beats: 0.9, decay: 0.42, volume: 0.6 });
        cells[s][KICK_ROW] = 1;
        kickFlash = 1;
        barFlash = Math.max(barFlash, 0.6);
        // Closed hat on every downbeat, open hat on the back half of the bar.
        voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.14 });
        if (s === 8) voices.hat(synth, { tone: 6500, beats: 0.4, volume: 0.12 }); // open hat
      }

      // House piano stab: the lit chord rows ring together as a Rhodes-ish stab
      // (bell shimmer + a detuned sine/triangle body), panned by column.
      const chordRows = lit.filter((r) => r !== KICK_ROW);
      if (chordRows.length) {
        const pan = ((s / STEPS) * 2 - 1) * 0.5;
        chordRows.forEach((r, i) => {
          const note = ROW_NOTES[r];
          const v = 0.16 - i * 0.02;
          // Rhodes/piano body: detuned sine + triangle (warm, quick decay).
          voices.bell(synth, note, { beats: 0.5, volume: v, pan });
          synth({ tone: note, type: "triangle", beats: 0.42, attack: 0.003, decay: 0.6, volume: v * 0.7, pan });
          synth({ tone: note, type: "sine", beats: 0.5, attack: 0.002, decay: 0.55, volume: v * 0.5, pan: -pan });
          cells[s][r] = 1;
        });
        // Offbeat open-hat tick riding the stabs → that house shuffle.
        voices.hat(synth, { tone: 5000, beats: 0.16, volume: 0.08 });
      }
    },

    onSim() {
      kickFlash *= 0.86;
      barFlash *= 0.9;
      for (let c = 0; c < STEPS; c++)
        for (let r = 0; r < ROWS; r++) if (cells[c][r] > 0.001) cells[c][r] *= 0.9;
    },

    // Tap = light the cell under the finger + play that cell's note.
    // X → column (which step), Y → row (which pitch). You're punching the
    // sequencer with your finger; the grid remembers the glow, the note rings.
    onTap({ x, y, synth }) {
      const col = Math.max(0, Math.min(STEPS - 1, Math.floor(x * STEPS)));
      const row = Math.max(0, Math.min(ROWS - 1, Math.floor(y * ROWS)));
      cells[col][row] = 1.4;
      const note = ROW_NOTES[row];
      const pan = x * 2 - 1;
      if (row === KICK_ROW) {
        voices.sub(synth, note, { beats: 0.9, decay: 0.42, volume: 0.7 });
        kickFlash = 1;
      } else {
        voices.bell(synth, note, { beats: 0.6, volume: 0.4, pan });
        synth({ tone: note, type: "triangle", beats: 0.5, attack: 0.003, decay: 0.6, volume: 0.24, pan });
      }
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { step, beatProgress, pump, amp, quality } = s;
      const { width: w, height: h } = screen;

      // Dark LED-panel backdrop (faint blue veil = phosphor persistence).
      ink(4, 5, 12).box(0, 0, w, h);
      if (barFlash > 0.02) ink(30, 20, 50, 40 * barFlash).box(0, 0, w, h);

      // Grid geometry: centred panel with margins. Dot-matrix LEDs are SQUARE
      // boxes (cheap immediate-mode fills) so the whole 16×8 panel is nearly
      // free — only the handful of LIT cells pay for round bloom halos. This is
      // what lets pledo hold 60fps native (the socket floor was the killer).
      const marginX = w * 0.06;
      const marginY = h * 0.08;
      const gw = w - marginX * 2;
      const gh = h - marginY * 2;
      const cellW = gw / STEPS;
      const cellH = gh / ROWS;
      const dotS = Math.min(cellW, cellH) * 0.6; // LED box side
      const half = dotS / 2;
      const energy = Math.min(1.6, amp * 1.2 + pump * 0.4);

      // Precompute row colors + centers once (not per cell).
      const rowRGB = [];
      const rowCY = [];
      for (let r = 0; r < ROWS; r++) {
        rowRGB[r] = num.hslToRgb(rowHue(r), 90, 55);
        rowCY[r] = marginY + r * cellH + cellH / 2;
      }

      // Smooth playhead position (column + sub-column glide) for a swept beam.
      const playX = marginX + (step + beatProgress) * cellW + cellW / 2;

      // --- Playhead COLUMN beam (draw first, behind the dots) ---------------
      // A bright vertical bar sweeping L→R IS the beat. Bloom scales with q.
      const bloomLayers = quality > 0.7 ? 3 : quality > 0.5 ? 2 : 1;
      for (let i = bloomLayers; i > 0; i--) {
        const bw = cellW * (0.5 + i * 0.55);
        const a = (18 + energy * 22) / i;
        ink(120, 180, 255, a).box(playX - bw / 2, marginY - cellH * 0.3, bw, gh + cellH * 0.6);
      }
      ink(200, 230, 255, 60 + energy * 40).box(playX - 1.5, marginY, 3, gh);

      // --- The LED cells (square boxes = cheap; lit cells get round bloom) ---
      const litHalo = quality > 0.5; // only draw halos when we have headroom
      const bigHalo = quality > 0.78;
      for (let r = 0; r < ROWS; r++) {
        const cy = rowCY[r];
        const isKickRow = r === KICK_ROW;
        const [hr, hg, hb] = rowRGB[r];
        for (let c = 0; c < STEPS; c++) {
          const cx = marginX + c * cellW + cellW / 2;
          const glow = cells[c][r];
          const under = c === step; // this column is under the playhead now
          const scheduled = PATTERN[c].includes(r);

          if (glow < 0.02) {
            // Unlit socket = dim square LED. Scheduled cells glow a touch
            // brighter so the empty grid still reads as a sequencer pattern;
            // the cell under the playhead gets a scheduled-note tint.
            const lit = scheduled ? (under ? 40 : 24) : 12;
            ink(lit * 0.5, lit * 0.6, lit + 8, 255).box(cx - half, cy - half, dotS, dotS);
            continue;
          }

          // Lit LED: round bloom halo (only when q allows) + square core +
          // white hot-spot. Playhead column boosts brightness.
          const g = Math.min(1.5, glow + (under ? 0.5 : 0));
          const kb = isKickRow ? 1 + kickFlash * 0.6 : 1;
          if (litHalo) {
            ink(hr, hg, hb, 55 * g * kb).circle(cx, cy, dotS * (0.9 + g * 0.35), true);
            if (bigHalo) ink(hr, hg, hb, 26 * g).circle(cx, cy, dotS * 1.4, true);
          }
          const cs = dotS * (1 + g * 0.18);
          ink(hr, hg, hb, Math.min(255, 210 * g)).box(cx - cs / 2, cy - cs / 2, cs, cs); // core
          const hs = dotS * 0.42;
          ink(255, 255, 255, Math.min(255, 210 * g)).box(cx - hs / 2, cy - hs / 2, hs, hs); // hot-spot
        }
      }

      // --- Bottom kick strip underline (reads as the sequencer's kick lane) --
      if (kickFlash > 0.03) {
        const ly = rowCY[KICK_ROW] + dotS * 0.9;
        ink(255, 120, 60, 120 * kickFlash).box(marginX, ly, gw, 2 + kickFlash * 3);
      }

      // --- Playhead head glow at the top rail (the "now" marker) ------------
      ink(220, 240, 255, 160).circle(playX, marginY - cellH * 0.25, 3 + energy * 4, true);
      ink(120, 180, 255, 90 + energy * 60).circle(playX, marginY - cellH * 0.25, 6 + energy * 8, true);

      if (energy > 1.2 && quality > 0.85) blur?.(1);
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
