// dorf, 26.07.12
// Rorschach inkblot × gong/bell tolls — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `dorf 1.2`, the tap/XY "pump", audio polling). dorf only describes what makes
// dorf dorf: its slow resonant toll score, its layered inharmonic gong/bell
// voices, and its mirror-symmetric ink paint.
//
// ALLEGORY: every audible note BLOTS a fresh mirror-symmetric mark on the
// parchment — bilateral (left↔right) symmetry across the vertical axis, like an
// unfolded Rorschach card. pitch → hue + size (low tolls = big dark spreading
// blooms, high = small bright specks); the bass = a deep symmetric central
// swell; the beat = a symmetric ripple. Blots feather and fade slowly to loop.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A slow, spacious sequence of tolls. Low roots ring long (big dark blooms),
// climbing to bright specks and back — meditative, resonant.
const TOLL = [
  "a2", "e3", "a3", "d3",
  "g2", "d3", "b3", "e3",
  "a2", "a3", "e4", "c4",
  "f2", "c3", "a3", "g3",
];
const BASS = ["a1", "a1", "g1", "g1", "a1", "a1", "f1", "f1"]; // deep half-time swell

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const TOLL_PITCHES = TOLL.map(notePitch);
const PITCH_MIN = Math.min(...TOLL_PITCHES);
const PITCH_MAX = Math.max(...TOLL_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);
// MIDI-ish semitone → Hz (A4 = 440 at semitone 69), for inharmonic gong partials
// (tone accepts a raw Hz number as well as a note string).
const noteHz = (n) => 440 * Math.pow(2, (notePitch(n) - 69) / 12);

// --- dorf-specific visual state (the engine owns pump/bursts/rhythm) ---------
// Each blot: normalized position off the mirror axis (bx 0..1 = distance from
// center, by 0..1 = vertical), hue, size, darkness, life, feather layer count.
let blots = []; // { bx, by, hue, size, dark, life, spread }
let bassSwell = 0; // deep central symmetric swell, kicked by each bass note
let ripple = 0; // symmetric ripple ring, kicked each beat

const CONFIG = {
  bpm: 66, // slow, meditative tolls
  steps: TOLL.length,
  drawBursts: false, // dorf draws its own symmetric tap blooms in onPaint
  hooks: {
    onBoot({ sound }) {
      // Long, wet room — gongs need space to ring out.
      sound.room?.set?.({ enabled: true, mix: 0.42, feedback: 0.72 });
    },

    // A new UTC beat crossed — toll the score + spawn the matching blot.
    onBeat({ idx, synth }) {
      const s = ((idx % TOLL.length) + TOLL.length) % TOLL.length;
      const note = TOLL[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / TOLL.length) * Math.PI * 2) * 0.5;

      // GONG/BELL TOLL: layered + inharmonic. Low pitches ring a longer, darker
      // gong (extra detuned partial); high pitches are bright short bells.
      const beats = 2.6 - pn * 1.4; // low tolls ring longer
      voices.bell(synth, note, { beats, volume: 0.34, pan });
      // Inharmonic gong partial (~2.76× fundamental, a classic bell/gong ratio)
      // → that shimmering non-integer metal beat. Raw Hz keeps it inharmonic.
      voices.bell(synth, noteHz(note) * 2.76, {
        beats: beats * 0.7,
        volume: 0.13 * (1 - pn),
        pan: -pan,
      });
      // Occasional airy flute overtone on the brightest tolls.
      if (pn > 0.65) voices.flute(synth, note, { beats: 1.4, volume: 0.12, pan });

      // ALLEGORY: this toll blots a fresh mirror-symmetric mark.
      blots.push({
        bx: 0.05 + (0.15 + pn * 0.4) + Math.sin(s * 1.7) * 0.12, // off-axis distance
        by: 0.28 + ((s * 0.618) % 1) * 0.44, // scattered vertically
        hue: 220 - pn * 200, // low = deep indigo/violet, high = warm amber
        size: 1 - pn * 0.72, // low = big spreading, high = small speck
        dark: 0.55 + (1 - pn) * 0.45, // low = darker
        life: 1,
        spread: 0,
      });

      // Deep symmetric swell on the half-beat — the toll you feel + SEE.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 3.0, decay: 0.6, volume: 0.5 });
        bassSwell = 1;
      }
      ripple = 1;
    },

    onSim() {
      bassSwell *= 0.965;
      ripple *= 0.94;
      for (const b of blots) {
        b.life -= 0.0055; // slow fade to loop
        b.spread = Math.min(1, b.spread + 0.03 * b.size); // feather outward
      }
      blots = blots.filter((b) => b.life > 0);
      if (blots.length > 26) blots = blots.slice(-26);
    },

    // Tap = a symmetric blot PAIR at the tap point (+ its mirror) + a bell toll.
    // X → hue + off-axis distance, Y → pitch + vertical.
    onTap({ x, y, synth }) {
      blots.push({
        bx: Math.abs(x - 0.5) * 2, // distance from mirror axis
        by: y,
        hue: 260 - y * 220,
        size: 0.9 - y * 0.5,
        dark: 0.85,
        life: 1.25,
        spread: 0,
      });
      bassSwell = Math.max(bassSwell, 0.6);
      const note =
        ["a", "d", "e", "g", "a"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 3));
      voices.bell(synth, note, { beats: 2.2, volume: 0.4, pan: x * 2 - 1 });
      voices.sub(synth, note, { beats: 1.6, volume: 0.22 * (1 - y) });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, beatProgress, bursts, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      const bass = band("subBass");
      const energy = Math.min(2, ripple * 0.5 + bass * 1.0 + amp * 0.5 + pump * 0.4);

      // Dark parchment wash — slight veil so blots fade slowly (loop).
      ink(8, 7, 12, 30).box(0, 0, w, h);

      // BASS = deep symmetric central swell (mirrored inherently — it's centered).
      if (bassSwell > 0.01 || bass > 0.04) {
        const sw = Math.max(bassSwell, bass * 1.1);
        const bR = Math.min(w, h) * (0.12 + sw * 0.3);
        const [br, bg, bb] = num.hslToRgb(258, 55, 22 + sw * 14);
        for (let i = 4; i > 0; i--)
          ink(br, bg, bb, 20 * sw).circle(cx, cy, bR * (i / 4), true);
      }

      // BLOTS — each drawn twice, mirrored across the vertical axis. Feather
      // layers AND live count scale with quality so we hold 60fps: cut the
      // fillrate-heavy nested circles first when the engine floors quality.
      const featherMax = quality < 0.6 ? 2 : quality < 0.85 ? 4 : 6;
      const liveMax = Math.max(6, Math.round(22 * quality));
      const halfW = w / 2;
      for (const b of blots.slice(-liveMax)) {
        // Off-axis pixel offset from center, and vertical position.
        const off = b.bx * halfW * 0.86;
        const py = b.by * h;
        const baseR = Math.min(w, h) * (0.03 + b.size * 0.14) * (1 + b.spread * 0.9);
        const [r0, g0, b0] = num.hslToRgb(
          ((b.hue % 360) + 360) % 360,
          70,
          18 + (1 - b.dark) * 42,
        );
        const layers = Math.max(2, Math.round(featherMax * (0.5 + b.size)));
        for (const mirror of [-1, 1]) {
          const bxp = cx + mirror * off;
          // Feathered bloom: nested translucent circles = soft ink spread.
          for (let i = layers; i > 0; i--) {
            const t = i / layers;
            const rr = baseR * t;
            const a = Math.round(46 * b.life * b.dark * (1 - t * 0.5));
            ink(r0, g0, b0, a).circle(bxp, py, rr, true);
          }
          // Dark ink core.
          ink(
            Math.round(r0 * 0.5),
            Math.round(g0 * 0.5),
            Math.round(b0 * 0.5),
            Math.round(120 * b.life * b.dark),
          ).circle(bxp, py, baseR * 0.34, true);
        }
      }

      // BEAT = symmetric ripple ring (centered → inherently mirrored).
      if (ripple > 0.05) {
        const rr = Math.min(w, h) * (0.15 + (1 - ripple) * 0.5) * (1 + energy * 0.2);
        const [rr0, rg0, rb0] = num.hslToRgb(250, 40, 60);
        ink(rr0, rg0, rb0, Math.round(90 * ripple)).circle(cx, cy, rr, false, 2);
      }

      // TAP BLOOMS (engine-tracked bursts) — drawn as symmetric pairs.
      for (const bst of bursts) {
        const [r0, g0, b0] = num.hslToRgb(((bst.hue % 360) + 360) % 360, 60, 55);
        const off = Math.abs(bst.x - cx);
        for (const mirror of [-1, 1]) {
          const bxp = cx + mirror * off;
          ink(r0, g0, b0, Math.round(150 * bst.life)).circle(bxp, bst.y, bst.r, false, 2);
          ink(r0, g0, b0, Math.round(80 * bst.life)).circle(bxp, bst.y, bst.r * 0.4, true);
        }
      }

      // Faint central mirror seam + heart — the fold line of the card.
      const seamA = 30 + ripple * 40 + bass * 30;
      ink(120, 110, 150, seamA).line(cx, 0, cx, h, 1);
      const heartR = 3 + ripple * 8 + bass * 10 + pump * 5 + bassSwell * 6;
      ink(180, 160, 220, 100 + ripple * 80).circle(cx, cy, heartR, true);
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
