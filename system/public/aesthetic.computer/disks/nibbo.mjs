// nibbo, 26.07.12
// Circle-packing gamelan pad — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `nibbo 0.5`, the
// tap/XY "pump", audio polling). This file only describes what makes nibbo nibbo:
// its pentatonic mallet score, its layered inharmonic bell voices, and its
// jewel-like circle packing.
// ALLEGORY: every note SPAWNS a circle that grows into an empty gap until it
// touches a neighbor or edge — pitch sets its final size + hue (high notes = small
// bright circles). The BEAT shimmers a pulse across all circles. BASS breathes a
// gentle global scale. Read the melody as the packing: the frame tessellates.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score (pelog-ish pentatonic mallet run) --------------------------------
const MEL = [
  "e3", "g3", "a3", "b3", "d4", "b3", "a3", "g3",
  "e4", "d4", "b3", "a3", "g3", "a3", "b3", "d4",
];
const BASS = ["e2", "e2", "a1", "a1", "d2", "d2", "g1", "g1"]; // half-time root

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const MEL_PITCHES = MEL.map(notePitch);
const PITCH_MIN = Math.min(...MEL_PITCHES);
const PITCH_MAX = Math.max(...MEL_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);
// midi → hz (A4=440, midi 69), so we can DETUNE a partial for gamelan shimmer.
const noteHz = (n) => 440 * Math.pow(2, (notePitch(n) - 69) / 12);

// --- nibbo-specific visual state (engine owns pump/bursts/rhythm) -----------
// A packed circle: normalized center (0..1), grown radius, target radius, hue,
// brightness, spawn-in progress, and life (loop-fade of the oldest).
let circles = []; // { nx, ny, r, target, hue, bright, grow, life }
let shimmer = 0; // beat pulse across all circles
let breathe = 1; // bass-driven global breathing scale
const BASE_COUNT = 46; // max live circles at quality 1 (scaled by ctx.quality)

// Try to seat a new circle at an empty spot: sample random points, keep the one
// whose gap to the nearest neighbor is largest, then set its target so it grows
// to just touch that neighbor (or the frame). Simple, cheap, satisfying.
function seatCircle(targetR, hue, bright) {
  let best = null;
  let bestGap = -1;
  for (let t = 0; t < 26; t++) {
    const nx = 0.05 + Math.random() * 0.9;
    const ny = 0.05 + Math.random() * 0.9;
    // gap in normalized units to nearest existing circle surface + to edges.
    let gap = Math.min(nx, 1 - nx, ny, 1 - ny);
    for (const c of circles) {
      const d = Math.hypot(nx - c.nx, ny - c.ny) - c.target;
      if (d < gap) gap = d;
    }
    if (gap > bestGap) {
      bestGap = gap;
      best = { nx, ny };
    }
  }
  if (!best) return;
  // Final radius = pitch-desired size, capped so it just kisses its neighbor
  // (tiny margin so jewels touch instead of overlapping — tessellation reads).
  const target = Math.max(0.012, Math.min(targetR, Math.max(0.012, bestGap - 0.004)));
  circles.push({
    nx: best.nx,
    ny: best.ny,
    r: 0.002,
    target,
    hue,
    bright,
    grow: 0.5,
    life: 1,
  });
}

const CONFIG = {
  bpm: 116,
  steps: MEL.length,
  drawBursts: false, // nibbo draws its own tap circles in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.34, feedback: 0.62 });
    },

    // A new UTC beat crossed — fire the mallet + spawn the matching circle.
    onBeat({ idx, synth }) {
      const s = ((idx % MEL.length) + MEL.length) % MEL.length;
      const note = MEL[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / MEL.length) * Math.PI * 2) * 0.6;

      // Gamelan mallet: layered inharmonic bell (slightly detuned) + soft pluck
      // body — bright, metallic, shimmering. High notes get an octave sparkle.
      voices.bell(synth, note, { beats: 1.3, volume: 0.34, pan });
      // Inharmonic detuned partial (~+2.7% ≈ +46¢) — the shimmering "gong"
      // beating that makes bells read as bronze gamelan, not clean sine.
      synth({ tone: noteHz(note) * 1.027, type: "sine", beats: 0.9,
        attack: 0.003, decay: 0.85, volume: 0.12, pan: -pan * 0.5 });
      voices.pluck(synth, note, { beats: 0.5, decay: 0.4, volume: 0.22, pan });
      if (pn > 0.6)
        voices.bell(synth, note, { beats: 0.6, volume: 0.14 * pn, pan: -pan });

      // ALLEGORY: this note packs a new circle — pitch → size (high = small) +
      // hue (low = warm amber, high = cool cyan/violet) + brightness.
      const targetR = 0.16 - pn * 0.11; // high notes = smaller jewels
      const hue = 40 + pn * 260; // amber → cyan/violet
      seatCircle(targetR, hue, 0.4 + pn * 0.6);

      // Sub-bass root on the half-beat — the downbeat you feel + SEE (breathe).
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.7, decay: 0.55, volume: 0.5 });
        breathe = 1.05;
      }

      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.1 });
      shimmer = 1;
    },

    onSim({ quality }) {
      shimmer *= 0.9;
      breathe += (1 - breathe) * 0.06; // ease back to rest
      for (const c of circles) {
        c.r += (c.target - c.r) * 0.12 * c.grow; // grow to fit the gap
        c.life -= 0.0022; // slow loop fade of the oldest
      }
      circles = circles.filter((c) => c.life > 0);
      // Cap live count by adaptive quality — fade oldest first (vector = cheap,
      // but keep counts sane to hold 60fps).
      const max = Math.max(8, Math.round(BASE_COUNT * (quality ?? 1)));
      if (circles.length > max) circles = circles.slice(circles.length - max);
    },

    // Tap = drop a growing circle at the tap point + a bell strike (engine
    // already bumped pump; X→hue/pan, Y→size/brightness).
    onTap({ x, y, synth }) {
      const targetR = 0.06 + (1 - y) * 0.12;
      // Seat directly at the tap: shrink target so it kisses its nearest neighbor.
      let gap = Math.min(x, 1 - x, y, 1 - y);
      for (const c of circles) {
        const d = Math.hypot(x - c.nx, y - c.ny) - c.target;
        if (d < gap) gap = d;
      }
      circles.push({
        nx: x,
        ny: y,
        r: 0.002,
        target: Math.max(0.02, Math.min(targetR, Math.max(0.02, gap))),
        hue: x * 360,
        bright: 0.6 + (1 - y) * 0.4,
        grow: 1.6, // taps grow faster — a satisfying pop
        life: 1.2,
      });
      const note = ["e", "g", "a", "b", "d"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 3));
      voices.bell(synth, note, { beats: 0.9, volume: 0.4, pan: x * 2 - 1 });
      voices.pluck(synth, note, { beats: 0.4, volume: 0.3, pan: x * 2 - 1 });
      shimmer = 1;
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band } = s;
      const { width: w, height: h } = screen;

      const bass = band("subBass");
      const glob = breathe * (1 + bass * 0.06 + pump * 0.02); // global breathing
      const shim = Math.min(1.4, shimmer * 0.8 + amp * 0.5 + pump * 0.3);

      // Dark jewel-box backdrop (subtle veil so packing accretes, not smears).
      ink(6, 8, 16, 255).box(0, 0, w, h);

      const px = (nx) => (nx - 0.5) * glob + 0.5; // breathe about center
      const py = (ny) => (ny - 0.5) * glob + 0.5;
      const scale = Math.min(w, h);

      // Packed circles, oldest first so new jewels sit on top.
      for (const c of circles) {
        const cx = px(c.nx) * w;
        const cy = py(c.ny) * h;
        const rad = c.r * scale * glob;
        if (rad < 0.6) continue;
        const light = Math.min(95, 45 + c.bright * 22 + shim * 18 * c.life);
        const [r0, g0, b0] = num.hslToRgb(((c.hue % 360) + 360) % 360, 82, light);
        const a = Math.max(0, Math.min(255, Math.round(210 * c.life)));
        // Body fill.
        ink(r0, g0, b0, a).circle(cx, cy, rad, true);
        // Facet rim (brighter ring = the "touch" edge that makes packing read).
        const [rr, rg, rb] = num.hslToRgb(((c.hue % 360) + 360) % 360, 90, Math.min(100, light + 22));
        ink(rr, rg, rb, Math.round(a * (0.7 + shim * 0.3))).circle(cx, cy, rad, false, 2);
        // Specular highlight (offset white dot) — jewel glint, brighter on beat.
        const hlR = rad * 0.28;
        if (hlR > 0.8) {
          ink(255, 255, 255, Math.round((90 + shim * 120) * c.life))
            .circle(cx - rad * 0.3, cy - rad * 0.3, hlR, true);
        }
      }

      // Bass breathing ghost ring at center — the felt downbeat, seen.
      if (bass > 0.04 || breathe > 1.005) {
        const bloom = Math.max(bass, (breathe - 1) * 6);
        const bR = scale * (0.06 + bloom * 0.16);
        ink(120, 90, 210, Math.round(50 * bloom)).circle(w / 2, h / 2, bR, false, 2);
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
