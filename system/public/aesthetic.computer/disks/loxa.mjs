// loxa, 26.07.12
// Lightning-storm pad — a thin wrapper over lib/pads.mjs (the shared pad engine:
// UTC-clock beat grid, `params[0]` rate override e.g. `loxa 0.5`, the tap/XY
// "pump", audio polling). This file only describes what makes loxa loxa: its
// electric zap score, its richened FM/noise voices, and its recursive-branch
// lightning paint.
// ALLEGORY: every note STRIKES a jagged forked bolt — pitch picks the strike
// x-position, the branch angle, and the hue (electric blue→violet). Bolts flash
// bright then fade fast (strobe-like), leaving a brief afterglow. The BASS is a
// thunder FLASH filling the whole storm sky + a low rumble; the BEAT is the
// strike. The whole screen is a button: tap strikes a bolt from the tap point
// with a zap (X→pan/hue, Y→pitch). Recursion depth + branch + live-bolt counts
// scale by ctx.quality to hold 60fps.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A driving E-minor storm loop. Each entry is a note that fires a bolt strike;
// low octaves double as thunder (flash + rumble + a felt downbeat).
const PATTERN = [
  "e3", "b3", "e4", "g4", // bar 1 — strikes climb
  "e2", "b3", "d4", "b3", // bar 2 — thunder on the 1
  "a3", "e4", "a4", "e4", // bar 3
  "e2", "g4", "b4", "g4", // bar 4 — thunder + high crackle
];
const BAR = 4; // beats per bar

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const oct = parseInt(n.slice(-1), 10) || 3;
  const pc = NOTE_SEMI[n[0]] ?? 0;
  return (oct + 1) * 12 + pc; // ~36..76 across this loop
}
const PATTERN_PITCHES = PATTERN.map(notePitch);
const PITCH_MIN = Math.min(...PATTERN_PITCHES);
const PITCH_MAX = Math.max(...PATTERN_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// Deterministic hash → 0..1 jitter (avoids flaky raw Math.random in capture).
function hash01(x) {
  const s = Math.sin(x * 127.1 + 311.7) * 43758.5453;
  return s - Math.floor(s);
}

// --- loxa-specific visual state (the engine owns pump/bursts/rhythm) ---------
let bolts = []; // active strikes: { x0,y0, x1,y1, hue, seed, life, born, width }
let flash = 0; // thunder FLASH envelope (0..1) — whole-sky white/violet fill
let rumble = 0; // eased low rumble level → sky glow
let boltId = 0;

// Strike a forked bolt from (nx0,ny0) to (nx1,ny1) in normalized 0..1 coords.
function strike(nx0, ny0, nx1, ny1, hue, width) {
  bolts.push({
    x0: nx0, y0: ny0,
    x1: nx1, y1: ny1,
    hue,
    seed: boltId++ * 13.37 + 1,
    life: 1,
    born: 1,
    width: width ?? 1,
  });
  if (bolts.length > 10) bolts = bolts.slice(-10);
}

const CONFIG = {
  bpm: 138,
  steps: PATTERN.length,
  drawBursts: false, // loxa draws its own tap zaps as bolts
  hooks: {
    onBoot({ sound }) {
      bolts = [];
      flash = 0;
      rumble = 0;
      boltId = 0;
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.55 });
    },

    // A new UTC beat crossed — fire the score + strike the matching bolt.
    onBeat({ idx, synth, pump }) {
      const s = ((idx % PATTERN.length) + PATTERN.length) % PATTERN.length;
      const note = PATTERN[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / PATTERN.length) * Math.PI * 2) * 0.7;
      const boost = 1 + Math.min(1, pump * 0.3);
      const isThunder = (parseInt(note.slice(-1), 10) || 3) <= 2;

      // ELECTRIC ZAP: detuned saw + square with a fast pitch-drop = FM-ish stab.
      const p = notePitch(note);
      const hz = 440 * Math.pow(2, (p - 69) / 12);
      synth({ type: "sawtooth", tone: hz, beats: 0.18, attack: 0.001, decay: 0.32, volume: 0.34 * boost, pan });
      synth({ type: "sawtooth", tone: hz * 1.01, beats: 0.16, attack: 0.001, decay: 0.3, volume: 0.24 * boost, pan: -pan });
      synth({ type: "square", tone: hz * 2, beats: 0.09, attack: 0.001, decay: 0.2, volume: 0.14 * boost, pan });
      // Fast pitch-drop tail (the "zap"): a bright chirp at the top followed a hair
      // later by an octave-down thump — reads as a rapid downward electric slide.
      synth({ type: "sawtooth", tone: hz * 1.5, beats: 0.05, attack: 0.001, decay: 0.18, volume: 0.16 * boost, pan });
      synth({ type: "sawtooth", tone: hz * 0.5, beats: 0.16, attack: 0.02, decay: 0.28, volume: 0.2 * boost, pan });
      // Crackle: a burst of filtered white noise = the spark's air ionizing.
      synth({ type: "noise-white", tone: 3000 + pn * 6000, beats: 0.08, attack: 0.001, decay: 0.16, volume: 0.16 * boost, pan: -pan });

      // ALLEGORY: strike a forked bolt. Pitch → strike x-position + hue; the
      // branch geometry comes from the bolt seed. High notes = tighter, whiter.
      const sx = 0.12 + pn * 0.76; // strike x follows pitch left→right
      const hue = 210 + pn * 70; // electric blue(210)→violet(280)
      strike(sx, -0.02, sx + (hash01(s + 0.5) - 0.5) * 0.3, 1.02, hue, 1 + (1 - pn) * 0.8);

      if (isThunder) {
        // THUNDER — a flash filling the sky + a low rumble you feel.
        voices.sub(synth, note, { beats: 2.0, decay: 0.5, volume: 0.62 * boost });
        synth({ type: "noise-brown", tone: 90, beats: 1.4, attack: 0.02, decay: 0.6, volume: 0.3 * boost });
        flash = 1;
        rumble = Math.min(1.5, rumble + 0.9);
      }
    },

    onSim({ band }) {
      flash *= 0.82; // fast strobe-like decay
      if (flash < 0.002) flash = 0;
      rumble *= 0.94;
      if (!Number.isFinite(rumble)) rumble = 0;
      rumble = Math.max(rumble, band("subBass") * 1.2);
      for (const b of bolts) {
        b.born *= 0.6; // the initial hot pop fades almost instantly
        b.life -= 0.045; // bolts fade fast (strobe-like) but leave a visible afterglow
      }
      bolts = bolts.filter((b) => b.life > 0);
    },

    // Tap = strike a bolt from the tap point + a zap. X→pan/hue, Y→pitch.
    onTap({ x, y, ex, ey, synth, screen, isDraw, pump }) {
      const nx = ex / (screen?.width || 1);
      const ny = ey / (screen?.height || 1);
      const hue = 200 + x * 90; // blue→violet by X
      // Y→pitch (top = higher). E-minor-ish scale for taps.
      const scale = ["e", "g", "a", "b", "d"];
      const deg = Math.floor((1 - y) * scale.length);
      const oct = 2 + Math.floor((1 - y) * 3);
      const note = scale[Math.max(0, Math.min(scale.length - 1, deg))] + oct;
      const p = notePitch(note);
      const hz = 440 * Math.pow(2, (p - 69) / 12);
      const pan = x * 2 - 1;
      const boost = 1 + Math.min(1, pump * 0.3);

      // Strike DOWN from the tap point (a bolt reaching to ground below).
      strike(nx, ny, nx + (hash01(nx * 7) - 0.5) * 0.24, 1.04, hue, 1.4);
      // And a smaller fork reaching UP toward the sky.
      strike(nx, ny, nx + (hash01(ny * 5) - 0.5) * 0.24, -0.04, hue, 0.9);

      // The zap.
      synth({ type: "sawtooth", tone: hz, beats: isDraw ? 0.12 : 0.2, attack: 0.001, decay: 0.3, volume: (isDraw ? 0.22 : 0.4) * boost, pan });
      synth({ type: "square", tone: hz * 2, beats: 0.08, attack: 0.001, decay: 0.18, volume: 0.14 * boost, pan: -pan });
      synth({ type: "sawtooth", tone: hz * 1.5, beats: 0.05, attack: 0.001, decay: 0.16, volume: 0.14 * boost, pan });
      synth({ type: "sawtooth", tone: hz * 0.5, beats: 0.16, attack: 0.02, decay: 0.26, volume: 0.2 * boost, pan });
      synth({ type: "noise-white", tone: 4000 + (1 - y) * 5000, beats: 0.08, attack: 0.001, decay: 0.15, volume: 0.18 * boost, pan });
      if (!isDraw && y > 0.55) {
        // Low tap → a little thunder.
        voices.sub(synth, note, { beats: 1.2, decay: 0.4, volume: 0.45 * boost });
        flash = Math.max(flash, 0.6);
        rumble = Math.min(1.5, rumble + 0.5);
      }
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, amp, quality } = s;
      const { width: w, height: h } = screen;
      const q = quality ?? 1;

      // Adaptive cost: recursion depth, segment count, and how many bolts we draw
      // all scale with quality so we hold 60fps.
      const depth = q >= 0.9 ? 4 : q >= 0.7 ? 3 : q >= 0.5 ? 2 : 1;
      const segs = q >= 0.9 ? 9 : q >= 0.7 ? 7 : q >= 0.5 ? 5 : 4;
      const maxDraw = q >= 0.7 ? bolts.length : Math.max(1, Math.round(bolts.length * q));

      // --- Storm sky: dark, with a rumble glow + thunder flash veil ------------
      const glow = Math.min(1.5, rumble * 0.7 + amp * 0.4 + pump * 0.2);
      // Deep storm gradient (a few horizontal bands, cheap).
      const bands = 6;
      for (let i = 0; i < bands; i++) {
        const t = i / bands;
        const r = 6 + t * 10 + glow * 14;
        const g = 6 + t * 8 + glow * 10;
        const b = 16 + t * 22 + glow * 30;
        ink(r, g, b, 255).box(0, (h * i) / bands, w, h / bands + 1);
      }

      // THUNDER FLASH — a bright violet-white veil filling the whole sky.
      if (flash > 0.01) {
        const [fr, fg, fb] = num.hslToRgb(270, 40, 70);
        ink(fr, fg, fb, Math.round(200 * flash)).box(0, 0, w, h);
        ink(255, 255, 255, Math.round(120 * flash * flash)).box(0, 0, w, h);
      }

      // --- Bolts: recursive midpoint-displacement forked lightning ------------
      // Draw the most recent bolts first-priority; older ones fade out.
      const drawList = bolts.slice(-maxDraw);
      for (const bolt of drawList) {
        const [r0, g0, b0] = num.hslToRgb(((bolt.hue % 360) + 360) % 360, 90, 62);
        drawBolt(
          api,
          bolt.x0 * w, bolt.y0 * h,
          bolt.x1 * w, bolt.y1 * h,
          depth, segs,
          bolt, r0, g0, b0,
          Math.min(w, h),
        );
      }

      // Bright flash veil / bloom when a big strike or thunder lands.
      if (flash > 0.5 || pump > 1.4) blur?.(1);

      // --- Ground horizon shimmer (a felt "strike zone") ----------------------
      const horiz = h * 0.9;
      const [hr, hg, hb] = num.hslToRgb(255, 60, 30 + glow * 20);
      ink(hr, hg, hb, 60 + glow * 60).box(0, horiz, w, h - horiz);
    },
  },
};

// Recursive midpoint-displacement bolt with forking branches. Draws a jagged
// polyline from (x0,y0) to (x1,y1); at each recursion level it can spawn a child
// branch veering off at an angle. `depth`/`segs` are quality-scaled by caller.
function drawBolt(api, x0, y0, x1, y1, depth, segs, bolt, r, g, b, minWH) {
  const { ink } = api;
  const life = bolt.life;
  const born = bolt.born;
  // Build the jagged path via midpoint displacement over `segs` points.
  const pts = jaggedPath(x0, y0, x1, y1, segs, bolt.seed, minWH * 0.06);
  const coreA = Math.round(240 * life + 15 * born);
  const haloA = Math.round(55 * life);
  const glowA = Math.round(120 * life);
  const haloW = (5 + bolt.width * 6) * (0.7 + born);
  const glowW = (3 + bolt.width * 3) * (0.7 + born);
  const coreW = Math.max(2, bolt.width * (born > 0.3 ? 3 : 2));

  for (let i = 0; i < pts.length - 1; i++) {
    const [ax, ay] = pts[i];
    const [bx, by] = pts[i + 1];
    // Three passes: a soft wide halo, a saturated glow, then the hot white core.
    ink(r, g, b, haloA).line(ax, ay, bx, by, haloW);
    ink(r, g, b, glowA).line(ax, ay, bx, by, glowW);
    ink(
      Math.min(255, r + 130),
      Math.min(255, g + 130),
      Math.min(255, b + 110),
      coreA,
    ).line(ax, ay, bx, by, coreW);
  }

  // FORK: at deeper recursion, split a branch from a mid-vertex.
  if (depth > 0 && pts.length > 2) {
    const bi = 1 + Math.floor(hash01(bolt.seed + depth * 3.1) * (pts.length - 2));
    const [bx, by] = pts[bi];
    const [ex, ey] = pts[pts.length - 1];
    // Branch angle veers off (pitch already set base hue); jitter by seed.
    const ang = Math.atan2(ey - by, ex - bx) + (hash01(bolt.seed + depth) - 0.5) * 1.4;
    const len = Math.hypot(ex - bx, ey - by) * (0.35 + hash01(bolt.seed + depth * 2) * 0.35);
    const cx = bx + Math.cos(ang) * len;
    const cy = by + Math.sin(ang) * len;
    drawBolt(
      api, bx, by, cx, cy,
      depth - 1, Math.max(3, segs - 2),
      { ...bolt, width: bolt.width * 0.6, seed: bolt.seed * 1.7 + 5 },
      r, g, b, minWH,
    );
  }
}

// Midpoint-displacement jagged path: subdivide the segment `segs` times, jitter
// each interior point perpendicular to the line by a seeded amount.
function jaggedPath(x0, y0, x1, y1, segs, seed, amp) {
  const pts = [[x0, y0]];
  const dx = x1 - x0, dy = y1 - y0;
  const len = Math.hypot(dx, dy) || 1;
  const nx = -dy / len, ny = dx / len; // perpendicular unit
  for (let i = 1; i < segs; i++) {
    const t = i / segs;
    const jitter = (hash01(seed + i * 2.3) - 0.5) * 2 * amp * (0.4 + Math.sin(t * Math.PI) * 0.9);
    pts.push([x0 + dx * t + nx * jitter, y0 + dy * t + ny * jitter]);
  }
  pts.push([x1, y1]);
  return pts;
}

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before the engine
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
