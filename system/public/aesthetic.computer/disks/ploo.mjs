// ploo, 26.07.12
// Concentric SHOCKWAVE / STARBURST bloom — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `ploo 0.5`, the tap/XY "pump", audio polling). This file only describes what
// makes ploo ploo: its cinematic score, its richened voices, and its radial
// shockwave paint. The bass FIRES huge expanding concentric rings (amplitude →
// ring size/brightness); every note BURSTS petal rays (pitch → ray angle + hue,
// high = thin bright rays, low = fat warm ones); a riser SWELLS then the beat
// DROPS a massive gong bloom. The visual IS the score — epic, radial, blooming.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A 16-step cinematic cycle: an 8-beat RISER climbs, then the DROP fires a gong
// on step 0 (and mid-cycle 8), root bass on the strong beats.
const MELODY = [
  "d4", "a3", "f4", "a4", "d5", "a4", "f4", "d4",
  "c4", "g3", "e4", "g4", "c5", "g4", "e4", "c4",
];
const BASS = ["d1", "d1", "d1", "d1", "c1", "c1", "c1", "c1"]; // root, half-time
const DROP_STEPS = new Set([0, 8]); // where the gong DROP lands

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const MEL_PITCHES = MELODY.map(notePitch);
const PITCH_MIN = Math.min(...MEL_PITCHES);
const PITCH_MAX = Math.max(...MEL_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- ploo-specific visual state (the engine owns pump/bursts/rhythm) ---------
let rays = []; // { angle, hue, life, thick, bright } — one per fired note
let shocks = []; // { r, life, hue, bright } — expanding concentric bass rings
let riser = 0; // 0..1 swell within the current bar (climbs then resets on drop)
let dropFlash = 0; // whole-screen bloom, kicked on the DROP beat
let coreGlow = 0; // central point brightness, kicked by bass

const CONFIG = {
  bpm: 120,
  steps: MELODY.length,
  drawBursts: false, // ploo draws its own shockwave tap rings in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.4, feedback: 0.7 });
    },

    // A new UTC beat crossed — fire the score + spawn the matching visuals.
    onBeat({ idx, synth }) {
      const s = ((idx % MELODY.length) + MELODY.length) % MELODY.length;
      const note = MELODY[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / MELODY.length) * Math.PI * 2) * 0.6;
      const isDrop = DROP_STEPS.has(s);

      // ALLEGORY: this note bursts PETAL RAYS radiating outward — angle ∝ step,
      // hue ∝ pitch, thickness inverse to pitch (high = thin bright, low = fat).
      rays.push({
        angle: (s / MELODY.length) * Math.PI * 2,
        hue: 200 + pn * 160, // deep-space blues → magenta/gold at the top
        life: 1,
        thick: 0.02 + (1 - pn) * 0.05, // low notes = fatter rays (still slim)
        bright: 0.4 + pn * 0.6,
      });

      // The DROP: a massive inharmonic gong/bell + sub boom + a big bloom ring.
      if (isDrop) {
        voices.bell(synth, note, { beats: 3.2, volume: 0.42, pan: 0 });
        // Inharmonic partial for gong shimmer (raw synth, detuned metallic).
        synth({ tone: note, type: "square", beats: 2.4, attack: 0.004, decay: 0.9, volume: 0.1, pan: 0.4 });
        synth({ tone: note, type: "triangle", beats: 2.8, attack: 0.006, decay: 0.85, volume: 0.14, pan: -0.4 });
        voices.sub(synth, BASS[0], { beats: 3.0, decay: 0.6, volume: 0.6 });
        dropFlash = 1;
        riser = 0; // reset the swell — the tension released
        shocks.push({ r: 0, life: 1.4, hue: 40, bright: 1 }); // gold DROP shock
        coreGlow = 1.4;
      } else {
        // A rising RISER voice: a swept saw whose tone climbs across the bar as
        // tension builds toward the next drop. Raw synth, rising pitch.
        const barPos = (s % 8) / 8; // 0..1 within the 8-beat riser
        const riseTone = 220 + barPos * 660; // Hz, sweeps up
        synth({ tone: riseTone, type: "sawtooth", beats: 0.9, attack: 0.05, decay: 0.4, volume: 0.08 + barPos * 0.12, pan });
        synth({ tone: riseTone * 1.5, type: "noise-white", beats: 0.6, attack: 0.03, decay: 0.3, volume: 0.03 + barPos * 0.06 });
        // The melody note itself as a bright pluck.
        voices.pluck(synth, note, { beats: 0.8, decay: 0.5, volume: 0.4, pan });
      }

      // Shimmer bell octave feel on the highest steps.
      if (pn > 0.7) voices.bell(synth, note, { beats: 0.6, volume: 0.16, pan: -pan });

      // Sub-bass root on the strong beats — the concentric SHOCKWAVE trigger.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.8, decay: 0.5, volume: 0.5 });
        shocks.push({ r: 0, life: 1, hue: 220, bright: 0.7 }); // blue bass shock
        coreGlow = Math.max(coreGlow, 1);
      }

      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.14 });
    },

    onSim({ amp, band }) {
      // Riser swells with audio energy between drops.
      riser = Math.min(1, riser + 0.006 + amp * 0.02);
      dropFlash *= 0.88;
      coreGlow *= 0.9;

      // Expanding concentric shockwaves — grow + fade.
      const bass = band("subBass");
      for (const sh of shocks) {
        sh.r += 6 + sh.bright * 5 + bass * 8; // bass amplitude drives ring speed
        sh.life -= 0.02;
      }
      shocks = shocks.filter((sh) => sh.life > 0);
      if (shocks.length > 20) shocks = shocks.slice(-20);

      // Petal rays fade slowly so several coexist → a full starburst bloom.
      for (const ry of rays) ry.life -= 0.011;
      rays = rays.filter((ry) => ry.life > 0);
      if (rays.length > 32) rays = rays.slice(-32);
    },

    // Tap = fire a shockwave + petal rays from the tap point + a gong.
    // X→hue, Y→pitch (engine already bumped pump + pushed a burst).
    onTap({ x, y, ex, ey, synth, screen }) {
      const ang = Math.atan2(ey - screen.height / 2, ex - screen.width / 2);
      // A fan of petal rays around the tap direction.
      for (let i = -2; i <= 2; i++) {
        rays.push({
          angle: ang + i * 0.28,
          hue: x * 360,
          life: 1.3,
          thick: 0.02 + (1 - y) * 0.05,
          bright: 1,
        });
      }
      shocks.push({ r: 0, life: 1.4, hue: x * 360, bright: 1 });
      dropFlash = Math.max(dropFlash, 0.7);
      coreGlow = 1.3;
      // Y→pitch: high (top) = high note.
      const note = ["d", "e", "f", "g", "a"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 4));
      voices.bell(synth, note, { beats: 1.6, volume: 0.4, pan: x * 2 - 1 });
      voices.sub(synth, ["d1", "c1", "e1", "g1", "a1"][Math.floor(x * 5)], { beats: 1.4, volume: 0.4 });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, step, beatProgress, bursts, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;
      const maxR = Math.hypot(w, h) * 0.55;

      const bass = band("subBass");
      const energy = Math.min(2, dropFlash * 0.8 + bass * 1.3 + amp * 0.6 + pump * 0.5);

      // Deep-space veil (trails, brightened by the drop flash).
      ink(4, 3, 14, 150 - dropFlash * 90).box(0, 0, w, h);

      // BASS = expanding concentric SHOCKWAVE rings from the center. Thin, bright
      // hollow rings so the concentric bloom reads (not solid discs).
      for (const sh of shocks) {
        const [r0, g0, b0] = num.hslToRgb(((sh.hue % 360) + 360) % 360, 90, 50 + sh.bright * 25);
        const a = 190 * Math.min(1, sh.life);
        // Thin hollow rings (lw 1 → clean circle rasterizer, not the oval polygon
        // path). Three trailing echoes → a concentric ripple, not a solid disc.
        ink(r0, g0, b0, a).circle(cx, cy, sh.r, false);
        ink(255, 255, 255, a * 0.5).circle(cx, cy, sh.r * 0.94, false);
        ink(r0, g0, b0, a * 0.4).circle(cx, cy, sh.r * 0.85, false);
        ink(r0, g0, b0, a * 0.25).circle(cx, cy, sh.r * 0.74, false);
      }

      // NOTE PETAL RAYS — thin radiating spokes that SHOOT outward as they age
      // (starburst). Count scaled by quality (60fps). Reach is bounded so a ray
      // is a slim petal, never a screen-filling wedge.
      const rayMax = Math.min(w, h) * 0.52;
      const RAY_BASE = 24;
      const maxRays = Math.max(6, Math.round(RAY_BASE * quality));
      const innerR = rayMax * 0.06;
      for (const ry of rays.slice(-maxRays)) {
        const shot = 1 - ry.life * ry.life; // 0 (fresh) → 1 (spent): flies out
        const reach = innerR + rayMax * (0.35 + shot * 0.6) * (0.8 + energy * 0.2);
        const [r0, g0, b0] = num.hslToRgb(((ry.hue % 360) + 360) % 360, 100, 55 + ry.bright * 20);
        const a = 110 + 145 * ry.life;
        const halfW = ry.thick * (0.7 + energy * 0.15); // thin: 0.04..0.15 rad
        const midR = innerR + (reach - innerR) * 0.35;
        const c = Math.cos(ry.angle), sn = Math.sin(ry.angle);
        const cl = Math.cos(ry.angle - halfW), snl = Math.sin(ry.angle - halfW);
        const cr = Math.cos(ry.angle + halfW), snr = Math.sin(ry.angle + halfW);
        const base = [cx + c * innerR, cy + sn * innerR];
        const l = [cx + cl * midR, cy + snl * midR];
        const rgt = [cx + cr * midR, cy + snr * midR];
        const tip = [cx + c * reach, cy + sn * reach];
        ink(r0, g0, b0, a).shape([base, l, tip, rgt]);
        // bright hot spine + tip spark
        ink(Math.min(255, r0 + 70), Math.min(255, g0 + 70), Math.min(255, b0 + 70), a)
          .line(base[0], base[1], tip[0], tip[1], 1 + ry.bright * 2);
        ink(255, 255, 255, 220 * ry.life).circle(tip[0], tip[1], 2 + ry.bright * 6 * ry.life, true);
      }

      // RISER indicator — a rising thin arc/ring that fills as tension builds.
      if (riser > 0.02) {
        const rr = maxR * (0.12 + riser * 0.5);
        const [rr0, rg0, rb0] = num.hslToRgb(280 - riser * 60, 90, 40 + riser * 30);
        ink(rr0, rg0, rb0, 60 + riser * 120).circle(cx, cy, rr, false);
        if (riser > 0.4) ink(rr0, rg0, rb0, 40 + riser * 60).circle(cx, cy, rr * 0.97, false);
      }

      // TAP BURSTS (engine-tracked): expanding hue rings.
      for (const b of bursts) {
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 95, 60);
        ink(r0, g0, b0, 200 * b.life).circle(b.x, b.y, b.r, false);
        ink(255, 255, 255, 140 * b.life).circle(b.x, b.y, b.r * 0.7, false);
        ink(r0, g0, b0, 120 * b.life).circle(b.x, b.y, b.r * 0.5, false);
      }

      // The DROP flash — big central bloom + soft screen lift.
      if (dropFlash > 0.02) {
        const bR = maxR * (0.15 + dropFlash * 0.4);
        for (let i = 3; i > 0; i--)
          ink(255, 200 + 55 * dropFlash, 120, 30 * dropFlash).circle(cx, cy, bR * (i / 3), true);
        if (dropFlash > 0.5) blur?.(1);
      }

      // Central core point — the origin everything blooms from.
      const coreR = 3 + coreGlow * 12 + bass * 14 + pump * 6;
      ink(120, 160, 255, 90 + coreGlow * 90).circle(cx, cy, coreR * 1.8, true);
      ink(255, 255, 255, 170 + coreGlow * 80).circle(cx, cy, coreR, true);
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
