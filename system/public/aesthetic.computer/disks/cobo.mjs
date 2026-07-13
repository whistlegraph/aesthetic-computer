// cobo, 26.07.12
// Fireworks × orchestral-hit stabs — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `cobo 0.5`, the tap/XY "pump", audio polling, adaptive ctx.quality). This
// file only describes what makes cobo cobo: its skyward score, its cinematic
// stab, and its particle night-sky.
// ALLEGORY: each beat LAUNCHES a firework shell that arcs up and BURSTS into a
// shower of sparks (pitch → burst height + hue; high notes = high, bright, small
// bursts). The bass = the BOOM + a screen-flash on the big shells. Sparks fall
// with gravity and leave glittering trails. Every audible note fires a shell —
// the visual IS the score. Particle count scales by ctx.quality to hold 60fps.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A rising/falling firework melody in D minor. Higher notes → higher, brighter,
// smaller bursts; low notes → low, wide, warm bursts. Even steps drop a big
// shell (boom + flash); odd steps are quick sparklers.
const MELODY = [
  "d3", "a3", "d4", "f4", "a4", "f4", "d4", "a3",
  "g3", "d4", "g4", "a4", "c5", "a4", "g4", "d4",
];
const BOOM = ["d2", "d2", "bb1", "bb1", "g1", "g1", "a1", "a1"]; // half-time root
const BIG_EVERY = 2; // a big shell every N steps

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(b?)(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[3], 10) + 1) * 12 + NOTE_SEMI[m[1]] + (m[2] ? -1 : 0);
}
const MEL_PITCHES = MELODY.map(notePitch);
const PITCH_MIN = Math.min(...MEL_PITCHES);
const PITCH_MAX = Math.max(...MEL_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- cobo-specific visual state (the engine owns pump/bursts/rhythm) --------
let shells = []; // rising rockets: { x, y, vx, vy, hue, big, life, fuse }
let sparks = []; // exploded sparks: { x, y, vx, vy, hue, life, bright }
let flash = 0; // screen-flash on big shells, decays
let boomGlow = 0; // low horizon glow, kicked by the boom
const GRAVITY = 0.06; // downward pull per sim frame (screen units)
const DRAG = 0.985; // spark air-drag
const BASE_SPARKS = 44; // sparks per burst at quality 1
const MAX_SPARKS = 440; // live-particle cap at quality 1

// Launch a shell from the ground toward a target height (0 low .. 1 high sky).
function launch(w, h, x0, height, hue, big, quality) {
  const targetY = h * (0.68 - height * 0.5); // higher pitch → higher in frame
  const groundY = h + 6;
  const rise = groundY - targetY;
  // vy chosen so the shell (under GRAVITY) tops out near targetY.
  const vy = -Math.sqrt(2 * GRAVITY * rise);
  shells.push({
    x: x0,
    y: groundY,
    vx: (Math.random() - 0.5) * w * 0.02,
    vy,
    hue,
    big,
    life: 1,
    targetY,
  });
}

// Burst a shell into a shower of sparks. Count scales with quality.
function burst(x, y, hue, big, quality) {
  const n = Math.max(10, Math.round(BASE_SPARKS * (big ? 1.5 : 1) * quality));
  const spd = (big ? 3.4 : 2.2) * (1 + Math.random() * 0.25);
  for (let i = 0; i < n; i++) {
    const a = (i / n) * Math.PI * 2 + Math.random() * 0.3;
    const s = spd * (0.4 + Math.random() * 0.6);
    sparks.push({
      x,
      y,
      vx: Math.cos(a) * s,
      vy: Math.sin(a) * s - 0.6,
      hue: hue + (Math.random() - 0.5) * 40,
      life: 0.8 + Math.random() * 0.4,
      bright: 0.6 + Math.random() * 0.4,
    });
  }
}

const CONFIG = {
  bpm: 120,
  steps: MELODY.length,
  drawBursts: false, // cobo draws its own firework visuals
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.55 });
    },

    // A new UTC beat crossed — fire the score: a cinematic orchestral-hit stab
    // and the matching skyward firework.
    onBeat({ idx, synth, screen }) {
      const s = ((idx % MELODY.length) + MELODY.length) % MELODY.length;
      const note = MELODY[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / MELODY.length) * Math.PI * 2) * 0.7;
      const big = s % BIG_EVERY === 0;

      // ORCHESTRAL-HIT STAB — a big layered stab with a fast attack: harp pluck
      // body + shimmering bell top + a detuned saw brass + a sub thump. Cinematic.
      voices.pluck(synth, note, { beats: 0.7, attack: 0.001, decay: 0.5, volume: 0.5, pan });
      voices.bell(synth, note, { beats: 0.55, volume: 0.22 + pn * 0.14, pan: -pan });
      // Detuned-saw brass swell (two saws, fast attack) — the "hit" grit.
      synth({ tone: note, type: "sawtooth", beats: 0.35, attack: 0.001, decay: 0.4, volume: 0.16, pan: pan * 0.5 });
      synth({ tone: note, type: "sawtooth", beats: 0.35, attack: 0.001, decay: 0.4, volume: 0.13, pan: pan * 0.5 + 0.06 });
      voices.sub(synth, BOOM[((Math.floor(idx / 2) % BOOM.length) + BOOM.length) % BOOM.length], { beats: 0.6, attack: 0.001, decay: 0.4, volume: 0.28 });
      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.13 + pn * 0.08 }); // sizzle

      // BIG SHELL = the BOOM + screen-flash + deep sub.
      if (big) {
        const bi = ((Math.floor(idx / BIG_EVERY) % BOOM.length) + BOOM.length) % BOOM.length;
        voices.sub(synth, BOOM[bi], { beats: 1.6, attack: 0.002, decay: 0.55, volume: 0.55 });
        flash = Math.min(1, flash + 0.6);
        boomGlow = 1;
      }

      // ALLEGORY: launch a firework shell. Pitch → height + hue (high = high,
      // bright, blue/white; low = low, warm, red/gold). Big shells are wider.
      const w = screen?.width || 256, h = screen?.height || 256;
      const x0 = w * (0.2 + (s / MELODY.length) * 0.6 + (Math.random() - 0.5) * 0.1);
      const hue = 20 + pn * 300; // warm-low → cool-high
      launch(w, h, x0, pn, hue, big, 1);
    },

    onSim({ quality }) {
      flash *= 0.86;
      boomGlow *= 0.9;

      // Advance rising shells; burst when they top out (vy turns downward past
      // their target height) or their fuse runs out.
      for (const sh of shells) {
        sh.x += sh.vx;
        sh.y += sh.vy;
        sh.vy += GRAVITY;
        sh.life -= 0.01;
      }
      const stillRising = [];
      for (const sh of shells) {
        if (sh.vy >= 0 || sh.y <= sh.targetY || sh.life <= 0) {
          burst(sh.x, sh.y, sh.hue, sh.big, quality ?? 1);
          if (sh.big) boomGlow = Math.max(boomGlow, 0.7);
        } else stillRising.push(sh);
      }
      shells = stillRising;

      // Sparks fall with gravity + air drag, twinkle out.
      for (const sp of sparks) {
        sp.x += sp.vx;
        sp.y += sp.vy;
        sp.vy += GRAVITY;
        sp.vx *= DRAG;
        sp.vy *= DRAG;
        sp.life -= 0.018;
      }
      sparks = sparks.filter((sp) => sp.life > 0);

      // Live-particle cap scales with quality (hold 60fps).
      const cap = Math.max(120, Math.round(MAX_SPARKS * (quality ?? 1)));
      if (sparks.length > cap) sparks = sparks.slice(-cap);
    },

    // Tap = launch an EXTRA firework from the tap point + a stab. X→pan/hue,
    // Y→height/pitch (engine already bumped pump + pushed its burst).
    onTap({ x, y, ex, ey, synth, screen }) {
      const w = screen?.width || 256, h = screen?.height || 256;
      const height = 1 - y; // top of screen → high, bright
      const hue = x * 360;
      launch(w, h, ex, height, hue, y < 0.5, 1);
      // Immediate spark puff at the tap so it reads instantly.
      burst(ex, ey, hue, y < 0.4, 1);

      const note = ["d", "f", "g", "a", "c"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 4));
      voices.pluck(synth, note, { beats: 0.6, attack: 0.001, volume: 0.55, pan: x * 2 - 1 });
      voices.bell(synth, note, { beats: 0.4, volume: 0.3 * (1 - y), pan: x * 2 - 1 });
      synth({ tone: note, type: "sawtooth", beats: 0.3, attack: 0.001, decay: 0.4, volume: 0.14, pan: x * 2 - 1 });
      if (y > 0.5) voices.sub(synth, "d1", { beats: 0.9, volume: 0.5 }); // low tap = boom
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      const bass = band("subBass");
      // NIGHT SKY — dark veil (trails glitter through the fade). Stronger fade so
      // one veil box reads as motion-blur without extra passes.
      ink(3, 4, 14, 210).box(0, 0, w, h);
      // Horizon boom-glow (warm band rising from the ground on big shells) — a
      // single band (was 3) to keep fixed per-frame cost low at native res.
      const glow = Math.max(boomGlow, bass * 1.1);
      if (glow > 0.02)
        ink(120 + glow * 60, 40, 20, Math.round(40 * glow)).box(0, h * 0.62, w, h * 0.38);

      // RISING SHELLS — bright climbing dots with a fading tail.
      for (const sh of shells) {
        const [r, g, b] = num.hslToRgb(((sh.hue % 360) + 360) % 360, 90, 70);
        ink(r, g, b, 180).line(sh.x, sh.y, sh.x - sh.vx * 3, sh.y - sh.vy * 3, sh.big ? 2 : 1);
        ink(255, 255, 240, 230).circle(sh.x, sh.y, sh.big ? 2.5 : 1.6, true);
      }

      // SPARKS — the shower. Bright twinkling head; glitter trail only on the
      // freshest sparks (cheap: one hslToRgb + one draw for most). Twinkle
      // brightness pulses so the sky sparkles.
      const trails = quality > 0.75; // drop glitter trails when perf is tight
      for (const sp of sparks) {
        const l = 40 + sp.bright * 40 * sp.life;
        const [r, g, b] = num.hslToRgb(((sp.hue % 360) + 360) % 360, 95, Math.min(90, l + sp.life * 20));
        const a = Math.round(200 * sp.life);
        if (trails && sp.life > 0.6)
          ink(r, g, b, Math.round(a * 0.5)).line(sp.x, sp.y, sp.x - sp.vx * 2, sp.y - sp.vy * 2, 1);
        // twinkling head — occasional white flicker
        const tw = sp.bright * (0.7 + 0.3 * Math.sin(sp.life * 40));
        ink(Math.min(255, r + 90 * tw), Math.min(255, g + 90 * tw), Math.min(255, b + 90 * tw), Math.min(255, a + 40))
          .circle(sp.x, sp.y, 1 + sp.life * 1.6, true);
      }

      // TAP PUMP — a subtle upward shimmer haze when the user is interacting.
      if (pump > 0.05 || amp > 0.1) {
        const e = Math.min(1, pump * 0.3 + amp * 0.5);
        ink(180, 200, 255, Math.round(20 * e)).box(0, 0, w, h);
      }

      // BIG-SHELL SCREEN FLASH — the boom lights up the whole sky, briefly.
      if (flash > 0.02) ink(255, 250, 235, Math.round(150 * flash)).box(0, 0, w, h);
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
