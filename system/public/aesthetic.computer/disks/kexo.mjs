// kexo, 26.07.14
// Falling/shattering glass-shard pad — thin wrapper over lib/pads.mjs (UTC
// beat grid, params[0] rate override e.g. `kexo 0.5`, tap/XY pump, audio
// polling). This file only says what makes kexo kexo: shards of light that
// fall on the beat and SHATTER outward on tap, piling up on a floor that
// keeps them.
//
// ALLEGORY: sound is glass. Each beat drops one sliver from above — a soft
// clink drifting down under real gravity, bouncing once, settling. Each tap
// SHATTERS a chord's worth of shards outward from the touch point in real
// arcs. The pile at the bottom is never cleared — it's the visible trace of
// everyone who has played kexo, glinting under the beat.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score --------------------------------------------------------------
const SCORE = ["c5", "d5", "e5", "g5", "a5", "g5", "e5", "d5"];
const BASS_NOTE = "c2";
const CHORD_NOTES = ["c5", "d5", "e5", "g5", "a5"];

// --- physics --------------------------------------------------------------
const GRAVITY = 0.00095;
const DRAG = 0.992;
const BOUNCE = 0.4;
const SLIDE_FRICTION = 0.86;
const SETTLE_EPS = 0.00025;
const FLOOR_NY = 0.92;
const HARD_CAP = 220; // bounded pile — new shards recycle the oldest

let shards = []; // { nx, ny, vx, vy, rot, vrot, len, hue, bounced, settled }
let shock = 0; // decaying impact flash from taps, felt in the floor glow

function spawnShard(nx, ny, vx, vy, hue, len) {
  shards.push({
    nx, ny, vx, vy, hue, len,
    rot: Math.random() * Math.PI,
    vrot: (Math.random() - 0.5) * 0.3,
    bounced: false,
    settled: false,
  });
  if (shards.length > HARD_CAP) shards.shift();
}

const CONFIG = {
  bpm: 112,
  steps: SCORE.length,
  drawBursts: false, // kexo draws its own tap glow, tinted with the shatter
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.4 });
    },

    // One shard drips from above on every beat — a slow, sparse glass rain.
    onBeat({ idx, synth }) {
      const s = ((idx % SCORE.length) + SCORE.length) % SCORE.length;
      const note = SCORE[s];
      const nx = 0.15 + (s / SCORE.length) * 0.7 + (Math.random() - 0.5) * 0.06;
      const pan = nx * 2 - 1;
      spawnShard(
        nx, -0.02,
        (Math.random() - 0.5) * 0.0015, 0.0008 + Math.random() * 0.0006,
        190 + (s / SCORE.length) * 140, 10 + Math.random() * 8,
      );
      voices.pluck(synth, note, { beats: 0.5, decay: 0.5, volume: 0.22, pan });
      synth({ tone: note, type: "sine", beats: 0.9, attack: 0.005, decay: 0.7, volume: 0.1, pan: pan * 0.6 });
      if (s % 4 === 0) voices.sub(synth, BASS_NOTE, { beats: 1.6, decay: 0.5, volume: 0.35 });
    },

    onSim() {
      shock *= 0.9;
      for (const sh of shards) {
        if (sh.settled) continue;
        sh.vy += GRAVITY;
        sh.vx *= DRAG;
        sh.vy *= DRAG;
        sh.nx += sh.vx;
        sh.ny += sh.vy;
        sh.rot += sh.vrot;
        sh.vrot *= 0.985;
        if (sh.ny >= FLOOR_NY) {
          sh.ny = FLOOR_NY;
          if (!sh.bounced) {
            sh.vy = -sh.vy * BOUNCE;
            sh.vx *= 0.7;
            sh.bounced = true;
          } else {
            sh.vy = 0;
            sh.vx *= SLIDE_FRICTION;
            if (Math.abs(sh.vx) < SETTLE_EPS) {
              sh.vx = 0;
              sh.settled = true;
            }
          }
        }
        if (sh.nx < 0 || sh.nx > 1) sh.vx *= -0.5;
      }
    },

    // Tap = shatter. X picks the chord root, Y adds sub-weight for a low
    // touch — a fistful of shards flies outward and falls back real.
    onTap({ x, y, synth }) {
      const root = Math.floor(x * CHORD_NOTES.length);
      const chord = [root, (root + 2) % CHORD_NOTES.length, (root + 4) % CHORD_NOTES.length];
      const n = 10 + Math.floor(Math.random() * 6);
      for (let i = 0; i < n; i++) {
        const a = Math.random() * Math.PI * 2;
        const speed = 0.006 + Math.random() * 0.014;
        spawnShard(
          x, y,
          Math.cos(a) * speed, Math.sin(a) * speed - 0.004,
          x * 360 + Math.random() * 30, 8 + Math.random() * 14,
        );
      }
      shock = 1;
      chord.forEach((ci, i) => {
        voices.bell(synth, CHORD_NOTES[ci], { beats: 0.8, volume: 0.3 - i * 0.06, pan: x * 2 - 1 });
      });
      voices.hat(synth, { tone: 8200, beats: 0.08, volume: 0.14 });
      voices.sub(synth, BASS_NOTE, { beats: 0.4, volume: 0.22 + (1 - y) * 0.15 });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      // phosphor veil — trails a little more when the frame is cheap.
      const veil = quality > 0.6 ? 26 : 40;
      ink(6, 8, 16, veil).box(0, 0, w, h);

      const bass = band("subBass");
      const glow = Math.min(1, shock * 0.8 + bass * 0.6 + pump * 0.3);
      const floorY = h * FLOOR_NY;
      ink(90, 160, 255, 30 + glow * 90).box(0, floorY, w, h - floorY);
      ink(160, 210, 255, 60 + glow * 140).box(0, floorY - 1, w, 2);

      // ADAPTIVE QUALITY: only the render is capped — physics stays full so
      // the pile is always correct, we just draw less of it when cheap.
      const maxDrawn = Math.max(40, Math.round(HARD_CAP * quality));
      const drawn = shards.length > maxDrawn ? shards.slice(-maxDrawn) : shards;
      const glint = quality > 0.55;

      for (const sh of drawn) {
        const x = sh.nx * w;
        const y = sh.ny * h;
        const hl = sh.len * (0.5 + quality * 0.5);
        const dx = Math.cos(sh.rot) * hl;
        const dy = Math.sin(sh.rot) * hl;
        const [r0, g0, b0] = num.hslToRgb(((sh.hue % 360) + 360) % 360, 80, sh.settled ? 55 : 68);
        const a = sh.settled ? 130 : 200;
        ink(r0, g0, b0, a).line(x - dx, y - dy, x + dx, y + dy);
        if (glint && !sh.settled) ink(255, 255, 255, 160).circle(x + dx, y + dy, 1.4, true);
      }

      for (const b of s.bursts) {
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 90, 70);
        ink(r0, g0, b0, 160 * b.life).circle(b.x, b.y, b.r, false, 2 + b.life * 3);
      }

      if (glow > 0.55) blur?.(1);
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
