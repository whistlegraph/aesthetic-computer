// jaxo, 26.07.12
// Confetti-rain footwork pad — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `jaxo 0.2`, the
// tap/XY "pump", audio polling, adaptive quality). This file only describes what
// makes jaxo jaxo: fast 160bpm footwork triplets and a sky of tumbling confetti.
//
// ALLEGORY — every audible note DROPS A BURST of colored confetti (pitch → hue);
// the skittering footwork triplet = rapid confetti bursts stuttering with the
// rhythm; each bass note = a WIND GUST that sweeps all the confetti sideways; the
// beat = a BIG DROP (a wide fountain from the top). Confetti = spinning little
// rects with gravity + flutter (rotation + air drag). Joyful, colorful, kinetic.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score (footwork: 160bpm, triplet skitter) ------------------------------
// steps = 12 → three 4-step groups per bar, driving the triplet feel. Each step
// is a 16th at 160bpm (beatSeconds default = 60/640 ≈ 0.094s — see CONFIG.bpm).
const SUB = ["c2", "c2", 0, "c2", 0, "g1", "c2", 0, "c2", 0, "d2", 0]; // skittering sub kick
const BASS = ["c1", "c1", "g0", "a0", "f0", "g0"]; // wind-gust roots (half-rate)
const BLEEP = ["c5", "e5", "g5", "b5", "d6", "b5", "g5", "e5"]; // chopped stabs
const STEPS = 12;

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const PMIN = 12, PMAX = 84; // c0..c6 range for hue mapping
const pitchNorm = (p) => Math.max(0, Math.min(1, (p - PMIN) / (PMAX - PMIN)));

// --- jaxo-specific visual state (the engine owns pump/bursts/rhythm) ---------
let confetti = []; // { x, y, vx, vy, w, h, rot, spin, r, g, b, life }
let wind = 0; // signed lateral wind, kicked by bass gusts
let drop = 0; // "big drop" flash on the beat

// Spawn a burst of confetti from (x,y). count scales with ctx.quality upstream.
function burstConfetti(x, y, spread, count, hue, up = 0) {
  for (let i = 0; i < count; i++) {
    const ang = (Math.random() - 0.5) * spread;
    const speed = 0.6 + Math.random() * 2.2;
    const [r, g, b] = hueToRgb((hue + (Math.random() - 0.5) * 50 + 360) % 360);
    confetti.push({
      x: x + (Math.random() - 0.5) * 12,
      y,
      vx: Math.sin(ang) * speed,
      vy: -up + Math.cos(ang) * speed * 0.5 - up * Math.random(),
      w: 3 + Math.random() * 5,
      h: 4 + Math.random() * 7,
      rot: Math.random() * Math.PI * 2,
      spin: (Math.random() - 0.5) * 0.5,
      r, g, b,
      life: 1,
    });
  }
}

// Cheap HSL(hue 0-360, full)→RGB — no api needed at spawn time.
function hueToRgb(h) {
  h = ((h % 360) + 360) % 360;
  const c = 1, x = 1 - Math.abs(((h / 60) % 2) - 1);
  let r = 0, g = 0, b = 0;
  if (h < 60) [r, g, b] = [c, x, 0];
  else if (h < 120) [r, g, b] = [x, c, 0];
  else if (h < 180) [r, g, b] = [0, c, x];
  else if (h < 240) [r, g, b] = [0, x, c];
  else if (h < 300) [r, g, b] = [x, 0, c];
  else [r, g, b] = [c, 0, x];
  return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
}

const CONFIG = {
  bpm: 640, // 16th notes at 160bpm → beatSeconds ≈ 0.094s, the skitter clock
  steps: STEPS,
  drawBursts: false, // jaxo draws its own confetti taps in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.18, feedback: 0.4 });
    },

    // A new 16th crossed — fire the footwork score + drop matching confetti.
    onBeat({ idx, screen, synth, quality }) {
      const s = ((idx % STEPS) + STEPS) % STEPS;
      const w = screen?.width || 256;
      const h = screen?.height || 256;
      // Scale confetti-per-burst by adaptive quality (hold 60fps under load).
      const q = quality ?? 1;
      const cnt = (base) => Math.max(2, Math.round(base * q));

      // SKITTERING SUB KICK — the footwork engine room.
      const sub = SUB[s];
      if (sub) {
        const pan = Math.sin((s / STEPS) * Math.PI * 2) * 0.5;
        voices.sub(synth, sub, { beats: 0.7, decay: 0.4, volume: 0.5, pan });
        // Each sub drop = a burst of confetti raining from a column top.
        const bx = w * (0.15 + (s / STEPS) * 0.7);
        burstConfetti(bx, -6, 0.9, cnt(9), pitchNorm(notePitch(sub)) * 300 + 10, 0);
      }

      // CHOPPED BLEEP STAB on the triplet accents (every other step).
      if (s % 2 === 0) {
        const bi = ((s / 2) % BLEEP.length + BLEEP.length) % BLEEP.length;
        const note = BLEEP[bi];
        const pn = pitchNorm(notePitch(note));
        const pan = Math.cos((s / STEPS) * Math.PI * 2) * 0.6;
        voices.pluck(synth, note, { beats: 0.22, decay: 0.35, volume: 0.32, pan });
        // Bleep = a quick confetti spray mid-air (pitch → hue).
        burstConfetti(w * (0.3 + pn * 0.4), h * 0.35, 1.6, cnt(6), pn * 320 + 20, 0.8);
      }

      // HAT ROLLS — dense footwork sizzle on odd/off steps.
      if (s % 3 !== 0) {
        voices.hat(synth, { tone: 7000 + (s % 4) * 900, beats: 0.06, volume: 0.11 });
      }

      // WIND-GUST BASS (half the sub rate) — sweeps all confetti sideways.
      if (s % 4 === 0) {
        const bi = ((Math.floor(idx / 4) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.4, decay: 0.6, volume: 0.42 });
        wind += (Math.random() < 0.5 ? -1 : 1) * (1.4 + Math.random());
      }

      // BIG DROP on the downbeat of each group — a wide fountain from the top.
      if (s === 0) {
        drop = 1;
        voices.bell(synth, "c3", { beats: 0.5, volume: 0.22 });
        burstConfetti(w * 0.5, -6, 2.6, cnt(18), Math.random() * 360, 0);
      }
    },

    onSim({ screen }) {
      const h = screen?.height || 256;
      const w = screen?.width || 256;
      wind *= 0.9; // gust decays back to calm
      drop *= 0.9;
      const floor = h + 20;
      for (const p of confetti) {
        p.vy += 0.09; // gravity
        p.vx += wind * 0.06; // wind sweep
        p.vx *= 0.985; // air drag
        p.x += p.vx;
        p.y += p.vy;
        p.rot += p.spin * (1 + Math.abs(p.vx) * 0.3); // flutter faster when blown
        p.spin += (Math.random() - 0.5) * 0.02;
        if (p.y > floor || p.x < -30 || p.x > w + 30) p.life -= 0.06;
      }
      confetti = confetti.filter((p) => p.life > 0);
      if (confetti.length > 420) confetti = confetti.slice(-420);
    },

    // Tap = a confetti burst from the tap point + a blip (X→hue/pan, Y→pitch).
    onTap({ x, y, ex, ey, synth }) {
      const hue = x * 360;
      burstConfetti(ex, ey, 2.4, 14, hue, 2.4); // upward pop that then rains
      const note = ["c", "e", "g", "b", "d"][Math.floor(x * 5) % 5] + (3 + Math.floor((1 - y) * 4));
      voices.pluck(synth, note, { beats: 0.3, decay: 0.35, volume: 0.5, pan: x * 2 - 1 });
      voices.bell(synth, note, { beats: 0.25, volume: 0.22 * (1 - y), pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen } = api;
      const { pump, quality, band, amp } = s;
      const { width: w, height: h } = screen;

      // Sky wash — dark, slightly warmer when the drop hits (cheap full-frame box).
      const sub = band("subBass");
      ink(6, 5, 14, 235).box(0, 0, w, h);
      if (drop > 0.05) ink(60, 40, 90, 60 * drop).box(0, 0, w, h);

      // Wind streak hint near the bottom (one cheap tinted band, alpha by gust).
      const gust = Math.min(1, Math.abs(wind) * 0.3);
      if (gust > 0.05) {
        const dir = wind > 0 ? 1 : -1;
        ink(80, 120, 200, 40 * gust).box(0, h * 0.7, w, h * 0.3);
        ink(180, 220, 255, 50 * gust).line(
          w * 0.5 - dir * w * 0.3, h * 0.82, w * 0.5 + dir * w * 0.3, h * 0.82, 1,
        );
      }

      // CONFETTI — spinning little rects. Cost scales with ctx.quality: at low
      // quality we draw fewer of the (already time-ordered) particles + skip the
      // second corner-glint pass, holding 60fps under load.
      const list = confetti;
      const stride = quality < 0.6 ? 2 : 1; // draw every Nth when starved
      const glints = quality > 0.75;
      for (let i = list.length - 1; i >= 0; i -= stride) {
        const p = list[i];
        const a = Math.round(255 * Math.min(1, p.life * 1.4));
        // A spinning quad (4 corners rotated about center) reads as tumbling paper.
        const c = Math.cos(p.rot), sn = Math.sin(p.rot);
        const hw = p.w * 0.5, hh = p.h * 0.5;
        const c0x = p.x + (-hw * c - -hh * sn), c0y = p.y + (-hw * sn + -hh * c);
        const c1x = p.x + (hw * c - -hh * sn), c1y = p.y + (hw * sn + -hh * c);
        const c2x = p.x + (hw * c - hh * sn), c2y = p.y + (hw * sn + hh * c);
        const c3x = p.x + (-hw * c - hh * sn), c3y = p.y + (-hw * sn + hh * c);
        ink(p.r, p.g, p.b, a).shape([
          [c0x, c0y], [c1x, c1y], [c2x, c2y], [c3x, c3y],
        ]);
        if (glints)
          ink(255, 255, 255, a * 0.5).line(c0x, c0y, c1x, c1y, 1); // top-edge glint
      }

      // TAP / big-drop pulse dot at center-top so the "drop" origin reads.
      const pulse = drop * 10 + pump * 5 + sub * 8 + amp * 4;
      if (pulse > 0.5) {
        ink(255, 240, 200, 120 + drop * 120).circle(w * 0.5, 6, 3 + pulse, true);
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
