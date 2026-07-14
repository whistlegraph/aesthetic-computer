// orbo, 26.07.14
// Gravity-well pad — a thin wrapper over lib/pads.mjs. Each beat drops a
// particle into orbit around screen-center; every OTHER pad on this list
// fakes motion with easing curves, orbo uses real Kepler physics: angular
// velocity ∝ 1/r^1.5, so a close tight orbit is fast and a wide one is slow —
// exactly like the solar system. Pitch sets orbit radius (high note = tight
// fast orbit, low note = wide slow orbit), so the SPEED you see IS the pitch
// you hear, not just its color.
//
// ALLEGORY: tapping doesn't spawn a burst — it drops a new mass into the
// field, right where you touched, and its gravity SLINGSHOTS every nearby
// particle (visibly speeding their orbit + brightening + repitching their
// next pass). The whole screen is a solar system you can drop moons into.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score --------------------------------------------------------------
const SCORE = ["c3", "e3", "g3", "c4", "g3", "e3", "a2", "c3", "e3", "a3", "e3", "c3"];
const TAP_SCALE = ["c3", "d3", "f3", "g3", "a3"]; // pentatonic — x picks the note

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const SCORE_PITCHES = SCORE.map(notePitch);
const PITCH_MIN = Math.min(...SCORE_PITCHES);
const PITCH_MAX = Math.max(...SCORE_PITCHES);
const pitchNorm = (p) => (p - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

const R_MIN = 0.16, R_MAX = 0.94; // orbit radius as a FRACTION of baseR
const BASE_OMEGA = 0.05; // rad/tick at r = baseR

// --- orbo state (engine owns pump/rhythm; this is the solar system) -----
let particles = []; // { angle, rFrac, omega, hue, life, bright }
let rings = []; // shockwave rings from taps: { x, y, r, life }
let centerFlash = 0;

const CONFIG = {
  bpm: 100,
  steps: SCORE.length,
  drawBursts: false, // orbo draws its own gravity rings, not generic bursts
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.26, feedback: 0.48 });
    },

    onBeat({ idx, synth }) {
      const s = ((idx % SCORE.length) + SCORE.length) % SCORE.length;
      const note = SCORE[s];
      const pn = pitchNorm(notePitch(note)); // 0 low .. 1 high
      const dir = s % 2 === 0 ? 1 : -1;
      const rFrac = R_MAX - pn * (R_MAX - R_MIN); // high pitch → tight orbit
      const omega = (BASE_OMEGA / Math.pow(Math.max(0.15, rFrac), 1.5)) * dir;
      particles.push({
        angle: (s / SCORE.length) * Math.PI * 2,
        rFrac,
        omega,
        hue: 260 - pn * 200,
        life: 1,
        bright: 0.4 + pn * 0.5,
      });
      if (particles.length > 60) particles.shift();

      const pan = Math.sin((s / SCORE.length) * Math.PI * 2) * 0.6;
      voices.pluck(synth, note, { beats: 0.55, decay: 0.5, volume: 0.24, pan });
      synth({ tone: note, type: "sine", beats: 0.9, attack: 0.004, decay: 0.8, volume: 0.12, pan: -pan * 0.5 });

      if (s % 4 === 0) {
        voices.sub(synth, "c1", { beats: 1.6, decay: 0.6, volume: 0.4 });
        centerFlash = 1;
      }
      if (s % 3 === 0) voices.hat(synth, { tone: 6800, beats: 0.08, volume: 0.11 });
    },

    onSim() {
      centerFlash *= 0.92;
      for (const p of particles) {
        p.angle += p.omega;
        p.life -= 0.0035;
        p.bright *= 0.995;
      }
      particles = particles.filter((p) => p.life > 0);
      for (const r of rings) {
        r.r += 0.02 + centerFlash * 0.02;
        r.life -= 0.022;
      }
      rings = rings.filter((r) => r.life > 0);
    },

    // Drop a new mass at the tap point, and slingshot every nearby orbit —
    // engine's own pump/burst are unused here (drawBursts:false).
    onTap({ x, y, ex, ey, synth, screen }) {
      const cx = screen.width / 2, cy = screen.height / 2;
      const dCenter = Math.hypot(ex - cx, ey - cy);
      const baseR = Math.min(screen.width, screen.height) * 0.46;
      const rFrac = Math.min(R_MAX, Math.max(R_MIN, dCenter / baseR));
      const angle = Math.atan2(ey - cy, ex - cx);
      const dir = x < 0.5 ? -1 : 1;
      const omega = (BASE_OMEGA / Math.pow(Math.max(0.15, rFrac), 1.5)) * dir;

      particles.push({ angle, rFrac, omega, hue: x * 300, life: 1.4, bright: 1 });
      rings.push({ x: ex, y: ey, r: 0, life: 1 });

      const influence = 160;
      for (const p of particles) {
        const pr = p.rFrac * baseR;
        const px = cx + Math.cos(p.angle) * pr, py = cy + Math.sin(p.angle) * pr;
        const d = Math.hypot(px - ex, py - ey);
        if (d < influence) {
          p.omega *= 1.6;
          p.bright = 1;
          p.hue += 30;
        }
      }

      const note = TAP_SCALE[Math.min(TAP_SCALE.length - 1, Math.floor(x * TAP_SCALE.length))];
      voices.bell(synth, note, { beats: 0.8, volume: 0.34, pan: x * 2 - 1 });
      voices.pluck(synth, note, { beats: 0.4, volume: 0.26, pan: x * 2 - 1 });
      if (dCenter / baseR > 0.7) voices.sub(synth, "a1", { beats: 1, decay: 0.5, volume: 0.3 });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;
      const baseR = Math.min(w, h) * 0.46;

      const bass = band("subBass");
      const energy = Math.min(2, bass * 1.2 + amp * 0.6 + pump * 0.5 + centerFlash * 0.6);

      const veil = 42 - Math.min(24, energy * 16 + pump * 6);
      ink(5, 4, 12, veil).box(0, 0, w, h);

      // the sun — center of the whole system, pulses with bass + tap flash.
      const sunR = 4 + bass * 14 + pump * 6 + centerFlash * 10;
      for (let i = 3; i > 0; i--)
        ink(255, 210, 120, (14 + centerFlash * 40) * (i / 3)).circle(cx, cy, sunR * (i / 3) * 2.2, true);
      ink(255, 240, 200, 200).circle(cx, cy, sunR * 0.6, true);

      for (const r of rings)
        ink(140, 200, 255, 180 * r.life).circle(r.x, r.y, r.r * baseR * 1.2, false, 2 + r.life * 3);

      const maxDraw = Math.max(8, Math.round(40 * quality));
      const trailSteps = quality < 0.6 ? 3 : 6;
      const drawn = particles.slice(-maxDraw);
      for (const p of drawn) {
        const r = p.rFrac * baseR;
        const [r0, g0, b0] = num.hslToRgb(((p.hue % 360) + 360) % 360, 92, 48 + p.bright * 22);
        let px = null, py = null;
        for (let i = trailSteps; i >= 0; i--) {
          const a = p.angle - p.omega * i * 0.8;
          const x = cx + Math.cos(a) * r, y = cy + Math.sin(a) * r;
          if (px !== null) {
            const alpha = (1 - i / trailSteps) * (60 + 150 * p.life);
            ink(r0, g0, b0, alpha).line(px, py, x, y);
          }
          px = x; py = y;
        }
        ink(255, 255, 255, 160 * p.life * p.bright).circle(px, py, 1.5 + p.bright * 3, true);
      }
    },
  },
};

function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
