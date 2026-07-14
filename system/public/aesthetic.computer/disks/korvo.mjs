// korvo, 26.07.14
// Orrery pad — a thin wrapper over lib/pads.mjs (UTC-clock beat grid,
// params[0] rate override, the tap/XY pump, audio polling). This file only
// says what makes korvo korvo: its Keplerian orbit field and its kalimba
// conjunction score.
//
// ALLEGORY: a handful of worlds circle a sun, inner ones faster and higher-
// pitched than outer ones (a kalimba comb: short tines close in, ring high).
// A radar beam sweeps once per bar, synced to the UTC beat grid. A note only
// sounds the instant a planet crosses the beam — so the SIGHT of a conjunction
// and the SOUND of it are the same event, not two systems pretending to agree.
// Tap flings a brand-new world into orbit at the tapped radius/angle; it keeps
// circling and keeps returning to the beam on its own, unprompted, for about
// twenty seconds — you plant a voice, then the sky plays it back to you.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

const STEPS = 12; // one full sweep revolution per bar
const TOL = (Math.PI / STEPS) * 0.85; // angular catch-window at the beam

// Outer → inner, low → high: a kalimba-style comb across three octaves.
const SCALE = ["a1", "c2", "e2", "g2", "a2", "c3", "e3", "g3", "a3", "c4"];
function noteForRadius(rn) {
  const idx = Math.round((1 - rn) * (SCALE.length - 1));
  return SCALE[Math.max(0, Math.min(SCALE.length - 1, idx))];
}
function orbitSpeed(rn) {
  return 0.3 / (0.25 + rn); // closer to the sun = faster (loosely Keplerian)
}

const SEED_RN = [0.22, 0.36, 0.5, 0.64, 0.78, 0.92]; // permanent worlds

// --- korvo-specific state (engine owns pump/bursts/rhythm) ------------------
let planets = []; // {radius(0..1), angle, speed, note, hue, life, perm, tap, spark}
let sweepAngle = 0;
let bassKick = 0;
let stars = [];
let starDims = { w: 0, h: 0 };

function ensureStars(w, h, quality) {
  if (starDims.w === w && starDims.h === h && stars.length) return;
  starDims = { w, h };
  const n = Math.round(70 * Math.max(0.4, quality));
  stars = [];
  for (let i = 0; i < n; i++) {
    stars.push({ x: Math.random(), y: Math.random(), r: Math.random() * 1.2 + 0.3, tw: 40 + Math.random() * 80 });
  }
}

const CONFIG = {
  bpm: 92,
  steps: STEPS,
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.5 });
      planets = SEED_RN.map((rn, i) => ({
        radius: rn,
        angle: (i / SEED_RN.length) * Math.PI * 2,
        speed: orbitSpeed(rn),
        note: noteForRadius(rn),
        hue: 30 + i * 48,
        life: 1,
        perm: true,
        tap: false,
        spark: 0,
      }));
    },

    // The beam reaches idx's grid angle — resync the visual sweep to it and
    // fire every world currently sitting inside the catch-window.
    onBeat({ idx, synth }) {
      const target = ((idx % STEPS) / STEPS) * Math.PI * 2;
      sweepAngle = target;
      for (const p of planets) {
        const diff = (((p.angle - target + Math.PI * 3) % (Math.PI * 2)) - Math.PI);
        if (Math.abs(diff) < TOL) {
          p.spark = 1;
          voices.pluck(synth, p.note, { beats: 0.6, volume: p.perm ? 0.24 : 0.3, pan: Math.cos(p.angle) * 0.8 });
          synth({ tone: p.note, type: "sine", beats: 0.9, attack: 0.006, decay: 0.8, volume: 0.1, pan: Math.sin(p.angle) * 0.5 });
        }
      }
      if (idx % STEPS === 0) {
        voices.sub(synth, "a1", { beats: 2, decay: 0.6, volume: 0.42 });
        bassKick = 1;
      }
      voices.hat(synth, { tone: 6800, beats: 0.08, volume: 0.08 });
    },

    onSim() {
      bassKick *= 0.9;
      sweepAngle += 0.012;
      for (const p of planets) {
        p.angle += p.speed * 0.02;
        p.spark *= 0.88;
        if (!p.perm) p.life -= 0.0008;
      }
      planets = planets.filter((p) => p.perm || p.life > 0);
    },

    // Fling a new world into orbit at the tapped radius/angle. X picks its
    // direction (spins CW or CCW), Y falls out of the radius naturally.
    onTap({ x, ex, ey, synth, screen }) {
      const cx = screen.width / 2, cy = screen.height / 2;
      const dx = ex - cx, dy = ey - cy;
      const maxR = Math.min(screen.width, screen.height) * 0.44;
      const rn = Math.max(0.08, Math.min(1, Math.hypot(dx, dy) / maxR));
      const angle = Math.atan2(dy, dx);
      const note = noteForRadius(rn);
      planets.push({
        radius: rn, angle, speed: orbitSpeed(rn) * (x < 0.5 ? -1 : 1),
        note, hue: x * 300 + 20, life: 1, perm: false, tap: true, spark: 1,
      });
      const tapCount = planets.filter((p) => !p.perm).length;
      if (tapCount > 10) {
        const i = planets.findIndex((p) => !p.perm);
        if (i >= 0) planets.splice(i, 1);
      }
      voices.pluck(synth, note, { beats: 0.5, volume: 0.32, pan: x * 2 - 1 });
      synth({ tone: note, type: "triangle", beats: 0.4, attack: 0.002, decay: 0.35, volume: 0.14, pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num, blur } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;
      const maxR = Math.min(w, h) * 0.44;

      ensureStars(w, h, quality);

      // Deep-space veil: translucent dark box, prior trails persist and fade.
      ink(3, 3, 12, 46).box(0, 0, w, h);

      for (const st of stars) ink(200, 210, 255, st.tw).circle(st.x * w, st.y * h, st.r, true);

      for (const rn of SEED_RN) ink(90, 90, 140, 18).circle(cx, cy, rn * maxR, false, 1);

      // The sweep beam — the score's needle, always visible where the next
      // conjunctions will land.
      const sx = cx + Math.cos(sweepAngle) * maxR, sy = cy + Math.sin(sweepAngle) * maxR;
      ink(140, 200, 255, 55).line(cx, cy, sx, sy);
      ink(200, 230, 255, 130).circle(sx, sy, 3, true);

      const bass = band("subBass");
      const sunPulse = Math.min(2, bassKick * 0.8 + bass * 1.1 + amp * 0.5 + pump * 0.4);
      for (let i = 3; i > 0; i--)
        ink(255, 210, 120, (26 * sunPulse) / i).circle(cx, cy, (6 + sunPulse * 10) * (i / 3), true);
      ink(255, 240, 200, 220).circle(cx, cy, 4 + sunPulse * 4, true);

      // ADAPTIVE QUALITY: seed worlds always render; tap-launched worlds are
      // the part that grows unbounded, so THAT count and tail length are what
      // scale down under load.
      const permP = planets.filter((p) => p.perm);
      const tapP = planets.filter((p) => !p.perm);
      const tapDrawCount = Math.max(2, Math.round(tapP.length * quality));
      const drawn = permP.concat(tapP.slice(-tapDrawCount));
      const tailLen = Math.max(3, Math.round(14 * quality));

      for (const p of drawn) {
        const r = p.radius * maxR;
        const [r0, g0, b0] = num.hslToRgb(((p.hue % 360) + 360) % 360, 80, p.tap ? 62 : 55);
        for (let i = tailLen; i > 0; i--) {
          const a = p.angle - Math.sign(p.speed || 1) * i * 0.045;
          const tx = cx + Math.cos(a) * r, ty = cy + Math.sin(a) * r;
          const al = (1 - i / tailLen) * 90 * (p.perm ? 1 : p.life);
          ink(r0, g0, b0, al).circle(tx, ty, 1.6, true);
        }
        const px = cx + Math.cos(p.angle) * r, py = cy + Math.sin(p.angle) * r;
        if (p.spark > 0.02) ink(255, 255, 255, 200 * p.spark).circle(px, py, 5 + p.spark * 8, true);
        ink(r0, g0, b0, p.perm ? 220 : 220 * p.life).circle(px, py, p.tap ? 4 : 3, true);
      }

      if (sunPulse > 1.1 || pump > 1.3) blur?.(1);
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
