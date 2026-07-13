// emberdrift, 26.07.12
// Cosmic starfield groove — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `emberdrift 0.5`,
// the tap/XY "pump", audio polling). This file only describes what makes
// emberdrift emberdrift: its 16th-note score, its richened voices, and its
// starfield paint. The visual IS the score:
//   • BASS/KICK = a big shockwave RING launched from center on the downbeat.
//   • ARP note  = a rising STAR/STREAK launched at height ∝ pitch, the moment
//                 it sounds (higher pitch = higher + brighter).
//   • HAT       = a tiny sparkle FLECK.
//   • THE BEAT  = the pulse of the whole starfield (it breathes on grid).
//   • TAP       = a SUPERNOVA (shockwave ring + spark shower + boom) at the point.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// 16th-note grid. The engine's beat = one 16th step, so we run at 4× the
// musical BPM (120 quarter → 480) with 16 steps.
const QUARTER_BPM = 120;
const BASS = ["c1", "c1", "g1", "c1"]; // kick-like root on the 4 downbeats
const ARP = [
  "c4", "g4", "a4", "e5",
  "c5", "g4", "d5", "b4",
  "c4", "a4", "c5", "g5",
  "e5", "d5", "b4", "g4",
];

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const ARP_PITCHES = ARP.map(notePitch);
const PITCH_MIN = Math.min(...ARP_PITCHES);
const PITCH_MAX = Math.max(...ARP_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- emberdrift-specific visual state (engine owns pump/bursts/rhythm) -------
const STAR_COUNT = 240;
const stars = [];
let sparks = [];   // transient embers (beat sparks + tap spark showers)
let rings = [];    // audible-onset rings: kick shockwaves + arp launch markers
let flecks = [];   // tiny hat sparkle flecks
let novas = [];    // tap supernova shockwaves { x, y, r, vel, life, hue, r0, g0, b0 }
let flash = 0;     // whole-field pulse, kicked on every step
let bassKick = 0;  // central kick shockwave energy, kicked on bass
let clockPulse = 0; // 0..1 saw driven by the step CLOCK (color engine)

let seeded = false;
function seedStars() {
  const R = () => Math.random();
  stars.length = 0;
  for (let i = 0; i < STAR_COUNT; i++) {
    stars.push({
      x: (R() - 0.5) * 2,
      y: (R() - 0.5) * 2,
      z: R() * 1 + 0.001,
      hue: R(),
    });
  }
  seeded = true;
}

// hue 0..1, sat/light 0..1 → [r,g,b] 0..255. num.hslToRgb wants hue in DEGREES
// + sat/light 0..100, and already returns 0..255 — do NOT ×255.
function hue2rgb(num, hue, sat, light) {
  return num.hslToRgb(((hue % 1) + 1) % 1 * 360, sat * 100, light * 100);
}

// Sustained cosmic drones (audio floor so the reel always sings).
let pad = null;
let pad2 = null;

const CONFIG = {
  bpm: QUARTER_BPM * 4, // engine beat = 16th-note step (125ms)
  steps: ARP.length,    // 16
  drawBursts: false,    // emberdrift draws its own supernova rings in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.1, feedback: 0.4 });
      seedStars();
      // Two sustained drones under the groove — a present audio bed + bass body
      // regardless of where the UTC phase lands in a capture window. Volume
      // breathes with the kick + tap energy in onSim.
      pad = sound.synth?.({ tone: "c2", type: "sawtooth", duration: "🔁",
        attack: 0.4, decay: 0.99, volume: 0.85, pan: -0.15 });
      pad2 = sound.synth?.({ tone: "g2", type: "triangle", duration: "🔁",
        attack: 0.6, decay: 0.99, volume: 0.6, pan: 0.15 });
    },

    // A new UTC 16th-step crossed — fire the score + spawn its visuals.
    onBeat({ idx, synth }) {
      const bar = ((idx % ARP.length) + ARP.length) % ARP.length; // 0..15

      // BASS / KICK on the 4 downbeats → a big shockwave RING from center.
      if (bar % 4 === 0) {
        const bn = BASS[(bar / 4) % BASS.length];
        // Warm sub with body (sine + faint saw). Short so it doesn't muddy.
        voices.sub(synth, bn, { beats: 0.4, attack: 0.005, decay: 0.4, volume: 0.9 });
        bassKick = 1;
        rings.push({ kind: "kick", r: 0, vel: 9, life: 1, hue: 0.02 });
      }

      // HAT on off-steps → a tiny sparkle FLECK.
      if (bar % 2 === 1) {
        const accent = bar % 4 === 3;
        voices.hat(synth, { tone: 800, beats: 0.12, volume: accent ? 0.4 : 0.24 });
        flecks.push({ x: Math.random(), y: Math.random() * 0.5, life: 1, accent });
      }

      // ARP note per step → a rising STAR at height ∝ pitch, the moment it sounds.
      const note = ARP[bar];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const arpPan = -0.6 + (bar / (ARP.length - 1)) * 1.2;
      // Richened arp: a bright Karplus–Strong pluck.
      voices.pluck(synth, note, { beats: 0.5, decay: 0.55, volume: 0.55, pan: arpPan });
      // Shimmer bell octave-feel on high steps.
      if (pn > 0.6) voices.bell(synth, note, { beats: 0.4, volume: 0.18, pan: -arpPan });

      rings.push({
        kind: "arp", r: 0, vel: 4 + pn * 5, life: 1,
        hue: 0.55 + pn * 0.35,
        launchY: pn,
        pan: arpPan,
      });

      flash = 1;
      clockPulse = bar / 16;
    },

    onSim({ pump }) {
      const surge = 1 + bassKick * 0.01 + pump * 0.012;

      // Advance drifting stars toward the camera.
      for (let i = 0; i < stars.length; i++) {
        const s = stars[i];
        s.z -= (0.004 + bassKick * 0.01) * surge;
        if (s.z <= 0.001) {
          s.x = (Math.random() - 0.5) * 2;
          s.y = (Math.random() - 0.5) * 2;
          s.z = 1;
          s.hue = Math.random();
        }
      }

      // Advance onset rings (kick shockwaves + arp launch markers).
      for (let i = 0; i < rings.length; i++) {
        const rg = rings[i];
        rg.r += rg.vel * (1 + pump * 0.2);
        rg.life -= rg.kind === "kick" ? 0.02 : 0.03;
      }
      rings = rings.filter((rg) => rg.life > 0);

      // Advance & cull sparks.
      for (let i = 0; i < sparks.length; i++) {
        const p = sparks[i];
        p.x += p.vx;
        p.y += p.vy;
        p.vx *= 0.96;
        p.vy *= 0.96;
        p.life -= p.decay || 0.03;
      }
      sparks = sparks.filter((p) => p.life > 0);

      // Advance flecks.
      for (let i = 0; i < flecks.length; i++) flecks[i].life -= 0.05;
      flecks = flecks.filter((f) => f.life > 0);

      // Advance supernova bursts (tap shockwaves).
      for (let i = 0; i < novas.length; i++) {
        const b = novas[i];
        b.r += b.vel * (1 + pump * 0.15);
        b.vel *= 0.985;
        b.life -= 0.02;
      }
      novas = novas.filter((b) => b.life > 0);

      // Decay pulses.
      flash *= 0.86;
      bassKick *= 0.9;

      // Pads breathe: swell on the kick + tap energy → a living bed.
      pad?.update?.({ volume: 0.85 + bassKick * 0.25 + pump * 0.12 });
      pad2?.update?.({ volume: 0.6 + bassKick * 0.18 + pump * 0.1 });
    },

    // Tap = a SUPERNOVA at the point (engine already bumped pump + pushed a
    // burst; we ignore that burst and draw our own). X→pan/hue, Y→pitch.
    onTap({ x, y, ex, ey, isDraw, synth, num }) {
      const drag = isDraw;
      const hue = x; // X → hue (0..1)
      const [br, bg, bb] = hue2rgb(num, hue, 1.0, 0.6);

      // Supernova shockwave ring at the tap point.
      novas.push({ x: ex, y: ey, r: 0, vel: drag ? 9 : 16, life: 1, hue,
        r0: br, g0: bg, b0: bb });

      // Spark shower radiating from the tap point.
      const n = drag ? 8 : 22;
      for (let i = 0; i < n && sparks.length < 320; i++) {
        const ang = Math.random() * Math.PI * 2;
        const sp = 2 + Math.random() * (drag ? 4 : 8);
        sparks.push({
          x: ex, y: ey,
          vx: Math.cos(ang) * sp, vy: Math.sin(ang) * sp,
          life: 1, decay: 0.02 + Math.random() * 0.02, hue,
        });
      }

      // SONIC BOOM — X→pan/hue (via note choice), Y→pitch (top = high).
      const scale = ["c", "d", "e", "g", "a"];
      const oct = 2 + Math.floor((1 - y) * 4); // 2..5, higher up = higher
      const note = scale[Math.min(4, Math.floor(x * 5))] + oct;
      voices.pluck(synth, note, { beats: drag ? 0.3 : 0.6, decay: 0.6,
        volume: drag ? 0.4 : 0.65, pan: x * 2 - 1 });
      voices.bell(synth, note, { beats: 0.4, volume: 0.3 * (1 - y) + 0.12,
        pan: x * 2 - 1 });
      // Sub thump so the boom has body.
      if (!drag) {
        voices.sub(synth, scale[Math.min(4, Math.floor(x * 5))] + "1",
          { beats: 0.5, decay: 0.35, volume: 0.6 });
      }
    },

    onPaint(api, s) {
      const { ink, circle, line, plot, screen, num, paintCount } = api;
      const { pump, beatProgress, amp, band } = s;
      // Adaptive quality (1 = full detail … 0.35 = auto-cut). We DRAW a subset of
      // the big particle arrays when under budget; at q=1 every count below is the
      // full length so the field is pixel-identical to full detail. `qc` remaps
      // the engine's [0.35..1] quality band onto a full [0..1] cut so the floor
      // actually thins the field hard (0.35 → ~0 extras) while q=1 stays whole.
      const q = s.quality ?? 1;
      const qc = q >= 0.99 ? 1 : Math.max(0, (q - 0.35) / 0.65);
      const sub = (arr) => Math.round(arr.length * qc); // how many to draw this frame
      const w = screen.width, h = screen.height;
      const cx = w / 2, cy = h / 2;
      const foc = Math.min(w, h) * 0.9;
      const minWH = Math.min(w, h);

      // Deep-space veil (trail fade → glowing streaks). Pump thins it slightly.
      ink(4, 2, 14, Math.max(24, 40 - pump * 4)).box(0, 0, w, h);

      // --- Live audio garnish (visuals never DEPEND on it) ---
      const bass = band("subBass");
      const air = band("air");

      // --- Clock-driven color engine ---
      const pc = Number(paintCount);
      const clockHue = ((pc % 600) / 600 + clockPulse * 0.5) % 1;
      const beatEnergy = Math.min(1, flash * 0.8 + bassKick * 0.6 + amp * 0.5 + pump * 0.3);

      // === BASS/KICK = central bloom, pulsed by bassKick (clock, not audio) ===
      const bloom = Math.max(bassKick, flash * 0.5) * (1 + pump * 0.4);
      if (bloom > 0.02) {
        const R = minWH * (0.14 + bass * 0.5) * bloom;
        const [r0, g0, b0] = hue2rgb(num, clockHue + 0.55, 0.9, 0.55);
        // These are big FILLED circles — the single largest raster cost after the
        // veil (each covers a big fraction of the screen). Draw all 5 halo layers
        // at q=1; ramp the stack down to NONE by the quality floor (~0.35). Alpha
        // is rescaled by the drawn count so peak density holds. The bright core
        // dot is cheap and always stays, so the kick flash still reads. Mapping
        // q∈[0.35,1] → layers∈[0,5] keeps q=1 pixel-identical (layers = 5).
        const layers = q >= 0.99
          ? 5
          : Math.max(0, Math.round(((q - 0.35) / 0.65) * 5));
        for (let i = layers; i > 0; i--) {
          ink(r0, g0, b0, (32 * 5 / layers) * bloom)
            .circle(cx, cy, R * (i / layers), true);
        }
        ink(255, 220, 255, 190 * bloom).circle(cx, cy, 7 + bass * 30, true);
      }

      // === Starfield: warp streaks; near stars throw sparks on a strong step ===
      const streak = 10 + flash * 26 + amp * 40 + pump * 30;
      // Stars define the look, so keep at least ~40% even at the floor (they're
      // not the dominant cost — the big FILLS are). Full set at q=1.
      const nStars = q >= 0.99
        ? stars.length
        : Math.round(stars.length * (0.4 + 0.6 * qc));
      for (let i = 0; i < nStars; i++) {
        const st = stars[i];
        const px = cx + (st.x / st.z) * foc * 0.5;
        const py = cy + (st.y / st.z) * foc * 0.5;
        if (px < -20 || px > w + 20 || py < -20 || py > h + 20) continue;

        const near = 1 - st.z; // 0 far .. 1 near
        const hue = (st.hue + clockHue) % 1;
        const [r, g, b] = hue2rgb(
          num, hue, 1.0,
          Math.min(0.9, 0.55 + near * 0.3 + beatEnergy * 0.12),
        );
        const bright = Math.min(255, 130 + near * 125 + pump * 25);

        const dx = px - cx, dy = py - cy;
        const len = Math.hypot(dx, dy) || 1;
        const ux = dx / len, uy = dy / len;
        // Warp-streak TAILS are the pixel-heavy part of the starfield. Draw them
        // in full at q=1; under pressure only the nearest stars keep their tail
        // (far, faint tails vanish first) — the bright head-point always stays.
        const tail = streak * near;
        if (q >= 0.99 || near > 1 - q) {
          ink(r, g, b, bright).line(px, py, px - ux * tail, py - uy * tail);
        }
        ink(Math.min(255, r + 80), Math.min(255, g + 80), Math.min(255, b + 80),
          bright).plot(px | 0, py | 0);

        // Colored spark from near stars on a strong step-flash.
        if (flash > 0.6 && near > 0.7 && Math.random() < 0.3 && sparks.length < 320) {
          const sp = 1.5 + Math.random() * 3;
          const ang = Math.random() * Math.PI * 2;
          sparks.push({
            x: px, y: py,
            vx: Math.cos(ang) * sp + ux * 2, vy: Math.sin(ang) * sp + uy * 2,
            life: 1, hue,
          });
        }
      }

      // === ARP note = a RISING STAR launched at height ∝ pitch ===
      const nRings = sub(rings);
      for (let i = 0; i < nRings; i++) {
        const rg = rings[i];
        if (rg.kind === "arp") {
          const targetY = h * (0.9 - rg.launchY * 0.8); // higher pitch = higher
          const yy = cy + (targetY - cy) * (1 - rg.life);
          const xx = cx + rg.pan * w * 0.32 * (1 - rg.life); // fan out by pan
          const [r, g, b] = hue2rgb(num, rg.hue + clockHue * 0.2, 1.0,
            0.55 + rg.launchY * 0.3);
          ink(r, g, b, 220 * rg.life).line(xx, cy, xx, yy);
          ink(Math.min(255, r + 60), Math.min(255, g + 60), Math.min(255, b + 60),
            255 * rg.life).circle(xx, yy, 2 + rg.launchY * 4 + rg.life * 3, true);
        } else {
          // === BASS/KICK = big shockwave RING from center on the beat ===
          const [r, g, b] = hue2rgb(num, clockHue + 0.02, 0.95, 0.6);
          ink(r, g, b, 170 * rg.life)
            .circle(cx, cy, rg.r, false, 2 + rg.life * 5 + air * 6);
        }
      }

      // === HAT = tiny sparkle FLECK ===
      const nFlecks = sub(flecks);
      for (let i = 0; i < nFlecks; i++) {
        const f = flecks[i];
        const fx = f.x * w, fy = f.y * h;
        const c = f.accent ? 255 : 210;
        ink(c, c, 255, 255 * f.life).plot(fx | 0, fy | 0);
        if (f.accent) ink(255, 255, 255, 150 * f.life).circle(fx, fy, 1.5, true);
      }

      // === Sparks — ember bursts (beat sparks + tap spark showers) ===
      const nSparks = sub(sparks);
      for (let i = 0; i < nSparks; i++) {
        const p = sparks[i];
        const [r, g, b] = hue2rgb(num, (p.hue ?? 0) + 0.04, 1.0, 0.62);
        ink(r, g, b, 245 * p.life).circle(p.x, p.y, 1.2 + p.life * 3, true);
      }

      // === SUPERNOVA bursts — the whole-piece "button" made visible ===
      for (let i = 0; i < novas.length; i++) {
        const b = novas[i];
        ink(b.r0, b.g0, b.b0, 200 * b.life)
          .circle(b.x, b.y, b.r, false, 2 + b.life * 6);
        const [ir, ig, ib] = hue2rgb(num, b.hue + 0.1, 1.0, 0.7);
        ink(ir, ig, ib, 150 * b.life * b.life)
          .circle(b.x, b.y, b.r * 0.4, true);
        ink(255, 255, 255, 220 * b.life * b.life).circle(b.x, b.y, 3 + b.life * 8, true);
      }

      // === The beat pulse of the whole field — a per-step shockwave ring ===
      // A near-screen-wide stroked circle every step (heavy pixel coverage). Keep
      // it at q=1; drop it first under pressure (flash stays high most frames).
      if (flash > 0.05 && q > 0.55) {
        const rr = minWH * 0.5 * beatProgress;
        const [rr2, rg2, rb2] = hue2rgb(num, clockHue + 0.7, 0.95, 0.6);
        ink(rr2, rg2, rb2, 120 * (1 - beatProgress) * flash)
          .circle(cx, cy, rr, false, 2 + air * 6);
      }

      // Air-band shimmer accent ring (garnish when audio is live).
      if (air > 0.05) {
        ink(200, 255, 255, air * 120).circle(cx, cy, minWH * 0.46, false, 1);
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

function leave() {
  pad?.kill?.(0.5);
  pad2?.kill?.(0.5);
  pad = null;
  pad2 = null;
}

export { boot, sim, paint, act, leave };
