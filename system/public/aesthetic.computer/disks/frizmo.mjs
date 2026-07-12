// frizmo, 26.07.12
// FIZZY-STATIC SPARKLE instrument — champagne fizz / effervescence, now a THIN
// WRAPPER over lib/pads.mjs (the shared pad engine: UTC-clock beat grid,
// `params[0]` rate override e.g. `frizmo 0.5`, the tap/XY "pump", audio polling).
// This file only describes what makes frizmo frizmo: its fizz score, its richened
// voices, and its grain-spray paint.
//
// TIMBRE→TEXTURE ALLEGORY (the whole point):
//   • NOISE FIZZ = a live GRAIN SPRAY written DIRECTLY to screen.pixels. Grain
//     density & brightness ∝ fizz amplitude — you literally SEE the noise as
//     shimmering static/grain. Frame-seeded hash (NOT Math.random) so it animates
//     identically headless.
//   • BLIP (pitched click) = a bright SPARK that pops and RISES like a bubble;
//     height & hue ∝ pitch (high blip = high on screen, cyan/white).
//   • KICK = a BURST of bubbles ERUPTING from the bottom edge.
// The engine owns one 16th-note beat = one engine beat (bpm 528 = 132×4), the
// UTC grid, params[0] rate, the tap pump + burst push, and audio polling.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// High BLIPS arpeggiate every step → rising sparks. KICK on the 4 downbeats
// (steps 0,4,8,12) → bubble eruption from the bottom. Bright, effervescent scale.
const BLIP = [
  "c6", "g5", "e6", "a5",
  "d6", "b5", "g6", "e6",
  "c6", "a5", "d6", "g5",
  "e6", "b5", "a6", "d6",
];
// Pitch range spanned by BLIP, used to map note → spark launch height / hue.
const BLIP_FREQ = {
  g5: 783.99, a5: 880.0, b5: 987.77,
  c6: 1046.5, d6: 1174.66, e6: 1318.51, g6: 1567.98, a6: 1760.0,
};
const BLIP_LOW = 783.99; // g5
const BLIP_HIGH = 1760.0; // a6
function pitchNorm(freq) {
  const t = (Math.log2(freq) - Math.log2(BLIP_LOW)) /
    (Math.log2(BLIP_HIGH) - Math.log2(BLIP_LOW));
  return Math.max(0, Math.min(1, t));
}

// --- frizmo-specific visual state (the engine owns pump/bursts/rhythm) -------
let sparks = []; // rising bubbles/sparks (blip onsets + kick eruptions + taps)
let bursts = []; // fizzy burst rings spawned by taps {x,y,r,vel,life,hue,r0,g0,b0}
let fizz = 0.35; // baseline fizz amplitude → grain density (0..~1.4)
let kick = 0; // decays; drives bottom bubble eruption + low pulse
let flash = 0; // decays; whole-field per-step sparkle

// Frame-advanced hash seed (Math.random may be flaky/black headless).
let seed = 1234567;
function rnd() {
  // xorshift32
  seed ^= seed << 13; seed |= 0;
  seed ^= seed >> 17;
  seed ^= seed << 5; seed |= 0;
  return ((seed >>> 0) % 100000) / 100000; // 0..1
}

// hue 0..1, sat/light 0..1 → [r,g,b] 0..255. num.hslToRgb wants hue in DEGREES
// + sat/light 0..100 and ALREADY returns 0..255 — do NOT ×255.
function hue2rgb(num, hue, sat, light) {
  return num.hslToRgb(((hue % 1) + 1) % 1 * 360, sat * 100, light * 100);
}

let bed = null; // sustained airy fizz bed (noise floor so the reel always sings)

const CONFIG = {
  bpm: 528, // 132 quarter-BPM × 4 → one engine beat = one 16th step
  steps: BLIP.length, // 16
  drawBursts: false, // frizmo draws its own fizzy tap rings in onPaint
  hooks: {
    onBoot({ sound }) {
      sparks = []; bursts = []; fizz = 0.35; kick = 0; flash = 0; seed = 1234567;
      sound.room?.set?.({ enabled: true, mix: 0.12, feedback: 0.35 });
      // A quiet sustained noise bed = the ever-present champagne hiss. Its volume
      // breathes with fizz + pump in onSim so the grain always has a voice.
      // NOTE: noise-white goes in `type`, NOT `tone`.
      bed = sound.synth?.({ type: "noise-white", tone: 2400, duration: "🔁",
        attack: 0.5, decay: 0.99, volume: 0.16 });
    },

    // A new UTC beat crossed (= one 16th step) — fire the score + spawn visuals.
    onBeat({ idx, synth, screen }) {
      const bar = ((idx % 16) + 16) % 16; // 0..15
      const w = screen?.width || 360, h = screen?.height || 640;

      // KICK on the 4 downbeats → punchy low thump (voices.sub) + a burst of
      // bubbles erupting from the bottom edge. The effervescence "pop" from below.
      if (bar % 4 === 0) {
        voices.sub(synth, "c2", { beats: 0.6, attack: 0.004, decay: 0.35, volume: 0.9 });
        // A short fizz "chsss" rides the kick — the cork pop.
        voices.hat(synth, { type: "noise-white", tone: 1200, beats: 0.4, volume: 0.5 });
        synth({ type: "noise-white", tone: 1200, beats: 0.3, attack: 0.002,
          decay: 0.4, volume: 0.4 });
        kick = 1;
        fizz = Math.min(1.4, fizz + 0.55); // the kick sprays the grain harder
        const n = 26;
        for (let i = 0; i < n && sparks.length < 380; i++) {
          sparks.push({
            x: rnd() * w, y: h + 2,
            vx: (rnd() - 0.5) * 2.2, vy: -(3.5 + rnd() * 6.5), // shoot upward
            grav: -0.02 - rnd() * 0.03, // gentle rise / buoyancy
            life: 1, decay: 0.012 + rnd() * 0.01,
            hue: 0.5 + rnd() * 0.12, // cyan-ish champagne
            big: rnd() < 0.35, kind: "bubble",
          });
        }
      }

      // BLIP on every step → a bright pitched click (voices.bell — layered
      // sine+triangle, bright) + a rising SPARK whose height & hue ∝ pitch.
      const bn = BLIP[bar % BLIP.length];
      const pan = -0.7 + (bar % BLIP.length) / (BLIP.length - 1) * 1.4;
      const accent = bar % 4 === 0; // downbeat blips a touch louder
      voices.bell(synth, bn, { beats: 0.4, volume: accent ? 0.34 : 0.24, pan });
      voices.pluck(synth, bn, { beats: 0.3, decay: 0.3, volume: 0.2, pan }); // bright ping
      // Tiny high fizz tick between blips → shimmer.
      if (bar % 2 === 1) {
        voices.hat(synth, { type: "noise-white", tone: 5000, beats: 0.2, volume: 0.18 });
        fizz = Math.min(1.4, fizz + 0.12);
      }

      const freq = BLIP_FREQ[bn] || 1000;
      const pn = pitchNorm(freq); // 0 low .. 1 high
      sparks.push({
        x: w * (0.15 + (bar % BLIP.length) / BLIP.length * 0.7) + (rnd() - 0.5) * 20,
        y: h * (0.86 - pn * 0.1), // starts a bit up for high notes
        vx: (rnd() - 0.5) * 1.2,
        vy: -(2.5 + pn * 4.5), // higher pitch rises faster/farther
        grav: -0.015 - pn * 0.02,
        life: 1, decay: 0.016 + (1 - pn) * 0.01,
        hue: 0.5 + pn * 0.13, // pitch → hue (cyan→bright)
        big: accent, kind: "spark", pn,
      });

      flash = 1; // whole-field sparkle pulse on every step
    },

    onSim({ pump }) {
      // Advance sparks/bubbles: rise, buoyancy, gentle drift, fizz jitter.
      const jit = 0.4 + pump * 0.3;
      for (let i = 0; i < sparks.length; i++) {
        const p = sparks[i];
        p.x += p.vx + (rnd() - 0.5) * jit; // fizzy wobble
        p.y += p.vy;
        p.vy += p.grav; // buoyancy pulls up (negative grav)
        p.vx *= 0.985;
        if (p.vy < -9) p.vy = -9;
        p.life -= p.decay * (1 + pump * 0.15);
      }
      sparks = sparks.filter((p) => p.life > 0 && p.y > -20);

      // Advance frizmo's own tap burst rings.
      for (let i = 0; i < bursts.length; i++) {
        const b = bursts[i];
        b.r += b.vel * (1 + pump * 0.15);
        b.vel *= 0.985;
        b.life -= 0.02;
      }
      bursts = bursts.filter((b) => b.life > 0);

      // Decay pulses; fizz relaxes toward a pump-boosted baseline.
      flash *= 0.85;
      kick *= 0.9;
      const fizzBase = 0.3 + pump * 0.35;
      fizz += (fizzBase - fizz) * 0.12;
      if (fizz < 0.05) fizz = 0.05;

      // Bed breathes: the champagne hiss swells with fizz + tap energy.
      bed?.update?.({ volume: 0.12 + fizz * 0.14 + pump * 0.05 });
    },

    // Tap = a spark FOUNTAIN + a fizzy burst ring at the tap point (engine
    // already bumped pump + pushed a burst; X→pan/hue, Y→pitch).
    onTap({ x, y, ex, ey, isDraw, synth, num }) {
      const drag = isDraw;
      fizz = Math.min(1.4, fizz + (drag ? 0.15 : 0.6));

      const hue = 0.5 + x * 0.2; // X → hue (champagne cyan→gold-ish)
      const [br, bg, bb] = hue2rgb(num, hue, 0.9, 0.62);
      bursts.push({ x: ex, y: ey, r: 0, vel: drag ? 8 : 15, life: 1, hue,
        r0: br, g0: bg, b0: bb });

      const n = drag ? 8 : 26;
      for (let i = 0; i < n && sparks.length < 400; i++) {
        const ang = -Math.PI / 2 + (rnd() - 0.5) * Math.PI * 1.1; // upward fan
        const sp = 2 + rnd() * (drag ? 4 : 9);
        sparks.push({
          x: ex, y: ey,
          vx: Math.cos(ang) * sp, vy: Math.sin(ang) * sp,
          grav: -0.02 - rnd() * 0.03,
          life: 1, decay: 0.02 + rnd() * 0.02,
          hue: hue + (rnd() - 0.5) * 0.1, big: rnd() < 0.3, kind: "spark",
        });
      }

      // SONIC BURST — X→pan, Y→pitch (top = high).
      const scale = ["c", "d", "e", "g", "a"];
      const oct = 4 + Math.floor((1 - y) * 3); // 4..6, higher up = higher
      const note = scale[Math.min(4, Math.floor(x * 5))] + oct;
      voices.bell(synth, note, { beats: drag ? 0.3 : 0.5, volume: drag ? 0.3 : 0.5, pan: x * 2 - 1 });
      voices.pluck(synth, note, { beats: drag ? 0.18 : 0.4, decay: 0.4, volume: drag ? 0.25 : 0.4, pan: x * 2 - 1 });
      // Bright fizzy chsss burst — brighter higher up the screen.
      synth({ type: "noise-white", tone: 2000 + (1 - y) * 4000,
        beats: drag ? 0.15 : 0.35, attack: 0.001, decay: 0.35,
        volume: (drag ? 0.22 : 0.4) * (0.6 + (1 - y) * 0.6), pan: x * 2 - 1 });
      if (!drag) voices.sub(synth, "c2", { beats: 0.4, decay: 0.3, volume: 0.5 });
    },

    onPaint(api, s) {
      const { ink, screen, num, paintCount } = api;
      const { pump, beatProgress } = s;
      const w = screen.width, h = screen.height;
      const pixels = screen.pixels;
      const pc = Number(paintCount);

      // Live audio garnish (visuals never DEPEND on it).
      const air = s.band("air");
      const treble = s.band("treble");
      const amp = s.amp;

      // === Dark fizzy ground + GRAIN SPRAY written directly to screen.pixels ===
      // Grain density/brightness ∝ fizz → you SEE the noise as sparse BRIGHT
      // speckles on a dark ground. Kick brightens the lower (eruption) band.
      const density = Math.min(0.34, 0.06 + fizz * 0.16 + amp * 0.1 + pump * 0.04);
      const speckBright = 150 + fizz * 90 + treble * 60;
      const kb = kick;
      // Advance the hash seed per frame so speckle animates (NOT Math.random).
      seed = (seed ^ (pc * 2654435761)) | 0;
      seed ^= seed << 13; seed |= 0;

      for (let y = 0; y < h; y++) {
        const vy = y / h; // 0 top .. 1 bottom
        const bgR = 2 + vy * 3 + kb * (vy > 0.7 ? (vy - 0.7) * 55 : 0);
        const bgG = 4 + vy * 9 + kb * (vy > 0.7 ? (vy - 0.7) * 85 : 0);
        const bgB = 9 + vy * 20 + kb * (vy > 0.7 ? (vy - 0.7) * 115 : 0);
        let row = y * w;
        for (let x = 0; x < w; x++) {
          const i = (row + x) * 4;
          let hshr = (x * 374761393 + y * 668265263 + seed) | 0;
          hshr = (hshr ^ (hshr >> 13)) * 1274126177;
          const rr = ((hshr ^ (hshr >> 16)) >>> 0) / 4294967295; // 0..1
          if (rr < density) {
            const b = speckBright * (0.35 + rr / density * 0.65);
            pixels[i] = Math.min(255, bgR + b * 0.65);
            pixels[i + 1] = Math.min(255, bgG + b * 0.92);
            pixels[i + 2] = Math.min(255, bgB + b);
          } else {
            pixels[i] = bgR;
            pixels[i + 1] = bgG;
            pixels[i + 2] = bgB;
          }
          pixels[i + 3] = 255;
        }
      }

      // === BLIP/BUBBLE sparks = rising bubbles; height & hue read the pitch ===
      for (let i = 0; i < sparks.length; i++) {
        const p = sparks[i];
        const light = 0.55 + (p.big ? 0.25 : 0.1) + flash * 0.1;
        const [r, g, b] = hue2rgb(num, p.hue, 0.85, Math.min(0.9, light));
        const rad = (p.big ? 2.2 : 1.3) + p.life * (p.big ? 2.5 : 1.6);
        ink(r, g, b, 90 * p.life).circle(p.x, p.y, rad + 1.5, true);
        ink(Math.min(255, r + 70), Math.min(255, g + 70), Math.min(255, b + 70),
          240 * p.life).circle(p.x, p.y, rad, true);
        ink(255, 255, 255, 200 * p.life).plot((p.x - rad * 0.4) | 0,
          (p.y - rad * 0.4) | 0);
      }

      // === KICK = eruption glow band along the bottom edge ===
      if (kick > 0.02) {
        for (let k = 0; k < 5; k++) {
          const [r, g, b] = hue2rgb(num, 0.52, 0.9, 0.55);
          ink(r, g, b, 40 * kick * (1 - k / 5)).box(0, h - (k + 1) * 8 * kick,
            w, 8 * kick);
        }
        ink(220, 255, 255, 120 * kick).box(0, h - 3, w, 3);
      }

      // === Tap FIZZY BURST rings — the whole-piece "button" made visible ===
      for (let i = 0; i < bursts.length; i++) {
        const b = bursts[i];
        ink(b.r0, b.g0, b.b0, 190 * b.life).circle(b.x, b.y, b.r, false,
          2 + b.life * 5);
        const [ir, ig, ib] = hue2rgb(num, b.hue + 0.08, 0.9, 0.72);
        ink(ir, ig, ib, 140 * b.life * b.life).circle(b.x, b.y, b.r * 0.4, true);
        ink(255, 255, 255, 220 * b.life * b.life).circle(b.x, b.y, 3 + b.life * 7,
          true);
      }

      // === Per-step sparkle pulse — a faint bright veil flash on each blip ===
      if (flash > 0.15 && treble > 0.02) {
        ink(180, 255, 255, 18 * flash * (1 - beatProgress)).box(0, 0, w, h);
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
