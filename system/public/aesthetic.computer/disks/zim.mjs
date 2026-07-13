// zim, 26.07.12
// Volumetric smoke pad — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `zim 0.5`, the
// tap/XY "pump", audio polling). This file only describes what makes zim zim:
// its ambient granular score, its softened voices, and its curl-noise smoke.
//
// ALLEGORY: soft colored smoke drifts and curls upward on a cheap flow field.
// Every audible NOTE injects a fresh puff (pitch → hue + inject height); the
// BASS sets the drift CURRENT (strength + direction) so the whole plume leans;
// ambient GRANULAR pads (held detuned chord + flute breaths + sparse bell
// grains + sub) shimmer under it. Dreamy, slow, volumetric — the smoke IS the
// score. Puff count scales by ctx.quality to hold 60fps.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score (ambient, slow, A-Dorian-ish drift) ------------------------------
// Gently flowing melody — occasional rests read as breaths. "." = rest.
const MEL = [
  "a3", "e4", "c4", "g4", "e4", "d4", "b3", "e4",
  "g3", "d4", "b3", "f4", "e4", "c4", "a3", "d4",
];
const BASS = ["a1", "a1", "f1", "f1", "c2", "c2", "e1", "e1"]; // drift current root
const CHORD = ["a2", "e3", "a3", "b3"]; // held detuned granular bed

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const MEL_PITCHES = MEL.filter((n) => n !== ".").map(notePitch);
const PITCH_MIN = Math.min(...MEL_PITCHES);
const PITCH_MAX = Math.max(...MEL_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- zim-specific smoke state (the engine owns pump/bursts/rhythm) ----------
// Each puff: soft translucent blob advected by a curl-noise flow field, rising.
let puffs = []; // { x, y, vx, vy, hue, life, r, seed, bright }
let flowT = 0; // slow time for the flow field
let current = { dir: 0, strength: 0.2 }; // drift CURRENT set by the bass
let held = null; // held granular chord handles
let chordHue = 200; // slow-drifting bed hue

// Cheap curl-of-a-sin-field flow (no fluid solver). Returns [fx, fy] at (nx,ny).
// curl(ψ) = (∂ψ/∂y, -∂ψ/∂x); ψ is a sum of sines → smooth swirling advection.
function flow(nx, ny, t) {
  const psiDy =
    Math.cos(nx * 3.1 + t * 0.6) * 0.9 +
    Math.cos(ny * 2.3 - nx * 1.7 + t * 0.4) * 0.6;
  const psiDx =
    -Math.sin(ny * 2.7 - t * 0.5) * 0.9 -
    Math.sin(nx * 2.1 + ny * 1.3 + t * 0.35) * 0.6;
  return [psiDy, -psiDx];
}

function injectPuff(x, y, hue, r, bright, vy0 = -0.6) {
  puffs.push({
    x, y,
    vx: (Math.random() - 0.5) * 0.3,
    vy: vy0 - Math.random() * 0.4,
    hue,
    life: 1,
    r: r ?? 30 + Math.random() * 20,
    seed: Math.random() * 1000,
    bright: bright ?? 0.6,
  });
}

const CONFIG = {
  bpm: 84, // slow, dreamy — but enough puffs to keep the plume alive
  steps: MEL.length,
  drawBursts: false, // zim renders its own tap smoke
  hooks: {
    onBoot({ sound }) {
      // Lush, long reverb for volumetric ambience.
      sound.room?.set?.({ enabled: true, mix: 0.45, feedback: 0.72 });
    },

    // A new UTC beat crossed — advance the ambient score + inject smoke.
    onBeat({ idx, synth, screen }) {
      const s = ((idx % MEL.length) + MEL.length) % MEL.length;
      const note = MEL[s];
      const w = screen?.width || 256;
      const h = screen?.height || 256;

      // Start the held granular chord on the first beat (onBoot has no synth).
      if (!held) {
        held = voices.padChord(synth, CHORD, {
          type: "sine",
          volume: 0.09,
          attack: 1.2,
          decay: 1.4,
          spread: 0.4,
        });
      }

      // BASS = drift CURRENT. Every half-beat set the current's direction from
      // the bass root — the whole plume leans toward it.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        const broot = BASS[bi];
        voices.sub(synth, broot, { beats: 3.2, decay: 0.6, volume: 0.42 });
        const bpn = (notePitch(broot) - 20) / 30; // roughly 0..1 across a1..c2
        current.dir = (bpn - 0.5) * 1.4; // lean left/right by bass pitch
        current.strength = 0.18 + bpn * 0.22;
      }

      if (note === ".") return; // rest — a breath, no puff

      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / MEL.length) * Math.PI * 2) * 0.6;

      // A soft flute breath carries the note (airy granular lead).
      voices.flute(synth, note, {
        beats: 2.4,
        attack: 0.4,
        decay: 0.6,
        volume: 0.18,
        pan,
      });
      // Sparse bell grain shimmer on the higher notes.
      if (pn > 0.55)
        voices.bell(synth, note, { beats: 1.6, volume: 0.1, pan: -pan });

      // ALLEGORY: inject a fresh colored puff — pitch → hue + inject HEIGHT
      // (high notes puff near the top, low notes near the bottom). Wide hue
      // span so low notes = warm amber smoke, high notes = cool teal/violet.
      const hue = 30 + pn * 260; // amber → green → teal → violet as pitch rises
      const injX = w * (0.3 + Math.sin(s) * 0.2 + Math.random() * 0.1);
      const injY = h * (0.85 - pn * 0.5);
      injectPuff(injX, injY, hue, 34 + pn * 16, 0.6 + pn * 0.4);
      chordHue += 4;
    },

    onSim({ quality }) {
      flowT += 0.012;
      // Advect + fade every puff along the curl flow, plus the bass current.
      const CUR = current;
      for (const p of puffs) {
        // normalize position into flow space (screen-independent-ish).
        const nx = p.x / 220;
        const ny = p.y / 220;
        const [fx, fy] = flow(nx + p.seed, ny, flowT);
        p.vx += fx * 0.5 + Math.cos(CUR.dir) * CUR.strength * 0.6;
        p.vy += fy * 0.5 - 0.35; // buoyancy — smoke rises
        p.vx *= 0.9;
        p.vy *= 0.9;
        p.x += p.vx;
        p.y += p.vy;
        p.r += 0.5; // slowly expand as it dissipates
        p.life -= 0.012;
      }
      puffs = puffs.filter((p) => p.life > 0);
      // Cap live puffs by quality to hold 60fps (each puff = a few filled
      // circles, so this is the dominant cost knob).
      const cap = Math.round(28 * quality) + 6;
      if (puffs.length > cap) puffs = puffs.slice(-cap);
    },

    // Tap = puff a BURST of smoke at the tap point + a soft swelling tone.
    // X → pan/hue, Y → pitch. (Engine already bumped pump + pushed a burst.)
    onTap({ x, y, ex, ey, synth }) {
      const hue = 180 + x * 160;
      const n = 5 + Math.floor(x * 4);
      for (let i = 0; i < n; i++) {
        const ang = Math.random() * Math.PI * 2;
        const rad = Math.random() * 26;
        injectPuff(
          ex + Math.cos(ang) * rad,
          ey + Math.sin(ang) * rad,
          hue + (Math.random() - 0.5) * 40,
          26 + Math.random() * 24,
          0.9,
          -0.4,
        );
      }
      // Soft swelling tone — Y → pitch, X → pan.
      const note = ["a", "c", "d", "e", "g"][Math.floor(x * 5) % 5] +
        (2 + Math.floor((1 - y) * 4));
      voices.flute(synth, note, {
        beats: 2.2,
        attack: 0.5,
        decay: 0.7,
        volume: 0.24,
        pan: x * 2 - 1,
      });
      voices.bell(synth, note, { beats: 1.2, volume: 0.16, pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      const bass = band("subBass") + band("lowMid") * 0.5;
      const energy = Math.min(1.6, bass * 1.4 + amp * 0.5 + pump * 0.4);

      // Soft dark veil — long trails so smoke lingers (dreamy, volumetric).
      ink(6, 8, 18, 44 + (1 - quality) * 20).box(0, 0, w, h);

      // Faint drifting bed glow — the granular chord made visible, leaning with
      // the current so you SEE the drift direction. Kept dim so puffs dominate.
      const [br, bg, bb] = num.hslToRgb(
        ((chordHue % 360) + 360) % 360, 35, 13 + energy * 7,
      );
      const lean = Math.cos(current.dir) * current.strength;
      // Smaller when quality is low (a big filled circle is O(r²) fill cost).
      const bedR = Math.min(w, h) * (0.3 + energy * 0.14) * (0.7 + quality * 0.3);
      ink(br, bg, bb, 16 + energy * 18).circle(
        w * (0.5 + lean * 0.4), h * 0.55, bedR, true,
      );

      // SMOKE PUFFS — soft translucent layered blobs. Each drawn as a few
      // concentric fading circles for a volumetric feel. Layers scale by quality.
      const layers = quality < 0.6 ? 2 : 3;
      for (const p of puffs) {
        const [r0, g0, b0] = num.hslToRgb(
          ((p.hue % 360) + 360) % 360,
          85,
          52 + p.bright * 22,
        );
        // Volumetric: inner layers are brighter/denser, outer are soft haze.
        for (let L = layers; L >= 1; L--) {
          const rr = p.r * (L / layers);
          const la = (90 * p.life * (layers - L + 1)) / layers;
          ink(r0, g0, b0, la).circle(p.x, p.y, rr, true);
        }
        // Bright wisp core.
        ink(
          Math.min(255, r0 + 90),
          Math.min(255, g0 + 90),
          Math.min(255, b0 + 90),
          170 * p.life,
        ).circle(p.x, p.y, 2 + p.bright * 4, true);
      }

      // A drift indicator: faint current streaks (only when moving) — cheap.
      if (quality > 0.6 && current.strength > 0.15) {
        const cy = h * 0.9;
        for (let i = 0; i < 5; i++) {
          const sx = w * (0.15 + i * 0.18);
          const dx = Math.cos(current.dir) * 20 * current.strength * 3;
          ink(120, 160, 200, 24).line(sx, cy, sx + dx, cy - 6, 1);
        }
      }

      // Gentle bloom on strong beats / taps.
      if (pump > 1.0 || bass > 0.35) {
        ink(200, 220, 255, 20 + pump * 20).box(0, 0, w, h);
      }
    },
  },
};

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before the engine's
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
