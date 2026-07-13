// flim, 26.07.12
// Spirograph epicycloid pad — a thin wrapper over lib/pads.mjs (shared pad
// engine: UTC beat grid, `flim 0.5` rate override, tap/XY pump, audio polling,
// adaptive quality). This file describes only what makes flim flim: a warm
// additive DRAWBAR-ORGAN drone and its glowing hypotrochoid curve.
//
// ALLEGORY — nested rotating gears trace one continuous neon curve. Every note
// sets a gear RATIO, so pitch → petal count (higher note = more petals). The
// beat advances the tracing angle; the bass note drives the overall rotation
// SPEED and swells a held organ drone. A phosphor veil keeps the trail glowing.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A slow modal organ progression (D dorian-ish) — each note becomes a gear.
const MELODY = [
  "d3", "a3", "d4", "f4", "a4", "f4", "d4", "a3",
  "c4", "g3", "c4", "e4", "g4", "e4", "c4", "g3",
];
const BASS = ["d2", "d2", "bb1", "bb1", "c2", "c2", "g1", "g1"]; // half-time root
const ORGAN_PARTIALS = [1, 2, 3, 4, 6, 8]; // drawbar harmonics (sine stack)

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(b?)(\d)$/.exec(n);
  if (!m) return 48;
  const semi = NOTE_SEMI[m[1]] - (m[2] ? 1 : 0);
  return (parseInt(m[3], 10) + 1) * 12 + semi;
}
const MEL_PITCHES = MELODY.map(notePitch);
const PITCH_MIN = Math.min(...MEL_PITCHES);
const PITCH_MAX = Math.max(...MEL_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// Note → gear ratio → petal count. Low notes = few fat petals, high = many.
function petalsFor(n) {
  return 3 + Math.round(pitchNorm(n) * 9); // 3..12 petals
}

// --- flim-specific state (engine owns pump/bursts/rhythm/quality) -----------
let ratio = 5; // current gear ratio (petals) — set by each note, eased toward
let targetRatio = 5;
let curveHue = 200; // curve hue, follows pitch
let targetHue = 200;
let traceAngle = 0; // tracing head angle, advanced by beat + bass speed
let spin = 0; // overall rotation, driven by bass
let droneSwell = 0; // organ drone swell, kicked by bass
let flourish = 0; // note flash (whole-curve brighten)
let perturb = 0; // tap perturbation (a wobbly burst petal)
let perturbAng = 0;
let organVoices = null; // held drawbar-organ chord handles (lazy)

const CONFIG = {
  bpm: 108,
  steps: MELODY.length,
  drawBursts: false, // flim draws its own tap flares in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.55 });
    },

    // A new UTC beat crossed — sound the note + turn the gears.
    onBeat({ idx, synth }) {
      const s = ((idx % MELODY.length) + MELODY.length) % MELODY.length;
      const note = MELODY[s];
      const pn = pitchNorm(note);
      const pan = Math.sin((s / MELODY.length) * Math.PI * 2) * 0.55;

      // ALLEGORY: this note sets the gear ratio → the curve's petal count.
      targetRatio = petalsFor(note);
      targetHue = 190 + pn * 150; // pitch → hue (cyan → magenta)
      flourish = 1;

      // Lazily start the HELD additive drawbar-organ drone (onBoot has no
      // top-level synth). A stack of raw sines at harmonic multiples = warm
      // Hammond-ish tone. voices.padChord gives us live handles to retrigger.
      if (!organVoices) {
        const tones = ORGAN_PARTIALS.map((h) => note); // sung on the root note
        organVoices = voices.padChord(synth, tones, {
          type: "sine",
          volume: 0.05,
          attack: 0.8,
          decay: 1.2,
          spread: 0.25,
        });
      }

      // Retrigger the organ chord voice for THIS note — additive sine partials.
      ORGAN_PARTIALS.forEach((h, i) => {
        synth({
          tone: note,
          type: "sine",
          beats: 1.4,
          attack: 0.02 + i * 0.01,
          decay: 0.7,
          volume: (0.16 / (1 + i * 0.6)) * (0.7 + pn * 0.5), // drawbar taper
          pan: pan * (i % 2 ? -1 : 1) * 0.5,
        });
      });
      // A brighter triangle top adds the reedy organ shimmer on high notes.
      if (pn > 0.5)
        synth({ tone: note, type: "triangle", beats: 0.5, attack: 0.01, decay: 0.5, volume: 0.08 * pn, pan });

      // BASS = rotation SPEED + a held sub drone swell on the half-beat.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.8, decay: 0.55, volume: 0.5 });
        // Low organ pedal (16' drawbar feel) — a deep sine an octave under.
        synth({ tone: BASS[bi], type: "sine", beats: 2.0, attack: 0.1, decay: 0.9, volume: 0.22 });
        droneSwell = 1;
        spin += 0.5 + pn * 0.4; // bass advances overall rotation
      }

      // Soft tick keeps the pulse without breaking the hypnotic mood.
      voices.hat(synth, { tone: 6000, beats: 0.1, volume: 0.1 });
      traceAngle += Math.PI * 0.5; // beat advances the tracing head
    },

    onSim() {
      // Ease gear state so petal count MORPHS (gears mesh, not snap).
      ratio += (targetRatio - ratio) * 0.12;
      curveHue += (targetHue - curveHue) * 0.1;
      droneSwell *= 0.94;
      flourish *= 0.9;
      perturb *= 0.9;
    },

    // Tap = perturb the gears (a wobbly burst petal) + an organ chord stab.
    // X → pan/hue, Y → pitch. Engine already bumped pump + pushed a burst.
    onTap({ x, y, ex, ey, synth, screen }) {
      perturb = 1.2;
      perturbAng = Math.atan2(ey - screen.height / 2, ex - screen.width / 2);
      targetHue = x * 360;
      // Y → pitch: pick a note up the modal scale.
      const scale = ["d", "e", "f", "g", "a", "c"];
      const oct = 2 + Math.floor((1 - y) * 3);
      const root = scale[Math.floor(x * scale.length) % scale.length] + oct;
      // Organ chord stab — three additive sine partials, warm + immediate.
      [0, 4, 7].forEach((semi, i) => {
        const note = scale[(Math.floor(x * scale.length) + i * 2) % scale.length] + (oct + (i > 1 ? 1 : 0));
        synth({ tone: note, type: "sine", beats: 0.8, attack: 0.01, decay: 0.6, volume: 0.16 / (1 + i * 0.4), pan: x * 2 - 1 });
      });
      synth({ tone: root, type: "triangle", beats: 0.4, attack: 0.01, decay: 0.4, volume: 0.1, pan: x * 2 - 1 });
      targetRatio = 3 + Math.round((1 - y) * 9);
      flourish = 1;
      spin += 0.6;
    },

    onPaint(api, s) {
      const { ink, screen, num, blur, zoom } = api;
      const { pump, beatProgress, bursts, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      const bass = band("subBass");
      const energy = Math.min(2, droneSwell * 0.7 + bass * 1.1 + amp * 0.6 + pump * 0.5);

      // Phosphor trail veil — darken toward the head, keep the curve glowing.
      // Slow feedback zoom gives the whole spiral a breathing drift.
      zoom?.(1.004 + energy * 0.006, 0.5, 0.5);
      ink(4, 3, 12, 40 + Math.round(30 * (1 - flourish))).box(0, 0, w, h);

      // Advance tracing continuously between beats + by bass-driven spin.
      const head = traceAngle + beatProgress * Math.PI * 0.5 + spin * 0.15;
      const R = Math.min(w, h) * 0.36 * (0.9 + energy * 0.18);
      const k = ratio; // gear ratio → petal count (eased)
      const r = R / k; // rolling gear radius
      const d = r * (1.4 + Math.sin(spin * 0.2) * 0.3); // pen offset (hypotrochoid)

      // ADAPTIVE QUALITY: scale traced segments per frame by ctx.quality.
      const BASE_SEG = 520;
      const segs = Math.max(120, Math.round(BASE_SEG * quality));
      const span = Math.PI * 2 * Math.max(1, Math.round(k)); // close the curve

      // Hypotrochoid: point traced by a pen inside a gear of radius r rolling
      // inside a fixed circle of radius R. Petal count follows (R-r)/r ≈ k-1.
      const rot = spin * 0.1;
      const [hr, hg, hb] = num.hslToRgb(((curveHue % 360) + 360) % 360, 100, 58 + flourish * 12);
      const [gr, gg, gb] = num.hslToRgb(((curveHue + 40) % 360 + 360) % 360, 100, 65);

      let px = null, py = null;
      for (let i = 0; i <= segs; i++) {
        const t = (i / segs) * span + head * 0.15 + rot;
        let x = (R - r) * Math.cos(t) + d * Math.cos(((R - r) / r) * t);
        let y = (R - r) * Math.sin(t) - d * Math.sin(((R - r) / r) * t);
        // Tap perturbation — a localized gear wobble near the tapped angle.
        if (perturb > 0.01) {
          const a = Math.atan2(y, x);
          const nearness = Math.max(0, 1 - Math.abs(((a - perturbAng + Math.PI * 3) % (Math.PI * 2)) - Math.PI) / 0.9);
          const bump = 1 + perturb * nearness * 0.5;
          x *= bump; y *= bump;
        }
        const sx = cx + x, sy = cy + y;
        if (px !== null) {
          const glow = 120 + flourish * 100;
          // Fatter warm underlay + bright core = neon.
          ink(gr, gg, gb, Math.round(glow * 0.4)).line(px, py, sx, sy, 3);
          ink(hr, hg, hb, glow).line(px, py, sx, sy, 1);
        }
        px = sx; py = sy;
      }

      // Tracing HEAD — a bright spark where the pen currently is.
      {
        const t = span + head * 0.15 + rot;
        const hx = cx + (R - r) * Math.cos(t) + d * Math.cos(((R - r) / r) * t);
        const hy = cy + (R - r) * Math.sin(t) - d * Math.sin(((R - r) / r) * t);
        const hr2 = 3 + flourish * 6 + pump * 4;
        ink(255, 255, 255, 220).circle(hx, hy, hr2, true);
        ink(hr, hg, hb, 140).circle(hx, hy, hr2 * 2.2, true);
      }

      // Central gear hub — organ drone bloom, swells with bass.
      if (droneSwell > 0.01 || bass > 0.04) {
        const bloom = Math.max(droneSwell, bass * 1.1);
        const bR = Math.min(w, h) * (0.05 + bloom * 0.16);
        for (let i = 3; i > 0; i--)
          ink(hr, hg, hb, Math.round(18 * bloom)).circle(cx, cy, bR * (i / 3), true);
      }

      // TAP FLARES (engine-tracked bursts): quick neon rings by X-hue.
      for (const b of bursts) {
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 95, 62);
        ink(r0, g0, b0, Math.round(200 * b.life)).circle(b.x, b.y, b.r, false, 2 + b.life * 3);
        ink(255, 255, 255, Math.round(150 * b.life)).circle(b.x, b.y, b.r * 0.5, false, 2);
      }

      if (flourish > 0.5 || pump > 1.4) blur?.(1);

      // Center pinlight heart.
      const heartR = 3 + droneSwell * 8 + bass * 10 + pump * 6;
      ink(hr, hg, hb, 90 + Math.round(droneSwell * 90)).circle(cx, cy, heartR * 1.6, true);
      ink(255, 255, 255, 150 + Math.round(flourish * 80)).circle(cx, cy, heartR, true);
    },
  },
};

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before boot/sim/
// paint/act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
