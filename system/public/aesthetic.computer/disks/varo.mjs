// varo, 26.07.12
// Moiré interference × detuned pad swell — a thin wrapper over lib/pads.mjs
// (the shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `varo 0.5`, the tap/XY "pump", audio polling, adaptive ctx.quality). This
// file only describes what makes varo varo: its detuned-pad score and its
// op-art moiré paint.
//
// ALLEGORY — two overlapping ring/line grids slowly slide over each other; the
// shimmer between them IS the BEAT-FREQUENCY of two slightly-detuned synth
// voices. You SEE the beating you HEAR. Each note offsets/rotates one grid
// (pitch → grid spacing & angle); the BASS = the slide speed; amplitude =
// contrast. Two concentric ring families drawn as cheap vector strokes, count
// scaled by ctx.quality so it holds 60fps native. Hypnotic, detuned, hypnagogic.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A slow, hovering detuned pad in D-dorian-ish territory. Each step nudges the
// second grid's spacing/angle. Long attacks = swelling. 8 steps, half-time bass.
const NOTES = ["d3", "a3", "f3", "c4", "d4", "a3", "g3", "e3"];
const BASS = ["d2", "d2", "bb1", "bb1", "c2", "c2", "a1", "a1"]; // half-time slide root

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(b?)(\d)$/.exec(n);
  if (!m) return 48;
  const acc = m[2] === "b" ? -1 : 0;
  return (parseInt(m[3], 10) + 1) * 12 + NOTE_SEMI[m[1]] + acc;
}
const PITCHES = NOTES.map(notePitch);
const PITCH_MIN = Math.min(...PITCHES);
const PITCH_MAX = Math.max(...PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- varo-specific visual state (engine owns pump/bursts/rhythm) ------------
// Two ring grids. gridA is the "anchor"; gridB slides & detunes off it. The
// visible gap between their spacings is the moiré — mirroring the audible beat
// frequency between the two detuned pad layers.
let heldPads = null; // the two detuned held-pad layers (the beating)
let spacingB = 1.0; // gridB spacing multiplier (nudged per note = pitch→spacing)
let angleB = 0; // gridB rotation (nudged per note = pitch→angle)
let spacingTarget = 1.0; // eased target for spacingB
let angleTarget = 0; // eased target for angleB
let slide = 0; // accumulated slide phase (bass = slide speed)
let slideSpeed = 0.006; // current slide speed, kicked by bass
let swell = 0; // overall swell/contrast, kicked each beat
let ripples = []; // tap ripples: { x, y, r, life, hue } — a moiré ping from a tap

const CONFIG = {
  bpm: 60, // slow, hovering pad (1s/beat)
  steps: NOTES.length,
  drawBursts: false, // varo draws its own moiré ripples in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.4, feedback: 0.7 });
    },

    // A new UTC beat crossed — swell the detuned pad + nudge grid B.
    onBeat({ idx, synth }) {
      const s = ((idx % NOTES.length) + NOTES.length) % NOTES.length;
      const note = NOTES[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / NOTES.length) * Math.PI * 2) * 0.6;

      // THE BEATING: (re)start two slightly-detuned held pad layers — the sonic
      // moiré. padChord returns handles; kill the previous swell first so tails
      // don't pile up. One layer at the note, one a few cents sharp → they beat.
      const detuneRatio = 1.006; // ~10 cents sharp → slow audible beat freq
      const hz = 440 * Math.pow(2, (notePitch(note) - 69) / 12);
      try { heldPads?.forEach?.((v) => v?.kill?.(0.4)); } catch (e) {}
      heldPads = [
        ...voices.padChord(synth, [hz], { type: "sine", volume: 0.1, attack: 0.7, decay: 1.2, spread: 0.25, pan }),
        ...voices.padChord(synth, [hz * detuneRatio], { type: "triangle", volume: 0.08, attack: 0.8, decay: 1.2, spread: 0.25, pan: -pan }),
      ];

      // Airy flute top swells over the pad on higher notes.
      if (pn > 0.4)
        voices.flute(synth, note, { beats: 1.6, attack: 0.4, decay: 0.8, volume: 0.16, pan: -pan });

      // NOTE → GRID: pitch sets grid B's spacing & angle target (the offset).
      spacingTarget = 0.82 + pn * 0.42; // higher pitch → wider grid gap → tighter moiré
      angleTarget = (s / NOTES.length) * Math.PI * 0.5 + pn * 0.4;
      swell = 1;

      // BASS = the SLIDE SPEED of grid B. Fires half-time; deeper root = faster slide.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        const bn = BASS[bi];
        voices.sub(synth, bn, { beats: 2.0, attack: 0.06, decay: 0.7, volume: 0.5 });
        const bpn = pitchNorm(bn.replace(/1$/, "3")); // rough low-ness
        slideSpeed = 0.004 + (1 - Math.min(1, bpn)) * 0.012;
      }
    },

    onSim() {
      // Ease grid B toward its per-note targets → smooth moiré breathing.
      spacingB += (spacingTarget - spacingB) * 0.06;
      angleB += (angleTarget - angleB) * 0.05;
      // Bass drives the slide; slide speed itself decays back to a gentle drift.
      slide += slideSpeed;
      slideSpeed += (0.005 - slideSpeed) * 0.02;
      swell *= 0.94;
      for (const rp of ripples) {
        rp.r += 3.2;
        rp.life -= 0.02;
      }
      ripples = ripples.filter((rp) => rp.life > 0);
      if (ripples.length > 8) ripples = ripples.slice(-8);
    },

    // Tap = a moiré ripple from the tap + a swelling detuned tone.
    // X → pan/hue, Y → pitch. The ripple locally shoves grid B (a moiré pulse).
    onTap({ x, y, ex, ey, synth }) {
      const midi = 40 + Math.floor((1 - y) * 34); // Y → pitch
      const hz = 440 * Math.pow(2, (midi - 69) / 12);
      const pan = x * 2 - 1;
      // A brief swelling detuned pair (the tap's own little beat).
      voices.padChord(synth, [hz], { type: "sine", volume: 0.16, attack: 0.15, decay: 0.9, pan });
      voices.padChord(synth, [hz * 1.008], { type: "triangle", volume: 0.12, attack: 0.2, decay: 0.9, pan: -pan });
      ripples.push({ x: ex, y: ey, r: 4, life: 1.2, hue: x * 360 });
      // Nudge grid B so the whole field ripples in response to the tap.
      angleTarget += (x - 0.5) * 0.5;
      spacingTarget = 0.8 + (1 - y) * 0.5;
      swell = Math.min(1.4, swell + 0.6);
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      const bass = band("subBass") + band("lowMid") * 0.5;
      const contrast = Math.min(1, 0.35 + amp * 1.2 + swell * 0.5 + pump * 0.25); // amplitude = contrast
      const energy = Math.min(2, swell * 0.6 + bass * 1.1 + amp * 0.6 + pump * 0.5);

      // Dark veil (leave a faint trail so the moiré shimmers rather than strobes).
      ink(5, 4, 12, 200).box(0, 0, w, h);

      const maxR = Math.hypot(w, h) * 0.55;
      // Grid A base spacing; grid B slides & detunes off it → moiré.
      const baseGap = Math.max(6, Math.min(w, h) * 0.045);
      // ctx.quality scales ring count → holds 60fps native. Base ~34 rings/grid.
      const BASE = 34;
      const nRings = Math.max(8, Math.round(BASE * quality));

      // Two hue poles that drift with the slide → the shimmer takes on color.
      const hueA = (slide * 22) % 360;
      const hueB = (slide * 22 + 40 + angleB * 60) % 360;

      // --- GRID A: concentric rings from center, fixed anchor ---------------
      // Offset by the slide so the whole anchor drifts (bass = slide speed).
      const slideOff = (slide * baseGap * 0.5) % baseGap;
      const [ar, ag, ab] = num.hslToRgb(((hueA % 360) + 360) % 360, 70, 42 + contrast * 22);
      for (let i = 1; i <= nRings; i++) {
        const r = i * baseGap - slideOff;
        if (r < 2 || r > maxR) continue;
        const a = Math.round((60 + contrast * 90) * (1 - r / maxR));
        ink(ar, ag, ab, a).circle(cx, cy, r, false, 1);
      }

      // --- GRID B: rings off a slightly-shifted center, detuned spacing -----
      // The DETUNE: gapB ≠ gapA (spacingB), and the center is nudged along the
      // slide/angle. Where the two ring families cross, you get moiré fringes —
      // the visual beat frequency of the two detuned pad layers.
      const gapB = baseGap * spacingB;
      const shift = baseGap * 1.4 * Math.min(1.5, 0.5 + swell * 0.8);
      const bx = cx + Math.cos(angleB + slide * 0.3) * shift;
      const by = cy + Math.sin(angleB + slide * 0.3) * shift;
      const slideOffB = (slide * gapB * 0.5) % gapB;
      const [br, bg, bb] = num.hslToRgb(((hueB % 360) + 360) % 360, 80, 46 + contrast * 22);
      for (let i = 1; i <= nRings; i++) {
        const r = i * gapB - slideOffB;
        if (r < 2 || r > maxR) continue;
        const a = Math.round((60 + contrast * 90) * (1 - r / maxR));
        ink(br, bg, bb, a).circle(bx, by, r, false, 1);
      }

      // --- BEAT-FREQUENCY MARKER: a soft bloom pulsing at the moiré node ----
      // Midpoint between the two grid centers = where the fringe is strongest;
      // it breathes with the swell (the audible beat you can point at).
      const mx = (cx + bx) / 2, my = (cy + by) / 2;
      const bloomR = (baseGap * 1.2) * (1 + swell * 1.4 + bass * 1.5);
      const [hr, hg, hb] = num.hslToRgb((((hueA + hueB) / 2) % 360 + 360) % 360, 90, 62);
      for (let k = 3; k > 0; k--)
        ink(hr, hg, hb, Math.round(24 * swell + 10 * bass)).circle(mx, my, bloomR * (k / 3), true);

      // Faint interference cross-lines between the two centers reinforce the beat.
      ink(hr, hg, hb, Math.round(30 + contrast * 60)).line(cx, cy, bx, by, 2);

      // --- TAP RIPPLES: expanding moiré pings, hue by X --------------------
      for (const rp of ripples) {
        const [rr, rg, rb] = num.hslToRgb(((rp.hue % 360) + 360) % 360, 95, 62);
        ink(rr, rg, rb, Math.round(180 * rp.life)).circle(rp.x, rp.y, rp.r, false, 2);
        ink(255, 255, 255, Math.round(120 * rp.life)).circle(rp.x, rp.y, rp.r * 0.6, false, 1);
      }

      // Center dot — the anchor of grid A.
      ink(230, 230, 255, 140 + swell * 90).circle(cx, cy, 3 + energy * 2, true);
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
