// prism, 26.07.12
// Kaleidoscopic A-minor arpeggio pad — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `prism 0.5`, the tap/XY "pump", audio polling). This file only describes what
// makes prism prism: its score, its richened voices, and its kaleidoscope paint.
// Every audible note FIRES a spoke (angle ∝ step, hue ∝ pitch); bass = a central
// low bloom; the beat pulses the ring — the visual IS the score.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
const ARP = [
  "a2", "e3", "a3", "c4", "e4", "c4", "a3", "e3",
  "a2", "g3", "b3", "d4", "g4", "d4", "b3", "g3",
];
const BASS = ["a1", "a1", "f1", "f1", "c2", "c2", "e1", "e1"]; // half-time root
const SYMMETRY = 8; // kaleidoscope fold count

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

// --- prism-specific visual state (the engine owns pump/bursts/rhythm) --------
let spokes = []; // { angle, hue, life, big, bright } — one per fired note
let bassBloom = 0; // central low bloom, kicked by each bass note
let ringPulse = 0; // whole-ring pulse, kicked each beat

const CONFIG = {
  bpm: 132,
  steps: ARP.length,
  drawBursts: false, // prism draws its own fancier tap rings in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.32, feedback: 0.6 });
    },

    // A new UTC beat crossed — fire the score + spawn the matching visuals.
    onBeat({ idx, synth }) {
      const s = ((idx % ARP.length) + ARP.length) % ARP.length;
      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / ARP.length) * Math.PI * 2) * 0.7;

      // Richened arp: a Karplus–Strong pluck (harp) instead of a bare triangle.
      voices.pluck(synth, note, { beats: 0.9, decay: 0.55, volume: 0.5, pan });

      // ALLEGORY: this note fires a spoke — angle ∝ step, hue ∝ pitch.
      spokes.push({
        angle: (s / ARP.length) * Math.PI * 2,
        hue: 20 + pn * 300,
        life: 1,
        big: 1 - pn,
        bright: 0.4 + pn * 0.6,
      });

      // Shimmer bell octave-up on high steps.
      if (pn > 0.6) voices.bell(synth, note, { beats: 0.5, volume: 0.18, pan: -pan });

      // Sub-bass root on the half-beat — the downbeat you feel + SEE.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.6, decay: 0.5, volume: 0.5 });
        bassBloom = 1;
      }

      voices.hat(synth, { tone: 8000, beats: 0.12, volume: 0.16 });
      ringPulse = 1;
    },

    onSim() {
      bassBloom *= 0.9;
      ringPulse *= 0.9;
      for (const sp of spokes) sp.life -= 0.013;
      spokes = spokes.filter((sp) => sp.life > 0);
      if (spokes.length > 24) spokes = spokes.slice(-24);
    },

    // Tap = a spoke at the tapped angle + a plucked boost (engine already bumped
    // pump + pushed the burst; X→pitch/pan, Y→brightness).
    onTap({ x, y, ex, ey, synth, screen }) {
      spokes.push({
        angle: Math.atan2(ey - screen.height / 2, ex - screen.width / 2),
        hue: x * 360,
        life: 1.2,
        big: 1,
        bright: 1,
      });
      const note = ["a", "c", "d", "e", "g"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 4));
      voices.pluck(synth, note, { beats: 0.6, volume: 0.55, pan: x * 2 - 1 });
      voices.bell(synth, note, { beats: 0.3, volume: 0.3 * (1 - y), pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { ink, screen, num, spin, zoom, blur } = api;
      const { pump, step, beatProgress, bursts, amp, band } = s;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      const bass = band("subBass");
      const energy = Math.min(2, ringPulse * 0.6 + bass * 1.2 + amp * 0.6 + pump * 0.5);

      // Feedback vortex base (recycle prev frame → kaleidoscopic bloom).
      zoom?.(1.01 + energy * 0.02, 0.5, 0.5);
      spin?.(0.18 + ringPulse * 0.9 + pump * 0.6);
      ink(6, 4, 16, 140).box(0, 0, w, h);

      const beatPhase = (step + beatProgress) % ARP.length;
      const maxR = Math.min(w, h) * 0.6;

      // BASS = big soft central low bloom.
      if (bassBloom > 0.01 || bass > 0.05) {
        const bloom = Math.max(bassBloom, bass * 1.2);
        const bR = Math.min(w, h) * (0.1 + bloom * 0.28);
        for (let i = 3; i > 0; i--)
          ink(140, 40, 180, 16 * bloom).circle(cx, cy, bR * (i / 3), true);
      }

      // NOTE PETALS — mirrored SYMMETRY-fold kaleidoscope flower.
      const innerR = maxR * 0.12;
      for (const sp of spokes.slice(-8)) {
        const reach = innerR + maxR * (0.42 + sp.big * 0.42) * (0.85 + energy * 0.3);
        const [r0, g0, b0] = num.hslToRgb(((sp.hue % 360) + 360) % 360, 100, 60 + sp.bright * 15);
        const a = 170 + 85 * sp.life;
        const halfW = (0.16 + sp.big * 0.1) * (1 + energy * 0.15);
        const midR = innerR + (reach - innerR) * 0.42;
        const tipR = 3 + sp.bright * 9 + sp.life * 5;
        for (let f = 0; f < SYMMETRY; f++) {
          const foldOff = (f / SYMMETRY) * Math.PI * 2;
          for (const mirror of [1, -1]) {
            const ctr = mirror * sp.angle + foldOff;
            const base = [cx + Math.cos(ctr) * innerR, cy + Math.sin(ctr) * innerR];
            const l = [cx + Math.cos(ctr - halfW) * midR, cy + Math.sin(ctr - halfW) * midR];
            const rgt = [cx + Math.cos(ctr + halfW) * midR, cy + Math.sin(ctr + halfW) * midR];
            const tip = [cx + Math.cos(ctr) * reach, cy + Math.sin(ctr) * reach];
            ink(r0, g0, b0, a).shape([base, l, tip, rgt]);
            const l2 = [cx + Math.cos(ctr - halfW * 0.5) * midR, cy + Math.sin(ctr - halfW * 0.5) * midR];
            const r2 = [cx + Math.cos(ctr + halfW * 0.5) * midR, cy + Math.sin(ctr + halfW * 0.5) * midR];
            const tip2 = [cx + Math.cos(ctr) * (reach * 0.85), cy + Math.sin(ctr) * (reach * 0.85)];
            ink(Math.min(255, r0 + 60), Math.min(255, g0 + 60), Math.min(255, b0 + 60), a)
              .shape([base, l2, tip2, r2]);
            ink(255, 255, 255, 220 * sp.life).circle(tip[0], tip[1], tipR, true);
          }
        }
      }

      // Central mandala core: two nested slow-rotating polygons.
      const coreSides = SYMMETRY;
      for (let k = 1; k >= 0; k--) {
        const kr = maxR * (0.1 + k * 0.09) * (1 + energy * 0.5);
        const rot = (k % 2 === 0 ? 1 : -1) * beatPhase * 0.25;
        const hue = (beatPhase / ARP.length) * 360;
        const [cr, cg, cb] = num.hslToRgb((hue + k * 40) % 360, 90, 60);
        let px = null, py = null;
        for (let i = 0; i <= coreSides; i++) {
          const ang = (i / coreSides) * Math.PI * 2 + rot;
          const x = cx + Math.cos(ang) * kr, y = cy + Math.sin(ang) * kr;
          if (px !== null) ink(cr, cg, cb, 200).line(px, py, x, y, 2);
          px = x; py = y;
        }
      }

      // TAP BURSTS (engine-tracked): expanding rings, hue by X.
      for (const b of bursts) {
        const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 95, 60);
        ink(r0, g0, b0, 200 * b.life).circle(b.x, b.y, b.r, false, 2 + b.life * 4);
        ink(255, 255, 255, 150 * b.life).circle(b.x, b.y, b.r * 0.5, false, 2);
        ink(r0, g0, b0, 130 * b.life).circle(b.x, b.y, 4 + b.life * 12, true);
      }

      if (ringPulse > 0.55 || pump > 1.2) blur?.(1);

      const heartR = 4 + ringPulse * 10 + bass * 12 + pump * 7;
      ink(180, 120, 255, 90 + ringPulse * 90).circle(cx, cy, heartR * 1.7, true);
      ink(255, 255, 255, 160 + ringPulse * 80).circle(cx, cy, heartR, true);
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
