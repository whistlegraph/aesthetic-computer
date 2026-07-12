// bloomwell, 26.07.12
// Supersampled feedback-mandala pad — a breathing kaleidoscope well, now a thin
// wrapper over lib/pads.mjs (the shared pad engine: UTC-clock beat grid,
// `params[0]` rate override e.g. `bloomwell 0.5`, the tap/XY "pump", audio
// polling). This file only describes what makes bloomwell bloomwell: its
// D-minor score, its richened voices, and its SSAA feedback-mandala paint.
//
// STRONG GRAPHIC<->SONIC ALLEGORY — each arpeggio note lights a petal-node
// (angle + hue from pitch) the instant it sounds; the bass IS the zoom/bloom
// breath (low root = a deeper inhale); every beat spawns a fresh motif ring that
// blooms outward through the feedback. The SSAA (2×-offscreen paste-downscale)
// keeps the mandala edges smooth — KEPT, it is NOT resolution().

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// One looping bar (16 steps) of a D-minor arpeggio. Step 0 == the seam.
const ARP = [
  "d2", "a2", "d3", "f3", "a3", "f3", "d3", "a2",
  "d2", "c3", "e3", "g3", "c4", "g3", "e3", "c3",
];
const BASS = ["d1", "d1", "bb1", "bb1", "g1", "g1", "a1", "a1"]; // half-time root
const FOLDS = 6; // radial symmetry

// Pitch->normalized-height table so a note maps to a screen angle + hue.
const PITCH_LO = 24; // d1 anchor
const PITCH_HI = 60; // c4
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function noteToNorm(n) {
  const m = /^([a-g])(b|#)?(\d)$/.exec(n);
  if (!m) return 0.5;
  let semi = NOTE_SEMI[m[1]];
  if (m[2] === "b") semi -= 1;
  if (m[2] === "#") semi += 1;
  const midi = (parseInt(m[3], 10) + 1) * 12 + semi;
  return Math.max(0, Math.min(1, (midi - PITCH_LO) / (PITCH_HI - PITCH_LO)));
}

// --- bloomwell-specific state (the engine owns pump/bursts/rhythm) ----------
let pulse = 0; // eased arp note-on flash, decays toward 0
let bassBreath = 0; // eased bass inhale, decays — drives the bloom depth
let seedBuf = null; // reusable offscreen 2× seed buffer (SSAA)

// Lit petal-nodes: each audible arp note lights one at its pitch angle/hue.
let petals = []; // { norm, hue, pan, life } — life 1->0
// Injected motif rings from taps + beats: bloom outward through feedback.
let rings = []; // { x, y, r, life, hue, spokes }

const CONFIG = {
  bpm: 126,
  steps: ARP.length,
  drawBursts: false, // bloomwell draws its own motif rings in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.34, feedback: 0.62 }); // lush hum
    },

    // A new UTC beat crossed — fire the score + spawn the matching visuals.
    onBeat({ idx, synth, screen }) {
      const s = ((idx % ARP.length) + ARP.length) % ARP.length;
      const note = ARP[s];
      const norm = noteToNorm(note); // 0..1 pitch height
      const pan = Math.sin((s / ARP.length) * Math.PI * 2) * 0.7;
      const hue = (norm * 260 + s * 6) % 360; // pitch -> hue

      // Richened arp: a Karplus–Strong pluck (harp) instead of a bare triangle.
      voices.pluck(synth, note, { beats: 0.9, decay: 0.55, volume: 0.5, pan });

      // Octave-up bell shimmer every 4th step (brighter high sparkle).
      if (s % 4 === 0)
        voices.bell(synth, note, { beats: 0.4, volume: 0.2, pan: -pan });

      // Sub-bass root every other step — the breath you can feel + see.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        const broot = BASS[bi];
        voices.sub(synth, broot, { beats: 1.6, decay: 0.5, volume: 0.5 });
        // Deeper root = deeper inhale (the bloom breathes on the bass).
        bassBreath = Math.max(bassBreath, 0.7 + (1 - noteToNorm(broot)) * 0.6);
      }

      // Closed-hat tick keeps momentum.
      voices.hat(synth, { tone: 900, beats: 0.12, volume: 0.18 });

      // ALLEGORY: light the petal whose angle/hue == this note's pitch.
      petals.push({ norm, hue, pan, life: 1 });
      if (petals.length > 24) petals.shift();
      pulse = 1; // fresh note-on flash

      // ALLEGORY: every beat spawns a fresh motif ring that blooms outward.
      const cx = screen.width / 2, cy = screen.height / 2;
      rings.push({ x: cx, y: cy, r: 6, life: 1, hue, spokes: 6 });
      if (rings.length > 20) rings.shift();
    },

    onSim({ screen, pump }) {
      // Ease flashes & breath down between onsets.
      pulse *= 0.9;
      bassBreath *= 0.94;

      // Advance lit petals (fade out).
      for (const p of petals) p.life -= 0.045;
      petals = petals.filter((p) => p.life > 0);

      // Advance motif rings — bloom outward + fade (faster under pump).
      const maxR = Math.max(screen.width, screen.height) * 0.75;
      for (const r of rings) {
        r.r += 3.2 + pump * 2.4 + (1 - r.life) * 2;
        r.life -= 0.028;
        if (r.r > maxR) r.life = 0;
      }
      rings = rings.filter((r) => r.life > 0);
    },

    // Tap = a fresh motif ring + lit petal blooming from the tap spot + a chime
    // (engine already bumped pump + pushed the burst; X→hue/pan, Y→pitch).
    onTap({ x, y, ex, ey, synth }) {
      const hue = x * 360; // X -> hue/pan
      const norm = 1 - y; // Y -> pitch height (top = high)

      // Inject a fresh motif ring blooming from the exact tap spot.
      rings.push({ x: ex, y: ey, r: 4, life: 1.2, hue, spokes: 6 });
      if (rings.length > 24) rings.shift();
      // And a lit petal at that pitch/hue.
      petals.push({ norm, hue, pan: x * 2 - 1, life: 1.2 });
      if (petals.length > 28) petals.shift();
      pulse = Math.max(pulse, 1);

      // SONIC BOOST — X->pitch(scale)+pan, Y->octave. Fits the D-minor palette.
      const scale = ["d", "e", "f", "g", "a", "c"];
      const deg = scale[Math.floor(x * (scale.length - 0.001))];
      const oct = 2 + Math.floor((1 - y) * 3); // top = higher octave
      const tone = deg + oct;
      voices.pluck(synth, tone, { beats: 0.6, volume: 0.55, pan: x * 2 - 1 });
      voices.bell(synth, tone, { beats: 0.3, volume: 0.3 * (1 - y), pan: x * 2 - 1 });
    },

    onPaint(api, ctx) {
      const { ink, screen, num, spin, zoom, blur, paste, painting } = api;
      const { pump, band, amp } = ctx;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      // --- Live audio reads ---------------------------------------------------
      const bass = band("subBass");
      const mid = band("mid");
      const air = band("air");

      const downbeat = pulse; // 1 at each note-on, decays
      // ALLEGORY: the bass IS the inhale. Low root -> deeper zoom-bloom breath.
      const breath = Math.min(1.4, bassBreath + bass * 1.4);
      const energy = Math.min(1.6, downbeat * 0.5 + breath * 0.8 + amp * 0.5 + pump * 0.5);

      // --- Seamless phase (wraps every 480 frames → frame 0 == frame N) -------
      const phase = (Number(api.paintCount) % 480) / 480; // paintCount is a Number
      const spinAng = phase * Math.PI * 2; // one full turn per loop
      const baseHue = (phase * 360 + ctx.step * 22) % 360;

      // --- TRANSFORM-FEEDBACK: recycle the whole previous frame into a vortex --
      // The bass drives the zoom depth: a low root inhales the whole field
      // harder, so the bloom visibly BREATHES with the bass. pump intensifies it.
      zoom?.(1.008 + breath * 0.016 + pump * 0.012, 0.5, 0.5); // gentle outward bloom
      spin?.(0.5 + downbeat * 1.1 + pump * 0.6); // rotation kicks on note-on + taps
      ink(6, 3, 16, 40).box(0, 0, w, h); // deep-indigo veil → glowing trails, decays fog

      // --- SUPERSAMPLED SEED MANDALA (rendered at 2×, downscaled → SSAA edges) -
      // A small radially-symmetric mandala is drawn into an offscreen buffer at
      // 2× then pasted back at 1/2 → smooth anti-aliased edges. Rendered CENTERED
      // on a small transparent buffer so, as the whole frame zooms out each
      // frame, successive seeds spiral outward into an infinite vortex. Nodes are
      // OPAQUE + SPARSE so each hue stays pure (translucent multi-hue overlap
      // greys out under the feedback loop). The LIT PETALS from the arpeggio sit
      // at their pitch angle/hue so each arp note reads as a petal lighting up.
      const S = 2; // supersample factor
      const seedSize = Math.floor(Math.min(w, h) * 0.46);
      const bw = seedSize * S, bh = seedSize * S;
      seedBuf = painting(bw, bh, (p) => {
        const scx = bw / 2, scy = bh / 2;
        const maxR = (bw / 2) * 0.94;

        // Base structural mandala — a SPARSE ring of OPAQUE nodes (outer + inner).
        for (let f = 0; f < FOLDS; f++) {
          const fold = (f / FOLDS) * Math.PI * 2 + spinAng * 0.5;
          for (let ringN = 0; ringN < 2; ringN++) {
            const rr = ringN === 0 ? 0.92 : 0.56;
            const swirl = num.wave(phase * 6.28 + f + ringN * 0.9) * 0.3;
            const ang = fold + swirl;
            const radius = maxR * rr;
            const tx = scx + Math.cos(ang) * radius;
            const ty = scy + Math.sin(ang) * radius;
            const hue = (baseHue + f * (360 / FOLDS) + ringN * 40) % 360;
            const [r, g, b] = num.hslToRgb(hue, 96, 56); // vivid, opaque
            const nodeR = (5 + (1 - rr) * 4 + energy * 4 + air * 5) * S;
            p.ink(r, g, b, 255).circle(tx, ty, nodeR, true);
            const [lr, lg, lb] = num.hslToRgb((hue + 30) % 360, 94, 62);
            p.ink(lr, lg, lb, 230).line(scx, scy, tx, ty, (1.5 + mid * 2) * S);
          }
        }

        // ALLEGORY: LIT PETALS — one per recent arp note, at its pitch angle/hue.
        // Bright + OPAQUE the instant it sounds (life ~1), fading as the note dies.
        for (const pet of petals) {
          const life = Math.max(0, Math.min(1.2, pet.life));
          const ringR = maxR * (0.3 + pet.norm * 0.6); // pitch -> radial distance
          const petalAng = pet.norm * Math.PI * 2 + spinAng * 0.5; // pitch -> angle
          const [r, g, b] = num.hslToRgb(pet.hue % 360, 98, 60); // vivid
          for (let f = 0; f < FOLDS; f++) { // mirror across folds → symmetric ring
            const a = petalAng + (f / FOLDS) * Math.PI * 2;
            const tx = scx + Math.cos(a) * ringR;
            const ty = scy + Math.sin(a) * ringR;
            const glow = (6 + life * 14 + air * 8) * S;
            p.ink(r, g, b, 255).circle(tx, ty, glow, true);
            p.ink(r, g, b, Math.min(255, 130 + life * 125)).line(scx, scy, tx, ty, (1 + life * 2) * S);
          }
        }

        // Pulsing heart — the beat you can see; swells on the bass breath. Kept
        // SATURATED + opaque (not near-white) so feedback doesn't wash to gray.
        const [hr, hg, hb] = num.hslToRgb((baseHue + 180) % 360, 92, 60);
        p.ink(hr, hg, hb, 255)
          .circle(scx, scy, (4 + downbeat * 8 + breath * 12) * S, true);
      });
      // Downsample-composite (poor-man's SSAA) centered on screen.
      paste(seedBuf, Math.round(cx - seedSize / 2), Math.round(cy - seedSize / 2), 1 / S);

      // --- MOTIF RINGS: fresh blooms from every beat + every tap --------------
      // Drawn at native res on top so tap-spawned rings visibly bloom outward
      // from the touch point and feed the feedback vortex (zoomed/spun next frame).
      for (const r of rings) {
        const life = Math.max(0, Math.min(1.2, r.life));
        const [rr, gg, bb] = num.hslToRgb(r.hue % 360, 92, 58);
        const a = Math.round(30 + life * 160);
        ink(rr, gg, bb, a).circle(r.x, r.y, r.r, false, 1 + life * 3);
        // A few petal spokes so a ring reads as a motif, not just a circle.
        for (let sp = 0; sp < r.spokes; sp++) {
          const ang = (sp / r.spokes) * Math.PI * 2 + spinAng;
          const px = r.x + Math.cos(ang) * r.r;
          const py = r.y + Math.sin(ang) * r.r;
          ink(rr, gg, bb, Math.round(a * 0.7)).circle(px, py, 2 + life * 4, true);
        }
      }

      // A soft bloom only on big taps — kept rare so the feedback field stays
      // colorful (blur averages toward gray if run every beat).
      if (pump > 1.6) blur?.(1);
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
