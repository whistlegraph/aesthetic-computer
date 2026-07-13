// gloob, 26.07.12
// Gooey metaball-VOICES instrument — a thin wrapper over lib/pads.mjs (the shared
// pad engine: UTC-clock beat grid, `params[0]` rate override e.g. `gloob 0.5`, the
// tap/XY "pump", audio polling). This file only describes what makes gloob gloob:
// its warm squishy score, its richened voices, and its native-res metaball paint.
// ALLEGORY: each metaball charge is a VOICE — it SWELLS + brightens (hue = pitch)
// when its note sounds; charged blobs pull together and MERGE, so you SEE the chord
// as goo gluing into one mass, then splitting as notes release. Bass = field
// viscosity/gloss; the kick jiggles the whole goo with a shockwave. The whole
// screen is a button: tap spawns a hot new blob-voice (X→pan/hue, Y→pitch).

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// One warm gooey loop. Each entry is a note; its index picks which resident blob
// voice sounds. Consecutive beats reuse/overlap voices → they merge into chords.
// Bass anchors low octaves; a soft kick fires on the downbeat of each bar.
const PATTERN = [
  "c2", "g2", "e3", "g2", // bar 1 — bass + a mid bloop
  "a2", "e3", "c3", "g3", // bar 2 — voices climb, some overlap
  "f2", "c3", "a3", "c3", // bar 3
  "g2", "d3", "g3", "e3", // bar 4
];
const BAR = 4; // beats per bar (kick lands on step % BAR === 0)
const VOICE_COUNT = 6; // resident gooey charges (keep modest for fps)

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const oct = parseInt(n.slice(-1), 10) || 3;
  const pc = NOTE_SEMI[n[0]] ?? 0;
  return (oct + 1) * 12 + pc; // ~24..60 across this loop
}

// --- gloob-specific visual state (the engine owns pump/bursts/rhythm) --------
let blobs = []; // resident voices: { hx, hy, x, y, vx, vy, base, charge, hue, note }
let sparks = []; // transient tap-spawned blob-voices: { x, y, vx, vy, charge, life, hue }
let bass = 0; // eased low-end level → field viscosity/gloss
let kick = 0; // shockwave envelope (0..1) → jiggles the whole goo
let kickWave = 0; // expanding shockwave radius (0..1.4)

const CONFIG = {
  bpm: 104,
  steps: PATTERN.length,
  drawBursts: false, // gloob renders taps as its own hot blob-voices in the field
  hooks: {
    onBoot() {
      // Seed the resident charges — 6 gooey voices in a ring that drift + merge.
      const cx = 0.5;
      blobs = [];
      for (let i = 0; i < VOICE_COUNT; i++) {
        const a = (i / VOICE_COUNT) * Math.PI * 2;
        const hx = cx + Math.cos(a) * 0.26;
        const hy = 0.5 + Math.sin(a) * 0.3;
        blobs.push({
          hx, hy,
          x: hx, y: hy,
          vx: 0, vy: 0,
          base: 0.5 + (i % 3) * 0.18, // resting blob size
          charge: 0,
          hue: 320 - (i / VOICE_COUNT) * 150, // magenta→teal candy ramp
          note: null,
        });
      }
      sparks = [];
      bass = 0;
      kick = 0;
      kickWave = 0;
    },

    // A new UTC beat crossed — fire the score; the matching voice's charge SWELLS
    // + brightens (hue ∝ pitch) so the goo lights up on its note.
    onBeat({ idx, synth, pump }) {
      const step = ((idx % PATTERN.length) + PATTERN.length) % PATTERN.length;
      const note = PATTERN[step];
      const v = blobs[step % VOICE_COUNT];
      if (!v) return;

      // Hue tracks pitch across the whole candy wheel so voices read as distinct.
      const tt = Math.max(0, Math.min(1, (notePitch(note) - 24) / (60 - 24)));
      v.hue = ((320 + tt * (200 - 320)) + 360) % 360; // magenta(low)→teal(high)
      v.charge = Math.min(1.6, v.charge + 1.0); // SWELL + glow now
      v.note = note;

      const isBass = (parseInt(note.slice(-1), 10) || 3) <= 2;
      const boost = 1 + Math.min(1, pump * 0.35);
      const pan = (v.x - 0.5) * 1.4;

      if (isBass) {
        // Wet gooey bass → thickens/glosses the whole field.
        voices.sub(synth, note, { beats: 1.9, decay: 0.5, volume: 0.6 * boost, pan });
        bass = Math.min(1.4, bass + 0.7);
      } else {
        // Squelchy bubble core + a bell shimmer → rounded, cute, pitched.
        synth({
          type: "bubble", tone: note, beats: 1.2, attack: 0.02,
          decay: 0.62, volume: 0.42 * boost, pan,
        });
        voices.bell(synth, note, { beats: 0.6, volume: 0.16 * boost, pan });
      }

      // Soft kick on each bar downbeat → shockwave that jiggles the goo.
      if (step % BAR === 0) {
        kick = 1;
        kickWave = 0;
        voices.sub(synth, "c1", { beats: 0.5, attack: 0.005, decay: 0.28, volume: 0.7 * boost });
        synth({ type: "noise", tone: 200, beats: 0.08, attack: 0.001, decay: 0.2, volume: 0.14 * boost });
        for (const b of blobs) { // shove every blob outward → squishy jiggle
          b.vx += (b.x - 0.5) * 0.02;
          b.vy += (b.y - 0.5) * 0.02;
        }
      }
    },

    onSim({ pump, simMs, band }) {
      // Kick + shockwave envelopes.
      kick *= 0.9;
      if (kick < 0.001) kick = 0;
      kickWave = Math.min(1.4, kickWave + 0.045);
      bass *= 0.97;
      if (!Number.isFinite(bass)) bass = 0;
      bass = Math.max(bass, band("subBass") * 1.1); // live low end keeps it breathing

      // Blob physics — drift toward home, wobble, gooey attraction so voices merge.
      const t = simMs * 0.001;
      for (let i = 0; i < blobs.length; i++) {
        const v = blobs[i];
        if (!Number.isFinite(v.x)) { v.x = v.hx; v.y = v.hy; v.vx = 0; v.vy = 0; }
        const ho = 0.06 * (1 + pump * 0.2);
        const tx = v.hx + Math.sin(t * 0.6 + i * 1.7) * ho;
        const ty = v.hy + Math.cos(t * 0.5 + i * 2.3) * ho;
        v.vx += (tx - v.x) * 0.02;
        v.vy += (ty - v.y) * 0.02;
        for (let j = 0; j < blobs.length; j++) { // charged voices pull → chords glue
          if (j === i) continue;
          const o = blobs[j];
          const dx = o.x - v.x, dy = o.y - v.y;
          const d2 = dx * dx + dy * dy + 0.0004;
          const pull = (v.charge + o.charge) * 0.0009 / d2;
          v.vx += dx * pull;
          v.vy += dy * pull;
        }
        v.vx *= 0.86; v.vy *= 0.86;
        v.x += v.vx; v.y += v.vy;
        v.charge *= 0.955; // release
        if (!Number.isFinite(v.charge)) v.charge = 0;
      }

      // Transient tap sparks decay away.
      for (let s = sparks.length - 1; s >= 0; s--) {
        const sp = sparks[s];
        sp.x += sp.vx; sp.y += sp.vy;
        sp.vx *= 0.9; sp.vy *= 0.9;
        sp.charge *= 0.94;
        sp.life -= 0.012;
        if (sp.life <= 0 || sp.charge < 0.03) sparks.splice(s, 1);
      }
    },

    // Tap = a hot new blob-voice at the tapped spot + a bloop (engine already
    // bumped pump). X→pan/hue, Y→pitch (top = higher).
    onTap({ x, y, ex, ey, synth, screen, isDraw, pump }) {
      const hue = x * 360;
      const scale = ["c", "d", "e", "g", "a"];
      const deg = Math.floor((1 - y) * scale.length);
      const oct = 2 + Math.floor((1 - y) * 3);
      const note = scale[Math.max(0, Math.min(scale.length - 1, deg))] + oct;
      sparks.push({
        x, y,
        vx: (Math.random() - 0.5) * 0.004,
        vy: (Math.random() - 0.5) * 0.004,
        charge: isDraw ? 0.8 : 1.5,
        life: 1,
        hue,
      });
      if (sparks.length > 10) sparks.shift();
      bass = Math.min(1.5, bass + (1 - y) * 0.3); // heat the resident goo a touch

      const pan = x * 2 - 1;
      const boost = 1 + Math.min(1, pump * 0.35);
      synth({
        type: "bubble", tone: note, beats: isDraw ? 0.4 : 0.9, attack: 0.005,
        decay: 0.55, volume: (isDraw ? 0.28 : 0.55) * boost, pan,
      });
      voices.bell(synth, note, { beats: 0.3, volume: 0.16 * (0.4 + (1 - y)) * boost, pan });
    },

    // Native-res metaball field written DIRECTLY to screen.pixels. RGB precomputed
    // per charge ONCE outside the pixel loop (never call hslToRgb per pixel).
    onPaint(api, s) {
      const { screen, num } = api;
      const { pump, simMs, quality } = s;
      const { width: w, height: h, pixels } = screen;
      if (!w || !h || !pixels) return;

      // Adaptive cost: compute one pixel per stride×stride block and fill the
      // block so coverage stays full. stride=1 at quality=1 (identical to now).
      const q = quality ?? 1;
      // At full quality render every pixel (identical to the original). As soon as
      // the engine trims quality below full, jump straight to a 3×3 stride — it's a
      // ~9× cost cut that reliably restores 60fps headroom, and the engine will
      // climb quality back toward 1 (stride 1) whenever there's room to spare.
      const stride = q >= 0.98 ? 1 : q >= 0.85 ? 2 : 3;

      // Assemble all charges (resident voices + tap sparks); precompute RGB once.
      const minWH = Math.min(w, h);
      const charges = [];
      for (const v of blobs) {
        const rad = (v.base + v.charge * 0.9) * (1 + pump * 0.12);
        const [cr, cg, cb] = num.hslToRgb(v.hue, 88, 46 + v.charge * 22);
        charges.push({
          x: v.x * w, y: v.y * h,
          r2: Math.max(1, (rad * minWH * 0.16) ** 2),
          hot: v.charge, cr, cg, cb,
        });
      }
      for (const sp of sparks) {
        const rad = 0.35 + sp.charge * 0.8;
        const [cr, cg, cb] = num.hslToRgb(sp.hue, 90, 52 + sp.charge * 18);
        charges.push({
          x: sp.x * w, y: sp.y * h,
          r2: Math.max(1, (rad * minWH * 0.16) ** 2),
          hot: sp.charge * 1.3, cr, cg, cb,
        });
      }
      const nCharges = charges.length;

      // Kick shockwave: a moving ring that adds field + jiggle.
      const shockR = kickWave * minWH * 0.7;
      const shockOn = kick > 0.02;
      const cx = w * 0.5, cy = h * 0.5;

      const visc = 1 + bass * 0.8 + pump * 0.15; // field viscosity/heat → thicker goo
      const gloss = 0.55 + bass * 0.35;          // rim brightness / glossiness
      const jig = (kick * 6) | 0;                // whole-goo jiggle in px

      for (let y = 0; y < h; y += stride) {
        for (let x = 0; x < w; x += stride) {
          let sx = x, sy = y;
          if (jig) {
            sx = x + ((Math.sin(y * 0.09 + simMs * 0.02) * jig) | 0);
            sy = y + ((Math.cos(x * 0.08 + simMs * 0.02) * jig) | 0);
          }

          // Metaball field: sum inverse-square falloff of every charge, hue-colored.
          let field = 0, r = 0, g = 0, b = 0, wsum = 0;
          for (let c = 0; c < nCharges; c++) {
            const ch = charges[c];
            const dx = sx - ch.x, dy = sy - ch.y;
            const d2 = dx * dx + dy * dy + 1;
            const weight = ch.r2 / d2;
            field += weight * visc;
            const cw = weight * (0.6 + ch.hot); // hotter voices contribute more color
            r += ch.cr * cw; g += ch.cg * cw; b += ch.cb * cw; wsum += cw;
          }
          if (shockOn) {
            const dd = num.dist(sx, sy, cx, cy) - shockR;
            field += kick * 3.2 * Math.exp(-(dd * dd) / 260);
          }

          // Resolve this sample to R,G,B (background vs iso-surface body).
          let R, G, B;
          const isBg = field < 0.9 || wsum <= 0;
          if (isBg) {
            const bg = 8 + (y / h) * 14; // deep candy background, not flat black
            R = bg + 6; G = bg * 0.4; B = bg + 18;
          } else {
            // Inside the iso-surface → glossy blob body with a bright rim.
            r /= wsum; g /= wsum; b /= wsum;
            const core = Math.min(1.6, field / 2.2);
            const rim = field > 0.9 && field < 1.5 ? (1.5 - field) * gloss * 220 : 0;
            R = r * core + rim;
            G = g * core + rim * 0.9;
            B = b * core + rim;
            if (field > 3) { const spc = Math.min(80, (field - 3) * 26); R += spc; G += spc; B += spc; }
          }

          // Fill the stride×stride block (clamped at edges); each pixel keeps its
          // own per-pixel ordered dither so texture matches the stride=1 path.
          const yEnd = y + stride < h ? y + stride : h;
          const xEnd = x + stride < w ? x + stride : w;
          for (let by = y; by < yEnd; by++) {
            const row = by * w;
            for (let bx = x; bx < xEnd; bx++) {
              const i = (row + bx) * 4;
              if (isBg) {
                pixels[i] = R; pixels[i + 1] = G; pixels[i + 2] = B;
              } else {
                const dith = ((bx + by) & 1) * 8; // ordered dither for candy texture
                pixels[i] = Math.max(0, Math.min(255, R - dith));
                pixels[i + 1] = Math.max(0, Math.min(255, G - dith * 0.7));
                pixels[i + 2] = Math.max(0, Math.min(255, B - dith * 0.5));
              }
              pixels[i + 3] = 255;
            }
          }
        }
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
