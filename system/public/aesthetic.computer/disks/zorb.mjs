// zorb, 26.07.12
// Reaction-diffusion DUB-TECHNO pad — a thin wrapper over lib/pads.mjs (the shared
// pad engine: UTC-clock beat grid, `params[0]` rate override e.g. `zorb 0.5`, the
// tap/XY "pump", audio polling). This file only describes what makes zorb zorb: its
// deep dub score, its richened voices, and its Gray-Scott Turing-pattern paint.
//
// ALLEGORY — the pattern is ALIVE and the music GROWS it. The sub-bass rides the
// feed rate F: louder bass = more nutrient = the spots/stripes bloom and spread.
// Every note SEEDS a fresh blot of chemical V at a scored spot — you watch it
// diffuse outward and fight the field. The kick is a DIFFUSION PULSE: a ring of
// agitation that ripples the whole tank, stirring the reaction. Deep, organic,
// hypnotic — the visual IS the reaction, and the reaction IS the groove. The whole
// screen is a button: tap injects a spreading blot + a sub thump at the tapped spot.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// Deep dub techno in F minor. Sub root walks slowly; a filtered stab answers
// off the sub; a chord swell breathes under it; hats tick the offbeats. Each
// step names a note; its index also picks WHERE on the field the blot is seeded
// (so the pattern grows from a repeatable, musical set of sites).
const SUB = ["f1", "f1", "f1", "f1", "ab1", "ab1", "c2", "c2"]; // half-time root walk
const STAB = [
  null, "f3", null, "ab3", null, "c4", null, "eb4",
  null, "f3", null, "ab3", null, "eb4", null, "c4",
]; // dubby off-sub stab (rests = null)
const CHORD = ["f2", "ab2", "c3"]; // held Fm swell, breathes across the loop
const STEPS = 16;
const BAR = 4; // kick lands on step % BAR === 0

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  if (!n) return 36;
  const m = /^([a-g])(b?)(\d)$/.exec(n);
  if (!m) return 36;
  const semi = NOTE_SEMI[m[1]] - (m[2] ? 1 : 0);
  return (parseInt(m[3], 10) + 1) * 12 + semi;
}
const pitchNorm = (n) => Math.max(0, Math.min(1, (notePitch(n) - 24) / (60 - 24)));

// --- Reaction-diffusion field (Gray-Scott) ----------------------------------
// Two chemicals U (substrate) + V (activator) on a small grid, scaled by quality.
// U starts full, V empty; seeds inject V. classic spot/worm params.
const DU = 0.16; // U diffusion
const DV = 0.08; // V diffusion
const F_BASE = 0.035; // base feed rate (bass rides this)
const K = 0.062; // kill rate (fixed → Turing spots/worms)

let GW = 0, GH = 0; // current grid dims
let U = null, V = null, Un = null, Vn = null;
let gridQ = -1; // quality bucket the grid was allocated at
let chordVoices = null; // held pad-chord handles

// zorb-specific reactive state (engine owns pump/bursts/rhythm)
let bass = 0; // eased low end → feed-rate lift (growth)
let kick = 0; // diffusion-pulse envelope (0..1)
let kickWave = 0; // expanding agitation ring (0..1.5)
let seedQueue = []; // pending seeds to stamp into the grid on next sim

// Allocate / resize the RD grid. Coarser grid at low quality = big fps win.
function ensureGrid(q) {
  // grid resolution buckets: high q = finer chemistry, low q = coarse.
  const bucket = q >= 0.85 ? 2 : q >= 0.6 ? 1 : 0;
  if (bucket === gridQ && U) return;
  gridQ = bucket;
  const target = bucket === 2 ? 150 : bucket === 1 ? 110 : 80; // longest side cells
  const aspect = 9 / 16; // portrait-ish reels; harmless if square
  GW = target;
  GH = Math.max(40, Math.round(target * (aspect < 1 ? 1.4 : 1)));
  const n = GW * GH;
  U = new Float32Array(n);
  V = new Float32Array(n);
  Un = new Float32Array(n);
  Vn = new Float32Array(n);
  for (let i = 0; i < n; i++) { U[i] = 1; V[i] = 0; }
  // Starter blots spread across the WHOLE field so the pattern reads rich from the
  // opening (RD grows to fill from these, but a sparse start looks empty for bars).
  for (let s = 0; s < 16; s++) {
    stampSeed(0.08 + 0.84 * Math.random(), 0.06 + 0.88 * Math.random(), 2 + ((Math.random() * 3) | 0), 0.6);
  }
}

// Inject a disc of activator V (a "blot") at normalized (nx, ny).
function stampSeed(nx, ny, rad, strength) {
  if (!V) return;
  const gx = Math.round(nx * (GW - 1));
  const gy = Math.round(ny * (GH - 1));
  const r = Math.max(1, rad | 0);
  for (let dy = -r; dy <= r; dy++) {
    const yy = gy + dy;
    if (yy < 0 || yy >= GH) continue;
    for (let dx = -r; dx <= r; dx++) {
      const xx = gx + dx;
      if (xx < 0 || xx >= GW) continue;
      if (dx * dx + dy * dy > r * r) continue;
      const i = yy * GW + xx;
      V[i] = Math.min(1, V[i] + strength);
      U[i] = Math.max(0, U[i] - strength * 0.5);
    }
  }
}

// One Gray-Scott step over the whole grid (wrap-around Laplacian, 5-point).
function stepRD(feed, kill, agitate) {
  if (!U) return;
  const f = Number.isFinite(feed) ? feed : F_BASE;
  const kk = Number.isFinite(kill) ? kill : K;
  for (let y = 0; y < GH; y++) {
    const yl = ((y - 1 + GH) % GH) * GW;
    const yr = ((y + 1) % GH) * GW;
    const yc = y * GW;
    for (let x = 0; x < GW; x++) {
      const xl = (x - 1 + GW) % GW;
      const xr = (x + 1) % GW;
      const i = yc + x;
      const u = U[i], v = V[i];
      // 5-point Laplacian (weights approximating the standard GS kernel).
      const lapU =
        U[yc + xl] + U[yc + xr] + U[yl + x] + U[yr + x] - 4 * u;
      const lapV =
        V[yc + xl] + V[yc + xr] + V[yl + x] + V[yr + x] - 4 * v;
      const uvv = u * v * v;
      let nu = u + (DU * lapU - uvv + f * (1 - u));
      let nv = v + (DV * lapV + uvv - (kk + f) * v);
      // agitation from the kick pulse gives the field a little diffusive kick.
      if (agitate) { nu += lapU * agitate; nv += lapV * agitate; }
      if (!(nu >= 0)) nu = 0; else if (nu > 1) nu = 1; // NaN-safe clamp
      if (!(nv >= 0)) nv = 0; else if (nv > 1) nv = 1;
      Un[i] = nu; Vn[i] = nv;
    }
  }
  const tu = U; U = Un; Un = tu;
  const tv = V; V = Vn; Vn = tv;
}

const CONFIG = {
  bpm: 122, // classic dub-techno tempo
  steps: STEPS,
  drawBursts: false, // zorb renders its own tap blots into the field
  hooks: {
    onBoot({ sound }) {
      sound?.room?.set?.({ enabled: true, mix: 0.45, feedback: 0.72 }); // deep dub space
      bass = 0; kick = 0; kickWave = 0; seedQueue = [];
      gridQ = -1; U = V = Un = Vn = null;
      chordVoices = null;
      ensureGrid(1);
    },

    // A new UTC beat crossed — fire the dub score + seed a fresh blot per note.
    onBeat({ idx, synth }) {
      const step = ((idx % STEPS) + STEPS) % STEPS;

      // Held Fm chord swell — a slow breathing bed. Start lazily (synth is only
      // valid inside beat/tap hooks); retrigger each loop, killing the prior
      // (infinite 🔁) handles first so voices don't stack.
      if (!chordVoices || step === 0) {
        if (chordVoices) for (const v of chordVoices) v?.kill?.(0.6);
        chordVoices = voices.padChord(synth, CHORD, {
          type: "sawtooth", volume: 0.05, attack: 1.2, decay: 1.0, spread: 0.4,
        });
      }

      // SUB BASS (half-time) — the growth engine. Louder sub = more feed = spread.
      if (step % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % SUB.length) + SUB.length) % SUB.length;
        const sn = SUB[bi];
        voices.sub(synth, sn, { beats: 1.9, attack: 0.02, decay: 0.6, volume: 0.62 });
        bass = Math.min(1.4, bass + 0.85);
        // sub seeds a low, wide blot near the base of the field
        seedQueue.push({ nx: 0.5 + Math.sin(idx * 1.3) * 0.42, ny: 0.55 + Math.cos(idx * 0.7) * 0.32, rad: 4, str: 0.7 });
      }

      // FILTERED DUB STAB — off the sub, panned, short & resonant.
      const stab = STAB[step];
      if (stab) {
        const pn = pitchNorm(stab);
        const pan = Math.sin((step / STEPS) * Math.PI * 2) * 0.6;
        voices.pluck(synth, stab, { beats: 0.7, attack: 0.004, decay: 0.4, volume: 0.34, pan });
        voices.bell(synth, stab, { beats: 0.5, volume: 0.12, pan: -pan });
        // stab seeds a bright blot; hue-height maps to pitch (high = top).
        seedQueue.push({ nx: 0.5 + pan * 0.5, ny: 0.2 + (1 - pn) * 0.5, rad: 3, str: 0.85 });
      }

      // KICK = a DIFFUSION PULSE that ripples the whole tank (dub 4-on-floor-ish).
      if (step % BAR === 0) {
        kick = 1; kickWave = 0;
        voices.sub(synth, "f1", { beats: 0.5, attack: 0.005, decay: 0.26, volume: 0.7 });
        synth({ type: "noise-white", tone: 150, beats: 0.06, attack: 0.001, decay: 0.18, volume: 0.12 });
      }

      // Offbeat dub hats — the tick that swings the groove.
      if (step % 2 === 1) {
        voices.hat(synth, { tone: 7500, beats: 0.1, volume: 0.14, pan: (step % 4 === 1 ? 0.4 : -0.4) });
      }
    },

    onSim({ band, simMs }) {
      // Envelopes: kick pulse + agitation ring, bass ease toward live low end.
      kick *= 0.9; if (kick < 0.001) kick = 0;
      kickWave = Math.min(1.5, kickWave + 0.05);
      bass *= 0.95; if (!Number.isFinite(bass)) bass = 0;
      bass = Math.max(bass, band("subBass") * 1.2); // live sub keeps growth breathing

      // Stamp any queued musical seeds into the grid before stepping.
      for (const sd of seedQueue) stampSeed(sd.nx, sd.ny, sd.rad, sd.str);
      seedQueue.length = 0;

      // ALLEGORY: bass rides the feed rate → louder low end grows the pattern.
      const feed = F_BASE + bass * 0.014; // 0.035 (quiet) .. ~0.055 (booming)
      // Kick injects a brief burst of extra diffusion → the whole field ripples.
      const agitate = kick * 0.14;
      // Two RD substeps per frame → the reaction actually evolves at tempo.
      stepRD(feed, K, agitate);
      stepRD(feed, K, agitate * 0.6);
    },

    // Tap = inject a spreading blot at the tapped point + a sub thump.
    onTap({ x, y, synth, isDraw }) {
      stampSeed(x, y, isDraw ? 3 : 5, isDraw ? 0.7 : 1.0);
      bass = Math.min(1.5, bass + (isDraw ? 0.2 : 0.6));
      const scale = ["f", "ab", "c", "eb", "f"];
      const deg = Math.max(0, Math.min(scale.length - 1, Math.floor(x * scale.length)));
      const oct = 1 + Math.floor((1 - y) * 2);
      const note = scale[deg] + oct;
      const pan = x * 2 - 1;
      voices.sub(synth, note, { beats: isDraw ? 0.5 : 1.1, attack: 0.006, decay: 0.4, volume: (isDraw ? 0.4 : 0.65), pan });
      if (!isDraw) voices.pluck(synth, scale[deg] + (oct + 2), { beats: 0.5, volume: 0.28, pan });
    },

    // Render the V (activator) field to screen.pixels — the Turing pattern IS the
    // picture. Strided blocks + a coarser grid at low quality hold 60fps. hslToRgb
    // is only ever called to build a small ramp LUT — NEVER per pixel.
    onPaint(api, s) {
      const { screen, num } = api;
      const { pump, quality } = s;
      const { width: w, height: h, pixels } = screen;
      if (!w || !h || !pixels || !V) return;

      const q = quality ?? 1;
      ensureGrid(q); // may swap the grid to a coarser/finer resolution
      if (!V) return;

      // Pixel stride: bigger blocks when the engine trims quality.
      const stride = q >= 0.98 ? 1 : q >= 0.85 ? 2 : 3;

      // Deep dub color: hue drifts slowly with bass; build a 64-entry ramp LUT so
      // hslToRgb runs 64× per frame, not per pixel.
      const baseHue = (180 + bass * 60) % 360; // teal→magenta as the bass swells
      const LUT_N = 64;
      const lutR = new Uint8Array(LUT_N);
      const lutG = new Uint8Array(LUT_N);
      const lutB = new Uint8Array(LUT_N);
      for (let i = 0; i < LUT_N; i++) {
        const t = i / (LUT_N - 1); // 0 = substrate/dark .. 1 = dense activator/hot
        const hue = (baseHue + t * 90) % 360; // ramp across the tank
        const light = 6 + t * t * 66; // dark background → bright vein
        const sat = 60 + t * 35;
        const [r, g, b] = num.hslToRgb(hue, sat, light);
        lutR[i] = r; lutG[i] = g; lutB[i] = b;
      }

      // Kick pulse ring: brighten a moving ring so the diffusion pulse READS.
      const shockR = kickWave * Math.min(w, h) * 0.75;
      const shockOn = kick > 0.02;
      const cx = w * 0.5, cy = h * 0.5;
      const glow = 1 + pump * 0.2 + bass * 0.25;

      const gx1 = GW - 1, gy1 = GH - 1;
      for (let y = 0; y < h; y += stride) {
        const gy = (y / h) * gy1;
        const gyi = gy | 0;
        const gyf = gy - gyi;
        const row0 = gyi * GW;
        const row1 = (gyi < gy1 ? gyi + 1 : gyi) * GW;
        for (let x = 0; x < w; x += stride) {
          const gxf0 = (x / w) * gx1;
          const gxi = gxf0 | 0;
          const gxf = gxf0 - gxi;
          const gxr = gxi < gx1 ? gxi + 1 : gxi;
          // Bilinear sample of V so the coarse grid reads smooth on screen.
          const v00 = V[row0 + gxi], v10 = V[row0 + gxr];
          const v01 = V[row1 + gxi], v11 = V[row1 + gxr];
          const vt = v00 + (v10 - v00) * gxf;
          const vb = v01 + (v11 - v01) * gxf;
          let vv = vt + (vb - vt) * gyf;
          if (!(vv >= 0)) vv = 0; else if (vv > 1) vv = 1;

          // Map activator density → LUT (a little gamma to fatten the veins).
          let t = Math.pow(vv, 0.75) * glow;
          if (t > 1) t = 1;
          const li = (t * (LUT_N - 1)) | 0;
          let R = lutR[li], G = lutG[li], B = lutB[li];

          // Kick ring: add a bright agitation halo where the pulse is passing.
          if (shockOn) {
            const dd = num.dist(x, y, cx, cy) - shockR;
            const ring = kick * 150 * Math.exp(-(dd * dd) / 520);
            R = R + ring; G = G + ring * 0.9; B = B + ring;
          }

          // Fill the stride×stride block (clamped at edges) with an ordered dither.
          const yEnd = y + stride < h ? y + stride : h;
          const xEnd = x + stride < w ? x + stride : w;
          for (let by = y; by < yEnd; by++) {
            const prow = by * w;
            for (let bx = x; bx < xEnd; bx++) {
              const idx = (prow + bx) * 4;
              const dith = ((bx + by) & 1) * 6;
              pixels[idx] = R > 255 ? 255 : R < dith ? 0 : R - dith;
              pixels[idx + 1] = G > 255 ? 255 : G < dith ? 0 : G - dith;
              pixels[idx + 2] = B > 255 ? 255 : B < dith ? 0 : B - dith;
              pixels[idx + 3] = 255;
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
