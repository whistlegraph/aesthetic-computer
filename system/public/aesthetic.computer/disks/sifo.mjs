// sifo, 26.07.12
// Water caustics × steel-pan — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `sifo 0.5`, the
// tap/XY "pump", audio polling). This file only describes what makes sifo sifo:
// its tropical steel-pan/marimba voices and its per-pixel WATER CAUSTIC field
// written DIRECTLY into screen.pixels at native res.
//
// ALLEGORY (the visual IS the score) — sunlight refracted on a pool floor:
//  • CAUSTIC LIGHT WEBS ripple across the frame — a few animated abs/sine
//    "veins" summed per pixel; bright where veins CROSS (the shimmering net).
//  • Each NOTE sends a caustic RIPPLE/BRIGHTENING from an origin (step→origin,
//    pitch→hue) — you SEE the note bloom the caustic web.
//  • BASS = the DEEP WATER SWELL that warps the whole caustic field (slow
//    displacement) — the low note bends the light.
//  • BEAT = a bright GLINT SWEEP crossing the surface.
//
// PER-PIXEL NOTE: the caustic field is a PURE FUNCTION of (x,y,time,music)
// written straight to screen.pixels at density-3's small logical resolution
// (~360×640, the per-pixel sweet spot). NO resolution() pin. Color comes from a
// precomputed HUE LUT (aqua→turquoise→gold over deep blue) — NEVER a per-pixel
// hslToRgb. Cost scales with ctx.quality via STRIDE + block fill.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// —— Score (UTC-driven) — a bright tropical pentatonic that loops seamlessly ——
// Higher index within a phrase = brighter caustic (gold); lower = aqua.
const MELODY = ["d4", "a3", "f4", "a4", "g4", "e4", "d4", "a3"];
const BASS = ["d2", "d2", "bb1", "bb1", "c2", "c2", "a1", "a1"]; // deep swell root

// —— pitch → scalar (0 low .. 1 high) for hue + ripple origin ——
const NOTE_ORDER = ["c", "d", "e", "f", "g", "a", "b"];
function noteToScalar(note) {
  const m = /([a-g])(#|b)?(\d)/.exec(note);
  if (!m) return 0.5;
  let semis = NOTE_ORDER.indexOf(m[1]);
  if (m[2] === "#") semis += 0.5;
  else if (m[2] === "b") semis -= 0.5;
  const oct = +m[3];
  const v = oct * 7 + semis; // rough pitch ordinal
  return Math.max(0, Math.min(1, (v - 21) / 12)); // a3..a4 → ~0..1
}

// —— HUE LUT: deep-blue → turquoise → aqua → gold, indexed by caustic
// brightness (0..255). Precomputed ONCE so the per-pixel loop never touches
// hslToRgb. Deep troughs = dark blue water; bright vein crossings = aqua→gold. ——
const LUT_R = new Uint8ClampedArray(256);
const LUT_G = new Uint8ClampedArray(256);
const LUT_B = new Uint8ClampedArray(256);
function buildLUT() {
  for (let i = 0; i < 256; i++) {
    const t = i / 255; // 0 deep water .. 1 hottest glint
    let r, g, b;
    if (t < 0.5) {
      // deep navy → turquoise
      const u = t / 0.5;
      r = 4 + u * 20;
      g = 18 + u * 150;
      b = 60 + u * 150;
    } else if (t < 0.82) {
      // turquoise → bright aqua
      const u = (t - 0.5) / 0.32;
      r = 24 + u * 90;
      g = 168 + u * 70;
      b = 210 + u * 30;
    } else {
      // aqua → gold glint (caustic crossings)
      const u = (t - 0.82) / 0.18;
      r = 114 + u * 141;
      g = 238 + u * 17;
      b = 240 - u * 130;
    }
    LUT_R[i] = r;
    LUT_G[i] = g;
    LUT_B[i] = b;
  }
}
buildLUT();

// —— sifo-specific state (the engine owns pump/rhythm/beat grid) ——
let phase = 0; // ever-advancing caustic flow phase
let swell = 0; // eased bass swell → warps the field
let noteBright = 0; // eased overall caustic brightening (decays after onsets)
let glint = 0; // beat glint-sweep energy (decays)
let glintX = 0; // glint sweep position 0..1 (advances each beat)
let ripples = []; // caustic ripples { x, y (0..1), r, life, hue(0..1), amp }
let bursts = []; // crisp native-res tap flashes { x, y (px), r, life, hue(0..1) }

const CONFIG = {
  bpm: 96,
  steps: MELODY.length,
  drawBursts: false, // sifo draws its own native-res tap flashes in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.42, feedback: 0.55 }); // pool reverb
    },

    // A new UTC beat crossed — fire the steel-pan note + deep-swell bass, brighten
    // the caustics, emit a ripple from a pitch-placed origin, and sweep a glint.
    onBeat({ idx, synth }) {
      const step = ((idx % MELODY.length) + MELODY.length) % MELODY.length;
      const note = MELODY[step];
      const bass = BASS[step];
      const s = noteToScalar(note); // 0 low(aqua) .. 1 high(gold)
      const pan = Math.sin((step / MELODY.length) * Math.PI * 2) * 0.6;

      // RICH steel-pan / marimba voice stack (no gm):
      //  • bell = the bright metallic overtone ping.
      //  • pluck = the marimba-ish mallet body.
      //  • a resonant DETUNED SINE pair = the steel-pan shimmer / ring.
      voices.bell(synth, note, { beats: 0.9, volume: 0.26, pan });
      voices.pluck(synth, note, { beats: 0.8, decay: 0.55, volume: 0.28, pan });
      // Detuned twin sines → shimmering steel-pan ring.
      synth({ tone: note, type: "sine", beats: 1.1, attack: 0.004, decay: 0.7, volume: 0.16, pan });
      synth({ tone: note, type: "triangle", beats: 0.7, attack: 0.003, decay: 0.55, volume: 0.12 * (0.5 + s), pan: -pan });
      // Bright octave sparkle on high notes.
      if (s > 0.5) {
        const up = note.replace(/\d/, (d) => String(+d + 1));
        voices.bell(synth, up, { beats: 0.5, volume: 0.12 * s, pan: -pan });
      }

      // BASS on the half-beat = the deep water swell that warps the caustics.
      if (step % 2 === 0) {
        voices.sub(synth, bass, { beats: 1.8, attack: 0.14, decay: 0.7, volume: 0.5, pan: pan * 0.5 });
      }
      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.12 });

      // Caustic BRIGHTENING + a ripple from a pitch-placed origin (step→x, pitch→y).
      noteBright = 1;
      ripples.push({
        x: 0.15 + (step / MELODY.length) * 0.7,
        y: 0.2 + (1 - s) * 0.6,
        r: 0.02,
        life: 1,
        hue: s,
        amp: 0.9,
      });

      // BEAT glint sweep — a bright band crosses the surface each beat.
      glint = 1;
      glintX = (step / MELODY.length + 0.5) % 1;
    },

    // Ease swell + note-brightening; decay glint; advance ripples/bursts/flow.
    onSim({ num, screen, band, pump }) {
      const bass = band("subBass");
      swell = num.lerp(swell, bass, 0.07);
      if (!Number.isFinite(swell)) swell = 0;

      noteBright = Math.max(0, noteBright - 0.03);
      glint = Math.max(0, glint - 0.045);

      for (const rp of ripples) {
        rp.r += 0.011 + rp.amp * 0.007;
        rp.life -= 0.028;
      }
      ripples = ripples.filter((rp) => rp.life > 0);
      if (ripples.length > 20) ripples = ripples.slice(-20);

      const w = screen?.width || 1;
      for (const b of bursts) {
        b.r += w * 0.02;
        b.life -= 0.04;
      }
      bursts = bursts.filter((b) => b.life > 0);

      phase += 0.018 + pump * 0.012 + swell * 0.01; // swell speeds the flow
    },

    // Tap = a bright caustic ripple from the tap point + a steel-pan note
    // (X→pan/hue, Y→pitch). Engine already bumped pump + pushed the shared burst.
    onTap({ x, y, ex, ey, isDraw, synth }) {
      noteBright = Math.min(1.4, noteBright + (isDraw ? 0.2 : 0.8));
      const hue = x; // X → hue along the aqua→gold LUT tail

      ripples.push({ x, y, r: 0.02, life: 1, hue, amp: isDraw ? 1.1 : 1.9 });
      bursts.push({ x: ex, y: ey, r: 4, life: 1, hue });

      // Steel-pan note — X→pan/hue, Y→pitch (top = high).
      const scale = ["d", "e", "g", "a", "c"];
      const oct = 3 + Math.floor((1 - y) * 3); // top = higher octave
      const tone = scale[Math.floor(x * 5) % 5] + oct;
      voices.bell(synth, tone, { beats: isDraw ? 0.4 : 0.8, volume: (isDraw ? 0.2 : 0.34) * (0.6 + (1 - y) * 0.5), pan: x * 2 - 1 });
      voices.pluck(synth, tone, { beats: isDraw ? 0.3 : 0.6, decay: 0.55, volume: isDraw ? 0.16 : 0.3, pan: x * 2 - 1 });
      synth({ tone, type: "sine", beats: isDraw ? 0.4 : 0.9, attack: 0.004, decay: 0.6, volume: isDraw ? 0.1 : 0.16, pan: x * 2 - 1 });
    },

    // The per-pixel WATER CAUSTIC field, written DIRECTLY to native screen.pixels.
    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, step, beatProgress, band, amp, quality } = s;
      const { width: w, height: h } = screen;

      // ADAPTIVE QUALITY: caustics are a per-pixel field written straight to
      // screen.pixels, so cost scales with pixel count. When the engine lowers
      // ctx.quality (over the 60fps budget) we STRIDE the loop and fill each
      // stride×stride block from one computed pixel — coverage stays full, cost
      // drops ~stride². At quality==1 stride=1; the caustic math (a few
      // abs(sine) veins + ripple loop) is lighter than molten's plasma, so the
      // mid bands are viable: 3 / 2 / 1.
      const stride = quality < 0.55 ? 3 : quality < 0.8 ? 2 : 1;

      const bass = band("subBass");
      const disp = 0.04 + (swell + pump * 0.4) * 0.08; // deep swell warps the field
      const nb = noteBright; // caustic brightening from note onsets
      const flow = phase;
      const cycle = (step + beatProgress) / MELODY.length; // 0..1
      const glintPos = glintX; // 0..1 sweep center
      const glintE = glint; // sweep energy

      const pix = screen.pixels;

      for (let y = 0; y < h; y += stride) {
        const fyN = y / h;
        // deep-swell vertical warp — the low note bends the light.
        const wy = fyN + Math.sin(fyN * 6.0 + flow * 0.8) * disp;
        const ry = wy * Math.PI * 2;
        const yEnd = y + stride < h ? y + stride : h;
        const rowBase = y * w;
        for (let x = 0; x < w; x += stride) {
          const fxN = x / w;
          const wx = fxN + Math.sin(fxN * 5.0 - flow) * disp;
          const rx = wx * Math.PI * 2;

          // CAUSTIC VEINS: a few animated abs(sine) sheets. Each is bright along a
          // thin moving line; the caustic web is BRIGHT WHERE VEINS CROSS, so we
          // multiply/sum ridge terms (1 - |sin|) that spike near sheet lines.
          const v1 = 1 - Math.abs(Math.sin(rx * 1.7 + wy * 2.3 + flow * 1.2));
          const v2 = 1 - Math.abs(Math.sin(ry * 1.9 - wx * 1.6 - flow * 0.9));
          const v3 = 1 - Math.abs(Math.sin((rx + ry) * 1.3 + flow * 0.7));
          const v4 = 1 - Math.abs(Math.sin((rx - ry) * 2.1 - flow * 1.4));
          // crossings glow: pairwise products spike only where two sheets meet.
          let c = v1 * v2 + v3 * v4 + (v1 * v3 + v2 * v4) * 0.5;
          c = c * 0.5; // → ~0..1 typical

          // Ripple shocks: a bright ring pulse near each expanding radius.
          let ring = 0;
          for (let k = 0; k < ripples.length; k++) {
            const rp = ripples[k];
            const dx = fxN - rp.x;
            const dy = fyN - rp.y;
            const d = Math.sqrt(dx * dx + dy * dy);
            const rr = d - rp.r;
            if (rr > -0.07 && rr < 0.07) {
              ring += Math.cos((rr / 0.07) * Math.PI * 0.5) * rp.life * rp.amp;
            }
          }

          // GLINT SWEEP: a bright vertical band crossing the surface (beat).
          let gl = 0;
          if (glintE > 0.01) {
            const gd = Math.abs(fxN - glintPos);
            if (gd < 0.14) gl = (1 - gd / 0.14) * glintE * 0.7;
          }

          // Caustic intensity 0..~1.4: the web (c) everywhere, brightened by note
          // onsets, ripples, glint, pump + bass swell. NaN-guarded.
          let intensity = c * (0.55 + nb * 0.5) + ring * 0.8 + gl + pump * 0.12 + bass * 0.15 + amp * 0.06;
          if (!(intensity >= 0)) intensity = 0.2;
          if (intensity > 1) intensity = 1;

          // Color from the precomputed LUT (deep blue → turquoise → aqua → gold).
          const li = (intensity * 255) | 0;
          const cr = LUT_R[li];
          const cg = LUT_G[li];
          const cb = LUT_B[li];

          // Fill the stride×stride block from this computed pixel (edge-clamped).
          const xEnd = x + stride < w ? x + stride : w;
          if (stride === 1) {
            const di = (rowBase + x) * 4;
            pix[di] = cr;
            pix[di + 1] = cg;
            pix[di + 2] = cb;
            pix[di + 3] = 255;
          } else {
            for (let by = y; by < yEnd; by++) {
              const bBase = by * w;
              for (let bx = x; bx < xEnd; bx++) {
                const di = (bBase + bx) * 4;
                pix[di] = cr;
                pix[di + 1] = cg;
                pix[di + 2] = cb;
                pix[di + 3] = 255;
              }
            }
          }
        }
      }

      // —— CRISP NATIVE-RES OVERLAYS ON TOP ——

      // Ripple pulses as expanding aqua/gold rings (native res).
      for (const rp of ripples) {
        if (rp.life < 0.12) continue;
        const li = Math.min(255, Math.round(150 + rp.hue * 105));
        const rad = Math.min(rp.r, 0.5) * Math.max(w, h);
        ink(LUT_R[li], LUT_G[li], LUT_B[li], 150 * rp.life).circle(rp.x * w, rp.y * h, rad, false, 2);
      }

      // Note origin marker — a small bright glyph where the current note bloomed.
      const cs = noteToScalar(MELODY[step]);
      const ox = (0.15 + (step / MELODY.length) * 0.7) * w;
      const oy = (0.2 + (1 - cs) * 0.6) * h;
      const oli = Math.min(255, Math.round(170 + cs * 85));
      const oR = Math.min(w, h) * 0.02 * (1 + nb * 1.2 + pump * 0.3);
      ink(LUT_R[oli], LUT_G[oli], LUT_B[oli], 90 + nb * 130).circle(ox, oy, oR, true);
      ink(255, 255, 250, 110 + nb * 120).circle(ox, oy, oR * 0.5, true);

      // Tap bursts as bright expanding flashes at the touch point.
      for (const b of bursts) {
        const li = Math.min(255, Math.round(160 + b.hue * 95));
        ink(LUT_R[li], LUT_G[li], LUT_B[li], 200 * b.life).circle(b.x, b.y, b.r, false, 3);
        ink(255, 255, 255, 160 * b.life).circle(b.x, b.y, b.r * 0.4, true);
      }
    },
  },
};

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before the engine
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  phase = 0;
  swell = 0;
  noteBright = 0;
  glint = 0;
  ripples = [];
  bursts = [];
  padBoot(api);
}

export { boot, sim, paint, act };
