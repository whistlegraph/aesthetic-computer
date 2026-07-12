// molten, 26.07.12
// Warm liquid molten field — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `molten 0.5`, the
// tap/XY "pump", audio polling). This file only describes what makes molten
// molten: its warm melody + bass swell, and its per-pixel LIQUID DISPLACEMENT
// written DIRECTLY into screen.pixels at native res.
//
// ALLEGORY (the visual IS the score):
//  • MELODY note = the molten GLOW's vertical position + hue (pitch→height,
//    high note = high + bright). You can READ the melody as the glow rising.
//  • BASS = a low swell that energizes the whole field.
//  • NOTE-ONSET = a ripple pulse emitted from the glow.
//
// PER-PIXEL NOTE: the field is a PURE FUNCTION of (x,y,time,music) written
// straight to screen.pixels at density-3's already-small logical resolution
// (~360×640, the per-pixel sweet spot). NO resolution() pin. An offscreen
// page()/paste buffer was tried first but paste of a custom buffer rendered
// black in the capture path, so we KEEP direct native-res screen.pixels.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// —— Score (UTC-driven) ——
// A minor-pentatonic-ish melody that resolves back to the start so the loop is
// seamless. Higher index = higher pitch = higher + brighter glow.
const MELODY = ["a3", "c4", "e4", "g4", "a4", "g4", "e4", "d4"];
const BASS = ["a1", "a1", "c2", "e2", "d2", "d2", "f2", "e2"];

// —— pitch → glow height + hue ——
const NOTE_ORDER = ["c", "d", "e", "f", "g", "a", "b"];
function noteToScalar(note) {
  const m = /([a-g])(#?)(\d)/.exec(note);
  if (!m) return 0.5;
  const semis = NOTE_ORDER.indexOf(m[1]) + (m[2] ? 0.5 : 0);
  const oct = +m[3];
  const v = oct * 7 + semis; // rough pitch ordinal
  return Math.max(0, Math.min(1, (v - 21) / 12)); // a3..a4 → 0..1
}

// —— molten-specific state (the engine owns pump/rhythm/beat grid) ——
let prev = null; // snapshot of last frame's plasma (light temporal blend)
let noteHeight = 0.5; // glow vertical target (from current pitch)
let noteHue = 30; // glow hue target (from current pitch)
let easedHeight = 0.5; // eased glow height
let easedHue = 30; // eased glow hue
let phase = 0; // ever-advancing flow phase
let swell = 0; // eased bass swell
let onsetPulse = 0; // decays after each note onset → ripple + brightness
let ripples = []; // molten shocks { x, y (0..1), r, life, hue, amp }
let bursts = []; // crisp native-res tap flashes { x, y (px), r, life, hue }

const CONFIG = {
  bpm: 60,
  steps: MELODY.length,
  drawBursts: false, // molten draws its own native-res tap flashes in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.5, feedback: 0.6 }); // lush tail
    },

    // A new UTC beat crossed — fire the melody note + bass, set the glow target
    // (pitch→height+hue), emit a ripple (the visible birth of the audible onset).
    onBeat({ idx, synth }) {
      const step = ((idx % MELODY.length) + MELODY.length) % MELODY.length;
      const note = MELODY[step];
      const bass = BASS[step];

      // Melody → the glow. pitch → height + hue.
      const s = noteToScalar(note);
      noteHeight = 0.18 + (1 - s) * 0.64; // high note = high on screen (low y)
      noteHue = 20 + s * 60; // low→warm red/orange, high→bright yellow
      onsetPulse = 1;

      const pan = Math.sin(idx * 0.4) * 0.3;

      // Warm melody: a breathy waveguide flute lead + a soft plucked body.
      voices.flute(synth, note, { beats: 1.4, attack: 0.05, decay: 0.7, volume: 0.34, pan });
      voices.pluck(synth, note, { beats: 0.9, decay: 0.6, volume: 0.22, pan });
      // A brighter octave-up sparkle on the higher notes.
      if (s > 0.5) {
        const up = note.replace(/\d/, (d) => String(+d + 1));
        voices.pluck(synth, up, { beats: 0.6, decay: 0.5, volume: 0.14 * s, pan: -pan });
      }

      // Bass → the global displacement amplitude / a low swell: warm sub + body.
      voices.sub(synth, bass, { beats: 1.8, attack: 0.16, decay: 0.7, volume: 0.5, pan: pan * 0.6 });

      // Ripple pulse emanating from the glow position (normalized coords).
      ripples.push({ x: 0.5, y: easedHeight, r: 0.02, life: 1, hue: noteHue, amp: 0.8 });
    },

    // Ease glow toward the note target; decay onset; advance ripples/bursts/flow.
    // (pump + tap-burst PUMP energy are owned by the engine; read via ctx.)
    onSim({ num, screen, band, pump }) {
      const bass = band("subBass");
      swell = num.lerp(swell, bass, 0.08);

      // KEEP the NaN guards — a bad eased value can never blank the frame.
      easedHeight = num.lerp(easedHeight, noteHeight, 0.14);
      if (!Number.isFinite(easedHeight)) easedHeight = 0.5;
      easedHue = num.lerp(easedHue, noteHue, 0.12);
      if (!Number.isFinite(easedHue)) easedHue = 30;

      onsetPulse = Math.max(0, onsetPulse - 0.03);

      for (const rp of ripples) {
        rp.r += 0.012 + rp.amp * 0.008;
        rp.life -= 0.03; // shorter life → clean shock, not a sprawling arc
      }
      ripples = ripples.filter((rp) => rp.life > 0);
      const w = screen?.width || 1;
      for (const b of bursts) {
        b.r += w * 0.02;
        b.life -= 0.04;
      }
      bursts = bursts.filter((b) => b.life > 0);

      phase += 0.02 + pump * 0.01;
    },

    // Tap = punch a ripple + shock into the molten field at the tap point + a
    // warm boom (engine already bumped pump + pushed the shared burst; X→pan/hue,
    // Y→pitch). We push our OWN crisp native-res flash + a bespoke molten shock.
    onTap({ x, y, ex, ey, isDraw, synth }) {
      onsetPulse = Math.min(1.4, onsetPulse + (isDraw ? 0.15 : 0.7));
      const hue = 20 + (1 - y) * 100;

      // Molten shock in the field at the tap point.
      ripples.push({ x, y, r: 0.02, life: 1, hue, amp: isDraw ? 1.0 : 1.8 });
      // Crisp native-res flash.
      bursts.push({ x: ex, y: ey, r: 4, life: 1, hue });

      // SONIC BOOM — X→pan/hue, Y→pitch (top = high). Warm palette.
      const scale = ["c", "d", "e", "g", "a"];
      const oct = 2 + Math.floor((1 - y) * 3); // top = higher octave
      const tone = scale[Math.floor(x * 5) % 5] + oct;
      voices.pluck(synth, tone, {
        beats: isDraw ? 0.3 : 0.7,
        decay: 0.6,
        volume: (isDraw ? 0.3 : 0.55) * (0.6 + (1 - y) * 0.5),
        pan: x * 2 - 1,
      });
      // A low warm boom body — the "shock" you feel.
      voices.sub(synth, scale[Math.floor(x * 5) % 5] + "1", {
        beats: isDraw ? 0.25 : 0.55,
        decay: 0.5,
        volume: isDraw ? 0.2 : 0.42,
        pan: x * 2 - 1,
      });
    },

    // The per-pixel molten field, written DIRECTLY to native screen.pixels.
    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, step, beatProgress, band, amp } = s;
      const { width: w, height: h } = screen;

      const beatPulse = 1 - beatProgress;
      const flow = phase;
      const cyclePhase = (step + beatProgress) / MELODY.length; // 0..1
      const rot = cyclePhase * Math.PI * 2;
      const disp = 0.05 + (swell + pump * 0.4 + beatPulse * 0.15) * 0.06;
      const gyN = Number.isFinite(easedHeight) ? easedHeight : 0.5; // glow y, 0..1
      const gh = Number.isFinite(easedHue) ? easedHue : 30; // glow hue
      const pumpBright = Math.min(1, pump * 0.5); // taps brighten the whole field
      const blend = 0.28; // light temporal smoothing — never accumulates

      // —— DIRECT PROCEDURAL MOLTEN PLASMA (native screen.pixels, no runaway) ——
      // The field is a PURE FUNCTION of (x,y,time,music): layered sines DISPLACED
      // by a traveling flow form warm molten ridges; the melody GLOW brightens a
      // horizontal band (pitch→height, hue→color); ripples add bright rings. A
      // light temporal blend (prev) softens the flow but can never flood.
      const pix = screen.pixels;
      if (!prev || prev.length !== pix.length) {
        prev = new Uint8ClampedArray(pix.length);
      }

      for (let y = 0; y < h; y++) {
        const fyN = y / h;
        const ry = fyN * Math.PI * 2;
        const distG = Math.abs(fyN - gyN);
        const bandBright = Math.max(0, 1 - distG * 4.5);
        const dispY = Math.sin(ry * 3.0 + flow + rot) * disp;
        const rowBase = y * w;
        for (let x = 0; x < w; x++) {
          const fxN = x / w;
          const rx = fxN * Math.PI * 2;
          const dispX = Math.sin(rx * 2.4 + flow * 1.3 - rot) * disp;
          const px = fxN + dispX;
          const py = fyN + dispY;

          const wx = px * Math.PI * 2;
          const wy = py * Math.PI * 2;
          let v =
            Math.sin(wx * 1.6 + flow * 1.4) * 0.5 +
            Math.sin(wy * 2.0 - flow) * 0.5 +
            Math.sin((wx + wy) * 1.2 + rot) * 0.4 +
            Math.sin((wx - wy) * 2.4 - flow * 0.6) * 0.3;
          v = v * 0.6 + 0.5; // → ~0..1
          if (v < 0) v = 0;
          else if (v > 1) v = 1;
          v = v * v * (3 - 2 * v); // smoothstep → punchier ridges

          // Ripple shocks: a bright ring pulse near each expanding radius.
          let ring = 0;
          for (let k = 0; k < ripples.length; k++) {
            const rp = ripples[k];
            const dx = fxN - rp.x;
            const dy = fyN - rp.y;
            const d = Math.sqrt(dx * dx + dy * dy);
            const rr = d - rp.r;
            if (rr > -0.08 && rr < 0.08) {
              ring += Math.cos((rr / 0.08) * Math.PI * 0.5) * rp.life * rp.amp;
            }
          }

          // Molten intensity 0..~1.2: flowing ridges (v) EVERYWHERE, boosted into
          // a bright hot river around the melody's height (bandBright), plus
          // transient ripples + pump + bass swell. NaN-guarded so a bad eased
          // value can never blank the frame.
          let intensity =
            v * (0.2 + bandBright * 0.9) + ring * 0.9 + pumpBright * 0.35 + swell * 0.18 + amp * 0.1;
          if (!(intensity >= 0)) intensity = 0.2; // NaN/negative guard
          intensity = intensity > 1.2 ? 1.2 : intensity < 0 ? 0 : intensity;
          const iMin = intensity > 1 ? 1 : intensity;

          // Color: deep ember troughs → hot glow-hue crests.
          const hue = (((gh - (1 - iMin) * 46) % 360) + 360) % 360;
          const light = 4 + iMin * iMin * 70; // 4..74, dark troughs, hot crests
          const sat = 96 - intensity * 20;
          const [cr, cg, cb] = num.hslToRgb(hue, sat, light);

          const di = (rowBase + x) * 4;
          // Light temporal blend against the PREVIOUS PLASMA only.
          pix[di] = cr * (1 - blend) + prev[di] * blend;
          pix[di + 1] = cg * (1 - blend) + prev[di + 1] * blend;
          pix[di + 2] = cb * (1 - blend) + prev[di + 2] * blend;
          pix[di + 3] = 255;
        }
      }

      // Snapshot the pure plasma (pre-overlay) for next frame's temporal blend.
      prev.set(pix);

      // —— CRISP NATIVE-RES OVERLAYS ON TOP ——

      // Bright glow core — the readable "melody head" (pitch→height).
      const ngx = w / 2;
      const ngy = gyN * h;
      const ncoreR = Math.min(w, h) * 0.03 * (1 + onsetPulse * 1.0 + pump * 0.3);
      const [gcr, gcg, gcb] = num.hslToRgb(gh % 360, 92, 70);
      ink(gcr, gcg, gcb, 90 + onsetPulse * 120).circle(ngx, ngy, ncoreR, true);
      ink(255, 250, 235, 120 + onsetPulse * 120).circle(ngx, ngy, ncoreR * 0.5, true);

      // Note markers: a small ladder showing where the melody sits (pitch height).
      for (let m = 0; m < MELODY.length; m++) {
        const sc = noteToScalar(MELODY[m]);
        const my = (0.18 + (1 - sc) * 0.64) * h;
        const active = m === step;
        const mx = w * 0.08;
        const [mr, mg, mb] = num.hslToRgb((20 + sc * 60) % 360, 80, active ? 70 : 45);
        ink(mr, mg, mb, active ? 220 : 90).circle(mx, my, active ? 5 + onsetPulse * 4 : 3, true);
      }

      // Ripple pulses as expanding rings (native res). Capped radius + short life.
      for (const rp of ripples) {
        if (rp.life < 0.12) continue;
        const [rr, rg, rb] = num.hslToRgb(rp.hue % 360, 85, 66);
        const rad = Math.min(rp.r, 0.45) * Math.max(w, h);
        ink(rr, rg, rb, 150 * rp.life).circle(rp.x * w, rp.y * h, rad, false, 2);
      }

      // Tap bursts as bright expanding flashes at the touch point.
      for (const b of bursts) {
        const [tr, tg, tb] = num.hslToRgb(b.hue % 360, 90, 68);
        ink(tr, tg, tb, 200 * b.life).circle(b.x, b.y, b.r, false, 3);
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
  prev = null;
  ripples = [];
  bursts = [];
  padBoot(api);
}

export { boot, sim, paint, act };
