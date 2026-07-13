// mibo, 26.07.12
// Texture-tunnel psy-trance pad — a thin wrapper over lib/pads.mjs (the shared
// pad engine: UTC-clock beat grid, `params[0]` rate override e.g. `mibo 0.5`,
// the tap/XY "pump", audio polling, adaptive ctx.quality). This file only
// describes what makes mibo mibo: its rolling psy score and its true PER-PIXEL
// raycast tunnel (angle/depth → a checker/spiral texture, flying forward).
//
// ALLEGORY — you fly forward through an infinite patterned tunnel:
//   • ARP (rolling 16th-note psy line) = the tunnel's RING BANDS lighting up.
//     Each note lights a band at a depth; PITCH → the band's HUE. The arp also
//     drives the tunnel SPEED (a running note flies you faster).
//   • BASS = the tunnel WIDTH PULSE — each root breathes the walls in/out.
//   • BEAT = a BRIGHT RING rushing past you (a light band accelerating outward).
//   • TAP = a bright ring pulse punched down the tunnel + a lead stab at the tap
//     (X → pan + hue, Y → pitch).
// Distinct from vroon (feedback-zoom radial streaks): here every pixel is a true
// angle/depth texture lookup with a checker/spiral pattern — a real tunnel.
//
// PER-PIXEL + 60fps: angle/depth are precomputed per pixel ONCE (rebuilt only on
// resize) and hue is a precomputed LUT (never hslToRgb per pixel). The paint loop
// STRIDES by ctx.quality and fills stride×stride blocks to hold 60fps native.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// 150 BPM psy roll. 16 sixteenth-steps per bar; the arp rolls the classic
// psy shape (root pedal + offbeat climbs). Pitch → ring-band hue.
const BPM = 150;
const ARP = [
  "e2", "e3", "e2", "b2", "e2", "g3", "e2", "b2",
  "e2", "d3", "e2", "a2", "e2", "c3", "e2", "fs3",
];
const BASS = ["e1", "e1", "c1", "g1"]; // per-bar root motion (width pulse)
const LEAD = ["e4", "g4", "b4", "d5", "e5", "d5", "b4", "g4"]; // squelch accents

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(s?)(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[3], 10) + 1) * 12 + NOTE_SEMI[m[1]] + (m[2] ? 1 : 0);
}
const ARP_PITCHES = ARP.map(notePitch);
const PITCH_MIN = Math.min(...ARP_PITCHES);
const PITCH_MAX = Math.max(...ARP_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN);

// --- mibo-specific state (the engine owns pump/bursts/rhythm) ---------------
// Ring bands lit by notes: each rides OUTWARD (toward the viewer) down the
// tunnel over its life. { depth (0 far..1 near), hue, life, bright }
let bands = [];
let widthPulse = 0; // bass breathing the tunnel walls, decays
let speedBoost = 0; // extra fly-forward speed from the arp/taps, decays
let scrollPhase = 0; // accumulated depth-scroll (fly-forward), advances in onSim
let bright = 0; // whole-tunnel flash on the beat, decays

// --- Precomputed per-pixel geometry + hue LUT (built once / on resize) -------
let geoW = 0, geoH = 0;
let angLUT = null; // per-pixel tunnel angle (0..1 around)
let depLUT = null; // per-pixel tunnel depth (0 near center-edge..big at center)
let radLUT = null; // per-pixel normalized radius (0 center..1 corner) for width pulse
const HUE_LUT = new Array(360); // hue(0..359) -> [r,g,b] (bright, saturated)

function buildHueLUT(num) {
  for (let h = 0; h < 360; h++) HUE_LUT[h] = num.hslToRgb(h, 95, 58);
}

function buildGeometry(w, h) {
  geoW = w; geoH = h;
  const n = w * h;
  angLUT = new Float32Array(n);
  depLUT = new Float32Array(n);
  radLUT = new Float32Array(n);
  const cx = w / 2, cy = h / 2;
  const invTwoPi = 1 / (Math.PI * 2);
  const maxR = Math.hypot(cx, cy);
  const SCALE = maxR * 0.5; // depth = SCALE/r → concentric rings dense at center
  let i = 0;
  for (let y = 0; y < h; y++) {
    const dy = y - cy;
    for (let x = 0; x < w; x++, i++) {
      const dx = x - cx;
      const r = Math.hypot(dx, dy) + 0.0001;
      // angle around the tunnel, 0..1
      const a = Math.atan2(dy, dx) * invTwoPi + 0.5; // 0..1
      angLUT[i] = a;
      // classic raycast tunnel: DEPTH = SCALE / r. Small near the walls (large r),
      // LARGE toward the vanishing point (small r) — concentric depth rings that
      // bunch up at the center, exactly like flying down a pipe.
      depLUT[i] = SCALE / r;
      radLUT[i] = r / maxR; // 0 center .. 1 corner (wall)
    }
  }
}

const CONFIG = {
  bpm: BPM,
  steps: ARP.length,
  drawBursts: false, // mibo draws its own tunnel rings
  hooks: {
    onBoot({ sound, num }) {
      sound.room?.set?.({ enabled: true, mix: 0.22, feedback: 0.5 });
      buildHueLUT(num);
    },

    // A new UTC beat crossed — fire the psy score + light a ring band.
    onBeat({ idx, synth }) {
      const s = ((idx % ARP.length) + ARP.length) % ARP.length;
      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const isRoot = s % 2 === 0; // the pedal-root 16ths

      // ROLLING PSY ARP: raw detuned saws (16th-note roll), pitch → hue band.
      const pan = Math.sin((s / ARP.length) * Math.PI * 2) * 0.5;
      synth({ tone: note, type: "sawtooth", beats: 0.14, attack: 0.002,
        decay: 0.32, volume: 0.3, pan });
      synth({ tone: note, type: "sawtooth", beats: 0.12, attack: 0.002,
        decay: 0.3, volume: 0.16, pan: -pan }); // detuned partner via opposite pan

      // ALLEGORY: this note lights a RING BAND far down the tunnel (depth ~0),
      // which then flies outward toward the viewer. Pitch → hue.
      bands.push({ depth: 0.04, hue: (20 + pn * 300) % 360, life: 1, bright: 0.5 + pn * 0.5 });
      speedBoost = Math.min(1.6, speedBoost + 0.22); // running note → fly faster

      // Squelchy lead accent on the offbeats (the psy "acid" bites).
      if (!isRoot) {
        const li = ((Math.floor(idx / 2) % LEAD.length) + LEAD.length) % LEAD.length;
        voices.pluck(synth, LEAD[li], { beats: 0.22, decay: 0.4, volume: 0.22, pan: -pan });
      }

      // BASS = tunnel WIDTH PULSE, once per beat of the bar (every 4 steps).
      if (s % 4 === 0) {
        const bi = ((Math.floor(idx / 4) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 0.9, decay: 0.5, volume: 0.5 });
        widthPulse = 1;
      }

      // BEAT = bright ring rushing past + driving hat.
      voices.hat(synth, { tone: 9000, beats: 0.08, volume: 0.14 });
      if (isRoot) voices.hat(synth, { tone: 4000, beats: 0.05, volume: 0.08, pan });
      bright = 1;
    },

    onSim(api) {
      const { simMs } = api;
      // Fly forward: depth-scroll advances with a base speed + arp/tap boost.
      // Tie the base rate to simMs delta so it's frame-rate independent.
      const dt = 1 / 60; // paint/sim ~60fps; boost dominates the feel anyway
      scrollPhase += (0.5 + speedBoost) * dt;
      speedBoost *= 0.92;
      widthPulse *= 0.9;
      bright *= 0.88;
      for (const b of bands) {
        b.depth += 0.02 + speedBoost * 0.03; // fly the band toward the viewer
        b.life -= 0.02;
      }
      bands = bands.filter((b) => b.life > 0 && b.depth < 1.4);
      if (bands.length > 20) bands = bands.slice(-20);
    },

    // Tap = a bright ring PUNCHED down the tunnel + a lead stab at the tap.
    // X → pan + hue, Y → pitch.
    onTap({ x, y, synth }) {
      bands.push({ depth: 0.02, hue: (x * 360) % 360, life: 1.3, bright: 1 });
      speedBoost = Math.min(1.8, speedBoost + 0.5);
      bright = 1;
      const note = ["e", "g", "a", "b", "d"][Math.floor(x * 5) % 5] + (3 + Math.floor((1 - y) * 3));
      voices.pluck(synth, note, { beats: 0.5, decay: 0.45, volume: 0.5, pan: x * 2 - 1 });
      voices.bell(synth, note, { beats: 0.3, volume: 0.28 * (1 - y), pan: x * 2 - 1 });
    },

    onPaint(api, s) {
      const { screen, num } = api;
      const { quality, amp, pump } = s;
      const bass = s.band("subBass");
      const w = screen.width, h = screen.height;
      const pix = screen.pixels; // Uint8ClampedArray RGBA
      if (!pix) return;

      // (Re)build per-pixel geometry + hue LUT if the screen resized.
      if (w !== geoW || h !== geoH || !angLUT) buildGeometry(w, h);
      if (!HUE_LUT[0]) buildHueLUT(num);

      // STRIDE by quality — the whole cost is per-pixel, so this is the knob.
      const stride = quality < 0.55 ? 3 : quality < 0.8 ? 2 : 1;

      // Tunnel params. Width pulse (bass) breathes the walls: scale depth so the
      // rings squeeze in/out (the pipe gets wider/narrower). Beat lifts brightness.
      const scroll = scrollPhase; // fly-forward: rings stream OUT from the center
      const widthMod = 1 + widthPulse * 0.55 + bass * 0.7; // walls breathe
      const RINGS = 5.0; // concentric ring frequency down the tunnel
      const SEGMENTS = 10; // angular checker segments (spiral bricks)
      const flash = bright;

      // BAND LIGHTING: each fired note lit a ring at b.depth (0 far..~1.3 near);
      // it flies outward. In tunnel coords the pixel's ring index is
      // frac(v)≈position along the pipe, and the ring's screen position tracks its
      // depth. We key band glow on the SAME concentric ring index the texture uses,
      // so a lit band appears as a colored concentric ring rushing outward.
      // Precompute a small LUT keyed on ring-index bucket (cheap per-pixel read).
      const NB = 48;
      const bandHue = new Float32Array(NB);
      const bandLit = new Float32Array(NB);
      for (const b of bands) {
        // b.depth 0..~1.3 maps to a ring index that decreases (moves outward) as
        // it approaches. Use scroll-relative index so it aligns with the texture.
        const ringIdx = (scroll - b.depth * RINGS * 1.4);
        let bk = (((ringIdx % 1) + 1) % 1) * NB | 0;
        for (let k = -2; k <= 2; k++) {
          const idx = ((bk + k) % NB + NB) % NB;
          const fall = 1 - Math.abs(k) * 0.35;
          const contrib = b.life * b.bright * fall;
          if (contrib > bandLit[idx]) { bandLit[idx] = contrib; bandHue[idx] = b.hue; }
        }
      }

      for (let y = 0; y < h; y += stride) {
        const rowBase = y * w;
        for (let x = 0; x < w; x += stride) {
          const i = rowBase + x;
          const ang = angLUT[i];
          const dep = depLUT[i] * widthMod; // SCALE/r, breathing — big at center
          // v = depth coordinate down the pipe (grows toward vanishing point);
          // scroll ADDS so rings stream outward (fly forward). u = angular bricks
          // that twist with depth → the classic spiral.
          const v = dep * RINGS + scroll;
          const u = ang * SEGMENTS + dep * 1.5;

          // CHECKER/SPIRAL texture: xor of ring parity and angular parity.
          const checker = ((v | 0) ^ (u | 0)) & 1;

          // Tunnel fog: WALLS (large r → radLUT→1, small dep) are NEAR and BRIGHT;
          // the vanishing point (center, small r, huge dep) is FAR and DARK.
          const wall = radLUT[i]; // 0 center .. 1 wall
          const near = wall * wall; // brightness toward the walls (viewer)
          const texBase = checker ? 1.0 : 0.32;
          const lum = (0.12 + near * 0.95) * texBase * (0.72 + flash * 0.6);

          // Which concentric ring bucket is this pixel in?
          const bk = (((v % 1) + 1) % 1) * NB | 0;
          const lit = bandLit[bk];
          let r, g, bl;
          if (lit > 0.06) {
            const c = HUE_LUT[((bandHue[bk] | 0) % 360 + 360) % 360] || HUE_LUT[0];
            const glow = Math.min(1.6, lit * (0.5 + near));
            r = Math.min(255, c[0] * glow * (0.5 + lum) + 25);
            g = Math.min(255, c[1] * glow * (0.5 + lum) + 12);
            bl = Math.min(255, c[2] * glow * (0.5 + lum) + 32);
          } else {
            // untinted pipe wall — cool teal/indigo checker, warm rim at the walls.
            r = Math.min(255, (18 + 90 * near) * lum * 2.0);
            g = Math.min(255, (14 + 40 * near) * lum * 2.0);
            bl = Math.min(255, (44 + 120 * near) * lum * 2.0);
          }

          // Center bloom — the vanishing point glows/pulses with beat + pump.
          if (wall < 0.16) {
            const core = 1 - wall / 0.16;
            const cg = core * core * (0.25 + flash * 0.6 + pump * 0.18 + amp * 0.5);
            r = Math.min(255, r + cg * 210);
            g = Math.min(255, g + cg * 170);
            bl = Math.min(255, bl + cg * 255);
          }

          // Fill the stride×stride block with this sample.
          const yMax = Math.min(h, y + stride);
          const xMax = Math.min(w, x + stride);
          const R = r | 0, G = g | 0, B = bl | 0;
          for (let yy = y; yy < yMax; yy++) {
            let p = (yy * w + x) * 4;
            for (let xx = x; xx < xMax; xx++, p += 4) {
              pix[p] = R; pix[p + 1] = G; pix[p + 2] = B; pix[p + 3] = 255;
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
