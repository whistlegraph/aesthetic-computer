// brindo, 26.07.12
// Demoscene PLASMA × TRANCE SUPERSAW — a thin wrapper over lib/pads.mjs (the
// shared pad engine: UTC-clock beat grid, `params[0]` rate override e.g.
// `brindo 0.5`, tap/XY "pump", audio polling, adaptive quality). This file only
// describes what makes brindo brindo: its euphoric supersaw score and its
// per-pixel plasma paint written DIRECTLY to screen.pixels.
//
// ALLEGORY — the plasma field IS the music:
//   • ARP  → shifts the palette HUE PHASE. Every arpeggio note advances the
//            256-entry hue LUT's rotation, so the color cycles with the melody.
//   • BASS → WARPS the plasma. Each sub note (and the live sub-band amplitude)
//            feeds a low-frequency ripple term that ripples the whole field.
//   • BEAT → a bright BLOOM pulse. Each beat kicks `bloom`, lifting the plasma's
//            brightness so the field glows brighter on the downbeat.
//   • TAP  → injects a bright travelling plasma ripple + a supersaw stab.
//
// Per-pixel plasma → screen.pixels, striding by ctx.quality to hold ~60fps.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// Euphoric trance arpeggio in A minor (i–VI–III–VII feel), 16 steps.
const ARP = [
  "a3", "c4", "e4", "a4", "e4", "c4", "a3", "e4",
  "f3", "a3", "c4", "f4", "c4", "a3", "f3", "c4",
];
// Rolling pad chord tones (held, detuned) — swaps every 8 steps A-minor ↔ F.
const PAD_A = ["a3", "c4", "e4"];
const PAD_F = ["f3", "a3", "c4"];
const BASS = ["a1", "a1", "f1", "f1", "c2", "c2", "e1", "e1"]; // half-time root

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

// --- Supersaw ----------------------------------------------------------------
// A trance supersaw = one note voiced as ~7 raw detuned sawtooth oscillators,
// each panned/detuned a touch, summed to a fat euphoric wall.
const DETUNE = [-0.22, -0.14, -0.06, 0, 0.06, 0.14, 0.22]; // semitone offsets
function midiToHz(m) {
  return 440 * Math.pow(2, (m - 69) / 12);
}
function supersaw(synth, note, o = {}) {
  const base = notePitch(note); // MIDI-ish
  const beats = o.beats ?? 1.0;
  const vol = (o.volume ?? 0.5) / DETUNE.length; // keep the sum in range
  for (let i = 0; i < DETUNE.length; i++) {
    const pan = ((i / (DETUNE.length - 1)) * 2 - 1) * (o.spread ?? 0.7);
    synth({
      tone: midiToHz(base + DETUNE[i]),
      type: "sawtooth",
      beats,
      attack: o.attack ?? 0.006,
      decay: o.decay ?? 0.65,
      volume: vol,
      pan,
    });
  }
}

// --- Palette LUT (never call hslToRgb per pixel) -----------------------------
// 256-entry euphoric hue ramp precomputed once; the arp rotates the read phase.
let LUT = null; // Uint8Array length 256*3
function buildLUT(num) {
  LUT = new Uint8Array(256 * 3);
  for (let i = 0; i < 256; i++) {
    const hue = (i / 256) * 360;
    // High-sat, bright liquid color; a light ripple keeps it glowing not flat.
    const light = 52 + 14 * Math.sin((i / 256) * Math.PI * 2 * 2);
    const [r, g, b] = num.hslToRgb(hue, 96, light);
    LUT[i * 3] = r;
    LUT[i * 3 + 1] = g;
    LUT[i * 3 + 2] = b;
  }
}

// --- brindo state (engine owns pump/bursts/rhythm) ---------------------------
let huePhase = 0; // 0..255 palette rotation, advanced by each arp note
let huePhaseTarget = 0; // eased toward
let bloom = 0; // beat bloom pulse (brightness lift)
let bassWarp = 0; // decaying bass-driven warp amount
let padHandles = null; // held supersaw pad chord (started in onBeat)
let padIsA = true;
let ripples = []; // tap ripples: { x, y, r, life } in 0..1 space

const CONFIG = {
  bpm: 138, // classic uplifting-trance tempo
  steps: ARP.length,
  drawBursts: false, // brindo draws its own tap ripples into the plasma
  hooks: {
    onBoot({ sound, num }) {
      buildLUT(num);
      sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.55 });
    },

    // A new UTC beat crossed — fire the supersaw score + drive the allegory.
    onBeat({ idx, synth }) {
      const s = ((idx % ARP.length) + ARP.length) % ARP.length;
      const note = ARP[s];
      const pn = pitchNorm(note); // 0 low .. 1 high
      const pan = Math.sin((s / ARP.length) * Math.PI * 2) * 0.6;

      // Start / swap the held supersaw PAD CHORD (rolling, detuned sustain).
      const wantA = s < 8;
      if (!padHandles || wantA !== padIsA) {
        padHandles?.forEach?.((v) => v?.kill?.(0.4));
        padHandles = voices.padChord(synth, wantA ? PAD_A : PAD_F, {
          type: "sawtooth",
          volume: 0.05,
          attack: 0.8,
          decay: 0.9,
          spread: 0.5,
        });
        padIsA = wantA;
      }

      // ARP note → a fat euphoric supersaw stab.
      supersaw(synth, note, { beats: 0.85, volume: 0.42, decay: 0.6, pan });

      // ALLEGORY: this note SHIFTS the palette hue phase (color cycles w/ arp).
      huePhaseTarget = (huePhaseTarget + 14 + pn * 22) % 256;

      // Sub-bass root on the half-beat → WARPS the plasma + you feel it.
      if (s % 2 === 0) {
        const bi = ((Math.floor(idx / 2) % BASS.length) + BASS.length) % BASS.length;
        voices.sub(synth, BASS[bi], { beats: 1.6, decay: 0.5, volume: 0.55 });
        bassWarp = 1;
      }

      voices.hat(synth, { tone: 9000, beats: 0.1, volume: 0.14 });
      bloom = 1; // BEAT → bright bloom pulse
    },

    onSim() {
      bloom *= 0.9;
      bassWarp *= 0.92;
      // Ease palette rotation toward target so hue shifts glide, not snap.
      let d = huePhaseTarget - huePhase;
      if (d > 128) d -= 256;
      if (d < -128) d += 256;
      huePhase = (huePhase + d * 0.15 + 256) % 256;
      for (const rp of ripples) {
        rp.r += 0.012;
        rp.life -= 0.02;
      }
      ripples = ripples.filter((rp) => rp.life > 0);
      if (ripples.length > 6) ripples = ripples.slice(-6);
    },

    // Tap = a bright plasma ripple at the tap + a supersaw stab (engine already
    // bumped pump + pushed a burst). X→pitch/pan, Y→octave.
    onTap({ x, y, synth }) {
      ripples.push({ x, y, r: 0, life: 1 });
      bloom = Math.min(1.4, bloom + 0.7);
      huePhaseTarget = (huePhaseTarget + 40) % 256;
      const note =
        ["a", "c", "d", "e", "g"][Math.floor(x * 5) % 5] +
        (3 + Math.floor((1 - y) * 3));
      supersaw(synth, note, { beats: 0.7, volume: 0.5, decay: 0.55, pan: x * 2 - 1 });
    },

    // PER-PIXEL PLASMA → screen.pixels. Summed sinusoids indexed into the hue
    // LUT; bass warps it, arp rotates its color, beat blooms its brightness.
    onPaint(api, s) {
      const { screen } = api;
      const { simMs, band, amp, quality } = s;
      const { width: w, height: h, pixels } = screen;
      if (!w || !h || !pixels || !LUT) return;

      // Adaptive cost: one computed pixel per stride×stride block.
      const q = quality ?? 1;
      const stride = q < 0.55 ? 3 : q < 0.8 ? 2 : 1;

      const t = simMs * 0.001;
      const bass = Math.max(bassWarp, band("subBass") * 1.2);
      const energy = 0.5 + amp * 0.6; // live loudness → plasma contrast
      const warp = bass * 5.5; // bass amplitude → low-frequency ripple depth
      const bloomLift = bloom * 60; // beat → brighter palette read

      // Precompute time-varying plasma coefficients (per frame, not per pixel).
      const t1 = t * 1.7,
        t2 = t * 1.1,
        t3 = t * 0.6;
      const cx = w * 0.5,
        cy = h * 0.5;
      const invW = 1 / w,
        invH = 1 / h;
      const hp = huePhase | 0;

      for (let y = 0; y < h; y += stride) {
        const ny = y * invH;
        // Bass warp: a low-frequency vertical ripple riding sub amplitude.
        const wy = warp * Math.sin(ny * 3 + t3);
        for (let x = 0; x < w; x += stride) {
          const nx = x * invW;
          // Classic summed-sinusoid plasma: a few sine layers + a radial term.
          const dx = x - cx,
            dy = y - cy;
          const rad = Math.sqrt(dx * dx + dy * dy) * 0.03;
          let v =
            Math.sin(nx * 10 + t1) +
            Math.sin(ny * 8 + t2 + wy) +
            Math.sin((nx + ny) * 6 + t3) +
            Math.sin(rad + t1 * 0.5) +
            Math.sin(nx * 6 + ny * 6 + t2 * 0.7 + warp * 0.3);

          // Tap ripples: bright travelling rings added into the field.
          for (let k = 0; k < ripples.length; k++) {
            const rp = ripples[k];
            const ddx = nx - rp.x,
              ddy = ny - rp.y;
            const dd = Math.sqrt(ddx * ddx + ddy * ddy) - rp.r;
            v += 2.2 * rp.life * Math.cos(dd * 22);
          }

          // Map plasma value → palette index, rotated by the arp's hue phase.
          // v ranges roughly -6..6; fold into 0..255 with contrast from energy.
          let idx = ((v * 20 * energy + hp) % 256 + 256) % 256;
          idx = idx | 0;
          const li = idx * 3;
          let R = LUT[li] + bloomLift;
          let G = LUT[li + 1] + bloomLift;
          let B = LUT[li + 2] + bloomLift;
          if (R > 255) R = 255;
          if (G > 255) G = 255;
          if (B > 255) B = 255;

          // Fill the stride×stride block (clamped at edges).
          const yEnd = y + stride < h ? y + stride : h;
          const xEnd = x + stride < w ? x + stride : w;
          for (let by = y; by < yEnd; by++) {
            const row = by * w;
            for (let bx = x; bx < xEnd; bx++) {
              const i = (row + bx) * 4;
              pixels[i] = R;
              pixels[i + 1] = G;
              pixels[i + 2] = B;
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
