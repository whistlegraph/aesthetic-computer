// lull, 26.07.12
// A slow melodic bassline + warm sustained pad drive an undulating gooey field
// of nested contour-blobs in a soft dawn palette — a thin wrapper over
// lib/pads.mjs (the shared pad engine: UTC-clock beat grid, `params[0]` rate
// override e.g. `lull 0.5` → 0.5 s/beat, the tap "pump", audio polling). This
// file only describes what makes lull lull: its dreamy score, its soft voices,
// and its liquid dawn paint.
//
// STRONG GRAPHIC↔SONIC ALLEGORY — the bass note IS the soft glowing CORE
// (pitch→hue, pitch→size, the heartbeat). Each held PAD chord tone IS one nested
// contour ring (radius ∝ that tone's pitch). Amplitude → swell/brightness. Every
// downbeat = a gentle whole-field breath. The whole piece is a soft button: a
// tap drops a glowing droplet that blooms outward and rings a gentle bell.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// —— Harmony ——
// A gentle 8-step bassline (one note per beat) over a slow BPM. Root motion
// walks a warm minor-ish progression that resolves back to the start so the
// audio loops as cleanly as the visuals.
const BASS = ["a1", "a1", "c2", "e2", "d2", "d2", "f2", "e2"];
// Pad chord tones (held triad), retuned each downbeat to shadow the bass.
// Each of the three tones becomes one nested contour ring (low→outer/large).
const PAD = [
  ["a2", "c3", "e3"],
  ["a2", "c3", "e3"],
  ["c3", "e3", "g3"],
  ["e3", "g3", "b3"],
  ["d3", "f3", "a3"],
  ["d3", "f3", "a3"],
  ["f3", "a3", "c4"],
  ["e3", "g3", "b3"],
];

const BPM = 66;
const LOOP = BASS.length; // 8-beat harmonic cycle

// Convert a note string to Hz for pitch→size/hue mapping (local, no deps).
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function noteToHz(note) {
  const m = /^([a-g])(#|s|b)?(\d)$/.exec(note.toLowerCase());
  if (!m) return 220;
  let semi = NOTE_SEMI[m[1]];
  if (m[2] === "#" || m[2] === "s") semi += 1;
  else if (m[2] === "b") semi -= 1;
  const octave = +m[3];
  const midi = semi + (octave + 1) * 12; // MIDI note number
  return 440 * Math.pow(2, (midi - 69) / 12);
}

// —— lull-specific visual/audio state (the engine owns pump/bursts/rhythm) ——
let padVoices = []; // sustained pad voice handles (one per chord tone)
let chord = PAD[0]; // the chord currently sounding (drives the rings)
let bassFreq = 55; // current bass note frequency (drives the core)
let bassHue = 0.55; // current bass hue 0..1 (pitch → hue)
let droplets = []; // soft blooms spawned by taps {x,y,r,life,hue,vol}

const CONFIG = {
  bpm: BPM,
  steps: LOOP,
  drawBursts: false, // lull draws its own soft droplet blooms in onPaint
  hooks: {
    onBoot({ sound }) {
      // Lush, dreamy reverb tail across everything.
      sound.room?.set?.({ enabled: true, mix: 0.5, feedback: 0.62 });
    },

    // A new UTC beat crossed — fire this beat's bass + pad retune + counter-melody.
    // Called on each UTC beat crossing so two instances anywhere sound the
    // identical step at the identical wall-clock ms.
    onBeat({ idx, synth }) {
      const step = ((idx % LOOP) + LOOP) % LOOP;

      // Warm sub bassline — one soft note per beat, panned in a slow drift.
      const bassNote = BASS[step];
      bassFreq = noteToHz(bassNote);
      // Pitch → hue: map the bass across a dawn arc (violet↔rose), 0..1.
      bassHue = ((Math.log2(bassFreq / 55) / 3) * 0.5 + 0.55) % 1;
      voices.sub(synth, bassNote, {
        beats: 1.6, // let notes overlap slightly for a legato swell
        attack: 0.12,
        decay: 0.7,
        volume: 0.5,
        pan: Math.sin(idx * 0.4) * 0.3,
      });

      // A soft breathy flute counter-melody an octave up on alternating beats —
      // the tender lead.
      if (step % 2 === 0) {
        voices.flute(synth, bassNote.replace(/\d/, (d) => String(+d + 2)), {
          beats: 1.4,
          attack: 0.2,
          decay: 0.6,
          volume: 0.18,
          pan: -Math.sin(idx * 0.4) * 0.4,
        });
      }

      // The sustained pad: create it once (held triad, retune the held voices
      // each beat). Each tone becomes one nested contour ring.
      chord = PAD[step];
      if (padVoices.length === 0) {
        padVoices = voices.padChord(synth, chord, {
          type: "sine",
          attack: 0.6,
          decay: 0.9,
          volume: 0.12,
          spread: 0.35,
        });
      } else {
        for (let i = 0; i < padVoices.length; i++) {
          padVoices[i]?.update?.({ tone: chord[i % chord.length] });
        }
      }
    },

    // Soft decay / expiry of lull's own visual state (the engine decays pump +
    // its bursts itself).
    onSim() {
      for (const d of droplets) {
        d.r += 1.6 + d.vol * 1.4; // gentle, lingering outward bloom
        d.life -= 0.012; // slow fade so the bloom reads clearly
      }
      droplets = droplets.filter((d) => d.life > 0);
    },

    // Tap = a soft glowing droplet that blooms outward + a gentle bell. The
    // engine already bumped pump + pushed the burst; add lull's tender bloom +
    // chime here. X → pan + hue, Y → pitch (top = high). Keep it soft.
    onTap({ x, y, ex, ey, isDraw, synth }) {
      // A soft glowing droplet blooms outward from the touch point (translucent,
      // no hard edge). Hue rides X across the dawn arc.
      const hue = (x * 0.5 + 0.5) % 1;
      droplets.push({ x: ex, y: ey, r: 0, life: 1, hue, vol: isDraw ? 0.5 : 1 });
      if (droplets.length > 24) droplets.shift();

      // Gentle bell tone — pentatonic so it always agrees with the pad. Y → pitch
      // (top = high, low = low). X → pan. Soft attack, long decay → a chime, not
      // a click. Quieter on a drag so a held sweep stays tender.
      const scale = ["c", "d", "e", "g", "a"];
      const noteName = scale[Math.min(4, Math.floor(x * 5))];
      const oct = 3 + Math.round((1 - y) * 3); // 3..6, higher up = higher note
      const pan = x * 2 - 1;
      voices.bell(synth, noteName + oct, {
        beats: 1.2,
        volume: 0.34 * (isDraw ? 0.6 : 1),
        pan,
      });
    },

    onPaint(api, s) {
      const { ink, screen, num, paintCount } = api;
      const { step, beatProgress, pump, bursts, amp, band } = s;
      const { width: w, height: h } = screen;

      // Soft dawn wash — translucent gradient veil each frame so blobs leave
      // gooey trails rather than snapping. The gradient itself is the dreamy
      // dawn ramp.
      ink("fade:midnightblue-rebeccapurple:vertical", 46).box(0, 0, w, h);

      // —— audio reads ——
      const bass = band("subBass");
      const lowMid = band("lowMid");
      const air = band("air");

      // Seamless global phase from the UTC cycle (wraps every LOOP beats).
      const cyclePhase = (((step + beatProgress) / LOOP) % 1 + 1) % 1; // 0..1
      const orbit = cyclePhase * Math.PI * 2;

      const cx = w / 2;
      const cy = h / 2;
      const base = Math.min(w, h);

      // Every downbeat = a gentle whole-field breath: brightest right on the
      // beat, easing to the next. Tap pump adds a soft extra swell.
      const beatPulse = 1 - beatProgress; // 1 on the beat → 0 just before next
      const breath = 1 + 0.05 * beatPulse + pump * 0.16;

      // —— the gooey field: the pad chord IS the nested rings ——
      // Each of the three sustained chord tones maps to one primary contour ring
      // whose radius ∝ that tone's pitch (low tone = large outer ring, high tone
      // = small inner ring). Intermediate rings feather between them for the
      // liquid body. Radii swell with the bass + pump; wobble with air/amp.
      const chordHz = chord.map((n) => noteToHz(n)); // ~[low, mid, high]
      const hzMin = Math.min(...chordHz);
      const hzMax = Math.max(...chordHz);
      const RINGS = 7;
      const SEG = 96; // polygon resolution — high for smooth liquid edges
      const swell = (1 + bass * 1.4) * breath;
      const wobble = 0.16 + air * 0.5 + amp * 0.3 + pump * 0.06;

      for (let r = RINGS; r >= 1; r--) {
        const t = r / RINGS; // 1 = outermost, →0 inner

        // Which chord tone does this ring embody? Low tone → outer (large), high
        // tone → inner (small). Interpolate pitch across the ring stack so the
        // radius is literally proportional to the pad tone's pitch. Higher pitch
        // → smaller radius, so invert the normalized pitch.
        const pitchN =
          hzMax > hzMin
            ? 1 - (chordHz[Math.min(chordHz.length - 1, Math.round((1 - t) * (chordHz.length - 1)))] - hzMin) /
                (hzMax - hzMin)
            : t;
        // Blend the pitch-derived size with the ring index for a smooth stack.
        const sizeN = 0.5 * t + 0.5 * pitchN;

        const radius =
          base *
          (0.08 + sizeN * 0.42) *
          swell *
          (1 + 0.06 * Math.sin(orbit * 2 + t * 6));

        // Slow drift of the whole blob so nothing is static.
        const dx = Math.cos(orbit + t * 3) * base * 0.06 * (1 + lowMid);
        const dy = Math.sin(orbit * 1.3 + t * 2) * base * 0.06;

        // Soft palette: hue rides the ring index + the harmonic cycle. hslToRgb
        // wants hue 0–360 and sat/light 0–100 and returns 0–255 (no ×255).
        const hueDeg = (((cyclePhase + t * 0.5 + 0.55) % 1) + 1) % 1 * 360;
        const [cr, cg, cb] = num.hslToRgb(hueDeg, 55, 62 - t * 12);
        const alpha = 40 + (1 - t) * 90 + amp * 60 + pump * 24;

        // Build the wobbling closed contour.
        const pts = [];
        for (let ss = 0; ss <= SEG; ss++) {
          const a = (ss / SEG) * Math.PI * 2;
          // Two ripples out of phase → gooey, organic asymmetry.
          const ripple =
            1 +
            wobble * Math.sin(a * 3 + orbit * 2 + r) * 0.5 +
            wobble * Math.sin(a * 5 - orbit * 3 - r * 0.7) * 0.5;
          const rad = radius * ripple;
          pts.push([cx + dx + Math.cos(a) * rad, cy + dy + Math.sin(a) * rad]);
        }
        ink(cr, cg, cb, alpha).poly(pts);
      }

      // —— the CORE is the bass note ——
      // A soft glowing radial heart whose SIZE and HUE ARE the current bass note
      // (pitch→hue, pitch→size) and which flares on each downbeat — the
      // heartbeat. Layered translucent circles (widest and faintest first) build
      // a feathered halo with no hard edge; the innermost ring warms toward
      // white. Pure circles → no opaque square.
      const bassN = num.clamp(Math.log2(bassFreq / 40) / 3, 0, 1); // 0=low..1=high
      const coreR =
        base *
        (0.06 + (1 - bassN) * 0.05) * // lower bass → larger core
        (1 + bass * 1.1) *
        breath +
        beatPulse * base * 0.025;
      // Bass pitch → core hue (dawn arc).
      const coreHueDeg = (((bassHue + 0.05) % 1) + 1) % 1 * 360;
      const [gr, gg, gb] = num.hslToRgb(coreHueDeg, 60, 78); // 0..255, no ×255
      const HALO = 6;
      for (let i = HALO; i >= 1; i--) {
        const f = i / HALO; // 1 = outermost/faintest ring, →0 = tight bright center
        // Warm hue at the edge, fading toward white in the middle for a molten glow.
        const wr = gr + (255 - gr) * (1 - f);
        const wg = gg + (255 - gg) * (1 - f);
        const wb = gb + (245 - gb) * (1 - f);
        // Low alpha per ring; overlapping layers accumulate into a soft bloom.
        const a = (10 + beatPulse * 14 + amp * 16 + pump * 12) * (1 - f * 0.55);
        ink(wr, wg, wb, a).circle(cx, cy, coreR * f, true);
      }

      // —— tap blooms (the "button" made visible) ——
      // Each tap's soft glowing droplet expands and fades — translucent rings +
      // a warm center, never a hard box. Hue rides where you touched.
      for (const d of droplets) {
        const dHueDeg = ((d.hue % 1) + 1) % 1 * 360;
        const [dr, dg, db] = num.hslToRgb(dHueDeg, 60, 74);
        const la = d.life * d.life; // ease-out fade
        // Two expanding soft ripple rings — a gentle bloom spreading outward.
        ink(dr, dg, db, 95 * la * d.vol).circle(d.x, d.y, d.r, false, 3);
        ink(dr, dg, db, 55 * la * d.vol).circle(d.x, d.y, d.r * 0.6, false, 2);
        // Feathered warm glow at the source (widest+faintest first → no hard edge).
        const srcR = 16 + d.r * 0.12;
        ink(255, 248, 232, 44 * la * d.vol).circle(d.x, d.y, srcR, true);
        ink(255, 250, 235, 80 * la * d.vol).circle(d.x, d.y, srcR * 0.55, true);
        ink(255, 253, 244, 150 * la * d.vol).circle(d.x, d.y, srcR * 0.28, true);
      }

      // A single slow drifting highlight droplet, wrapping every 480 frames so
      // the top-layer motion also loops cleanly. A soft round glow, not a hard box.
      const dt = (Number(paintCount) % 480) / 480; // paintCount is a Number
      const da = dt * Math.PI * 2;
      const dropX = cx + Math.cos(da) * base * 0.28;
      const dropY = cy + Math.sin(da * 1.5) * base * 0.32;
      const dropR = (5 + bass * 22) * breath;
      ink(255, 250, 235, 30 + amp * 50).circle(dropX, dropY, dropR * 1.8, true);
      ink(255, 252, 240, 70 + amp * 90).circle(dropX, dropY, dropR, true);
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
