// dax, 26.07.12
// A hanging CLOTH / FABRIC MESH ripples in the wind while warm RHODES chords wash
// color gradients across it — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `dax 0.5` → 0.5 s/
// beat, the tap "pump", audio polling). This file only describes what makes dax
// dax: its warm Rhodes score, its electric-piano voices, and its shaded textile.
//
// STRONG GRAPHIC↔SONIC ALLEGORY — the mesh is a height field summed from a few
// decaying RIPPLE SOURCES. Every audible note PUSHES a wave into the cloth: the
// note's pitch chooses the ripple's origin (low→left, high→right) and its hue.
// The held Rhodes CHORD washes a slow color gradient across the fabric (each
// chord tone tints a band). The BASS is a big slow BILLOW (a low broad swell in
// the sheet). The beat = a SHIMMER SWEEP of brightness rolling across the weave.
// The whole sheet is a button: a tap pushes a ripple in at the touch point and
// plays a Rhodes note (X→hue, Y→pitch).

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// —— Harmony ——
// A warm, jazzy Rhodes progression in A minor. One held chord per two beats so
// the cloth's color gradient shifts slowly. The bass walks the roots; a soft
// arpeggiated top note sparkles each beat and pushes a ripple.
const CHORDS = [
  ["a2", "c3", "e3", "g3"], // Am7
  ["a2", "c3", "e3", "g3"], // Am7
  ["d3", "f3", "a3", "c4"], // Dm7
  ["d3", "f3", "a3", "c4"], // Dm7
  ["g2", "b2", "d3", "f3"], // G7
  ["g2", "b2", "d3", "f3"], // G7
  ["c3", "e3", "g3", "b3"], // Cmaj7
  ["e2", "g2", "b2", "d3"], // Em7
];
const BASS = ["a1", "a1", "d2", "d2", "g1", "g1", "c2", "e1"];
// Sparkle line (one note per beat) — pushes a ripple; walks the chord tops.
const TOP = ["e4", "g4", "a4", "f4", "d4", "g4", "b4", "e4"];

const BPM = 76;
const LOOP = BASS.length; // 8-beat harmonic cycle

const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function noteToHz(note) {
  const m = /^([a-g])(#|s|b)?(\d)$/.exec(note.toLowerCase());
  if (!m) return 220;
  let semi = NOTE_SEMI[m[1]];
  if (m[2] === "#" || m[2] === "s") semi += 1;
  else if (m[2] === "b") semi -= 1;
  const midi = semi + (+m[3] + 1) * 12;
  return 440 * Math.pow(2, (midi - 69) / 12);
}
// Pitch → 0..1 across the audible pad range (~a1..b4) for origin + hue mapping.
const HZ_LO = noteToHz("a1");
const HZ_HI = noteToHz("b4");
const pitchNorm = (hz) =>
  Math.max(0, Math.min(1, Math.log2(hz / HZ_LO) / Math.log2(HZ_HI / HZ_LO)));

// —— Rhodes voice ——
// A warm electric-piano: a bell-ish tine (sine + detuned triangle, soft attack,
// long decay) layered as a chord. Not gm — pure oscillator stack.
function rhodesNote(synth, tone, o = {}) {
  const v = o.volume ?? 0.22;
  const beats = o.beats ?? 1.4;
  const pan = o.pan ?? 0;
  // Fundamental tine — sine with a soft attack and a long, bell-like decay.
  synth({ tone, type: "sine", beats, attack: o.attack ?? 0.02, decay: 0.9, volume: v, pan });
  // Detuned triangle body — a touch sharp for the classic Rhodes shimmer/beat.
  synth({ tone, type: "triangle", beats: beats * 0.8, attack: 0.012, decay: 0.7, volume: v * 0.45, pan });
  // A faint octave-up sine "tine ping" for the metallic strike.
  synth({ tone: tone.replace(/\d/, (d) => String(+d + 1)), type: "sine", beats: beats * 0.4, attack: 0.004, decay: 0.5, volume: v * 0.16, pan });
}
function rhodesChord(synth, tones, o = {}) {
  tones.forEach((t, i) =>
    rhodesNote(synth, t, {
      ...o,
      pan: (i - (tones.length - 1) / 2) * (o.spread ?? 0.28),
    }),
  );
}

// —— dax-specific state (the engine owns pump/bursts/rhythm) ——
let ripples = []; // { ox, oy, amp, freq, speed, born, decay, hue } — wave sources
let billow = 0; // slow bass swell, kicked by each bass note
let billowPhase = 0; // drifting phase of the big billow
let shimmer = 0; // beat shimmer sweep energy (rolls x-position with age)
let shimmerX = 0; // 0..1 sweep position across the sheet
let chordHues = [200, 200, 200, 200]; // per-chord-tone hue bands (color wash)
let simTime = 0; // accumulated seconds for ripple animation

const CONFIG = {
  bpm: BPM,
  steps: LOOP,
  drawBursts: false, // dax pushes its own ripples on tap; no default rings
  hooks: {
    onBoot({ sound }) {
      // Warm, roomy tail so the Rhodes blooms like felt.
      sound.room?.set?.({ enabled: true, mix: 0.4, feedback: 0.55 });
    },

    // A new UTC beat crossed — fire the score + push waves into the cloth.
    onBeat({ idx, synth }) {
      const step = ((idx % LOOP) + LOOP) % LOOP;

      // Warm sub billow on every beat (root) — a big slow swell in the sheet.
      const bassNote = BASS[step];
      voices.sub(synth, bassNote, {
        beats: 1.8,
        attack: 0.06,
        decay: 0.7,
        volume: 0.42,
        pan: Math.sin(idx * 0.3) * 0.2,
      });
      billow = 1;

      // Held Rhodes chord retunes each downbeat (every 2 beats) — washes the
      // color gradient. Each chord tone tints one horizontal band of the weave.
      if (step % 2 === 0) {
        const chord = CHORDS[step];
        rhodesChord(synth, chord, { beats: 2.2, volume: 0.2, spread: 0.3 });
        chordHues = chord.map((n) => 30 + pitchNorm(noteToHz(n)) * 260);
      }

      // Sparkle top note — one per beat. PUSHES a ripple into the cloth: pitch →
      // horizontal origin (low left, high right) + hue.
      const topNote = TOP[step];
      const tHz = noteToHz(topNote);
      const pn = pitchNorm(tHz);
      rhodesNote(synth, topNote, {
        beats: 1.2,
        volume: 0.16,
        pan: pn * 1.4 - 0.7,
      });
      pushRipple(pn, 0.35 + Math.sin(step * 1.3) * 0.25, 0.9, 30 + pn * 260);

      // Soft closed-hat shimmer tick — kicks the brightness sweep.
      voices.hat(synth, { tone: 7000, beats: 0.1, volume: 0.1 });
      shimmer = 1;
      shimmerX = 0; // sweep starts at the left edge each beat
    },

    onSim() {
      simTime += 1 / 60;
      billow *= 0.94;
      billowPhase += 0.012;
      shimmer *= 0.9;
      shimmerX = Math.min(1.4, shimmerX + 0.06); // roll the sweep rightward
      // Age + expire ripples.
      for (const r of ripples) r.life -= r.decay;
      ripples = ripples.filter((r) => r.life > 0);
      if (ripples.length > 14) ripples = ripples.slice(-14);
    },

    // Tap = push a ripple in at the touch point + a Rhodes note (X→hue, Y→pitch).
    onTap({ x, y, synth }) {
      const hue = x * 360;
      pushRipple(x, 1 - y, 1.4, hue);
      const scale = ["a", "c", "d", "e", "g"];
      const noteName = scale[Math.min(4, Math.floor(x * 5))];
      const oct = 2 + Math.round((1 - y) * 3); // top = higher
      rhodesNote(synth, noteName + oct, {
        beats: 1.6,
        volume: 0.24,
        pan: x * 2 - 1,
      });
    },

    onPaint(api, s) {
      const { ink, screen, num } = api;
      const { pump, amp, band, quality } = s;
      const { width: w, height: h } = screen;

      // Dark textile ground with a faint vertical gradient (loom light from top).
      ink("fade:midnightblue-black:vertical", 255).box(0, 0, w, h);

      // —— audio reads ——
      const bass = band("subBass");
      const air = band("air");
      const energy = Math.min(1.6, bass * 1.1 + amp * 0.7 + pump * 0.4);

      // —— adaptive mesh resolution (READ ctx.quality to hold 60fps) ——
      // Scale rows×cols by quality: full weave at q=1, coarser when auto-cut.
      const COLS = Math.max(10, Math.round(30 * quality));
      const ROWS = Math.max(7, Math.round(20 * quality));

      // Mesh spans most of the sheet with a soft margin so it reads as a hung
      // fabric, not a full-bleed fill.
      const mx = w * 0.06, my = h * 0.08;
      const gw = w - mx * 2, gh = h - my * 2;
      const cellW = gw / COLS, cellH = gh / ROWS;

      // Big slow BILLOW amplitude (bass) — a broad low swell across the whole
      // sheet, breathing with the sub. Depth in pixels.
      const billowAmp = (10 + billow * 26 + bass * 40) * (0.6 + quality * 0.4);
      const rippleScale = Math.min(w, h) * 0.11 * (0.7 + quality * 0.3);

      // Height field at a normalized (u,v) — sum of the big billow + each decaying
      // ripple source (a radial decaying sine wave), in pixels of vertical push.
      const t = simTime;
      function heightAt(u, v) {
        // Broad billow: a slow low-frequency swell drifting across the sheet.
        let z =
          Math.sin(u * Math.PI * 1.5 + billowPhase * 2) *
          Math.cos(v * Math.PI + billowPhase) *
          billowAmp;
        // Gravity sag — the cloth hangs, heaviest in the middle-bottom.
        z += Math.sin(v * Math.PI) * 6;
        // Ripple sources: radial decaying waves from each note's origin.
        for (const r of ripples) {
          const du = u - r.ox, dv = v - r.oy;
          const dist = Math.sqrt(du * du + dv * dv);
          const age = t - r.born;
          const wave = Math.sin(dist * r.freq * 18 - age * r.speed * 9);
          const falloff = Math.exp(-dist * 2.4) * r.life;
          z += wave * falloff * r.amp * rippleScale;
        }
        return z;
      }

      // Which chord-tone hue tints this row band (v 0..1 → chord tone).
      const nHues = chordHues.length;
      function bandHue(v) {
        const fi = v * (nHues - 1);
        const i0 = Math.floor(fi), i1 = Math.min(nHues - 1, i0 + 1);
        const f = fi - i0;
        return chordHues[i0] * (1 - f) + chordHues[i1] * f;
      }

      // —— draw the shaded mesh ——
      // Sample a grid of heights, then draw quads shaded by height + slope. A
      // brighter, warmer color where the cloth rises toward the light; darker in
      // the troughs. Draw as filled shape rows + weave lines over the top.
      // Precompute the grid once.
      const zGrid = new Array(ROWS + 1);
      for (let ry = 0; ry <= ROWS; ry++) {
        const v = ry / ROWS;
        const row = new Array(COLS + 1);
        for (let cxi = 0; cxi <= COLS; cxi++) {
          const u = cxi / COLS;
          row[cxi] = heightAt(u, v);
        }
        zGrid[ry] = row;
      }

      // Shimmer sweep: a bright vertical band that rolls left→right on the beat.
      const sweepU = shimmerX; // 0..1.4

      // Fill quads (back to front by row so lower rows layer over).
      for (let ry = 0; ry < ROWS; ry++) {
        const v0 = ry / ROWS, v1 = (ry + 1) / ROWS;
        const hue = bandHue((v0 + v1) / 2);
        for (let cxi = 0; cxi < COLS; cxi++) {
          const u0 = cxi / COLS, u1 = (cxi + 1) / COLS;
          const z00 = zGrid[ry][cxi], z10 = zGrid[ry][cxi + 1];
          const z01 = zGrid[ry + 1][cxi], z11 = zGrid[ry + 1][cxi + 1];

          // Screen positions (height pushes the point UP the screen).
          const x0 = mx + u0 * gw, x1 = mx + u1 * gw;
          const y0 = my + v0 * gh, y1 = my + v1 * gh;
          const p00 = [x0, y0 - z00];
          const p10 = [x1, y0 - z10];
          const p11 = [x1, y1 - z11];
          const p01 = [x0, y1 - z01];

          // Shade by average height (rise = light) + slope (steep = highlight).
          const zAvg = (z00 + z10 + z01 + z11) * 0.25;
          const slope = Math.abs(z10 - z00) + Math.abs(z01 - z00);
          const lift = zAvg / (rippleScale + billowAmp + 12); // ~-1..1
          const light = Math.max(8, Math.min(92, 34 + lift * 42 + slope * 0.25));
          const sat = 42 + energy * 22 + Math.min(30, slope * 0.4);

          // Shimmer sweep brightens the band it's crossing.
          const swDist = Math.abs(u0 - sweepU);
          const swLight = swDist < 0.12 ? (0.12 - swDist) / 0.12 * shimmer * 30 : 0;

          const [r, g, b] = num.hslToRgb(
            ((hue % 360) + 360) % 360,
            Math.min(95, sat),
            Math.min(96, light + swLight),
          );
          // Alpha rises with the fabric lift so crests feel taut/lit.
          const a = 150 + Math.min(90, Math.max(0, lift * 70) + air * 40);
          ink(r, g, b, a).shape([p00, p10, p11, p01]);
        }
      }

      // —— weave lines (warp + weft) over the shaded quads for textile texture ——
      // Only when quality is decent; these are the threads catching the light.
      if (quality > 0.55) {
        const lineA = 60 + air * 40;
        // Weft (horizontal) threads.
        for (let ry = 0; ry <= ROWS; ry += 1) {
          const v = ry / ROWS;
          const hue = bandHue(v);
          const [r, g, b] = num.hslToRgb(((hue % 360) + 360) % 360, 50, 78);
          let px = null, py = null;
          for (let cxi = 0; cxi <= COLS; cxi++) {
            const x = mx + (cxi / COLS) * gw;
            const y = my + v * gh - zGrid[ry][cxi];
            if (px !== null) ink(r, g, b, lineA).line(px, py, x, y, 1);
            px = x; py = y;
          }
        }
        // Warp (vertical) threads — sparser to stay cheap.
        for (let cxi = 0; cxi <= COLS; cxi += 2) {
          const u = cxi / COLS;
          let px = null, py = null;
          for (let ry = 0; ry <= ROWS; ry++) {
            const v = ry / ROWS;
            const x = mx + u * gw;
            const y = my + v * gh - zGrid[ry][cxi];
            if (px !== null) ink(210, 210, 230, lineA * 0.5).line(px, py, x, y, 1);
            px = x; py = y;
          }
        }
      }

      // —— ripple origin sparks — where each note struck the cloth ——
      for (const r of ripples) {
        const sx = mx + r.ox * gw;
        const sy = my + r.oy * gh - heightAt(r.ox, r.oy);
        const [cr, cg, cb] = num.hslToRgb(((r.hue % 360) + 360) % 360, 90, 66);
        const glowR = 4 + r.life * 10 * r.amp;
        ink(cr, cg, cb, 120 * r.life).circle(sx, sy, glowR * 1.6, true);
        ink(255, 255, 255, 180 * r.life).circle(sx, sy, glowR * 0.5, true);
      }

      // Tap pump adds a soft global brightening veil so a touch reads instantly.
      if (pump > 0.05) {
        ink(255, 250, 240, Math.min(50, pump * 22)).box(0, 0, w, h);
      }
    },
  },
};

// Push a decaying ripple source into the cloth. u,v are 0..1 sheet coords.
function pushRipple(u, v, amp, hue) {
  ripples.push({
    ox: Math.max(0, Math.min(1, u)),
    oy: Math.max(0, Math.min(1, v)),
    amp,
    freq: 0.7 + Math.random() * 0.4,
    speed: 1 + Math.random() * 0.6,
    born: simTime,
    life: 1,
    decay: 0.01,
    hue,
  });
}

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before the engine
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  padBoot(api);
}

export { boot, sim, paint, act };
