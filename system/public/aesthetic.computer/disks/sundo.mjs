// sundo, 26.07.12
// Slow SUNRISE DRONE — a gentle, meditative ambient PAD that shifts every couple
// of beats over a deep held sub, with soft airy shimmer. A thin wrapper over
// lib/pads.mjs (the shared pad engine: UTC-clock beat grid, `params[0]` rate
// override e.g. `sundo 0.5`, the tap/XY "pump", audio polling). This file only
// describes what makes sundo sundo: its slow harmony, its warm held voices, and
// its soft dawn paint. No hard edges — everything swells.
//
// ALLEGORY (graphic ↔ sonic): each held PAD chord tone IS one horizon LIGHT
// BAND whose vertical center ∝ its pitch (low tone → low warm band near the
// horizon, high tone → high pale band) — the held triad literally IS the stacked
// dawn sky. The SUB is the SUN disc's size + glow. Chord changes = the sky
// re-coloring (a slow gradient sweep). Airy shimmer = drifting light motes near
// the sun. Amplitude → overall glow. You read the harmony as the bands.

import {
  initPad,
  boot as padBoot,
  sim,
  paint,
  act,
  voices,
} from "../lib/pads.mjs";

// —— Harmony ——————————————————————————————————————————————————————————————
// A slow-moving held triad that shifts every couple of beats over a deep sub
// pedal. Warm, drifting, dawn-hued; resolves back to the start so audio loops as
// cleanly as the visuals. One SUB note per beat (the sun's heartbeat pedal).
const SUB = ["a1", "a1", "a1", "a1", "e1", "e1", "e1", "e1"]; // deep foundation
const PAD = [
  ["a2", "c3", "e3"], // Am — the resting dawn
  ["a2", "c3", "e3"],
  ["c3", "e3", "g3"], // C — the sky warms
  ["c3", "e3", "g3"],
  ["e2", "g2", "b2"], // Em — deeper, slower
  ["e2", "g2", "b2"],
  ["d3", "f3", "a3"], // Dm — the horizon lifts
  ["e2", "g2", "b2"],
];

// —— pitch → band-height helpers (local, no deps) —————————————————————————————
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
// Normalize a chord's pitches to 0..1 band-height positions (low tone → low).
// Blends absolute pitch (keeps chord-to-chord feel) with rank (guarantees three
// separated strata), compressed into the mid band so bands never touch extremes.
function bandHeights(ch) {
  const hz = ch.map(noteToHz);
  const n = ch.length;
  return hz.map((f, i) => {
    const abs = Math.max(0, Math.min(1, Math.log2(f / 100) / 2.4));
    const rank = n > 1 ? i / (n - 1) : 0.5;
    return 0.12 + (0.45 * abs + 0.55 * rank) * 0.76;
  });
}

// —— sundo-specific state (the engine owns pump/bursts/rhythm/beatProgress) ——
let chord = PAD[0]; // the chord currently sounding (drives the bands)
let prevChord = PAD[0]; // the chord we're easing away from (gradient sweep)
let chordBlend = 1; // 0..1 crossfade from prevChord→chord on each change
let padVoices = []; // sustained pad handles (one per chord tone)
let subVoice = null; // sustained sub pedal (the sun's foundation)
let subFreq = 55; // current sub note frequency (drives sun size + glow)
let motes = []; // drifting light motes (the airy shimmer)
let ripples = []; // soft warm light-ripples spawned by taps

const CONFIG = {
  bpm: 56, // slow, meditative
  steps: PAD.length, // 8-beat harmonic cycle
  drawBursts: false, // sundo draws its own soft light-ripples in onPaint
  hooks: {
    onBoot({ sound }) {
      // Big, spacious dawn reverb tail across everything.
      sound.room?.set?.({ enabled: true, mix: 0.55, feedback: 0.66 });
      // Start the held sub pedal (the sun's foundation) — very slow swell in.
      const subNote = SUB[0];
      subFreq = noteToHz(subNote);
      subVoice = sound.synth?.({
        tone: subNote,
        type: "sine",
        duration: "🔁", // held forever until updated/killed in leave()
        attack: 1.2,
        decay: 0.95,
        volume: 0.34,
        pan: 0,
      });
      // Seed a scatter of drifting light motes near where the sun will be.
      motes = [];
      for (let i = 0; i < 26; i++) {
        motes.push({
          a: Math.random() * Math.PI * 2, // orbit angle around the sun
          rad: 0.2 + Math.random() * 0.9, // orbit radius (× sun radius)
          spd: 0.05 + Math.random() * 0.14, // drift speed
          ph: Math.random() * Math.PI * 2, // twinkle phase
          size: 0.6 + Math.random() * 1.6,
        });
      }
      ripples = [];
      chord = PAD[0];
      prevChord = PAD[0];
      chordBlend = 1;
      padVoices = [];
    },

    // A new UTC beat crossed — slow chord changes + sub pulses keyed off idx.
    onBeat({ idx, synth }) {
      const step = ((idx % PAD.length) + PAD.length) % PAD.length;

      // —— deep SUB pedal (the sun's foundation) — retune the held voice ——
      const subNote = SUB[step];
      subFreq = noteToHz(subNote);
      if (subVoice) subVoice.update?.({ tone: subNote });
      else
        subVoice = synth({
          tone: subNote,
          type: "sine",
          duration: "🔁",
          attack: 1.2,
          decay: 0.95,
          volume: 0.34,
          pan: 0,
        });
      // A gentle re-articulation on the downbeat so the sub breathes audibly
      // (long overlap → seamless legato swell). voices.sub adds warm body.
      voices.sub(synth, subNote, {
        beats: 3.0,
        attack: 0.9,
        decay: 0.8,
        volume: 0.2,
      });

      // —— the held PAD chord (the horizon bands) ——
      const nextChord = PAD[step];
      const changed = nextChord.join() !== chord.join();
      if (changed) {
        prevChord = chord;
        chord = nextChord;
        chordBlend = 0; // begin the slow gradient sweep to the new sky
      }
      if (padVoices.length === 0) {
        // Held, detuned triad — luscious slow fade-in. Returns kill/update handles.
        padVoices = voices.padChord(synth, chord, {
          type: "sine",
          attack: 1.4,
          decay: 0.95,
          volume: 0.13,
          spread: 0.4,
        });
      } else if (changed) {
        // RETUNE the held handles on chord change (no re-attack → seamless).
        for (let i = 0; i < padVoices.length; i++)
          padVoices[i]?.update?.({ tone: chord[i % chord.length] });
      }

      // —— soft airy shimmer on top (the motes' sound) ——
      // A very quiet high flute a couple octaves above the top tone on sky-change
      // beats — a pale glimmer, never a click.
      if (changed) {
        const top = chord[chord.length - 1];
        const hi = top.replace(/\d/, (d) => String(Math.min(7, +d + 2)));
        voices.flute(synth, hi, {
          beats: 4.0,
          attack: 1.0,
          decay: 0.7,
          volume: 0.07,
          pan: 0.2,
        });
      }
    },

    onSim() {
      // Slow crossfade of the sky when the chord changes (gradient re-color).
      chordBlend = Math.min(1, chordBlend + 0.02);
      // Advance/expire the soft light-ripples.
      for (const r of ripples) {
        r.r += 2.0 + r.vol * 1.8; // gentle, lingering outward spread
        r.life -= 0.01; // slow fade so the ripple reads clearly
      }
      ripples = ripples.filter((r) => r.life > 0);
      // Drift the light motes slowly around the sun.
      for (const m of motes) {
        m.a += m.spd * 0.01;
        m.ph += 0.02;
      }
    },

    // Tap = a SOFT warm light-ripple across the sky + a gentle swelling chord
    // tone (engine already bumped pump + pushed the burst). X → pan/hue, Y →
    // pitch (top = high). Keep every burst SOFT — a swell, never a stab.
    onTap({ x, y, ex, ey, isDraw, synth }) {
      const hue = (x * 0.16 + 0.02) % 1; // ~violet-rose..amber-gold band
      ripples.push({ x: ex, y: ey, r: 0, life: 1, hue, vol: isDraw ? 0.5 : 1 });
      if (ripples.length > 20) ripples.shift();

      // Gentle swelling chord tone — pentatonic so it always agrees with the pad.
      const scale = ["c", "d", "e", "g", "a"];
      const noteName = scale[Math.min(4, Math.floor(x * 5))];
      const oct = 3 + Math.round((1 - y) * 3); // 3..6, higher up = higher note
      const pan = x * 2 - 1;
      synth({
        tone: noteName + oct,
        type: "sine",
        beats: 1.6,
        attack: 0.15, // soft swell, not a stab
        decay: 0.85,
        volume: 0.3 * (isDraw ? 0.6 : 1),
        pan,
      });
      // A faint flute shimmer an octave up — a little glimmer, softer high up.
      voices.flute(synth, noteName + Math.min(7, oct + 1), {
        beats: 0.9,
        attack: 0.1,
        decay: 0.6,
        volume: 0.08 * (isDraw ? 0.5 : 1),
        pan,
      });
    },

    onPaint(api, s) {
      const { ink, screen, num, paintCount } = api;
      const { pump, step, beatProgress, amp, band } = s;
      const { width: w, height: h } = screen;
      const base = Math.min(w, h);

      // —— audio reads (via engine ctx band()/amp) ——
      const sub = band("subBass");
      const air = band("air");

      // Seamless global phase from the UTC cycle (wraps every PAD.length beats).
      const cyclePhase = ((step + beatProgress) / PAD.length) % 1;
      const drift = cyclePhase * Math.PI * 2;

      // Every beat = a gentle whole-sky breath: brightest right on the beat.
      const beatPulse = 1 - beatProgress; // 1 on the beat → 0 just before next
      const glow = 1 + 0.04 * beatPulse + pump * 0.14 + amp * 0.3;

      // The sun sits low, near the horizon — a rising dawn. Keep it low so the
      // sky above has room for the horizon light bands to read clearly.
      const horizonY = h * 0.74;
      const sunX = w * 0.5;
      const sunY =
        horizonY + Math.sin(drift) * base * 0.025 - base * 0.015 * beatPulse;

      // —— DEEP SKY WASH (soft dawn gradient veil, feedback trails) ——
      const veilA = 40 + amp * 22 + pump * 14;
      ink("fade:midnightblue-indigo-mediumvioletred:vertical", veilA).box(
        0, 0, w, h,
      );

      // —— THE HORIZON LIGHT BANDS (the pad chord made visible) ——
      // Each sustained chord tone is ONE horizontal light band; its vertical
      // center ∝ pitch. Bands crossfade prev→current on chord change (a slow
      // gradient re-color sweep) and breathe up/down with the cycle.
      const curH = bandHeights(chord);
      const prvH = bandHeights(prevChord);
      const heights = curH.map((c, i) => num.lerp(prvH[i] ?? c, c, chordBlend));
      const order = heights.map((v, i) => i).sort((a, b) => heights[a] - heights[b]);

      const bandTop = horizonY - base * 0.62;
      const bandBot = horizonY - base * 0.02;
      for (const i of order) {
        const bh = heights[i]; // 0 (low/warm) .. 1 (high/pale)
        const bandY = num.lerp(bandBot, bandTop, bh);
        const thick =
          base * (0.06 + (1 - bh) * 0.05) *
          (1 + 0.12 * Math.sin(drift * 1.3 + i) + sub * 0.4 + air * 0.25) *
          glow;
        const hueDeg = 12 + bh * 42; // ~amber → gold
        const sat = 84 - bh * 36; // paler up high
        const light = 50 + bh * 26 + amp * 8; // brighter up high
        const [br, bg, bb] = num.hslToRgb(hueDeg, sat, Math.min(88, light));
        // Feathered band: stacked translucent boxes, widest+faintest first.
        const layers = 4;
        for (let L = layers; L >= 1; L--) {
          const f = L / layers;
          const th = thick * f;
          const a = (34 + (1 - bh) * 28 + amp * 18 + pump * 14) * (1 - f * 0.42);
          ink(br, bg, bb, a).box(0, bandY - th / 2, w, th);
        }
        // Crisp bright core stratum → the band is unmistakably readable.
        ink(br, bg, bb, 120 + amp * 40 + pump * 20).box(
          0, bandY - thick * 0.16, w, thick * 0.32,
        );
      }

      // —— THE SUN DISC (the sub pedal made visible) ——
      // Soft glowing sun whose SIZE + GLOW ARE the sub note (lower = larger,
      // warmer); flares gently on each beat. Feathered corona, no hard edge.
      const subN = num.clamp(Math.log2(subFreq / 30) / 2.2, 0, 1);
      const sunR =
        base * (0.075 + (1 - subN) * 0.03) * (1 + sub * 0.7) * glow +
        beatPulse * base * 0.012;
      const sunHue = 40 + sub * 8;
      const [sr, sg, sb] = num.hslToRgb(sunHue, 82, 62);
      const CORONA = 7;
      for (let i = CORONA; i >= 1; i--) {
        const f = i / CORONA;
        const wr = sr + (255 - sr) * (1 - f);
        const wg = sg + (250 - sg) * (1 - f);
        const wb = sb + (225 - sb) * (1 - f);
        const a =
          (11 + beatPulse * 12 + sub * 20 + amp * 14 + pump * 12) *
          (1 - f * 0.62);
        ink(wr, wg, wb, a).circle(sunX, sunY, sunR * (0.55 + f * 1.15), true);
      }
      ink(255, 250, 236, 130 + beatPulse * 30 + amp * 40).circle(
        sunX, sunY, sunR * 0.5, true,
      );

      // —— DRIFTING LIGHT MOTES (the airy shimmer made visible) ——
      for (const m of motes) {
        const orb = sunR * (1.1 + m.rad * 1.4);
        const mx = sunX + Math.cos(m.a) * orb;
        const my = sunY + Math.sin(m.a) * orb * 0.7; // flatter orbit
        const tw = 0.5 + 0.5 * Math.sin(m.ph);
        const mr = m.size * (1 + air * 3 + amp * 2) * (0.6 + tw * 0.8);
        const a = (18 + air * 60 + amp * 30 + pump * 16) * (0.4 + tw * 0.6);
        ink(255, 244, 214, a * 0.5).circle(mx, my, mr * 2.2, true);
        ink(255, 250, 232, a).circle(mx, my, mr, true);
      }

      // —— tap light-ripples (the "button" made visible) ——
      for (const r of ripples) {
        const rHueDeg = 8 + ((((r.hue % 1) + 1) % 1) * 52); // amber→gold arc
        const [rr, rg, rb] = num.hslToRgb(rHueDeg, 74, 66);
        const la = r.life * r.life; // ease-out fade
        ink(rr, rg, rb, 90 * la * r.vol).circle(r.x, r.y, r.r, false, 3);
        ink(rr, rg, rb, 50 * la * r.vol).circle(r.x, r.y, r.r * 0.62, false, 2);
        const srcR = 14 + r.r * 0.1;
        ink(255, 240, 210, 42 * la * r.vol).circle(r.x, r.y, srcR, true);
        ink(255, 246, 222, 78 * la * r.vol).circle(r.x, r.y, srcR * 0.55, true);
        ink(255, 251, 236, 140 * la * r.vol).circle(r.x, r.y, srcR * 0.28, true);
      }

      // A single slow drifting high mote, wrapping every 600 frames so the
      // top-layer motion also loops cleanly. paintCount is a Number.
      const dt = (Number(paintCount) % 600) / 600;
      const da = dt * Math.PI * 2;
      const gx = sunX + Math.cos(da) * base * 0.34;
      const gy = sunY - base * 0.18 + Math.sin(da * 1.5) * base * 0.1;
      const gr2 = (4 + air * 18 + sub * 10) * glow;
      ink(255, 248, 224, 24 + amp * 44).circle(gx, gy, gr2 * 1.9, true);
      ink(255, 252, 238, 60 + amp * 80).circle(gx, gy, gr2, true);
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

// Kill the held voices so the drone doesn't linger after leaving the piece.
function leave() {
  padVoices.forEach((v) => v?.kill?.(0.8));
  padVoices = [];
  subVoice?.kill?.(0.8);
  subVoice = null;
}

export { boot, sim, paint, act, leave };
