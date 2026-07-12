// murmo, 26.07.12
// Murmuration-swarm pad — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `murmo 0.5`, the
// tap/XY "pump", audio polling). This file only describes what makes murmo
// murmo: its score, its richened breathing voices, and its swarm paint.
//
// STRONG GRAPHIC↔SONIC ALLEGORY —
//   • the low DRONE bed = the big slow collective UNDULATION of the whole flock
//     (the flock's overall wave motion keys to a continuous phase from simMs, so
//     two instances anywhere flock together and it loops seamlessly);
//   • each mid VOICE that sounds = a cluster of the swarm IGNITING BRIGHT at a
//     height ∝ its pitch, HUE = pitch → you SEE the harmony as glowing bands
//     rippling through the flock;
//   • the PULSE = the whole swarm contracting/expanding (a breath);
//   • amplitude → flock density/brightness.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
// A slow murmuring chorus. One mid "voice" ignites per beat over an 8-beat cycle
// that resolves back to the start so the audio loops as cleanly as the visuals.
// A stack of pentatonic tones — soft, always-agreeing, twilight-warm.
const VOICES = ["a3", "c4", "e4", "d4", "g4", "e4", "c4", "d4"];
const DRONE = ["a1", "a2"]; // root + octave — the flock's collective low body
const BPM = 84;

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
// Map a note's Hz into a normalized 0..1 pitch band (roughly a2..c6).
function pitchNorm(hz) {
  const n =
    Math.log2(hz / noteToHz("a2")) / Math.log2(noteToHz("c6") / noteToHz("a2"));
  return Math.max(0, Math.min(1, n));
}

// --- murmo-specific state (the engine owns pump/bursts/rhythm/clock) ---------
const AGENTS = 320; // a few hundred — reasonable for smooth fps at density 3
let agents = [];

// A "band" is a voice ignition made visible: a bright height slice that ripples
// through the flock. pitch → y (top = high), pitch → hue. Spawned in onBeat and
// onTap; fades over ~1s so every audible onset has a visible birth.
let bands = []; // { y, hue, life, strength }

// Held drone bed (the flock's collective body); started once in onBoot.
let droneStarted = false;

// The flock's slow undulation phase (the drone) + breath (the pulse) — both
// derived from simMs each sim so they stay UTC-synced and loop with the cycle.
let flowPhase = 0;
let breath = 1;

const CONFIG = {
  bpm: BPM,
  steps: VOICES.length, // seamless 8-beat cycle
  drawBursts: false, // murmo draws its own tap shocks in onPaint
  hooks: {
    onBoot({ sound, screen }) {
      // A wide, airy hall so the murmur chorus blooms and blurs into a haze.
      sound.room?.set?.({ enabled: true, mix: 0.48, feedback: 0.58 });

      // Start the held DRONE bed once — the flock's collective body / low hum.
      // A held detuned pad chord (root + octave + a beating partner) → a warm,
      // breathing drone that literally IS the slow undulation you see.
      if (!droneStarted) {
        droneStarted = true;
        voices.padChord(
          sound.synth,
          [DRONE[0], noteToHz(DRONE[0]) * 1.006, DRONE[1]],
          { volume: 0.13, attack: 1.4, decay: 0.9, spread: 0.4 },
        );
      }

      // Seed the swarm. Positions are re-derived from the flow field each frame,
      // so we only need each agent's phase seed + a home spread.
      const w = screen?.width || 360;
      const h = screen?.height || 640;
      agents = [];
      for (let i = 0; i < AGENTS; i++) {
        agents.push({
          seed: (i / AGENTS) * Math.PI * 2, // spreads the flock + own wobble
          spread: Math.random(), // 0..1 across the flow field
          x: Math.random() * w,
          y: Math.random() * h,
          wob: Math.random() * Math.PI * 2, // personal flap phase
          glow: 0, // ignition, raised when a band lights this height
          hue: 0.6,
        });
      }
    },

    // A new UTC beat crossed — fire the score + spawn the matching ignition band.
    onBeat({ idx, synth }) {
      const step = ((idx % VOICES.length) + VOICES.length) % VOICES.length;
      const note = VOICES[step];
      const hz = noteToHz(note);
      const pN = pitchNorm(hz); // 0..1 → visual height + hue
      const pan = Math.sin(idx * 0.7) * 0.4; // slow stereo drift

      // Richened murmur VOICE: a breathy waveguide flute + a detuned sine partner
      // → a fuller, flocking mid tone that swells and ebbs (beating shimmer).
      voices.flute(synth, note, {
        beats: 2.4,
        attack: 0.35,
        decay: 0.7,
        volume: 0.26,
        pan,
      });
      synth({
        tone: hz * 1.008, // gently detuned partner → beating shimmer
        type: "sine",
        beats: 2.2,
        attack: 0.4,
        decay: 0.65,
        volume: 0.2,
        pan: -pan,
      });
      // A faint bell an octave up on alternating beats — a high glint in the flock.
      if (step % 2 === 0) {
        voices.bell(synth, hz * 2, { beats: 1.2, volume: 0.08, pan });
      }

      // The slow breath PULSE — a soft low thump every other beat: the whole
      // swarm contracting/expanding. Sub so it's felt, not clicky.
      if (step % 2 === 0) {
        voices.sub(synth, "a1", { beats: 1.4, attack: 0.06, decay: 0.5, volume: 0.3 });
      }

      // The VISIBLE counterpart of the voice: a bright height band (pitch → y,
      // pitch → hue) that ripples through the flock and lights agents at that
      // height. This is how you SEE the harmony.
      bands.push({ y: 1 - pN, hue: pN, life: 1, strength: 1 });
      if (bands.length > 12) bands.shift();
    },

    onSim(api) {
      const { simMs, beatProgress, pump } = api;

      // The DRONE undulation drives the flock, keyed to a CONTINUOUS phase from
      // simMs (UTC-guarded) so it wraps with the loop and two instances flock
      // together. One full cycle = steps beats; 2 undulations per cycle.
      const beatMs = api.beatSeconds * 1000;
      const globalCycle = simMs / (beatMs * VOICES.length); // continuous cycle pos
      flowPhase = globalCycle * Math.PI * 2 * 2; // 2 undulations per loop → seamless

      // The breath pulse: the whole swarm contracts/expands. Tightest on the
      // beat, easing to the next; pump swells it when the flock is agitated.
      const beatPulse = 1 - beatProgress;
      breath =
        1 +
        0.14 * Math.sin(globalCycle * Math.PI * 2 * 2) +
        beatPulse * 0.04 +
        pump * 0.04;

      for (const b of bands) b.life -= 0.02; // ~1s ignition fade
      bands = bands.filter((b) => b.life > 0);
    },

    // Tap = scatter/ignite the swarm from the tap point (engine already bumped
    // pump + pushed the burst) + a swelling detuned murmur voice. X→pan/hue,
    // Y→pitch (top = high). The engine's burst is the shock ripple; agents react
    // to it in onPaint's flock advance.
    onTap({ x, y, ex, ey, isDraw, synth, burst }) {
      // Warm hue across the twilight arc; store it on the burst for the shock rings.
      const hue = (x * 0.5 + 0.55) % 1;
      burst.hue = hue * 360;
      burst.grow = 5 + (isDraw ? 5 : 10); // ripple races outward through the flock
      burst.vol = isDraw ? 0.5 : 1;

      // A bright ignition band at the tap height so the visible birth matches the
      // audible one exactly.
      bands.push({ y: 1 - y, hue, life: 1, strength: isDraw ? 0.7 : 1 });
      if (bands.length > 12) bands.shift();

      // SONIC BOOST — a swelling detuned murmur voice. Pentatonic so it always
      // agrees with the chorus. Y → pitch (top = high), X → pan. Soft attack → a
      // swell igniting, not a click. Quieter on a drag so a sweep stays tender.
      const scale = ["c", "d", "e", "g", "a"];
      const noteName = scale[Math.min(4, Math.floor(x * 5))];
      const oct = 3 + Math.round((1 - y) * 3); // 3..6 — higher up = higher note
      const pan = x * 2 - 1;
      const g = isDraw ? 0.6 : 1;
      synth({
        tone: noteName + oct,
        type: "sine",
        beats: 1.6,
        attack: 0.05,
        decay: 0.8,
        volume: 0.32 * g,
        pan,
      });
      synth({
        tone: noteToHz(noteName + oct) * 1.008, // detuned partner → murmur shimmer
        type: "sine",
        beats: 1.3,
        attack: 0.06,
        decay: 0.7,
        volume: 0.18 * g,
        pan: -pan,
      });
    },

    onPaint(api, s) {
      const { ink, circle, box, screen, num } = api;
      const { pump, band, amp, bursts } = s;
      const { width: w, height: h } = screen;

      // Twilight wash — a translucent gradient veil each frame so the swarm
      // leaves a silky motion-smear trail. Indigo→teal twilight bed.
      ink("fade:midnightblue-teal:vertical", 40).box(0, 0, w, h);

      // —— audio reads (make the ambient field react too) ——
      const subBass = band("subBass");
      const air = band("air");
      // Density/brightness of the whole flock rides amplitude + the low drone.
      const density = 0.5 + amp * 1.2 + subBass * 0.8 + pump * 0.4;

      // —— move the flock ——
      // A SHARED sine flow field (cheap): each agent samples a smooth vector
      // field driven by flowPhase (the drone undulation) + its own seed/spread,
      // plus a gentle pull toward center scaled by breath (contraction), plus a
      // shock push from tap bursts. Advanced here so it rides the paint clock.
      const cx = w / 2;
      const cy = h / 2;
      const spanX = w * 0.42;
      const spanY = h * 0.34;

      for (const a of agents) {
        a.wob += 0.06;
        const p = a.spread; // 0..1 across the flock
        const angle = a.seed + flowPhase + p * Math.PI * 2;
        // Big collective wave (the drone's undulation) — the flock's overall motion.
        const wave =
          Math.sin(flowPhase + p * 3.4) * 0.6 +
          Math.sin(flowPhase * 0.6 + a.seed) * 0.4;
        let tx =
          cx +
          (Math.cos(angle) * spanX * (0.5 + 0.5 * Math.cos(flowPhase + p * 2))) /
            breath;
        let ty = cy + wave * spanY * breath + Math.sin(a.wob) * 6;

        // Shock push — each tap burst front shoves nearby agents outward (scatter).
        for (const b of bursts) {
          const dx = a.x - b.x;
          const dy = a.y - b.y;
          const d = Math.hypot(dx, dy) || 1;
          const front = Math.abs(d - b.r);
          if (front < 40) {
            const push = (1 - front / 40) * b.life * 26 * (b.vol ?? 1);
            tx += (dx / d) * push;
            ty += (dy / d) * push;
          }
        }

        // Ease toward the target → smooth flocking. Pump makes it snappier/nervous.
        const ease = 0.06 + pump * 0.05;
        a.x += (tx - a.x) * ease;
        a.y += (ty - a.y) * ease;

        // Ignition: if a bright band is near this agent's height, it lights up.
        let g = 0;
        let gh = a.hue;
        for (const bnd of bands) {
          const by = bnd.y * h;
          const dyy = Math.abs(a.y - by);
          if (dyy < 85) {
            const prox = 1 - dyy / 85;
            const lit = prox * prox * bnd.life * bnd.strength * 1.6;
            if (lit > g) {
              g = lit;
              gh = bnd.hue;
            }
          }
        }
        a.glow += (Math.min(1, g) - a.glow) * 0.3; // ease → ignitions bloom & fade
        a.hue += (gh - a.hue) * 0.25;
      }

      // —— draw the flock ——
      // Each agent is a soft additive dot. Base twilight color (indigo/teal)
      // blends toward its warm ignition hue by its glow → bright pitch-bands.
      for (const a of agents) {
        const baseHueDeg = (200 + Math.sin(a.seed + flowPhase) * 40 + 360) % 360;
        const glow = Math.pow(Math.min(1, a.glow), 0.5);
        const [br, bg, bb] = num.hslToRgb(baseHueDeg, 65, 42 + amp * 12); // cool body
        // Ignition color: WARM igniting voice (a.hue 0..1 → amber↔gold ~30..70°).
        // Blend in RGB (NOT hue) so cool→warm never sweeps through green.
        const litHueDeg = a.hue * 40 + 30;
        const [lr, lg, lb] = num.hslToRgb(litHueDeg, 95, 62); // hot warm pop
        const r = br * (1 - glow) + lr * glow;
        const g = bg * (1 - glow) + lg * glow;
        const b = bb * (1 - glow) + lb * glow;

        const size = 1.4 + glow * 3.4 + subBass * 1.6 + air * 1.4;
        const alpha = Math.min(230, 60 + glow * 150 + density * 40);
        if (glow > 0.15) {
          ink(r, g, b, alpha * 0.4).circle(a.x, a.y, size * 2.2, true);
        }
        ink(r, g, b, alpha).circle(a.x, a.y, size, true);
      }

      // —— tap shocks: expanding rings rippling through the flock ——
      for (const b of bursts) {
        const [r, g, bl] = num.hslToRgb(((b.hue % 360) + 360) % 360, 88, 66);
        ink(r, g, bl, 170 * b.life).circle(b.x, b.y, b.r, false, 2 + (b.vol ?? 1) * 2);
        ink(r, g, bl, 90 * b.life).circle(b.x, b.y, b.r * 0.7, false, 1);
      }

      // —— pitch-band guide glow ——
      // A soft warm horizontal wash at each live voice's height so you read the
      // harmony as bands even in sparse flock regions (graphic notation layer).
      for (const bnd of bands) {
        const by = bnd.y * h;
        const [r, g, b2] = num.hslToRgb(bnd.hue * 55 + 32, 78, 60);
        ink(r, g, b2, 34 * bnd.life * bnd.strength).box(0, by - 24, w, 48);
      }
    },
  },
};

// initPad runs in boot (NOT at import) because lib/pads.mjs is a shared
// singleton — each entry must re-assert THIS pad's config before the engine
// boot/sim/paint/act run.
function boot(api) {
  initPad(CONFIG);
  droneStarted = false; // re-arm the held drone bed for this fresh boot
  padBoot(api);
}

export { boot, sim, paint, act };
