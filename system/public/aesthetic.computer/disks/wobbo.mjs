// wobbo, 26.07.12
// WOBBLE-BASS OSCILLOSCOPE — a thin wrapper over lib/pads.mjs (the shared pad
// engine: UTC-clock beat grid, `params[0]` rate override e.g. `wobbo 0.5`, the
// tap/XY "pump", audio polling). This file only describes what makes wobbo
// wobbo: its score, its richened wobble-bass voices, and its scope paint.
//
// You literally SEE the bass. The CENTER of the screen is one big rubbery
// WAVEFORM line driven by the live audio; a UTC-phase-locked wobble LFO visibly
// stretches/squashes the line's amplitude in time with the bass → the membrane
// bulges as the bass wobbles. The KICK punches a shockwave outward across the
// membrane; STABS flash bright spikes on the line; bass energy = the membrane's
// girth + neon glow. Tap = a punch (a local bulge + a bass boom). The
// oscilloscope line IS the score.

import { initPad, boot as padBoot, sim, paint, act, voices } from "../lib/pads.mjs";

// --- Score ------------------------------------------------------------------
const BPM = 140; // half-time dubstep feel
const WOBBLE_RATE = 2; // LFO cycles per beat (the wobble you SEE + HEAR)

// One looping bar (8 steps). Root notes for the wobble bass; step 0 == the seam.
const BASS_NOTES = ["e1", "e1", "g1", "e1", "a1", "a1", "d1", "b1"];
// Off-beat stab notes (only fire on certain steps) — bright rubbery accents.
const STAB_NOTES = [null, "e3", null, "b3", null, "g3", null, "d3"];
const KICK_STEPS = [0, 2, 4, 6]; // punchy four-on-the-grid kick (half-time feel)

// --- wobbo-specific state (the engine owns pump/bursts/rhythm) ---------------
let bassA = null; // detuned saw voice A (sustained wobble bass)
let bassB = null; // detuned saw voice B
let bassSub = null; // clean sub sine under it
let wobblePhase = 0; // 0..1 LFO phase within a wobble cycle (UTC-derived)
let wobbleAmt = 0; // eased wobble depth for the visual squash
let kickShock = 0; // shockwave punch on the membrane, kicked by kicks, decays
let stabFlash = 0; // bright spike flash, kicked by stabs, decays
let bassBloom = 0; // membrane girth/glow, driven by bass energy, decays
let bulges = []; // tap-spawned membrane bulges { x, y, r, life, hue, punch }

const CONFIG = {
  bpm: BPM,
  steps: BASS_NOTES.length,
  drawBursts: false, // wobbo draws its own tap bulges/rings in onPaint
  hooks: {
    onBoot({ sound }) {
      sound.room?.set?.({ enabled: true, mix: 0.22, feedback: 0.5 }); // rubbery tail

      // Autostart the sustained wobble-bass stack (detuned saws + clean sub) at
      // volume 0 so the reel has sound from frame 0. Their tone/volume are
      // wobbled live in onSim. RICH: two detuned saws + a sub, raw synth ADSR.
      bassA = sound.synth({ tone: "e1", type: "sawtooth", duration: "🔁", attack: 0.02, decay: 0.9, volume: 0.0 });
      bassB = sound.synth({ tone: "e1", type: "sawtooth", duration: "🔁", attack: 0.02, decay: 0.9, volume: 0.0, pan: 0.1 });
      bassSub = sound.synth({ tone: "e1", type: "sine", duration: "🔁", attack: 0.02, decay: 0.9, volume: 0.0 });
    },

    // A new UTC beat crossed — retarget the sustained bass + fire kick/stabs.
    onBeat({ idx, synth }) {
      const s = ((idx % BASS_NOTES.length) + BASS_NOTES.length) % BASS_NOTES.length;

      // Retarget the sustained wobble bass to this step's root (voices stay alive).
      const root = BASS_NOTES[s];
      bassA?.update?.({ tone: root });
      bassB?.update?.({ tone: root });
      bassSub?.update?.({ tone: root });

      // KICK — a punchy grid thump that PUNCHES the membrane outward. Rich sub
      // body via voices.sub + a filtered-noise click transient.
      if (KICK_STEPS.includes(s)) {
        voices.sub(synth, "c1", { beats: 0.5, attack: 0.001, decay: 0.28, volume: 0.62 });
        voices.hat(synth, { tone: 200, beats: 0.05, volume: 0.32 }); // click
        kickShock = 1; // ALLEGORY: kick = shockwave across the membrane
      }

      // STAB — a bright off-beat rubbery accent → a spike flash on the line.
      const stab = STAB_NOTES[s];
      if (stab) {
        const pan = Math.sin((s / BASS_NOTES.length) * Math.PI * 2) * 0.6;
        voices.pluck(synth, stab, { beats: 0.35, decay: 0.4, volume: 0.3, pan });
        voices.bell(synth, stab, { beats: 0.3, volume: 0.16, pan: -pan });
        stabFlash = 1; // ALLEGORY: stab = bright spike flash on the line
      }

      bassBloom = 1; // ALLEGORY: each step re-energizes the membrane girth/glow
    },

    // Live-wobble the sustained stack from the UTC-phase-locked LFO + decay the
    // eased visual energy. The wobble PHASE derives from ctx.simMs (guarded UTC
    // ms) and ctx.beatSeconds so two instances wobble in perfect lock-step.
    onSim(ctx) {
      const { simMs, beatSeconds, pump, num } = ctx;

      // UTC-phase-locked wobble LFO: WOBBLE_RATE cycles per beat, from guarded
      // UTC ms → deterministic across instances.
      wobblePhase = ((simMs / 1000) / beatSeconds * WOBBLE_RATE) % 1;
      const lfo = 0.5 + 0.5 * Math.sin(wobblePhase * Math.PI * 2); // 0..1 wobble

      // WOBBLE the sustained bass: LFO modulates volume + a slight pan spread so
      // you HEAR the wobble. Pump fattens it; bloom keeps it present.
      const drive = 0.32 + pump * 0.14;
      const wob = 0.28 + lfo * 0.72; // never fully silent → rubbery, not gated
      const vA = drive * wob;
      bassA?.update?.({ volume: vA });
      bassB?.update?.({ volume: vA * 0.9, pan: (lfo - 0.5) * 0.5 }); // detune-pan sway
      bassSub?.update?.({ volume: (0.22 + pump * 0.05) * (0.6 + lfo * 0.4) });

      // Eased wobble depth for the VISUAL squash (so the membrane bulge matches).
      wobbleAmt = num.lerp(wobbleAmt, lfo, 0.4);

      // Decays.
      kickShock *= 0.86;
      stabFlash *= 0.8;
      bassBloom *= 0.94;

      // Advance / cull tap bulges.
      for (const b of bulges) {
        b.r += 4 + b.life * 8;
        b.life -= 0.04;
      }
      bulges = bulges.filter((b) => b.life > 0);
    },

    // Tap PUNCHES the membrane at the tap point — a local bulge + a bass boom
    // (pitch by Y, pan by X). Engine already bumped pump + pushed the burst.
    onTap({ x, y, ex, ey, synth }) {
      // A membrane bulge at the tap point, hue by X, punch by tap-vs-drag.
      bulges.push({ x: ex, y: ey, r: 0, life: 1, hue: x * 360, punch: 1 });
      kickShock = Math.min(1.2, kickShock + 0.7);

      // SONIC BOOM — a bass boom: pitch by Y (top = higher), pan by X.
      const note = ["e1", "g1", "a1", "b1", "d2"][Math.floor((1 - y) * 5)] || "e1";
      const pan = x * 2 - 1;
      voices.sub(synth, note, { beats: 0.6, decay: 0.5, volume: 0.5, pan });
      // A bright rubbery tip an octave up, brighter higher on the screen.
      const tip = ["e3", "g3", "a3", "b3", "d4"][Math.floor(x * 5)] || "e3";
      voices.pluck(synth, tip, { beats: 0.25, decay: 0.3, volume: 0.22 * (1 - y * 0.6), pan });
      stabFlash = 1;
    },

    onPaint(api, ctx) {
      const { ink, line, circle, box, screen, num, help, paintCount } = api;
      const { pump, amp, band, simMs, beatSeconds } = ctx;
      const { width: w, height: h } = screen;
      const cx = w / 2, cy = h / 2;

      // --- Live audio reads ---------------------------------------------------
      const bass = band("subBass");
      const air = band("air");
      const wf = api.sound?.speaker?.waveforms?.left || [];

      // Recompute the UTC-locked wobble phase HERE too (from ctx.simMs) so the
      // paint spatial sway stays phase-locked even between sim ticks.
      const wPhase = ((simMs / 1000) / beatSeconds * WOBBLE_RATE) % 1;

      // Combined "energy" — bass + tap pump + bloom. Drives girth + glow.
      const energy = Math.min(2, bass * 1.4 + amp * 0.6 + pump * 0.5 + bassBloom * 0.4);

      // --- Trail veil (near-black) → glowing neon trails on the line ----------
      ink(4, 2, 10, 150).box(0, 0, w, h);

      // --- The WOBBLE OSCILLOSCOPE MEMBRANE (the graphic score) --------------
      // A thick glowing polyline across the full width. Its base amplitude is
      // the live waveform; the wobble LFO (wobbleAmt) stretches/squashes that
      // amplitude → you SEE the bass wobble as the membrane bulging. The kick
      // punches a travelling shockwave; the tap bulges deform it locally.
      const N = 128; // points across the membrane
      const half = h * 0.32; // vertical reach of the line
      const wobEnv = 0.35 + wobbleAmt * 0.65 * (1 + pump * 0.3); // visible squash
      const girth = 2 + energy * 6 + bassBloom * 4; // girth/glow driven by energy

      const scope = wf.length ? help.resampleArray(wf, N) : null;

      const pts = [];
      const tphase = (paintCount % 240) / 240; // wrapping phase (seamless drift)
      for (let i = 0; i < N; i++) {
        const fx = i / (N - 1); // 0..1 across
        const px = fx * w;

        // Base: live waveform (or a gentle idle sine so it's never dead).
        let v = scope ? scope[i] : Math.sin(fx * Math.PI * 6 + tphase * Math.PI * 2) * 0.2;

        // WOBBLE: a spatial sine tied to the UTC LFO phase bulges the whole
        // line, squashing/stretching in time with the wobble you HEAR.
        const spatial = Math.sin(fx * Math.PI * 2 + wPhase * Math.PI * 2);
        v *= wobEnv; // amplitude breathes with the wobble
        v += spatial * 0.14 * wobbleAmt * (0.6 + energy * 0.7); // rubbery sway

        // KICK shockwave: a gaussian pulse travelling outward from center.
        if (kickShock > 0.01) {
          const front = kickShock; // 1 → center, 0 → edges
          const d = Math.abs(fx - 0.5) * 2; // 0 center .. 1 edge
          const near = Math.exp(-((d - (1 - front)) ** 2) / 0.02);
          v += near * kickShock * 0.55;
        }

        // TAP bulges: local gaussian deformation at each active bulge's x.
        for (const b of bulges) {
          const bfx = b.x / w;
          const d = Math.abs(fx - bfx);
          const g = Math.exp(-(d * d) / 0.004);
          v += g * b.life * b.punch * 0.7 * ((b.y < cy) ? -1 : 1);
        }

        pts.push([px, cy + v * half]);
      }

      // Draw the membrane: acid-green glow halo + neon-magenta core.
      const glowA = Math.min(220, 60 + energy * 120);
      for (let g = 0; g < N - 1; g++) {
        ink(80, 255, 120, glowA * 0.35).line(pts[g][0], pts[g][1], pts[g + 1][0], pts[g + 1][1], girth + 5);
      }
      const [cr, cg, cb] = num.hslToRgb((310 + air * 40 + wobbleAmt * 20) % 360, 90, 62);
      for (let g = 0; g < N - 1; g++) {
        ink(cr, cg, cb, 235).line(pts[g][0], pts[g][1], pts[g + 1][0], pts[g + 1][1], Math.max(1, girth * 0.5));
      }
      // Bright white crest on stabs → the spike flash.
      if (stabFlash > 0.05) {
        for (let g = 0; g < N - 1; g++) {
          ink(255, 255, 255, 200 * stabFlash).line(pts[g][0], pts[g][1], pts[g + 1][0], pts[g + 1][1], 1);
        }
      }

      // --- Bass girth bloom behind the line (the membrane's "body") ----------
      if (bassBloom > 0.02 || bass > 0.04) {
        const bloom = Math.max(bassBloom, bass * 1.3);
        const bH = half * (0.4 + bloom * 0.6);
        ink(120, 40, 200, 22 * bloom).box(cx, cy, w, bH * 2, "*center");
      }

      // --- Kick shockwave rings from center ----------------------------------
      if (kickShock > 0.03) {
        const kr = (1 - kickShock) * Math.max(w, h) * 0.7;
        ink(80, 255, 160, 160 * kickShock).circle(cx, cy, kr, false, 2 + kickShock * 4);
      }

      // --- Tap bulge rings at their punch points -----------------------------
      for (const b of bulges) {
        const [br, bg, bb] = num.hslToRgb(((b.hue % 360) + 360) % 360, 90, 60);
        ink(br, bg, bb, 180 * b.life).circle(b.x, b.y, b.r, false, 1 + b.life * 3);
      }

      // --- UTC wobble side-glow that breathes with the LFO -------------------
      const sideA = 24 + wobbleAmt * 40;
      ink(80, 255, 120, sideA).box(0, cy, 6, half * 2 * wobEnv, "*center");
      ink(255, 40, 180, sideA).box(w - 3, cy, 6, half * 2 * wobEnv, "*center");
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

// The engine doesn't export leave; kill our own held wobble stack here.
function leave() {
  bassA?.kill?.(0.3);
  bassB?.kill?.(0.3);
  bassSub?.kill?.(0.3);
  bassA = bassB = bassSub = null;
}

export { boot, sim, paint, act, leave };
