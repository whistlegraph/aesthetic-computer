// substrate.mjs — the MEDIUM a pop track gets PRINTED on.
//
// A "substrate" bundles everything about the physical-feeling medium the final
// mix is baked onto, so a song isn't just a dry sum of voices — it's PRINTED.
// Two layers:
//
//   • sat   — the per-sample PRINT character applied by the render engine
//             (tube/tape drive, even-harmonic bias, noise floor). Baked into the
//             score header as `sat <drive> <bias> <hiss>` so the C engine prints
//             with it.
//   • master — the mixdown chain (EQ tilt, tube-GLUE compression, wow/flutter,
//             ceiling) applied as the ffmpeg -af graph.
//
// Swap the substrate name to change what the song is printed on. Shared by the
// score baker (sat params) and the C renderer's master (run-c.mjs).

export const SUBSTRATES = {
  // ── TAPE — warm bedroom-pop: tube-glued, gently saturated, soft hiss, a slow
  // wow + flutter, and a two-stage bus compressor so the whole mix feels baked
  // together onto one reel (cohesive "pop" glue, not a transparent limiter). ──
  tape: {
    sat: { drive: 1.6, bias: 0.22, hiss: 0.0055 },
    master: () => [
      "highpass=f=26",
      "equalizer=f=52:t=q:w=0.9:g=2.2",       // sub
      "equalizer=f=100:t=q:w=1.0:g=1.6",      // low body / kick
      "equalizer=f=240:t=q:w=1.0:g=1.7",      // PADDED warmth
      "equalizer=f=420:t=q:w=1.3:g=0.9",      // cushion
      "equalizer=f=900:t=q:w=1.2:g=-0.6",     // de-box
      "equalizer=f=2600:t=q:w=1.4:g=0.2",     // presence pulled back
      "equalizer=f=11000:t=q:w=0.7:g=2.6",    // AIR
      "treble=g=1.6:f=13500",                 // air shelf
      "lowpass=f=18000",
      "vibrato=f=0.6:d=0.45",                 // WOW — slow warped-reel sag
      "vibrato=f=5.0:d=0.16",                 // FLUTTER — fast wobble
      // TUBE-GLUE — two gentle program-dependent stages = cohesive printed feel:
      //   1) a slow LEVELER rides the long dynamics (low ratio, slow times)
      //   2) a faster GLUE knits transients so the mix prints together
      "acompressor=threshold=-20dB:ratio=2:attack=40:release=400:makeup=2:knee=8",
      "acompressor=threshold=-13dB:ratio=3:attack=8:release=150:makeup=2.2:knee=8",
      "alimiter=limit=0.96:attack=5:release=90",
    ].join(","),
  },

  // ── VINYL — a touch darker + a hair more wow, deeper low-mid, rolled top.
  // (A second medium to prove the abstraction; not yet wired as default.) ──
  vinyl: {
    sat: { drive: 1.8, bias: 0.30, hiss: 0.0070 },
    master: () => [
      "highpass=f=30",
      "equalizer=f=60:t=q:w=0.9:g=2.4",
      "equalizer=f=200:t=q:w=1.0:g=2.0",
      "equalizer=f=900:t=q:w=1.2:g=-0.8",
      "equalizer=f=3000:t=q:w=1.4:g=-0.6",
      "treble=g=-1.2:f=10000",
      "lowpass=f=15000",
      "vibrato=f=0.5:d=0.6",
      "vibrato=f=4.4:d=0.22",
      "acompressor=threshold=-20dB:ratio=2.2:attack=50:release=450:makeup=2:knee=8",
      "acompressor=threshold=-12dB:ratio=3.2:attack=10:release=160:makeup=2.4:knee=8",
      "alimiter=limit=0.95:attack=5:release=90",
    ].join(","),
  },
};

export const DEFAULT_SUBSTRATE = "tape";
const pick = (name) => SUBSTRATES[name] ?? SUBSTRATES[DEFAULT_SUBSTRATE];

// the ffmpeg -af master chain for a substrate.
export const masterChain = (name) => pick(name).master();
// the per-sample print params { drive, bias, hiss } for a substrate.
export const satParams = (name) => pick(name).sat;
