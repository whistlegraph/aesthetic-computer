// Percussion synth kit — shared between web notepat and native notepat.
// Exports the 12-drum layout, display labels, pad colors, and a pure
// `playPercussion(sound, letter, opts)` that fires the layered voices
// for a single drum on the supplied AC sound API.
//
// Natural notes = 7 core drums (kick/snare/clap/snap/hat-c/hat-o/ride).
// Sharps = 5 accents (crash/splash/cowbell/block/tambo).
//
// The native kit layered a voice-inspector and hold-release machinery on
// top of this; both are kept callback-based here so the module itself has
// no persistent state and works identically on web + native.

// TR-808 hi-hat 6-square inharmonic cluster (trimmed to 4 for clarity).
export const HAT_FREQS = [800, 540, 522.7, 369.6];

export const PERCUSSION_NAMES = {
  c: "kick", d: "snare", e: "clap", f: "snap",
  g: "hat-c", a: "hat-o", b: "ride",
  "c#": "crash", "d#": "splash",
  "f#": "cowbell", "g#": "block", "a#": "tambo",
};

// 3-char display labels shown on drum pads in place of note names.
export const PERCUSSION_LABELS = {
  c: "BAS", d: "SNR", e: "CLP", f: "SNP",
  g: "HHC", a: "HHO", b: "RDE",
  "c#": "CRS", "d#": "SPL",
  "f#": "CBL", "g#": "BLK", "a#": "TMB",
};

// Metallic / earthy colors to visually distinguish drum pads from keys.
export const PERCUSSION_COLORS = {
  c: [220, 90, 40],      // kick — deep orange
  d: [220, 180, 110],    // snare — tan
  e: [240, 220, 130],    // clap — pale yellow
  f: [220, 240, 140],    // snap — yellow-green
  g: [120, 220, 180],    // closed hat — mint
  a: [120, 200, 240],    // open hat — cyan
  b: [180, 180, 230],    // ride — silver-blue
  "c#": [220, 150, 240], // crash — lavender
  "d#": [240, 160, 220], // splash — pink
  "f#": [200, 150, 80],  // cowbell — brass
  "g#": [190, 120, 70],  // block — wood brown
  "a#": [230, 210, 170], // tambourine — sandy
};

// Graphic-notation voice signatures for the pad-idle animation renderer.
// t: "s"=sine "t"=triangle "q"=square "w"=sawtooth "n"=noise
// Format per voice: [type, freq, volume]. Consumed by drawGrid() in each
// notepat implementation.
export const PERCUSSION_NOTATION = {
  c:  [["t",1800,1.0],["n",4000,0.55],["q",180,1.2],["w",90,1.0],["s",44,1.9],["s",54,1.3],["q",120,0.85]],
  d:  [["n",2200,0.55],["t",220,0.4],["q",180,0.2]],
  e:  [["q",2500,0.9],["n",6000,0.6],["n",1600,0.75],["n",1700,0.6],["n",1500,0.5],["n",1800,0.45]],
  f:  [["n",3200,0.45],["q",1800,0.22],["t",2400,0.18]],
  g:  [["n",7000,0.35],["n",5000,0.2]],
  a:  [["n",6500,0.3],["n",4800,0.18]],
  b:  [["n",4200,0.28],["q",3100,0.1],["q",4600,0.08]],
  "c#": [["n",3500,0.42],["n",6500,0.28],["q",4200,0.08]],
  "d#": [["n",5500,0.38],["n",8500,0.25]],
  "f#": [["q",810,0.22],["q",540,0.18]],
  "g#": [["t",900,0.35],["q",1800,0.14]],
  "a#": [["n",7000,0.3],["n",4500,0.18],["q",6500,0.1]],
};

// Wave type → accent color for the notation glyph.
export const NOTATION_WAVE_RGB = {
  s: [120, 200, 255],   // sine      — sky blue
  t: [120, 255, 180],   // triangle  — mint
  q: [255, 220, 120],   // square    — amber
  w: [255, 140, 80],    // sawtooth  — orange
  n: [220, 220, 230],   // noise     — pale grey
};

// Fire the layered voices for a single drum. Pure function — no
// persistent state and no module-level side effects.
//
// Params:
//   sound        AC sound API (needs .synth).
//   letter       "c" | "d" | ... | "a#". Unknown letters are ignored.
//   opts:
//     volume        master volume scalar (0.1–2.2 clamped). Default 1.0.
//     pan           stereo pan (−1..1). Default 0.
//     pitchFactor   tone multiplier for transpose (0.25–4 clamped). Default 1.
//     phase         "down" (live key press, sustains go into holdVoices),
//                   "up"   (legacy release — ignored), or
//                   "both" (self-contained one-shot, sustains play finite).
//                   Default "both".
//     holdVoices    array to receive live-sustain voice records. If absent
//                   or phase !== "down", sustains fire as finite durations.
//     onVoice       optional callback(voiceDesc) called once per voice
//                   fired, for inspector/debug overlays. `voiceDesc` has
//                   { type, tone, duration, volume, attack, decay, pan, kind }
//                   where kind is "hit" or "sustain".
export function playPercussion(sound, letter, opts = {}) {
  if (!sound?.synth) return;
  const {
    volume = 1.0,
    pan = 0,
    pitchFactor = 1.0,
    phase = "both",
    holdVoices = null,
    onVoice = null,
  } = opts;

  const v = Math.max(0.1, Math.min(2.2, volume));
  const pf = Math.max(0.25, Math.min(4, pitchFactor));

  const fireDown = phase !== "up";
  const isLive = phase === "down" && Array.isArray(holdVoices);
  if (!fireDown) return;

  // Per-hit random helpers (inline, stateless). `rj` jitters around a center
  // by a ± fraction; `rn` returns a uniform range.
  const rj = (center, frac) => center * (1 + (Math.random() - 0.5) * 2 * frac);
  const rn = (min, max) => min + Math.random() * (max - min);

  const inspect = (params, kind) => {
    if (!onVoice) return;
    onVoice({
      type: params?.type,
      tone: params?.tone,
      duration: params?.duration,
      volume: params?.volume,
      attack: params?.attack,
      decay: params?.decay,
      pan: params?.pan,
      kind,
    });
  };

  const addSustain = (params, bothDuration, releaseFade, releaseUpdate, onRelease) => {
    inspect({ ...params, duration: bothDuration }, "sustain");
    if (isLive) {
      const handle = sound.synth({ ...params, duration: Infinity });
      if (handle) {
        const liveEntry = { handle, releaseFade, releaseUpdate, onRelease };
        if (params?.tone !== undefined) liveEntry.baseTone = params.tone / pf;
        if (params?.volume !== undefined) liveEntry.baseVolumeUnit = params.volume / v;
        if (params?.base !== undefined) liveEntry.sampleBase = params.base;
        if (releaseUpdate?.tone !== undefined) liveEntry.releaseBaseTone = releaseUpdate.tone / pf;
        if (releaseUpdate?.volume !== undefined) liveEntry.releaseBaseVolumeUnit = releaseUpdate.volume / v;
        holdVoices.push(liveEntry);
      }
      return handle;
    }
    return sound.synth({ ...params, duration: bothDuration });
  };

  const addHit = (params) => {
    inspect(params, "hit");
    const handle = sound.synth(params);
    if (isLive && handle) {
      const liveEntry = {
        handle,
        ignoreRelease: true,
        releaseFade: 0,
        tailSeconds: Math.max(
          Number(params?.duration) || 0,
          (Number(params?.attack) || 0) + (Number(params?.decay) || 0),
        ),
      };
      if (params?.tone !== undefined) liveEntry.baseTone = params.tone / pf;
      if (params?.volume !== undefined) liveEntry.baseVolumeUnit = params.volume / v;
      if (params?.base !== undefined) liveEntry.sampleBase = params.base;
      holdVoices.push(liveEntry);
    }
    return handle;
  };

  const addReleaseBurst = (onRelease) => {
    if (isLive && onRelease) holdVoices.push({ handle: null, releaseFade: 0, onRelease });
  };

  switch (letter) {
    // === ONE-SHOT DRUMS (c/d/e/f/g) ===
    case "c": { // kick — tight TR-808: transient + short body + optional sub
      const downPan = pan + rn(-0.02, 0.02);
      addHit({ type: "noise", tone: 2500 * pf, duration: 0.0025, volume: rj(0.50, 0.12) * v, attack: 0.0002, decay: 0.0022, pan: downPan });
      addHit({ type: "sine", tone: 200 * pf, duration: 0.012, volume: rj(1.1, 0.10) * v, attack: 0.0005, decay: 0.011, pan: downPan });
      addHit({ type: "sine", tone: 150 * pf, duration: 0.045, volume: rj(1.3, 0.10) * v, attack: 0.001, decay: 0.044, pan: downPan });
      addHit({ type: "sine", tone: 90 * pf, duration: 0.080, volume: rj(0.85, 0.12) * v, attack: 0.002, decay: 0.078, pan: downPan });
      addHit({ type: "sine", tone: 55 * pf, duration: rj(0.35, 0.20), volume: rj(1.0, 0.12) * v, attack: 0.003, decay: 0.345, pan: downPan });
      break;
    }

    case "d": { // snare — TR-909-leaning: big transient + short tone + dominant noise
      const downPan = pan + rn(-0.02, 0.02);
      addHit({ type: "noise", tone: 3500 * pf, duration: 0.004, volume: rj(0.95, 0.10) * v, attack: 0.0001, decay: 0.004, pan: downPan });
      addHit({ type: "sine", tone: 238 * pf, duration: 0.030, volume: rj(0.35, 0.12) * v, attack: 0.0003, decay: 0.029, pan: downPan });
      addHit({ type: "sine", tone: 476 * pf, duration: 0.030, volume: rj(0.28, 0.12) * v, attack: 0.0003, decay: 0.029, pan: downPan });
      addHit({ type: "noise", tone: 3500 * pf, duration: rj(0.11, 0.20), volume: rj(0.85, 0.10) * v, attack: 0.0005, decay: 0.108, pan: downPan + rn(-0.04, 0.04) });
      addHit({ type: "noise", tone: 1800 * pf, duration: rj(0.07, 0.20), volume: rj(0.38, 0.15) * v, attack: 0.0008, decay: 0.068, pan: downPan + rn(-0.04, 0.04) });
      addHit({ type: "triangle", tone: 180 * pf, duration: 0.025, volume: rj(0.22, 0.15) * v, attack: 0.001, decay: 0.024, pan: downPan });
      break;
    }

    case "e": { // clap — TR-808 4-burst pattern via staggered attacks (one-shot)
      const downPan = pan + rn(-0.06, 0.02);
      addHit({ type: "noise", tone: 1000 * pf, duration: 0.025, volume: rj(0.90, 0.15) * v, attack: 0.005, decay: 0.020, pan: downPan });
      addHit({ type: "noise", tone: 1100 * pf, duration: 0.035, volume: rj(0.95, 0.15) * v, attack: 0.015, decay: 0.020, pan: downPan });
      addHit({ type: "noise", tone: 900 * pf,  duration: 0.045, volume: rj(0.85, 0.15) * v, attack: 0.025, decay: 0.020, pan: downPan });
      addHit({ type: "noise", tone: 3000 * pf, duration: 0.008, volume: rj(0.55, 0.15) * v, attack: 0.001, decay: 0.007, pan: downPan });
      addHit({ type: "noise", tone: 1000 * pf, duration: rj(0.14, 0.25), volume: rj(0.85, 0.15) * v, attack: 0.045, decay: 0.135, pan: downPan + rn(-0.02, 0.10) });
      addHit({ type: "noise", tone: 2200 * pf, duration: rj(0.10, 0.25), volume: rj(0.35, 0.18) * v, attack: 0.050, decay: 0.095, pan: downPan + rn(-0.02, 0.10) });
      break;
    }

    case "f": { // snap — finger snap physics (one-shot)
      const downPan = pan + rn(-0.04, 0.04);
      addHit({ type: "noise", tone: 6000 * pf, duration: 0.003, volume: rj(0.70, 0.15) * v, attack: 0.0001, decay: 0.0028, pan: downPan });
      addHit({ type: "sine", tone: 2100 * pf, duration: rj(0.045, 0.20), volume: rj(0.55, 0.12) * v, attack: 0.0005, decay: 0.044, pan: downPan });
      addHit({ type: "sine", tone: 3500 * pf, duration: rj(0.020, 0.25), volume: rj(0.28, 0.18) * v, attack: 0.0005, decay: 0.019, pan: downPan });
      break;
    }

    case "g": { // closed hi-hat — 4-square cluster + subtle lift click on release
      const downPan = pan + rn(-0.03, 0.03);
      for (const f of HAT_FREQS) {
        addHit({ type: "square", tone: f * pf, duration: rj(0.008, 0.20), volume: rj(0.18, 0.18) * v, attack: 0.0005, decay: 0.0075, pan: downPan });
      }
      addHit({ type: "noise", tone: 8000 * pf, duration: rj(0.040, 0.20), volume: rj(0.38, 0.12) * v, attack: 0.0005, decay: 0.038, pan: downPan });
      addReleaseBurst(() => {
        sound.synth({ type: "noise", tone: 9000 * pf, duration: 0.004, volume: rj(0.22, 0.20) * v, attack: 0.0002, decay: 0.0038, pan: downPan });
        sound.synth({ type: "square", tone: 6000 * pf, duration: 0.003, volume: rj(0.08, 0.25) * v, attack: 0.0003, decay: 0.0027, pan: downPan });
      });
      break;
    }

    case "a": { // open hi-hat — TR-808 cluster, LONG sustain. Foot-pedal release = damp fast.
      const downPan = pan + rn(-0.04, 0.04);
      for (const f of HAT_FREQS) {
        addHit({ type: "square", tone: f * pf, duration: 0.012, volume: rj(0.16, 0.18) * v, attack: 0.0005, decay: 0.011, pan: downPan });
      }
      addHit({ type: "noise", tone: 8200 * pf, duration: 0.012, volume: rj(0.42, 0.12) * v, attack: 0.0003, decay: 0.011, pan: downPan });
      addSustain(
        { type: "noise", tone: 7000 * pf, volume: rj(0.32, 0.15) * v, attack: 0.003, decay: 0, pan: downPan + rn(-0.02, 0.08) },
        rj(0.40, 0.25),
        0.12,
        { tone: 3500 },
      );
      addSustain(
        { type: "noise", tone: 5000 * pf, volume: rj(0.20, 0.18) * v, attack: 0.003, decay: 0, pan: downPan + rn(-0.02, 0.08) },
        rj(0.25, 0.25),
        0.10,
        { tone: 2800 },
      );
      addSustain(
        { type: "square", tone: 800 * pf, volume: rj(0.08, 0.20) * v, attack: 0.005, decay: 0, pan: downPan },
        rj(0.20, 0.25),
        0.08,
      );
      break;
    }

    case "b": { // ride — bell ping + long shimmer sustain
      const downPan = pan + rn(-0.03, 0.03);
      addHit({ type: "square", tone: 800 * pf, duration: 0.020, volume: rj(0.10, 0.18) * v, attack: 0.0005, decay: 0.019, pan: downPan });
      addHit({ type: "square", tone: 540 * pf, duration: 0.020, volume: rj(0.08, 0.18) * v, attack: 0.0005, decay: 0.019, pan: downPan });
      addSustain(
        { type: "sine", tone: 440 * pf, volume: rj(0.24, 0.12) * v, attack: 0.0008, decay: 0, pan: downPan },
        rj(0.40, 0.20),
        0.25,
      );
      addSustain(
        { type: "sine", tone: 587 * pf, volume: rj(0.20, 0.12) * v, attack: 0.0008, decay: 0, pan: downPan },
        rj(0.40, 0.20),
        0.25,
      );
      addSustain(
        { type: "noise", tone: 4200 * pf, volume: rj(0.26, 0.12) * v, attack: 0.005, decay: 0, pan: downPan + rn(-0.03, 0.03) },
        rj(0.9, 0.20),
        0.30,
      );
      break;
    }

    case "c#": { // crash — explosive noise attack + LONG shimmer wash
      const downPan = pan + rn(-0.05, 0.05);
      addHit({ type: "noise", tone: 8000 * pf, duration: 0.030, volume: rj(0.75, 0.15) * v, attack: 0.0005, decay: 0.029, pan: downPan });
      for (const f of HAT_FREQS) {
        addHit({ type: "square", tone: f * pf, duration: 0.030, volume: rj(0.12, 0.20) * v, attack: 0.0005, decay: 0.029, pan: downPan });
      }
      addSustain(
        { type: "noise", tone: 5000 * pf, volume: rj(0.45, 0.12) * v, attack: 0.008, decay: 0, pan: downPan + rn(-0.04, 0.04) },
        rj(1.4, 0.18),
        0.45,
      );
      addSustain(
        { type: "noise", tone: 7500 * pf, volume: rj(0.30, 0.15) * v, attack: 0.008, decay: 0, pan: downPan + rn(-0.04, 0.04) },
        rj(0.9, 0.18),
        0.35,
      );
      addSustain(
        { type: "square", tone: 800 * pf, volume: rj(0.08, 0.20) * v, attack: 0.008, decay: 0, pan: downPan },
        rj(0.5, 0.20),
        0.20,
      );
      break;
    }

    case "d#": { // splash — short bright cymbal burst (one-shot)
      const downPan = pan + rn(-0.04, 0.04);
      addHit({ type: "noise", tone: 9000 * pf, duration: 0.012, volume: rj(0.55, 0.15) * v, attack: 0.0003, decay: 0.011, pan: downPan });
      addHit({ type: "square", tone: 800 * pf, duration: 0.015, volume: rj(0.14, 0.20) * v, attack: 0.0005, decay: 0.014, pan: downPan });
      addHit({ type: "square", tone: 540 * pf, duration: 0.015, volume: rj(0.10, 0.20) * v, attack: 0.0005, decay: 0.014, pan: downPan });
      addHit({ type: "noise", tone: 6000 * pf, duration: rj(0.35, 0.20), volume: rj(0.42, 0.12) * v, attack: 0.004, decay: 0.345, pan: downPan + rn(-0.03, 0.03) });
      addHit({ type: "noise", tone: 8500 * pf, duration: rj(0.22, 0.20), volume: rj(0.25, 0.15) * v, attack: 0.004, decay: 0.215, pan: downPan + rn(-0.03, 0.03) });
      break;
    }

    case "f#": { // cowbell — TR-808: two triangles 800/540 Hz (one-shot)
      const downPan = pan + rn(-0.03, 0.03);
      addHit({ type: "square", tone: 1800 * pf, duration: 0.004, volume: rj(0.35, 0.15) * v, attack: 0.0002, decay: 0.0038, pan: downPan });
      addHit({ type: "triangle", tone: 800 * pf, duration: rj(0.28, 0.20), volume: rj(0.42, 0.12) * v, attack: 0.0008, decay: 0.275, pan: downPan });
      addHit({ type: "triangle", tone: 540 * pf, duration: rj(0.28, 0.20), volume: rj(0.36, 0.12) * v, attack: 0.0008, decay: 0.275, pan: downPan });
      break;
    }

    case "g#": { // wood block — single triangle @ 2500 Hz (one-shot)
      const downPan = pan + rn(-0.03, 0.03);
      addHit({ type: "noise", tone: 5000 * pf, duration: 0.002, volume: rj(0.35, 0.18) * v, attack: 0.0001, decay: 0.0018, pan: downPan });
      addHit({ type: "triangle", tone: 2500 * pf, duration: rj(0.050, 0.25), volume: rj(0.52, 0.12) * v, attack: 0.0003, decay: 0.048, pan: downPan });
      addHit({ type: "triangle", tone: 1250 * pf, duration: rj(0.050, 0.25), volume: rj(0.18, 0.18) * v, attack: 0.0005, decay: 0.048, pan: downPan });
      break;
    }

    case "a#": { // tambourine — staggered jingle bursts (one-shot)
      const downPan = pan + rn(-0.04, 0.04);
      addHit({ type: "noise", tone: 7000 * pf, duration: 0.08, volume: rj(0.38, 0.18) * v, attack: 0.002, decay: 0.075, pan: downPan });
      addHit({ type: "noise", tone: 7500 * pf, duration: 0.09, volume: rj(0.30, 0.18) * v, attack: 0.015, decay: 0.075, pan: downPan });
      addHit({ type: "noise", tone: 6500 * pf, duration: 0.10, volume: rj(0.25, 0.18) * v, attack: 0.030, decay: 0.070, pan: downPan });
      addHit({ type: "square", tone: 6000 * pf, duration: 0.030, volume: rj(0.14, 0.20) * v, attack: 0.001, decay: 0.028, pan: downPan });
      addHit({ type: "noise", tone: 7000 * pf, duration: rj(0.20, 0.22), volume: rj(0.32, 0.18) * v, attack: 0.050, decay: 0.195, pan: downPan + rn(-0.04, 0.04) });
      addHit({ type: "noise", tone: 4500 * pf, duration: rj(0.15, 0.22), volume: rj(0.20, 0.18) * v, attack: 0.055, decay: 0.145, pan: downPan + rn(-0.04, 0.04) });
      break;
    }
  }
}
