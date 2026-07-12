// lull, 26.07.11
// Self-running A/V loop: a slow melodic bassline + warm sustained pad drive an
// undulating gooey field of nested contour-blobs in a soft dawn palette. The
// liquid breathes with the music and loops seamlessly (an 8-beat harmonic
// cycle). Zero input — autostarts its own audio on boot.

// —— Harmony ——
// A gentle 8-step bassline (one note per beat) over a slow BPM. Root motion
// walks a warm minor-ish progression that resolves back to the start so the
// audio loops as cleanly as the visuals.
const BASS = ["a1", "a1", "c2", "e2", "d2", "d2", "f2", "e2"];
// Pad chord tones (held triad), retuned each downbeat to shadow the bass.
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

let beatCount = 0;
let beatStart = 0;
let beatProgress = 0; // 0..1 through the current beat
let pad = []; // live sustained pad voices (one per chord tone)
let started = false;

const BPM = 66;

function boot({ sound }) {
  sound.bpm(BPM);
  // Lush, dreamy reverb tail across everything.
  sound.room?.set?.({ enabled: true, mix: 0.5, feedback: 0.62 });
}

// Rhythmic bass + pad retune land on the metronome grid → locked to the beat.
function beat({ sound: { bpm, synth, time } }) {
  bpm(BPM);
  const step = beatCount % BASS.length;

  // Warm sine bassline — one soft note per beat, panned in a slow drift.
  synth({
    tone: BASS[step],
    type: "sine",
    beats: 1.6, // let notes overlap slightly for a legato swell
    attack: 0.12,
    decay: 0.7,
    volume: 0.5,
    pan: Math.sin(beatCount * 0.4) * 0.3,
  });

  // A soft triangle counter-melody an octave up on alternating beats — the lead.
  if (step % 2 === 0) {
    synth({
      tone: BASS[step].replace(/\d/, (d) => String(+d + 2)),
      type: "triangle",
      beats: 1.4,
      attack: 0.2,
      decay: 0.6,
      volume: 0.18,
      pan: -Math.sin(beatCount * 0.4) * 0.4,
    });
  }

  // The sustained pad: create it once, then retune the held voices each downbeat.
  const chord = PAD[step];
  if (pad.length === 0) {
    for (let i = 0; i < chord.length; i++) {
      pad.push(
        synth({
          tone: chord[i],
          type: "sine",
          duration: "🔁", // held forever until updated/killed
          attack: 0.6,
          decay: 0.9,
          volume: 0.12,
          pan: (i - 1) * 0.35,
        }),
      );
    }
    started = true;
  } else {
    for (let i = 0; i < pad.length; i++) {
      pad[i]?.update?.({ tone: chord[i % chord.length] });
    }
  }

  beatCount = (beatCount + 1) % BASS.length; // loop the score forever
  beatStart = time;
  beatProgress = 0;
}

// Poll live audio (mandatory) + smooth inter-beat progress for silky motion.
function sim({ sound: { speaker, time, bpm } }) {
  speaker?.poll();
  beatProgress = (time - beatStart) / (60 / bpm()) || 0;
}

function paint({ ink, box, screen, sound, num, paintCount }) {
  const { width: w, height: h } = screen;

  // Soft dawn wash — translucent veil each frame so blobs leave gooey trails
  // rather than snapping. The gradient itself slowly breathes vertically.
  ink("fade:midnightblue-rebeccapurple:vertical", 46).box(0, 0, w, h);

  // —— audio reads ——
  const bands = sound.speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const lowMid = bands.find((b) => b.name === "lowMid")?.amplitude || 0;
  const air = bands.find((b) => b.name === "air")?.amplitude || 0;
  const amp = sound.speaker?.amplitudes?.left || 0;

  // Global phase drives the loop: full harmonic cycle = BASS.length beats.
  // Blend beatCount + inter-beat progress → continuous, wraps at the loop point.
  const cyclePhase =
    ((beatCount + beatProgress) % BASS.length) / BASS.length; // 0..1, seamless
  const orbit = cyclePhase * Math.PI * 2;

  const cx = w / 2;
  const cy = h / 2;
  const base = Math.min(w, h);

  // —— the gooey field ——
  // Concentric wobbling contours, each a closed loop of points warped by two
  // out-of-phase sine ripples. Radii swell with the bass; wobble with air/amp.
  const RINGS = 7;
  const SEG = 96; // polygon resolution — high for smooth liquid edges
  const swell = 1 + bass * 1.4;
  const wobble = 0.16 + air * 0.5 + amp * 0.3;

  for (let r = RINGS; r >= 1; r--) {
    const t = r / RINGS;
    // Radius breathes with the pad phase and the bass swell.
    const radius =
      base *
      (0.08 + t * 0.42) *
      swell *
      (1 + 0.06 * Math.sin(orbit * 2 + t * 6));

    // Slow drift of the whole blob so nothing is static.
    const dx = Math.cos(orbit + t * 3) * base * 0.06 * (1 + lowMid);
    const dy = Math.sin(orbit * 1.3 + t * 2) * base * 0.06;

    // Soft palette: hue rides the ring index + the harmonic cycle.
    const hue = (cyclePhase + t * 0.5 + 0.55) % 1;
    const [cr, cg, cb] = num
      .hslToRgb(hue, 0.55, 0.62 - t * 0.12)
      .map((v) => Math.round(v * 255));
    const alpha = 40 + (1 - t) * 90 + amp * 60;

    // Build the wobbling closed contour.
    const pts = [];
    for (let s = 0; s <= SEG; s++) {
      const a = (s / SEG) * Math.PI * 2;
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

  // Warm molten core — a soft pulsing heart that flares on the downbeat.
  const beatPulse = 1 - beatProgress; // brightest right on each beat
  const coreR = base * 0.05 * (1 + bass * 2) + beatPulse * base * 0.02;
  const [gr, gg, gb] = num.hslToRgb((cyclePhase + 0.05) % 1, 0.6, 0.75);
  for (let i = 4; i > 0; i--) {
    ink(gr * 255, gg * 255, gb * 255, 30 + beatPulse * 40).box(
      cx,
      cy,
      coreR * (i / 4) * 2,
      coreR * (i / 4) * 2,
      "*center",
    );
  }

  // A single slow drifting highlight droplet, wrapping every 480 frames so the
  // top-layer motion also loops cleanly.
  const dt = Number(paintCount % 480n) / 480;
  const da = dt * Math.PI * 2;
  ink(255, 250, 235, 90 + amp * 120).box(
    cx + Math.cos(da) * base * 0.28,
    cy + Math.sin(da * 1.5) * base * 0.32,
    6 + bass * 30,
    6 + bass * 30,
    "*center",
  );
}

function leave() {
  pad.forEach((v) => v?.kill?.(0.5));
  pad = [];
}

export { boot, beat, sim, paint, leave };
