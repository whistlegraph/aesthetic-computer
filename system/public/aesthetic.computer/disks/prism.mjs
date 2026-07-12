// prism, 26.07.11
// Self-running kaleidoscopic A/V loop: a BPM-locked minor arpeggio drives
// 8-fold mirrored geometry that pulses, rotates, and cycles saturated color.
// Zero input — autostarts its own sound. Loops seamlessly (state cycles on a
// 16-step arpeggio + a wrapping paint phase).

// One looping bar of an A-minor arpeggio (16 steps). First step == the seam,
// so the score returns to its start and the visuals return to their start.
const ARP = [
  "a2", "e3", "a3", "c4", "e4", "c4", "a3", "e3",
  "a2", "g3", "b3", "d4", "g4", "d4", "b3", "g3",
];
const BASS = ["a1", "a1", "f1", "f1", "c2", "c2", "e1", "e1"]; // half-time root

const SYMMETRY = 8; // kaleidoscope fold count

let step = 0; // arpeggio index (loops 0..15)
let bassStep = 0; // bass index (loops 0..7)
let beatCount = 0; // total beats since boot
let beatStart = 0; // sound.time at last beat
let beatProgress = 0; // 0..1 through the current beat
let pulse = 0; // eased downbeat flash, decays toward 0

function boot({ sound }) {
  sound.bpm(132); // brisk, hypnotic
  // Lush tail + a touch of grit for the "kaleidoscope hum".
  sound.room?.set?.({ enabled: true, mix: 0.32, feedback: 0.6 });
}

// Schedule the rhythmic line on the metronome — locks audio to the beat grid.
function beat({ sound: { bpm, synth } }) {
  bpm(132);

  const note = ARP[step % ARP.length];
  // Pan sweeps across the stereo field with the arpeggio for width.
  const pan = Math.sin((step / ARP.length) * Math.PI * 2) * 0.7;
  synth({
    tone: note,
    type: "triangle",
    beats: 0.9,
    attack: 0.005,
    decay: 0.55,
    volume: 0.5,
    pan,
  });

  // A brighter "shimmer" octave-up sparkle on every 4th step.
  if (step % 4 === 0) {
    synth({
      tone: note,
      type: "sine",
      beats: 0.4,
      attack: 0.002,
      decay: 0.4,
      volume: 0.22,
      pan: -pan,
    });
  }

  // Sub-bass root on the half-beat pulse — the downbeat you can feel.
  if (step % 2 === 0) {
    synth({
      tone: BASS[bassStep % BASS.length],
      type: "sawtooth",
      beats: 1.6,
      attack: 0.01,
      decay: 0.5,
      volume: 0.5,
    });
    bassStep = (bassStep + 1) % BASS.length;
  }

  // Closed-hat tick off the beat keeps momentum.
  synth({ tone: "noise-white", beats: 0.12, attack: 0.001, decay: 0.2, volume: 0.16 });

  pulse = 1; // fresh downbeat flash
  step = (step + 1) % ARP.length;
  beatCount++;
  beatStart = undefined; // reset in sim on first read this beat
}

// Poll live audio (mandatory) + smooth inter-beat progress + decay the pulse.
function sim({ sound: { speaker, time, bpm } }) {
  speaker?.poll();
  if (beatStart === undefined) beatStart = time;
  beatProgress = ((time - beatStart) / (60 / bpm())) || 0;
  if (beatProgress < 0) beatProgress = 0;
  if (beatProgress > 1) beatProgress = 1;
  pulse *= 0.92; // exponential fade between downbeats
}

function paint({ ink, box, line, circle, screen, sound, num, paintCount, spin, zoom, blur }) {
  const { width: w, height: h } = screen;
  const cx = w / 2, cy = h / 2;

  // --- Live audio reads -----------------------------------------------------
  const bands = sound.speaker?.frequencies?.left || [];
  const bandAmp = (name) => bands.find((b) => b.name === name)?.amplitude || 0;
  const bass = bandAmp("subBass");
  const lowMid = bandAmp("lowMid");
  const mid = bandAmp("mid");
  const air = bandAmp("air");
  const amp = sound.speaker?.amplitudes?.left || 0;
  const beatHit = sound.speaker?.beat?.detected ? 1 : 0;

  // Combined "energy" for scale — beat clock + live audio, so it hits even
  // before the analyser warms up.
  const downbeat = pulse; // 1 at each note-on, decays
  const energy = Math.min(1, downbeat * 0.7 + bass * 1.2 + amp * 0.6 + beatHit * 0.3);

  // --- Feedback bloom base: recycle the previous frame into a slow vortex ----
  // A tiny zoom + spin every frame recycles what we already drew, so the
  // kaleidoscope "breathes" outward. No hard wipe — trails glow.
  zoom?.(1.008 + energy * 0.01, 0.5, 0.5); // gentle bloom, stronger on downbeats
  spin?.(0.35 + downbeat * 1.2); // slow rotation, kicks on the beat
  ink(6, 4, 16, 30).box(0, 0, w, h); // deep-violet trail veil over the recycled frame

  // --- Kaleidoscope geometry ------------------------------------------------
  // Build ONE wedge, mirrored/rotated SYMMETRY times around the center.
  // Everything keys to a wrapping phase so frame 0 == frame N (seamless loop).
  const phase = Number(paintCount % 480n) / 480; // 0..1, wraps every 480 frames
  const spinAng = phase * Math.PI * 2; // full turn per loop
  const beatPhase = (beatCount % ARP.length) + beatProgress; // 0..16 smooth
  const maxR = Math.min(w, h) * 0.62;

  // Concentric radial "petals" — position along the arpeggio drives the count.
  const rings = 6;
  for (let s = 0; s < SYMMETRY; s++) {
    const baseAng = (s / SYMMETRY) * Math.PI * 2 + spinAng;
    for (let r = 0; r < rings; r++) {
      const rr = ((r + 1) / rings);
      // radius breathes with energy + a per-ring sine so petals ripple outward
      const wobble = num.wave(beatPhase * 0.6 + r * 0.9 + s) * 0.06;
      const radius = maxR * (rr + wobble) * (0.75 + energy * 0.4);

      // Two mirrored spokes per fold = kaleidoscopic reflection.
      for (const mirror of [1, -1]) {
        const ang = baseAng + mirror * (0.18 + lowMid * 0.5 + rr * 0.35);
        const x = cx + Math.cos(ang) * radius;
        const y = cy + Math.sin(ang) * radius;
        const nx = cx + Math.cos(ang) * radius * 0.62;
        const ny = cy + Math.sin(ang) * radius * 0.62;

        const thick = 1 + Math.round((1 - rr) * 4 + mid * 6);
        ink("rainbow", 150).line(nx, ny, x, y, thick);

        // A jewel node at each petal tip; size pulses with the downbeat.
        const nodeR = 2 + (1 - rr) * 6 + energy * 10 + air * 14;
        ink("rainbow", 200).circle(x, y, nodeR, true);
      }
    }
  }

  // --- Central mandala core: nested rotating polygons ----------------------
  const coreSides = SYMMETRY;
  for (let k = 3; k >= 0; k--) {
    const kr = (Math.min(w, h) * 0.06) * (k + 1) * (1 + energy * 0.9);
    const rot = spinAng * (k % 2 === 0 ? 1 : -1) + beatPhase * 0.2;
    let px = null, py = null; // loop closes because i runs 0..coreSides inclusive
    for (let i = 0; i <= coreSides; i++) {
      const a = (i / coreSides) * Math.PI * 2 + rot;
      const x = cx + Math.cos(a) * kr;
      const y = cy + Math.sin(a) * kr;
      if (px !== null) ink("rainbow", 220).line(px, py, x, y, 2);
      px = x; py = y;
    }
  }

  // A soft blur on strong downbeats blooms the whole kaleidoscope for an instant.
  if (downbeat > 0.5) blur?.(1);

  // Bright pulsing heart — the beat you can see.
  ink(255, 255, 255, 160 + downbeat * 95)
    .circle(cx, cy, 6 + downbeat * 26 + bass * 40, true);
}

export { boot, beat, sim, paint };
