// molten, 26.07.11
// Self-running A/V loop: a slow warm melodic bassline + held pad drive a
// LIQUID PIXEL DISPLACEMENT field. Each frame recycles the previous
// framebuffer and re-samples every pixel from a sine-warped source position —
// traveling displacement + a soft dawn gradient veil — so the whole image
// undulates like molten glass. Zero input; autostarts its own audio and loops
// seamlessly on an 8-beat harmonic cycle.

// —— Harmony ——
// One warm sine note per beat over a slow BPM; the root walks a minor-ish
// progression that resolves back to the start so the audio loops cleanly.
const BASS = ["a1", "a1", "c2", "e2", "d2", "d2", "f2", "e2"];
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

const BPM = 60;

let beatCount = 0;
let beatStart = 0;
let beatProgress = 0; // 0..1 through the current beat
let pad = []; // held sustained pad voices

// Displacement state
let prev = null; // snapshot of last frame's pixels
let phase = 0; // ever-advancing flow phase (smoothed in sim)
let swell = 0; // eased bass swell
let shimmer = 0; // eased high-band shimmer

function boot({ resolution, sound }) {
  // Pin the logical grid a touch below the density-3 reel size so the
  // per-pixel displacement loop stays smooth (fewer than 360×640 iterations).
  resolution(300, 534);
  sound.bpm(BPM);
  sound.room?.set?.({ enabled: true, mix: 0.5, feedback: 0.6 }); // lush tail
}

// Rhythmic bass + held pad land on the metronome grid → locked to the beat.
function beat({ sound: { bpm, synth, time } }) {
  bpm(BPM);
  const step = beatCount % BASS.length;

  // Warm sine bass — one soft, overlapping note per beat.
  synth({
    tone: BASS[step],
    type: "sine",
    beats: 1.7,
    attack: 0.14,
    decay: 0.7,
    volume: 0.6,
    pan: Math.sin(beatCount * 0.4) * 0.3,
  });

  // A soft triangle lead an octave up on alternating beats.
  if (step % 2 === 0) {
    synth({
      tone: BASS[step].replace(/\d/, (d) => String(+d + 2)),
      type: "triangle",
      beats: 1.5,
      attack: 0.2,
      decay: 0.6,
      volume: 0.22,
      pan: -Math.sin(beatCount * 0.4) * 0.4,
    });
  }

  // Held pad: create once, then retune the sustained voices each downbeat.
  const chord = PAD[step];
  if (pad.length === 0) {
    for (let i = 0; i < chord.length; i++) {
      pad.push(
        synth({
          tone: chord[i],
          type: "sine",
          duration: "🔁",
          attack: 0.7,
          decay: 0.9,
          volume: 0.16,
          pan: (i - 1) * 0.35,
        }),
      );
    }
  } else {
    for (let i = 0; i < pad.length; i++) {
      pad[i]?.update?.({ tone: chord[i % chord.length] });
    }
  }

  beatCount = (beatCount + 1) % BASS.length; // loop the score forever
  beatStart = time;
  beatProgress = 0;
}

// Poll live audio (mandatory) + smooth inter-beat progress + eased reactivity.
function sim({ sound: { speaker, time, bpm }, num }) {
  speaker?.poll();
  beatProgress = (time - beatStart) / (60 / bpm()) || 0;

  const bands = speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const air = bands.find((b) => b.name === "air")?.amplitude || 0;
  swell = num.lerp(swell, bass, 0.08);
  shimmer = num.lerp(shimmer, air, 0.12);

  // Advance the flow phase continuously so displacement never stalls.
  phase += 0.02;
}

function paint({ ink, box, screen, num }) {
  const { width: w, height: h, pixels } = screen;

  // Snapshot last frame so we sample undisplaced source pixels.
  if (!prev || prev.length !== pixels.length) {
    prev = new Uint8ClampedArray(pixels.length);
    prev.set(pixels);
  } else {
    prev.set(pixels);
  }

  // Seamless global phase = full 8-beat harmonic cycle.
  const cyclePhase =
    ((beatCount + beatProgress) % BASS.length) / BASS.length; // 0..1
  const flow = phase; // smooth travel
  const beatPulse = 1 - beatProgress; // brightest on the downbeat

  // Displacement strengths breathe with the music.
  const ampX = 3.0 + swell * 5.0 + beatPulse * 1.5;
  const ampY = 2.0 + swell * 3.0;
  const rot = cyclePhase * Math.PI * 2;

  // —— LIQUID PIXEL DISPLACEMENT ——
  // Re-sample each pixel from a sine-warped source in the previous frame. A
  // per-row horizontal ripple crossed with a per-column vertical ripple makes
  // the whole image undulate like molten glass; a gentle fade toward a warm
  // dawn floor keeps trails from saturating.
  const invW = (Math.PI * 2) / w;
  const invH = (Math.PI * 2) / h;
  for (let y = 0; y < h; y++) {
    // Per-row precompute (hoisted out of the inner loop).
    const ry = y * invH;
    const ox = Math.sin(ry * 3.0 + flow + rot) * ampX;
    const oxi = ox | 0;
    const rowBase = y * w;
    for (let x = 0; x < w; x++) {
      const rx = x * invW;
      const oy = (Math.sin(rx * 4.0 + flow * 1.3 - rot) * ampY) | 0;
      let sx = x + oxi;
      let sy = y + oy;
      // Clamp to edges (avoids wrap seams → clean loop).
      if (sx < 0) sx = 0;
      else if (sx >= w) sx = w - 1;
      if (sy < 0) sy = 0;
      else if (sy >= h) sy = h - 1;
      const si = (sy * w + sx) * 4;
      const di = (rowBase + x) * 4;
      // Warm dawn floor toward which everything gently fades → living gradient.
      pixels[di] = prev[si] * 0.965 + 5; // r warm
      pixels[di + 1] = prev[si + 1] * 0.955 + 2; // g
      pixels[di + 2] = prev[si + 2] * 0.95 + 6; // b (cool base)
      pixels[di + 3] = 255;
    }
  }

  // —— feed the molten material ——
  // A soft dawn gradient veil low-alpha over the whole field so the palette
  // keeps warm even as pixels displace.
  ink("fade:midnightblue-rebeccapurple:vertical", 12).box(0, 0, w, h);

  // A drifting warm molten source: a layered radial glow that orbits with the
  // harmonic cycle so there's always fresh light to smear. Circles only → no
  // opaque square, feathered halo.
  const cx = w / 2 + Math.cos(rot) * w * 0.22;
  const cy = h / 2 + Math.sin(rot * 1.3) * h * 0.22;
  const base = Math.min(w, h);
  const coreR = base * 0.14 * (1 + swell * 1.2) + beatPulse * base * 0.04;
  const [gr, gg, gb] = num
    .hslToRgb((cyclePhase * 40 + 20) % 360, 80, 62)
    .map((v) => v); // hslToRgb already returns 0–255
  const HALO = 7;
  for (let i = HALO; i >= 1; i--) {
    const f = i / HALO; // 1 outer/faint → 0 tight/bright
    const wr = gr + (255 - gr) * (1 - f);
    const wg = gg + (240 - gg) * (1 - f);
    const wb = gb + (215 - gb) * (1 - f);
    const a = (18 + beatPulse * 20 + shimmer * 24) * (1 - f * 0.6);
    ink(wr, wg, wb, a).circle(cx, cy, coreR * f, true);
  }

  // A second cooler counter-glow orbiting opposite → two-color molten mixing.
  const c2x = w / 2 - Math.cos(rot * 0.8) * w * 0.26;
  const c2y = h / 2 - Math.sin(rot) * h * 0.2;
  const [br, bg, bb] = num.hslToRgb((cyclePhase * 40 + 210) % 360, 70, 55);
  const r2 = base * 0.1 * (1 + swell);
  for (let i = 5; i >= 1; i--) {
    const f = i / 5;
    const a = (10 + shimmer * 18) * (1 - f * 0.6);
    ink(br, bg, bb, a).circle(c2x, c2y, r2 * f, true);
  }
}

function leave() {
  pad.forEach((v) => v?.kill?.(0.5));
  pad = [];
  prev = null;
}

export { boot, beat, sim, paint, leave };
