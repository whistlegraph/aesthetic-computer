// bloomwell, 26.07.11
// Self-running SUPERSAMPLED FEEDBACK MIRROR VORTEX. A tiny seed motif is drawn
// each frame into an offscreen buffer at 2× and downsampled (SSAA → crisp edges),
// then the WHOLE framebuffer is recycled with a zoom+spin+partial-alpha veil
// (transform-feedback) so it blooms into an infinite vortex. steal()/putback()
// mirror the frame into 4-fold symmetry → a breathing kaleidoscope mandala.
// Zero input: autostarts a BPM-locked arpeggio + bass. Loops seamlessly.

// One looping bar (16 steps) of a D-minor arpeggio. Step 0 == the seam.
const ARP = [
  "d2", "a2", "d3", "f3", "a3", "f3", "d3", "a2",
  "d2", "c3", "e3", "g3", "c4", "g3", "e3", "c3",
];
const BASS = ["d1", "d1", "bb1", "bb1", "g1", "g1", "a1", "a1"]; // half-time root

let step = 0;
let bassStep = 0;
let beatCount = 0;
let beatStart = 0;
let beatProgress = 0;
let pulse = 0; // eased downbeat flash, decays toward 0
let seedBuf = null; // reusable offscreen 2× seed buffer

function boot({ sound, resolution }) {
  resolution?.(360, 640); // pin logical grid → density-3 reel budget + crisp upscale
  sound.bpm(126);
  sound.room?.set?.({ enabled: true, mix: 0.34, feedback: 0.62 }); // lush hum
}

// Schedule the rhythmic line on the metronome — locks audio to the beat grid.
function beat({ sound: { bpm, synth } }) {
  bpm(126);

  const note = ARP[step % ARP.length];
  const pan = Math.sin((step / ARP.length) * Math.PI * 2) * 0.7;
  synth({
    tone: note,
    type: "triangle",
    beats: 0.9,
    attack: 0.004,
    decay: 0.55,
    volume: 0.55,
    pan,
  });

  // Octave-up sine shimmer every 4th step.
  if (step % 4 === 0) {
    synth({
      tone: note,
      type: "sine",
      beats: 0.4,
      attack: 0.002,
      decay: 0.4,
      volume: 0.26,
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
      volume: 0.55,
    });
    bassStep = (bassStep + 1) % BASS.length;
  }

  // Closed-hat tick keeps momentum — noise goes in TYPE, never tone.
  synth({ type: "noise-white", tone: 900, beats: 0.12, attack: 0.001, decay: 0.2, volume: 0.18 });

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
  pulse *= 0.9; // exponential fade between downbeats
}

function paint({
  ink, box, line, circle, screen, sound, num, help, paintCount,
  spin, zoom, blur, paste, painting, steal, putback,
}) {
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

  const downbeat = pulse; // 1 at each note-on, decays
  const energy = Math.min(1, downbeat * 0.6 + bass * 1.2 + amp * 0.6 + beatHit * 0.3);

  // --- Seamless phase (wraps every 480 frames → frame 0 == frame N) ---------
  const phase = (Number(paintCount) % 480) / 480; // paintCount is a Number
  const spinAng = phase * Math.PI * 2; // one full turn per loop
  const beatPhase = (beatCount % ARP.length) + beatProgress; // 0..16 smooth
  const baseHue = (phase * 360 + beatPhase * 22) % 360; // cycles through the wheel

  // --- TRANSFORM-FEEDBACK: recycle the whole previous frame into a vortex ----
  // Small inward-drawn seed + gentle outward zoom → the seed spirals outward and
  // blooms. spin rotates the whole recycled field. A partial-alpha veil keeps it
  // from accumulating to solid white (trails decay).
  zoom?.(1.022 + energy * 0.026, 0.5, 0.5); // strong outward bloom fills the frame
  spin?.(0.6 + downbeat * 1.4); // continuous rotation → kaleidoscopic swirl
  ink(4, 2, 12, 22).box(0, 0, w, h); // deep-indigo veil, low alpha → longer glowing trails

  // --- SUPERSAMPLED SEED MOTIF (rendered at 2×, downscaled → crisp edges) ----
  // A SMALL radially-symmetric mandala at the exact center. Because it's small
  // and the whole frame zooms out each frame, successive seeds spiral outward
  // into an infinite vortex. Drawn with FOLD-way symmetry so it's a mandala.
  const S = 2; // supersample factor
  const FOLDS = 6; // radial symmetry of the seed itself
  const seedSize = Math.floor(Math.min(w, h) * 0.5); // small → leaves room to bloom
  const bw = seedSize * S, bh = seedSize * S;
  seedBuf = painting(bw, bh, (p) => {
    const scx = bw / 2, scy = bh / 2;
    const maxR = (bw / 2) * (0.9 + energy * 0.1);

    for (let f = 0; f < FOLDS; f++) {
      const fold = (f / FOLDS) * Math.PI * 2 + spinAng * 0.5;
      // Petal arm: a curved line of nodes radiating out, mirrored per fold.
      const petals = 4;
      for (let k = 0; k < petals; k++) {
        const rr = (k + 1) / petals;
        const swirl = num.wave(beatPhase * 0.4 + f + k * 0.6) * 0.5;
        const ang = fold + swirl;
        const radius = maxR * rr * (0.55 + energy * 0.45);
        const tx = scx + Math.cos(ang) * radius;
        const ty = scy + Math.sin(ang) * radius;

        // Bold cycling color per node — HSL hue walks the wheel.
        const hue = (baseHue + f * (360 / FOLDS) + k * 30) % 360;
        const [r, g, b] = num.hslToRgb(hue, 95, 55 + air * 25); // 0-255 already
        const nodeR = (2 + (1 - rr) * 6 + energy * 10 + air * 12) * S;
        p.ink(r, g, b, 235).circle(tx, ty, nodeR, true);

        // Thin bright spoke connecting toward center for structure.
        if (k === petals - 1) {
          const [lr, lg, lb] = num.hslToRgb((hue + 40) % 360, 90, 60);
          p.ink(lr, lg, lb, 170).line(scx, scy, tx, ty, (1 + mid * 4) * S);
        }
      }
    }

    // Bright pulsing heart at the seed center — the beat you can see.
    const [hr, hg, hb] = num.hslToRgb((baseHue + 180) % 360, 30, 92);
    p.ink(hr, hg, hb, 200 + downbeat * 55)
      .circle(scx, scy, (3 + downbeat * 14 + bass * 26) * S, true);
  });
  // Downsample-composite (poor-man's SSAA) centered on screen.
  paste(seedBuf, Math.round(cx - seedSize / 2), Math.round(cy - seedSize / 2), 1 / S);

  // --- ECHO FOLD via steal/putback (extra kaleidoscope depth) ---------------
  // Grab a shrinking window around center and stamp a smaller scaled copy back
  // onto the center → a self-similar echo that the spin swirls into deeper folds.
  // Scaling (not a half-tile) avoids the hard mid-screen seam.
  const grab = Math.floor(Math.min(w, h) * 0.7);
  steal(Math.floor(cx - grab / 2), Math.floor(cy - grab / 2), grab, grab);
  const echoScale = 0.5;
  const es = grab * echoScale;
  putback(Math.round(cx - es / 2), Math.round(cy - es / 2), echoScale);

  // A soft bloom on strong downbeats.
  if (downbeat > 0.6) blur?.(1);
}

export { boot, beat, sim, paint };
