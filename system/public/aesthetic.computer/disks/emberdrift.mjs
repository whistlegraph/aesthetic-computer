// emberdrift, 26.07.11
// Self-running cosmic A/V loop: a punchy bass pulse + rolling arpeggio drive a
// swarm of stars that streak outward and burst into sparks on every beat.
// Zero input required. Loops seamlessly on a 16-step bar.

// --- Score ---------------------------------------------------------------
// One bar = 16 sixteenth-note steps, loops forever.
const BPM = 132;
// Bass pulse: a low kick-like note on the strong steps (four-on-the-floor).
const BASS = ["c1", "c1", "g1", "c1"]; // one per beat (fires on downbeats)
// Arpeggio: a bright synth line, one note per 16th step. Minor-key, spacey.
const ARP = [
  "c4", "g4", "a4", "e5",
  "c5", "g4", "d5", "b4",
  "c4", "a4", "c5", "g5",
  "e5", "d5", "b4", "g4",
];

let step = 0; // 0..15 sixteenth-step within the bar
let beatStart = 0, beatProgress = 0; // inter-step easing
let flash = 0; // decays each frame; kicked to 1 on a bass hit
let bassKick = 0; // decays; drives the central bloom

// --- Particles -----------------------------------------------------------
const STAR_COUNT = 240; // 3D drifting stars
const stars = [];
let sparks = []; // transient beat-burst embers

let seeded = false;
function seedStars() {
  const R = () => Math.random();
  for (let i = 0; i < STAR_COUNT; i++) {
    stars.push({
      x: (R() - 0.5) * 2,
      y: (R() - 0.5) * 2,
      z: R() * 1 + 0.001, // depth 0..1
      hue: R(),
    });
  }
  seeded = true;
}

// --- Lifecycle -----------------------------------------------------------

function boot({ sound }) {
  sound.bpm(BPM * 4); // beat() fires per 16th step
  sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.6 });
  if (!seeded) seedStars();
}

// beat() fires once per 16th step (bpm set to BPM*4). Schedule the groove.
function beat({ sound: { bpm, synth, time } }) {
  bpm(BPM * 4);

  // Bass pulse on the 4 downbeats (steps 0,4,8,12) — kick-like.
  if (step % 4 === 0) {
    const bn = BASS[(step / 4) % BASS.length];
    synth({
      tone: bn,
      type: "sawtooth",
      beats: 1.6,
      attack: 0.005,
      decay: 0.4,
      volume: 0.85,
      pan: 0,
    });
    // Sub thump layer.
    synth({
      tone: bn,
      type: "sine",
      beats: 1.2,
      attack: 0.002,
      decay: 0.3,
      volume: 0.6,
    });
    bassKick = 1;
  }

  // Hi-hat texture on off-steps.
  if (step % 2 === 1) {
    synth({
      tone: "noise-white",
      beats: 0.25,
      attack: 0.001,
      decay: 0.2,
      volume: step % 4 === 3 ? 0.22 : 0.12,
    });
  }

  // Arpeggio: one note per step, panned across the field.
  const an = ARP[step % ARP.length];
  synth({
    tone: an,
    type: "triangle",
    beats: 0.9,
    attack: 0.005,
    decay: 0.55,
    volume: 0.4,
    pan: -0.6 + (step % ARP.length) / (ARP.length - 1) * 1.2,
  });

  flash = 1; // visible pulse on every step
  step = (step + 1) % 16;
  beatStart = time;
  beatProgress = 0;
}

function sim({ sound: { speaker, time, bpm } }) {
  speaker?.poll();
  beatProgress = (time - beatStart) / (60 / bpm()) || 0;
  if (beatProgress < 0) beatProgress = 0;
  if (beatProgress > 1) beatProgress = 1;

  // Advance drifting stars toward the camera.
  for (let i = 0; i < stars.length; i++) {
    const s = stars[i];
    s.z -= 0.004 + bassKick * 0.01; // faster surge on a kick
    if (s.z <= 0.001) {
      s.x = (Math.random() - 0.5) * 2;
      s.y = (Math.random() - 0.5) * 2;
      s.z = 1;
      s.hue = Math.random();
    }
  }

  // Advance & cull sparks.
  for (let i = 0; i < sparks.length; i++) {
    const p = sparks[i];
    p.x += p.vx;
    p.y += p.vy;
    p.vx *= 0.96;
    p.vy *= 0.96;
    p.life -= 0.03;
  }
  sparks = sparks.filter((p) => p.life > 0);

  // Decay pulses.
  flash *= 0.86;
  bassKick *= 0.9;
}

function paint({ ink, box, circle, line, plot, screen, sound, num }) {
  const w = screen.width, h = screen.height;
  const cx = w / 2, cy = h / 2;
  const foc = Math.min(w, h) * 0.9;

  // Deep-space veil (trail fade instead of hard wipe → glowing streaks).
  ink(4, 2, 14, 40).box(0, 0, w, h);

  // Live audio reads.
  const bands = sound.speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const air = bands.find((b) => b.name === "air")?.amplitude || 0;
  const amp = sound.speaker?.amplitudes?.left || 0;
  const beatDet = sound.speaker?.beat?.detected;

  // Central bloom on the downbeat / detected beat.
  const bloom = Math.max(bassKick, beatDet ? 1 : 0);
  if (bloom > 0.02) {
    const R = Math.min(w, h) * (0.12 + bass * 0.6) * bloom;
    for (let i = 5; i > 0; i--) {
      ink(120, 40, 200, 26 * bloom).circle(cx, cy, R * (i / 5), true);
    }
    ink(255, 200, 255, 180 * bloom).circle(cx, cy, 6 + bass * 30, true);
  }

  // Starfield: project 3D stars, streak them radially, spawn sparks on beat.
  const streak = 8 + flash * 22 + amp * 40; // streak length
  for (let i = 0; i < stars.length; i++) {
    const s = stars[i];
    const px = cx + (s.x / s.z) * foc * 0.5;
    const py = cy + (s.y / s.z) * foc * 0.5;
    if (px < -20 || px > w + 20 || py < -20 || py > h + 20) continue;

    const near = 1 - s.z; // 0 far .. 1 near
    const bright = 60 + near * 195;
    // Accent color cycling from the star's hue toward the ember palette.
    const [r, g, b] = num.hslToRgb(
      (s.hue * 0.4 + 0.7) % 1, // violet→magenta→cyan band
      0.85,
      0.5 + near * 0.4,
    ).map((v) => v * 255);

    // Radial streak from center → gives warp motion.
    const dx = px - cx, dy = py - cy;
    const len = Math.hypot(dx, dy) || 1;
    const ux = dx / len, uy = dy / len;
    const tail = streak * near;
    ink(r, g, b, bright).line(px, py, px - ux * tail, py - uy * tail);
    ink(255, 255, 255, bright).plot(px | 0, py | 0);

    // On a strong step-flash, near stars throw a spark.
    if (flash > 0.7 && near > 0.75 && Math.random() < 0.25 && sparks.length < 220) {
      const sp = 1.5 + Math.random() * 3;
      const ang = Math.random() * Math.PI * 2;
      sparks.push({
        x: px, y: py,
        vx: Math.cos(ang) * sp + ux * 2,
        vy: Math.sin(ang) * sp + uy * 2,
        life: 1,
        hue: s.hue,
      });
    }
  }

  // Draw sparks — bright ember bursts.
  for (let i = 0; i < sparks.length; i++) {
    const p = sparks[i];
    const [r, g, b] = num.hslToRgb((p.hue * 0.2 + 0.06) % 1, 0.95, 0.6)
      .map((v) => v * 255);
    ink(r, g, b, 240 * p.life).circle(p.x, p.y, 1 + p.life * 2.5, true);
  }

  // Beat ring — a shockwave that expands within each step (seamless loop).
  if (flash > 0.05) {
    const rr = Math.min(w, h) * 0.5 * beatProgress;
    ink(180, 120, 255, 120 * (1 - beatProgress) * flash)
      .circle(cx, cy, rr, false, 2 + air * 6);
  }

  // Corner-to-center vignette accent stripes from the air band (shimmer).
  if (air > 0.05) {
    ink(200, 255, 255, air * 120).circle(cx, cy, Math.min(w, h) * 0.46, false, 1);
  }
}

export { boot, beat, sim, paint };
