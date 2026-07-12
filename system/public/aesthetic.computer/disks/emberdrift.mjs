// emberdrift, 26.07.11
// Self-running cosmic A/V loop: a punchy bass pulse + rolling arpeggio drive a
// swarm of stars that streak outward and burst into colored sparks on every
// beat. Zero input required. Loops seamlessly on a 16-step bar.

// --- Score ---------------------------------------------------------------
// One bar = 16 sixteenth-note steps, looping forever. We run the metronome at
// a moderate BPM and fire one 16th-note event per beat() tick (like prism's
// per-beat arpeggio) — proven to sound cleanly in capture.
const BPM = 480; // 16th-note grid rate (≈120 quarter-note feel: BPM/4).

// Bass pulse: a low kick-like note on the downbeats (four-on-the-floor).
const BASS = ["c1", "c1", "g1", "c1"]; // one per beat, fired on steps 0,4,8,12
// Arpeggio: a bright synth line, one note per 16th step. Minor-key, spacey.
const ARP = [
  "c4", "g4", "a4", "e5",
  "c5", "g4", "d5", "b4",
  "c4", "a4", "c5", "g5",
  "e5", "d5", "b4", "g4",
];

let step = 0; // 0..15 sixteenth-step within the bar
let beatStart = 0, beatProgress = 0; // inter-step easing
let flash = 0; // decays each frame; kicked to 1 on every step
let bassKick = 0; // decays; drives the central bloom, kicked on bass hits
let clockPulse = 0; // 0..1 saw driven purely by the step CLOCK (color engine)

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
  sound.bpm(BPM); // beat() fires per 16th step
  sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.6 });
  if (!seeded) seedStars();
}

// beat() fires once per 16th step. Schedule the groove — locks to the grid.
function beat({ sound: { bpm, synth, time } }) {
  bpm(BPM);

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

  // Hi-hat texture on off-steps. Noise is selected by `type` (a numeric `tone`
  // is required — passing tone:"noise-white" throws "Note not found").
  if (step % 2 === 1) {
    synth({
      type: "noise-white",
      tone: 800,
      beats: 0.4,
      attack: 0.001,
      decay: 0.2,
      volume: step % 4 === 3 ? 0.28 : 0.16,
    });
  }

  // Arpeggio: one note per step, panned across the field.
  const an = ARP[step % ARP.length];
  synth({
    tone: an,
    type: "triangle",
    beats: 1.4,
    attack: 0.005,
    decay: 0.55,
    volume: 0.5,
    pan: -0.6 + (step % ARP.length) / (ARP.length - 1) * 1.2,
  });

  flash = 1; // visible pulse on every step
  // Clock-driven color phase — advances 1/16 each step, wraps at the bar.
  clockPulse = (step % 16) / 16;
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

// hue 0..1, sat/light 0..1 → [r,g,b] 0..255. Wraps num.hslToRgb, which wants
// hue in DEGREES and sat/light in 0..100 and already returns 0..255 (do NOT
// re-multiply by 255 — that was the old white-out bug).
function hue2rgb(num, hue, sat, light) {
  return num.hslToRgb(((hue % 1) + 1) % 1 * 360, sat * 100, light * 100);
}

function paint({ ink, circle, line, plot, screen, sound, num, paintCount }) {
  const w = screen.width, h = screen.height;
  const cx = w / 2, cy = h / 2;
  const foc = Math.min(w, h) * 0.9;

  // Deep-space veil (trail fade instead of hard wipe → glowing streaks).
  ink(4, 2, 14, 40).box(0, 0, w, h);

  // --- Live audio reads (optional garnish; visuals never depend on them) ---
  const bands = sound.speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const air = bands.find((b) => b.name === "air")?.amplitude || 0;
  const amp = sound.speaker?.amplitudes?.left || 0;

  // --- CLOCK-driven color engine (fires regardless of live audio) ----------
  // A hue that sweeps through the bar so every frame is vivid, plus a beat
  // clock ("step energy") that pulses from flash/bassKick — driven by beat().
  // paintCount is a plain Number here (NOT BigInt) — use Number math only.
  const pc = Number(paintCount);
  const clockHue = ((pc % 600) / 600 + clockPulse * 0.5) % 1; // slow global drift
  const beatEnergy = Math.min(1, flash * 0.8 + bassKick * 0.6 + amp * 0.5);

  // Central bloom on every downbeat — driven by bassKick (clock), not audio.
  const bloom = Math.max(bassKick, flash * 0.5);
  if (bloom > 0.02) {
    const R = Math.min(w, h) * (0.14 + bass * 0.5) * bloom;
    const [br, bg, bb] = hue2rgb(num, clockHue + 0.55, 0.9, 0.55);
    for (let i = 5; i > 0; i--) {
      ink(br, bg, bb, 32 * bloom).circle(cx, cy, R * (i / 5), true);
    }
    ink(255, 220, 255, 190 * bloom).circle(cx, cy, 7 + bass * 30, true);
  }

  // Starfield: project 3D stars, streak them radially, spawn colored sparks.
  const streak = 10 + flash * 26 + amp * 40; // streak length
  for (let i = 0; i < stars.length; i++) {
    const s = stars[i];
    const px = cx + (s.x / s.z) * foc * 0.5;
    const py = cy + (s.y / s.z) * foc * 0.5;
    if (px < -20 || px > w + 20 || py < -20 || py > h + 20) continue;

    const near = 1 - s.z; // 0 far .. 1 near
    // Vivid saturated color from the star hue + clock drift — NOT from audio.
    // Full saturation and a bright base lightness so it always reads as color.
    const hue = (s.hue + clockHue) % 1;
    const [r, g, b] = hue2rgb(
      num,
      hue,
      1.0,
      Math.min(0.85, 0.55 + near * 0.3 + beatEnergy * 0.1),
    );
    const bright = 130 + near * 125;

    // Radial streak from center → gives warp motion.
    const dx = px - cx, dy = py - cy;
    const len = Math.hypot(dx, dy) || 1;
    const ux = dx / len, uy = dy / len;
    const tail = streak * near;
    ink(r, g, b, bright).line(px, py, px - ux * tail, py - uy * tail);
    // A colored (not white) core keeps the whole field chromatic.
    ink(
      Math.min(255, r + 80),
      Math.min(255, g + 80),
      Math.min(255, b + 80),
      bright,
    ).plot(px | 0, py | 0);

    // On a strong step-flash, near stars throw a colored spark.
    if (flash > 0.6 && near > 0.7 && Math.random() < 0.3 && sparks.length < 240) {
      const sp = 1.5 + Math.random() * 3;
      const ang = Math.random() * Math.PI * 2;
      sparks.push({
        x: px, y: py,
        vx: Math.cos(ang) * sp + ux * 2,
        vy: Math.sin(ang) * sp + uy * 2,
        life: 1,
        hue,
      });
    }
  }

  // Draw sparks — bright ember bursts, warm-to-hue colored.
  for (let i = 0; i < sparks.length; i++) {
    const p = sparks[i];
    const [r, g, b] = hue2rgb(num, p.hue + 0.04, 1.0, 0.62);
    ink(r, g, b, 245 * p.life).circle(p.x, p.y, 1.2 + p.life * 3, true);
  }

  // Beat ring — a shockwave that expands within each step (seamless loop).
  if (flash > 0.05) {
    const rr = Math.min(w, h) * 0.5 * beatProgress;
    const [rr2, rg2, rb2] = hue2rgb(num, clockHue + 0.7, 0.95, 0.6);
    ink(rr2, rg2, rb2, 140 * (1 - beatProgress) * flash)
      .circle(cx, cy, rr, false, 2 + air * 6);
  }

  // Air-band shimmer accent ring (extra garnish when audio is live).
  if (air > 0.05) {
    ink(200, 255, 255, air * 120).circle(cx, cy, Math.min(w, h) * 0.46, false, 1);
  }
}

export { boot, beat, sim, paint };
