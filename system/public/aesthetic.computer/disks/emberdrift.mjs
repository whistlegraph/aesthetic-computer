// emberdrift, 26.07.12
// Self-running cosmic A/V loop — now a UTC-synced, whole-screen instrument.
//
// FOUR PRINCIPLES (see marketing/av-reels/button-sync-allegory.md):
//   1. NO resolution() — renders at native res, fills any screen.
//   2. THE WHOLE PIECE IS A BUTTON — tap or XY-drag ignites a SUPERNOVA at the
//      touch point: an expanding shockwave ring + a spark shower + a boom
//      (X→pan/hue, Y→pitch). Repeated taps accumulate a decaying `pump` that
//      intensifies the whole starfield (faster warp, bigger blooms, brighter).
//   3. UTC-SYNCED RHYTHM — the bass/kick + arp + hats are driven from
//      clock.time() inside sim() (NOT beat()). We compute the absolute global
//      beat from epoch-ms and index the pattern by floor(globalBeat)%len, so two
//      instances opened anywhere lock to the identical grid and each other.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY — the score is legible on screen:
//        • BASS/KICK  = a big shockwave RING launched from center on the beat.
//        • ARP note   = a STAR/STREAK launched the instant it sounds, at a
//                       height (and angle) proportional to its pitch.
//        • HAT        = a tiny sparkle FLECK.
//        • THE BEAT   = the pulse of the whole starfield (it breathes on grid).

// --- UTC-synced groove grid ----------------------------------------------
// 16th-note grid. Pick a musical BPM (quarter-notes); the 16th step advances
// at 4× that. Absolute-from-epoch → every instance computes the same step at
// the same wall-clock ms.
const QUARTER_BPM = 120; // musical feel
const STEP_MS = 60000 / QUARTER_BPM / 4; // ms per 16th-note step

// Bass pulse: kick-like low note on the 4 downbeats (steps 0,4,8,12).
const BASS = ["c1", "c1", "g1", "c1"];
// Arpeggio: one note per 16th step. Minor-key, spacey. Pitch → launch height.
const ARP = [
  "c4", "g4", "a4", "e5",
  "c5", "g4", "d5", "b4",
  "c4", "a4", "c5", "g5",
  "e5", "d5", "b4", "g4",
];
// Pitch range spanned by ARP, used to map note → vertical launch height.
const ARP_LOW = 261.63; // c4
const ARP_HIGH = 783.99; // g5

let lastStep = -1; // last integer 16th-step we fired (UTC scheduler)
let stepProgress = 0; // 0..1 through the current 16th step (smooth easing)

let flash = 0; // decays each frame; kicked to 1 on every step
let bassKick = 0; // decays; drives the central kick shockwave, kicked on bass
let clockPulse = 0; // 0..1 saw driven by the step CLOCK (color engine)

// --- Tap / button energy -------------------------------------------------
let pump = 0; // decaying global energy from taps/drags, 0..~3
let bursts = []; // supernova shockwaves spawned by taps {x,y,r,life,hue,vel}

// --- Particles -----------------------------------------------------------
const STAR_COUNT = 240; // 3D drifting stars
const stars = [];
let sparks = []; // transient embers (beat sparks + tap spark showers)
let rings = []; // audible-onset rings: kick shockwaves + arp launch markers
let flecks = []; // tiny hat sparkle flecks

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

let pad = null; // sustained cosmic drone (audio floor so the reel always sings)
let pad2 = null; // a fifth above → a fuller chord bed

function boot({ sound, clock }) {
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline — fine)
  sound.room?.set?.({ enabled: true, mix: 0.1, feedback: 0.4 });
  if (!seeded) seedStars();
  // Two sustained drones under the groove — guarantee a present audio bed and
  // give the bass its body regardless of where the UTC phase lands in a capture
  // window. Volume breathes with the kick + tap energy in sim().
  pad = sound.synth?.({ tone: "c2", type: "sawtooth", duration: "🔁",
    attack: 0.4, decay: 0.99, volume: 0.85, pan: -0.15 });
  pad2 = sound.synth?.({ tone: "g2", type: "triangle", duration: "🔁",
    attack: 0.6, decay: 0.99, volume: 0.6, pan: 0.15 });
}

// Map a frequency (Hz) → normalized 0..1 across the ARP pitch range.
function pitchNorm(freq) {
  const t = (Math.log2(freq) - Math.log2(ARP_LOW)) /
    (Math.log2(ARP_HIGH) - Math.log2(ARP_LOW));
  return Math.max(0, Math.min(1, t));
}

// Fire everything for a single 16th step. Called from the UTC scheduler in sim
// so onsets align across instances. `synth` is sound.synth.
function fireStep(step, synth) {
  const bar = ((step % 16) + 16) % 16; // 0..15

  // Durations are in SECONDS (not `beats`), sized to the 125ms step grid, so
  // notes don't massively overlap and choke the bus limiter (which would sap
  // the mean level). No sound.bpm() is set — the UTC scheduler is authoritative.
  const STEP_S = STEP_MS / 1000; // 0.125s

  // BASS / KICK on the 4 downbeats → a big shockwave RING from center.
  if (bar % 4 === 0) {
    const bn = BASS[(bar / 4) % BASS.length];
    synth({ tone: bn, type: "sawtooth", duration: STEP_S * 3, attack: 0.005,
      decay: 0.4, volume: 0.95, pan: 0 });
    synth({ tone: bn, type: "sine", duration: STEP_S * 2.5, attack: 0.002,
      decay: 0.3, volume: 0.8 }); // sub thump layer
    bassKick = 1;
    // The kick's visible counterpart: a low-hue shockwave launched from center.
    rings.push({ kind: "kick", r: 0, vel: 9, life: 1, hue: 0.02 });
  }

  // HAT on off-steps → a tiny sparkle FLECK.
  if (bar % 2 === 1) {
    synth({ type: "noise-white", tone: 800, duration: STEP_S * 0.5, attack: 0.001,
      decay: 0.2, volume: bar % 4 === 3 ? 0.4 : 0.24 });
    // Fleck spawns in the upper field, jittered — a bright grain of noise.
    flecks.push({ x: Math.random(), y: Math.random() * 0.5, life: 1,
      accent: bar % 4 === 3 });
  }

  // ARP note per step → a STAR launched at height ∝ pitch the instant it sounds.
  const an = ARP[bar % ARP.length];
  const arpPan = -0.6 + (bar % ARP.length) / (ARP.length - 1) * 1.2;
  synth({ tone: an, type: "triangle", duration: STEP_S * 1.6, attack: 0.005,
    decay: 0.55, volume: 0.85, pan: arpPan });
  // Sawtooth harmonic thickens the arp (more presence).
  synth({ tone: an, type: "sawtooth", duration: STEP_S * 1.2, attack: 0.006,
    decay: 0.5, volume: 0.45, pan: arpPan });

  // Resolve the note frequency for the pitch→height mapping. sound.freq is the
  // note→Hz parser; guard in case a raw number came through.
  const freq = typeof an === "number" ? an : ARP_FREQ[an];
  const pn = pitchNorm(freq); // 0 low .. 1 high
  // A rising launch marker: born at center, flies UP (higher pitch = higher &
  // brighter), a small ring so you can read the pitch as vertical position.
  rings.push({
    kind: "arp", r: 0, vel: 4 + pn * 5, life: 1,
    hue: 0.55 + pn * 0.35, // pitch → hue too (consistent mapping)
    launchY: pn, // 0 low .. 1 high — used to place the star's rise target
    pan: arpPan,
  });

  flash = 1; // whole-field pulse on every step
  clockPulse = bar / 16;
}

// Precomputed ARP note → Hz (equal-temperament) so we don't need sound.freq in
// the hot path. Only the notes we actually use.
const ARP_FREQ = {
  c4: 261.63, d4: 293.66, e4: 329.63, g4: 392.0, a4: 440.0, b4: 493.88,
  c5: 523.25, d5: 587.33, e5: 659.25, g5: 783.99, a5: 880.0,
};

function sim({ sound: { speaker, synth }, clock }) {
  speaker?.poll(); // mandatory for audio-reactive reads

  // --- UTC-synced scheduler: derive the beat grid from the shared clock ---
  const ms = (clock?.time?.() ?? new Date()).getTime();
  const globalStep = ms / STEP_MS;
  const idx = Math.floor(globalStep);
  stepProgress = globalStep - idx; // 0..1 within this 16th step
  if (idx !== lastStep) {
    // Guard against a huge jump (first frame) firing a burst of steps.
    lastStep = idx;
    fireStep(idx, synth);
  }

  const surge = 1 + bassKick * 0.01 + pump * 0.012; // kick/tap warp boost

  // Advance drifting stars toward the camera.
  for (let i = 0; i < stars.length; i++) {
    const s = stars[i];
    s.z -= (0.004 + bassKick * 0.01) * surge;
    if (s.z <= 0.001) {
      s.x = (Math.random() - 0.5) * 2;
      s.y = (Math.random() - 0.5) * 2;
      s.z = 1;
      s.hue = Math.random();
    }
  }

  // Advance onset rings (kick shockwaves + arp launch markers).
  for (let i = 0; i < rings.length; i++) {
    const rg = rings[i];
    rg.r += rg.vel * (1 + pump * 0.2);
    rg.life -= rg.kind === "kick" ? 0.02 : 0.03;
  }
  rings = rings.filter((rg) => rg.life > 0);

  // Advance & cull sparks.
  for (let i = 0; i < sparks.length; i++) {
    const p = sparks[i];
    p.x += p.vx;
    p.y += p.vy;
    p.vx *= 0.96;
    p.vy *= 0.96;
    p.life -= p.decay || 0.03;
  }
  sparks = sparks.filter((p) => p.life > 0);

  // Advance flecks.
  for (let i = 0; i < flecks.length; i++) flecks[i].life -= 0.05;
  flecks = flecks.filter((f) => f.life > 0);

  // Advance supernova bursts (tap shockwaves).
  for (let i = 0; i < bursts.length; i++) {
    const b = bursts[i];
    b.r += b.vel * (1 + pump * 0.15);
    b.vel *= 0.985;
    b.life -= 0.02;
  }
  bursts = bursts.filter((b) => b.life > 0);

  // Decay pulses & tap energy.
  flash *= 0.86;
  bassKick *= 0.9;
  pump *= 0.965; // slow decay so repeated taps accumulate

  // Pads breathe: swell on the kick and with tap energy → a living bed.
  pad?.update?.({ volume: 0.85 + bassKick * 0.25 + pump * 0.12 });
  pad2?.update?.({ volume: 0.6 + bassKick * 0.18 + pump * 0.1 });
}

// --- Button: the whole screen ignites a supernova on tap / XY-drag --------
function act({ event: e, sound: { synth }, screen, num }) {
  if (!(e.is("touch") || e.is("draw"))) return;

  const nx = e.x / screen.width; // 0..1
  const ny = e.y / screen.height; // 0..1
  const drag = e.is("draw");

  // Accumulate energy: taps punch, drags feed.
  pump = Math.min(3, pump + (drag ? 0.14 : 0.95));

  const hue = nx; // X → hue (0..1)
  const [br, bg, bb] = hue2rgb(num, hue, 1.0, 0.6);

  // Supernova shockwave ring at the tap point.
  bursts.push({ x: e.x, y: e.y, r: 0, vel: drag ? 9 : 16, life: 1, hue,
    r0: br, g0: bg, b0: bb });

  // Spark shower radiating from the tap point (denser on a full tap).
  const n = drag ? 8 : 22;
  for (let i = 0; i < n && sparks.length < 320; i++) {
    const ang = Math.random() * Math.PI * 2;
    const sp = 2 + Math.random() * (drag ? 4 : 8);
    sparks.push({
      x: e.x, y: e.y,
      vx: Math.cos(ang) * sp, vy: Math.sin(ang) * sp,
      life: 1, decay: 0.02 + Math.random() * 0.02, hue,
    });
  }

  // SONIC BOOM — X→pan/hue (via note choice), Y→pitch (top = high).
  const scale = ["c", "d", "e", "g", "a"];
  const oct = 2 + Math.floor((1 - ny) * 4); // 2..5, higher up = higher
  const note = scale[Math.min(4, Math.floor(nx * 5))] + oct;
  synth({ tone: note, type: "sawtooth", duration: drag ? 0.2 : 0.45,
    attack: 0.004, decay: 0.6, volume: drag ? 0.4 : 0.75, pan: nx * 2 - 1 });
  // Bright sparkle harmonic — brighter higher up the screen.
  synth({ tone: note, type: "triangle", duration: 0.25, attack: 0.002,
    decay: 0.4, volume: 0.35 * (1 - ny) + 0.15, pan: nx * 2 - 1 });
  // Sub thump so the boom has body.
  if (!drag) {
    synth({ tone: scale[Math.min(4, Math.floor(nx * 5))] + "1", type: "sine",
      duration: 0.5, attack: 0.003, decay: 0.35, volume: 0.6 });
  }
}

// hue 0..1, sat/light 0..1 → [r,g,b] 0..255. Wraps num.hslToRgb (wants hue in
// DEGREES + sat/light 0..100, and already returns 0..255 — do NOT ×255).
function hue2rgb(num, hue, sat, light) {
  return num.hslToRgb(((hue % 1) + 1) % 1 * 360, sat * 100, light * 100);
}

function paint({ ink, circle, line, plot, screen, sound, num, paintCount }) {
  const w = screen.width, h = screen.height;
  const cx = w / 2, cy = h / 2;
  const foc = Math.min(w, h) * 0.9;
  const minWH = Math.min(w, h);

  // Deep-space veil (trail fade → glowing streaks). Pump thins it slightly so
  // the field brightens/energizes when tapped.
  ink(4, 2, 14, Math.max(24, 40 - pump * 4)).box(0, 0, w, h);

  // --- Live audio garnish (visuals never DEPEND on it) ---
  const bands = sound.speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const air = bands.find((b) => b.name === "air")?.amplitude || 0;
  const amp = sound.speaker?.amplitudes?.left || 0;

  // --- Clock-driven color engine ---
  const pc = Number(paintCount);
  const clockHue = ((pc % 600) / 600 + clockPulse * 0.5) % 1;
  const beatEnergy = Math.min(1, flash * 0.8 + bassKick * 0.6 + amp * 0.5 + pump * 0.3);

  // === BASS/KICK = central bloom, pulsed by bassKick (the clock, not audio) ===
  const bloom = Math.max(bassKick, flash * 0.5) * (1 + pump * 0.4);
  if (bloom > 0.02) {
    const R = minWH * (0.14 + bass * 0.5) * bloom;
    const [r0, g0, b0] = hue2rgb(num, clockHue + 0.55, 0.9, 0.55);
    for (let i = 5; i > 0; i--) {
      ink(r0, g0, b0, 32 * bloom).circle(cx, cy, R * (i / 5), true);
    }
    ink(255, 220, 255, 190 * bloom).circle(cx, cy, 7 + bass * 30, true);
  }

  // === Starfield: warp streaks; near stars throw sparks on a strong step ===
  const streak = 10 + flash * 26 + amp * 40 + pump * 30;
  for (let i = 0; i < stars.length; i++) {
    const s = stars[i];
    const px = cx + (s.x / s.z) * foc * 0.5;
    const py = cy + (s.y / s.z) * foc * 0.5;
    if (px < -20 || px > w + 20 || py < -20 || py > h + 20) continue;

    const near = 1 - s.z; // 0 far .. 1 near
    const hue = (s.hue + clockHue) % 1;
    const [r, g, b] = hue2rgb(
      num, hue, 1.0,
      Math.min(0.9, 0.55 + near * 0.3 + beatEnergy * 0.12),
    );
    const bright = Math.min(255, 130 + near * 125 + pump * 25);

    const dx = px - cx, dy = py - cy;
    const len = Math.hypot(dx, dy) || 1;
    const ux = dx / len, uy = dy / len;
    const tail = streak * near;
    ink(r, g, b, bright).line(px, py, px - ux * tail, py - uy * tail);
    ink(Math.min(255, r + 80), Math.min(255, g + 80), Math.min(255, b + 80),
      bright).plot(px | 0, py | 0);

    // Colored spark from near stars on a strong step-flash.
    if (flash > 0.6 && near > 0.7 && Math.random() < 0.3 && sparks.length < 320) {
      const sp = 1.5 + Math.random() * 3;
      const ang = Math.random() * Math.PI * 2;
      sparks.push({
        x: px, y: py,
        vx: Math.cos(ang) * sp + ux * 2, vy: Math.sin(ang) * sp + uy * 2,
        life: 1, hue,
      });
    }
  }

  // === ARP note = a RISING STAR launched at height ∝ pitch, the moment it sounds ===
  // Each arp ring travels UP toward its pitch height and leaves a bright head so
  // you can literally read the melody as rising sparks (high note = high & bright).
  for (let i = 0; i < rings.length; i++) {
    const rg = rings[i];
    if (rg.kind === "arp") {
      // Position: rise from center toward launchY (0 low, 1 high → top).
      const targetY = h * (0.9 - rg.launchY * 0.8); // higher pitch = higher up
      const yy = cy + (targetY - cy) * (1 - rg.life);
      const xx = cx + rg.pan * w * 0.32 * (1 - rg.life); // fan out by pan
      const [r, g, b] = hue2rgb(num, rg.hue + clockHue * 0.2, 1.0,
        0.55 + rg.launchY * 0.3);
      // Rising streak + bright head — the audible arp note made visible.
      ink(r, g, b, 220 * rg.life).line(xx, cy, xx, yy);
      ink(Math.min(255, r + 60), Math.min(255, g + 60), Math.min(255, b + 60),
        255 * rg.life).circle(xx, yy, 2 + rg.launchY * 4 + rg.life * 3, true);
    } else {
      // === BASS/KICK = big shockwave RING launched from center on the beat ===
      const [r, g, b] = hue2rgb(num, clockHue + 0.02, 0.95, 0.6);
      ink(r, g, b, 170 * rg.life)
        .circle(cx, cy, rg.r, false, 2 + rg.life * 5 + air * 6);
    }
  }

  // === HAT = tiny sparkle FLECK ===
  for (let i = 0; i < flecks.length; i++) {
    const f = flecks[i];
    const fx = f.x * w, fy = f.y * h;
    const c = f.accent ? 255 : 210;
    ink(c, c, 255, 255 * f.life).plot(fx | 0, fy | 0);
    if (f.accent) ink(255, 255, 255, 150 * f.life).circle(fx, fy, 1.5, true);
  }

  // === Sparks — ember bursts (beat sparks + tap spark showers) ===
  for (let i = 0; i < sparks.length; i++) {
    const p = sparks[i];
    const [r, g, b] = hue2rgb(num, (p.hue ?? 0) + 0.04, 1.0, 0.62);
    ink(r, g, b, 245 * p.life).circle(p.x, p.y, 1.2 + p.life * 3, true);
  }

  // === SUPERNOVA bursts — the whole-piece "button" made visible ===
  for (let i = 0; i < bursts.length; i++) {
    const b = bursts[i];
    // Expanding shockwave ring at the tap point.
    ink(b.r0, b.g0, b.b0, 200 * b.life)
      .circle(b.x, b.y, b.r, false, 2 + b.life * 6);
    // A brighter inner flash core that collapses as the ring flies out.
    const [ir, ig, ib] = hue2rgb(num, b.hue + 0.1, 1.0, 0.7);
    ink(ir, ig, ib, 150 * b.life * b.life)
      .circle(b.x, b.y, b.r * 0.4, true);
    ink(255, 255, 255, 220 * b.life * b.life).circle(b.x, b.y, 3 + b.life * 8, true);
  }

  // === The beat pulse of the whole field — a per-step shockwave ring ===
  if (flash > 0.05) {
    const rr = minWH * 0.5 * stepProgress;
    const [rr2, rg2, rb2] = hue2rgb(num, clockHue + 0.7, 0.95, 0.6);
    ink(rr2, rg2, rb2, 120 * (1 - stepProgress) * flash)
      .circle(cx, cy, rr, false, 2 + air * 6);
  }

  // Air-band shimmer accent ring (garnish when audio is live).
  if (air > 0.05) {
    ink(200, 255, 255, air * 120).circle(cx, cy, minWH * 0.46, false, 1);
  }
}

function leave() {
  pad?.kill?.(0.5); // fade the sustained drones out on exit
  pad2?.kill?.(0.5);
  pad = null;
  pad2 = null;
}

// Note: there is intentionally NO `beat` export. The metronome is not the audio
// source — all onsets come from the UTC scheduler in sim() (indexed by
// floor(globalStep)) so two instances opened anywhere auto-align.
export { boot, sim, paint, act, leave };
