// frizmo, 26.07.12
// FIZZY-STATIC SPARKLE instrument — champagne fizz / effervescence.
//
// FOUR PRINCIPLES (see marketing/av-reels/button-sync-allegory.md):
//   1. NO resolution() — native res. The noise fizz is written DIRECTLY to
//      screen.pixels (no paste-of-buffer, which can render black headless).
//      Crisp vector sparks/bubbles are drawn on top.
//   2. THE WHOLE PIECE IS A BUTTON — tap / XY-drag erupts a FOUNTAIN of sparks
//      + a fizzy burst at the tap point (X→pan/hue, Y→pitch). Repeated taps
//      accumulate a decaying `pump` that makes the whole field fizz harder
//      (denser + brighter grain, more sparks).
//   3. UTC-SYNCED RHYTHM — blip + kick onsets come from clock.time() inside
//      sim() (NOT beat()). We compute the absolute global 16th-step from
//      epoch-ms and index the pattern by floor(step)%len, so two instances
//      opened anywhere lock to the same grid and each other.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY (timbre→texture is the key):
//        • NOISE FIZZ = a live GRAIN SPRAY of shimmering pixels. Grain density
//          & brightness ∝ the fizz amplitude — you literally SEE the noise as
//          static/grain.
//        • BLIP (pitched click) = a bright SPARK that pops and RISES like a
//          bubble; height & hue ∝ pitch (high blip = high, cyan/white).
//        • KICK = a BURST of bubbles ERUPTING from the bottom edge.
//      So: rough noise reads as grainy static, pitched blips read as rising
//      sparks, kick reads as an eruption from below.

// --- UTC-synced groove grid ----------------------------------------------
// 16th-note grid at a lively BPM. Absolute-from-epoch → every instance
// computes the identical step at the identical wall-clock ms.
const QUARTER_BPM = 132; // lively, sparkly feel
const STEP_MS = 60000 / QUARTER_BPM / 4; // ms per 16th-note step (~113ms)

// KICK on the 4 downbeats (steps 0,4,8,12) → bubble eruption from the bottom.
// High BLIPS arpeggiate every step → rising sparks. Bright, effervescent scale.
const BLIP = [
  "c6", "g5", "e6", "a5",
  "d6", "b5", "g6", "e6",
  "c6", "a5", "d6", "g5",
  "e6", "b5", "a6", "d6",
];
// Pitch range spanned by BLIP, used to map note → spark launch height / hue.
const BLIP_LOW = 783.99; // g5
const BLIP_HIGH = 1760.0; // a6

// Precomputed BLIP note → Hz (equal-temperament) — no sound.freq in hot path.
const BLIP_FREQ = {
  g5: 783.99, a5: 880.0, b5: 987.77,
  c6: 1046.5, d6: 1174.66, e6: 1318.51, g6: 1567.98, a6: 1760.0,
};

let lastStep = -1; // last integer 16th-step fired (UTC scheduler)
let stepProgress = 0; // 0..1 through the current 16th step

let fizz = 0.35; // baseline fizz amplitude → grain density (0..~1.4)
let kick = 0; // decays; drives bottom bubble eruption + low pulse
let flash = 0; // decays; whole-field per-step sparkle

// --- Tap / button energy -------------------------------------------------
let pump = 0; // decaying global energy from taps/drags, 0..~3
let bursts = []; // fizzy burst rings spawned by taps {x,y,r,vel,life,hue,r0,g0,b0}

// --- Particles -----------------------------------------------------------
let sparks = []; // rising bubbles/sparks (blip onsets + tap fountains)
let seed = 1234567; // frame-advanced hash seed (Math.random may be flaky headless)

// Cheap deterministic hash PRNG — self-run identically, no Math.random reliance.
function rnd() {
  // xorshift32
  seed ^= seed << 13; seed |= 0;
  seed ^= seed >> 17;
  seed ^= seed << 5; seed |= 0;
  return ((seed >>> 0) % 100000) / 100000; // 0..1
}

// --- Lifecycle -----------------------------------------------------------

let bed = null; // sustained airy fizz bed (noise floor so the reel always sings)

function boot({ sound, clock }) {
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline — fine)
  sound.room?.set?.({ enabled: true, mix: 0.12, feedback: 0.35 });
  // A quiet sustained noise bed = the ever-present champagne hiss. Its volume
  // breathes with fizz + tap energy in sim() so the grain always has a voice.
  bed = sound.synth?.({ type: "noise-white", tone: 2400, duration: "🔁",
    attack: 0.5, decay: 0.99, volume: 0.16 });
}

// Map a frequency (Hz) → normalized 0..1 across the BLIP pitch range.
function pitchNorm(freq) {
  const t = (Math.log2(freq) - Math.log2(BLIP_LOW)) /
    (Math.log2(BLIP_HIGH) - Math.log2(BLIP_LOW));
  return Math.max(0, Math.min(1, t));
}

// Fire everything for a single 16th step. Called from the UTC scheduler in sim
// so onsets align across instances. `synth` is sound.synth.
function fireStep(step, synth, screen) {
  const bar = ((step % 16) + 16) % 16; // 0..15
  const STEP_S = STEP_MS / 1000; // ~0.113s

  // KICK on the 4 downbeats → punchy low thump + a burst of bubbles erupting
  // from the bottom edge. This is the effervescence "pop" from below.
  if (bar % 4 === 0) {
    synth({ tone: "c2", type: "sine", duration: STEP_S * 3, attack: 0.004,
      decay: 0.35, volume: 0.9 });
    synth({ tone: "c1", type: "sawtooth", duration: STEP_S * 2, attack: 0.003,
      decay: 0.3, volume: 0.55 }); // body
    // A short fizz "chsss" rides the kick — the cork pop.
    synth({ type: "noise-white", tone: 1200, duration: STEP_S * 1.5,
      attack: 0.002, decay: 0.4, volume: 0.5 });
    kick = 1;
    fizz = Math.min(1.4, fizz + 0.55); // the kick sprays the grain harder
    // Eruption of bubbles from random spots along the bottom edge.
    const w = screen?.width || 360, h = screen?.height || 640;
    const n = 26;
    for (let i = 0; i < n && sparks.length < 380; i++) {
      const bx = rnd() * w;
      sparks.push({
        x: bx, y: h + 2,
        vx: (rnd() - 0.5) * 2.2, vy: -(3.5 + rnd() * 6.5), // shoot upward
        grav: -0.02 - rnd() * 0.03, // gentle rise / buoyancy
        life: 1, decay: 0.012 + rnd() * 0.01,
        hue: 0.5 + rnd() * 0.12, // cyan-ish champagne
        big: rnd() < 0.35, kind: "bubble",
      });
    }
  }

  // BLIP on every step → a bright pitched click + a rising SPARK bubble whose
  // height & hue ∝ pitch (high blip = high on screen, bright cyan/white).
  const bn = BLIP[bar % BLIP.length];
  const pan = -0.7 + (bar % BLIP.length) / (BLIP.length - 1) * 1.4;
  const accent = bar % 4 === 0; // downbeat blips a touch louder
  synth({ tone: bn, type: "square", duration: STEP_S * 0.35, attack: 0.001,
    decay: 0.25, volume: accent ? 0.42 : 0.3, pan });
  // A brighter sine ping doubles the click for sparkle.
  synth({ tone: bn, type: "sine", duration: STEP_S * 0.5, attack: 0.001,
    decay: 0.3, volume: 0.24, pan });
  // Tiny high fizz tick between blips → shimmer.
  if (bar % 2 === 1) {
    synth({ type: "noise-white", tone: 5000, duration: STEP_S * 0.25,
      attack: 0.001, decay: 0.15, volume: 0.18 });
    fizz = Math.min(1.4, fizz + 0.12);
  }

  const freq = typeof bn === "number" ? bn : BLIP_FREQ[bn];
  const pn = pitchNorm(freq); // 0 low .. 1 high
  const w = screen?.width || 360, h = screen?.height || 640;
  // Rising spark: born low, flies up toward a height ∝ pitch. Pitch → hue too.
  sparks.push({
    x: w * (0.15 + (bar % BLIP.length) / BLIP.length * 0.7) + (rnd() - 0.5) * 20,
    y: h * (0.86 - pn * 0.1), // starts a bit up for high notes
    vx: (rnd() - 0.5) * 1.2,
    vy: -(2.5 + pn * 4.5), // higher pitch rises faster/farther
    grav: -0.015 - pn * 0.02,
    life: 1, decay: 0.016 + (1 - pn) * 0.01,
    hue: 0.5 + pn * 0.13, // pitch → hue (cyan→bright)
    big: accent, kind: "spark", pn,
  });

  flash = 1; // whole-field sparkle pulse on every step
}

function sim({ sound: { speaker, synth }, clock, screen }) {
  speaker?.poll(); // mandatory for audio-reactive reads

  // --- UTC-synced scheduler: derive the beat grid from the shared clock ---
  // ⚠️ clock.time() returns an *Invalid Date* (getTime()→NaN, not null) before
  // the UTC offset resolves and offline. `?? new Date()` does NOT catch it —
  // guard with isFinite or the step goes NaN → onset spam every tick.
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now(); // local fallback; prod uses UTC
  const globalStep = ms / STEP_MS;
  const idx = Math.floor(globalStep);
  stepProgress = globalStep - idx; // 0..1 within this 16th step
  if (idx !== lastStep) {
    lastStep = idx;
    fireStep(idx, synth, screen);
  }

  // Advance sparks/bubbles: rise, buoyancy, gentle horizontal drift, fizz jitter.
  const jit = 0.4 + pump * 0.3;
  for (let i = 0; i < sparks.length; i++) {
    const p = sparks[i];
    p.x += p.vx + (rnd() - 0.5) * jit; // fizzy wobble
    p.y += p.vy;
    p.vy += p.grav; // buoyancy pulls up (negative grav)
    p.vx *= 0.985;
    if (p.vy < -9) p.vy = -9;
    p.life -= p.decay * (1 + pump * 0.15);
  }
  sparks = sparks.filter((p) => p.life > 0 && p.y > -20);

  // Advance tap burst rings.
  for (let i = 0; i < bursts.length; i++) {
    const b = bursts[i];
    b.r += b.vel * (1 + pump * 0.15);
    b.vel *= 0.985;
    b.life -= 0.02;
  }
  bursts = bursts.filter((b) => b.life > 0);

  // Decay pulses & tap energy; fizz relaxes toward a pump-boosted baseline.
  flash *= 0.85;
  kick *= 0.9;
  pump *= 0.965; // slow decay so repeated taps accumulate
  const fizzBase = 0.3 + pump * 0.35;
  fizz += (fizzBase - fizz) * 0.12; // ease back toward baseline
  if (fizz < 0.05) fizz = 0.05;

  // Bed breathes: the champagne hiss swells with fizz + tap energy.
  bed?.update?.({ volume: 0.12 + fizz * 0.14 + pump * 0.05 });
}

// --- Button: the whole screen erupts a fountain on tap / XY-drag ----------
function act({ event: e, sound: { synth }, screen, num }) {
  if (!(e.is("touch") || e.is("draw"))) return;

  const nx = e.x / screen.width; // 0..1
  const ny = e.y / screen.height; // 0..1
  const drag = e.is("draw");

  pump = Math.min(3, pump + (drag ? 0.14 : 0.95)); // taps punch, drags feed
  fizz = Math.min(1.4, fizz + (drag ? 0.15 : 0.6));

  const hue = 0.5 + nx * 0.2; // X → hue (champagne cyan→gold-ish)
  const [br, bg, bb] = hue2rgb(num, hue, 0.9, 0.62);

  // Fizzy burst ring at the tap point.
  bursts.push({ x: e.x, y: e.y, r: 0, vel: drag ? 8 : 15, life: 1, hue,
    r0: br, g0: bg, b0: bb });

  // Spark FOUNTAIN erupting from the tap point (upward-biased, denser on a tap).
  const n = drag ? 8 : 26;
  for (let i = 0; i < n && sparks.length < 400; i++) {
    const ang = -Math.PI / 2 + (rnd() - 0.5) * Math.PI * 1.1; // upward fan
    const sp = 2 + rnd() * (drag ? 4 : 9);
    sparks.push({
      x: e.x, y: e.y,
      vx: Math.cos(ang) * sp, vy: Math.sin(ang) * sp,
      grav: -0.02 - rnd() * 0.03,
      life: 1, decay: 0.02 + rnd() * 0.02,
      hue: hue + (rnd() - 0.5) * 0.1, big: rnd() < 0.3, kind: "spark",
    });
  }

  // SONIC BURST — X→pan/hue (via note choice), Y→pitch (top = high).
  const scale = ["c", "d", "e", "g", "a"];
  const oct = 4 + Math.floor((1 - ny) * 3); // 4..6, higher up = higher
  const note = scale[Math.min(4, Math.floor(nx * 5))] + oct;
  synth({ tone: note, type: "square", duration: drag ? 0.18 : 0.4,
    attack: 0.002, decay: 0.4, volume: drag ? 0.35 : 0.6, pan: nx * 2 - 1 });
  // Bright fizzy chsss burst — brighter higher up the screen.
  synth({ type: "noise-white", tone: 2000 + (1 - ny) * 4000,
    duration: drag ? 0.15 : 0.35, attack: 0.001, decay: 0.35,
    volume: (drag ? 0.22 : 0.4) * (0.6 + (1 - ny) * 0.6), pan: nx * 2 - 1 });
  // Sub thump so a full tap has body.
  if (!drag) {
    synth({ tone: "c2", type: "sine", duration: 0.4, attack: 0.003,
      decay: 0.3, volume: 0.5 });
  }
}

// hue 0..1, sat/light 0..1 → [r,g,b] 0..255. Wraps num.hslToRgb (wants hue in
// DEGREES + sat/light 0..100, and already returns 0..255 — do NOT ×255).
function hue2rgb(num, hue, sat, light) {
  return num.hslToRgb(((hue % 1) + 1) % 1 * 360, sat * 100, light * 100);
}

function paint({ ink, circle, line, plot, screen, sound, num, paintCount }) {
  const w = screen.width, h = screen.height;
  const pixels = screen.pixels;
  const pc = Number(paintCount);

  // --- Live audio garnish (visuals never DEPEND on it) ---
  const bands = sound.speaker?.frequencies?.left || [];
  const air = bands.find((b) => b.name === "air")?.amplitude || 0;
  const treble = bands.find((b) => b.name === "treble")?.amplitude || 0;
  const amp = sound.speaker?.amplitudes?.left || 0;

  // === Dark fizzy ground: a deep-navy vertical wash, brightening toward the
  // bottom (where bubbles are born) — written directly to screen.pixels. ===
  // The GRAIN density/brightness ∝ fizz amplitude → you SEE the noise as sparse
  // BRIGHT speckles popping on a dark ground (discrete grains, not a wash).
  // Kick brightens the lower band (eruption zone).
  const density = Math.min(0.34, 0.06 + fizz * 0.16 + amp * 0.1 + pump * 0.04);
  const speckBright = 150 + fizz * 90 + treble * 60; // grain brightness
  const kb = kick; // 0..1 bottom eruption glow
  // Advance the hash seed per frame so speckle animates (NOT Math.random).
  seed = (seed ^ (pc * 2654435761)) | 0;
  seed ^= seed << 13; seed |= 0;

  for (let y = 0; y < h; y++) {
    const vy = y / h; // 0 top .. 1 bottom
    // Base gradient: near-black top → deep teal-navy bottom, with an eruption
    // brightening at the very bottom on the kick. Kept DARK so grains pop.
    const bgR = 2 + vy * 3 + kb * (vy > 0.7 ? (vy - 0.7) * 55 : 0);
    const bgG = 4 + vy * 9 + kb * (vy > 0.7 ? (vy - 0.7) * 85 : 0);
    const bgB = 9 + vy * 20 + kb * (vy > 0.7 ? (vy - 0.7) * 115 : 0);
    let row = y * w;
    for (let x = 0; x < w; x++) {
      const i = (row + x) * 4;
      // Cheap per-pixel hash → 0..1 speckle, animated by frame via seed.
      let hshr = (x * 374761393 + y * 668265263 + seed) | 0;
      hshr = (hshr ^ (hshr >> 13)) * 1274126177;
      const rr = ((hshr ^ (hshr >> 16)) >>> 0) / 4294967295; // 0..1
      if (rr < density) {
        // A grain of champagne static — bright cyan/white speckle. Brightness
        // varies per grain so the fizz shimmers rather than reading as a mask.
        const b = speckBright * (0.35 + rr / density * 0.65);
        pixels[i] = Math.min(255, bgR + b * 0.65);
        pixels[i + 1] = Math.min(255, bgG + b * 0.92);
        pixels[i + 2] = Math.min(255, bgB + b);
      } else {
        pixels[i] = bgR;
        pixels[i + 1] = bgG;
        pixels[i + 2] = bgB;
      }
      pixels[i + 3] = 255;
    }
  }

  // === BLIP/BUBBLE sparks = rising bubbles; height & hue read the pitch ===
  for (let i = 0; i < sparks.length; i++) {
    const p = sparks[i];
    const light = 0.55 + (p.big ? 0.25 : 0.1) + flash * 0.1;
    const [r, g, b] = hue2rgb(num, p.hue, 0.85, Math.min(0.9, light));
    const rad = (p.big ? 2.2 : 1.3) + p.life * (p.big ? 2.5 : 1.6);
    // Soft glow then bright core → reads as a shimmering bubble.
    ink(r, g, b, 90 * p.life).circle(p.x, p.y, rad + 1.5, true);
    ink(Math.min(255, r + 70), Math.min(255, g + 70), Math.min(255, b + 70),
      240 * p.life).circle(p.x, p.y, rad, true);
    // A tiny white highlight = the bubble's shine.
    ink(255, 255, 255, 200 * p.life).plot((p.x - rad * 0.4) | 0,
      (p.y - rad * 0.4) | 0);
  }

  // === KICK = eruption glow band along the bottom edge ===
  if (kick > 0.02) {
    for (let k = 0; k < 5; k++) {
      const [r, g, b] = hue2rgb(num, 0.52, 0.9, 0.55);
      ink(r, g, b, 40 * kick * (1 - k / 5)).box(0, h - (k + 1) * 8 * kick,
        w, 8 * kick);
    }
    ink(220, 255, 255, 120 * kick).box(0, h - 3, w, 3);
  }

  // === Tap FIZZY BURST rings — the whole-piece "button" made visible ===
  for (let i = 0; i < bursts.length; i++) {
    const b = bursts[i];
    ink(b.r0, b.g0, b.b0, 190 * b.life).circle(b.x, b.y, b.r, false,
      2 + b.life * 5);
    const [ir, ig, ib] = hue2rgb(num, b.hue + 0.08, 0.9, 0.72);
    ink(ir, ig, ib, 140 * b.life * b.life).circle(b.x, b.y, b.r * 0.4, true);
    ink(255, 255, 255, 220 * b.life * b.life).circle(b.x, b.y, 3 + b.life * 7,
      true);
  }

  // === Per-step sparkle pulse — a faint bright veil flash on each blip ===
  if (flash > 0.15 && treble > 0.02) {
    ink(180, 255, 255, 18 * flash * (1 - stepProgress)).box(0, 0, w, h);
  }
}

function leave() {
  bed?.kill?.(0.5); // fade the champagne hiss out on exit
  bed = null;
}

// Note: there is intentionally NO `beat` export. All onsets come from the UTC
// scheduler in sim() (indexed by floor(globalStep)) so two instances opened
// anywhere auto-align.
export { boot, sim, paint, act, leave };
