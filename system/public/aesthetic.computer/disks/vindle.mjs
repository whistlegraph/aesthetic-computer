// vindle, 26.07.12
// A GROWING-VINE instrument — the melody literally DRAWS a plant.
//
// FOUR PRINCIPLES (see marketing/av-reels/button-sync-allegory.md):
//   1. NO resolution() — renders at native res, fills any screen.
//   2. THE WHOLE PIECE IS A BUTTON — tap or XY-drag sprouts a NEW SHOOT from
//      the touch point that grows + plucks a note (X→pan/hue, Y→pitch; top =
//      high). Repeated taps accumulate a decaying `pump` that makes the whole
//      plant grow FASTER + LUSHER (thicker stems, more buds).
//   3. UTC-SYNCED RHYTHM — the growth (one new vine segment per arp note) + the
//      trunk bass are scheduled from clock.time() inside sim() (NOT beat()). We
//      compute the absolute global step from epoch-ms and index the pattern by
//      floor(globalStep)%len, so two instances opened anywhere grow in lockstep.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY — the score IS the drawn plant:
//        • ARP note  = a new VINE SEGMENT. Its TURN ANGLE ∝ pitch (high note
//                      bends the vine UP, low note bends it DOWN) and its HUE =
//                      pitch. So the melodic contour is literally the shape +
//                      color of the growing vine — you read the melody as the plant.
//        • ACCENT    = a LEAF / FLOWER buds at the growing tip.
//        • BASS      = the trunk thickens + a root pulse blooms from the base.
//        • HAT       = a tiny leaf-flicker flecks off the vine.
//        • THE LOOP  = at the pattern seam the vine gracefully fades and regrows
//                      → seamless (the vine resets/regrows on every loop).

// --- UTC-synced growth grid ----------------------------------------------
// 8th-note grid. A musical BPM (quarter-notes); the 8th step advances at 2×.
// Absolute-from-epoch → every instance computes the same step at the same ms.
const QUARTER_BPM = 108; // patient, hypnotic
const STEP_MS = 60000 / QUARTER_BPM / 2; // ms per 8th-note step (~278ms)

// One woody plucked note per 8th step → one new vine segment. The vine REGROWS
// every PATTERN.length steps (the loop seam), so the whole phrase is one plant.
// D minor pentatonic-ish, rising then settling → the vine climbs then curls.
const ARP = [
  "d3", "a3", "d4", "f4",
  "a4", "g4", "e4", "c4",
  "d4", "f4", "a4", "d5",
  "c5", "a4", "f4", "d4",
];
const PATTERN_LEN = ARP.length; // 16 segments per grown vine

// Accents (buds sprout here): the phrase's peaks.
const ACCENT_STEPS = new Set([4, 11, 12]);

// Warm woody trunk bass — one note per bar (every 4 steps).
const BASS = ["d2", "d2", "f2", "a2"];

// Pitch range across ARP, used to map note → turn angle + hue.
const ARP_LOW = 146.83; // d3
const ARP_HIGH = 587.33; // d5

// Precomputed note → Hz (equal temperament) so we don't touch sound.freq in the
// hot path. Only the notes we actually use.
const NOTE_FREQ = {
  d2: 73.42, f2: 87.31, a2: 110.0,
  c4: 261.63, d3: 146.83, e4: 329.63, f4: 349.23, g4: 392.0, a3: 220.0,
  a4: 440.0, d4: 293.66, c5: 523.25, d5: 587.33,
};

let lastStep = -1; // last integer 8th-step fired (UTC scheduler)
let stepProgress = 0; // 0..1 through the current 8th step (smooth easing)
let seedStep = 0; // step index of the current vine's first segment

// --- Vine state ----------------------------------------------------------
// The vine is an array of segments appended on each UTC step. Each segment
// records its endpoints, thickness, hue and any bud — so older growth persists
// and the whole melody is legible as the drawn plant.
let vine = []; // [{ x0,y0,x1,y1, hue, thick, born, bud, budHue, pan }]
let tipX = 0, tipY = 0; // current growing tip (screen coords)
let tipAngle = -Math.PI / 2; // heading; -90° = straight up
let vineFade = 1; // 1 = solid, → 0 as the vine fades at the loop seam
let rootPulse = 0; // decays; blooms from the base on the bass/trunk beat
let trunkThick = 0; // decays; extra base-stem girth on the bass beat

// --- Shoots (tap-sprouted mini vines) ------------------------------------
let shoots = []; // [{ segs:[...], tipX,tipY,angle, hue,pan, grow, life }]

// --- Button energy -------------------------------------------------------
let pump = 0; // decaying global energy from taps/drags, 0..~3
let flecks = []; // tiny leaf-flicker flecks (hats + tap sparkle)

// --- Lifecycle -----------------------------------------------------------
let drone = null; // sustained woody floor so the reel always sings
let drone2 = null;

function boot({ sound, clock }) {
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline — fine)
  sound.room?.set?.({ enabled: true, mix: 0.18, feedback: 0.45 }); // woody room
  // Two sustained low drones = the warm woody bed + the trunk's body, so the
  // capture always has present bass wherever the UTC phase lands.
  drone = sound.synth?.({ tone: "d2", type: "sine", duration: "🔁",
    attack: 0.6, decay: 0.99, volume: 0.7, pan: -0.12 });
  drone2 = sound.synth?.({ tone: "a2", type: "triangle", duration: "🔁",
    attack: 0.8, decay: 0.99, volume: 0.45, pan: 0.12 });
}

// note (Hz) → 0..1 across the ARP pitch range (log/pitch space).
function pitchNorm(freq) {
  const t = (Math.log2(freq) - Math.log2(ARP_LOW)) /
    (Math.log2(ARP_HIGH) - Math.log2(ARP_LOW));
  return Math.max(0, Math.min(1, t));
}

// hue 0..1, sat/light 0..1 → [r,g,b] 0..255. Wraps num.hslToRgb (wants hue in
// DEGREES + sat/light 0..100, and already returns 0..255 — do NOT ×255).
function hue2rgb(num, hue, sat, light) {
  return num.hslToRgb(((hue % 1) + 1) % 1 * 360, sat * 100, light * 100);
}

// Reset the vine to a fresh sprout at the base — called at each loop seam.
function resprout(w, h) {
  vine = [];
  tipX = w / 2;
  tipY = h * 0.92; // grow up from near the bottom
  tipAngle = -Math.PI / 2; // straight up
  vineFade = 1;
}

// Fire one 8th step: append a vine segment (angle+hue ∝ pitch), bud on accents,
// trunk bass every bar, hat on off-steps. Called from the UTC scheduler in sim
// so growth + onsets align across instances. `w,h` = screen for geometry.
function fireStep(idx, synth, num, w, h) {
  const local = ((idx - seedStep) % PATTERN_LEN + PATTERN_LEN) % PATTERN_LEN;

  // At the seam (local wraps to 0) the previous vine has faded — resprout.
  if (local === 0) resprout(w, h);

  const note = ARP[local];
  const freq = typeof note === "number" ? note : (NOTE_FREQ[note] || 220);
  const pn = pitchNorm(freq); // 0 low .. 1 high
  const accent = ACCENT_STEPS.has(local);
  const STEP_S = STEP_MS / 1000;

  // --- SONIC: woody plucked note (triangle body + sine sub-pluck), quick decay.
  const pan = -0.5 + (local / (PATTERN_LEN - 1)) * 1.0;
  synth({ tone: note, type: "triangle", duration: STEP_S * 2.2, attack: 0.004,
    decay: 0.42, volume: 0.82, pan });
  synth({ tone: note, type: "sine", duration: STEP_S * 1.6, attack: 0.003,
    decay: 0.5, volume: 0.4, pan }); // rounder woody body
  if (accent) {
    // A brighter bloom pluck when a leaf/flower buds.
    synth({ tone: note, type: "triangle", duration: STEP_S * 2.6, attack: 0.002,
      decay: 0.4, volume: 0.5, pan });
  }

  // --- TRUNK BASS every bar (every 4 steps) → root pulse + trunk thickens.
  if (local % 4 === 0) {
    const bn = BASS[(local / 4) % BASS.length];
    synth({ tone: bn, type: "sine", duration: STEP_S * 3.4, attack: 0.006,
      decay: 0.35, volume: 0.85, pan: 0 });
    synth({ tone: bn, type: "triangle", duration: STEP_S * 2.4, attack: 0.008,
      decay: 0.3, volume: 0.5, pan: 0 });
    rootPulse = 1;
    trunkThick = 1;
  }

  // --- HAT on off-steps → soft leaf-rustle + a tiny flicker fleck.
  if (local % 2 === 1) {
    synth({ type: "noise-white", tone: 1200, duration: STEP_S * 0.4,
      attack: 0.001, decay: 0.18, volume: local % 4 === 3 ? 0.22 : 0.14 });
    flecks.push({ x: tipX, y: tipY, vx: (Math.random() - 0.5) * 1.6,
      vy: -Math.random() * 1.2, life: 1, hue: 0.28 });
  }

  // --- GRAPHIC: extend the vine by one segment. TURN ANGLE ∝ pitch: a high
  // note bends the vine UP (toward -90°), a low note bends it DOWN. Centered on
  // straight-up so the melodic contour becomes the vine's shape.
  const turn = (pn - 0.5) * (Math.PI * 0.9); // -0.45π (low)..+0.45π (high)
  // Bias the heading toward the pitch-implied angle, plus a gentle sway.
  const targetAngle = -Math.PI / 2 + -turn; // high pitch → more upward
  tipAngle = tipAngle * 0.55 + targetAngle * 0.45;
  tipAngle += Math.sin(idx * 0.7) * 0.12; // organic sway

  const segLen = Math.min(w, h) * (0.05 + pn * 0.03) * (1 + pump * 0.35);
  const nx = tipX + Math.cos(tipAngle) * segLen;
  const ny = tipY + Math.sin(tipAngle) * segLen;
  const hue = 0.22 + pn * 0.22; // green (low) → chartreuse/gold (high)
  const thick = (2 + pn * 3) * (1 + pump * 0.5);

  vine.push({
    x0: tipX, y0: tipY, x1: nx, y1: ny,
    hue, thick, born: idx, pan,
    bud: accent || pump > 1.2, // buds on accents (and lushly when pumped)
    budHue: accent ? 0.06 + pn * 0.08 : hue + 0.05, // gold/coral flower vs leaf
  });
  tipX = nx;
  tipY = ny;

  // If the tip wanders off-screen, nudge it back toward center so the plant
  // stays legible in-frame.
  if (tipX < w * 0.12) tipAngle += 0.25;
  if (tipX > w * 0.88) tipAngle -= 0.25;
  if (tipY < h * 0.12) tipAngle = -tipAngle * 0.3 + 0.4; // ceiling → curl down

  stepProgress = 0;
}

function sim({ sound: { speaker, synth }, clock, screen, num }) {
  speaker?.poll(); // mandatory for audio-reactive reads
  const w = screen.width, h = screen.height;

  // Lazily seed the first sprout once we know the screen size.
  if (vine.length === 0 && tipX === 0 && tipY === 0) resprout(w, h);

  // --- UTC-synced scheduler: derive the growth grid from the shared clock ---
  // clock.time() returns an *Invalid Date* (getTime() → NaN) before the offset
  // resolves and offline. `?? new Date()` does NOT catch it. Guard with isFinite
  // or the step index goes NaN → fireStep every tick (audio spam).
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now(); // local fallback; prod uses UTC
  const globalStep = ms / STEP_MS;
  const idx = Math.floor(globalStep);
  stepProgress = globalStep - idx; // 0..1 within this 8th step

  if (idx !== lastStep) {
    // On the very first real step, align the seam so the vine sprouts from step 0
    // of its pattern (a clean plant), rather than mid-phrase.
    if (lastStep === -1) seedStep = idx;
    lastStep = idx;
    fireStep(idx, synth, num, w, h);
  }

  // Fade the vine across the last part of its life so it gracefully dissolves
  // just before the seam → seamless regrow. local phase 0..1 across the pattern.
  const local = ((idx - seedStep) % PATTERN_LEN + PATTERN_LEN) % PATTERN_LEN;
  const phase = (local + stepProgress) / PATTERN_LEN; // 0..1 through this vine
  vineFade = phase > 0.86 ? Math.max(0, 1 - (phase - 0.86) / 0.14) : 1;

  // Advance flecks (leaf flickers).
  for (let i = 0; i < flecks.length; i++) {
    const f = flecks[i];
    f.x += f.vx;
    f.y += f.vy;
    f.vy += 0.08; // gentle gravity
    f.vx *= 0.97;
    f.life -= 0.035;
  }
  flecks = flecks.filter((f) => f.life > 0);

  // Advance tap-sprouted shoots — each grows its own little vine over time.
  for (let i = 0; i < shoots.length; i++) {
    const s = shoots[i];
    s.grow += (0.06 + pump * 0.05); // grow faster when pumped
    // Append a segment as the shoot grows past each integer step.
    while (s.segs.length < Math.floor(s.grow) && s.segs.length < 9) {
      const segLen = Math.min(w, h) * 0.045 * (1 + pump * 0.3);
      s.angle += (Math.random() - 0.5) * 0.5 - 0.12; // curl upward-ish
      const nx = s.tipX + Math.cos(s.angle) * segLen;
      const ny = s.tipY + Math.sin(s.angle) * segLen;
      s.segs.push({ x0: s.tipX, y0: s.tipY, x1: nx, y1: ny,
        bud: s.segs.length === 8 });
      s.tipX = nx;
      s.tipY = ny;
    }
    s.life -= 0.006;
  }
  shoots = shoots.filter((s) => s.life > 0);

  // Decay pulses & tap energy.
  rootPulse *= 0.9;
  trunkThick *= 0.92;
  pump *= 0.968; // slow decay so repeated taps accumulate

  // Drones breathe with the trunk pulse + tap energy → a living woody bed.
  drone?.update?.({ volume: 0.7 + rootPulse * 0.2 + pump * 0.1 });
  drone2?.update?.({ volume: 0.45 + rootPulse * 0.15 + pump * 0.08 });
}

// --- Button: tap / XY-drag sprouts a NEW SHOOT that grows + plucks a note ----
function act({ event: e, sound: { synth }, screen, num }) {
  if (!(e.is("touch") || e.is("draw"))) return;

  const nx = e.x / screen.width; // 0..1
  const ny = e.y / screen.height; // 0..1
  const drag = e.is("draw");

  // Accumulate energy: taps punch, drags feed.
  pump = Math.min(3, pump + (drag ? 0.12 : 0.9));

  const hue = 0.22 + (1 - ny) * 0.24; // higher tap → brighter/golder shoot
  // A new shoot sprouts from the tap point and grows upward from there.
  shoots.push({
    segs: [], tipX: e.x, tipY: e.y,
    angle: -Math.PI / 2 + (nx - 0.5) * 1.0, // X biases the initial lean
    hue, pan: nx * 2 - 1, grow: 0,
    life: 1,
  });
  // A little burst of leaf flecks at the sprout point.
  const n = drag ? 3 : 9;
  for (let i = 0; i < n && flecks.length < 200; i++) {
    const ang = Math.random() * Math.PI * 2;
    const sp = 1 + Math.random() * (drag ? 2 : 4);
    flecks.push({ x: e.x, y: e.y, vx: Math.cos(ang) * sp,
      vy: Math.sin(ang) * sp - 1, life: 1, hue });
  }

  // SONIC pluck — X→pan/hue, Y→pitch (top = high). Woody triangle + sine body.
  const scale = ["d", "e", "f", "a", "c"];
  const oct = 2 + Math.floor((1 - ny) * 3); // 2..4, higher tap = higher
  const note = scale[Math.min(4, Math.floor(nx * 5))] + oct;
  synth({ tone: note, type: "triangle", duration: drag ? 0.22 : 0.5,
    attack: 0.004, decay: 0.45, volume: drag ? 0.42 : 0.78, pan: nx * 2 - 1 });
  synth({ tone: note, type: "sine", duration: drag ? 0.16 : 0.4, attack: 0.003,
    decay: 0.5, volume: 0.35, pan: nx * 2 - 1 }); // woody body
}

let grounded = false; // has the deep-forest ground been laid solid yet?
function paint({ ink, line, circle, box, screen, sound, num, paintCount }) {
  const w = screen.width, h = screen.height;
  const baseX = w / 2, baseY = h * 0.92;

  // Lay a SOLID deep-forest ground on the first frames so the field reads dark
  // green (a translucent veil alone converges to neutral gray over the default
  // canvas). After that, a translucent green veil each frame → older growth
  // persists then fades softly (trails). Vertical gradient reads as depth.
  if (!grounded || Number(paintCount) < 3) {
    ink("fade:#06180c-#010604:vertical").box(0, 0, w, h);
    grounded = true;
  }
  // Two-part veil: a stronger opaque-ish dark forest wash keeps the ground deep
  // green (a thin veil alone drifts to gray), plus a lighter tint for long trails.
  ink(5, 20, 11, Math.max(80, 110 - pump * 8)).box(0, 0, w, h);
  ink("fade:#06180c-#010604:vertical", 40).box(0, 0, w, h);

  // --- Live audio garnish (visuals never DEPEND on it) ---
  const bands = sound.speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const air = bands.find((b) => b.name === "air")?.amplitude || 0;

  // === ROOT PULSE — the bass/trunk beat blooms from the base of the plant ===
  const bloom = Math.max(rootPulse, 0) * (1 + pump * 0.3);
  if (bloom > 0.02) {
    const R = Math.min(w, h) * (0.1 + bass * 0.4) * bloom;
    const [r0, g0, b0] = hue2rgb(num, 0.11, 0.7, 0.35); // warm earthy glow
    for (let i = 4; i > 0; i--) {
      ink(r0, g0, b0, 34 * bloom).circle(baseX, baseY, R * (i / 4), true);
    }
  }

  // === THE TRUNK — a thick woody stem rising from the base into the vine ===
  const trunkH = Math.min(h, w) * 0.08;
  const trunkW = 4 + trunkThick * 5 + pump * 2;
  const [tr, tg, tb] = hue2rgb(num, 0.09, 0.55, 0.28); // deep brown-green
  ink(tr, tg, tb, 235 * vineFade)
    .line(baseX, baseY, baseX, baseY - trunkH, trunkW);

  // === THE VINE — the drawn melody. Each segment's angle+hue ∝ its note. ===
  // Draw oldest→newest so the tip sits on top. Segments born recently glow.
  const now = lastStep;
  for (let i = 0; i < vine.length; i++) {
    const s = vine[i];
    const age = now - s.born; // steps since it grew
    const fresh = Math.max(0, 1 - age * 0.16); // recent segments glow brighter
    const [r, g, b] = hue2rgb(num, s.hue, 0.85, 0.42 + fresh * 0.28);
    const a = 240 * vineFade;
    const th = s.thick * (1 + fresh * 0.6 + bass * 0.5);
    ink(r, g, b, a).line(s.x0, s.y0, s.x1, s.y1, Math.max(1, th));
    // A soft highlight core down the stem.
    if (th > 2) {
      const [hr, hg, hb] = hue2rgb(num, s.hue, 0.5, 0.72);
      ink(hr, hg, hb, 120 * vineFade).line(s.x0, s.y0, s.x1, s.y1, Math.max(1, th * 0.4));
    }

    // === LEAF / FLOWER bud at the tip of accented segments ===
    if (s.bud) {
      const budGrow = Math.min(1, 0.3 + fresh); // pops as it's born
      const br = (3 + budGrow * 5) * (1 + pump * 0.3);
      const isFlower = ACCENT_STEPS.has(((s.born - seedStep) % PATTERN_LEN + PATTERN_LEN) % PATTERN_LEN);
      const [pr, pg, pb] = hue2rgb(num, s.budHue, isFlower ? 0.9 : 0.7,
        isFlower ? 0.6 : 0.45);
      if (isFlower) {
        // A little flower: petals around the tip.
        for (let p = 0; p < 5; p++) {
          const ang = (p / 5) * Math.PI * 2 + now * 0.05;
          ink(pr, pg, pb, 220 * vineFade)
            .circle(s.x1 + Math.cos(ang) * br, s.y1 + Math.sin(ang) * br,
              br * 0.6 + budGrow * 2, true);
        }
        ink(255, 230, 120, 230 * vineFade).circle(s.x1, s.y1, br * 0.7, true); // pollen
      } else {
        ink(pr, pg, pb, 220 * vineFade).circle(s.x1, s.y1, br, true); // leaf
      }
    }
  }

  // === The growing TIP — a bright bud that swells within each step ===
  if (vine.length > 0 && vineFade > 0.02) {
    const tipR = (3 + stepProgress * 4 + bass * 8) * (1 + pump * 0.4);
    ink(230, 255, 180, 220 * vineFade).circle(tipX, tipY, tipR, true);
    ink(255, 255, 255, 160 * vineFade).circle(tipX, tipY, tipR * 0.45, true);
  }

  // === TAP SHOOTS — the whole-piece "button" made visible as new plants ===
  for (let i = 0; i < shoots.length; i++) {
    const s = shoots[i];
    const [r, g, b] = hue2rgb(num, s.hue, 0.85, 0.5);
    for (let j = 0; j < s.segs.length; j++) {
      const sg = s.segs[j];
      ink(r, g, b, 235 * s.life).line(sg.x0, sg.y0, sg.x1, sg.y1,
        Math.max(1, (2 + pump) * s.life));
      if (sg.bud) {
        const [br2, bg2, bb2] = hue2rgb(num, s.hue + 0.05, 0.9, 0.6);
        ink(br2, bg2, bb2, 230 * s.life).circle(sg.x1, sg.y1, 4 + pump, true);
      }
    }
    // Growing tip glow.
    ink(230, 255, 180, 200 * s.life).circle(s.tipX, s.tipY, 3 + pump, true);
  }

  // === HAT flecks = tiny leaf flickers ===
  for (let i = 0; i < flecks.length; i++) {
    const f = flecks[i];
    const [r, g, b] = hue2rgb(num, f.hue, 0.7, 0.55);
    ink(r, g, b, 220 * f.life).circle(f.x, f.y, 1 + f.life * 2, true);
  }

  // Air-band shimmer accent (garnish when audio is live) — dew on the leaves.
  if (air > 0.05) {
    ink(200, 255, 210, air * 90).circle(baseX, baseY - trunkH, Math.min(w, h) * 0.2, false, 1);
  }
}

function leave() {
  drone?.kill?.(0.5); // fade the woody drones out on exit
  drone2?.kill?.(0.5);
  drone = null;
  drone2 = null;
}

// Note: there is intentionally NO `beat` export. The metronome is not the audio
// source — all onsets + growth come from the UTC scheduler in sim() (indexed by
// floor(globalStep)) so two instances opened anywhere auto-align.
export { boot, sim, paint, act, leave };
