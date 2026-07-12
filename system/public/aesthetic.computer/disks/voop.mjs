// voop, 26.07.12
// Self-running bouncing-droplet A/V instrument: a UTC-synced pentatonic bounce
// pattern makes a glossy droplet BOUNCE on a floor line. Every audible "voop" is
// one BOUNCE IMPACT — the instant a note sounds the droplet hits the floor,
// emits a ripple ring, and its post-impact arc HEIGHT ∝ the note's pitch (high
// note = high arc), its HUE ∝ pitch. So a viewer literally READS the melody as
// the height + color of the bounces. A soft sub thump = the floor shockwave;
// sparkles = tiny droplets flung up on impact; a motion-blur ghost traces the
// melodic contour.
//
// FOUR PRINCIPLES (marketing/av-reels/button-sync-allegory.md):
//   1. NO resolution() — renders at native res (vector + alpha veil trail).
//   2. WHOLE PIECE IS A BUTTON — tap / XY-drag FLINGS an extra droplet from the
//      tap point that bounces + voops (X→pan/hue, Y→pitch/launch-height); taps
//      accumulate decaying `pump` that makes bounces bigger/glossier + flashes
//      the floor.
//   3. UTC-SYNCED RHYTHM — bounce impacts are scheduled from clock.time() in sim
//      (NOT beat()), indexed by floor(globalBeat) % len, so two instances opened
//      anywhere bounce in lockstep on the shared wall-clock grid.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY — the visual IS the score (see above).
//
// BPM ~132. Loops seamlessly (the bounce pattern returns to its seam every bar).

// --- Score ------------------------------------------------------------------
// One looping bar of a bright pentatonic bounce (12 steps). Step 0 == the seam.
const BOUNCE = [
  "c3", "e3", "g3", "a3", "g3", "c4",
  "a3", "g3", "e3", "d3", "g3", "e3",
];
const SUB = ["c2", "c2", "g1", "a1", "g1", "c2"]; // half-time floor thump root

// MIDI-ish pitch of each note, for pitch→arc-height / pitch→hue mapping.
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
const BOUNCE_PITCHES = BOUNCE.map(notePitch);
const PITCH_MIN = Math.min(...BOUNCE_PITCHES);
const PITCH_MAX = Math.max(...BOUNCE_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN); // 0..1

const BPM = 132; // springy, hypnotic
const BEAT_MS = 60000 / BPM; // ms per beat — the UTC grid unit

// --- Rhythm state (UTC-driven) ---------------------------------------------
let lastBeat = -1; // last integer global-beat we fired
let step = 0; // current bounce index, from UTC
let beatProgress = 0; // 0..1 within the current beat (smooth arc motion)

// --- The main droplet (the score's ball) ------------------------------------
// It flies a parabolic arc between impacts. On each impact we set its launch
// velocity so it reaches an apex whose height ∝ the just-fired note's pitch, and
// it lands exactly one beat later — so the arc is the melody, drawn live.
let dropX = 0.5; // 0..1 horizontal (sweeps across on each bounce)
let dropXNext = 0.5; // target X for the current flight (lerp)
let dropXPrev = 0.5;
let arcHeight = 0.4; // 0..1 apex height of the current flight (pitch)
let dropHue = 180; // hue of the droplet this flight (pitch)
let squash = 0; // impact squash, 1 at impact, decays — glossy bounce feel

// --- Allegory state ---------------------------------------------------------
let ripples = []; // floor impact rings { x, r, life, hue }
let sparkles = []; // tiny droplets flung up on impact { x, y, vx, vy, life, hue }
let ghost = []; // motion-blur trail of the droplet { x, y, r, hue, life }
let ghostTick = 0; // throttle counter for ghost sampling
let floorFlash = 0; // floor line flash on sub-thump / taps, decays
let subThump = 0; // central low shockwave from bass, decays

// --- Button / pump state (§2) ----------------------------------------------
let pump = 0; // decaying global energy from taps, 0..~3
// Extra tap-flung droplets: independent bouncing balls the viewer flings in.
let flung = []; // { x, y, vx, vy, r, hue, life, bounces }

function boot({ sound, clock }) {
  sound.bpm(BPM); // only affects the internal metronome; UTC scheduler is boss
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline)
  // A gentle rubbery room so each "voop" has a springy tail.
  sound.room?.set?.({ enabled: true, mix: 0.28, feedback: 0.5 });
}

// Fire one step of the score. Called the instant a new global beat crosses in
// sim, so every onset is UTC-aligned and every note IS a bounce impact.
function fireStep(idx, synth, w, h) {
  const s = ((idx % BOUNCE.length) + BOUNCE.length) % BOUNCE.length;
  step = s;
  const note = BOUNCE[s];
  const pn = pitchNorm(note); // 0 (low) .. 1 (high)

  // The droplet just LANDED here (end of previous flight). Impact!
  const impactX = dropXNext; // where we are now
  dropXPrev = impactX;

  // ALLEGORY: bounce HEIGHT after impact ∝ pitch; HUE ∝ pitch.
  arcHeight = 0.22 + pn * 0.6; // high note = high arc
  dropHue = 200 - pn * 200; // low→cyan/blue, high→warm pink (candy)
  if (dropHue < 0) dropHue += 360;

  // Next landing X: step across the floor so the arcs march left↔right, reading
  // as a bouncing melody scanning the width. Deterministic from step for loop.
  dropXNext = 0.12 + (s / (BOUNCE.length - 1)) * 0.76;
  dropX = impactX;
  squash = 1; // fat impact squash → gloss

  // The rubbery "voop": a plucky sine+triangle blip pitched to the note.
  const pan = impactX * 2 - 1;
  synth({
    tone: note,
    type: "sine",
    beats: 0.55,
    attack: 0.004,
    decay: 0.62,
    volume: 0.5,
    pan,
  });
  synth({
    tone: note,
    type: "triangle",
    beats: 0.28,
    attack: 0.003,
    decay: 0.4,
    volume: 0.24,
    pan,
  });

  // Floor ripple ring born exactly at the impact point.
  ripples.push({ x: impactX * w, r: 6, life: 1, hue: dropHue });

  // Sparkle droplets flung up on impact — a few tiny secondary droplets.
  const nSpark = 4 + Math.round(pn * 5);
  for (let i = 0; i < nSpark; i++) {
    const ang = -Math.PI / 2 + (Math.random() - 0.5) * 1.6;
    const spd = 2 + Math.random() * (3 + pn * 4);
    sparkles.push({
      x: impactX * w,
      y: h * 0.82,
      vx: Math.cos(ang) * spd,
      vy: Math.sin(ang) * spd,
      life: 1,
      hue: dropHue,
    });
  }

  // A bright high sparkle harmonic on high steps — the flung droplets' shimmer.
  if (pn > 0.6) {
    synth({
      tone: note,
      type: "sine",
      beats: 0.35,
      attack: 0.002,
      decay: 0.35,
      volume: 0.16,
      pan: -pan,
    });
  }

  // Sub thump on the half-beat — the floor shockwave you can feel + SEE.
  if (s % 2 === 0) {
    const bi = ((Math.floor(idx / 2) % SUB.length) + SUB.length) % SUB.length;
    synth({
      tone: SUB[bi],
      type: "sine",
      beats: 1.4,
      attack: 0.008,
      decay: 0.5,
      volume: 0.5,
    });
    // Soft rubber body under the sub for that bouncing-bass droplet weight.
    synth({
      tone: SUB[bi],
      type: "triangle",
      beats: 0.7,
      attack: 0.005,
      decay: 0.45,
      volume: 0.18,
    });
    subThump = 1; // ALLEGORY: bass = central floor shockwave
    floorFlash = Math.max(floorFlash, 0.8);
  }
}

// Drive audio + rhythm from UTC in sim (NOT beat()) so onsets align across
// instances. Also advance the droplet's parabolic flight + decay energies.
function sim({ sound: { speaker, synth }, clock, screen }) {
  speaker?.poll(); // mandatory before reading audio

  const w = screen?.width || 360;
  const h = screen?.height || 640;

  // clock.time() can return an *Invalid Date* before UTC sync completes (its
  // getTime() is NaN, and it's not null so `??` won't catch it) — guard with
  // Date.now() so the rhythm always runs, aligning across instances once synced.
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now();
  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx; // 0..1 through the current beat (arc phase)

  if (idx !== lastBeat) {
    lastBeat = idx;
    fireStep(idx, synth, w, h); // a new UTC beat crossed → a bounce impact
  }

  // Interpolate the droplet's horizontal position across the flight (impact→next).
  dropX = dropXPrev + (dropXNext - dropXPrev) * beatProgress;

  // Ghost trail: sample the droplet's current position each tick (throttled).
  // Only every few ticks so the trail is a comet, not a solid line.
  ghostTick = (ghostTick + 1) % 2;
  if (ghostTick === 0) {
    ghost.push({ x: dropX, yPhase: beatProgress, hue: dropHue, life: 1 });
    if (ghost.length > 40) ghost.shift();
  }

  // Decays (per 120 Hz tick).
  pump *= 0.965;
  squash *= 0.86;
  subThump *= 0.9;
  floorFlash *= 0.9;

  // Advance / cull floor ripples.
  for (const rp of ripples) {
    rp.r += 5 + rp.life * 7;
    rp.life -= 0.05;
  }
  ripples = ripples.filter((rp) => rp.life > 0);

  // Advance sparkle droplets under gravity.
  for (const sp of sparkles) {
    sp.x += sp.vx;
    sp.y += sp.vy;
    sp.vy += 0.35; // gravity
    sp.life -= 0.035;
  }
  sparkles = sparkles.filter((sp) => sp.life > 0 && sp.y < h + 20);

  // Fade the ghost trail.
  for (const g of ghost) g.life -= 0.03;
  ghost = ghost.filter((g) => g.life > 0);

  // Advance tap-flung droplets (§2): full bouncing physics on the floor line.
  const floorY = h * 0.82;
  for (const f of flung) {
    f.x += f.vx;
    f.y += f.vy;
    f.vy += 0.45; // gravity
    if (f.y >= floorY && f.vy > 0) {
      f.y = floorY;
      f.vy *= -0.72; // bouncy restitution
      f.vx *= 0.94;
      f.bounces++;
      // Each landing = a voop + a ripple, so flung droplets are audible+visible.
      ripples.push({ x: f.x, r: 5, life: 1, hue: f.hue });
      floorFlash = Math.max(floorFlash, 0.5);
      if (Math.abs(f.vy) > 1.2 && f.bounces < 6) {
        const pn = 1 - Math.min(1, f.y / h);
        const note = ["c", "e", "g", "a", "c"][Math.min(4, f.bounces)] + 3;
        synth({
          tone: note,
          type: "sine",
          beats: 0.4,
          attack: 0.003,
          decay: 0.5,
          volume: 0.4,
          pan: (f.x / w) * 2 - 1,
        });
      }
    }
    f.life -= 0.006;
  }
  // Keep a flung droplet while it still has life AND is either still bouncing
  // with energy or hasn't settled yet (few bounces).
  flung = flung.filter(
    (f) => f.life > 0 && (Math.abs(f.vy) > 0.2 || f.bounces < 3),
  );
  if (flung.length > 8) flung = flung.slice(-8);
}

// THE WHOLE PIECE IS A BUTTON (§2): any tap / XY-drag FLINGS an extra droplet
// from the touch point that bounces + voops — X→pan/hue, Y→pitch/launch-height.
function act({ event: e, sound: { synth }, screen }) {
  if (e.is("touch") || e.is("draw")) {
    const x = e.x / screen.width; // 0..1
    const y = e.y / screen.height; // 0..1
    // Taps punch, drags feed — accumulate decaying pump energy.
    pump = Math.min(3, pump + (e.is("draw") ? 0.1 : 0.9));
    floorFlash = Math.max(floorFlash, 0.7);

    const hue = x * 300; // candy hue across the width
    // Launch height ∝ (1 - y): tapping high flings hard, low = gentle bounce.
    const launch = -(4 + (1 - y) * 12);
    flung.push({
      x: e.x,
      y: e.y,
      vx: (Math.random() - 0.5) * 4,
      vy: launch,
      r: 6 + (1 - y) * 8,
      hue,
      life: 1,
      bounces: 0,
    });

    // SONIC BOOST — X→pitch/pan, Y→octave, pentatonic candy palette.
    const note =
      ["c", "d", "e", "g", "a"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 4));
    synth({
      tone: note,
      type: "sine",
      beats: 0.5,
      attack: 0.004,
      decay: 0.6,
      volume: 0.55,
      pan: x * 2 - 1,
    });
    synth({
      tone: note,
      type: "triangle",
      beats: 0.25,
      attack: 0.002,
      decay: 0.35,
      volume: 0.3 * (1 - y), // brighter flick higher up
      pan: x * 2 - 1,
    });
  }
}

// Parabolic arc height for a flight: 0 at the impacts (t=0,1), 1 at apex (t=.5).
function arcParab(t) {
  return 4 * t * (1 - t); // 0..1..0
}

function paint({ ink, box, circle, oval, screen, sound, num }) {
  const { width: w, height: h } = screen;
  const floorY = h * 0.82;

  // --- Live audio reads -----------------------------------------------------
  const bands = sound.speaker?.frequencies?.left || [];
  const bandAmp = (name) => bands.find((b) => b.name === name)?.amplitude || 0;
  const bass = bandAmp("subBass");
  const amp = sound.speaker?.amplitudes?.left || 0;

  const energy = Math.min(2, subThump * 0.6 + bass * 1.1 + amp * 0.5 + pump * 0.5);

  // --- Background gradient (solid) ------------------------------------------
  // Deep candy gradient, repainted opaque each frame — the gooey comet trail is
  // carried by the `ghost` object list (below), so we DON'T rely on a partial
  // veil (which accumulates to muddy gray). Hand-rolled vertical scanline ramp
  // for guaranteed deep, saturated candy tones (named/hex fade came up gray).
  {
    const bandH = Math.max(2, Math.ceil(h / 48));
    for (let by = 0; by < h; by += bandH) {
      const t = by / h; // 0 top .. 1 bottom
      const r = 14 + t * 30; // deep indigo → violet
      const g = 6 + t * 8;
      const bl = 40 + t * 50;
      ink(r, g, bl).box(0, by, w, bandH);
    }
  }

  // --- Floor line -----------------------------------------------------------
  // The floor the droplet bounces on. Flashes on sub-thumps + taps.
  const ff = Math.min(1, floorFlash + subThump * 0.6);
  const [fr, fg, fb] = num.hslToRgb((dropHue + 180) % 360, 80, 55 + ff * 30);
  ink(fr, fg, fb, 90 + ff * 130).box(0, floorY - 1 - ff * 2, w, 2 + ff * 4);
  // A soft glow band under the floor line.
  ink(fr, fg, fb, 24 + ff * 40).box(0, floorY, w, h - floorY);

  // Central floor shockwave from the sub-thump (§4: bass = shockwave).
  if (subThump > 0.02) {
    const sr = w * (0.1 + subThump * 0.55);
    for (let i = 3; i > 0; i--) {
      ink(fr, fg, fb, 22 * subThump).oval(w / 2, floorY, sr * (i / 3), sr * (i / 3) * 0.28, true);
    }
  }

  // --- Motion-blur ghost trail (melodic contour) ----------------------------
  // The droplet's recent arc positions, fading — a gooey comet tracing the
  // melody's shape (halo + brighter core per sample).
  for (const g of ghost) {
    const gy = floorY - arcParab(g.yPhase) * arcHeight * (h * 0.62);
    const gx = g.x * w;
    const [r0, g0, b0] = num.hslToRgb(((g.hue % 360) + 360) % 360, 95, 62);
    ink(r0, g0, b0, 45 * g.life).circle(gx, gy, 5 + g.life * 9, true); // gooey halo
    ink(Math.min(255, r0 + 40), Math.min(255, g0 + 40), Math.min(255, b0 + 40), 110 * g.life)
      .circle(gx, gy, 2 + g.life * 5, true); // brighter core
  }

  // --- Floor ripples (§4: each note = one impact ripple) --------------------
  for (const rp of ripples) {
    const [r0, g0, b0] = num.hslToRgb(((rp.hue % 360) + 360) % 360, 95, 62);
    // Flattened ellipse ripple hugging the floor plane.
    ink(r0, g0, b0, 180 * rp.life).oval(rp.x, floorY, rp.r, rp.r * 0.32, false);
    ink(255, 255, 255, 120 * rp.life).oval(rp.x, floorY, rp.r * 0.55, rp.r * 0.18, false);
  }

  // --- The main droplet (the score's ball) ----------------------------------
  // Its height this frame is the parabola of the current flight, scaled by the
  // just-fired note's arcHeight (pitch). Squash flattens it at impact for gloss.
  const arc = arcParab(beatProgress);
  const dy = floorY - arc * arcHeight * (h * 0.62);
  const dx = dropX * w;
  const [dr, dg, db] = num.hslToRgb(((dropHue % 360) + 360) % 360, 95, 62);
  const baseR = (12 + pump * 5 + energy * 4) * (1 + subThump * 0.2);
  // Squash & stretch: wide + flat at impact (arc~0), round at apex.
  const sq = squash * (1 - arc); // most squash right at the floor
  const rw = baseR * (1 + sq * 0.9);
  const rh = baseR * (1 - sq * 0.55);

  // Glossy droplet: soft halo, body, bright specular highlight.
  ink(dr, dg, db, 70).oval(dx, dy, rw * 1.9, rh * 1.9, true); // halo
  ink(dr, dg, db, 235).oval(dx, dy, rw, rh, true); // body
  ink(Math.min(255, dr + 70), Math.min(255, dg + 70), Math.min(255, db + 70), 220)
    .oval(dx - rw * 0.25, dy - rh * 0.3, rw * 0.45, rh * 0.4, true); // inner sheen
  ink(255, 255, 255, 230).oval(dx - rw * 0.3, dy - rh * 0.35, rw * 0.2, rh * 0.2, true); // specular

  // --- Sparkle droplets flung up on impact ----------------------------------
  for (const sp of sparkles) {
    const [r0, g0, b0] = num.hslToRgb(((sp.hue % 360) + 360) % 360, 100, 70);
    ink(r0, g0, b0, 220 * sp.life).circle(sp.x, sp.y, 1 + sp.life * 3, true);
  }

  // --- Tap-flung droplets (§2) ----------------------------------------------
  for (const f of flung) {
    const [r0, g0, b0] = num.hslToRgb(((f.hue % 360) + 360) % 360, 95, 62);
    ink(r0, g0, b0, 80 * f.life).circle(f.x, f.y, f.r * 1.7, true); // halo
    ink(r0, g0, b0, 240 * f.life).circle(f.x, f.y, f.r, true); // body
    ink(255, 255, 255, 220 * f.life).circle(f.x - f.r * 0.3, f.y - f.r * 0.3, f.r * 0.28, true);
  }

}

export { boot, sim, act, paint };
