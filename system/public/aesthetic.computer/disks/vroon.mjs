// vroon, 26.07.12
// Self-running warp-tunnel A/V instrument. A big SWEEPING saw/PWM DRONE rises
// and falls ("vrooon") over a deep sub; rhythmic stabs mark the grid. The
// sweep is the star — and it IS the speed of the warp: as the sweep rises in
// pitch the tunnel of radial light-streaks rushes FASTER outward and the hue
// shifts up (low sweep = slow warm rush, high sweep = fast bright rush). The
// sub is the tunnel's core glow; accents/stabs blast shockwave rings outward.
// You literally SEE the sweep as acceleration through a warp tunnel.
//
// FOUR PRINCIPLES (marketing/av-reels/button-sync-allegory.md):
//   1. NO resolution() — renders at native res (vector + one feedback zoom).
//   2. WHOLE PIECE IS A BUTTON — tap / XY-drag blasts a shockwave ring from the
//      tap point + a rev of the drone. X→pan/hue, Y→pitch/sweep target. Repeated
//      taps accumulate decaying `pump` that throttles the tunnel faster+brighter.
//   3. UTC-SYNCED RHYTHM — the sweep PHASE + the accent grid derive from
//      clock.time() in sim (NOT beat()), so two instances warp in lock-step;
//      accents index floor(globalBeat) % len. BPM 128.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY — sweep→speed+hue, sub→core glow, amp→
//      streak brightness/length, stabs→shockwave rings. The visual IS the sound.

const BPM = 128; // brisk, propulsive
const BEAT_MS = 60000 / BPM; // ms per beat — the UTC grid unit
// The SWEEP cycles over a whole musical bar (4 beats) so it loops seamlessly:
// phase 0 == the seam. One full "vrooon" per bar.
const SWEEP_BARS = 1;
const SWEEP_MS = BEAT_MS * 4 * SWEEP_BARS; // ms per full sweep rise+fall

// Accent stabs (one bar, 8 steps). Root motion under the sweep. Step 0 = seam.
const STABS = ["e1", "e1", "e2", "e1", "c2", "c2", "g1", "b1"];
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 36;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}

// --- Rhythm / sweep state (all UTC-driven) ----------------------------------
let lastBeat = -1; // last integer global-beat we fired a stab on
let sweep = 0; // 0..1, the sweep's normalized rise/fall value (the STAR)
let sweepTarget = 0.5; // tap-set bias the sweep drifts toward (Y of last tap)
let beatProgress = 0; // 0..1 within the current beat (smooth visuals)

// --- The sustained drone voices (created once, mutated live every sim) ------
let droneSaw = null; // the sweeping saw — the "vrooon"
let dronePwm = null; // a square layer for PWM-ish grit
let subVoice = null; // the deep sub under it all
let started = false; // guard so we build the drone exactly once

// --- Allegory / visual state ------------------------------------------------
let streaks = []; // radial light-streaks rushing outward { ang, dist, len, hue, life }
let rings = []; // shockwave rings from stabs + taps { x, y, r, life, hue, thick }
let coreGlow = 0; // central tunnel-core glow, kicked by the sub, decays
let spawnAcc = 0; // fractional accumulator so streak spawn rate ∝ speed

// --- Button / pump state (§2) -----------------------------------------------
let pump = 0; // decaying tap energy, 0..~3 — throttles the tunnel
let lastPan = 0; // last tap's pan, biases the drone pan on a rev

function boot({ sound, clock }) {
  sound.bpm(BPM); // only affects the internal metronome; UTC scheduler is boss
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline)
  // Cavernous tail so the warp feels deep + endless.
  sound.room?.set?.({ enabled: true, mix: 0.3, feedback: 0.62 });
}

// Build the three sustained drone voices once (AC is silent without a voice).
function startDrone(synth) {
  if (started) return;
  started = true;
  droneSaw = synth({ tone: "e2", type: "sawtooth", duration: "🔁",
    attack: 0.4, decay: 0.9, volume: 0.4, pan: 0 });
  dronePwm = synth({ tone: "e3", type: "square", duration: "🔁",
    attack: 0.4, decay: 0.9, volume: 0.16, pan: 0 });
  subVoice = synth({ tone: "e1", type: "sine", duration: "🔁",
    attack: 0.5, decay: 0.9, volume: 0.5, pan: 0 });
}

// Fire one accent stab. Called the instant a new global beat crosses in sim, so
// every onset is UTC-aligned. Also blasts a shockwave ring (§4 allegory).
function fireStab(idx, synth, screen) {
  const s = ((idx % STABS.length) + STABS.length) % STABS.length;
  const note = STABS[s];
  synth({ tone: note, type: "sawtooth", beats: 0.4, attack: 0.004,
    decay: 0.45, volume: 0.42, pan: Math.sin((s / STABS.length) * Math.PI * 2) * 0.5 });
  // A pulse/tick keeps the grid crisp (noise → type, NOT tone).
  synth({ type: "noise-white", tone: 6000, beats: 0.1, attack: 0.001,
    decay: 0.18, volume: 0.14 });
  // Sub thumps on the downbeats → drives the tunnel CORE GLOW.
  if (s % 2 === 0) {
    synth({ tone: STABS[s], type: "sine", beats: 1.2, attack: 0.008,
      decay: 0.5, volume: 0.4 });
    coreGlow = 1;
  }
  // ALLEGORY: the stab BLASTS a shockwave ring outward from the vanishing point.
  const cx = screen.width / 2, cy = screen.height / 2;
  rings.push({ x: cx, y: cy, r: Math.min(screen.width, screen.height) * 0.05,
    life: 1, hue: 200 + (notePitch(note) % 12) * 10, thick: 3 });
}

// Drive audio + rhythm + the sweep from UTC in sim (NOT beat()) so everything
// aligns across instances. Also decay all the eased visual energies here.
function sim({ sound: { speaker, synth }, screen, clock }) {
  speaker?.poll(); // mandatory before reading audio
  startDrone(synth); // build the sustained drone on the first sim tick

  // clock.time() can return an *Invalid Date* before UTC sync completes (its
  // getTime() is NaN, and it's not null so `??` won't catch it) — guard with
  // Date.now() so the sweep + rhythm always run and align once synced.
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now();

  // --- THE SWEEP (the star): a UTC-phase-locked rise+fall, 0..1. Absolute from
  // epoch → every instance computes the identical sweep at the same wall ms. ---
  const sweepPhase = (ms % SWEEP_MS) / SWEEP_MS; // 0..1 across the bar
  // A smooth asymmetric rise (fast up) then fall — a satisfying "vrooon". Bias
  // toward the last tap's Y (sweepTarget) so taps steer the warp.
  const raw = 0.5 - 0.5 * Math.cos(sweepPhase * Math.PI * 2); // 0→1→0 smooth
  sweep = Math.min(1, raw * 0.7 + sweepTarget * 0.3 + pump * 0.12);

  // --- Beat grid: fire accent stabs on integer beats ---
  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx; // 0..1 through the current beat
  if (idx !== lastBeat) {
    lastBeat = idx;
    fireStab(idx, synth, screen);
  }

  // --- Steer the sustained drone LIVE from the sweep. The saw pitch rises with
  // the sweep (the audible "vrooon"); PWM layer brightens; volume swells at the
  // peak. This is the sweep you HEAR that you also SEE as tunnel speed. ---
  if (droneSaw) {
    // Sweep the saw across ~1.5 octaves of E; pump nudges it up on a rev.
    const semis = sweep * 18; // 0..18 semitones above E2
    const sawHz = 82.41 * Math.pow(2, semis / 12); // E2 = 82.41 Hz
    droneSaw.update?.({ tone: sawHz, volume: 0.32 + sweep * 0.16,
      pan: lastPan * 0.4 });
    dronePwm?.update?.({ tone: sawHz * 2, volume: 0.1 + sweep * 0.14,
      pan: -lastPan * 0.4 });
    subVoice?.update?.({ volume: 0.42 + coreGlow * 0.14 });
  }

  // --- Spawn radial streaks at a rate ∝ warp SPEED (sweep + pump). Faster
  // sweep → more streaks born per tick → the tunnel visibly RUSHES faster. ---
  const speed = 0.5 + sweep * 2.5 + pump * 1.2; // the warp speed
  spawnAcc += speed * 2.2; // dense streaks — the tunnel is MADE of them
  while (spawnAcc >= 1) {
    spawnAcc -= 1;
    const ang = Math.random() * Math.PI * 2;
    streaks.push({
      ang,
      dist: 0.02 + Math.random() * 0.04, // born near the vanishing point
      len: 0,
      hue: 200 + sweep * 130, // low sweep = warm blue/violet, high = bright cyan/white-hot
      life: 1,
    });
  }

  // --- Advance streaks OUTWARD at the warp speed (this is the rush you read as
  // the sweep). Higher sweep = they accelerate away faster. ---
  for (const st of streaks) {
    st.dist *= 1 + 0.05 + speed * 0.03; // exponential rush outward (tunnel perspective)
    st.len = Math.min(0.25, st.len + 0.01 + speed * 0.004);
    st.life -= 0.02 + speed * 0.004;
  }
  streaks = streaks.filter((st) => st.life > 0 && st.dist < 1.4);
  if (streaks.length > 420) streaks = streaks.slice(-420); // safety cap

  // --- Advance shockwave rings + tap bursts ---
  for (const r of rings) {
    r.r += 5 + r.life * 12 + pump * 4;
    r.life -= 0.03;
  }
  rings = rings.filter((r) => r.life > 0);

  // Decays (per 120 Hz tick).
  pump *= 0.965;
  coreGlow *= 0.9;
}

// THE WHOLE PIECE IS A BUTTON (§2): any tap / XY-drag blasts a shockwave ring
// from the tap point + revs the drone. X→pan/hue, Y→pitch/sweep target.
function act({ event: e, sound: { synth }, screen }) {
  if (e.is("touch") || e.is("draw")) {
    const x = e.x / screen.width; // 0..1
    const y = e.y / screen.height; // 0..1
    // Taps punch, drags feed — accumulate decaying pump (throttles the tunnel).
    pump = Math.min(3, pump + (e.is("draw") ? 0.1 : 0.9));
    // Y steers the sweep target (top = high/fast, bottom = low/slow).
    sweepTarget = 1 - y;
    lastPan = x * 2 - 1;

    // SHOCKWAVE RING blasting outward FROM the tap point (§2 + §4).
    rings.push({ x: e.x, y: e.y, r: 4, life: 1.2,
      hue: 200 + x * 160, thick: 4 });
    // Seed a burst of streaks radiating from the tap → a visible rev.
    for (let i = 0; i < 10; i++) {
      streaks.push({ ang: Math.random() * Math.PI * 2,
        dist: 0.03 + Math.random() * 0.05, len: 0, hue: 200 + x * 160, life: 1.2 });
    }

    // SONIC BOOST — a REV of the drone. X→pan, Y→pitch/brightness.
    const note = ["e", "g", "a", "b", "d"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 3));
    synth({ tone: note, type: "sawtooth", beats: 0.6, attack: 0.006,
      decay: 0.55, volume: 0.5, pan: x * 2 - 1 });
    synth({ tone: note, type: "square", beats: 0.3, attack: 0.003,
      decay: 0.35, volume: 0.22 * (1 - y), pan: x * 2 - 1 });
  }
}

function paint({ ink, box, line, circle, screen, sound, num, zoom, blur }) {
  const { width: w, height: h } = screen;
  const cx = w / 2, cy = h / 2;
  const maxR = Math.max(w, h) * 0.72; // tunnel reaches past the corners

  // --- Live audio reads -----------------------------------------------------
  const bands = sound.speaker?.frequencies?.left || [];
  const bandAmp = (name) => bands.find((b) => b.name === name)?.amplitude || 0;
  const sub = bandAmp("subBass");
  const amp = sound.speaker?.amplitudes?.left || 0;

  // Warp SPEED (must match sim's read so the picture tracks the sweep).
  const speed = 0.5 + sweep * 2.5 + pump * 1.2;

  // --- TRANSFORM-FEEDBACK TUNNEL (§4): zoom the previous frame OUTWARD from the
  // vanishing point every frame — faster sweep = stronger zoom = the frame's
  // light rushes past you faster. A balanced veil recycles it into an endless
  // tunnel without blowing out to white. This is the core allegory: the sweep
  // (which you HEAR rising) directly sets the zoom (the speed you SEE). ---
  const zoomAmt = 1.03 + sweep * 0.06 + pump * 0.04; // >1 → outward rush
  zoom?.(zoomAmt, 0.5, 0.5);
  // Deep-space veil — alpha tuned so streaks smear into a legible tunnel but the
  // frame keeps decaying (never accumulates to white). Higher alpha = the core
  // glow can't smear into a big soft blob under the feedback zoom.
  ink(4, 3, 14, 150).box(0, 0, w, h);

  // --- CORE GLOW: the sub = the tunnel's core light at the vanishing point.
  // A small hot pinpoint + a tight soft halo — kept small/dim so the STREAKS are
  // the star, not a big blob (the feedback zoom would otherwise bloom it huge). ---
  const glow = Math.max(coreGlow, sub * 1.4);
  const coreR = Math.min(w, h) * (0.012 + glow * 0.05 + sweep * 0.02);
  for (let i = 3; i > 0; i--) {
    const t = i / 3;
    ink(150, 180, 255, 22 * glow + 8).circle(cx, cy, coreR * t * 2.4, true);
  }
  // The hot pinpoint core.
  ink(235, 248, 255, 220).circle(cx, cy, coreR, true);

  // --- RADIAL SPEED-LINES / STREAKS rushing outward (§4). Each streak is a line
  // segment from its inner point toward the rim; length + brightness ∝ amp and
  // speed. Hue climbs with the sweep (warm→bright). Together they read as a warp
  // tunnel accelerating with the drone. ---
  for (const st of streaks) {
    const d0 = st.dist;
    const d1 = Math.min(1.4, st.dist + st.len * (1 + speed * 0.3));
    const x0 = cx + Math.cos(st.ang) * d0 * maxR;
    const y0 = cy + Math.sin(st.ang) * d0 * maxR;
    const x1 = cx + Math.cos(st.ang) * d1 * maxR;
    const y1 = cy + Math.sin(st.ang) * d1 * maxR;
    const hue = ((st.hue + sweep * 40) % 360 + 360) % 360;
    const [r0, g0, b0] = num.hslToRgb(hue, 95, 58 + sweep * 22 + amp * 15);
    // Brightness/thickness ∝ life * amplitude — louder = brighter, longer trails.
    const a = 235 * st.life * (0.75 + amp * 0.8 + sweep * 0.4);
    const thick = 1.2 + st.dist * (2.2 + speed * 0.6);
    ink(r0, g0, b0, Math.min(255, a)).line(x0, y0, x1, y1, thick);
    // A brighter leading tip near the outer end for a comet feel.
    ink(Math.min(255, r0 + 80), Math.min(255, g0 + 80), 255,
      Math.min(255, 220 * st.life)).circle(x1, y1, 1 + st.dist * 2.5, true);
  }

  // --- SHOCKWAVE RINGS (§2 + §4): stabs + taps blast expanding rings outward. ---
  for (const r of rings) {
    const hue = ((r.hue % 360) + 360) % 360;
    const [rr, gg, bb] = num.hslToRgb(hue, 95, 62);
    ink(rr, gg, bb, 190 * r.life).circle(r.x, r.y, r.r, false, r.thick * r.life + 1);
    ink(255, 255, 255, 120 * r.life).circle(r.x, r.y, r.r * 0.6, false, 1);
  }

  // --- Bloom on loud peaks so the tunnel feels like it's punching through. ---
  if (amp > 0.4 || pump > 1) blur?.(1);
}

export { boot, sim, act, paint };
