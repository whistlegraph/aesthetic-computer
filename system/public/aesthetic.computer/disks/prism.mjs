// prism, 26.07.12
// Self-running kaleidoscopic A/V loop: a UTC-synced A-minor arpeggio drives
// 8-fold mirrored geometry. Every audible note FIRES a spoke/petal the instant
// it sounds (angle ∝ step, hue ∝ pitch); the bass is a big central low bloom;
// the beat pulses the whole ring; high shimmer notes are small bright tips —
// so a viewer can "read" the arpeggio off the mirrored geometry.
//
// FOUR PRINCIPLES (marketing/av-reels/button-sync-allegory.md):
//   1. NO resolution() — renders at native res (vector, no offscreen buffer).
//   2. WHOLE PIECE IS A BUTTON — tap / XY-drag pumps a synced sonic+visual
//      boost in act(); repeated taps accumulate decaying `pump` energy that
//      swells the kaleidoscope; a burst ring blooms at the tap point, hue by X,
//      pitch by X/Y.
//   3. UTC-SYNCED RHYTHM — arpeggio + bass + hat onsets are derived from
//      clock.time() in sim (NOT beat()), indexed by floor(globalBeat) % len, so
//      two instances opened anywhere auto-align to the same wall-clock grid.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY — the visual IS the score.
//
// BPM ~132. Loops seamlessly (the score returns to its seam every 16 steps).

// --- Score ------------------------------------------------------------------
// One looping bar of an A-minor arpeggio (16 steps). Step 0 == the seam.
const ARP = [
  "a2", "e3", "a3", "c4", "e4", "c4", "a3", "e3",
  "a2", "g3", "b3", "d4", "g4", "d4", "b3", "g3",
];
const BASS = ["a1", "a1", "f1", "f1", "c2", "c2", "e1", "e1"]; // half-time root

// MIDI-ish pitch of each note, for pitch→hue / pitch→size mapping.
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(\d)$/.exec(n);
  if (!m) return 48;
  return (parseInt(m[2], 10) + 1) * 12 + NOTE_SEMI[m[1]];
}
// Map the whole arpeggio's pitch span to 0..1 for consistent color.
const ARP_PITCHES = ARP.map(notePitch);
const PITCH_MIN = Math.min(...ARP_PITCHES);
const PITCH_MAX = Math.max(...ARP_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN); // 0..1

const SYMMETRY = 8; // kaleidoscope fold count
const BPM = 132; // brisk, hypnotic
const BEAT_MS = 60000 / BPM; // ms per beat — the UTC grid unit

// --- Rhythm state (UTC-driven) ---------------------------------------------
let lastBeat = -1; // last integer global-beat we fired
let step = 0; // current arpeggio index (0..15), from UTC
let beatProgress = 0; // 0..1 within the current beat (smooth visuals)

// --- Allegory state ---------------------------------------------------------
// Each fired note spawns a decaying "spoke" flash so every audible onset has a
// visible birth exactly when it sounds. Kept tiny (a handful live at once).
let spokes = []; // { angle, hue, life, big, bright, thick }
let bassBloom = 0; // central low bloom, kicked by each bass note, decays
let ringPulse = 0; // whole-ring pulse, kicked on each beat, decays

// --- Button / pump state (§2) ----------------------------------------------
let pump = 0; // decaying global energy from taps, 0..~3
let bursts = []; // tap-spawned rings { x, y, r, life, hue }

function boot({ sound, clock }) {
  sound.bpm(BPM); // only affects the internal metronome; UTC scheduler is boss
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline)
  // Lush tail + a touch of grit for the "kaleidoscope hum".
  sound.room?.set?.({ enabled: true, mix: 0.32, feedback: 0.6 });
}

// Fire one step of the score. Called the instant a new global beat crosses in
// sim, so every onset is UTC-aligned and every note also spawns its visual.
function fireStep(idx, synth) {
  const s = ((idx % ARP.length) + ARP.length) % ARP.length;
  step = s;
  const note = ARP[s];
  const pn = pitchNorm(note); // 0 (low) .. 1 (high)

  // Pan sweeps across the stereo field with the arpeggio for width.
  const pan = Math.sin((s / ARP.length) * Math.PI * 2) * 0.7;
  synth({
    tone: note,
    type: "triangle",
    beats: 0.9,
    attack: 0.005,
    decay: 0.55,
    volume: 0.5,
    pan,
  });

  // ALLEGORY: this note FIRES a spoke — angle ∝ step, hue ∝ pitch,
  // high notes = small bright tips, low notes = fat warm spokes.
  const angle = (s / ARP.length) * Math.PI * 2;
  spokes.push({
    angle,
    hue: 20 + pn * 300, // full prism: low→red/amber, mid→green/cyan, high→magenta
    life: 1,
    big: 1 - pn, // low notes reach out fatter
    bright: 0.4 + pn * 0.6, // high notes tip brighter
    thick: 1 + Math.round((1 - pn) * 5),
  });

  // A brighter "shimmer" octave-up sparkle on high steps.
  if (pn > 0.6) {
    synth({
      tone: note,
      type: "sine",
      beats: 0.4,
      attack: 0.002,
      decay: 0.4,
      volume: 0.2,
      pan: -pan,
    });
  }

  // Sub-bass root on the half-beat pulse — the downbeat you can feel + SEE.
  if (s % 2 === 0) {
    const bi = (Math.floor(idx / 2) % BASS.length + BASS.length) % BASS.length;
    synth({
      tone: BASS[bi],
      type: "sawtooth",
      beats: 1.6,
      attack: 0.01,
      decay: 0.5,
      volume: 0.5,
    });
    bassBloom = 1; // ALLEGORY: bass = big central low bloom
  }

  // Closed-hat tick keeps momentum (noise → type, NOT tone).
  synth({ type: "noise-white", tone: 8000, beats: 0.12, attack: 0.001, decay: 0.2, volume: 0.16 });

  ringPulse = 1; // ALLEGORY: the beat = the whole 8-fold ring pulsing
}

// Drive audio + rhythm from UTC in sim (NOT beat()) so onsets align across
// instances. Also decay all the eased visual energies here.
function sim({ sound: { speaker, synth }, clock }) {
  speaker?.poll(); // mandatory before reading audio

  // clock.time() can return an *Invalid Date* before UTC sync completes (its
  // getTime() is NaN, and it's not null so `??` won't catch it) — guard with
  // Date.now() so the rhythm always runs, aligning across instances once synced.
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now();
  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx; // 0..1 through the current beat

  if (idx !== lastBeat) {
    lastBeat = idx;
    fireStep(idx, synth); // a new UTC beat crossed → fire its note
  }

  // Decays (per 120 Hz tick).
  pump *= 0.965;
  bassBloom *= 0.9;
  ringPulse *= 0.9;

  // Advance / cull tap bursts.
  for (const b of bursts) {
    b.r += 6 + b.life * 10;
    b.life -= 0.045;
  }
  bursts = bursts.filter((b) => b.life > 0);

  // Advance / cull note petals. Slow decay (~1.3s) so ~3 notes' petals overlap
  // into a lush, persistent flower instead of blinking in and out.
  for (const sp of spokes) sp.life -= 0.013;
  spokes = spokes.filter((sp) => sp.life > 0);
  if (spokes.length > 24) spokes = spokes.slice(-24); // hard safety cap
}

// THE WHOLE PIECE IS A BUTTON (§2): any tap / XY-drag pumps a synced sonic +
// visual boost at the touch point — hue by X, pitch by X/Y.
function act({ event: e, sound: { synth }, screen }) {
  if (e.is("touch") || e.is("draw")) {
    const x = e.x / screen.width; // 0..1
    const y = e.y / screen.height; // 0..1
    // Taps punch, drags feed — accumulate decaying pump energy.
    pump = Math.min(3, pump + (e.is("draw") ? 0.1 : 0.9));

    // Visual burst ring at the tap point, hue by X.
    bursts.push({ x: e.x, y: e.y, r: 0, life: 1, hue: x * 360 });

    // A spoke fires at the tapped angle too, so the tap reads in the geometry.
    spokes.push({
      angle: Math.atan2(e.y - screen.height / 2, e.x - screen.width / 2),
      hue: x * 360,
      life: 1.2,
      big: 1,
      bright: 1,
      thick: 4,
    });

    // SONIC BOOST — X→pitch/pan, Y→octave/brightness, in A-minor palette.
    const note =
      ["a", "c", "d", "e", "g"][Math.floor(x * 5)] + (2 + Math.floor((1 - y) * 4));
    synth({
      tone: note,
      type: "triangle",
      beats: 0.5,
      attack: 0.005,
      decay: 0.6,
      volume: 0.55,
      pan: x * 2 - 1,
    });
    synth({
      tone: note,
      type: "sine",
      beats: 0.25,
      attack: 0.002,
      decay: 0.3,
      volume: 0.3 * (1 - y), // brighter sparkle higher up
      pan: x * 2 - 1,
    });
  }
}

function paint({ ink, box, line, circle, shape, screen, sound, num, spin, zoom, blur }) {
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

  // Combined "energy" — beat clock + live audio + tap pump. Pump swells all.
  const energy = Math.min(
    2,
    ringPulse * 0.6 + bass * 1.2 + amp * 0.6 + pump * 0.5,
  );

  // --- Feedback base: recycle the previous frame into a slow kaleidoscopic
  // vortex. Gentle zoom + slow spin drift the recycled petals outward; a strong
  // veil (short trail) keeps the fresh 8-fold flower LEGIBLE (§4) rather than
  // smearing into spaghetti. Spin/zoom kick harder on downbeats and taps.
  zoom?.(1.01 + energy * 0.02, 0.5, 0.5); // gentle outward bloom on hits/taps
  spin?.(0.18 + ringPulse * 0.9 + pump * 0.6); // slow rotation, kicks on beats/taps
  ink(6, 4, 16, 140).box(0, 0, w, h); // deep-violet veil — short glow, crisp petals

  // --- Kaleidoscope geometry ------------------------------------------------
  const beatPhase = (step + beatProgress) % ARP.length; // bounded 0..16 (smooth)
  const maxR = Math.min(w, h) * 0.6;

  // BASS = a big soft central low bloom (§4) — behind everything else. Kept
  // dim/small so it doesn't wash out the colored petals in front of it.
  if (bassBloom > 0.01 || bass > 0.05) {
    const bloom = Math.max(bassBloom, bass * 1.2);
    const bR = Math.min(w, h) * (0.1 + bloom * 0.28);
    for (let i = 3; i > 0; i--) {
      ink(140, 40, 180, 16 * bloom).circle(cx, cy, bR * (i / 3), true);
    }
  }

  // NOTE PETALS (§4): every audible note FIRES a petal the instant it sounds —
  // angle ∝ step, hue ∝ pitch — mirrored SYMMETRY-fold into a kaleidoscopic
  // flower. Low notes = fat warm petals reaching far; high notes = short bright
  // tips. Each petal is a filled kite (not a thin line), so 8 folds read as
  // clean symmetry instead of pick-up-sticks. The feedback zoom+spin then
  // blooms the fired petals outward — the picture is literally built from the
  // notes as they play. (Newest few only, so the flower stays legible.)
  const innerR = maxR * 0.12;
  for (const sp of spokes.slice(-8)) {
    const reach = innerR + maxR * (0.42 + sp.big * 0.42) * (0.85 + energy * 0.3);
    const [r0, g0, b0] = num.hslToRgb(((sp.hue % 360) + 360) % 360, 100, 60 + sp.bright * 15);
    const a = 170 + 85 * sp.life; // stays bold and bright across its life
    const halfW = (0.16 + sp.big * 0.1) * (1 + energy * 0.15); // WIDE petal → reads as a shape
    const midR = innerR + (reach - innerR) * 0.42;
    const tipR = 3 + sp.bright * 9 + sp.life * 5;
    for (let f = 0; f < SYMMETRY; f++) {
      const foldOff = (f / SYMMETRY) * Math.PI * 2;
      for (const mirror of [1, -1]) {
        const ctr = mirror * sp.angle + foldOff;
        // Kite: base at innerR, two side points at midR ± halfW, tip at reach.
        const base = [cx + Math.cos(ctr) * innerR, cy + Math.sin(ctr) * innerR];
        const l = [cx + Math.cos(ctr - halfW) * midR, cy + Math.sin(ctr - halfW) * midR];
        const rgt = [cx + Math.cos(ctr + halfW) * midR, cy + Math.sin(ctr + halfW) * midR];
        const tip = [cx + Math.cos(ctr) * reach, cy + Math.sin(ctr) * reach];
        ink(r0, g0, b0, a).shape([base, l, tip, rgt]); // filled colored petal
        // A brighter inner core petal for depth.
        const l2 = [cx + Math.cos(ctr - halfW * 0.5) * midR, cy + Math.sin(ctr - halfW * 0.5) * midR];
        const r2 = [cx + Math.cos(ctr + halfW * 0.5) * midR, cy + Math.sin(ctr + halfW * 0.5) * midR];
        const tip2 = [cx + Math.cos(ctr) * (reach * 0.85), cy + Math.sin(ctr) * (reach * 0.85)];
        ink(Math.min(255, r0 + 60), Math.min(255, g0 + 60), Math.min(255, b0 + 60), a)
          .shape([base, l2, tip2, r2]);
        ink(255, 255, 255, 220 * sp.life).circle(tip[0], tip[1], tipR, true); // bright tip
      }
    }
  }

  // Central mandala core: two nested slow-rotating polygons — the steady hub
  // the spokes radiate from. Kept sparse (2 rings) so it reads, not tangles.
  const coreSides = SYMMETRY;
  for (let k = 1; k >= 0; k--) {
    const kr = maxR * (0.1 + k * 0.09) * (1 + energy * 0.5);
    const rot = (k % 2 === 0 ? 1 : -1) * beatPhase * 0.25;
    const hue = (beatPhase / ARP.length) * 360;
    const [cr, cg, cb] = num.hslToRgb((hue + k * 40) % 360, 90, 60);
    let px = null, py = null;
    for (let i = 0; i <= coreSides; i++) {
      const ang = (i / coreSides) * Math.PI * 2 + rot;
      const x = cx + Math.cos(ang) * kr, y = cy + Math.sin(ang) * kr;
      if (px !== null) ink(cr, cg, cb, 200).line(px, py, x, y, 2);
      px = x; py = y;
    }
  }

  // TAP BURSTS (§2): expanding rings at each tap point, hue by X.
  for (const b of bursts) {
    const [r0, g0, b0] = num.hslToRgb(((b.hue % 360) + 360) % 360, 95, 60);
    ink(r0, g0, b0, 200 * b.life).circle(b.x, b.y, b.r, false, 2 + b.life * 4);
    ink(255, 255, 255, 150 * b.life).circle(b.x, b.y, b.r * 0.5, false, 2);
    ink(r0, g0, b0, 130 * b.life).circle(b.x, b.y, 4 + b.life * 12, true);
  }

  // Soft blur blooms the whole kaleidoscope for an instant on strong hits/taps.
  if (ringPulse > 0.55 || pump > 1.2) blur?.(1);

  // Bright pulsing heart — the beat you can see. Small, so feedback zoom
  // doesn't bloom it into a blown-out disk.
  const heartR = 4 + ringPulse * 10 + bass * 12 + pump * 7;
  ink(180, 120, 255, 90 + ringPulse * 90).circle(cx, cy, heartR * 1.7, true);
  ink(255, 255, 255, 160 + ringPulse * 80).circle(cx, cy, heartR, true);
}

export { boot, sim, act, paint };
