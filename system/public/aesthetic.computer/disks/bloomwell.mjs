// bloomwell, 26.07.12
// Self-running SUPERSAMPLED FEEDBACK MANDALA — a breathing kaleidoscope well.
//
// FOUR PRINCIPLES (marketing/av-reels/button-sync-allegory.md):
//  1. NO resolution() — renders at native res. SSAA is done by drawing the seed
//     motif into an offscreen buffer at 2× and pasting it back downscaled.
//  2. WHOLE PIECE IS A BUTTON — any tap / XY-drag injects a fresh petal-ring that
//     blooms outward through the feedback + a chime (X→hue/pan, Y→pitch). Repeated
//     taps accumulate a decaying `pump` that intensifies the zoom/bloom.
//  3. UTC-SYNCED RHYTHM — the arpeggio + bass onsets are driven from clock.time()
//     in sim (NOT beat()), indexed by floor(globalBeat)%len, so two instances
//     opened anywhere auto-align to the same wall-clock grid. Loops seamlessly.
//  4. STRONG GRAPHIC<->SONIC ALLEGORY — each arpeggio note lights a petal-node
//     (angle + hue from pitch) the instant it sounds; the bass IS the zoom/bloom
//     breath (low root = a deeper inhale); every beat spawns a fresh motif ring
//     that blooms outward through the feedback. Read the arps as lighting petals,
//     the bass as the breathing bloom.

// --- UTC beat grid ----------------------------------------------------------
const BPM = 126;
const BEAT_MS = 60000 / BPM; // ms per beat

// One looping bar (16 steps) of a D-minor arpeggio. Step 0 == the seam.
const ARP = [
  "d2", "a2", "d3", "f3", "a3", "f3", "d3", "a2",
  "d2", "c3", "e3", "g3", "c4", "g3", "e3", "c3",
];
const BASS = ["d1", "d1", "bb1", "bb1", "g1", "g1", "a1", "a1"]; // half-time root

// Pitch->normalized-height table so a note maps to a screen angle + hue.
// Low = deep/warm, high = bright. Spans the arpeggio's range (d1..c4).
const PITCH_LO = 24; // d1 midi-ish anchor
const PITCH_HI = 60; // c4
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function noteToNorm(n) {
  const m = /^([a-g])(b|#)?(\d)$/.exec(n);
  if (!m) return 0.5;
  let semi = NOTE_SEMI[m[1]];
  if (m[2] === "b") semi -= 1;
  if (m[2] === "#") semi += 1;
  const midi = (parseInt(m[3], 10) + 1) * 12 + semi;
  return Math.max(0, Math.min(1, (midi - PITCH_LO) / (PITCH_HI - PITCH_LO)));
}

let lastBeat = -1; // integer UTC beat we last fired
let beatProgress = 0; // 0..1 within the current beat (smooth visuals)
let globalStep = 0; // absolute step index (for pan/hue continuity)
let bassStep = 0; // half-time bass index

let pulse = 0; // eased arp note-on flash, decays toward 0
let bassBreath = 0; // eased bass inhale, decays — drives the bloom depth
let pump = 0; // decaying tap energy, 0..~3 — intensifies zoom/bloom

let seedBuf = null; // reusable offscreen 2× seed buffer

// Lit petal-nodes: each audible arp note lights one at its pitch angle/hue.
let petals = []; // { norm, hue, pan, life } — life 1->0
// Injected motif rings from taps + beats: bloom outward through feedback.
let rings = []; // { x, y, r, life, hue, spokes }

function boot({ sound, clock }) {
  sound.bpm(BPM); // metronome only — UTC scheduler below is authoritative
  sound.room?.set?.({ enabled: true, mix: 0.34, feedback: 0.62 }); // lush hum
  clock?.resync?.(); // fetch UTC offset (local fallback offline — fine)
}

// Fire an arpeggio note + its bass companion, and light the matching petal.
function fireBeat(idx, synth, screen) {
  const step = ((idx % ARP.length) + ARP.length) % ARP.length;
  const note = ARP[step];
  const norm = noteToNorm(note); // 0..1 pitch height
  const pan = Math.sin((step / ARP.length) * Math.PI * 2) * 0.7;
  const hue = (norm * 260 + step * 6) % 360; // pitch -> hue

  synth({
    tone: note,
    type: "triangle",
    beats: 0.9,
    attack: 0.004,
    decay: 0.55,
    volume: 0.55,
    pan,
  });

  // Octave-up sine shimmer every 4th step (brighter high sparkle).
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

  // Sub-bass root every other step — the breath you can feel + see.
  if (step % 2 === 0) {
    const broot = BASS[bassStep % BASS.length];
    synth({
      tone: broot,
      type: "sawtooth",
      beats: 1.6,
      attack: 0.01,
      decay: 0.5,
      volume: 0.55,
    });
    bassStep = (bassStep + 1) % BASS.length;
    // Deeper root = deeper inhale (the bloom breathes on the bass).
    bassBreath = Math.max(bassBreath, 0.7 + (1 - noteToNorm(broot)) * 0.6);
  }

  // Closed-hat tick keeps momentum — noise goes in TYPE, never tone.
  synth({ type: "noise-white", tone: 900, beats: 0.12, attack: 0.001, decay: 0.2, volume: 0.18 });

  // ALLEGORY: light the petal whose angle/hue == this note's pitch.
  petals.push({ norm, hue, pan, life: 1 });
  if (petals.length > 24) petals.shift();
  pulse = 1; // fresh note-on flash

  // ALLEGORY: every beat spawns a fresh motif ring that blooms outward.
  const cx = screen.width / 2, cy = screen.height / 2;
  rings.push({ x: cx, y: cy, r: 6, life: 1, hue, spokes: 6 });
  if (rings.length > 20) rings.shift();

  globalStep = idx;
}

// THE BUTTON: any tap / XY-drag injects a fresh petal-ring + a chime, and feeds
// the decaying `pump` that intensifies the whole bloom.
function act({ event: e, sound: { synth }, screen }) {
  if (e.is("touch") || e.is("draw")) {
    const x = Math.max(0, Math.min(1, e.x / screen.width)); // 0..1
    const y = Math.max(0, Math.min(1, e.y / screen.height)); // 0..1
    // Taps punch, drags feed. Accumulate decaying energy.
    pump = Math.min(3, pump + (e.is("draw") ? 0.14 : 0.95));

    const hue = x * 360; // X -> hue/pan
    const norm = 1 - y; // Y -> pitch height (top = high)

    // Inject a fresh motif ring blooming from the exact tap spot.
    rings.push({ x: e.x, y: e.y, r: 4, life: 1.2, hue, spokes: 6 });
    if (rings.length > 24) rings.shift();
    // And a lit petal at that pitch/hue.
    petals.push({ norm, hue, pan: x * 2 - 1, life: 1.2 });
    if (petals.length > 28) petals.shift();
    pulse = Math.max(pulse, 1);

    // SONIC BOOST — X->pitch(scale)+pan, Y->octave. Fits the D-minor palette.
    const scale = ["d", "e", "f", "g", "a", "c"];
    const deg = scale[Math.floor(x * (scale.length - 0.001))];
    const oct = 2 + Math.floor((1 - y) * 3); // top = higher octave
    const tone = deg + oct;
    synth({ tone, type: "triangle", beats: 0.5, attack: 0.004, decay: 0.6,
      volume: 0.6, pan: x * 2 - 1 });
    synth({ tone, type: "sine", beats: 0.25, attack: 0.002, decay: 0.35,
      volume: 0.28 * (1 - y), pan: x * 2 - 1 }); // brighter sparkle higher up
  }
}

// UTC-driven scheduler + physics. Poll audio, advance the beat grid from the
// network-synced wall clock, ease the flashes, decay the pump.
function sim({ sound: { speaker, synth }, screen, clock }) {
  speaker?.poll(); // mandatory before audio reads

  // UTC-synced wall clock; fall back to local time if the clock is unsynced /
  // returns an Invalid Date (e.g. the offline local capture server).
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now();
  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx; // 0..1

  if (idx !== lastBeat) {
    if (lastBeat !== -1) fireBeat(idx, synth, screen); // skip the very first (align only)
    lastBeat = idx;
  }

  // Ease flashes & breath down between onsets.
  pulse *= 0.9;
  bassBreath *= 0.94;
  pump *= 0.965; // decaying tap energy

  // Advance lit petals (fade out).
  for (const p of petals) p.life -= 0.045;
  petals = petals.filter((p) => p.life > 0);

  // Advance motif rings — bloom outward + fade.
  const maxR = Math.max(screen.width, screen.height) * 0.75;
  for (const r of rings) {
    r.r += 3.2 + pump * 2.4 + (1 - r.life) * 2; // grow, faster under pump
    r.life -= 0.028;
    if (r.r > maxR) r.life = 0;
  }
  rings = rings.filter((r) => r.life > 0);
}

function paint({
  ink, box, line, circle, screen, sound, num, paintCount,
  spin, zoom, blur, paste, painting,
}) {
  const { width: w, height: h } = screen;
  const cx = w / 2, cy = h / 2;

  // --- Live audio reads -----------------------------------------------------
  const bands = sound.speaker?.frequencies?.left || [];
  const bandAmp = (name) => bands.find((b) => b.name === name)?.amplitude || 0;
  const bass = bandAmp("subBass");
  const mid = bandAmp("mid");
  const air = bandAmp("air");
  const amp = sound.speaker?.amplitudes?.left || 0;

  const downbeat = pulse; // 1 at each note-on, decays
  // ALLEGORY: the bass IS the inhale. Low root -> deeper zoom-bloom breath.
  const breath = Math.min(1.4, bassBreath + bass * 1.4);
  const energy = Math.min(1.6, downbeat * 0.5 + breath * 0.8 + amp * 0.5 + pump * 0.5);

  // --- Seamless phase (wraps every 480 frames → frame 0 == frame N) ---------
  const phase = (Number(paintCount) % 480) / 480; // paintCount is a Number
  const spinAng = phase * Math.PI * 2; // one full turn per loop
  const baseHue = (phase * 360 + globalStep * 22) % 360;

  // --- TRANSFORM-FEEDBACK: recycle the whole previous frame into a vortex ----
  // The bass drives the zoom depth: a low root inhales the whole field harder,
  // so the bloom visibly BREATHES with the bass. pump (taps) intensifies it.
  zoom?.(1.008 + breath * 0.016 + pump * 0.012, 0.5, 0.5); // gentle outward bloom = the breath
  spin?.(0.5 + downbeat * 1.1 + pump * 0.6); // rotation kicks on note-on + taps
  ink(6, 3, 16, 40).box(0, 0, w, h); // deep-indigo veil → glowing trails, decays fog

  // --- SUPERSAMPLED SEED MANDALA (rendered at 2×, downscaled → SSAA edges) ---
  // A small radially-symmetric mandala is drawn into an offscreen buffer at 2×
  // then pasted back at 1/2 → smooth anti-aliased edges. It's rendered CENTERED
  // on a small transparent buffer so, as the whole frame zooms out each frame,
  // successive seeds spiral outward into an infinite vortex. Nodes are drawn
  // OPAQUE + SPARSE so each hue stays pure (translucent multi-hue overlap greys
  // out under the feedback loop). The LIT PETALS from the arpeggio sit at their
  // pitch angle/hue so each arp note reads as a petal lighting up as it sounds.
  const S = 2; // supersample factor
  const FOLDS = 6; // radial symmetry
  const seedSize = Math.floor(Math.min(w, h) * 0.46);
  const bw = seedSize * S, bh = seedSize * S;
  seedBuf = painting(bw, bh, (p) => {
    const scx = bw / 2, scy = bh / 2;
    const maxR = (bw / 2) * 0.94;

    // Base structural mandala — a SPARSE ring of OPAQUE nodes (outer + inner).
    for (let f = 0; f < FOLDS; f++) {
      const fold = (f / FOLDS) * Math.PI * 2 + spinAng * 0.5;
      for (let ringN = 0; ringN < 2; ringN++) {
        const rr = ringN === 0 ? 0.92 : 0.56;
        const swirl = num.wave(phase * 6.28 + f + ringN * 0.9) * 0.3;
        const ang = fold + swirl;
        const radius = maxR * rr;
        const tx = scx + Math.cos(ang) * radius;
        const ty = scy + Math.sin(ang) * radius;
        const hue = (baseHue + f * (360 / FOLDS) + ringN * 40) % 360;
        const [r, g, b] = num.hslToRgb(hue, 96, 56); // vivid, opaque
        const nodeR = (5 + (1 - rr) * 4 + energy * 4 + air * 5) * S;
        p.ink(r, g, b, 255).circle(tx, ty, nodeR, true);
        const [lr, lg, lb] = num.hslToRgb((hue + 30) % 360, 94, 62);
        p.ink(lr, lg, lb, 230).line(scx, scy, tx, ty, (1.5 + mid * 2) * S);
      }
    }

    // ALLEGORY: LIT PETALS — one per recent arp note, at its pitch angle/hue.
    // Bright + OPAQUE the instant it sounds (life ~1), fading as the note dies.
    for (const pet of petals) {
      const life = Math.max(0, Math.min(1.2, pet.life));
      const ringR = maxR * (0.3 + pet.norm * 0.6); // pitch -> radial distance
      const petalAng = pet.norm * Math.PI * 2 + spinAng * 0.5; // pitch -> angle
      const [r, g, b] = num.hslToRgb(pet.hue % 360, 98, 60); // vivid
      for (let f = 0; f < FOLDS; f++) { // mirror across folds → symmetric ring
        const a = petalAng + (f / FOLDS) * Math.PI * 2;
        const tx = scx + Math.cos(a) * ringR;
        const ty = scy + Math.sin(a) * ringR;
        const glow = (6 + life * 14 + air * 8) * S;
        p.ink(r, g, b, 255).circle(tx, ty, glow, true);
        p.ink(r, g, b, Math.min(255, 130 + life * 125)).line(scx, scy, tx, ty, (1 + life * 2) * S);
      }
    }

    // Pulsing heart — the beat you can see; swells on the bass breath. Kept
    // SATURATED + opaque (not near-white) so feedback doesn't wash to gray.
    const [hr, hg, hb] = num.hslToRgb((baseHue + 180) % 360, 92, 60);
    p.ink(hr, hg, hb, 255)
      .circle(scx, scy, (4 + downbeat * 8 + breath * 12) * S, true);
  });
  // Downsample-composite (poor-man's SSAA) centered on screen.
  paste(seedBuf, Math.round(cx - seedSize / 2), Math.round(cy - seedSize / 2), 1 / S);

  // --- MOTIF RINGS: fresh blooms from every beat + every tap ----------------
  // Drawn at native res on top so tap-spawned rings visibly bloom outward from
  // the touch point and feed the feedback vortex (they get zoomed/spun next frame).
  for (const r of rings) {
    const life = Math.max(0, Math.min(1.2, r.life));
    const [rr, gg, bb] = num.hslToRgb(r.hue % 360, 92, 58);
    const a = Math.round(30 + life * 160);
    ink(rr, gg, bb, a).circle(r.x, r.y, r.r, false, 1 + life * 3);
    // A few petal spokes so a ring reads as a motif, not just a circle.
    for (let s = 0; s < r.spokes; s++) {
      const ang = (s / r.spokes) * Math.PI * 2 + spinAng;
      const px = r.x + Math.cos(ang) * r.r;
      const py = r.y + Math.sin(ang) * r.r;
      ink(rr, gg, bb, Math.round(a * 0.7)).circle(px, py, 2 + life * 4, true);
    }
  }

  // A soft bloom only on big taps — kept rare so the feedback field stays
  // colorful (blur averages toward gray if run every beat).
  if (pump > 1.6) blur?.(1);
}

export { boot, act, sim, paint };
