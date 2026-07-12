// plax, 26.07.12
// Self-running glassy PRISM-SHARD instrument. A UTC-synced crystalline bell
// arpeggio scatters light through glass: every audible note FLASHES a triangular
// SHARD into existence the instant it sounds — the shard's ANGLE + vertical
// POSITION ∝ pitch, its HUE = the pitch's place in the spectrum (low = warm
// red/amber, high = violet), so the rising arpeggio literally fans out as a
// spray of colored light. A slow central rotating PRISM splits a persistent
// rainbow (the glassy pad). Accents fire a bright refraction burst. A gentle
// feedback trail leaves each shard a spectral streak on a near-black glass field
// — so a viewer READS the melody as the angle + color of the shards.
//
// FOUR PRINCIPLES (marketing/av-reels/button-sync-allegory.md):
//   1. NO resolution() — renders at native res (vector shards, no offscreen buf).
//   2. WHOLE PIECE IS A BUTTON — tap / XY-drag SHATTERS a burst of shards + a
//      glassy chime at the tap point (X → hue/pan, Y → pitch); repeated taps
//      accumulate decaying `pump` energy that intensifies the refraction.
//   3. UTC-SYNCED RHYTHM — arpeggio onsets are derived from clock.time() in sim
//      (NOT beat()), indexed by floor(globalBeat) % len, so two instances opened
//      anywhere auto-align to the same wall-clock grid. BPM ~136.
//   4. STRONG GRAPHIC↔SONIC ALLEGORY — the visual IS the score (notes = shards).
//
// Loops seamlessly: the score returns to its seam every 16 steps.

// --- Score ------------------------------------------------------------------
// A shimmering, icy arpeggio (16 steps). Step 0 == the seam. Rising fans of a
// D-major-ish glass scale so the spectrum sweeps warm→violet as pitch climbs.
const ARP = [
  "d3", "a3", "d4", "f#4", "a4", "f#4", "d4", "a3",
  "e3", "b3", "e4", "g4", "b4", "g4", "e4", "b3",
];
// A slow glassy pad root under it all — the prism's split rainbow hum.
const PAD = ["d2", "d2", "d2", "d2", "e2", "e2", "e2", "e2"];

// MIDI-ish pitch of each note, for pitch→hue / pitch→position / pitch→angle.
const NOTE_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function notePitch(n) {
  const m = /^([a-g])(#?)(\d)$/.exec(n);
  if (!m) return 48;
  const sharp = m[2] === "#" ? 1 : 0;
  return (parseInt(m[3], 10) + 1) * 12 + NOTE_SEMI[m[1]] + sharp;
}
const ARP_PITCHES = ARP.map(notePitch);
const PITCH_MIN = Math.min(...ARP_PITCHES);
const PITCH_MAX = Math.max(...ARP_PITCHES);
const pitchNorm = (n) =>
  (notePitch(n) - PITCH_MIN) / Math.max(1, PITCH_MAX - PITCH_MIN); // 0..1

// Spectral hue: low pitch = warm red/amber (~10°) → high pitch = violet (~280°).
const pitchHue = (pn) => 10 + pn * 270;

const BPM = 136; // brisk, icy, precise
const BEAT_MS = 60000 / BPM; // ms per beat — the UTC grid unit

// --- Rhythm state (UTC-driven) ---------------------------------------------
let lastBeat = -1; // last integer global-beat we fired
let step = 0; // current arpeggio index (0..15), from UTC
let beatProgress = 0; // 0..1 within the current beat (smooth visuals)

// --- Allegory state ---------------------------------------------------------
// Each fired note spawns a decaying SHARD (a triangular prism flash refracting a
// spectral streak) so every audible onset has a visible birth as it sounds.
let shards = []; // { angle, y, hue, life, big, bright, spin }
let refract = 0; // bright refraction burst energy (accents), decays
let padGlow = 0; // slow pad/rainbow-split glow, kicked by pad notes, decays

// --- Button / pump state (§2) ----------------------------------------------
let pump = 0; // decaying global energy from taps, 0..~3
let bursts = []; // tap-spawned shard-shatter bursts { x, y, hue, shards:[...] }

function boot({ sound, clock }) {
  sound.bpm(BPM); // only affects the internal metronome; UTC scheduler is boss
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline)
  // A cool glassy tail — light bouncing inside the crystal.
  sound.room?.set?.({ enabled: true, mix: 0.34, feedback: 0.5 });
}

// Spawn a shard for a note. angle ∝ step (fans across), y ∝ pitch (high=up),
// hue = spectral pitch place. Shared by the arpeggio and by taps.
function spawnShard(pn, angle, life, bright, big, screenH) {
  shards.push({
    angle, // radiating direction of the refracted streak
    y: (1 - pn) * screenH, // high pitch = high on screen
    hue: pitchHue(pn), // low→warm red, high→violet
    life,
    big,
    bright,
    spin: (Math.random() * 2 - 1) * 0.04, // faint glassy twist
  });
}

// Fire one step of the score. Called the instant a new global beat crosses in
// sim, so every onset is UTC-aligned and every note also spawns its shard.
function fireStep(idx, synth, screenH) {
  const s = ((idx % ARP.length) + ARP.length) % ARP.length;
  step = s;
  const note = ARP[s];
  const pn = pitchNorm(note); // 0 (low) .. 1 (high)

  // Pan fans with the arpeggio so the light sprays across the stereo field.
  const pan = Math.sin((s / ARP.length) * Math.PI * 2) * 0.75;

  // Crystalline bell: fast attack, shimmering. Sine core + triangle overtone.
  synth({
    tone: note,
    type: "sine",
    beats: 0.9,
    attack: 0.003,
    decay: 0.55,
    volume: 0.42,
    pan,
  });
  synth({
    tone: note,
    type: "triangle",
    beats: 0.5,
    attack: 0.002,
    decay: 0.4,
    volume: 0.22 * (0.5 + pn * 0.5), // brighter overtone climbs with pitch
    pan,
  });

  // ALLEGORY: this note FLASHES a shard — angle ∝ step, y ∝ pitch, hue ∝ pitch.
  const angle = (s / ARP.length) * Math.PI * 2 - Math.PI / 2; // fan from top
  spawnShard(pn, angle, 1, 0.4 + pn * 0.6, 1 - pn * 0.5, screenH);

  // A delicate HIGH SPARKLE on the accent (top of each rising run).
  const accent = pn > 0.72;
  if (accent) {
    synth({
      tone: note,
      type: "sine",
      beats: 0.3,
      attack: 0.001,
      decay: 0.35,
      volume: 0.16,
      pan: -pan,
    });
    refract = 1; // ALLEGORY: accent = a bright refraction burst
  }

  // The glassy pad root on the half-beat — the slow prism-split rainbow hum.
  if (s % 2 === 0) {
    const bi = (Math.floor(idx / 2) % PAD.length + PAD.length) % PAD.length;
    synth({
      tone: PAD[bi],
      type: "triangle",
      beats: 1.7,
      attack: 0.06, // soft glassy swell
      decay: 0.5,
      volume: 0.3,
    });
    padGlow = 1; // ALLEGORY: pad = the central prism's rainbow glow
  }
}

// Drive audio + rhythm from UTC in sim (NOT beat()) so onsets align across
// instances. Also decay all the eased visual energies here.
function sim({ sound: { speaker, synth }, clock, screen }) {
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
    fireStep(idx, synth, screen?.height || 640); // new UTC beat → fire its note
  }

  // Decays (per 120 Hz tick).
  pump *= 0.965;
  refract *= 0.9;
  padGlow *= 0.94;

  // Advance / cull note shards. Slow decay (~1.2s) so several shards overlap
  // into a lush fan of refracted light instead of blinking.
  for (const sh of shards) {
    sh.life -= 0.014;
    sh.angle += sh.spin; // faint glassy twist as it fades
  }
  shards = shards.filter((sh) => sh.life > 0);
  if (shards.length > 28) shards = shards.slice(-28); // safety cap

  // Advance / cull tap shatter bursts (each carries its own little shard spray).
  for (const b of bursts) {
    for (const p of b.shards) {
      p.x += p.vx;
      p.y += p.vy;
      p.vx *= 0.96;
      p.vy *= 0.96;
      p.life -= 0.03;
      p.rot += p.spin;
    }
    b.shards = b.shards.filter((p) => p.life > 0);
  }
  bursts = bursts.filter((b) => b.shards.length > 0);
}

// THE WHOLE PIECE IS A BUTTON (§2): any tap / XY-drag SHATTERS a burst of shards
// + a glassy chime at the touch point — hue by X, pitch by X/Y.
function act({ event: e, sound: { synth }, screen }) {
  if (e.is("touch") || e.is("draw")) {
    const x = e.x / screen.width; // 0..1
    const y = e.y / screen.height; // 0..1
    const pn = 1 - y; // top of screen = high pitch (matches y-mapping)
    const hue = pitchHue(pn);

    // Taps punch, drags feed — accumulate decaying pump energy → more refraction.
    pump = Math.min(3, pump + (e.is("draw") ? 0.1 : 0.9));

    // SHATTER: spray a burst of little glass shards flying out from the tap.
    const spray = [];
    const count = e.is("draw") ? 4 : 10;
    for (let i = 0; i < count; i++) {
      const a = (i / count) * Math.PI * 2 + Math.random() * 0.6;
      const spd = 2 + Math.random() * (e.is("draw") ? 3 : 7);
      spray.push({
        x: e.x,
        y: e.y,
        vx: Math.cos(a) * spd,
        vy: Math.sin(a) * spd,
        rot: Math.random() * Math.PI,
        spin: (Math.random() * 2 - 1) * 0.3,
        size: 4 + Math.random() * (10 + pump * 6),
        hue: hue + (Math.random() * 2 - 1) * 40, // spectral spread of the shatter
        life: 1,
      });
    }
    bursts.push({ x: e.x, y: e.y, hue, shards: spray });

    // Also flash a big directional shard in the note geometry so the tap reads.
    spawnShard(pn, Math.atan2(e.y - screen.height / 2, e.x - screen.width / 2),
      1.2, 1, 1, screen.height);
    refract = Math.max(refract, 0.8);

    // GLASSY CHIME — X→pitch/pan, Y→octave/brightness, crystalline bell.
    const note =
      ["d", "e", "f#", "a", "b"][Math.floor(x * 5)] + (3 + Math.floor((1 - y) * 3));
    synth({
      tone: note,
      type: "sine",
      beats: 0.5,
      attack: 0.002,
      decay: 0.6,
      volume: 0.5,
      pan: x * 2 - 1,
    });
    synth({
      tone: note,
      type: "triangle",
      beats: 0.25,
      attack: 0.001,
      decay: 0.3,
      volume: 0.28 * (1 - y), // icier sparkle higher up
      pan: x * 2 - 1,
    });
  }
}

// Draw a single triangular glass shard: a thin refracting sliver + a bright tip,
// tinted by its spectral hue. Additive-feeling translucent layering.
function drawShard(ink, num, cx, cy, angle, reach, halfW, hue, alpha, tipR, life) {
  const [r0, g0, b0] = num.hslToRgb(((hue % 360) + 360) % 360, 100, 58);
  const base = [cx, cy];
  const midR = reach * 0.5;
  const l = [cx + Math.cos(angle - halfW) * midR, cy + Math.sin(angle - halfW) * midR];
  const rgt = [cx + Math.cos(angle + halfW) * midR, cy + Math.sin(angle + halfW) * midR];
  const tip = [cx + Math.cos(angle) * reach, cy + Math.sin(angle) * reach];
  // Outer refracting shard (translucent colored glass).
  ink(r0, g0, b0, alpha).shape([base, l, tip, rgt]);
  // Inner brighter core streak (the light passing through).
  const l2 = [cx + Math.cos(angle - halfW * 0.4) * midR, cy + Math.sin(angle - halfW * 0.4) * midR];
  const r2 = [cx + Math.cos(angle + halfW * 0.4) * midR, cy + Math.sin(angle + halfW * 0.4) * midR];
  const tip2 = [cx + Math.cos(angle) * (reach * 0.9), cy + Math.sin(angle) * (reach * 0.9)];
  ink(Math.min(255, r0 + 80), Math.min(255, g0 + 80), Math.min(255, b0 + 80), alpha)
    .shape([base, l2, tip2, r2]);
  // Bright refracting tip — a spark of pure light where the glass focuses it.
  ink(255, 255, 255, 220 * life).circle(tip[0], tip[1], tipR, true);
}

function paint({ ink, box, line, circle, shape, screen, sound, num, zoom, spin, blur }) {
  const { width: w, height: h } = screen;
  const cx = w / 2, cy = h / 2;

  // --- Live audio reads -----------------------------------------------------
  const bands = sound.speaker?.frequencies?.left || [];
  const bandAmp = (name) => bands.find((b) => b.name === name)?.amplitude || 0;
  const lowMid = bandAmp("lowMid");
  const presence = bandAmp("presence");
  const air = bandAmp("air");
  const amp = sound.speaker?.amplitudes?.left || 0;

  // Combined "energy" — refraction accents + live audio + tap pump.
  const energy = Math.min(
    2,
    refract * 0.6 + presence * 0.9 + air * 0.9 + amp * 0.5 + pump * 0.5,
  );

  // --- Glass field: near-black background so spectral colors POP. A gentle
  // feedback drift (slow zoom out + faint spin) smears each shard into a
  // spectral STREAK; a strong veil keeps the fresh fan of shards LEGIBLE (§4)
  // rather than washing into mush. Refraction/taps kick the drift harder.
  zoom?.(0.992 - energy * 0.006, 0.5, 0.42); // gentle inward drift → light streaks toward center
  spin?.(0.06 + refract * 0.5 + pump * 0.4); // faint glassy twist, kicks on accents/taps
  ink(3, 4, 10, 150).box(0, 0, w, h); // deep near-black glass veil — short trails, crisp shards

  const maxR = Math.min(w, h) * 0.62;

  // --- Central rotating PRISM (the pad): a slow triangular crystal that SPLITS
  // a persistent rainbow. Its glow swells with the pad note (§4). Behind shards.
  const prismSpin = (step + beatProgress) * 0.12;
  const prismR = maxR * (0.16 + padGlow * 0.06 + lowMid * 0.4);
  // The split rainbow: faint radiating spectral spokes fanning from the prism.
  const spokes = 12;
  for (let i = 0; i < spokes; i++) {
    const a = (i / spokes) * Math.PI * 2 + prismSpin;
    const hue = (i / spokes) * 360;
    const [rr, gg, bb] = num.hslToRgb(hue, 90, 55);
    const reach = prismR + maxR * (0.5 + padGlow * 0.3);
    ink(rr, gg, bb, 40 + padGlow * 55).line(
      cx, cy,
      cx + Math.cos(a) * reach, cy + Math.sin(a) * reach,
      1 + (padGlow > 0.4 ? 1 : 0),
    );
  }
  // The prism body: a small rotating triangle, faintly rainbow-edged.
  for (let k = 0; k < 2; k++) {
    const rot = prismSpin * (k === 0 ? 1 : -1.4);
    const kr = prismR * (0.7 + k * 0.35);
    let px = null, py = null, first = null;
    for (let i = 0; i <= 3; i++) {
      const a = (i / 3) * Math.PI * 2 + rot;
      const x = cx + Math.cos(a) * kr, y = cy + Math.sin(a) * kr;
      if (i === 0) first = [x, y];
      const [rr, gg, bb] = num.hslToRgb((i / 3) * 360 + step * 20, 95, 62);
      if (px !== null) ink(rr, gg, bb, 150 + padGlow * 60).line(px, py, x, y, 2);
      px = x; py = y;
    }
    if (first) ink(255, 255, 255, 120).line(px, py, first[0], first[1], 2);
  }

  // --- NOTE SHARDS (§4): every audible note FLASHED a shard the instant it
  // sounded — angle ∝ step (fans across), y ∝ pitch (high notes up), hue ∝
  // pitch (low=warm red → high=violet). The rising arpeggio scatters as a fan of
  // colored light. Newest few only, so the fan stays legible.
  for (const sh of shards.slice(-14)) {
    // Origin point: shards emit from a point set by pitch height, near the prism.
    const oy = num.lerp(cy, sh.y, 0.6);
    const ox = cx;
    const reach = maxR * (0.4 + sh.big * 0.4) * (0.8 + energy * 0.35) * (0.6 + sh.life * 0.4);
    const halfW = (0.1 + sh.big * 0.09) * (1 + energy * 0.2); // fatter jewel-like shard
    const alpha = 150 + 90 * sh.life;
    const tipR = 2 + sh.bright * 7 + sh.life * 4;
    drawShard(ink, num, ox, oy, sh.angle, reach, halfW, sh.hue, alpha, tipR, sh.life);
    // A faint mirrored twin on the opposite side → the prism's split symmetry.
    drawShard(ink, num, ox, oy, sh.angle + Math.PI, reach * 0.5, halfW,
      sh.hue + 30, alpha * 0.45, tipR * 0.5, sh.life * 0.7);
  }

  // --- TAP SHATTER BURSTS (§2): little glass shards flying out from each tap,
  // spectrally spread, tumbling as they fade.
  for (const b of bursts) {
    for (const p of b.shards) {
      const [r0, g0, b0] = num.hslToRgb(((p.hue % 360) + 360) % 360, 95, 60);
      const s = p.size * (0.4 + p.life * 0.6);
      // A tiny tumbling triangular shard.
      const pts = [];
      for (let i = 0; i < 3; i++) {
        const a = p.rot + (i / 3) * Math.PI * 2;
        pts.push([p.x + Math.cos(a) * s, p.y + Math.sin(a) * s]);
      }
      ink(r0, g0, b0, 210 * p.life).shape(pts);
      ink(255, 255, 255, 180 * p.life).circle(p.x, p.y, 1 + p.life * 2, true);
    }
  }

  // Refraction burst flash: a bright white bloom on accents/taps blooms the
  // whole glass field for an instant.
  if (refract > 0.5 || pump > 1.2) {
    const bloom = Math.max(refract, pump * 0.6);
    for (let i = 3; i > 0; i--) {
      ink(220, 235, 255, 22 * bloom).circle(cx, cy, prismR * (1 + i * 0.5), true);
    }
    blur?.(1);
  }

  // Bright refracting heart at the prism core — the pulse you can see. Small so
  // the feedback drift doesn't bloom it into a blown-out disk.
  const heartR = 3 + refract * 8 + padGlow * 6 + pump * 6;
  ink(180, 210, 255, 90 + padGlow * 90).circle(cx, cy, heartR * 1.7, true);
  ink(255, 255, 255, 170 + refract * 80).circle(cx, cy, heartR, true);
}

export { boot, sim, act, paint };
