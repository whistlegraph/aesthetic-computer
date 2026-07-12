// gloob, 26.07.12
// Gooey metaball-voices instrument: each blob is a VOICE; blobs that sound at the
// same time SWELL, brighten (hue = pitch) and physically MERGE into one gooey mass
// — you SEE the chord as goo gluing together, then splitting as notes release. A
// warm squelchy "gloob" bass sets the field viscosity; a soft kick jiggles the whole
// goo with a shockwave. Whole screen is a button: tap/drag spawns a hot new blob-voice
// (X→pan/hue, Y→pitch). Rhythm is UTC-synced (clock.time() in sim) so two instances
// glob in lockstep. Native-res per-pixel field written directly to screen.pixels.

// ── UTC-synced rhythm ────────────────────────────────────────────────────────
const BPM = 104;
const BEAT_MS = 60000 / BPM; // ms per beat

// One warm gooey loop. Each entry is a note; its index picks which resident blob
// voice sounds. Chords happen when consecutive beats reuse/overlap voices → they
// merge. Bass anchors even beats; a soft kick fires on the downbeat of each bar.
// notes in a low-mid warm register; keeps the "deep, squishy, cute" identity.
const PATTERN = [
  "c2", "g2", "e3", "g2", // bar 1 — bass + a mid bloop
  "a2", "e3", "c3", "g3", // bar 2 — voices climb, some overlap
  "f2", "c3", "a3", "c3", // bar 3
  "g2", "d3", "g3", "e3", // bar 4
];
const BAR = 4; // beats per bar (kick lands on step % BAR === 0)

// ── Resident blob voices ─────────────────────────────────────────────────────
// 6 gooey charges that drift; each is a "voice" that swells + glows when it sounds.
const VOICE_COUNT = 6;
let voices = []; // { hx, hy (0..1 home), x, y, vx, vy, base, charge (0..1 loud), hue }

// Taps spawn extra transient blobs that decay away.
let sparks = []; // { x, y, vx, vy, charge, life, hue, note }

let pump = 0; // decaying global energy from taps (0..~3) → heat/thicken the goo
let bass = 0; // eased low-end level → field viscosity/gloss
let kick = 0; // shockwave envelope (0..1), jiggles the whole goo
let kickWave = 0; // expanding radius of the current shockwave (0..1)

let lastBeat = -1;
let beatProgress = 0; // 0..1 within current beat
let simMs = 0;
let bootedMs = 0;

function boot({ clock, sound, screen, num }) {
  clock?.resync?.(); // fetch UTC offset (silent no-op offline → local fallback)
  sound?.bpm?.(BPM);
  bootedMs = Date.now();
  const cx = 0.5;
  voices = [];
  for (let i = 0; i < VOICE_COUNT; i++) {
    const a = (i / VOICE_COUNT) * Math.PI * 2;
    const hx = cx + Math.cos(a) * 0.26;
    const hy = 0.5 + Math.sin(a) * 0.3;
    voices.push({
      hx, hy,
      x: hx, y: hy,
      vx: 0, vy: 0,
      base: 0.5 + (i % 3) * 0.18, // resting blob size
      charge: 0,
      hue: 320 - (i / VOICE_COUNT) * 150, // spread along the magenta→teal candy ramp
    });
  }
}

// ── Audio + physics on the UTC grid (NOT beat()) ─────────────────────────────
function onBeat(step, idx, synth) {
  const note = PATTERN[step];
  const voice = voices[step % VOICE_COUNT];
  if (!voice) return;

  // Hue tracks pitch across the WHOLE candy wheel so voices read as distinct colors.
  // Map the chromatic pitch class + octave to a hue: low = magenta/red, high = cyan/blue.
  const oct = parseInt(note.slice(-1), 10) || 3;
  const names = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
  const pc = names[note[0]] ?? 0;
  const midi = (oct + 1) * 12 + pc; // ~24..60 here
  const tt = Math.max(0, Math.min(1, (midi - 24) / (60 - 24)));
  const hue = ((320 + tt * (200 - 320)) + 360) % 360; // magenta(low)→teal(high) ramp
  voice.hue = hue;
  voice.charge = Math.min(1.6, voice.charge + 1.0); // SWELL + glow now
  voice.note = note;

  const isBass = oct <= 2;
  const boost = 1 + Math.min(1, pump * 0.35);

  // The gloob voice: sine core + soft triangle body → wet, rounded, squelchy.
  synth({
    tone: note,
    type: "sine",
    beats: isBass ? 1.9 : 1.2,
    attack: 0.02,
    decay: isBass ? 0.5 : 0.62,
    volume: (isBass ? 0.6 : 0.42) * boost,
    pan: (voice.x - 0.5) * 1.4,
  });
  synth({
    tone: note,
    type: "triangle",
    beats: 0.55,
    attack: 0.01,
    decay: 0.4,
    volume: (isBass ? 0.22 : 0.3) * boost,
    pan: (voice.x - 0.5) * 1.4,
  });

  if (isBass) bass = Math.min(1.4, bass + 0.7); // thicken/gloss the whole field

  // Soft kick on each bar downbeat → shockwave that jiggles the goo.
  if (step % BAR === 0) {
    kick = 1;
    kickWave = 0;
    synth({ tone: "c1", type: "sine", beats: 0.5, attack: 0.005, decay: 0.28, volume: 0.7 * boost });
    synth({ type: "noise-white", tone: 200, beats: 0.08, attack: 0.001, decay: 0.2, volume: 0.14 * boost });
    // shove every blob outward a touch for a squishy jiggle
    for (const v of voices) {
      v.vx += (v.x - 0.5) * 0.02;
      v.vy += (v.y - 0.5) * 0.02;
    }
  }
}

function sim({ sound: { speaker, synth }, clock, num }) {
  speaker?.poll(); // mandatory before audio reads

  // UTC-synced beat grid. clock.time() is an Invalid Date (getTime()→NaN) before
  // the offset resolves and offline; guard with isFinite or onBeat spams every tick.
  let ms = clock?.time?.()?.getTime?.();
  if (!Number.isFinite(ms)) ms = Date.now(); // local fallback; prod uses synced UTC
  simMs = ms;
  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx;
  if (idx !== lastBeat) {
    lastBeat = idx;
    const step = ((idx % PATTERN.length) + PATTERN.length) % PATTERN.length;
    onBeat(step, idx, synth);
  }

  // Envelopes.
  pump *= 0.965;
  if (pump < 0.001) pump = 0;
  kick *= 0.9;
  if (kick < 0.001) kick = 0;
  kickWave = Math.min(1.4, kickWave + 0.045); // shockwave expands
  bass *= 0.97;
  if (!Number.isFinite(bass)) bass = 0;

  // Read live low end to keep the field breathing even between scheduled bass notes.
  const bands = speaker?.frequencies?.left || [];
  const sub = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  bass = Math.max(bass, sub * 1.1);

  // Blob physics — drift toward home, wobble, gooey attraction so near voices merge.
  const t = simMs * 0.001;
  for (let i = 0; i < voices.length; i++) {
    const v = voices[i];
    if (!Number.isFinite(v.x)) { v.x = v.hx; v.y = v.hy; v.vx = 0; v.vy = 0; }
    // organic home orbit
    const ho = 0.06 * (1 + pump * 0.2);
    const tx = v.hx + Math.sin(t * 0.6 + i * 1.7) * ho;
    const ty = v.hy + Math.cos(t * 0.5 + i * 2.3) * ho;
    v.vx += (tx - v.x) * 0.02;
    v.vy += (ty - v.y) * 0.02;
    // gooey pull toward other charged voices → chords glue together
    for (let j = 0; j < voices.length; j++) {
      if (j === i) continue;
      const o = voices[j];
      const dx = o.x - v.x, dy = o.y - v.y;
      const d2 = dx * dx + dy * dy + 0.0004;
      const pull = (v.charge + o.charge) * 0.0009 / d2;
      v.vx += dx * pull;
      v.vy += dy * pull;
    }
    v.vx *= 0.86; v.vy *= 0.86;
    v.x += v.vx; v.y += v.vy;
    v.charge *= 0.955; // release
    if (!Number.isFinite(v.charge)) v.charge = 0;
  }

  // Transient tap sparks.
  for (let s = sparks.length - 1; s >= 0; s--) {
    const sp = sparks[s];
    sp.x += sp.vx; sp.y += sp.vy;
    sp.vx *= 0.9; sp.vy *= 0.9;
    sp.charge *= 0.94;
    sp.life -= 0.012;
    if (sp.life <= 0 || sp.charge < 0.03) sparks.splice(s, 1);
  }
}

// ── Whole piece is a button ──────────────────────────────────────────────────
function act({ event: e, screen, sound: { synth } }) {
  if (e.is("touch") || e.is("draw")) {
    const x = e.x / screen.width;
    const y = e.y / screen.height;
    pump = Math.min(3, pump + (e.is("draw") ? 0.14 : 0.95)); // taps punch, drags feed
    const hue = x * 360;
    // Y → pitch (top = higher). Discretize to the warm scale for cohesion.
    const scale = ["c", "d", "e", "g", "a"];
    const deg = Math.floor((1 - y) * scale.length);
    const oct = 2 + Math.floor((1 - y) * 3);
    const note = scale[Math.max(0, Math.min(scale.length - 1, deg))] + oct;
    sparks.push({
      x, y,
      vx: (Math.random() - 0.5) * 0.004,
      vy: (Math.random() - 0.5) * 0.004,
      charge: e.is("draw") ? 0.8 : 1.5,
      life: 1,
      hue,
      note,
    });
    if (sparks.length > 10) sparks.shift();
    // heat the resident goo a touch too
    bass = Math.min(1.5, bass + (1 - y) * 0.3);

    const pan = x * 2 - 1;
    synth({ tone: note, type: "sine", beats: e.is("draw") ? 0.4 : 0.9, attack: 0.005,
      decay: 0.55, volume: e.is("draw") ? 0.28 : 0.55, pan });
    synth({ tone: note, type: "triangle", beats: 0.3, attack: 0.002, decay: 0.32,
      volume: 0.22 * (0.4 + (1 - y)), pan });
  }
}

// ── Render: native-res metaball field straight to screen.pixels ──────────────
function paint({ screen, num, paintCount }) {
  const { width: w, height: h, pixels } = screen;
  if (!w || !h || !pixels) return;

  // Assemble all charges (resident voices + tap sparks) in pixel space once.
  // Precompute each charge's RGB ONCE here (never call hslToRgb in the pixel loop —
  // that's charges×pixels calls and tanks fps).
  const minWH = Math.min(w, h);
  const charges = [];
  for (const v of voices) {
    const rad = (v.base + v.charge * 0.9) * (1 + pump * 0.12);
    const [cr, cg, cb] = num.hslToRgb(v.hue, 88, 46 + v.charge * 22);
    charges.push({
      x: v.x * w, y: v.y * h,
      r2: Math.max(1, (rad * minWH * 0.16) ** 2),
      hot: v.charge, cr, cg, cb,
    });
  }
  for (const s of sparks) {
    const rad = (0.35 + s.charge * 0.8);
    const [cr, cg, cb] = num.hslToRgb(s.hue, 90, 52 + s.charge * 18);
    charges.push({
      x: s.x * w, y: s.y * h,
      r2: Math.max(1, (rad * minWH * 0.16) ** 2),
      hot: s.charge * 1.3, cr, cg, cb,
    });
  }
  const nCharges = charges.length;

  // Kick shockwave: a moving ring that adds field + jiggle.
  const shockR = kickWave * Math.min(w, h) * 0.7;
  const shockOn = kick > 0.02;
  const cx = w * 0.5, cy = h * 0.5;

  const visc = 1 + bass * 0.8 + pump * 0.15; // field viscosity/heat → thicker goo
  const gloss = 0.55 + bass * 0.35;          // rim brightness / glossiness
  const jig = (kick * 6) | 0;                // whole-goo jiggle in px

  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      // gooey jiggle displacement on kick
      let sx = x, sy = y;
      if (jig) {
        sx = x + ((Math.sin(y * 0.09 + simMs * 0.02) * jig) | 0);
        sy = y + ((Math.cos(x * 0.08 + simMs * 0.02) * jig) | 0);
      }

      // Metaball field: sum inverse-square falloff of every charge, colored by hue.
      let field = 0, r = 0, g = 0, b = 0, wsum = 0;
      for (let c = 0; c < nCharges; c++) {
        const ch = charges[c];
        const dx = sx - ch.x, dy = sy - ch.y;
        const d2 = dx * dx + dy * dy + 1;
        const weight = ch.r2 / d2; // 1 at radius edge, >1 inside
        field += weight * visc;
        // precomputed hue rgb, weighted; hotter voices contribute more color
        const cw = weight * (0.6 + ch.hot);
        r += ch.cr * cw; g += ch.cg * cw; b += ch.cb * cw; wsum += cw;
      }
      // shockwave ring adds field near its radius
      if (shockOn) {
        const dd = num.dist(sx, sy, cx, cy) - shockR;
        field += kick * 3.2 * Math.exp(-(dd * dd) / 260);
      }

      const i = (y * w + x) * 4;
      if (field < 0.9 || wsum <= 0) {
        // deep candy background — a dark warm gradient, not flat black
        const bg = 8 + (y / h) * 14;
        pixels[i] = bg + 6; pixels[i + 1] = bg * 0.4; pixels[i + 2] = bg + 18;
        pixels[i + 3] = 255;
        continue;
      }

      // inside the iso-surface → glossy blob body
      r /= wsum; g /= wsum; b /= wsum;
      // brighten toward the core, add a bright rim right at the threshold (glossy edge)
      const core = Math.min(1.6, field / 2.2);
      const rim = field > 0.9 && field < 1.5 ? (1.5 - field) * gloss * 220 : 0;
      let R = r * core + rim;
      let G = g * core + rim * 0.9;
      let B = b * core + rim;
      // specular highlight where the field is very strong
      if (field > 3) { const sp = Math.min(80, (field - 3) * 26); R += sp; G += sp; B += sp; }
      // ordered dither for candy texture
      const dith = ((x + y) & 1) * 8;
      pixels[i] = Math.max(0, Math.min(255, R - dith));
      pixels[i + 1] = Math.max(0, Math.min(255, G - dith * 0.7));
      pixels[i + 2] = Math.max(0, Math.min(255, B - dith * 0.5));
      pixels[i + 3] = 255;
    }
  }
}

export { boot, sim, act, paint };
