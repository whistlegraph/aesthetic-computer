// lavabath, 26.07.12
// Self-running A/V metaballs plasma where every charge is a VOICE.
//
// FOUR PRINCIPLES:
// 1. NO resolution() — the heavy inverse-square metaball loop renders into a
//    SMALL offscreen page() buffer (BUF_W×BUF_H) and is `paste`d full-screen
//    at native res; crisp overlays (voice halos, tap rings) drawn on top.
// 2. WHOLE PIECE = a button — tap / XY-drag spawns a fresh hot charge (a new
//    voice) at the touch point that pulses + booms (X→pan/hue, Y→pitch).
//    Repeated taps accumulate a decaying `pump` that heats the whole field.
// 3. UTC-SYNCED RHYTHM — the groove is driven from clock.time() in sim (not
//    beat()); onsets index by floor(globalBeat)%len so two instances align.
// 4. GRAPHIC↔SONIC ALLEGORY — each charge is a glowing blob; when its note
//    sounds it brightens/pulses, its color = its pitch (pitch→hue); the bass
//    is the overall field heat; the BEAT literally tears the color channels
//    (channel-shift glitch on UTC beat crossings). Read voices as blobs, the
//    beat as the glitch. Loops seamlessly (periodic orbits + UTC grid).
//
// A sustained low sine drone (the field's continuous heat/hum) fills the gaps
// between onsets so the reel carries; its volume swells with live bass + pump.

// ── tuning ────────────────────────────────────────────────────────────
const BPM = 132;
const BEAT_MS = 60000 / BPM; // ms per beat
const NCHARGES = 5; // seed voices (orbiting)
const MAX_CHARGES = 12; // cap incl. tap-spawned voices (keeps the loop cheap)

// One bar (8 eighth-note steps) of driving groove. Each step = one voice's
// note; a bass pulse anchors the downbeats. Pitch → hue keeps color legible.
const BASS = ["c2", "g1", "c2", "a1", "c2", "g1", "d2", "a1"]; // per step low
const ARP = ["c4", "e4", "g4", "b4", "c5", "g4", "e4", "d4"]; // per step lead
const PATTERN_LEN = 8;

// Note → hue map so a charge's color reads as its pitch.
const NOTE_HUE = {
  c: 0, d: 45, e: 90, f: 140, g: 200, a: 260, b: 310,
};
function noteHue(tone) {
  if (typeof tone !== "string") return 0;
  const letter = tone[0].toLowerCase();
  const oct = parseInt(tone.replace(/[^0-9]/g, ""), 10) || 4;
  // Base hue from letter, nudged by octave so highs drift bright.
  return (NOTE_HUE[letter] ?? 0 + (oct - 2) * 8) % 360;
}
const TAP_NOTES = ["c", "d", "e", "g", "a"]; // pentatonic for taps

// ── offscreen buffer (small — keeps the N-charge per-pixel loop cheap) ──
let field = null; // {pixels,width,height}
let BUF_W = 132; // small → the N-charge inverse-square loop stays smooth
let BUF_H = 234;

// ── state ─────────────────────────────────────────────────────────────
let charges = []; // { cx,cy,orbitRX,orbitRY,speed,phase,x,y,strength,
//                     hue, voice, life, seed, born }
let time = 0; // smooth animation clock (seconds-ish)
let beatProgress = 0; // 0..1 through current UTC beat
let lastBeat = -1; // last integer global-beat we fired
let downbeatPulse = 0; // decays after each beat, spikes on bar starts
let glitchAmt = 0; // channel-shift glitch strength (decays)
let paletteShift = 0; // global hue rotation, snaps on the bar
let liveBass = 0; // smoothed subBass amplitude
let liveAmp = 0; // smoothed overall amplitude
let pump = 0; // decaying tap energy, heats whole field
let bursts = []; // tap rings drawn crisp at native res
let bootMs = 0; // fallback epoch anchor if clock unavailable
let drone = null; // sustained low pad = the field's continuous heat/hum
let droneStarted = false;

function makeSeedCharge(i) {
  return {
    seed: true,
    // Periodic orbits → seamless loop. Each charge on its own ellipse.
    cx: BUF_W * (0.3 + 0.4 * ((i % 3) / 2)),
    cy: BUF_H * (0.22 + 0.56 * (i % 2)),
    orbitRX: BUF_W * (0.18 + (0.1 * ((i * 7) % 5)) / 5),
    orbitRY: BUF_H * (0.12 + (0.09 * ((i * 3) % 4)) / 4),
    speed: 0.4 + 0.22 * (i % 4), // integer-ish ratios → clean loop
    phase: (i / NCHARGES) * Math.PI * 2,
    hue: (i / NCHARGES) * 360,
    x: BUF_W / 2,
    y: BUF_H / 2,
    strength: 120 + i * 26,
    life: 1, // seed voices never die
    fade: 1, // current visibility 0..1 (seeds pinned to 1)
    pulse: 0, // brightens when this voice's note sounds
    voice: i % PATTERN_LEN, // which pattern step lights this charge
  };
}

function boot({ clock, screen }) {
  clock?.resync?.(); // fetch UTC offset (silent local fallback offline)
  bootMs = Date.now();
  // Size the small buffer to the screen's aspect so the paste-up isn't skewed.
  if (screen?.width && screen?.height) {
    const aspect = screen.height / screen.width;
    BUF_W = 132;
    BUF_H = Math.max(120, Math.round(BUF_W * aspect));
  }
  charges = [];
  for (let i = 0; i < NCHARGES; i++) charges.push(makeSeedCharge(i));
}

// Lazily (re)build the offscreen field buffer at the right size.
function ensureField({ painting, screen }) {
  const aspect = screen.height / screen.width;
  const wantH = Math.max(120, Math.round(BUF_W * aspect));
  if (!field || field.width !== BUF_W || field.height !== wantH) {
    BUF_H = wantH;
    field = painting(BUF_W, BUF_H, () => {});
  }
}

// ── UTC-driven groove (NOT beat()) so instances lock to the same grid ──
function fireStep(step, synth) {
  const bass = BASS[step];
  const lead = ARP[step];
  const bar = step % 4;

  // Bass pulse — the overall field HEAT reads off these low notes.
  synth({ tone: bass, type: "sawtooth", beats: 0.9, attack: 0.005, decay: 0.55, volume: 0.72 });
  synth({ tone: bass, type: "sine", beats: 0.6, attack: 0.005, decay: 0.5, volume: 0.6 });

  // Lead note — the VOICE for this step. Light the matching charge.
  const pan = -0.4 + (step / (PATTERN_LEN - 1)) * 0.8;
  synth({ tone: lead, type: "triangle", beats: 0.45, attack: 0.005, decay: 0.4, volume: 0.4, pan });

  // Hat on the offbeat (noise → TYPE, never tone).
  if (step % 2 === 1)
    synth({ type: "noise-white", tone: 1200, beats: 0.12, attack: 0.001, decay: 0.2, volume: 0.2 });

  // Light the seed charge that owns this step: brighten + recolor to its pitch.
  // Steps map onto the ring of seed voices so every onset lights a blob.
  const hue = noteHue(lead);
  const seeds = charges.filter((c) => c.seed);
  if (seeds.length) {
    const c = seeds[step % seeds.length];
    c.pulse = 1; // synchronized visible birth at the moment it sounds
    c.hue = hue; // color = pitch
  }

  downbeatPulse = 1;
  if (bar === 0) {
    glitchAmt = 1; // bar start → hard channel-shift glitch (you SEE the beat)
    paletteShift = (paletteShift + 47) % 360;
  }
}

function sim({ sound: { speaker, synth }, clock }) {
  speaker?.poll(); // mandatory before reading audio

  // Continuous low pad = the field's ever-present heat/hum. Fills the gaps
  // between onsets (so the reel carries) and swells with bass + tap pump.
  if (!droneStarted) {
    droneStarted = true;
    drone = synth({ tone: "c2", type: "sine", duration: "🔁", attack: 0.4, decay: 0.9, volume: 0.001 });
  }
  if (drone) {
    const vol = 0.16 + liveBass * 0.22 + Math.min(0.2, pump * 0.12);
    drone.update?.({ volume: vol });
  }

  // Absolute global beat from UTC epoch ms → identical step on every instance.
  const ms = (clock?.time?.() ?? new Date(bootMs + (Date.now() - bootMs))).getTime();
  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx; // 0..1 within current beat
  if (idx !== lastBeat) {
    lastBeat = idx;
    const step = ((idx % PATTERN_LEN) + PATTERN_LEN) % PATTERN_LEN;
    fireStep(step, synth);
  }

  // Read live audio → smooth it so motion breathes with the groove.
  const bands = speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const amp = speaker?.amplitudes?.left || 0;
  liveBass += (bass - liveBass) * 0.25;
  liveAmp += (amp - liveAmp) * 0.2;
  if (speaker?.beat?.detected)
    glitchAmt = Math.max(glitchAmt, speaker.beat.strength || 0.6);

  // Advance the animation clock; faster on the beat + with more energy + pump.
  time += 0.02 + liveBass * 0.05 + downbeatPulse * 0.01 + pump * 0.03;

  // Decay transient pulses + tap energy.
  downbeatPulse *= 0.9;
  glitchAmt *= 0.88;
  pump *= 0.965;

  // Move + age charges.
  for (let i = charges.length - 1; i >= 0; i--) {
    const c = charges[i];
    const a = time * c.speed + c.phase;
    const kick = 1 + liveBass * 0.8 + downbeatPulse * 0.35 + pump * 0.4;
    c.x = c.cx + Math.cos(a) * c.orbitRX * kick;
    c.y = c.cy + Math.sin(a * 1.3) * c.orbitRY * kick;
    c.pulse *= 0.9; // note-flash decays back to ambient

    if (c.seed) {
      c.fade = 1;
    } else {
      // Tap-spawned voices decay + drift, then die (freeing the slot).
      c.life -= 0.008;
      c.fade = Math.max(0, Math.min(1, c.life * 1.4));
      if (c.life <= 0) {
        charges.splice(i, 1);
        continue;
      }
    }
  }

  // Advance tap rings (crisp overlays).
  for (let i = bursts.length - 1; i >= 0; i--) {
    const b = bursts[i];
    b.r += 6 + pump * 6;
    b.life -= 0.045;
    if (b.life <= 0) bursts.splice(i, 1);
  }
}

// Whole piece is a button: tap/drag → new hot charge (voice) + boom.
function act({ event: e, sound: { synth }, screen, num }) {
  if (!(e.is("touch") || e.is("draw"))) return;

  const nx = num.clamp(e.x / screen.width, 0, 1); // 0..1
  const ny = num.clamp(e.y / screen.height, 0, 1); // 0..1
  const isDraw = e.is("draw");

  // Repeated taps/drags accumulate decaying pump → heats the whole field.
  pump = Math.min(2, pump + (isDraw ? 0.08 : 0.7));

  // X → note/pan/hue, Y → octave/pitch. Same mapping drives sound AND visual.
  const letter = TAP_NOTES[Math.min(TAP_NOTES.length - 1, Math.floor(nx * TAP_NOTES.length))];
  const oct = 2 + Math.floor((1 - ny) * 4); // top = higher
  const tone = letter + oct;
  const hue = noteHue(tone);
  const pan = nx * 2 - 1;

  // BOOM — the poke's audible burst.
  synth({ tone, type: "sine", beats: 0.5, attack: 0.004, decay: 0.6, volume: 0.55, pan });
  synth({ tone, type: "triangle", beats: 0.25, attack: 0.002, decay: 0.3, volume: 0.28 * (1 - ny), pan });

  // Spawn a fresh hot charge (a new VOICE) at the tap point in buffer space.
  if (charges.length < MAX_CHARGES) {
    const bx = nx * BUF_W;
    const by = ny * BUF_H;
    charges.push({
      seed: false,
      cx: bx,
      cy: by,
      orbitRX: BUF_W * 0.06,
      orbitRY: BUF_H * 0.05,
      speed: 0.7 + Math.random() * 0.6,
      phase: Math.random() * Math.PI * 2,
      hue,
      x: bx,
      y: by,
      strength: 150, // hot on birth (but not so hot it whites out the field)
      life: 1,
      fade: 1,
      pulse: 1, // flashes as it sounds
      voice: -1, // not on the UTC grid; it's a one-shot voice
    });
  }

  // Visible burst at the touch point (crisp, native res).
  bursts.push({ x: e.x, y: e.y, r: 4, life: 1, hue });
}

function paint({ screen, num, page, paste, ink, circle, painting }) {
  ensureField({ painting, screen });

  // ── draw the heavy metaball field into the SMALL offscreen buffer ──
  page(field);
  const w = field.width;
  const h = field.height;
  const pixels = field.pixels;
  const nb = charges.length;

  // Precompute per-charge values out of the inner loop.
  const cx = new Float32Array(nb);
  const cy = new Float32Array(nb);
  const cr = new Float32Array(nb);
  const cg = new Float32Array(nb);
  const cbb = new Float32Array(nb);
  const cs = new Float32Array(nb);
  // Bass = overall field heat; pump adds heat; each voice's pulse brightens it.
  // Keep heat bounded so a hard pump thickens/brightens blobs without washing
  // the black field to white (lava-on-black must survive heavy tapping).
  const heat = 1 + liveBass * 1.0 + downbeatPulse * 0.8 + Math.min(0.9, pump * 0.35);
  for (let i = 0; i < nb; i++) {
    const c = charges[i];
    cx[i] = c.x;
    cy[i] = c.y;
    const hue = (c.hue + time * 20 + paletteShift) % 360;
    // Note-flash lifts lightness so a sounding voice reads as a bright bloom.
    const light = Math.min(64, 42 + c.pulse * 22);
    const rgb = num.hslToRgb(hue, 100, light); // returns 0-255 already
    cr[i] = rgb[0];
    cg[i] = rgb[1];
    cbb[i] = rgb[2];
    // Strength scales with global heat, this voice's pulse, and its fade.
    cs[i] = c.strength * heat * (0.6 + c.pulse * 0.9) * (c.fade ?? 1);
  }

  // ── raw per-pixel inverse-square field (squared distance, no sqrt) ──
  let idx = 0;
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      let tw = 0;
      let r = 0;
      let g = 0;
      let b = 0;
      for (let i = 0; i < nb; i++) {
        const dx = x - cx[i];
        const dy = y - cy[i];
        const wgt = cs[i] / (dx * dx + dy * dy + 22);
        tw += wgt;
        r += cr[i] * wgt;
        g += cg[i] * wgt;
        b += cbb[i] * wgt;
      }
      const inv = 1 / (tw || 1);
      r *= inv;
      g *= inv;
      b *= inv;
      // Threshold the field → lava blobs on black. A hot field (pump/bass) makes
      // blobs more viscous — the threshold drops a touch so cores bleed together
      // — but the floor stays firmly black so the field never washes out.
      const floor = 0.12 - Math.min(0.03, pump * 0.012);
      let glow = (tw - floor) * (3.0 + liveBass * 1.4 + Math.min(0.5, pump * 0.2));
      if (glow < 0) glow = 0;
      else if (glow > 1.5) glow = 1.5; // ceiling keeps merged cores colored, not white
      glow += 0.04; // faint nebula floor
      r *= glow;
      g *= glow;
      b *= glow;
      const dith = ((x + y) & 1) * 8;
      r -= dith;
      g -= dith;
      b -= dith;
      pixels[idx] = r < 0 ? 0 : r > 255 ? 255 : r;
      pixels[idx + 1] = g < 0 ? 0 : g > 255 ? 255 : g;
      pixels[idx + 2] = b < 0 ? 0 : b > 255 ? 255 : b;
      pixels[idx + 3] = 255;
      idx += 4;
    }
  }

  // ── the BEAT = channel-shift glitch (torn rows) IN the small buffer ──
  if (glitchAmt > 0.05) {
    const shift = 2 + ((glitchAmt * 8) | 0);
    const rows = (glitchAmt * h * 0.5) | 0;
    for (let n = 0; n < rows; n++) {
      const yy = (Math.random() * h) | 0;
      const base = yy * w * 4;
      for (let x = w - 1; x >= shift; x--) {
        const di = base + x * 4;
        const sr = base + (x - shift) * 4; // red lags left
        const sb = base + (x - (shift >> 1)) * 4; // blue lags half
        pixels[di] = pixels[sr];
        pixels[di + 2] = pixels[sb + 2];
      }
    }
  }

  // ── composite the field up to NATIVE resolution ──
  page(screen);
  paste(field, 0, 0, { width: screen.width, height: screen.height });

  // ── crisp native-res overlays: voice cores + tap rings ──
  const sw = screen.width;
  const sh = screen.height;
  const sx = sw / w;
  const sy = sh / h;
  for (let i = 0; i < charges.length; i++) {
    const c = charges[i];
    if (c.pulse < 0.04) continue; // only sounding voices get a crisp halo
    const hue = (c.hue + time * 20 + paletteShift) % 360;
    const [rr, gg, bb] = num.hslToRgb(hue, 100, 60);
    const px = c.x * sx;
    const py = c.y * sy;
    const rad = (10 + c.pulse * 34) * (0.6 + (c.fade ?? 1));
    ink(rr, gg, bb, 90 * c.pulse * (c.fade ?? 1)).circle(px, py, rad, true);
    ink(255, 255, 255, 180 * c.pulse * (c.fade ?? 1)).circle(px, py, 3 + c.pulse * 5, true);
  }

  // Tap rings — the poke's visible shock.
  for (let i = 0; i < bursts.length; i++) {
    const b = bursts[i];
    const [rr, gg, bb] = num.hslToRgb(b.hue % 360, 100, 62);
    ink(rr, gg, bb, 200 * b.life).circle(b.x, b.y, b.r, false, 2 + b.life * 3);
  }
}

function leave() {
  drone?.kill?.(0.3);
}

export { boot, sim, act, paint, leave };
