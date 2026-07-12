// molten, 26.07.12
// Self-running A/V loop: a warm UTC-synced melody + low bass swell drive a
// LIQUID MOLTEN field — a warm displaced-sine plasma with a hot glow river.
// Rendered directly into the native framebuffer at density-3's already-small
// logical resolution (~360×640, the per-pixel sweet spot), so there is NO
// resolution() pin and the heavy per-pixel loop stays cheap. Crisp glow core,
// note markers, ripple rings and tap bursts are drawn at native res ON TOP.
//
// FOUR PRINCIPLES:
// 1. No resolution() — the field is a PURE FUNCTION of (x,y,time,music) written
//    straight to screen.pixels (never accumulates → cannot flood); at density 3
//    the logical screen is ~360×640, so the per-pixel loop is inexpensive.
//    (An offscreen page()/paste buffer was tried first but paste of a custom
//    buffer rendered nothing in the capture path, so we render native + cheap.)
// 2. Whole piece is a BUTTON — tap / XY-drag punches a ripple + shock into the
//    molten field at the tap point + a warm boom (X→pan/hue, Y→pitch). Repeated
//    taps accumulate a decaying `pump` that brightens + energizes the field.
// 3. UTC-synced rhythm — melody + bass onsets derive from clock.time() in sim
//    (NOT beat()), indexed by floor(globalBeat)%len, so two instances align.
// 4. Strong graphic↔sonic allegory — the melody note IS the molten GLOW's
//    vertical position + hue (pitch→height, high note=high+bright); the bass is
//    a low swell that energizes the field; each note-onset emits a ripple pulse
//    from the glow. You can READ the melody as the glow rising and coloring.

// —— Harmony (UTC-driven) ——
// A minor-pentatonic-ish melody that resolves back to the start so the loop is
// seamless. Higher index = higher pitch = higher + brighter glow.
const MELODY = ["a3", "c4", "e4", "g4", "a4", "g4", "e4", "d4"];
const BASS = ["a1", "a1", "c2", "e2", "d2", "d2", "f2", "e2"];

const BPM = 60;
const BEAT_MS = 60000 / BPM;

// Displacement state
let prev = null; // snapshot of last frame's pixels (light temporal blend)

// Rhythm (UTC) state
let lastBeat = -1;
let beatProgress = 0; // 0..1 within the current beat
let globalStep = 0; // current melody/bass step
let noteHeight = 0.5; // 0..1 — glow vertical target (from current pitch)
let noteHue = 30; // degrees — glow hue target (from current pitch)
let easedHeight = 0.5; // eased glow height
let easedHue = 30; // eased glow hue

// Audio-reactive easing
let phase = 0; // ever-advancing flow phase
let swell = 0; // eased bass swell
let shimmer = 0; // eased high-band shimmer
let onsetPulse = 0; // decays after each note onset → ripple + brightness

// Button / pump state
let pump = 0; // decaying global energy 0..~3
let ripples = []; // molten shocks {x,y (0..1), r, life, hue, amp}
let bursts = []; // crisp native-res tap flashes {x,y (px), r, life, hue}

// —— pitch → glow height + hue ——
const NOTE_ORDER = ["c", "d", "e", "f", "g", "a", "b"];
function noteToScalar(note) {
  // Map a note string like "e4" to a 0..1 scalar for height/hue.
  const m = /([a-g])(#?)(\d)/.exec(note);
  if (!m) return 0.5;
  const semis = NOTE_ORDER.indexOf(m[1]) + (m[2] ? 0.5 : 0);
  const oct = +m[3];
  const v = oct * 7 + semis; // rough pitch ordinal
  // Melody spans roughly a3..a4 → normalize into 0..1.
  return Math.max(0, Math.min(1, (v - 21) / 12));
}

function boot({ clock, sound }) {
  clock?.resync?.(); // fetch UTC offset (local fallback offline — fine)
  sound.bpm(BPM);
  sound.room?.set?.({ enabled: true, mix: 0.5, feedback: 0.6 }); // lush tail
}

// Drive audio from the UTC clock in sim (NOT beat()) so onsets align across
// instances. Poll audio, ease reactivity, advance the flow phase.
function sim({ sound: { speaker, synth }, clock, num, screen }) {
  speaker?.poll();

  const ms = (clock?.time?.() ?? new Date()).getTime();
  const globalBeat = ms / BEAT_MS;
  const idx = Math.floor(globalBeat);
  beatProgress = globalBeat - idx;

  if (idx !== lastBeat) {
    lastBeat = idx;
    const step = ((idx % MELODY.length) + MELODY.length) % MELODY.length;
    globalStep = step;
    onBeat(step, idx, synth, num, screen);
  }

  // Audio-reactive easing.
  const bands = speaker?.frequencies?.left || [];
  const bass = bands.find((b) => b.name === "subBass")?.amplitude || 0;
  const air = bands.find((b) => b.name === "air")?.amplitude || 0;
  swell = num.lerp(swell, bass, 0.08);
  shimmer = num.lerp(shimmer, air, 0.12);

  // Ease the glow toward the current note's height + hue (the melody, drawn).
  easedHeight = num.lerp(easedHeight, noteHeight, 0.14);
  // Hue lerp with wraparound guard (kept simple — melody hues stay in a band).
  easedHue = num.lerp(easedHue, noteHue, 0.12);

  // Decays.
  pump = Math.max(0, pump - 0.02);
  onsetPulse = Math.max(0, onsetPulse - 0.03);

  // Advance ripples (molten shocks) + bursts (crisp flashes).
  for (const rp of ripples) {
    rp.r += 0.012 + rp.amp * 0.008;
    rp.life -= 0.03; // shorter life → clean shock, not a sprawling arc
  }
  ripples = ripples.filter((rp) => rp.life > 0);
  for (const b of bursts) {
    b.r += screen.width * 0.02;
    b.life -= 0.04;
  }
  bursts = bursts.filter((b) => b.life > 0);

  phase += 0.02 + pump * 0.01;
}

// A UTC beat crossed → fire the melody note + bass, set the glow target, emit a
// ripple pulse (the visible birth of the audible onset).
function onBeat(step, idx, synth, num, screen) {
  const note = MELODY[step];
  const bass = BASS[step];

  // Melody → the glow. pitch → height + hue.
  const s = noteToScalar(note);
  noteHeight = 0.18 + (1 - s) * 0.64; // high note = high on screen (low y)
  noteHue = 20 + s * 60; // low→warm red/orange, high→bright yellow
  onsetPulse = 1;

  // Warm melody voice.
  synth({
    tone: note,
    type: "sine",
    beats: 1.4,
    attack: 0.02,
    decay: 0.7,
    volume: 0.5,
    pan: Math.sin(idx * 0.4) * 0.3,
  });
  // A soft triangle sparkle an octave up on the brighter notes.
  if (s > 0.5) {
    synth({
      tone: note.replace(/\d/, (d) => String(+d + 1)),
      type: "triangle",
      beats: 0.7,
      attack: 0.01,
      decay: 0.5,
      volume: 0.16 * s,
      pan: -Math.sin(idx * 0.4) * 0.4,
    });
  }

  // Bass → the global displacement amplitude / a low swell.
  synth({
    tone: bass,
    type: "sine",
    beats: 1.8,
    attack: 0.16,
    decay: 0.7,
    volume: 0.55,
    pan: Math.sin(idx * 0.4) * 0.2,
  });

  // Ripple pulse emanating from the glow position (normalized coords).
  ripples.push({
    x: 0.5,
    y: easedHeight,
    r: 0.02,
    life: 1,
    hue: noteHue,
    amp: 0.8,
  });
}

// —— THE WHOLE PIECE IS A BUTTON ——
// Tap / XY-drag punches a ripple + shock into the molten field + a warm boom.
function act({ event: e, sound: { synth }, screen, num }) {
  if (e.is("touch") || e.is("draw")) {
    const nx = num.clamp(e.x / screen.width, 0, 1);
    const ny = num.clamp(e.y / screen.height, 0, 1);
    const draw = e.is("draw");

    pump = Math.min(3, pump + (draw ? 0.14 : 0.95));
    onsetPulse = Math.min(1.4, onsetPulse + (draw ? 0.15 : 0.7));

    const hue = 20 + (1 - ny) * 100;

    // Molten shock in the field at the tap point.
    ripples.push({
      x: nx,
      y: ny,
      r: 0.02,
      life: 1,
      hue,
      amp: draw ? 1.0 : 1.8,
    });
    // Crisp native-res flash.
    bursts.push({
      x: e.x,
      y: e.y,
      r: 4,
      life: 1,
      hue,
    });

    // SONIC BOOM — X→pan/hue, Y→pitch (top = high). Fits the warm palette.
    const scale = ["c", "d", "e", "g", "a"];
    const oct = 2 + Math.floor((1 - ny) * 3); // top = higher octave
    const tone = scale[Math.floor(nx * 5) % 5] + oct;
    synth({
      tone,
      type: "sine",
      beats: draw ? 0.3 : 0.7,
      attack: 0.004,
      decay: 0.6,
      volume: (draw ? 0.28 : 0.6) * (0.6 + (1 - ny) * 0.5),
      pan: nx * 2 - 1,
    });
    // A low warm boom body — the "shock" you feel.
    synth({
      tone: scale[Math.floor(nx * 5) % 5] + "1",
      type: "sine",
      beats: draw ? 0.25 : 0.55,
      attack: 0.006,
      decay: 0.5,
      volume: draw ? 0.18 : 0.4,
      pan: nx * 2 - 1,
    });
  }
}

function paint({ ink, circle, screen, num }) {
  const { width: w, height: h } = screen;

  const beatPulse = 1 - beatProgress;
  const flow = phase;
  const cyclePhase = (globalStep + beatProgress) / MELODY.length; // 0..1
  const rot = cyclePhase * Math.PI * 2;
  const disp = 0.05 + (swell + pump * 0.4 + beatPulse * 0.15) * 0.06;
  const gyN = Number.isFinite(easedHeight) ? easedHeight : 0.5; // glow y, 0..1
  const gh = Number.isFinite(easedHue) ? easedHue : 30; // glow hue
  const pumpBright = Math.min(1, pump * 0.5); // taps brighten the whole field
  const blend = 0.28; // light temporal smoothing — never accumulates

  // —— DIRECT PROCEDURAL MOLTEN PLASMA (native screen.pixels, no runaway) ——
  // Rendered straight into the native framebuffer (360×640 at density 3 — the
  // per-pixel sweet spot, ~230K px). No resolution() pin. The field is a PURE
  // FUNCTION of (x, y, time, music): layered sines DISPLACED by a traveling flow
  // form warm molten ridges; the melody GLOW brightens a horizontal band
  // (pitch→height, hue→color); ripples add bright rings. A light temporal blend
  // (prev) softens the flow but can never accumulate to a flood.
  const pix = screen.pixels;
  if (!prev || prev.length !== pix.length) {
    prev = new Uint8ClampedArray(pix.length);
  }

  for (let y = 0; y < h; y++) {
    const fyN = y / h;
    const ry = fyN * Math.PI * 2;
    const distG = Math.abs(fyN - gyN);
    // Tight bright river around the note height → the melody reads as a rising,
    // coloring band, not a full disc; the rest stays dark flowing molten glass.
    const bandBright = Math.max(0, 1 - distG * 4.5);
    const dispY = Math.sin(ry * 3.0 + flow + rot) * disp;
    const rowBase = y * w;
    for (let x = 0; x < w; x++) {
      const fxN = x / w;
      const rx = fxN * Math.PI * 2;
      const dispX = Math.sin(rx * 2.4 + flow * 1.3 - rot) * disp;
      const px = fxN + dispX;
      const py = fyN + dispY;

      // Layered molten ridges (flowing plasma). Frequencies in RADIANS via the
      // 2π-scaled coords so the sines cycle several times across the field →
      // clearly-visible molten bands that undulate with the traveling flow.
      const wx = px * Math.PI * 2;
      const wy = py * Math.PI * 2;
      let v =
        Math.sin(wx * 1.6 + flow * 1.4) * 0.5 +
        Math.sin(wy * 2.0 - flow) * 0.5 +
        Math.sin((wx + wy) * 1.2 + rot) * 0.4 +
        Math.sin((wx - wy) * 2.4 - flow * 0.6) * 0.3;
      v = v * 0.6 + 0.5; // → ~0..1
      if (v < 0) v = 0;
      else if (v > 1) v = 1;
      v = v * v * (3 - 2 * v); // smoothstep → punchier ridges

      // Ripple shocks: a bright ring pulse near each expanding radius.
      let ring = 0;
      for (let k = 0; k < ripples.length; k++) {
        const rp = ripples[k];
        const dx = fxN - rp.x;
        const dy = fyN - rp.y;
        const d = Math.sqrt(dx * dx + dy * dy);
        const rr = d - rp.r;
        if (rr > -0.08 && rr < 0.08) {
          ring += Math.cos((rr / 0.08) * Math.PI * 0.5) * rp.life * rp.amp;
        }
      }

      // Molten intensity: a warm ridged plasma (always visible) lifted brighter
      // in the glow band + by ripples + pump. The molten material glows all over
      // with a bright hot river around the melody's height. NaN-guarded so a bad
      // eased value can never blank the frame.
      // Molten intensity 0..~1.3: flowing ridges (v) EVERYWHERE, boosted into a
      // bright hot river around the melody's height (bandBright), plus transient
      // ripples + pump + bass swell. High-contrast so ridges read as molten glass.
      let intensity =
        v * (0.2 + bandBright * 0.9) + ring * 0.9 + pumpBright * 0.35 + swell * 0.18;
      if (!(intensity >= 0)) intensity = 0.2; // NaN/negative guard
      intensity = intensity > 1.2 ? 1.2 : intensity < 0 ? 0 : intensity;
      const iMin = intensity > 1 ? 1 : intensity;

      // Color: deep ember troughs → hot glow-hue crests. Contrast-curved lightness
      // keeps the dark molten troughs deep so the flowing ridges read as glass.
      const hue = (((gh - (1 - iMin) * 46) % 360) + 360) % 360;
      const light = 4 + iMin * iMin * 70; // 4..74, dark troughs, hot crests
      const sat = 96 - intensity * 20;
      const [cr, cg, cb] = num.hslToRgb(hue, sat, light);

      const di = (rowBase + x) * 4;
      // Light temporal blend against the PREVIOUS PLASMA only (prev holds the
      // pre-overlay field snapshot) for soft flow without smearing overlays.
      pix[di] = cr * (1 - blend) + prev[di] * blend;
      pix[di + 1] = cg * (1 - blend) + prev[di + 1] * blend;
      pix[di + 2] = cb * (1 - blend) + prev[di + 2] * blend;
      pix[di + 3] = 255;
    }
  }

  // Snapshot the pure plasma (pre-overlay) for next frame's temporal blend so
  // the crisp overlays never smear into the field.
  prev.set(pix);

  // —— CRISP NATIVE-RES OVERLAYS ON TOP ——

  // Bright glow core at native res — the readable "melody head" (pitch→height).
  const ngx = w / 2;
  const ngy = easedHeight * h;
  const ncoreR = Math.min(w, h) * 0.03 * (1 + onsetPulse * 1.0 + pump * 0.3);
  const [cr, cg, cb] = num.hslToRgb(easedHue % 360, 92, 70);
  ink(cr, cg, cb, 90 + onsetPulse * 120).circle(ngx, ngy, ncoreR, true);
  ink(255, 250, 235, 120 + onsetPulse * 120).circle(ngx, ngy, ncoreR * 0.5, true);

  // Note markers: a small ladder showing where the melody sits (pitch height).
  for (let s = 0; s < MELODY.length; s++) {
    const sc = noteToScalar(MELODY[s]);
    const my = (0.18 + (1 - sc) * 0.64) * h;
    const active = s === globalStep;
    const mx = w * 0.08;
    const [mr, mg, mb] = num.hslToRgb((20 + sc * 60) % 360, 80, active ? 70 : 45);
    ink(mr, mg, mb, active ? 220 : 90).circle(
      mx,
      my,
      active ? 5 + onsetPulse * 4 : 3,
      true,
    );
  }

  // Ripple pulses as expanding rings (native res). Capped radius + short life so
  // they read as clean shock rings, not sprawling arcs across the whole field.
  for (const rp of ripples) {
    if (rp.life < 0.12) continue;
    const [rr, rg, rb] = num.hslToRgb(rp.hue % 360, 85, 66);
    const rad = Math.min(rp.r, 0.45) * Math.max(w, h);
    ink(rr, rg, rb, 150 * rp.life).circle(rp.x * w, rp.y * h, rad, false, 2);
  }

  // Tap bursts as bright expanding flashes at the touch point.
  for (const b of bursts) {
    const [tr, tg, tb] = num.hslToRgb(b.hue % 360, 90, 68);
    ink(tr, tg, tb, 200 * b.life).circle(b.x, b.y, b.r, false, 3);
    ink(255, 255, 255, 160 * b.life).circle(b.x, b.y, b.r * 0.4, true);
  }
}

function leave() {
  ripples = [];
  bursts = [];
  prev = null;
}

export { boot, sim, act, paint, leave };
