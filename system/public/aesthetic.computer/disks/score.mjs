// Score, 2026.3.15
// A spatial bytecode instrument for Aesthetic Computer.
// Type to compose. The keyboard is the instrument.
// The pixels are the waveform. The waveform draws the pixels.

/* 📝 Notes
  QWERTY keyboard = instrument. Each key is an opcode.
  Typed characters appear along a contour path (drawn or auto-laid).
  The pixel buffer feeds back into audio — what you see is what you hear.
  The audio feeds back into pixels — what you hear is what you see.

  Rows:
    1234567890  → values (literal push)
    QWERTYUIOP → pitch / frequency ops
    ASDFGHJKL  → drawing / shape ops
    ZXCVBNM    → control flow / modifiers

  Space = toggle score view / run view
  Backspace = delete last opcode
  Enter = new contour (new line in the score)
  Arrow keys = change scan pattern
*/

// --- Opcode table: every printable key is an opcode ---
// Each opcode has: glyph, family, exec function
// Families: val, pitch, draw, ctrl, color

const FAMILIES = {
  val:   { color: [200, 160, 50],  name: "value" },
  pitch: { color: [60, 160, 200],  name: "pitch" },
  draw:  { color: [200, 70, 90],   name: "draw" },
  ctrl:  { color: [110, 80, 180],  name: "control" },
  mod:   { color: [60, 160, 90],   name: "modifier" },
};

// Every typable key → opcode definition
const OPS = {
  // Number row: literal values (push 0-9 scaled)
  "1": { family: "val", desc: "push 1",  val: 1 },
  "2": { family: "val", desc: "push 2",  val: 2 },
  "3": { family: "val", desc: "push 3",  val: 3 },
  "4": { family: "val", desc: "push 4",  val: 4 },
  "5": { family: "val", desc: "push 5",  val: 5 },
  "6": { family: "val", desc: "push 6",  val: 6 },
  "7": { family: "val", desc: "push 7",  val: 7 },
  "8": { family: "val", desc: "push 8",  val: 8 },
  "9": { family: "val", desc: "push 9",  val: 9 },
  "0": { family: "val", desc: "push 0",  val: 0 },

  // QWERTY row: pitch / frequency / oscillation
  "q": { family: "pitch", desc: "wave ~",    fn: "wave" },
  "w": { family: "pitch", desc: "saw /|",    fn: "saw" },
  "e": { family: "pitch", desc: "square []", fn: "square" },
  "r": { family: "pitch", desc: "noise ##",  fn: "noise" },
  "t": { family: "pitch", desc: "time t",    fn: "time" },
  "y": { family: "pitch", desc: "freq up",   fn: "freqUp" },
  "u": { family: "pitch", desc: "freq dn",   fn: "freqDn" },
  "i": { family: "pitch", desc: "octave +",  fn: "octUp" },
  "o": { family: "pitch", desc: "octave -",  fn: "octDn" },
  "p": { family: "pitch", desc: "phase",     fn: "phase" },

  // ASDF row: drawing / shape ops
  "a": { family: "draw", desc: "plot .",     fn: "plot" },
  "s": { family: "draw", desc: "line -",     fn: "line" },
  "d": { family: "draw", desc: "circle O",   fn: "circle" },
  "f": { family: "draw", desc: "box []",     fn: "box" },
  "g": { family: "draw", desc: "fill #",     fn: "fill" },
  "h": { family: "draw", desc: "wipe",       fn: "wipe" },
  "j": { family: "draw", desc: "scroll ≋",   fn: "scroll" },
  "k": { family: "draw", desc: "spin ↻",     fn: "spin" },
  "l": { family: "draw", desc: "fade ≈",     fn: "fade" },

  // ZXCV row: control flow / modifiers
  "z": { family: "ctrl", desc: "loop ∿",     fn: "loop" },
  "x": { family: "ctrl", desc: "skip ?",     fn: "skip" },
  "c": { family: "ctrl", desc: "halt ■",     fn: "halt" },
  "v": { family: "ctrl", desc: "dup ⌐",      fn: "dup" },
  "b": { family: "ctrl", desc: "swap ⇄",     fn: "swap" },
  "n": { family: "ctrl", desc: "drop ↓",     fn: "drop" },
  "m": { family: "ctrl", desc: "emit ♫",     fn: "emit" },

  // Punctuation: modifiers
  ",": { family: "mod", desc: "AND &",       fn: "and" },
  ".": { family: "mod", desc: "OR |",        fn: "or" },
  "/": { family: "mod", desc: "XOR ⊕",      fn: "xor" },
  ";": { family: "mod", desc: "shift «",     fn: "shl" },
  "'": { family: "mod", desc: "shift »",     fn: "shr" },
  "[": { family: "mod", desc: "mod %",       fn: "mod" },
  "]": { family: "mod", desc: "not ¬",       fn: "not" },
  "-": { family: "mod", desc: "sub −",       fn: "sub" },
  "=": { family: "mod", desc: "add +",       fn: "add" },
};

// Scan patterns for reading the pixel buffer as audio
const SCAN_MODES = ["raster", "spiral", "column", "diagonal"];
const SCAN_NAMES = ["raster ↔", "spiral ◎", "column ↕", "diagonal ╱"];

// --- Pixel buffer (the field) ---
const FIELD_W = 64;
const FIELD_H = 64;

let field; // Uint8ClampedArray, RGBA
let stack = [];
let slots = new Float32Array(256);
let frame = 0;
let scanMode = 0;
let showRun = false;

// Audio state
let audioStarted = false;
let synthVoice = null;
let scanPhase = 0;
const SCAN_RATE = 44100;
const SAMPLES_PER_FRAME = 735; // ~60fps

// Contour: the typed score as a path of characters
let contours = [[]]; // Array of arrays of opcode keys
let activeContour = 0;
let cursorBlink = 0;

// Feedback state: pixel-derived values that modulate audio
let feedback = {
  avgBrightness: 0,
  avgR: 0, avgG: 0, avgB: 0,
  contrast: 0,
  variance: 0,
};

function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

// --- VM: execute the score on the pixel field ---
function resetVM() {
  stack.length = 0;
  frame = 0;
}

function push(v) { if (stack.length < 64) stack.push(v); }
function pop() { return stack.length > 0 ? stack.pop() : 0; }
function peek() { return stack.length > 0 ? stack[stack.length - 1] : 0; }

function fieldIndex(x, y) {
  const wx = ((x % FIELD_W) + FIELD_W) % FIELD_W;
  const wy = ((y % FIELD_H) + FIELD_H) % FIELD_H;
  return (wy * FIELD_W + wx) * 4;
}

function plotPixel(x, y, r, g, b, a = 255) {
  const i = fieldIndex(x, y);
  field[i] = r;
  field[i + 1] = g;
  field[i + 2] = b;
  field[i + 3] = a;
}

function readPixel(x, y) {
  const i = fieldIndex(x, y);
  return [field[i], field[i + 1], field[i + 2], field[i + 3]];
}

// Current drawing state within the VM
let inkR = 255, inkG = 255, inkB = 255;
let curX = 0, curY = 0;
let waveType = 0; // 0=sine, 1=saw, 2=square, 3=noise
let baseFreq = 220;
let octShift = 0;
let phaseOffset = 0;

function execOp(key) {
  const op = OPS[key];
  if (!op) return;

  // Values
  if (op.val !== undefined) {
    push(op.val * 28); // Scale 0-9 → 0-252
    return;
  }

  const fn = op.fn;
  switch (fn) {
    // Pitch
    case "wave":    waveType = 0; break;
    case "saw":     waveType = 1; break;
    case "square":  waveType = 2; break;
    case "noise":   waveType = 3; break;
    case "time":    push(frame & 255); break;
    case "freqUp":  baseFreq = Math.min(2000, baseFreq * 1.1); break;
    case "freqDn":  baseFreq = Math.max(30, baseFreq / 1.1); break;
    case "octUp":   octShift = Math.min(3, octShift + 1); break;
    case "octDn":   octShift = Math.max(-3, octShift - 1); break;
    case "phase":   phaseOffset = (phaseOffset + 0.25) % 1; break;

    // Draw
    case "plot": {
      const py = pop(), px = pop();
      plotPixel(px & 63, py & 63, inkR, inkG, inkB);
      break;
    }
    case "line": {
      const y2 = pop() & 63, x2 = pop() & 63;
      const y1 = pop() & 63, x1 = pop() & 63;
      // Bresenham
      let dx = Math.abs(x2 - x1), dy = Math.abs(y2 - y1);
      let sx = x1 < x2 ? 1 : -1, sy = y1 < y2 ? 1 : -1;
      let err = dx - dy, lx = x1, ly = y1;
      for (let steps = 0; steps < 128; steps++) {
        plotPixel(lx, ly, inkR, inkG, inkB);
        if (lx === x2 && ly === y2) break;
        const e2 = 2 * err;
        if (e2 > -dy) { err -= dy; lx += sx; }
        if (e2 < dx) { err += dx; ly += sy; }
      }
      break;
    }
    case "circle": {
      const r = pop() & 31, cy = pop() & 63, cx = pop() & 63;
      for (let a = 0; a < 64; a++) {
        const angle = (a / 64) * Math.PI * 2;
        plotPixel(
          Math.round(cx + Math.cos(angle) * r),
          Math.round(cy + Math.sin(angle) * r),
          inkR, inkG, inkB
        );
      }
      break;
    }
    case "box": {
      const bh = pop() & 63, bw = pop() & 63;
      const by = pop() & 63, bx = pop() & 63;
      for (let dy = 0; dy < bh; dy++) {
        for (let dx = 0; dx < bw; dx++) {
          plotPixel(bx + dx, by + dy, inkR, inkG, inkB);
        }
      }
      break;
    }
    case "fill": {
      const r = pop() & 255, g = pop() & 255, b = pop() & 255;
      inkR = r; inkG = g; inkB = b;
      break;
    }
    case "wipe": {
      for (let i = 0; i < field.length; i += 4) {
        field[i] = inkR; field[i + 1] = inkG;
        field[i + 2] = inkB; field[i + 3] = 255;
      }
      break;
    }
    case "scroll": {
      const dx = pop(), dy = pop();
      const copy = new Uint8ClampedArray(field);
      for (let y = 0; y < FIELD_H; y++) {
        for (let x = 0; x < FIELD_W; x++) {
          const si = fieldIndex(x - dx, y - dy);
          const di = (y * FIELD_W + x) * 4;
          field[di] = copy[si];
          field[di + 1] = copy[si + 1];
          field[di + 2] = copy[si + 2];
          field[di + 3] = copy[si + 3];
        }
      }
      break;
    }
    case "spin": break; // TODO: rotate field
    case "fade": {
      for (let i = 0; i < field.length; i += 4) {
        field[i] = Math.max(0, field[i] - 4);
        field[i + 1] = Math.max(0, field[i + 1] - 4);
        field[i + 2] = Math.max(0, field[i + 2] - 4);
      }
      break;
    }

    // Control
    case "loop":  break; // Handled by contour walker
    case "skip": {
      const v = pop();
      if (v === 0) push(1); // Signal to skip next op
      break;
    }
    case "halt":  break; // Stops execution
    case "dup":   push(peek()); break;
    case "swap": {
      const a = pop(), b = pop();
      push(a); push(b);
      break;
    }
    case "drop":  pop(); break;
    case "emit":  break; // Marker for audio emission point

    // Modifiers (bitwise/math)
    case "and": { const b = pop(), a = pop(); push((a & b) & 255); break; }
    case "or":  { const b = pop(), a = pop(); push((a | b) & 255); break; }
    case "xor": { const b = pop(), a = pop(); push((a ^ b) & 255); break; }
    case "shl": { const b = pop(), a = pop(); push((a << b) & 255); break; }
    case "shr": { const b = pop(), a = pop(); push(a >> b); break; }
    case "mod": { const b = pop(), a = pop(); push(b !== 0 ? a % b : 0); break; }
    case "not": { push(~pop() & 255); break; }
    case "sub": { const b = pop(), a = pop(); push(a - b); break; }
    case "add": { const b = pop(), a = pop(); push(a + b); break; }
  }
}

function execScore() {
  // Reset drawing state each frame
  curX = 0; curY = 0;
  inkR = 255; inkG = 255; inkB = 255;

  for (const contour of contours) {
    let loopStart = -1;
    let loopCount = 0;

    for (let i = 0; i < contour.length; i++) {
      const key = contour[i];
      if (key === "z") { // loop
        if (loopStart === -1) {
          loopStart = i;
          loopCount = 0;
        } else {
          loopCount++;
          if (loopCount < (pop() || 4)) {
            i = loopStart; // Jump back
          } else {
            loopStart = -1;
          }
        }
        continue;
      }
      if (key === "c") break; // halt

      execOp(key);
    }
  }
}

// --- Pixel → Audio scan ---
function sampleField() {
  feedback.avgR = 0; feedback.avgG = 0; feedback.avgB = 0;
  let maxB = 0, minB = 765;
  const brightnesses = [];

  for (let y = 0; y < FIELD_H; y += 4) {
    for (let x = 0; x < FIELD_W; x += 4) {
      const [r, g, b] = readPixel(x, y);
      feedback.avgR += r;
      feedback.avgG += g;
      feedback.avgB += b;
      const bright = r + g + b;
      brightnesses.push(bright);
      if (bright > maxB) maxB = bright;
      if (bright < minB) minB = bright;
    }
  }

  const count = brightnesses.length || 1;
  feedback.avgR /= count;
  feedback.avgG /= count;
  feedback.avgB /= count;
  feedback.avgBrightness = (feedback.avgR + feedback.avgG + feedback.avgB) / 3;
  feedback.contrast = maxB - minB;
  const avg = brightnesses.reduce((s, b) => s + b, 0) / count;
  feedback.variance = brightnesses.reduce((s, b) => s + (b - avg) ** 2, 0) / count;
}

// Read a scanline of pixel values as audio samples
function scanToAudio(numSamples) {
  const samples = [];
  const freq = baseFreq * Math.pow(2, octShift);
  // Feedback modulates frequency
  const modFreq = freq * (0.8 + (feedback.avgBrightness / 255) * 0.4);

  for (let i = 0; i < numSamples; i++) {
    scanPhase += 1;
    let x, y;

    switch (scanMode) {
      case 0: // Raster
        x = scanPhase % FIELD_W;
        y = Math.floor(scanPhase / FIELD_W) % FIELD_H;
        break;
      case 1: // Spiral
        const angle = scanPhase * 0.05;
        const radius = (scanPhase * 0.01) % (FIELD_W / 2);
        x = Math.floor(FIELD_W / 2 + Math.cos(angle) * radius);
        y = Math.floor(FIELD_H / 2 + Math.sin(angle) * radius);
        break;
      case 2: // Column (vertical scan)
        x = Math.floor(scanPhase / FIELD_H) % FIELD_W;
        y = scanPhase % FIELD_H;
        break;
      case 3: // Diagonal
        x = (scanPhase + Math.floor(scanPhase / FIELD_W)) % FIELD_W;
        y = scanPhase % FIELD_H;
        break;
      default:
        x = scanPhase % FIELD_W;
        y = Math.floor(scanPhase / FIELD_W) % FIELD_H;
    }

    const [r, g, b] = readPixel(x, y);
    const brightness = (r + g + b) / 765; // 0-1

    // Generate base waveform
    const t = scanPhase / SCAN_RATE;
    const phase = t * modFreq * Math.PI * 2 + phaseOffset * Math.PI * 2;
    let wave;
    switch (waveType) {
      case 0: wave = Math.sin(phase); break;
      case 1: wave = ((phase % (Math.PI * 2)) / Math.PI) - 1; break; // saw
      case 2: wave = Math.sin(phase) > 0 ? 1 : -1; break; // square
      case 3: wave = Math.random() * 2 - 1; break; // noise
      default: wave = Math.sin(phase);
    }

    // Pixel brightness modulates amplitude
    // Pixel color modulates timbre (R=fundamental, G=harmonic, B=noise)
    const rNorm = r / 255, gNorm = g / 255, bNorm = b / 255;
    const harmonic = Math.sin(phase * 2) * gNorm * 0.3;
    const noiseComponent = (Math.random() * 2 - 1) * bNorm * 0.1;

    let sample = (wave * rNorm * 0.6 + harmonic + noiseComponent) * brightness;

    // Feedback: contrast drives bit-crushing
    if (feedback.contrast > 200) {
      const crush = 32;
      sample = Math.floor(sample * crush) / crush;
    }

    // Feedback: variance drives chaos
    if (feedback.variance > 5000) {
      const t2 = scanPhase + i;
      sample = sample * 0.7 + ((t2 ^ (t2 >> 5)) & 255) / 255 * 0.3 - 0.15;
    }

    samples.push(clamp(sample, -1, 1));
  }

  return samples;
}

// Audio → Pixel feedback: sound amplitude paints into the field
function audioToPixels(samples) {
  if (!samples || samples.length === 0) return;

  const step = Math.max(1, Math.floor(samples.length / FIELD_W));
  for (let x = 0; x < FIELD_W; x++) {
    const sampleIdx = Math.min(x * step, samples.length - 1);
    const amp = Math.abs(samples[sampleIdx]);
    const y = Math.floor((1 - amp) * (FIELD_H - 1));

    // Additive: audio energy accumulates as brightness
    const [cr, cg, cb] = readPixel(x, y);
    plotPixel(x, y,
      Math.min(255, cr + Math.floor(amp * 60)),
      Math.min(255, cg + Math.floor(amp * 40)),
      Math.min(255, cb + Math.floor(amp * 80))
    );
  }
}

// --- Layout helpers ---
const CHAR_W = 6;
const CHAR_H = 8;
const CONTOUR_MARGIN = 4;
const HEADER_H = 14;
const FIELD_DISPLAY_SIZE = 128; // Rendered size of the 64x64 field on screen

function contourToString(contour) {
  return contour.join("");
}

function allOpsString() {
  return contours.map(contourToString).join("\n");
}

// --- Piece lifecycle ---

function boot({ screen }) {
  field = new Uint8ClampedArray(FIELD_W * FIELD_H * 4);
  // Start with dark field
  for (let i = 0; i < field.length; i += 4) {
    field[i] = 10; field[i + 1] = 10;
    field[i + 2] = 15; field[i + 3] = 255;
  }
}

function act({ event: e, sound, screen }) {
  cursorBlink++;

  // Toggle run/score view
  if (e.is("keyboard:down:space") && !e.repeat) {
    showRun = !showRun;
    if (showRun && !audioStarted) {
      // Start the audio feedback loop
      synthVoice = sound.synth({
        type: "custom",
        tone: baseFreq,
        duration: "🔁",
        volume: 0.4,
        attack: 0.05,
        decay: 0.95,
        generator: {
          bytebeat: ({ samplesNeeded }) => scanToAudio(samplesNeeded),
        },
      });
      audioStarted = true;
    } else if (!showRun && audioStarted) {
      synthVoice?.kill(0.15);
      audioStarted = false;
      synthVoice = null;
    }
    return;
  }

  // Scan mode cycle
  if (e.is("keyboard:down:arrowright") && !e.repeat) {
    scanMode = (scanMode + 1) % SCAN_MODES.length;
    return;
  }
  if (e.is("keyboard:down:arrowleft") && !e.repeat) {
    scanMode = (scanMode - 1 + SCAN_MODES.length) % SCAN_MODES.length;
    return;
  }

  // Delete last opcode
  if (e.is("keyboard:down:backspace") && !e.repeat) {
    if (contours[activeContour].length > 0) {
      contours[activeContour].pop();
    } else if (activeContour > 0) {
      contours.pop();
      activeContour--;
    }
    return;
  }

  // New contour line
  if (e.is("keyboard:down:enter") && !e.repeat) {
    contours.push([]);
    activeContour = contours.length - 1;
    return;
  }

  // Clear all
  if (e.is("keyboard:down:escape") && !e.repeat) {
    contours = [[]];
    activeContour = 0;
    resetVM();
    // Clear field
    for (let i = 0; i < field.length; i += 4) {
      field[i] = 10; field[i + 1] = 10;
      field[i + 2] = 15; field[i + 3] = 255;
    }
    return;
  }

  // Type an opcode
  for (const key of Object.keys(OPS)) {
    if (e.is(`keyboard:down:${key}`) && !e.repeat) {
      contours[activeContour].push(key);
      // Immediate execution in live mode
      if (showRun) {
        execOp(key);
      }
      return;
    }
  }
}

function sim({ sound }) {
  if (!showRun) return;

  // Execute the score each frame
  execScore();
  frame++;

  // Sample pixels for feedback
  sampleField();

  // Audio → pixel feedback
  const samples = scanToAudio(64);
  audioToPixels(samples);

  // Update synth frequency based on feedback
  if (synthVoice) {
    const modFreq = baseFreq * Math.pow(2, octShift) *
      (0.8 + (feedback.avgBrightness / 255) * 0.4);
    synthVoice.update?.({ tone: modFreq });
  }
}

function paint({ wipe, ink, line, box, circle, plot, write, screen, paste }) {
  const sw = screen.width, sh = screen.height;

  if (showRun) {
    // --- RUN VIEW: show the pixel field scaled up + score overlay ---
    wipe(5, 5, 10);

    // Draw the 64x64 field scaled to fit
    const scale = Math.min(
      Math.floor(sw / FIELD_W),
      Math.floor((sh - HEADER_H) / FIELD_H)
    );
    const fieldPxW = FIELD_W * scale;
    const fieldPxH = FIELD_H * scale;
    const ox = Math.floor((sw - fieldPxW) / 2);
    const oy = HEADER_H;

    for (let fy = 0; fy < FIELD_H; fy++) {
      for (let fx = 0; fx < FIELD_W; fx++) {
        const [r, g, b] = readPixel(fx, fy);
        if (r > 5 || g > 5 || b > 5) {
          ink(r, g, b);
          if (scale <= 2) {
            plot(ox + fx * scale, oy + fy * scale);
          } else {
            box(ox + fx * scale, oy + fy * scale, scale, scale);
          }
        }
      }
    }

    // Scan position indicator
    let scanX, scanY;
    switch (scanMode) {
      case 0:
        scanX = scanPhase % FIELD_W;
        scanY = Math.floor(scanPhase / FIELD_W) % FIELD_H;
        break;
      case 1:
        const angle = scanPhase * 0.05;
        const radius = (scanPhase * 0.01) % (FIELD_W / 2);
        scanX = Math.floor(FIELD_W / 2 + Math.cos(angle) * radius);
        scanY = Math.floor(FIELD_H / 2 + Math.sin(angle) * radius);
        break;
      case 2:
        scanX = Math.floor(scanPhase / FIELD_H) % FIELD_W;
        scanY = scanPhase % FIELD_H;
        break;
      case 3:
        scanX = (scanPhase + Math.floor(scanPhase / FIELD_W)) % FIELD_W;
        scanY = scanPhase % FIELD_H;
        break;
    }
    ink(255, 255, 255, 180);
    const spx = ox + (scanX & 63) * scale;
    const spy = oy + (scanY & 63) * scale;
    plot(spx, spy);
    if (scale > 2) {
      box(spx, spy, scale, scale, "outline");
    }

    // Header: frame, scan mode, feedback stats
    ink(30, 28, 40, 200);
    box(0, 0, sw, HEADER_H);
    ink(200, 200, 220);
    write(`f:${frame}`, { x: 2, y: 3 });
    ink(60, 160, 200);
    write(SCAN_NAMES[scanMode], { x: 36, y: 3 });
    ink(200, 70, 90);
    write(`br:${Math.round(feedback.avgBrightness)}`, { x: sw - 44, y: 3 });

    // Score text overlay (bottom, dimmed)
    const scoreStr = allOpsString();
    if (scoreStr.length > 0) {
      ink(80, 80, 100, 150);
      write(scoreStr.slice(-Math.floor(sw / CHAR_W)), {
        x: 2,
        y: sh - CHAR_H - 2,
      });
    }

    return;
  }

  // --- SCORE VIEW: type and see the score ---
  wipe(22, 20, 30);

  // Header
  ink(30, 28, 40);
  box(0, 0, sw, HEADER_H);
  ink(140, 140, 160);
  const totalOps = contours.reduce((n, c) => n + c.length, 0);
  write(`score  ${totalOps} ops`, { x: 2, y: 3 });
  ink(100, 100, 120);
  write("space:run", { x: sw - 54, y: 3 });

  // Draw contour lines as colored text
  let textY = HEADER_H + CONTOUR_MARGIN;

  for (let ci = 0; ci < contours.length; ci++) {
    const contour = contours[ci];
    let textX = CONTOUR_MARGIN;

    // Contour number
    ink(60, 58, 70);
    write(`${ci + 1}:`, { x: textX, y: textY });
    textX += CHAR_W * 2 + 2;

    // Each opcode as a colored character
    for (let oi = 0; oi < contour.length; oi++) {
      const key = contour[oi];
      const op = OPS[key];
      if (op) {
        const fam = FAMILIES[op.family];
        const [r, g, b] = fam.color;
        ink(r, g, b);
      } else {
        ink(100, 100, 100);
      }
      write(key, { x: textX, y: textY });
      textX += CHAR_W;

      // Wrap
      if (textX + CHAR_W > sw - CONTOUR_MARGIN) {
        textX = CONTOUR_MARGIN + CHAR_W * 2 + 2;
        textY += CHAR_H + 2;
      }
    }

    // Cursor on active contour
    if (ci === activeContour) {
      const blink = Math.floor(cursorBlink / 15) % 2 === 0;
      if (blink) {
        ink(240, 240, 250);
        write("_", { x: textX, y: textY });
      }
    }

    textY += CHAR_H + CONTOUR_MARGIN;
  }

  // Keyboard guide at bottom
  const guideY = sh - 52;
  ink(40, 38, 50);
  box(0, guideY, sw, 52);
  ink(55, 53, 65);
  line(0, guideY, sw, guideY);

  const rows = [
    { keys: "1234567890", label: "val", family: "val", y: guideY + 4 },
    { keys: "qwertyuiop", label: "pitch", family: "pitch", y: guideY + 16 },
    { keys: "asdfghjkl", label: "draw", family: "draw", y: guideY + 28 },
    { keys: "zxcvbnm,./", label: "ctrl", family: "ctrl", y: guideY + 40 },
  ];

  for (const row of rows) {
    // Label
    const fam = FAMILIES[row.family] || FAMILIES.mod;
    ink(fam.color[0], fam.color[1], fam.color[2], 120);
    write(row.label, { x: 2, y: row.y });

    // Keys
    let kx = 34;
    for (const ch of row.keys) {
      const op = OPS[ch];
      const f = op ? FAMILIES[op.family] : FAMILIES.mod;
      const [r, g, b] = f.color;

      // Highlight if recently typed
      const lastTyped = contours[activeContour]?.[contours[activeContour].length - 1];
      if (ch === lastTyped) {
        ink(r, g, b, 255);
        box(kx - 1, row.y - 1, CHAR_W + 1, CHAR_H + 1);
        ink(22, 20, 30);
      } else {
        ink(r, g, b, 180);
      }
      write(ch, { x: kx, y: row.y });
      kx += CHAR_W + 2;
    }
  }

  // Scan mode indicator
  ink(60, 160, 200, 150);
  write(`←→ scan: ${SCAN_NAMES[scanMode]}`, { x: sw - 100, y: guideY + 4 });

  // Empty state hint
  if (totalOps === 0) {
    ink(80, 80, 100);
    write("type to compose", { x: sw / 2 - 42, y: sh / 2 - 30 });
    write("space to run", { x: sw / 2 - 34, y: sh / 2 - 18 });
    write("what you see is what you hear", { x: sw / 2 - 80, y: sh / 2 - 6 });
  }
}

function leave() {
  if (synthVoice) {
    synthVoice.kill(0.05);
    synthVoice = null;
    audioStarted = false;
  }
}

export { boot, act, sim, paint, leave };
export const desc = "Spatial bytecode instrument — type to compose, pixels are the waveform.";
