// Jas, 2026.3.15
// Spatial bytecode instrument — type to compose, pixels are the waveform.

/* 📝 Notes
  QWERTY keyboard = instrument. Each key is an opcode.
  Typed characters form the score — shareable as plain text.
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

const hudSafeTop = 22;

// --- Opcode table: every printable key is an opcode ---

const FAMILIES = {
  val:   { color: [200, 160, 50],  name: "value" },
  pitch: { color: [60, 160, 200],  name: "pitch" },
  draw:  { color: [200, 70, 90],   name: "draw" },
  ctrl:  { color: [110, 80, 180],  name: "control" },
  mod:   { color: [60, 160, 90],   name: "modifier" },
};

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
  "j": { family: "draw", desc: "scroll",     fn: "scroll" },
  "k": { family: "draw", desc: "spin",       fn: "spin" },
  "l": { family: "draw", desc: "fade",       fn: "fade" },

  // ZXCV row: control flow / modifiers
  "z": { family: "ctrl", desc: "loop",       fn: "loop" },
  "x": { family: "ctrl", desc: "skip ?",     fn: "skip" },
  "c": { family: "ctrl", desc: "halt",       fn: "halt" },
  "v": { family: "ctrl", desc: "dup",        fn: "dup" },
  "b": { family: "ctrl", desc: "swap",       fn: "swap" },
  "n": { family: "ctrl", desc: "drop",       fn: "drop" },
  "m": { family: "ctrl", desc: "emit",       fn: "emit" },

  // Punctuation: modifiers
  ",": { family: "mod", desc: "AND &",       fn: "and" },
  ".": { family: "mod", desc: "OR |",        fn: "or" },
  "/": { family: "mod", desc: "XOR",         fn: "xor" },
  ";": { family: "mod", desc: "shl",         fn: "shl" },
  "'": { family: "mod", desc: "shr",         fn: "shr" },
  "[": { family: "mod", desc: "mod %",       fn: "mod" },
  "]": { family: "mod", desc: "not",         fn: "not" },
  "-": { family: "mod", desc: "sub",         fn: "sub" },
  "=": { family: "mod", desc: "add",         fn: "add" },
};

const SCAN_MODES = ["raster", "spiral", "column", "diagonal"];
const SCAN_NAMES = ["raster", "spiral", "column", "diagonal"];

// --- Pixel buffer (the field) ---
const FIELD_W = 64;
const FIELD_H = 64;

let field;
let stack = [];
let frame = 0;
let scanMode = 0;
let showRun = false;

// Audio state
let audioStarted = false;
let synthVoice = null;

// Contour: the typed score as a path of characters
let contours = [[]];
let activeContour = 0;
let cursorBlink = 0;
let showKey = false; // Tab toggles full opcode reference

// Feedback state: pixel-derived values that modulate the synth
let feedback = {
  avgBrightness: 0,
  avgR: 0, avgG: 0, avgB: 0,
  contrast: 0,
  variance: 0,
};

function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

// --- VM ---
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

function plotPixel(x, y, r, g, b) {
  const i = fieldIndex(x, y);
  field[i] = r; field[i + 1] = g; field[i + 2] = b; field[i + 3] = 255;
}

function readPixel(x, y) {
  const i = fieldIndex(x, y);
  return [field[i], field[i + 1], field[i + 2]];
}

// Drawing state within the VM
let inkR = 255, inkG = 255, inkB = 255;
let waveType = 0;
let baseFreq = 220;
let octShift = 0;
let phaseOffset = 0;

function execOp(key) {
  const op = OPS[key];
  if (!op) return;

  if (op.val !== undefined) {
    push(op.val * 28);
    return;
  }

  const fn = op.fn;
  switch (fn) {
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

    case "plot": {
      const py = pop(), px = pop();
      plotPixel(px & 63, py & 63, inkR, inkG, inkB);
      break;
    }
    case "line": {
      const y2 = pop() & 63, x2 = pop() & 63;
      const y1 = pop() & 63, x1 = pop() & 63;
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
          field[di] = copy[si]; field[di + 1] = copy[si + 1];
          field[di + 2] = copy[si + 2]; field[di + 3] = copy[si + 3];
        }
      }
      break;
    }
    case "spin": break;
    case "fade": {
      for (let i = 0; i < field.length; i += 4) {
        field[i] = Math.max(0, field[i] - 4);
        field[i + 1] = Math.max(0, field[i + 1] - 4);
        field[i + 2] = Math.max(0, field[i + 2] - 4);
      }
      break;
    }

    case "loop":  break;
    case "skip": {
      const v = pop();
      if (v === 0) push(1);
      break;
    }
    case "halt":  break;
    case "dup":   push(peek()); break;
    case "swap": {
      const a = pop(), b = pop();
      push(a); push(b);
      break;
    }
    case "drop":  pop(); break;
    case "emit":  break;

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
  inkR = 255; inkG = 255; inkB = 255;

  for (const contour of contours) {
    let loopStart = -1;
    let loopCount = 0;

    for (let i = 0; i < contour.length; i++) {
      const key = contour[i];
      if (key === "z") {
        if (loopStart === -1) {
          loopStart = i;
          loopCount = 0;
        } else {
          loopCount++;
          if (loopCount < (pop() || 4)) {
            i = loopStart;
          } else {
            loopStart = -1;
          }
        }
        continue;
      }
      if (key === "c") break;
      execOp(key);
    }
  }
}

// --- Pixel → feedback sampling ---
function sampleField() {
  feedback.avgR = 0; feedback.avgG = 0; feedback.avgB = 0;
  let maxB = 0, minB = 765;
  const brightnesses = [];

  for (let y = 0; y < FIELD_H; y += 4) {
    for (let x = 0; x < FIELD_W; x += 4) {
      const [r, g, b] = readPixel(x, y);
      feedback.avgR += r; feedback.avgG += g; feedback.avgB += b;
      const bright = r + g + b;
      brightnesses.push(bright);
      if (bright > maxB) maxB = bright;
      if (bright < minB) minB = bright;
    }
  }

  const count = brightnesses.length || 1;
  feedback.avgR /= count; feedback.avgG /= count; feedback.avgB /= count;
  feedback.avgBrightness = (feedback.avgR + feedback.avgG + feedback.avgB) / 3;
  feedback.contrast = maxB - minB;
  const avg = brightnesses.reduce((s, b) => s + b, 0) / count;
  feedback.variance = brightnesses.reduce((s, b) => s + (b - avg) ** 2, 0) / count;
}

// Audio → Pixel feedback: amplitude paints into the field
function audioToPixels(speaker) {
  const waveform = speaker?.waveforms?.left?.length
    ? speaker.waveforms.left
    : speaker?.waveforms?.right;
  if (!waveform || waveform.length === 0) return;

  const step = Math.max(1, Math.floor(waveform.length / FIELD_W));
  for (let x = 0; x < FIELD_W; x++) {
    const sampleIdx = Math.min(x * step, waveform.length - 1);
    const amp = Math.abs(waveform[sampleIdx]);
    const y = Math.floor((1 - amp) * (FIELD_H - 1));

    const [cr, cg, cb] = readPixel(x, y);
    plotPixel(x, y,
      Math.min(255, cr + Math.floor(amp * 60)),
      Math.min(255, cg + Math.floor(amp * 40)),
      Math.min(255, cb + Math.floor(amp * 80))
    );
  }
}

// --- Self-contained bytebeat generator (no closures) ---
// This function is serialized to a string and runs inside the audio worklet.
// It cannot reference any outer scope variables.
const bytebeatGenerator = ({ frequency, sampleRate, time, samplesNeeded }) => {
  const samples = [];
  const freqScale = frequency / 440;
  const timeOffset = Math.floor(time * sampleRate * freqScale * 0.3);

  for (let i = 0; i < samplesNeeded; i++) {
    const t = timeOffset + Math.floor(i * freqScale * 0.8);

    const pattern1 = (t ^ (t >> 8) ^ (t >> 9)) & 255;
    const harmonic = Math.max(1, Math.floor(freqScale * 2));
    const pattern2 = ((t * harmonic) & (t >> 5) | (t >> 4)) & 255;
    const rhythmMod = Math.floor((freqScale - 1) * 8) + 7;
    const pattern3 = (t | (t >> rhythmMod | t >> 7)) * (t & (t >> 11 | t >> 9)) & 255;
    const pattern4 = (t & (t >> 5 | t >> 8)) & 255;

    const mixPhase = (time * 0.08 + freqScale * 0.5) % 4;
    let finalPattern;

    if (mixPhase < 1) {
      finalPattern = pattern1 * (1 - mixPhase) + pattern2 * mixPhase;
    } else if (mixPhase < 2) {
      const blend = mixPhase - 1;
      finalPattern = pattern2 * (1 - blend) + pattern3 * blend;
    } else if (mixPhase < 3) {
      const blend = mixPhase - 2;
      finalPattern = pattern3 * (1 - blend) + pattern4 * blend;
    } else {
      const blend = mixPhase - 3;
      finalPattern = pattern4 * (1 - blend) + pattern1 * blend;
    }

    let sample = (finalPattern / 127.5) - 1;
    const crushFactor = 64;
    sample = Math.floor(sample * crushFactor) / crushFactor;
    samples.push(sample);
  }
  return samples;
};

// --- Layout ---
const CHAR_W = 6;
const CHAR_H = 8;
const CONTOUR_MARGIN = 4;

function allOpsString() {
  return contours.map((c) => c.join("")).join("\n");
}

// 🥾 Boot
function boot({ screen }) {
  field = new Uint8ClampedArray(FIELD_W * FIELD_H * 4);
  for (let i = 0; i < field.length; i += 4) {
    field[i] = 10; field[i + 1] = 10;
    field[i + 2] = 15; field[i + 3] = 255;
  }
}

// 🎪 Act
function act({ event: e, sound }) {
  cursorBlink++;

  // Toggle run/score view
  if (e.is("keyboard:down:space") && !e.repeat) {
    showRun = !showRun;
    if (showRun && !audioStarted) {
      synthVoice = sound.synth({
        type: "custom",
        tone: baseFreq,
        duration: "🔁",
        volume: 0.4,
        attack: 0.05,
        decay: 0.95,
        generator: bytebeatGenerator,
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

  // Toggle reference key
  if (e.is("keyboard:down:tab") && !e.repeat) {
    showKey = !showKey;
    return;
  }

  // New contour line
  if (e.is("keyboard:down:enter") && !e.repeat) {
    contours.push([]);
    activeContour = contours.length - 1;
    return;
  }

  // Type an opcode
  for (const key of Object.keys(OPS)) {
    if (e.is(`keyboard:down:${key}`) && !e.repeat) {
      contours[activeContour].push(key);
      if (showRun) execOp(key);
      return;
    }
  }
}

// 🧮 Sim
function sim({ sound }) {
  if (!showRun) return;

  sound.speaker?.poll();

  execScore();
  frame++;

  sampleField();

  // Audio → pixel feedback via speaker waveform data
  audioToPixels(sound.speaker);

  // Modulate synth frequency based on pixel feedback
  if (synthVoice) {
    const modFreq = baseFreq * Math.pow(2, octShift) *
      (0.8 + (feedback.avgBrightness / 255) * 0.4);
    synthVoice.update?.({ tone: modFreq });
  }
}

// 🎨 Paint
function paint({ wipe, ink, line, box, plot, write, screen }) {
  const sw = screen.width, sh = screen.height;

  if (showRun) {
    wipe(5, 5, 10);

    // Draw the 64x64 field scaled to fit below HUD
    const scale = Math.min(
      Math.floor(sw / FIELD_W),
      Math.floor((sh - hudSafeTop) / FIELD_H)
    );
    const fieldPxW = FIELD_W * scale;
    const ox = Math.floor((sw - fieldPxW) / 2);
    const oy = hudSafeTop;

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

    // Score text overlay (bottom, dimmed)
    const scoreStr = allOpsString();
    if (scoreStr.length > 0) {
      ink(80, 80, 100, 150);
      write(scoreStr.slice(-Math.floor(sw / CHAR_W)), {
        x: 2,
        y: sh - CHAR_H - 2,
      });
    }

    // Status line below HUD
    ink(60, 160, 200, 180);
    write(`${SCAN_NAMES[scanMode]}  f:${frame}`, { x: 2, y: sh - CHAR_H * 2 - 4 });

    return;
  }

  // --- SCORE VIEW ---
  wipe(22, 20, 30);

  // Draw contour lines as colored text, starting below HUD
  let textY = hudSafeTop + CONTOUR_MARGIN;
  const totalOps = contours.reduce((n, c) => n + c.length, 0);

  for (let ci = 0; ci < contours.length; ci++) {
    const contour = contours[ci];
    let textX = CONTOUR_MARGIN;

    ink(60, 58, 70);
    write(`${ci + 1}:`, { x: textX, y: textY });
    textX += CHAR_W * 2 + 2;

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

      if (textX + CHAR_W > sw - CONTOUR_MARGIN) {
        textX = CONTOUR_MARGIN + CHAR_W * 2 + 2;
        textY += CHAR_H + 2;
      }
    }

    if (ci === activeContour) {
      const blink = Math.floor(cursorBlink / 15) % 2 === 0;
      if (blink) {
        ink(240, 240, 250);
        write("_", { x: textX, y: textY });
      }
    }

    textY += CHAR_H + CONTOUR_MARGIN;
  }

  // Show last typed opcode description
  const lastTyped = contours[activeContour]?.[contours[activeContour].length - 1];
  if (lastTyped && OPS[lastTyped]) {
    const op = OPS[lastTyped];
    const fam = FAMILIES[op.family];
    ink(fam.color[0], fam.color[1], fam.color[2]);
    write(`${lastTyped} = ${op.desc}`, { x: sw - (op.desc.length + 4) * CHAR_W, y: hudSafeTop + 2 });
  }

  if (showKey) {
    // --- FULL REFERENCE KEY OVERLAY ---
    ink(18, 16, 24, 230);
    box(0, hudSafeTop, sw, sh - hudSafeTop);

    let ky = hudSafeTop + 4;
    const colW = Math.floor(sw / 2);

    // Title
    ink(200, 200, 220);
    write("opcode reference", { x: 4, y: ky });
    ink(100, 100, 120);
    write("tab to close", { x: sw - 72, y: ky });
    ky += CHAR_H + 6;

    // Draw each family as a section
    const familyKeys = {
      val:   "1234567890",
      pitch: "qwertyuiop",
      draw:  "asdfghjkl",
      ctrl:  "zxcvbnm",
      mod:   ",./;'[]-=",
    };

    for (const [famName, keys] of Object.entries(familyKeys)) {
      const fam = FAMILIES[famName];
      ink(fam.color[0], fam.color[1], fam.color[2]);
      write(fam.name, { x: 4, y: ky });
      ky += CHAR_H + 2;

      for (const ch of keys) {
        const op = OPS[ch];
        if (!op) continue;
        ink(fam.color[0], fam.color[1], fam.color[2], 255);
        write(ch, { x: 8, y: ky });
        ink(160, 160, 180);
        write(op.desc, { x: 20, y: ky });
        ky += CHAR_H + 1;

        if (ky > sh - CHAR_H - 4) break;
      }
      ky += 4;
      if (ky > sh - CHAR_H - 4) break;
    }

    // Controls at bottom
    ky = sh - CHAR_H * 4 - 8;
    ink(100, 100, 130);
    write("controls:", { x: 4, y: ky }); ky += CHAR_H + 2;
    write("space    run/stop", { x: 8, y: ky }); ky += CHAR_H + 1;
    write("enter    new line", { x: 8, y: ky }); ky += CHAR_H + 1;
    write("bksp     delete", { x: 8, y: ky });
  } else {
    // --- COMPACT KEYBOARD GUIDE ---
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
      const fam = FAMILIES[row.family] || FAMILIES.mod;
      ink(fam.color[0], fam.color[1], fam.color[2], 120);
      write(row.label, { x: 2, y: row.y });

      let kx = 34;
      for (const ch of row.keys) {
        const op = OPS[ch];
        const f = op ? FAMILIES[op.family] : FAMILIES.mod;
        const [r, g, b] = f.color;

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

    // Scan mode + status
    ink(60, 160, 200, 150);
    write(`scan: ${SCAN_NAMES[scanMode]}`, { x: sw - 76, y: guideY + 4 });
    ink(100, 100, 120);
    write(`${totalOps} ops`, { x: sw - 40, y: guideY + 28 });
    write("tab: key", { x: sw - 52, y: guideY + 40 });
  }

  // Empty state hint
  if (totalOps === 0 && !showKey) {
    ink(80, 80, 100);
    write("type to compose", { x: sw / 2 - 42, y: sh / 2 - 30 });
    write("space to run", { x: sw / 2 - 34, y: sh / 2 - 18 });
    write("tab for reference", { x: sw / 2 - 48, y: sh / 2 - 6 });
  }
}

// 👋 Leave
function leave() {
  if (synthVoice) {
    synthVoice.kill(0.05);
    synthVoice = null;
    audioStarted = false;
  }
}

function meta() {
  return {
    title: "Jas",
    desc: "Spatial bytecode instrument — type to compose, pixels are the waveform.",
  };
}

export { boot, act, sim, paint, leave, meta };
