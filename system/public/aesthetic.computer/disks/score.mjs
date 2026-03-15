// Score, 2026.3.15
// A contour bytecode instrument for Aesthetic Computer.
// Draw curves — opcodes sit along the path like beads on a string.
// The shape of the drawing IS the program. The drawing runs.

/* 📝 Notes
  You draw a stroke. Each ~20px segment auto-assigns the selected opcode.
  Colored beads appear along your path — that's the score.
  Press space to flip: see what the score produces when it runs.
  Segment geometry (length, angle, bend) feeds into opcode arguments.
  Loops in the path create repetition. Self-intersections create branches.
*/

// --- Opcodes (6-bit = 64 max) ---
const OP = {
  // 0x00-0x06: Flow & structure
  NOP:    0x00,
  EMBED:  0x01,
  EVERY:  0x02,
  ONCE:   0x03,
  AFTER:  0x04,
  BAKE:   0x05,
  HALT:   0x06,

  // 0x08-0x0D: Stack & memory
  PUSH:   0x08,
  DUP:    0x09,
  DROP:   0x0A,
  SWAP:   0x0B,
  LOAD:   0x0C,
  STORE:  0x0D,

  // 0x10-0x1A: Arithmetic & bitwise
  ADD:    0x10,
  SUB:    0x11,
  MUL:    0x12,
  DIV:    0x13,
  MOD:    0x14,
  AND:    0x15,
  OR:     0x16,
  XOR:    0x17,
  SHL:    0x18,
  SHR:    0x19,
  NOT:    0x1A,

  // 0x20-0x26: Sensing
  FRAME:  0x20,
  TIME:   0x21,
  WIDTH:  0x22,
  HEIGHT: 0x23,
  TAP:    0x24,
  MIC:    0x25,
  CHOOSE: 0x26,

  // 0x28-0x32: Rendering
  WIPE:   0x28,
  INK:    0x29,
  LINE:   0x2A,
  BOX:    0x2B,
  CIRCLE: 0x2C,
  PLOT:   0x2D,
  SCROLL: 0x2E,
  SPIN:   0x2F,
  ZOOM:   0x30,
  FADE:   0x31,
  EMIT:   0x32,

  // 0x38-0x3F: Color literals
  RED:    0x38,
  GREEN:  0x39,
  BLUE:   0x3A,
  BLACK:  0x3B,
  WHITE:  0x3C,
  CYAN:   0x3D,
  YELLOW: 0x3E,
  MAGENTA:0x3F,
};

const OP_NAME = {};
for (const [name, code] of Object.entries(OP)) OP_NAME[code] = name;

// Opcode families for palette grouping.
const OP_FAMILIES = [
  { name: "Flow",   ops: [OP.NOP, OP.EMBED, OP.EVERY, OP.ONCE, OP.AFTER, OP.BAKE, OP.HALT] },
  { name: "Stack",  ops: [OP.PUSH, OP.DUP, OP.DROP, OP.SWAP, OP.LOAD, OP.STORE] },
  { name: "Math",   ops: [OP.ADD, OP.SUB, OP.MUL, OP.DIV, OP.MOD, OP.AND, OP.OR, OP.XOR, OP.SHL, OP.SHR, OP.NOT] },
  { name: "Sense",  ops: [OP.FRAME, OP.TIME, OP.WIDTH, OP.HEIGHT, OP.TAP, OP.MIC, OP.CHOOSE] },
  { name: "Render", ops: [OP.WIPE, OP.INK, OP.LINE, OP.BOX, OP.CIRCLE, OP.PLOT, OP.SCROLL, OP.SPIN, OP.ZOOM, OP.FADE, OP.EMIT] },
  { name: "Color",  ops: [OP.RED, OP.GREEN, OP.BLUE, OP.BLACK, OP.WHITE, OP.CYAN, OP.YELLOW, OP.MAGENTA] },
];

// Build flat palette list with family separators.
const PALETTE = [];
for (const fam of OP_FAMILIES) {
  for (const op of fam.ops) PALETTE.push(op);
  PALETTE.push(-1); // separator
}
PALETTE.pop(); // Remove trailing separator.

// --- Glyph colors per opcode family ---
function opColor(op) {
  if (op === OP.NOP) return [50, 50, 60];
  if (op <= 0x06) return [110, 80, 180];
  if (op <= 0x0D) return [60, 160, 90];
  if (op <= 0x1A) return [200, 160, 50];
  if (op <= 0x26) return [60, 160, 200];
  if (op <= 0x32) return [200, 70, 90];
  if (op === OP.RED)     return [220, 50, 50];
  if (op === OP.GREEN)   return [50, 200, 50];
  if (op === OP.BLUE)    return [50, 80, 220];
  if (op === OP.BLACK)   return [30, 30, 30];
  if (op === OP.WHITE)   return [240, 240, 240];
  if (op === OP.CYAN)    return [50, 220, 220];
  if (op === OP.YELLOW)  return [240, 220, 50];
  if (op === OP.MAGENTA) return [220, 50, 220];
  return [100, 100, 100];
}

function opGlyph(op) {
  const g = {
    [OP.NOP]: "·", [OP.EMBED]: "◊", [OP.EVERY]: "∿", [OP.ONCE]: "¹",
    [OP.AFTER]: "→", [OP.BAKE]: "▣", [OP.HALT]: "■",
    [OP.PUSH]: "↑", [OP.DUP]: "⌐", [OP.DROP]: "↓", [OP.SWAP]: "⇄",
    [OP.LOAD]: "◁", [OP.STORE]: "▷",
    [OP.ADD]: "+", [OP.SUB]: "−", [OP.MUL]: "×", [OP.DIV]: "÷",
    [OP.MOD]: "%", [OP.AND]: "&", [OP.OR]: "|", [OP.XOR]: "⊕",
    [OP.SHL]: "«", [OP.SHR]: "»", [OP.NOT]: "¬",
    [OP.FRAME]: "f", [OP.TIME]: "t", [OP.WIDTH]: "w", [OP.HEIGHT]: "h",
    [OP.TAP]: "●", [OP.MIC]: "♪", [OP.CHOOSE]: "?",
    [OP.WIPE]: "█", [OP.INK]: "◆", [OP.LINE]: "╱", [OP.BOX]: "□",
    [OP.CIRCLE]: "○", [OP.PLOT]: "∗", [OP.SCROLL]: "≋",
    [OP.SPIN]: "↻", [OP.ZOOM]: "⊙", [OP.FADE]: "≈", [OP.EMIT]: "♫",
    [OP.RED]: "R", [OP.GREEN]: "G", [OP.BLUE]: "B", [OP.BLACK]: "K",
    [OP.WHITE]: "W", [OP.CYAN]: "C", [OP.YELLOW]: "Y", [OP.MAGENTA]: "M",
  };
  return g[op] || "?";
}

// --- Contour: a drawn path of opcode beads ---
class Contour {
  constructor() {
    this.points = [];  // [{x, y}, ...] vertices along the drawn path
    this.ops = [];     // [opcode, ...] one per segment (between consecutive points)
  }

  addPoint(x, y, op) {
    this.points.push({ x, y });
    // First point has no segment yet; segments start from point 1.
    if (this.points.length > 1) {
      this.ops.push(op);
    }
  }

  segmentCount() { return this.ops.length; }

  // Get the midpoint + geometry of segment i.
  segmentInfo(i) {
    if (i < 0 || i >= this.ops.length) return null;
    const p0 = this.points[i];
    const p1 = this.points[i + 1];
    const dx = p1.x - p0.x, dy = p1.y - p0.y;
    const length = Math.sqrt(dx * dx + dy * dy);
    const angle = Math.atan2(dy, dx);
    const mx = (p0.x + p1.x) / 2, my = (p0.y + p1.y) / 2;

    // Bend = angle change from previous segment.
    let bend = 0;
    if (i > 0) {
      const pp = this.points[i - 1];
      const prevAngle = Math.atan2(p0.y - pp.y, p0.x - pp.x);
      bend = angle - prevAngle;
      // Normalize to [-PI, PI].
      while (bend > Math.PI) bend -= 2 * Math.PI;
      while (bend < -Math.PI) bend += 2 * Math.PI;
    }

    return { p0, p1, mx, my, length, angle, bend, op: this.ops[i] };
  }
}

// --- Score: collection of contours ---
class Score {
  constructor() {
    this.contours = [];
    this.children = []; // Sub-scores for EMBED hierarchy.
  }

  addContour(contour) {
    this.contours.push(contour);
  }

  // Iterate all segments across all contours.
  eachSegment(fn) {
    for (const c of this.contours) {
      for (let i = 0; i < c.segmentCount(); i++) {
        fn(c.segmentInfo(i), c, i);
      }
    }
  }

  totalSegments() {
    let n = 0;
    for (const c of this.contours) n += c.segmentCount();
    return n;
  }
}

// --- VM: walks contours executing opcodes ---
class ScoreVM {
  constructor(screenW, screenH) {
    this.stack = [];
    this.slots = new Float32Array(256);
    this.frame = 0;
    this.screenW = screenW;
    this.screenH = screenH;
    this.currentInk = [255, 255, 255];
    this.drawOps = [];
  }

  reset() {
    this.stack.length = 0;
    this.drawOps.length = 0;
    this.currentInk = [255, 255, 255];
  }

  push(v) { if (this.stack.length < 64) this.stack.push(v); }
  pop() { return this.stack.length > 0 ? this.stack.pop() : 0; }
  peek() { return this.stack.length > 0 ? this.stack[this.stack.length - 1] : 0; }

  exec(score) {
    for (const contour of score.contours) {
      this.walkContour(contour);
    }
  }

  walkContour(contour) {
    for (let i = 0; i < contour.segmentCount(); i++) {
      const seg = contour.segmentInfo(i);
      this.execOp(seg.op, seg);
    }
  }

  execOp(op, seg) {
    // Segment geometry feeds into arguments:
    // length → magnitude, count, radius
    // bend → direction, sign
    // angle → mode
    const len = seg ? seg.length : 10;
    const bend = seg ? seg.bend : 0;
    const normLen = Math.min(255, Math.floor(len));

    switch (op) {
      case OP.NOP: break;
      case OP.HALT: return;
      case OP.EMBED: break; // TODO: hierarchy navigation

      // Timing: use segment length as the interval.
      case OP.EVERY:
        if (this.frame % Math.max(1, Math.floor(len / 5)) !== 0) return;
        break;
      case OP.ONCE:
        if (this.frame !== 0) return;
        break;
      case OP.AFTER:
        if (this.frame < Math.floor(len)) return;
        break;
      case OP.BAKE:
        this.drawOps.push({ type: "bake" });
        break;

      // Stack: length → push value, bend → swap direction.
      case OP.PUSH:  this.push(normLen); break;
      case OP.DUP:   this.push(this.peek()); break;
      case OP.DROP:  this.pop(); break;
      case OP.SWAP: {
        const a = this.pop(), b = this.pop();
        this.push(a); this.push(b);
        break;
      }
      case OP.LOAD:  this.push(this.slots[normLen & 255]); break;
      case OP.STORE: this.slots[normLen & 255] = this.pop(); break;

      // Arithmetic.
      case OP.ADD: { const b = this.pop(), a = this.pop(); this.push(a + b); break; }
      case OP.SUB: { const b = this.pop(), a = this.pop(); this.push(a - b); break; }
      case OP.MUL: { const b = this.pop(), a = this.pop(); this.push(a * b); break; }
      case OP.DIV: { const b = this.pop(), a = this.pop(); this.push(b !== 0 ? a / b : 0); break; }
      case OP.MOD: { const b = this.pop(), a = this.pop(); this.push(b !== 0 ? a % b : 0); break; }
      case OP.AND: { const b = this.pop(), a = this.pop(); this.push((a & b) & 255); break; }
      case OP.OR:  { const b = this.pop(), a = this.pop(); this.push((a | b) & 255); break; }
      case OP.XOR: { const b = this.pop(), a = this.pop(); this.push((a ^ b) & 255); break; }
      case OP.SHL: { const b = this.pop(), a = this.pop(); this.push((a << b) & 0xFFFF); break; }
      case OP.SHR: { const b = this.pop(), a = this.pop(); this.push(a >> b); break; }
      case OP.NOT: { this.push(~this.pop() & 255); break; }

      // Sensing.
      case OP.FRAME:  this.push(this.frame); break;
      case OP.TIME:   this.push(this.frame * 735); break;
      case OP.WIDTH:  this.push(this.screenW); break;
      case OP.HEIGHT: this.push(this.screenH); break;
      case OP.TAP:    this.push(this.tapState || 0); break;
      case OP.MIC:    this.push(this.micAmp || 0); break;
      case OP.CHOOSE: {
        const n = Math.max(1, Math.floor(normLen / 10));
        const vals = [];
        for (let i = 0; i < n; i++) vals.push(this.pop());
        if (vals.length > 0) this.push(vals[Math.floor(Math.random() * vals.length)]);
        break;
      }

      // Rendering.
      case OP.WIPE:
        this.drawOps.push({ type: "wipe", color: [...this.currentInk] });
        break;
      case OP.INK:
        this.currentInk = [this.pop(), this.pop(), this.pop()].reverse();
        break;
      case OP.LINE: {
        const y2 = this.pop(), x2 = this.pop(), y1 = this.pop(), x1 = this.pop();
        this.drawOps.push({ type: "line", x1, y1, x2, y2, color: [...this.currentInk] });
        break;
      }
      case OP.BOX: {
        const bh = this.pop(), bw = this.pop(), by = this.pop(), bx = this.pop();
        this.drawOps.push({ type: "box", x: bx, y: by, w: bw, h: bh, color: [...this.currentInk] });
        break;
      }
      case OP.CIRCLE: {
        const r = this.pop(), cy = this.pop(), cx = this.pop();
        this.drawOps.push({ type: "circle", x: cx, y: cy, r, color: [...this.currentInk] });
        break;
      }
      case OP.PLOT: {
        const py = this.pop(), px = this.pop();
        this.drawOps.push({ type: "plot", x: px, y: py, color: [...this.currentInk] });
        break;
      }
      case OP.SCROLL: {
        const sy = this.pop(), sx = this.pop();
        this.drawOps.push({ type: "scroll", dx: sx, dy: sy });
        break;
      }
      case OP.SPIN:
        this.drawOps.push({ type: "spin", angle: bend || this.pop() });
        break;
      case OP.ZOOM:
        this.drawOps.push({ type: "zoom", factor: 1 + (normLen - 20) * 0.01 });
        break;
      case OP.EMIT:
        this.drawOps.push({ type: "emit", sample: this.peek() });
        break;

      // Color literals.
      case OP.RED:     this.push(220); this.push(50);  this.push(50);  break;
      case OP.GREEN:   this.push(50);  this.push(200); this.push(50);  break;
      case OP.BLUE:    this.push(50);  this.push(80);  this.push(220); break;
      case OP.BLACK:   this.push(15);  this.push(15);  this.push(15);  break;
      case OP.WHITE:   this.push(240); this.push(240); this.push(240); break;
      case OP.CYAN:    this.push(50);  this.push(220); this.push(220); break;
      case OP.YELLOW:  this.push(240); this.push(220); this.push(50);  break;
      case OP.MAGENTA: this.push(220); this.push(50);  this.push(220); break;

      default: break;
    }
  }
}

// --- Example scores as contours ---

function makeExampleBytebeat() {
  const s = new Score();
  const c = new Contour();
  // A diagonal stroke: TIME DUP PUSH SHR XOR PUSH AND DUP DUP INK FRAME WIDTH MOD SWAP PLOT
  const ops = [OP.TIME, OP.DUP, OP.PUSH, OP.SHR, OP.XOR, OP.PUSH, OP.AND,
               OP.DUP, OP.DUP, OP.INK, OP.FRAME, OP.WIDTH, OP.MOD, OP.SWAP,
               OP.HEIGHT, OP.MUL, OP.PUSH, OP.DIV, OP.PLOT, OP.EMIT];
  // Lay out as a zigzag.
  const startX = 40, startY = 30;
  c.addPoint(startX, startY, OP.NOP);
  for (let i = 0; i < ops.length; i++) {
    const row = Math.floor(i / 5);
    const col = i % 5;
    const goRight = row % 2 === 0;
    const x = goRight ? startX + (col + 1) * 24 : startX + (4 - col) * 24;
    const y = startY + row * 28 + 28;
    c.addPoint(x, y, ops[i]);
  }
  s.addContour(c);
  return s;
}

function makeExampleCircle() {
  const s = new Score();
  const c = new Contour();
  // A circular path: BLACK INK WIPE RED INK PUSH PUSH PUSH CIRCLE
  const ops = [OP.BLACK, OP.INK, OP.WIPE, OP.RED, OP.INK, OP.PUSH, OP.PUSH, OP.PUSH, OP.CIRCLE];
  const cx = 100, cy = 80, radius = 50;
  c.addPoint(cx + radius, cy, OP.NOP);
  for (let i = 0; i < ops.length; i++) {
    const angle = ((i + 1) / ops.length) * Math.PI * 2;
    c.addPoint(cx + Math.cos(angle) * radius, cy + Math.sin(angle) * radius, ops[i]);
  }
  s.addContour(c);
  return s;
}

function makeExampleSpiral() {
  const s = new Score();
  const c = new Contour();
  // A spiral: FRAME PUSH MUL WIDTH MOD FRAME PUSH MUL HEIGHT MOD PLOT with SCROLL
  const ops = [OP.CYAN, OP.INK, OP.FRAME, OP.PUSH, OP.MUL,
               OP.WIDTH, OP.MOD, OP.FRAME, OP.PUSH, OP.MUL,
               OP.HEIGHT, OP.MOD, OP.PLOT, OP.PUSH, OP.PUSH, OP.SCROLL];
  const cx = 100, cy = 90;
  c.addPoint(cx, cy, OP.NOP);
  for (let i = 0; i < ops.length; i++) {
    const angle = (i / ops.length) * Math.PI * 4; // 2 full turns
    const r = 10 + i * 4;
    c.addPoint(cx + Math.cos(angle) * r, cy + Math.sin(angle) * r, ops[i]);
  }
  s.addContour(c);
  return s;
}

// --- Piece state ---
const BEAD_SPACING = 20;     // Min px between points when drawing.
const BEAD_RADIUS = 5;       // Visual bead size.
const PALETTE_H = 36;        // Palette strip height at bottom.
const PALETTE_CELL = 18;     // Palette cell size.
const HEADER_H = 12;         // Top header height.

let vm;
let currentScore;
let selectedOp = OP.PLOT;    // Current brush opcode.
let showRun = false;          // false = score view, true = run view.
let activeContour = null;     // Contour being drawn right now.
let hoverBead = null;         // {contourIdx, segIdx} of bead under cursor.
let paletteScroll = 0;        // Horizontal scroll offset for palette.

const examples = [];
const exampleNames = ["bytebeat zigzag", "circle path", "spiral"];
let exampleIndex = -1; // -1 = user drawing, 0+ = example.

function boot({ screen }) {
  vm = new ScoreVM(screen.width, screen.height);
  examples.push(makeExampleBytebeat());
  examples.push(makeExampleCircle());
  examples.push(makeExampleSpiral());
  currentScore = new Score(); // Start with blank canvas.
}

function act({ event: e, screen }) {
  const paletteTop = screen.height - PALETTE_H;

  // --- Run view: tap to flip back ---
  if (showRun) {
    if (e.is("touch") || e.is("keyboard:down:space")) {
      showRun = false;
    }
    return;
  }

  // --- Space: flip to run view ---
  if (e.is("keyboard:down:space")) {
    showRun = true;
    vm.frame = 0;
    return;
  }

  // --- Arrow keys: cycle examples ---
  if (e.is("keyboard:down:arrowright")) {
    exampleIndex = (exampleIndex + 1) % examples.length;
    currentScore = examples[exampleIndex];
    return;
  }
  if (e.is("keyboard:down:arrowleft")) {
    exampleIndex = (exampleIndex - 1 + examples.length) % examples.length;
    currentScore = examples[exampleIndex];
    return;
  }

  // --- Clear: backspace ---
  if (e.is("keyboard:down:backspace")) {
    currentScore = new Score();
    exampleIndex = -1;
    return;
  }

  // --- Palette touch ---
  if (e.is("touch") && e.y >= paletteTop) {
    const cellIdx = Math.floor((e.x + paletteScroll) / PALETTE_CELL);
    if (cellIdx >= 0 && cellIdx < PALETTE.length && PALETTE[cellIdx] !== -1) {
      selectedOp = PALETTE[cellIdx];
    }
    return;
  }

  // --- Drawing on canvas ---
  if (e.is("touch") && e.y < paletteTop) {
    // Start a new contour.
    if (exampleIndex >= 0) {
      // Switch to user mode, keep current score.
      currentScore = new Score();
      exampleIndex = -1;
    }
    activeContour = new Contour();
    activeContour.addPoint(e.x, e.y, selectedOp);
    currentScore.addContour(activeContour);
  }

  if (e.is("draw") && activeContour && e.y < paletteTop) {
    // Add point if far enough from last.
    const last = activeContour.points[activeContour.points.length - 1];
    const dx = e.x - last.x, dy = e.y - last.y;
    const dist = Math.sqrt(dx * dx + dy * dy);
    if (dist >= BEAD_SPACING) {
      activeContour.addPoint(e.x, e.y, selectedOp);
    }
  }

  if (e.is("lift")) {
    activeContour = null;
  }

  // --- Hover detection for beads ---
  if (e.is("move")) {
    hoverBead = findBeadAt(e.x, e.y);
  }
}

function findBeadAt(mx, my) {
  for (let ci = 0; ci < currentScore.contours.length; ci++) {
    const c = currentScore.contours[ci];
    for (let si = 0; si < c.segmentCount(); si++) {
      const seg = c.segmentInfo(si);
      const dx = mx - seg.mx, dy = my - seg.my;
      if (dx * dx + dy * dy < (BEAD_RADIUS + 4) * (BEAD_RADIUS + 4)) {
        return { ci, si, seg };
      }
    }
  }
  return null;
}

function paint({ wipe, ink, line, box, circle, plot, write, screen }) {
  if (showRun) {
    // --- RUN VIEW ---
    wipe(0, 0, 0);
    vm.screenW = screen.width;
    vm.screenH = screen.height;
    vm.reset();
    vm.exec(currentScore);

    for (const op of vm.drawOps) {
      switch (op.type) {
        case "wipe":
          wipe(op.color[0], op.color[1], op.color[2]);
          break;
        case "line":
          ink(op.color[0], op.color[1], op.color[2]);
          line(op.x1, op.y1, op.x2, op.y2);
          break;
        case "box":
          ink(op.color[0], op.color[1], op.color[2]);
          box(op.x, op.y, op.w, op.h);
          break;
        case "circle":
          ink(op.color[0], op.color[1], op.color[2]);
          circle(op.x, op.y, op.r);
          break;
        case "plot":
          ink(op.color[0], op.color[1], op.color[2]);
          plot(op.x, op.y);
          break;
        default: break;
      }
    }

    // Run mode indicator.
    ink(200, 70, 90);
    box(0, 0, screen.width, 2);
    ink(100, 100, 120);
    write(`f:${vm.frame}`, { x: 2, y: 4 });
    vm.frame++;
    return;
  }

  // --- SCORE VIEW ---
  wipe(22, 20, 30);

  const paletteTop = screen.height - PALETTE_H;

  // Draw contours: colored segments + beads.
  for (const contour of currentScore.contours) {
    // Draw path lines (thin, dimmed).
    for (let i = 0; i < contour.segmentCount(); i++) {
      const seg = contour.segmentInfo(i);
      const [r, g, b] = opColor(seg.op);
      ink(r, g, b, 120);
      line(seg.p0.x, seg.p0.y, seg.p1.x, seg.p1.y);
    }

    // Draw vertices (small dots).
    for (const pt of contour.points) {
      ink(80, 80, 100);
      plot(pt.x, pt.y);
    }

    // Draw beads at segment midpoints.
    for (let i = 0; i < contour.segmentCount(); i++) {
      const seg = contour.segmentInfo(i);
      const [r, g, b] = opColor(seg.op);

      // Bead circle.
      ink(r, g, b);
      circle(Math.round(seg.mx), Math.round(seg.my), BEAD_RADIUS);

      // Glyph on bead.
      ink(240, 240, 250);
      write(opGlyph(seg.op), {
        x: Math.round(seg.mx) - 3,
        y: Math.round(seg.my) - 3
      });
    }
  }

  // Hover tooltip.
  if (hoverBead) {
    const { seg } = hoverBead;
    ink(255, 255, 255);
    circle(Math.round(seg.mx), Math.round(seg.my), BEAD_RADIUS + 2, "outline");
    ink(220, 220, 240);
    const name = OP_NAME[seg.op] || "?";
    write(name, { x: Math.round(seg.mx) + BEAD_RADIUS + 4, y: Math.round(seg.my) - 3 });
  }

  // --- Header ---
  ink(30, 28, 40);
  box(0, 0, screen.width, HEADER_H);
  ink(140, 140, 160);
  const label = exampleIndex >= 0 ? exampleNames[exampleIndex] : "score";
  const segCount = currentScore.totalSegments();
  write(`${label}  ${segCount} ops`, { x: 2, y: 2 });
  ink(100, 100, 120);
  write("space:run", { x: screen.width - 52, y: 2 });

  // --- Palette strip ---
  ink(30, 28, 40);
  box(0, paletteTop, screen.width, PALETTE_H);

  // Thin separator line.
  ink(60, 58, 70);
  line(0, paletteTop, screen.width, paletteTop);

  // Draw palette cells.
  const palY = paletteTop + Math.floor((PALETTE_H - PALETTE_CELL) / 2);
  const palStartX = 4 - paletteScroll;

  for (let i = 0; i < PALETTE.length; i++) {
    const op = PALETTE[i];
    const px = palStartX + i * (PALETTE_CELL + 2);

    // Skip if off-screen.
    if (px + PALETTE_CELL < 0 || px > screen.width) continue;

    if (op === -1) {
      // Family separator: thin vertical line.
      ink(50, 48, 60);
      line(px + PALETTE_CELL / 2, palY + 2, px + PALETTE_CELL / 2, palY + PALETTE_CELL - 2);
      continue;
    }

    const [r, g, b] = opColor(op);
    const isSelected = op === selectedOp;

    // Cell background.
    ink(r, g, b, isSelected ? 255 : 160);
    box(px, palY, PALETTE_CELL, PALETTE_CELL);

    // Selected indicator: bright border.
    if (isSelected) {
      ink(255, 255, 255);
      box(px, palY, PALETTE_CELL, PALETTE_CELL, "outline");
    }

    // Glyph.
    ink(240, 240, 250);
    write(opGlyph(op), { x: px + 3, y: palY + 3 });
  }

  // Empty canvas hint.
  if (currentScore.totalSegments() === 0) {
    ink(80, 80, 100);
    write("draw a stroke", { x: screen.width / 2 - 34, y: screen.height / 2 - 10 });
    write("to compose a score", { x: screen.width / 2 - 46, y: screen.height / 2 + 2 });
  }
}

export { boot, act, paint };
export const desc = "Contour bytecode — draw a curve, the drawing runs.";
