// Morpho, 2026.3.02
// Pixel sorting as a model of morphogenesis — a laboratory / sandbox.

/* 📝 Engineering Notes
  Classical sorting algorithms transform disorder into order through iterated
  local operations — the same principle by which embryos self-organize from
  homogeneous cell masses. This piece lets you watch that process unfold
  pixel by pixel.

  Controls:
    S — cycle source (noise / gradient-h / gradient-v / camera / draw)
    A — cycle algorithm (bubble / selection / insertion / quick)
    K — cycle sort key (brightness / hue / red / green / blue)
    D — cycle direction (rows / columns / both)
    T / G — threshold up / down (0 = sort everything)
    + / - — speed up / slow down
    X — toggle damage (frozen pixels that refuse to swap)
    R — reset / regenerate
    Space — pause / resume
*/

const { floor, min, max, abs, random, round } = Math;

// --- Source ---
const SOURCES = ["noise", "gradient-h", "gradient-v", "camera", "draw"];
let sourceIdx = 0;

// --- Algorithm ---
const ALGORITHMS = ["bubble", "selection", "insertion", "quick"];
let algoIdx = 0;

// --- Sort key ---
const KEYS = ["brightness", "hue", "red", "green", "blue"];
let keyIdx = 0;

// --- Direction ---
const DIRECTIONS = ["rows", "columns", "both"];
let dirIdx = 0;

// --- Parameters ---
let threshold = 0; // 0–255; 0 = sort all, >0 = interval sorting
let speed = 4; // comparison steps per row/col per frame
let paused = false;
let damageEnabled = false;

// --- Pixel buffer (separate from screen so HUD text doesn't contaminate) ---
let buf = null;
let bufW = 0,
  bufH = 0;

// --- Per-row / per-column sort state ---
let rowStates = [];
let colStates = [];
let sortComplete = false;

// --- Damage map ---
let damageMap = null; // Uint8Array, 1 = frozen

// --- Camera ---
let vid = null;
let cameraReady = false;

// --- Drawing ---
let drawColor = [255, 100, 50];
let penDown = false;
let lastPenX = -1,
  lastPenY = -1;

// -------------------------------------------------------------------------
// Utilities
// -------------------------------------------------------------------------

function clamp8(v) {
  return max(0, min(255, round(v)));
}

function hslToRgb(h, s, l) {
  let r, g, b;
  if (s === 0) {
    r = g = b = l;
  } else {
    const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
    const p = 2 * l - q;
    const hue2rgb = (pp, qq, t) => {
      if (t < 0) t += 1;
      if (t > 1) t -= 1;
      if (t < 1 / 6) return pp + (qq - pp) * 6 * t;
      if (t < 1 / 2) return qq;
      if (t < 2 / 3) return pp + (qq - pp) * (2 / 3 - t) * 6;
      return pp;
    };
    r = hue2rgb(p, q, h + 1 / 3);
    g = hue2rgb(p, q, h);
    b = hue2rgb(p, q, h - 1 / 3);
  }
  return [round(r * 255), round(g * 255), round(b * 255)];
}

function rgbToHue(r, g, b) {
  r /= 255;
  g /= 255;
  b /= 255;
  const cmax = max(r, g, b),
    cmin = min(r, g, b);
  const d = cmax - cmin;
  if (d === 0) return 0;
  let h;
  if (cmax === r) h = ((g - b) / d) % 6;
  else if (cmax === g) h = (b - r) / d + 2;
  else h = (r - g) / d + 4;
  h *= 60;
  if (h < 0) h += 360;
  return h; // 0–360
}

// -------------------------------------------------------------------------
// Sort key extraction
// -------------------------------------------------------------------------

function getSortKey(idx) {
  const r = buf[idx],
    g = buf[idx + 1],
    b = buf[idx + 2];
  const key = KEYS[keyIdx];
  if (key === "brightness") return r * 0.299 + g * 0.587 + b * 0.114;
  if (key === "red") return r;
  if (key === "green") return g;
  if (key === "blue") return b;
  if (key === "hue") return rgbToHue(r, g, b);
  return 0;
}

// -------------------------------------------------------------------------
// Source generation
// -------------------------------------------------------------------------

function generateSource() {
  const src = SOURCES[sourceIdx];

  if (src === "noise") {
    for (let i = 0; i < bufW * bufH; i++) {
      const idx = i * 4;
      buf[idx] = floor(random() * 256);
      buf[idx + 1] = floor(random() * 256);
      buf[idx + 2] = floor(random() * 256);
      buf[idx + 3] = 255;
    }
  } else if (src === "gradient-h") {
    for (let y = 0; y < bufH; y++) {
      for (let x = 0; x < bufW; x++) {
        const hue = x / bufW;
        const [r, g, b] = hslToRgb(hue, 0.8, 0.5);
        const n = (random() - 0.5) * 80;
        const idx = (y * bufW + x) * 4;
        buf[idx] = clamp8(r + n);
        buf[idx + 1] = clamp8(g + n);
        buf[idx + 2] = clamp8(b + n);
        buf[idx + 3] = 255;
      }
    }
  } else if (src === "gradient-v") {
    for (let y = 0; y < bufH; y++) {
      for (let x = 0; x < bufW; x++) {
        const hue = y / bufH;
        const [r, g, b] = hslToRgb(hue, 0.8, 0.5);
        const n = (random() - 0.5) * 80;
        const idx = (y * bufW + x) * 4;
        buf[idx] = clamp8(r + n);
        buf[idx + 1] = clamp8(g + n);
        buf[idx + 2] = clamp8(b + n);
        buf[idx + 3] = 255;
      }
    }
  } else if (src === "draw") {
    for (let i = 0; i < bufW * bufH; i++) {
      const idx = i * 4;
      buf[idx] = 15;
      buf[idx + 1] = 10;
      buf[idx + 2] = 20;
      buf[idx + 3] = 255;
    }
  }
  // Camera source is handled separately in paint().

  damageMap.fill(0);
}

// -------------------------------------------------------------------------
// Sort state
// -------------------------------------------------------------------------

function createSortState(algo, length) {
  if (algo === "bubble")
    return { type: "bubble", n: length, i: 0, pass: 0, done: false };
  if (algo === "selection")
    return {
      type: "selection",
      n: length,
      current: 0,
      scanPos: 1,
      minIdx: 0,
      done: false,
    };
  if (algo === "insertion")
    return { type: "insertion", n: length, current: 1, j: 1, done: false };
  if (algo === "quick")
    return {
      type: "quick",
      n: length,
      stack: [[0, length - 1]],
      ps: null,
      done: false,
    };
  return { done: true };
}

function initSortState() {
  sortComplete = false;
  rowStates = [];
  colStates = [];

  const dir = DIRECTIONS[dirIdx];
  const algo = ALGORITHMS[algoIdx];

  if (dir === "rows" || dir === "both") {
    for (let y = 0; y < bufH; y++)
      rowStates.push(createSortState(algo, bufW));
  }
  if (dir === "columns" || dir === "both") {
    for (let x = 0; x < bufW; x++)
      colStates.push(createSortState(algo, bufH));
  }
}

// -------------------------------------------------------------------------
// Pixel helpers
// -------------------------------------------------------------------------

function rowIdx(y, x) {
  return (y * bufW + x) * 4;
}
function colIdx(x, y) {
  return (y * bufW + x) * 4;
}

function swapPixels(a, b) {
  for (let c = 0; c < 4; c++) {
    const tmp = buf[a + c];
    buf[a + c] = buf[b + c];
    buf[b + c] = tmp;
  }
}

function inThreshold(flatIdx) {
  if (threshold === 0) return true;
  const bright =
    buf[flatIdx] * 0.299 + buf[flatIdx + 1] * 0.587 + buf[flatIdx + 2] * 0.114;
  return bright > threshold;
}

function isDamaged(flatIdx) {
  if (!damageEnabled) return false;
  return damageMap[flatIdx >> 2] === 1; // flatIdx is byte offset, /4 for pixel index
}

// -------------------------------------------------------------------------
// Sort step functions — one comparison/swap per call
// -------------------------------------------------------------------------

function stepBubble(st, getIdx) {
  if (st.done) return;
  const bound = st.n - 1 - st.pass;
  if (bound <= 0) {
    st.done = true;
    return;
  }
  if (st.i >= bound) {
    st.pass++;
    st.i = 0;
    if (st.n - 1 - st.pass <= 0) st.done = true;
    return;
  }
  const a = getIdx(st.i),
    b = getIdx(st.i + 1);
  if (inThreshold(a) && inThreshold(b) && !isDamaged(a) && !isDamaged(b)) {
    if (getSortKey(a) > getSortKey(b)) swapPixels(a, b);
  }
  st.i++;
}

function stepSelection(st, getIdx) {
  if (st.done) return;
  if (st.current >= st.n - 1) {
    st.done = true;
    return;
  }
  const idxScan = getIdx(st.scanPos);
  const idxMin = getIdx(st.minIdx);
  if (
    inThreshold(idxScan) &&
    inThreshold(idxMin) &&
    !isDamaged(idxScan)
  ) {
    if (getSortKey(idxScan) < getSortKey(idxMin)) st.minIdx = st.scanPos;
  }
  st.scanPos++;
  if (st.scanPos >= st.n) {
    const idxCur = getIdx(st.current);
    const idxMinF = getIdx(st.minIdx);
    if (
      !isDamaged(idxCur) &&
      !isDamaged(idxMinF) &&
      st.minIdx !== st.current
    )
      swapPixels(idxCur, idxMinF);
    st.current++;
    st.scanPos = st.current + 1;
    st.minIdx = st.current;
    if (st.current >= st.n - 1) st.done = true;
  }
}

function stepInsertion(st, getIdx) {
  if (st.done) return;
  if (st.current >= st.n) {
    st.done = true;
    return;
  }
  if (st.j > 0) {
    const a = getIdx(st.j),
      b = getIdx(st.j - 1);
    if (inThreshold(a) && inThreshold(b) && !isDamaged(a) && !isDamaged(b)) {
      if (getSortKey(b) > getSortKey(a)) {
        swapPixels(b, a);
        st.j--;
        return;
      }
    }
    st.current++;
    st.j = st.current;
  } else {
    st.current++;
    st.j = st.current;
  }
}

function stepQuick(st, getIdx) {
  if (st.done) return;
  if (st.stack.length === 0 && !st.ps) {
    st.done = true;
    return;
  }
  if (!st.ps) {
    const [lo, hi] = st.stack.pop();
    if (lo >= hi) return;
    const pivotKey = getSortKey(getIdx(hi));
    st.ps = { lo, hi, pivotKey, i: lo, store: lo };
  }
  const p = st.ps;
  if (p.i < p.hi) {
    const idxI = getIdx(p.i),
      idxS = getIdx(p.store);
    if (inThreshold(idxI) && !isDamaged(idxI) && !isDamaged(idxS)) {
      if (getSortKey(idxI) <= p.pivotKey) {
        if (p.i !== p.store) swapPixels(idxI, idxS);
        p.store++;
      }
    }
    p.i++;
  } else {
    const idxS = getIdx(p.store),
      idxH = getIdx(p.hi);
    if (!isDamaged(idxS) && !isDamaged(idxH)) swapPixels(idxS, idxH);
    const piv = p.store;
    if (piv - 1 > p.lo) st.stack.push([p.lo, piv - 1]);
    if (piv + 1 < p.hi) st.stack.push([piv + 1, p.hi]);
    st.ps = null;
  }
}

function stepSort(st, getIdx) {
  if (st.type === "bubble") stepBubble(st, getIdx);
  else if (st.type === "selection") stepSelection(st, getIdx);
  else if (st.type === "insertion") stepInsertion(st, getIdx);
  else if (st.type === "quick") stepQuick(st, getIdx);
}

// -------------------------------------------------------------------------
// Advance all rows / columns by `speed` steps
// -------------------------------------------------------------------------

function advanceSort() {
  if (paused || sortComplete) return;
  let allDone = true;

  for (let y = 0; y < rowStates.length; y++) {
    const st = rowStates[y];
    if (st.done) continue;
    allDone = false;
    const getIdx = (pos) => rowIdx(y, pos);
    for (let s = 0; s < speed; s++) {
      stepSort(st, getIdx);
      if (st.done) break;
    }
  }

  for (let x = 0; x < colStates.length; x++) {
    const st = colStates[x];
    if (st.done) continue;
    allDone = false;
    const getIdx = (pos) => colIdx(x, pos);
    for (let s = 0; s < speed; s++) {
      stepSort(st, getIdx);
      if (st.done) break;
    }
  }

  if (allDone && (rowStates.length > 0 || colStates.length > 0))
    sortComplete = true;
}

// -------------------------------------------------------------------------
// Drawing helpers (for "draw" source mode)
// -------------------------------------------------------------------------

function drawPixel(x, y) {
  x = floor(x);
  y = floor(y);
  if (x < 0 || x >= bufW || y < 0 || y >= bufH) return;
  const radius = 3;
  for (let dy = -radius; dy <= radius; dy++) {
    for (let dx = -radius; dx <= radius; dx++) {
      const px = x + dx,
        py = y + dy;
      if (
        px >= 0 &&
        px < bufW &&
        py >= 0 &&
        py < bufH &&
        dx * dx + dy * dy <= radius * radius
      ) {
        const idx = (py * bufW + px) * 4;
        buf[idx] = drawColor[0];
        buf[idx + 1] = drawColor[1];
        buf[idx + 2] = drawColor[2];
        buf[idx + 3] = 255;
      }
    }
  }
}

function drawLineOnBuf(x0, y0, x1, y1) {
  x0 = floor(x0);
  y0 = floor(y0);
  x1 = floor(x1);
  y1 = floor(y1);
  const dx = abs(x1 - x0),
    dy = abs(y1 - y0);
  const sx = x0 < x1 ? 1 : -1,
    sy = y0 < y1 ? 1 : -1;
  let err = dx - dy;
  while (true) {
    drawPixel(x0, y0);
    if (x0 === x1 && y0 === y1) break;
    const e2 = 2 * err;
    if (e2 > -dy) {
      err -= dy;
      x0 += sx;
    }
    if (e2 < dx) {
      err += dx;
      y0 += sy;
    }
  }
}

// -------------------------------------------------------------------------
// Lifecycle
// -------------------------------------------------------------------------

function boot({ screen, params }) {
  bufW = screen.width;
  bufH = screen.height;
  buf = new Uint8ClampedArray(bufW * bufH * 4);
  damageMap = new Uint8Array(bufW * bufH);

  // Parse params: morpho:camera, morpho:gradient-h, etc.
  if (params[0]) {
    const p = params[0].toLowerCase();
    const idx = SOURCES.indexOf(p);
    if (idx >= 0) sourceIdx = idx;
    if (p === "cam") sourceIdx = SOURCES.indexOf("camera");
  }

  generateSource();
  initSortState();
}

function paint({ screen, ink, video }) {
  const { width: w, height: h, pixels } = screen;

  // Reinitialize on resize
  if (w !== bufW || h !== bufH) {
    bufW = w;
    bufH = h;
    buf = new Uint8ClampedArray(bufW * bufH * 4);
    damageMap = new Uint8Array(bufW * bufH);
    vid = null;
    cameraReady = false;
    generateSource();
    initSortState();
  }

  // Camera source: capture frame into buf
  if (SOURCES[sourceIdx] === "camera") {
    if (!vid) {
      vid = video("camera", { width: w, height: h, facing: "environment" });
    }
    if (!cameraReady) {
      const frame = vid(function shader(_, c) {});
      if (frame && frame.pixels && frame.pixels.length === buf.length) {
        buf.set(frame.pixels);
        cameraReady = true;
        initSortState();
      } else if (frame && frame.pixels) {
        // Size mismatch — copy what we can
        const len = min(buf.length, frame.pixels.length);
        for (let i = 0; i < len; i++) buf[i] = frame.pixels[i];
        cameraReady = true;
        initSortState();
      }
    }
  }

  // Advance sorting
  advanceSort();

  // Copy buffer to screen
  if (buf.length === pixels.length) {
    pixels.set(buf);
  }

  // --- HUD ---
  const algo = ALGORITHMS[algoIdx];
  const src = SOURCES[sourceIdx];
  const key = KEYS[keyIdx];
  const dir = DIRECTIONS[dirIdx];

  ink(255, 255, 255, 200).write(algo, { x: 6, y: 6 });
  ink(180, 180, 200, 180).write(`${src} / ${key} / ${dir}`, { x: 6, y: 18 });
  ink(180, 180, 200, 180).write(`threshold:${threshold} speed:${speed}`, {
    x: 6,
    y: 30,
  });

  if (paused) ink(255, 200, 0).write("PAUSED", { x: 6, y: 42 });
  if (sortComplete) ink(0, 255, 120).write("SORTED", { x: 6, y: 42 });
  if (damageEnabled) ink(255, 80, 80).write("DAMAGE", { x: 6, y: 54 });

  // Bottom hint
  ink(100, 100, 120, 160).write(
    "S:src A:algo K:key D:dir T/G:thresh +/-:spd X:dmg R:reset",
    { x: 6, y: h - 12 },
  );
}

function act({ event: e, screen }) {
  // Algorithm
  if (e.is("keyboard:down:a")) {
    algoIdx = (algoIdx + 1) % ALGORITHMS.length;
    initSortState();
  }

  // Source
  if (e.is("keyboard:down:s")) {
    sourceIdx = (sourceIdx + 1) % SOURCES.length;
    vid = null;
    cameraReady = false;
    generateSource();
    initSortState();
  }

  // Sort key
  if (e.is("keyboard:down:k")) {
    keyIdx = (keyIdx + 1) % KEYS.length;
    initSortState();
  }

  // Direction
  if (e.is("keyboard:down:d")) {
    dirIdx = (dirIdx + 1) % DIRECTIONS.length;
    initSortState();
  }

  // Threshold
  if (e.is("keyboard:down:t")) {
    threshold = min(255, threshold + 16);
    initSortState();
  }
  if (e.is("keyboard:down:g")) {
    threshold = max(0, threshold - 16);
    initSortState();
  }

  // Speed
  if (e.is("keyboard:down:=")) {
    speed = min(128, speed * 2);
  }
  if (e.is("keyboard:down:-")) {
    speed = max(1, floor(speed / 2));
  }

  // Pause
  if (e.is("keyboard:down:space")) {
    paused = !paused;
  }

  // Damage
  if (e.is("keyboard:down:x")) {
    damageEnabled = !damageEnabled;
    if (damageEnabled) {
      const count = floor(bufW * bufH * 0.02);
      for (let i = 0; i < count; i++) {
        damageMap[floor(random() * bufW * bufH)] = 1;
      }
    } else {
      damageMap.fill(0);
    }
    initSortState();
  }

  // Reset
  if (e.is("keyboard:down:r")) {
    vid = null;
    cameraReady = false;
    generateSource();
    initSortState();
  }

  // Drawing (in draw mode)
  if (SOURCES[sourceIdx] === "draw") {
    if (e.is("touch")) {
      penDown = true;
      // Randomize brush color per stroke
      drawColor = hslToRgb(random(), 0.7 + random() * 0.3, 0.4 + random() * 0.3);
      lastPenX = e.x;
      lastPenY = e.y;
      drawPixel(e.x, e.y);
    }
    if (e.is("draw") && penDown) {
      drawLineOnBuf(lastPenX, lastPenY, e.x, e.y);
      lastPenX = e.x;
      lastPenY = e.y;
    }
    if (e.is("lift")) {
      penDown = false;
    }
  }

  // Touch to place damage
  if (damageEnabled && SOURCES[sourceIdx] !== "draw") {
    if (e.is("touch") || e.is("draw")) {
      const r = 6;
      for (let dy = -r; dy <= r; dy++) {
        for (let dx = -r; dx <= r; dx++) {
          const px = floor(e.x) + dx,
            py = floor(e.y) + dy;
          if (
            px >= 0 &&
            px < bufW &&
            py >= 0 &&
            py < bufH &&
            dx * dx + dy * dy <= r * r
          ) {
            damageMap[py * bufW + px] = 1;
          }
        }
      }
    }
  }
}

export { boot, paint, act };
export const noBios = true;
