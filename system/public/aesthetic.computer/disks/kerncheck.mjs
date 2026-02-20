// Kerncheck, 2026.02.20
// Kerning & advance overlap detector for AC fonts.
// Red dots mark where adjacent characters are tightest or collide.

/* #region 📚 README
  Heatmap: every cell = visual pixel gap for that character pair.
  Red=overlap, orange=touching, amber=1px, green=2px+
  Tab: cycle fonts. Space: pause/resume.  ← →: navigate pairs.
  Click a grid cell to jump to that pair.
#endregion */

const CHARS =
  '!"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
const N = CHARS.length; // 94

const FONTS = [
  { name: "MatrixChunky8", label: "MatrixChunky8", color: [100, 220, 160] },
  { name: "font_1",        label: "font_1",        color: [255, 210, 55]  },
  { name: "unifont",       label: "unifont",        color: [180, 140, 255] },
];

const CELL      = 3;    // px per heatmap cell
const ZOOM      = 8;    // px per glyph pixel in zoom view
const P         = 10;   // screen padding
const PAIR_CAP  = 500;  // max pairs in spotlight list
const SHOW_BELOW = 1;   // pairs with gap <= this go in spotlight

let fontIdx    = 0;
let gStore     = {};    // { fontName: { char: glyphData } }
let gapGrid    = {};    // { fontName: Int8Array(N*N) }  visual gap (127=unscanned)
let done       = {};    // { fontName: Uint8Array(N*N) }
let scanPos    = 0;
let scanPass   = 0;     // retry pass counter (up to 3 retries for async glyph loads)
let scanning   = true;
let pairIdx    = 0;
let tightPairs = [];    // flat indices with gap <= SHOW_BELOW
let autoPlay   = true;
let frame      = 0;
let gridY0     = 40;    // y of heatmap top (set in paint)

// 🥾 Boot
function boot({ glyphs, text, cursor }) {
  cursor("native");
  for (const f of FONTS) {
    gStore[f.name]  = {};
    gapGrid[f.name] = new Int8Array(N * N).fill(127);
    done[f.name]    = new Uint8Array(N * N);
    // Prime typeface so its glyph Proxy is ready, then trigger async loads.
    text.width("A", f.name);
    const g = glyphs(f.name);
    for (const ch of CHARS) {
      const gd = g[ch];
      if (gd?.resolution && !gd.isPlaceholder) gStore[f.name][ch] = gd;
    }
  }
}

// 🧮 Sim
function sim({ glyphs, needsPaint }) {
  frame++;
  const fontName = FONTS[fontIdx].name;
  const g = glyphs(fontName);
  const BATCH = 50;

  if (scanning) {
    let worked = false;
    for (let b = 0; b < BATCH; b++) {
      if (scanPos >= N * N) break;
      const ai = Math.floor(scanPos / N);
      const bi = scanPos % N;
      const charA = CHARS[ai];
      const charB = CHARS[bi];

      let gA = gStore[fontName][charA];
      let gB = gStore[fontName][charB];
      if (!gA) {
        const raw = g[charA];
        if (raw?.resolution && !raw.isPlaceholder) gA = gStore[fontName][charA] = raw;
      }
      if (!gB) {
        const raw = g[charB];
        if (raw?.resolution && !raw.isPlaceholder) gB = gStore[fontName][charB] = raw;
      }

      if (gA && gB) {
        gapGrid[fontName][scanPos] = Math.max(-128, Math.min(126, computeGap(gA, gB)));
        done[fontName][scanPos] = 1;
        worked = true;
      }
      scanPos++;
    }

    // When we've gone through all positions, check if any remain unscanned.
    // If so, retry (up to 3 passes) to catch late-loading glyphs.
    if (scanPos >= N * N && scanning) {
      const stillPending = done[fontName].indexOf(0) >= 0;
      if (stillPending && scanPass < 3) {
        scanPos = 0;
        scanPass++;
      } else {
        scanning = false;
        tightPairs = buildTightList(fontName);
      }
    }

    if (worked) needsPaint();
  } else if (autoPlay && tightPairs.length > 0 && frame % 55 === 0) {
    pairIdx = (pairIdx + 1) % tightPairs.length;
    needsPaint();
  }
}

// 🎨 Paint
function paint({ wipe, ink, write, screen, box, line }) {
  wipe(6, 6, 10);
  const W = screen.width;

  // ── header ──────────────────────────────────────────────────────────
  let y = P;
  ink(255, 50, 60).write("kerncheck", P, y); y += 13;

  let fx = P;
  for (let f = 0; f < FONTS.length; f++) {
    const fc = FONTS[f];
    const act = f === fontIdx;
    ink(...(act ? fc.color : [40, 42, 56]));
    write(fc.label, fx, y, undefined, false, act ? fc.name : undefined);
    fx += 82;
  }
  y += 13;
  ink(18, 20, 32).line(P, y, W - P, y);
  gridY0 = y + 4;
  y = gridY0;

  // ── heatmap grid ────────────────────────────────────────────────────
  const fontName = FONTS[fontIdx].name;
  const gx = Math.max(P, Math.floor((W - N * CELL) / 2));

  for (let i = 0; i < N; i++) {
    for (let j = 0; j < N; j++) {
      const idx = i * N + j;
      const gap = done[fontName][idx] ? gapGrid[fontName][idx] : 127;
      ink(...gapColor(gap)).box(gx + j * CELL, y + i * CELL, CELL, CELL);
    }
  }

  // crosshair on selected pair
  if (tightPairs.length > 0) {
    const pi   = pairIdx % tightPairs.length;
    const flat = tightPairs[pi];
    const si = Math.floor(flat / N);
    const sj = flat % N;
    ink(255, 255, 255, 60).line(gx, y + si * CELL + 1, gx + N * CELL, y + si * CELL + 1);
    ink(255, 255, 255, 60).line(gx + sj * CELL + 1, y, gx + sj * CELL + 1, y + N * CELL);
    ink(255, 70, 70).box(gx + sj * CELL - 1, y + si * CELL - 1, CELL + 2, CELL + 2);
  }

  y += N * CELL + 5;
  ink(18, 20, 32).line(P, y, W - P, y);
  y += 5;

  // ── status / pair spotlight ─────────────────────────────────────────
  if (scanning) {
    const pct = Math.floor((scanPos / (N * N)) * 100);
    ink(40, 44, 62).write(`scanning ${pct}%`, P, y);
    return;
  }

  if (tightPairs.length === 0) {
    ink(50, 200, 80).write("all pairs: comfortable spacing", P, y);
    return;
  }

  const pi    = pairIdx % tightPairs.length;
  const flat  = tightPairs[pi];
  const ai    = Math.floor(flat / N);
  const bi    = flat % N;
  const charA = CHARS[ai];
  const charB = CHARS[bi];
  const gap   = gapGrid[fontName][flat];

  const gapStr = gap < 0 ? `${gap}px OVERLAP` : gap === 0 ? `0px touching` : `${gap}px`;
  ink(200, 200, 220)
    .write(`"${charA}${charB}"  ${gapStr}  (${pi + 1}/${tightPairs.length})`, P, y);
  y += 12;

  const gA = gStore[fontName][charA];
  const gB = gStore[fontName][charB];
  if (gA && gB) drawZoom(gA, gB, P, y, gap, ink, box, line);
}

// ── gap → color mapping ──────────────────────────────────────────────
function gapColor(gap) {
  if (gap === 127)  return [12, 13, 20];    // unscanned
  if (gap < 0)      return [220, 30, 30];   // overlap   – red
  if (gap === 0)    return [240, 100, 20];  // touching  – orange
  if (gap === 1)    return [210, 175, 30];  // 1px gap   – amber
  if (gap === 2)    return [70, 160, 70];   // 2px gap   – green
  return                  [22, 70, 30];    // 3+ px gap – dark green
}

// ── zoomed pair inspector ────────────────────────────────────────────
function drawZoom(gA, gB, x, y, gap, ink, box, line) {
  const S    = ZOOM;
  const advA = gA.advance ?? gA.dwidth?.x ?? gA.resolution[0];
  const oAx  = gA.offset?.[0] ?? 0;
  const oBx  = gB.offset?.[0] ?? 0;
  const rmA  = rightmost(gA);
  const lmB  = leftmost(gB);
  const base = y + 10 * S;  // baseline y (bottom of glyph rows)

  // Char A in blue (overlap columns in red)
  const pA = getPixels(gA);
  if (pA) {
    for (let row = 0; row < pA.length; row++) {
      for (let col = 0; col < pA[row].length; col++) {
        if (!pA[row][col]) continue;
        const sx = x + (oAx + col) * S;
        const sy = base - (pA.length - 1 - row) * S;
        const penCol = oAx + col;
        const isOv   = gap < 0 && penCol >= advA + oBx + lmB;
        const isEdge = penCol === oAx + rmA;
        ink(...(isOv ? [255, 40, 40] : isEdge ? [140, 200, 255] : [70, 130, 220]))
          .box(sx, sy, S - 1, S - 1);
      }
    }
  }

  // Advance divider line
  const advLineX = x + advA * S;
  ink(255, 255, 255, 45).line(advLineX, y - 2, advLineX, base + S + 5);

  // Char B in orange (overlap columns in red)
  const pB = getPixels(gB);
  const bX = x + advA * S;
  if (pB) {
    for (let row = 0; row < pB.length; row++) {
      for (let col = 0; col < pB[row].length; col++) {
        if (!pB[row][col]) continue;
        const sx = bX + (oBx + col) * S;
        const sy = base - (pB.length - 1 - row) * S;
        const penCol = advA + oBx + col;
        const isOv   = gap < 0 && penCol <= oAx + rmA;
        const isEdge = oBx + col === lmB;
        ink(...(isOv ? [255, 40, 40] : isEdge ? [255, 210, 140] : [255, 155, 50]))
          .box(sx, sy, S - 1, S - 1);
      }
    }
  }

  // Gap / overlap dot markers between the two characters
  const rEdge = oAx + rmA;         // rightmost A pixel in pen space
  const lEdge = advA + oBx + lmB;  // leftmost  B pixel in pen space
  const colMin = Math.min(rEdge, lEdge);
  const colMax = Math.max(rEdge, lEdge);

  for (let col = colMin; col <= colMax; col++) {
    const dotX = x + col * S + Math.floor(S / 2);
    const dotY1 = y - 4;
    const dotY2 = base + S + 3;
    let clr;
    if (gap < 0)      clr = [255, 40, 40];   // overlap – red
    else if (gap === 0) clr = [255, 110, 30]; // touching – orange
    else              clr = [210, 175, 30];   // gap – amber

    // Three-pixel wide dot cluster
    ink(...clr).line(dotX - 1, dotY1, dotX + 1, dotY1);
    ink(...clr).line(dotX - 1, dotY2, dotX + 1, dotY2);
    const dimClr = clr.map(v => Math.round(v * 0.55));
    ink(...dimClr).line(dotX, dotY1 - 1, dotX, dotY1 - 1);
    ink(...dimClr).line(dotX, dotY2 + 1, dotX, dotY2 + 1);
  }
}

// 🎪 Act
function act({ event: e, screen }) {
  if (e.is("keyboard:down:tab")) { fontIdx = (fontIdx + 1) % FONTS.length; resetScan(); }
  if (e.is("keyboard:down: "))   autoPlay = !autoPlay;

  if (!scanning && tightPairs.length > 0) {
    if (e.is("keyboard:down:arrowleft")) {
      pairIdx = (pairIdx - 1 + tightPairs.length) % tightPairs.length;
      autoPlay = false;
    }
    if (e.is("keyboard:down:arrowright")) {
      pairIdx = (pairIdx + 1) % tightPairs.length;
      autoPlay = false;
    }
  }

  // Click on grid to jump to pair
  if (e.is("touch") && !scanning) {
    const fontName = FONTS[fontIdx].name;
    const gx = Math.max(P, Math.floor((screen.width - N * CELL) / 2));
    const gi = Math.floor((e.y - gridY0) / CELL);
    const gj = Math.floor((e.x - gx) / CELL);
    if (gi >= 0 && gi < N && gj >= 0 && gj < N) {
      const flat = gi * N + gj;
      if (done[fontName][flat]) {
        const idx = tightPairs.indexOf(flat);
        if (idx >= 0) { pairIdx = idx; autoPlay = false; }
      }
    }
  }
}

// 📰 Meta
function meta() {
  return { title: "kerncheck", desc: "Kerning & advance overlap detector for AC fonts." };
}

export { boot, sim, paint, act, meta };
export const nohud = true;

// ─── helpers ────────────────────────────────────────────────────────

function resetScan() {
  scanPos = 0;
  scanPass = 0;
  scanning = true;
  tightPairs = [];
  pairIdx = 0;
  const fontName = FONTS[fontIdx].name;
  gapGrid[fontName] = new Int8Array(N * N).fill(127);
  done[fontName]    = new Uint8Array(N * N);
  // Glyph loading for the new font is triggered from sim on next frame.
}

function buildTightList(fontName) {
  const list = [];
  for (let i = 0; i < N * N; i++) {
    if (done[fontName][i] && gapGrid[fontName][i] <= SHOW_BELOW) list.push(i);
  }
  // Sort tightest first, then alphabetically
  list.sort((a, b) => {
    const gDiff = gapGrid[fontName][a] - gapGrid[fontName][b];
    return gDiff !== 0 ? gDiff : a - b;
  });
  // Cap list size so auto-cycle is manageable
  if (list.length > PAIR_CAP) list.length = PAIR_CAP;
  return list;
}

// Visual pixel gap: how many pixels of empty space between A's rightmost
// rendered pixel and B's leftmost rendered pixel.
// Negative = overlap, 0 = touching, positive = space.
function computeGap(gA, gB) {
  const advA = gA.advance ?? gA.dwidth?.x ?? gA.resolution?.[0] ?? 4;
  const oAx  = gA.offset?.[0] ?? 0;
  const oBx  = gB.offset?.[0] ?? 0;
  const rmA  = rightmost(gA);
  const lmB  = leftmost(gB);
  if (rmA < 0) return 10;                     // blank glyph
  const rightEdge_A = oAx + rmA;              // in pen space
  const leftEdge_B  = advA + oBx + lmB;       // in pen space
  return leftEdge_B - rightEdge_A - 1;
}

// Rightmost set pixel column index within the glyph's BBX.
function rightmost(glyph) {
  if (glyph.pixels) {
    let rm = -1;
    for (const row of glyph.pixels)
      for (let c = row.length - 1; c > rm; c--)
        if (row[c]) { rm = c; break; }
    return rm;
  }
  if (glyph.commands) {
    let rm = -1;
    for (const cmd of glyph.commands) {
      if (cmd.name === "point")
        rm = Math.max(rm, cmd.args[0]);
      else if (cmd.name === "line")
        rm = Math.max(rm, cmd.args[0], cmd.args[2]);
    }
    return rm;
  }
  return (glyph.resolution?.[0] ?? 1) - 1;
}

// Leftmost set pixel column index within the glyph's BBX.
function leftmost(glyph) {
  if (glyph.pixels) {
    let lm = Infinity;
    for (const row of glyph.pixels)
      for (let c = 0; c < row.length; c++)
        if (row[c]) { lm = Math.min(lm, c); break; }
    return lm === Infinity ? 0 : lm;
  }
  if (glyph.commands) {
    let lm = Infinity;
    for (const cmd of glyph.commands) {
      if (cmd.name === "point")
        lm = Math.min(lm, cmd.args[0]);
      else if (cmd.name === "line")
        lm = Math.min(lm, cmd.args[0], cmd.args[2]);
    }
    return lm === Infinity ? 0 : lm;
  }
  return 0;
}

// Convert a glyph's pixel data (commands or pixels array) to a 2D array.
// Row 0 = topmost row. Returns null if no data.
function getPixels(glyph) {
  if (glyph.pixels) return glyph.pixels;
  if (!glyph.commands || !glyph.resolution) return null;
  const [w, h] = glyph.resolution;
  const px = Array.from({ length: h }, () => Array(w).fill(0));
  for (const cmd of glyph.commands) {
    if (cmd.name === "point") {
      const [cx, cy] = cmd.args;
      if (cx >= 0 && cx < w && cy >= 0 && cy < h) px[cy][cx] = 1;
    } else if (cmd.name === "line") {
      const [x1, y1, x2, y2] = cmd.args;
      if (y1 === y2) {
        for (let cx = Math.min(x1, x2); cx <= Math.max(x1, x2); cx++)
          if (cx >= 0 && cx < w && y1 >= 0 && y1 < h) px[y1][cx] = 1;
      } else if (x1 === x2) {
        for (let cy = Math.min(y1, y2); cy <= Math.max(y1, y2); cy++)
          if (x1 >= 0 && x1 < w && cy >= 0 && cy < h) px[cy][x1] = 1;
      }
    }
  }
  return px;
}
