// 📊 The fps HUD widget, one for every surface (disk.mjs for pieces and the
// prompt, rax/view.mjs for rax). Each samples only its own paints and hands
// over its own rows; this file owns how it looks and where it sits.
//
// A row is a meter { label, value (0–1), text }, drawn as a short label, a
// bar and its number, or a plain string for a line of words. The widget hangs
// in the top-right corner, `margin` from the edges, and fits the room it is
// given: on a narrow screen the graph and bars get shorter, on a short one
// the bottom rows go first, and with no room at all only the fps shows.
const WIDE = 74, NARROW = 30; // widget widths, in pixels of its own
const HEAD = 11, GRAPH_H = 12, ROW = 8, PAD = 3;
const green = 0x77dda2, amber = 0xffcf69, red = 0xff8580;
const back = 0x131a24, well = 0x243245, dim = 0x8796a8, words = 0xc5d5e5;
const frameInk = ms => ms <= 18 ? green : ms <= 34 ? amber : red;
const meterInk = v => v < 0.6 ? green : v < 0.85 ? amber : red;
const MAX_COLUMNS = WIDE - 18;

export function createFrameChart() {
  const gaps = new Float32Array(MAX_COLUMNS);
  let head = 0, count = 0, previous = null, elapsed = 0;
  function reset() {
    head = count = elapsed = 0;
    previous = null;
  }
  function sample(now) {
    if (previous !== null && now > previous) {
      const gap = now - previous;
      if (count === MAX_COLUMNS) elapsed -= gaps[head];
      gaps[head] = gap;
      elapsed += gaps[head];
      head = (head + 1) % MAX_COLUMNS;
      count = Math.min(MAX_COLUMNS, count + 1);
    }
    previous = now;
  }

  // Where the widget goes and what fits, for a screen `screenW` × `screenH`
  // in the widget's pixels. `top` keeps it under whatever else owns the
  // top-right (rax's hints); `margin` is the gap from the edges.
  function layout(rows = [], { screenW = Infinity, screenH = Infinity, top = 6, margin = 6 } = {}) {
    const width = Math.max(NARROW, Math.min(WIDE, Math.floor(screenW - margin * 2)));
    margin = Math.max(0, Math.min(margin, Math.floor((screenW - width) / 2)));
    const room = screenH - top - margin;
    const graph = room >= HEAD + GRAPH_H + PAD * 2 && width >= 34;
    let height = graph ? HEAD + GRAPH_H + PAD * 2 : HEAD + 1;
    const kept = [];
    for (const row of rows) {
      if (!graph || height + ROW > room) break;
      kept.push(row);
      height += ROW;
    }
    return {
      width, height, rows: kept, graph,
      left: Math.max(0, screenW - width - margin),
      top: Math.max(0, Math.min(top, screenH - height)),
    };
  }

  // Draws at (0, 0) of the widget; the caller's `box` and `text` move it to
  // `layout.left`, `layout.top`. Without a layout it draws at full size.
  function draw({ box, text }, rows = [], fit = layout(rows)) {
    const { width, height, graph } = fit;
    box(0, 0, width, height, back, 0);
    const mean = count ? elapsed / count : 0;
    text(mean ? Math.round(1000 / mean) + ' fps' : '– fps', PAD, 2, mean ? frameInk(mean) : dim);
    if (!graph) return;

    const columns = width - 18, graphY = HEAD + 1, graphX = PAD;
    box(graphX, graphY, columns, GRAPH_H, well, 1);
    const tall = gap => Math.min(GRAPH_H, Math.max(1, Math.round(gap / 50 * GRAPH_H)));
    const shown = Math.min(count, columns);
    for (let i = 0; i < shown; i++) {
      const gap = gaps[(head + MAX_COLUMNS - shown + i) % MAX_COLUMNS], h = tall(gap);
      box(graphX + columns - shown + i, graphY + GRAPH_H - h, 1, h, frameInk(gap), 2);
    }
    // The 60 tick sits outside the graph so it never hides a short frame.
    const y = graphY + GRAPH_H - tall(1000 / 60);
    box(graphX + columns, y, 2, 1, dim, 2);
    text('60', graphX + columns + 3, y - 3, dim);

    const barX = 18, barW = Math.max(6, width - barX - 18);
    fit.rows.forEach((row, i) => {
      const ry = HEAD + GRAPH_H + PAD * 2 + i * ROW - 1;
      if (typeof row === 'string') return text(row, PAD, ry, words);
      const value = Math.max(0, Math.min(1, row.value || 0));
      text(row.label, PAD, ry, dim);
      box(barX, ry + 2, barW, 3, well, 1);
      if (value > 0) box(barX, ry + 2, Math.max(1, Math.round(value * barW)), 3, row.ink ?? meterInk(value), 2);
      if (row.text) text(row.text, barX + barW + 2, ry, words);
    });
  }
  return { sample, reset, layout, draw, width: WIDE };
}
