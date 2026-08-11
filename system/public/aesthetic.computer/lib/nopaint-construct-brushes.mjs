// Faithful, deterministic score adapters for recovered Construct No Paint brushes.
// Constants are transcribed from the exported C3 expression table in
// nopaint.art/scripts/c3runtime.js (Grid Worm and Dark Window sections).

const freezePoints = (points) => Object.freeze(points.map((point) => Object.freeze(point)));
const clamp = (value, low, high) => Math.max(low, Math.min(high, value));

function hsv(h, s, v, alpha) {
  const i = Math.floor(h * 6);
  const f = h * 6 - i;
  const p = v * (1 - s);
  const q = v * (1 - f * s);
  const t = v * (1 - (1 - f) * s);
  const [r, g, b] = [[v, t, p], [q, v, p], [p, v, t], [p, q, v], [t, p, v], [v, p, q]][i % 6];
  return Object.freeze([Math.round(r * 255), Math.round(g * 255), Math.round(b * 255), alpha]);
}

export const gridWormProposal = Object.freeze({
  version: 1,
  slug: "grid-worm",
  label: "Grid Worm",
  compatible: true,
  source: Object.freeze({
    // division = choose(32, 64, 128, 256) is how many divisions the buffer is
    // cut into, NOT the size of a cell: width = GridWormBuffer.Width /
    // division. Reading it as a cell size gave 256px cells on a 256 painting,
    // which is why the worm used to render as two stray lines.
    divisions: Object.freeze([32, 64, 128, 256]),
    canvas: 256,
    wormLength: "division",         // wormLength = division
    loopRates: Object.freeze([[0.1, 0.09], [0.05, 0.1], [0.1, 0.05, 0.02]]),
    channelAlpha: Object.freeze([153, 204, 153]), // Construct 0.6, 0.8, 0.6
    blend: "exclusion",
    renderFunction: "DrawGridWorm",
    // Each GridWormSegments row is [column, row, colour] and DrawGridWorm
    // fills that one cell; the AC read draws the chain instead.
    reconstructed: Object.freeze(["cells drawn as a chain, not per-cell fills"]),
  }),
  generate({ random, width, height, base }) {
    const { divisions, canvas } = this.source;
    const division = divisions[Math.floor(random() * divisions.length)];
    const scale = Math.min(width, height) / canvas;
    const cell = Math.max(1, canvas / division * scale);
    const columns = Math.max(1, Math.ceil(width / cell));
    const rows = Math.max(1, Math.ceil(height / cell));
    const length = division; // wormLength = division
    let column = Math.floor(random() * columns);
    let row = Math.floor(random() * rows);
    const cells = [{ column, row }];
    // C3 selects signed neighbours and applies abs(mod(..., grid dimension)).
    for (let index = 1; index < length; index += 1) {
      if (random() < 0.5) column = Math.abs((column + (random() < 0.5 ? -1 : 1)) % columns);
      else row = Math.abs((row + (random() < 0.5 ? -1 : 1)) % rows);
      cells.push({ column, row });
    }
    const colors = Object.freeze([
      hsv(random(), 0.8 + random() * 0.2, 0.35 + random() * 0.35, 153),
      hsv(random(), 0.8 + random() * 0.1, 0.2 + random() * 0.2, 204),
      hsv(random(), 0.8 + random() * 0.2, 0.75 + random() * 0.25, 153),
    ]);
    return Object.freeze({
      ...base,
      kind: this.slug,
      color: colors[0],
      division,
      gridSize: cell,
      cells: freezePoints(cells),
      colors,
      brush: Object.freeze({
        slug: this.slug,
        params: Object.freeze([String(division)]),
        colon: Object.freeze([]),
        parameters: Object.freeze({ division, cell, columns, rows, length, blend: "exclusion" }),
      }),
    });
  },
  render({ ink }, score, frame) {
    const center = (cell) => ({
      x: cell.column * score.gridSize + score.gridSize / 2,
      y: cell.row * score.gridSize + score.gridSize / 2,
    });
    for (let channel = 0; channel < 3; channel += 1) {
      const rates = this.source.loopRates[channel];
      // The loop rates crawl each exclusion channel around the worm at its own
      // speed; they do not reveal it. The worm is `division` segments long from
      // the first frame — drawing a growing prefix left it a dot.
      const offset = Math.floor(frame * rates[frame % rates.length]) % score.cells.length;
      for (let step = 1; step < score.cells.length; step += 1) {
        const from = center(score.cells[(offset + step - 1) % score.cells.length]);
        const to = center(score.cells[(offset + step) % score.cells.length]);
        ink(score.colors[channel]).line(from.x, from.y, to.x, to.y, Math.max(1, score.gridSize / 2));
      }
    }
  },
});

export const darkWindowProposal = Object.freeze({
  version: 1,
  slug: "dark-window",
  label: "Dark Window",
  compatible: true,
  source: Object.freeze({ windowCount: 2, rotateStep: 8, drift: 0.1, notes: 4 }),
  generate({ random, width, height, base }) {
    const note = Math.floor(random() * 4);
    const windows = freezePoints(Array.from({ length: 2 }, (_, index) => ({
      x: Math.floor(random() * width),
      y: Math.floor(random() * height),
      w: Math.max(16, Math.floor(width * (0.2 + random() * 0.35))),
      h: Math.max(16, Math.floor(height * (0.2 + random() * 0.35))),
      direction: index ? -1 : 1,
    })));
    return Object.freeze({
      ...base,
      kind: this.slug,
      windows,
      note,
      color: Object.freeze([Math.floor(random() * 256), Math.floor(random() * 256), Math.floor(random() * 256), 160]),
      brush: Object.freeze({
        slug: this.slug,
        params: Object.freeze([String(note + 1)]),
        colon: Object.freeze([]),
        parameters: Object.freeze({ note, noteLabel: `Dark Window - Note ${note + 1}`, windowCount: 2, rotateStep: 8, drift: 0.1 }),
      }),
    });
  },
  render({ ink }, score, frame) {
    score.windows.forEach((window, index) => {
      const angle = (frame * 0.1 * window.direction + index * Math.PI) * Math.PI / 180;
      const drift = frame * 0.1 * window.direction;
      const cx = window.x + drift;
      const cy = window.y - drift;
      const cosine = Math.cos(angle), sine = Math.sin(angle);
      const corners = [[-1, -1], [1, -1], [1, 1], [-1, 1]].map(([sx, sy]) => ({
        x: clamp(cx + sx * window.w / 2 * cosine - sy * window.h / 2 * sine, 0, 16384),
        y: clamp(cy + sx * window.w / 2 * sine + sy * window.h / 2 * cosine, 0, 16384),
      }));
      ink(score.color).poly(corners.map(({ x, y }) => [x, y]));
    });
  },
});

export const recoveredConstructProposals = Object.freeze([gridWormProposal, darkWindowProposal]);
