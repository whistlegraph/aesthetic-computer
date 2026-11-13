// map, 2026.11.13
// dool from above: every block, who lives in it and who is awake.

const BLOCK = 16; // metres along a block's edge
const LANE = 2;
const FONT = "MatrixChunky8";
const PROD = "https://aesthetic.computer/dool/geography";
const EVERY = 30000;
const DETAIL = 2; // pixels a metre at which lots are worth drawing

const DARK = { bg: [16, 14, 24], ink: [215, 212, 230], dim: [112, 108, 136], pink: [255, 105, 180], purple: [170, 125, 255], awake: [80, 255, 120], rest: [128, 124, 150] };
const LIGHT = { bg: [246, 244, 250], ink: [40, 36, 56], dim: [150, 146, 168], pink: [220, 60, 140], purple: [120, 70, 200], awake: [0, 165, 70], rest: [156, 152, 172] };

// The spiral and the lots are re-derived from duel/geography.mjs, which reads
// the disk and cannot be imported here. East is +x, north is +z.
const ring = (n) => Math.ceil((Math.sqrt(n + 1) - 1) / 2);

function blockAt(n) {
  if (n === 0) return { i: 0, j: 0 };
  const r = ring(n), side = 2 * r;
  let k = n - (2 * r - 1) ** 2;
  if (k < side) return { i: r, j: -r + 1 + k };
  if ((k -= side) < side) return { i: r - 1 - k, j: r };
  if ((k -= side) < side) return { i: -r, j: r - 1 - k };
  return { i: -r + 1 + (k - side), j: -r };
}

const blockId = ({ i, j }) => (i || j ? `${i < 0 ? "w" : "e"}${Math.abs(i)}${j < 0 ? "s" : "n"}${Math.abs(j)}` : "lobby");

function lot(k) {
  const half = BLOCK / 2, inner = LANE / 2, depth = (half - inner) / 2;
  const east = k < 4, north = k < 2 || k > 5;
  const x0 = inner + (k % 2 === (north === east ? 1 : 0) ? depth : 0);
  const [xMin, xMax] = east ? [x0, x0 + depth] : [-x0 - depth, -x0];
  const [zMin, zMax] = north ? [inner, half] : [-half, -inner];
  return { xMin, xMax, zMin, zMax };
}

// Shown only when the endpoint cannot be reached.
const SAMPLE = {
  settled: 3, opened: 1, lots: 8, span: { metres: 48, seconds: 5 }, block: BLOCK,
  lotRects: [0, 1, 2, 3, 4, 5, 6, 7].map(lot),
  blocks: [
    { n: 0, id: "lobby", i: 0, j: 0, homes: [] },
    { n: 1, id: "e1n0", i: 1, j: 0, homes: [
      { handle: "jeffrey", lot: 0, at: 1794600000000 },
      { handle: "prutti", lot: 1, at: 1794600000000 },
      { handle: "oskie", lot: 6, at: 1794600000000 },
    ] },
  ],
  awake: [{ handle: "jeffrey", room: "lobby" }],
  resting: [{ handle: "prutti", room: "e1n0", x: 3.1, z: -2, at: 1794600000000 }],
};

let geo, offline = false; // the census, and whether it is the sample
let cells = [], where = new Map(), lots = [], dots = [];
let view = { x: 0, z: 0, s: 4 }; // world metres at the screen's middle; pixels a metre
let moved = false; // once the view is moved by hand, new data stops refitting it
let scr, pick, cursor, pinch, timer;

// Every open block, then the rest of that ring and one more, faint.
function chart(data) {
  const open = new Map(data.blocks.map((b) => [b.n, b]));
  const byId = new Map();
  lots = data.lotRects || SAMPLE.lotRects;
  cells = [];
  where = new Map();
  for (let n = 0; n < (2 * ring(data.opened) + 3) ** 2; n++) {
    const b = open.get(n), cell = { n, ...blockAt(n), open: !!b, homes: [], people: [] };
    cell.id = blockId(cell);
    b?.homes.forEach((h) => (cell.homes[h.lot] = h));
    cells.push(cell);
    where.set(cell.i + "," + cell.j, cell);
    byId.set(cell.id, cell);
  }
  dots = [];
  const up = new Set();
  for (const a of data.awake || []) {
    const cell = byId.get(a.room);
    if (!cell) continue; // an older named room, outside the geography
    up.add(a.handle);
    // The endpoint gives the awake a room and no position: fan them out from
    // the block's middle so several stay countable.
    const k = cell.people.length;
    place(cell, a, true, a.x ?? Math.cos(k * 2.4) * 1.2 * Math.sqrt(k), a.z ?? Math.sin(k * 2.4) * 1.2 * Math.sqrt(k));
  }
  for (const r of data.resting || []) {
    const cell = byId.get(r.room);
    if (cell && !up.has(r.handle)) place(cell, r, false, r.x, r.z);
  }
  geo = data;
  if (!moved) fit();
  pick = cursor && scr ? hit(cursor.x, cursor.y) : null; // the old pick pointed into the old chart
}

function place(cell, who, awake, x, z) {
  const dot = { handle: who.handle, awake, x: cell.i * BLOCK + x, z: cell.j * BLOCK + z };
  cell.people.push(dot);
  dots.push(dot);
}

async function load() {
  const here = typeof location !== "undefined" && /(^|\.)aesthetic\.computer$/.test(location.hostname);
  try {
    const res = await fetch(here ? "/dool/geography" : PROD);
    if (!res.ok) throw new Error(res.status);
    const data = await res.json();
    offline = false;
    chart(data);
  } catch (err) {
    if (geo) return; // keep the last census through a dropped poll
    offline = true;
    chart(SAMPLE);
  }
}

function fit() {
  if (!scr || !geo) return;
  const across = (2 * ring(geo.opened) + 1.5) * BLOCK; // a sliver of the unopened ring shows
  view = { x: 0, z: 0, s: Math.min(scr.width, scr.height - 50) / across };
  moved = false;
}

function zoom(f, x = scr.width / 2, y = scr.height / 2) {
  const s = Math.min(48, Math.max(0.2, view.s * f)), dx = x - scr.width / 2, dy = y - scr.height / 2;
  view.x += dx / view.s - dx / s; // the point under the finger stays under it
  view.z -= dy / view.s - dy / s;
  view.s = s;
  moved = true;
}

const px = (x, z) => [Math.round(scr.width / 2 + (x - view.x) * view.s), Math.round(scr.height / 2 - (z - view.z) * view.s)];

// Neighbours round to the same pixel line, so shared edges draw once.
function rect(xMin, zMin, xMax, zMax) {
  const [l, b] = px(xMin, zMin), [r, t] = px(xMax, zMax);
  return [l, t, r - l + 1, b - t + 1];
}

function hit(x, y) {
  const wx = view.x + (x - scr.width / 2) / view.s, wz = view.z - (y - scr.height / 2) / view.s;
  const i = Math.floor(wx / BLOCK + 0.5), j = Math.floor(wz / BLOCK + 0.5);
  const cell = where.get(i + "," + j);
  if (!cell) return null;
  const lx = wx - i * BLOCK, lz = wz - j * BLOCK;
  const k = cell.open && cell.n ? lots.findIndex((r) => lx >= r.xMin && lx <= r.xMax && lz >= r.zMin && lz <= r.zMax) : -1;
  return { cell, lot: k };
}

const metres = (v) => +Math.abs(v).toFixed(2);
const address = (x, z) => `${metres(x)} m ${x < 0 ? "west" : "east"}, ${metres(z)} m ${z < 0 ? "south" : "north"}`;
const count = (n, word) => `${n} ${word}${n === 1 ? "" : "s"}`;

function readout({ cell, lot: k }) {
  const lines = [], x = cell.i * BLOCK, z = cell.j * BLOCK;
  if (k >= 0) {
    const r = lots[k], home = cell.homes[k];
    lines.push(`${cell.id} lot ${k}  ${home ? "@" + home.handle : "free"}`, address(x + (r.xMin + r.xMax) / 2, z + (r.zMin + r.zMax) / 2));
    if (home?.at) lines.push("since " + new Date(home.at).toISOString().slice(0, 10));
  } else {
    const state = !cell.n ? "the plaza" : cell.open ? `${cell.homes.filter(Boolean).length} of ${lots.length} settled` : "not open yet";
    lines.push(`${cell.id}  block ${cell.n}  ${state}`, address(x, z));
  }
  const names = (awake) => cell.people.filter((p) => p.awake === awake).map((p) => "@" + p.handle).join(" ");
  if (names(true)) lines.push("awake " + names(true));
  if (names(false)) lines.push("resting " + names(false));
  return lines;
}

function boot({ screen }) {
  scr = screen;
  load();
  timer = setInterval(load, EVERY);
}

function paint({ wipe, ink, box, circle, write, screen, dark, text }) {
  scr = screen;
  const c = dark ? DARK : LIGHT, { s } = view, B = BLOCK * s;
  const say = (color, words, x, y) => ink(...color).write(words, { x: Math.floor(x), y: Math.floor(y) }, undefined, undefined, false, FONT);
  const wide = (words) => text.width(words, FONT);
  const centred = (color, words, x, y) => say(color, words, x - wide(words) / 2, y);
  wipe(...c.bg);
  if (!geo) return;

  for (const cell of cells) {
    const x0 = cell.i * BLOCK, z0 = cell.j * BLOCK, half = BLOCK / 2;
    const [x, y, w, h] = rect(x0 - half, z0 - half, x0 + half, z0 + half);
    if (x > screen.width || y > screen.height || x + w < 0 || y + h < 0) continue;
    const lit = pick?.cell === cell && pick.lot < 0;
    if (!cell.n) {
      ink(...c.pink, 60).box(x, y, w, h);
      ink(...(lit ? c.ink : c.pink)).box(x, y, w, h, "outline");
      if (B >= 30) say(c.pink, "lobby", x + 3, y + 3);
    } else if (!cell.open) {
      ink(...c.dim, lit ? 255 : 90).box(x, y, w, h, "outline");
      if (B >= 14) centred(c.dim, String(cell.n), x + w / 2, y + h / 2 - (B >= 44 ? 9 : 4));
      if (B >= 44) centred([...c.dim, 160], cell.id, x + w / 2, y + h / 2 + 1);
    } else if (s < DETAIL) {
      // Too far out for lots: the block darkens as it fills, as in the paper's first figure.
      ink(...c.purple, 30 + (170 * cell.homes.filter(Boolean).length) / lots.length).box(x, y, w, h);
      ink(...(lit ? c.ink : c.purple)).box(x, y, w, h, "outline");
      if (B >= 14) centred(c.ink, String(cell.n), x + w / 2, y + h / 2 - 4);
    } else {
      lots.forEach((r, k) => {
        const home = cell.homes[k], [lx, ly, lw, lh] = rect(x0 + r.xMin, z0 + r.zMin, x0 + r.xMax, z0 + r.zMax);
        if (home) ink(...c.purple, 110).box(lx, ly, lw, lh);
        const on = pick?.cell === cell && pick.lot === k;
        ink(...(on ? c.ink : c.purple), on || home ? 255 : 70).box(lx, ly, lw, lh, "outline");
        if (home && lh >= 12 && wide("@" + home.handle) <= lw - 3) centred(c.ink, "@" + home.handle, lx + lw / 2, ly + lh / 2 - 4);
      });
      if (lit) ink(...c.ink).box(x, y, w, h, "outline");
      // The id rides the east-west lane, west of the crossing where the awake stand.
      if (LANE * s >= 9) centred(c.dim, cell.id, x + w / 4, y + h / 2 - 4);
    }
  }

  const r = Math.max(1, Math.min(3, Math.round(s / 3)));
  for (const d of dots) {
    const [x, y] = px(d.x, d.z);
    ink(...(d.awake ? c.awake : c.rest)).circle(x, y, r, true);
    if (s >= 8) say(d.awake ? c.awake : c.rest, "@" + d.handle, x + r + 2, y - 4);
  }

  // Readouts keep to the bottom and the right: the top-left is the label's.
  const long = `${count(geo.settled, "home")} settled  ${count(geo.opened, "block")} open  ${geo.span.metres} m across  ${geo.span.seconds} s to cross`;
  const status = wide(long) <= screen.width - 12 ? long : `${count(geo.settled, "home")}  ${count(geo.opened, "block")}  ${geo.span.metres} m  ${geo.span.seconds} s`;
  const lines = pick ? readout(pick) : [];
  const top = screen.height - 12 - lines.length * 9;
  ink(...c.bg, 215).box(0, top - 3, screen.width, screen.height - top + 3);
  lines.forEach((line, n) => say(c.ink, line, 6, top + n * 9));
  say(c.dim, status, 6, screen.height - 11);
  if (offline) say(c.dim, "offline sample", screen.width - wide("offline sample") - 6, 6);
}

function act({ event: e, screen, pens }) {
  scr = screen;
  if (!geo) return;
  if (e.is("reframed") && !moved) fit();
  if (e.is("move") || e.is("touch")) {
    cursor = { x: e.x, y: e.y };
    pick = hit(e.x, e.y);
  }
  if (e.is("touch") || e.is("lift")) pinch = null; // the fingers changed: measure the pinch afresh
  if (e.is("draw")) {
    const [a, b] = (pens?.() || []).filter((p) => p?.drawing);
    if (a && b) {
      const x = (a.x + b.x) / 2, y = (a.y + b.y) / 2, d = Math.hypot(a.x - b.x, a.y - b.y) || 1;
      if (pinch) {
        view.x -= (x - pinch.x) / view.s;
        view.z += (y - pinch.y) / view.s;
        zoom(d / pinch.d, x, y);
      }
      pinch = { x, y, d };
    } else {
      view.x -= e.delta.x / view.s;
      view.z += e.delta.y / view.s;
    }
    moved = true;
  }
  // The wheel carries no position, so it zooms on wherever the pointer last was.
  if (e.is("scroll")) zoom(Math.exp(-e.y * 0.004), cursor?.x, cursor?.y);
  if (e.is("keyboard:down:=") || e.is("keyboard:down:+")) zoom(1.25);
  if (e.is("keyboard:down:-")) zoom(0.8);
  if (e.is("keyboard:down:0")) fit();
}

function leave() {
  clearInterval(timer);
}

export { boot, paint, act, leave };
