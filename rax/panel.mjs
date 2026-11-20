// 🖼️ World panels: a piece running on a rectangle in a room, pointed at with
// the aim. This is rax's model for things in the world you can work: a panel
// is a small screen with a piece on it, and the crosshair is its mouse.
//
// A piece is shaped as an AC piece is (disks/CLAUDE.md): it exports `boot`,
// `paint`, `act`, and if it likes `sim` and `leave`, and each is handed an api
// to take what it uses from. The api here is a small, faithful part of the
// real one: `screen`, `wipe`, `ink`, `box`, `line`, `plot`, `write`, `text`,
// `pen`, and whatever the host mounts it with (the keypad is given `car`).
// `act({ event })` hears the pointer as AC says it: `event.is('move')`,
// `'touch'`, `'draw'` (moved while down) and `'lift'`, with `event.x` and
// `event.y` in the panel's own pixels. A real piece that keeps to these could
// be hosted as it is; disk.mjs is not loaded.
//
// The panels live in the page for now (client.mjs mounts the one there is).
// A placed object could carry one later: its record would name a piece and a
// face (`panel: { piece, w, h, cols, rows }`), the page would mount it where
// the object stands, and what must be shared (a number typed, a switch
// thrown) would go through the room as the object's own state.
import { glyphs, ascent } from './chunky-font.mjs';

const NAMED = {
  black: [0, 0, 0], white: [255, 255, 255], gray: [128, 128, 128], grey: [128, 128, 128],
  red: [255, 0, 0], green: [0, 128, 0], lime: [0, 255, 0], blue: [0, 0, 255],
  yellow: [255, 255, 0], cyan: [0, 255, 255], magenta: [255, 0, 255], orange: [255, 165, 0],
  pink: [255, 192, 203], purple: [128, 0, 128], brown: [165, 42, 42],
};
// A colour as AC's `ink` and `wipe` take it: r, g, b; [r, g, b]; one number
// for a grey; a name; or "#rrggbb".
export function colorOf(args) {
  const a = Array.isArray(args[0]) ? args[0] : args;
  if (typeof a[0] === 'string') {
    if (a[0][0] === '#') { const n = parseInt(a[0].slice(1), 16); return [(n >> 16) & 255, (n >> 8) & 255, n & 255]; }
    return NAMED[a[0].toLowerCase()] || NAMED.white;
  }
  if (a.length === 1) return [a[0], a[0], a[0]];
  return a.length ? [a[0], a[1], a[2]] : NAMED.white;
}

// How wide a line of text is, in pixels (MatrixChunky8, as AC has it).
const glyph = (ch) => glyphs[ch] || glyphs['?'];
export const textWidth = (text, size = 1) =>
  Math.max(0, [...String(text)].reduce((w, ch) => w + glyph(ch)[4], 0) - 1) * size;

// 🎨 A pixel buffer and the few things a piece draws on it with.
export function painter(width, height) {
  const pixels = new Uint8ClampedArray(width * height * 4).fill(255);
  let color = NAMED.white;
  const plot = (x, y) => {
    x = Math.floor(x); y = Math.floor(y);
    if (x < 0 || y < 0 || x >= width || y >= height) return;
    const k = (y * width + x) * 4;
    pixels[k] = color[0]; pixels[k + 1] = color[1]; pixels[k + 2] = color[2];
  };
  const fill = (x, y, w, h) => {
    const x1 = Math.min(width, Math.floor(x + w)), y1 = Math.min(height, Math.floor(y + h));
    for (let j = Math.max(0, Math.floor(y)); j < y1; j++)
      for (let i = Math.max(0, Math.floor(x)); i < x1; i++) plot(i, j);
  };
  const g = {
    screen: { width, height, pixels },
    ink: (...c) => { color = colorOf(c); return g; }, // (it chains: ink('red').box(…))
    wipe: (...c) => { if (c.length) color = colorOf(c); fill(0, 0, width, height); return g; },
    plot: (x, y) => { plot(x, y); return g; },
    box(x, y, w, h, mode = 'fill') {
      if (x && typeof x === 'object') { mode = y || 'fill'; ({ x, y, w, h } = x); } // (a Box, as AC's ui hands one: box(button.box, 'in'))
      if (mode === 'fill') fill(x, y, w, h);
      else { fill(x, y, w, 1); fill(x, y + h - 1, w, 1); fill(x, y, 1, h); fill(x + w - 1, y, 1, h); }
      return g;
    },
    line(x0, y0, x1, y1) {
      x0 = Math.floor(x0); y0 = Math.floor(y0); x1 = Math.floor(x1); y1 = Math.floor(y1);
      const dx = Math.abs(x1 - x0), dy = -Math.abs(y1 - y0), sx = x0 < x1 ? 1 : -1, sy = y0 < y1 ? 1 : -1;
      for (let e = dx + dy; ; ) {
        plot(x0, y0);
        if (x0 === x1 && y0 === y1) break;
        const e2 = 2 * e;
        if (e2 >= dy) { e += dy; x0 += sx; }
        if (e2 <= dx) { e += dx; y0 += sy; }
      }
      return g;
    },
    // write(text, { x, y, size, center }): the top left of the line, or
    // centred on the screen along 'x', 'y' or 'xy'.
    write(text, { x = 0, y = 0, size = 1, center = '' } = {}) {
      text = String(text);
      if (center.includes('x')) x = Math.floor((width - textWidth(text, size)) / 2);
      if (center.includes('y')) y = Math.floor((height - ascent * size) / 2);
      for (const ch of text) {
        const [w, h, xoff, yoff, advance, rows] = glyph(ch);
        for (let r = 0; r < h; r++)
          for (let c = 0; c < w; c++)
            if (rows[r] & (128 >> c)) fill(x + (xoff + c) * size, y + (ascent - yoff - h + r) * size, size, size);
        x += advance * size;
      }
      return g;
    },
    text: { width: textWidth, height: (size = 1) => ascent * size },
  };
  return g;
}

// A pointer event as a piece's `act` hears it.
export const pointerEvent = (name, x, y, device = 'mouse') =>
  ({ name, x, y, device, is: (q) => name === q || name.startsWith(`${q}:`) });

// 🖱️ Where a ray meets a panel's face, in the panel's pixels (x from its left
// edge, y from its top, as it is read), and how far along the ray: or null if
// it misses, comes from behind, or is further than `reach`. A panel is `w` by
// `h` metres about (x, y, z), `cols` by `rows` pixels, and faces south (-z)
// until turned by `yaw` degrees, as the view turns what it draws.
export function panelHit(panel, origin, dir, reach = Infinity) {
  const a = ((panel.yaw || 0) * Math.PI) / 180, c = Math.cos(a), s = Math.sin(a);
  const local = (x, z) => [x * c - z * s, x * s + z * c];
  const [ox, oz] = local(origin.x - panel.x, origin.z - panel.z), [dx, dz] = local(dir.x, dir.z);
  if (!(dz > 1e-6) || oz >= 0) return null;
  const t = -oz / dz;
  if (t > reach) return null;
  const x = ((panel.w / 2 - (ox + dx * t)) / panel.w) * panel.cols;
  const y = ((panel.h / 2 - (origin.y + dir.y * t - panel.y)) / panel.h) * panel.rows;
  return x >= 0 && y >= 0 && x < panel.cols && y < panel.rows ? { t, x, y } : null;
}

// One panel: a place, a pixel buffer, and a piece painting on it. `frame`
// counts the pictures that differed, so the view rebuilds only on a new one.
export function createPanel({ id, piece, cols = 64, rows = 96, host = {}, ...place }) {
  const g = painter(cols, rows), pen = { x: -1, y: -1, down: false };
  const api = { ...g, pen, ...host };
  let shown = null;
  const panel = {
    id, cols, rows, yaw: 0, deep: 0.04, ...place,
    pixels: g.screen.pixels, frame: 0, cursor: null, hidden: false, pen,
    act(name, x, y, device) {
      pen.x = x; pen.y = y;
      if (name === 'touch') pen.down = true;
      if (name === 'lift') pen.down = false;
      piece.act?.({ ...api, event: pointerEvent(name, Math.floor(x), Math.floor(y), device) });
    },
    step() {
      piece.sim?.(api);
      piece.paint?.(api);
      const px = g.screen.pixels;
      if (shown && shown.every((v, k) => v === px[k])) return;
      shown = px.slice();
      panel.frame++;
    },
    leave() { piece.leave?.(api); },
  };
  piece.boot?.(api);
  return panel;
}

// 🖱️ The page's panels and the one virtual mouse on them. Each frame the page
// says where its aim meets a panel (`point`); a press there is `press` then
// `release`, or `tap` for a key or a button that has no letting go.
export const PANEL_REACH = 3; // metres: an arm and a lean
const TAP_MS = 130;
export function createPanels() {
  const list = [];
  let hot = null, held = null, liftAt = 0, pinned = false;
  const off = (panel) => { panel.act('move', -1, -1); panel.cursor = null; };
  const panels = {
    list,
    get hot() { return hot; }, // { panel, x, y, t } under the aim now
    mount(def) { const p = createPanel(def); list.push(p); return p; },
    unmount(id) {
      const k = list.findIndex((p) => p.id === id);
      if (k < 0) return;
      const [p] = list.splice(k, 1);
      if (hot?.panel === p) hot = null;
      if (held === p) held = null;
      p.leave();
    },
    // The nearest panel a ray meets, within reach: { panel, x, y, t } or null.
    aim(origin, dir, reach = PANEL_REACH) {
      let best = null;
      for (const panel of list) {
        if (panel.hidden) continue;
        const hit = panelHit(panel, origin, dir, best ? best.t : reach);
        if (hit) best = { panel, ...hit };
      }
      return best;
    },
    // The mouse is here now (or nowhere). Off a panel's edge the piece hears
    // a move to (-1, -1), and a press held there is let go out in the open,
    // so the key it began on is not pressed.
    point(hit) {
      if (pinned) return; // a finger is on a panel: the crosshair waits
      if (hot && hot.panel !== hit?.panel) {
        if (held === hot.panel) { held.act('lift', -1, -1); held = null; }
        off(hot.panel);
      }
      hot = hit;
      if (!hit) return;
      hit.panel.act(held === hit.panel ? 'draw' : 'move', hit.x, hit.y);
      hit.panel.cursor = { x: hit.x, y: hit.y, down: held === hit.panel };
    },
    press() {
      if (!hot || held) return false;
      held = hot.panel;
      held.act('touch', hot.x, hot.y);
      if (held.cursor) held.cursor.down = true;
      return true;
    },
    release() {
      if (!held) return;
      held.act('lift', held.pen.x, held.pen.y);
      if (held.cursor) held.cursor.down = false;
      held = null;
      liftAt = 0;
      if (pinned) { pinned = false; off(hot.panel); hot = null; }
    },
    tap(now) {
      const pressed = panels.press();
      if (pressed) liftAt = now + TAP_MS;
      return pressed;
    },
    // A finger on the glass presses where it lands, not where the crosshair is.
    pressAt(hit) {
      if (!hit || held) return false;
      panels.point(hit);
      pinned = panels.press();
      return pinned;
    },
    get pinned() { return pinned; },
    frame(now) {
      if (liftAt && now >= liftAt) panels.release();
      for (const p of list) if (!p.hidden) p.step();
    },
  };
  return panels;
}
