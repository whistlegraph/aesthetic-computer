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
// for a grey; a name; or "#rrggbb"; each with an alpha after it if it likes
// (r, g, b, a; grey, a; name, a). → [r, g, b, a], a 255 when none is said.
export function colorOf(args) {
  const a = Array.isArray(args[0]) ? args[0] : args;
  if (typeof a[0] === 'string') {
    const alpha = a[1] ?? 255;
    if (a[0][0] === '#') { const n = parseInt(a[0].slice(1), 16); return [(n >> 16) & 255, (n >> 8) & 255, n & 255, alpha]; }
    return [...(NAMED[a[0].toLowerCase()] || NAMED.white), alpha];
  }
  if (a.length === 1) return [a[0], a[0], a[0], 255];
  if (a.length === 2) return [a[0], a[0], a[0], a[1]];
  return a.length ? [a[0], a[1], a[2], a[3] ?? 255] : [...NAMED.white, 255];
}

// How wide a line of text is, in pixels (MatrixChunky8, as AC has it).
const glyph = (ch) => glyphs[ch] || glyphs['?'];
export const textWidth = (text, size = 1) =>
  Math.max(0, [...String(text)].reduce((w, ch) => w + glyph(ch)[4], 0) - 1) * size;

// 🎨 A pixel buffer and the few things a piece draws on it with. A buffer is
// { width, height, pixels } (RGBA), as AC's `screen` and its paintings are.
export function painter(width, height) {
  const screen = { width, height, pixels: new Uint8ClampedArray(width * height * 4).fill(255) };
  let on = screen; // 📄 what is drawn on now: the screen, or a painting it was turned to (`page`)
  let color = [255, 255, 255, 255];
  // (A colour with an alpha is laid over what is there, which may itself be part clear, as a painting is; a whole one replaces it.)
  const put = (px, k, r, g, b, a) => {
    if (a >= 255) { px[k] = r; px[k + 1] = g; px[k + 2] = b; px[k + 3] = 255; return; }
    if (!(a > 0)) return;
    const under = px[k + 3] * (1 - a / 255), all = a + under;
    px[k] = (r * a + px[k] * under) / all; px[k + 1] = (g * a + px[k + 1] * under) / all; px[k + 2] = (b * a + px[k + 2] * under) / all;
    px[k + 3] = all;
  };
  const plot = (x, y) => {
    x = Math.floor(x); y = Math.floor(y);
    if (x < 0 || y < 0 || x >= on.width || y >= on.height) return;
    put(on.pixels, (y * on.width + x) * 4, color[0], color[1], color[2], color[3]);
  };
  const packedColor = new Uint8ClampedArray(4);
  const packedWord = new Uint32Array(packedColor.buffer);
  const wordFor = () => { packedColor.set(color); return packedWord[0]; };
  const words = () => new Uint32Array(on.pixels.buffer, on.pixels.byteOffset, on.pixels.length / 4);
  const fill = (x, y, w, h) => {
    const x1 = Math.min(on.width, Math.floor(x + w)), y1 = Math.min(on.height, Math.floor(y + h));
    const x0 = Math.max(0, Math.floor(x)), y0 = Math.max(0, Math.floor(y));
    if (x0 >= x1 || y0 >= y1) return;
    if (color[3] >= 255 && x1 - x0 >= 8 && on.pixels.byteOffset % 4 === 0) {
      const px = words(), c = wordFor();
      for (let j = y0; j < y1; j++) px.fill(c, j * on.width + x0, j * on.width + x1);
      return;
    }
    for (let j = y0; j < y1; j++)
      for (let i = Math.max(0, Math.floor(x)); i < x1; i++) plot(i, j);
  };
  const g = {
    screen,
    ink: (...c) => { color = colorOf(c); return g; }, // (it chains: ink('red').box(…))
    // (A wipe is not laid over: every pixel becomes the colour, its alpha too, so `wipe(0, 0, 0, 0)` clears a painting to nothing.)
    wipe: (...c) => {
      if (c.length) color = colorOf(c);
      if (on.pixels.byteOffset % 4 === 0) { words().fill(wordFor()); return g; }
      const px = on.pixels;
      for (let k = 0; k < px.length; k += 4) { px[k] = color[0]; px[k + 1] = color[1]; px[k + 2] = color[2]; px[k + 3] = color[3]; }
      return g;
    },
    plot: (x, y) => { plot(x, y); return g; },
    box(x, y, w, h, mode = 'fill') {
      if (x && typeof x === 'object') { mode = y || 'fill'; ({ x, y, w, h } = x); } // (a Box, as AC's ui hands one: box(button.box, 'in'))
      if (mode === 'fill') fill(x, y, w, h);
      else { fill(x, y, w, 1); fill(x, y + h - 1, w, 1); fill(x, y + 1, 1, h - 2); fill(x + w - 1, y + 1, 1, h - 2); }
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
    // centred on what is drawn on along 'x', 'y' or 'xy'. AC's write takes
    // more after it (a background, bounds, word wrap, a typeface): the
    // background is drawn; the rest is not heeded, for there is the one face
    // and a line is never wrapped.
    write(text, at = {}, bg) {
      text = String(text);
      let { x = 0, y = 0, size = 1, center = '' } = at || {};
      if (center.includes('x')) x = Math.floor((on.width - textWidth(text, size)) / 2);
      if (center.includes('y')) y = Math.floor((on.height - ascent * size) / 2);
      if (bg !== undefined && bg !== null) {
        const fore = color;
        color = colorOf([bg]);
        fill(x, y, textWidth(text, size), ascent * size);
        color = fore;
      }
      for (const ch of text) {
        const [w, h, xoff, yoff, advance, rows] = glyph(ch);
        for (let r = 0; r < h; r++)
          for (let c = 0; c < w; c++)
            if (rows[r] & (128 >> c)) fill(x + (xoff + c) * size, y + (ascent - yoff - h + r) * size, size, size);
        x += advance * size;
      }
      return g;
    },
    // (AC's `text.width` takes a typeface where a size is taken here: anything that is not a number is the one face at its own size.)
    text: { width: (text, size) => textWidth(text, Number.isFinite(size) ? size : 1), height: (size) => ascent * (Number.isFinite(size) ? size : 1) },
    // 🖼️ painting(w, h, fn): a buffer of its own, clear until `fn` draws on it with a painter of its own. → { width, height, pixels }
    painting(w, h, fn) {
      const p = painter(Math.max(1, Math.floor(w)), Math.max(1, Math.floor(h)));
      p.screen.pixels.fill(0);
      fn?.(p);
      return p.screen;
    },
    // paste(buffer, x, y, scale): a buffer laid over what is drawn on, its clear pixels left clear; `scale` is a number, { scale }, or { width, height } to stretch it to. Nearest pixel, as AC enlarges.
    paste(from, x = 0, y = 0, scale = 1) {
      if (!from?.pixels) return g;
      const sx = scale?.width ? scale.width / from.width : Number(scale?.scale ?? scale) || 1, sy = scale?.height ? scale.height / from.height : sx;
      const w = Math.round(from.width * sx), h = Math.round(from.height * sy), px = on.pixels, src = from.pixels;
      x = Math.floor(x); y = Math.floor(y);
      for (let j = Math.max(0, -y); j < h && y + j < on.height; j++)
        for (let i = Math.max(0, -x); i < w && x + i < on.width; i++) {
          const s = (Math.min(from.height - 1, Math.floor(j / sy)) * from.width + Math.min(from.width - 1, Math.floor(i / sx))) * 4;
          put(px, ((y + j) * on.width + x + i) * 4, src[s], src[s + 1], src[s + 2], src[s + 3]);
        }
      return g;
    },
    // 📄 page(buffer): what is drawn from now goes on this buffer (a painting, or `screen` to come back).
    page(buffer) { if (buffer?.pixels) on = buffer; return g; },
    // scroll(dx, dy): what is drawn on, moved over and wrapped round its edges.
    scroll(dx = 0, dy = 0) {
      const { width: w, height: h, pixels: px } = on, was = px.slice();
      dx = ((Math.floor(dx) % w) + w) % w; dy = ((Math.floor(dy) % h) + h) % h;
      if (!dx && !dy) return g;
      for (let j = 0; j < h; j++)
        for (let i = 0; i < w; i++) px.set(was.subarray((j * w + i) * 4, (j * w + i) * 4 + 4), (((j + dy) % h) * w + ((i + dx) % w)) * 4);
      return g;
    },
    // (AC's `layer` orders what is drawn across its layers: a panel has the one, and things lie in the order they are drawn.)
    layer: () => g,
    // ⚠️ Not done here: a picture a piece blurs, sharpens or zooms is left as it was drawn. (Nothing hosted asks for these on what it shows a kiosk: notepat's are in its picture overlay and its perf view, which only a keyboard opens.)
    blur: () => g, sharpen: () => g, zoom: () => g,
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
// ⌨️ A key as AC's keyboard says it (lib/keyboard.mjs): `keyboard:down:a`, `keyboard:up:space`, with the key, its code and what was held with it. `is('keyboard:down')` is any key down; a name with the key in it is that key alone (disk.mjs).
export function keyEvent(way, e) {
  const key = e.key === 'Unidentified' && e.which === 8 ? 'Backspace' : String(e.key ?? '');
  const name = `keyboard:${way}:${key === ' ' ? 'space' : key.toLowerCase()}`;
  return { name, device: 'keyboard', key, code: e.code, repeat: !!e.repeat, shift: !!e.shiftKey, alt: !!e.altKey, ctrl: !!e.ctrlKey, meta: !!e.metaKey,
    caps: !!e.getModifierState?.('CapsLock'), velocity: e.velocity ?? 127, is: (q) => (q.split(':').length > 2 ? name === q : name.startsWith(q)) };
}
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

// A tracked fingertip touches the glass from its front. Once down, a wider
// release band lets it slide without chattering as tracking depth fluctuates.
export function fingertipHit(face, tip, eye, held = false) {
  const a = (face.yaw || 0) * Math.PI / 180, c = Math.cos(a), s = Math.sin(a);
  const dx = tip.x - face.x, dz = tip.z - face.z;
  const depth = dx*s + dz*c;
  if ((eye.x-face.x)*s + (eye.z-face.z)*c >= 0) return null;
  if (depth < -tip.radius - (held ? 0.022 : 0.003) || depth > (held ? 0.085 : 0.025)) return null;
  const x = (0.5 - (dx*c - dz*s) / face.w) * face.cols;
  const y = (0.5 - (tip.y - face.y) / face.h) * face.rows;
  return x >= 0 && x < face.cols && y >= 0 && y < face.rows ? {x, y, t: Math.abs(depth)} : null;
}

// One panel: a place, a pixel buffer, and a piece painting on it. `frame`
// counts the pictures that differed, so the view rebuilds only on a new one.
export function createPanel({ id, piece, cols = 64, rows = 96, host = {}, ...place }) {
  const g = painter(cols, rows), pen = { x: -1, y: -1, down: false };
  // (A piece may take the whole api as one of its parts (`api`), and count its frames: `paintCount`, `simCount`.)
  const hands = new Map();
  const api = { ...g, pen, paintCount: 0, simCount: 0, ...host, pens: () => [...hands.values()] };
  function dispatch(name, px, py, device, pointer) {
    const x = Math.floor(px), y = Math.floor(py), before = hands.get(pointer);
    const down = name === 'touch' || name === 'draw';
    if (down) hands.set(pointer, { pointer, x, y, down, device });
    if (name === 'lift') hands.delete(pointer);
    if (device !== 'remote') {
      pen.x = px; pen.y = py;
      pen.down = down || (hands.size > 0 && [...hands.values()].some(p => p.device !== 'remote'));
    }
    const delta = (device === 'touch' || device === 'remote') && before && name === 'draw' ? { x: x - before.x, y: y - before.y } : { x: 0, y: 0 };
    piece.act?.({ ...api, event: { ...pointerEvent(name, x, y, device), pointer, delta } });
  }
  api.api = api;
  const livePixels = new Uint32Array(g.screen.pixels.buffer);
  let shown = null;
  const panel = {
    id, cols, rows, yaw: 0, deep: 0.04, ...place,
    pixels: g.screen.pixels, frame: 0, cursor: null, hidden: false, pen,
    act(name, x, y, device, pointer = 1) {
      dispatch(name, x, y, device, pointer);
    },
    // ⌨️ A key, for a piece that hears a keyboard (an AC disk: `takesKeys`). Only the page that has stepped in to the panel sends it any.
    get takesKeys() { return !!piece.keys; },
    key(way, e) { if (piece.keys) piece.act?.({ ...api, event: keyEvent(way, e) }); },
    // 👥 Someone else in the room presses here: the piece hears their pointer (numbered, so two hands are two), and this page's own pen is left alone.
    hear(name, x, y, pointer) {
      dispatch(name, x, y, 'remote', pointer);
    },
    step() {
      piece.sim?.(api);
      api.simCount++;
      piece.paint?.(api);
      api.paintCount++;
      if (shown) {
        let k = 0;
        while (k < shown.length && shown[k] === livePixels[k]) k++;
        if (k === shown.length) return;
        shown.set(livePixels);
      } else shown = livePixels.slice();
      panel.frame++;
    },
    leave() { piece.leave?.(api); },
    // ↻ The piece begun again from nothing: the old one is told it is leaving, and a fresh load of it (`next.piece`, with its own host parts) boots on the same screen.
    renew(next) {
      try { piece.leave?.(api); } catch {}
      piece = next.piece;
      Object.assign(api, next.host || {});
      api.paintCount = api.simCount = 0;
      hands.clear();
      pen.down = false;
      shown = null;
      piece.boot?.(api);
    },
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
        // (An instrument's panel, `completes`, is let go where it was held, so a note pressed is a note finished when its player turns or walks away; any other is let go out in the open.)
        if (held === hot.panel) { held.completes ? held.act('lift', held.pen.x, held.pen.y) : held.act('lift', -1, -1); held = null; }
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
