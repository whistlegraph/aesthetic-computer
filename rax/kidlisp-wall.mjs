// A wall that runs real KidLisp: AC's own evaluator (kidlisp.mjs) drawing
// through AC's own renderer (graph.mjs) into a pixel buffer, which the view
// lays onto a wall as a grid of coloured cells. Fades, rainbows, spin, zoom,
// scroll, blur, contrast, stamps and bake layers all come from the same code
// AC runs.

import { pointerEvent } from './panel.mjs';
import { KidLisp } from './ac/lib/kidlisp.mjs';
import * as graph from './ac/lib/graph.mjs';
import { glyphRows } from './ac/lib/pixel-font-data.mjs';

// 📐 A piece is written for an AC screen a few hundred pixels across
// (`stamp #n89 150 100`, `write "hi" 150 150`), and the grid a wall shows is
// 16 to 256 cells by how far the eye is. So the program always runs on a
// screen LOGICAL pixels on its long side, and its picture is brought down to
// the grid: what a piece draws is in one place at every distance.
const LOGICAL = 320;
// Each cell of the grid is the mean of the pixels under it.
function shrink(from, wide, tall, to, cols, rows) {
  for (let j = 0; j < rows; j++) {
    const y0 = Math.floor((j * tall) / rows), y1 = Math.max(y0 + 1, Math.floor(((j + 1) * tall) / rows));
    for (let i = 0; i < cols; i++) {
      const x0 = Math.floor((i * wide) / cols), x1 = Math.max(x0 + 1, Math.floor(((i + 1) * wide) / cols));
      let r = 0, g = 0, b = 0, a = 0;
      for (let y = y0; y < y1; y++)
        for (let k = (y * wide + x0) * 4, end = (y * wide + x1) * 4; k < end; k += 4) {
          r += from[k];
          g += from[k + 1];
          b += from[k + 2];
          a += from[k + 3];
        }
      const n = (x1 - x0) * (y1 - y0), o = (j * cols + i) * 4;
      to[o] = r / n;
      to[o + 1] = g / n;
      to[o + 2] = b / n;
      to[o + 3] = a / n;
    }
  }
}

// 🖼️ What a piece stamps or pastes by name (`#n89`, `@handle/slug`, a URL):
// fetched once for the page from where AC's own disk fetches it (disk.mjs
// `get.painting`), and drawn from the frame after it is here.
// An ink put back as it was read from graph.c (a fade is a name, not numbers).
const inked = (c) => graph.color(...(typeof c[0] === 'number' ? c : graph.findColor(...c)));

const AC = 'https://aesthetic.computer';
const paintings = new Map(); // name → { width, height, pixels }, or null while it is fetched
function painting(name) {
  if (paintings.has(name)) return paintings.get(name);
  paintings.set(name, null);
  const code = name.replace(/^#/, ''), [author, slug] = code.split('/');
  const dated = /^\d{4}(\.\d{1,3}){6}$/.test(slug || '');
  const url = code.startsWith('http') ? code
    : dated ? `${AC}/media/@${author.replace(/^@/, '')}/painting/${slug}.png`
    : `${AC}/media/paintings/${slug || code}.png`;
  fetch(url, { signal: AbortSignal.timeout(10000) })
    .then((r) => (r.ok ? r.blob() : Promise.reject(Error(`${r.status} for ${name}`))))
    .then((blob) => createImageBitmap(blob))
    .then((bitmap) => {
      const { width, height } = bitmap;
      const pen = new OffscreenCanvas(width, height).getContext('2d');
      pen.drawImage(bitmap, 0, 0);
      bitmap.close();
      paintings.set(name, { width, height, pixels: pen.getImageData(0, 0, width, height).data });
    })
    // Not there, or the network is not: ask again in a while, not every frame.
    .catch(() => setTimeout(() => paintings.delete(name), 15000));
  return null;
}

// A KidLisp colour name ("cyan", "255,0,0", "fade:red-blue") to 0xRRGGBB.
const colors = new Map();
function colorOf(name) {
  if (colors.has(name)) return colors.get(name);
  let rgb = [242, 242, 242];
  try {
    const n = name.startsWith('fade:') ? name.slice(5).split('-')[0] : name;
    const parts = n.split(',').map(Number);
    const found = parts.length >= 3 && parts.every(Number.isFinite) ? parts : graph.findColor(n);
    if (Array.isArray(found) && found.length >= 3) rgb = found;
  } catch {}
  const c = (Math.round(rgb[0]) << 16) | (Math.round(rgb[1]) << 8) | Math.round(rgb[2]);
  if (colors.size > 256) colors.clear();
  colors.set(name, c);
  return c;
}

// (`logical`: the long side of the screen the program runs on. A quarter the pixels at half of it: its blurs and scrolls are a quarter the work.)
export async function kidlispWall(code, { width = 96, height = 54, source: given = null, logical = LOGICAL } = {}) {
  let source = given;
  if (!source) {
    const response = await fetch(new URL(`./kidlisp-source?code=${encodeURIComponent(code)}`, import.meta.url));
    if (!response.ok) throw Error(`no KidLisp for $${code}`);
    ({ source } = await response.json());
  }
  const scale = Math.max(1, logical / Math.max(width, height));
  const wide = Math.round(width * scale), tall = Math.round(height * scale);
  const frame = new Uint8ClampedArray(wide * tall * 4); // the program's screen
  const pixels = new Uint8ClampedArray(width * height * 4); // the grid shown
  const display = { width: wide, height: tall, pixels: frame };
  const lisp = new KidLisp();
  const start = performance.now();
  const base = {
    screen: display,
    params: [],
    colon: [],
    clock: { time: () => new Date(), resync() {} },
    system: { fps: 60 },
    sound: {},
    net: {},
    hud: { hidden: () => true, label() {}, tinyLabel() {}, qr() {} },
    cachedKidlispOwner: null,
    inkrn: () => graph.c.slice(),
    fps() {},
    needsPaint() {},
    toggleHUD() {},
    page(buffer) {
      Object.assign(display, buffer);
      graph.setBuffer(buffer);
    },
    ink(...args) {
      graph.color(...graph.findColor(...args));
      return api;
    },
    // (The ink is as it was after a wipe, as in disk.mjs: a piece that
    // writes with no ink of its own writes in white, not in its background.)
    wipe(...args) {
      const ink = graph.c.slice();
      if (args.length) graph.color(...graph.findColor(...args));
      graph.clear();
      inked(ink);
      return api;
    },
    paste(from, ...at) {
      const image = typeof from === 'string' ? painting(from) : from;
      if (image) graph.paste(image, ...at);
      return api;
    },
    stamp(from, ...at) {
      const image = typeof from === 'string' ? painting(from) : from;
      if (image) graph.stamp(image, ...at);
      return api;
    },
    // 🔤 AC's font_1 (6 by 10 to a letter), placed as Typeface.print places
    // it: `center` in "x", "y" or both, `size` a whole-number scale, and an
    // axis nobody gave is anywhere on the screen.
    write(text, at, third, fourth) {
      if (text == null || text === '') return api;
      const pos = typeof at === 'number' ? { x: at, y: third } : { ...at };
      const bg = (typeof at === 'number' ? fourth : third)?.bg;
      const size = pos.size || 1, center = pos.center || '', lost = pos.center === undefined;
      const anywhere = (span, room) => (lost ? Math.floor(Math.random() * (room + span) - span / 2) : 0);
      String(text).split('\n').forEach((line, n) => {
        const letters = [...line], span = letters.length * 6 * size;
        let x = Math.floor(pos.x ?? anywhere(span, wide)), y = Math.floor(pos.y ?? anywhere(11 * size, tall));
        if (center.includes('x')) x = Math.floor((pos.x ?? wide / 2) - span / 2);
        if (center.includes('y')) y = Math.floor((pos.y ?? tall / 2) - (11 * size) / 2);
        y += n * 11 * size;
        if (bg !== undefined) {
          const ink = graph.c.slice();
          graph.color(...graph.findColor(bg));
          graph.box(x, y, span, 11 * size);
          inked(ink);
        }
        letters.forEach((letter, k) => {
          (glyphRows[letter] || glyphRows['?']).forEach((bits, row) => {
            for (let col = 0; col < 6; col++)
              if (bits & (1 << col)) graph.box(x + (k * 6 + col) * size, y + row * size, size, size);
          });
        });
      });
      return api;
    },
  };
  // Anything else KidLisp asks for is graph.mjs's function of that name
  // (line, box, circle, spin, zoom, scroll, blur, contrast, ...), or a no-op.
  const api = new Proxy(base, {
    get(target, key) {
      if (key in target) return target[key];
      const f = graph[key];
      if (typeof f === 'function')
        return (...args) => {
          const out = f(...args);
          return out === undefined ? api : out;
        };
      return () => api;
    },
  });
  graph.setBuffer(display);
  lisp.setAPI?.(api);
  const piece = lisp.module(source, true);
  lisp.cacheInitiated = true;
  try {
    piece?.boot?.(api);
  } catch {}
  const hands = new Map();
  base.pens = () => [...hands.values()];
  return {
    source,
    width,
    height,
    pixels,
    // A touch arrives in the grid's cells and reaches the piece in its own pixels.
    point(name, cx, cy, pointer = 0, device = 'gamepad') {
      const x = cx * scale, y = cy * scale;
      graph.setBuffer(display);
      const previous = hands.get(pointer) || { x, y };
      if (name === 'lift') hands.delete(pointer);
      else hands.set(pointer, { pointer, x, y, down: true });
      const event = { ...pointerEvent(name, x, y, device), pointer, delta: { x: x - previous.x, y: y - previous.y } };
      piece?.act?.({ event, api });
      // KidLisp exposes one pen and a combined down flag. When a finger
      // lifts, leave that pen on a finger which is still touching the screen.
      if (name === 'lift' && hands.size) {
        const held = [...hands.values()].at(-1);
        Object.assign(lisp.globalDef, { down: 1, penx: held.x, peny: held.y });
      }
    },
    // One frame of the program; errors leave the last good frame.
    step() {
      try {
        // (A frame that threw part way may have left a bake layer as the page.)
        Object.assign(display, { width: wide, height: tall, pixels: frame });
        graph.setBuffer(display);
        // graph.mjs has one ink for every wall: a frame starts in white, not
        // in what the last wall drew with.
        graph.color(255, 255, 255, 255);
        piece?.paint?.(api);
      } catch {}
      shrink(frame, wide, tall, pixels, width, height);
      return pixels;
    },
    elapsed: () => (performance.now() - start) / 1000,
    // KidLisp's own live highlighting (timers flash as they fire, fades as
    // gradients, the running expression lit): lines of { text, color } runs.
    colored() {
      try {
        lisp.syntaxHighlightSource ||= source;
        const coded = lisp.buildColoredKidlispString();
        const lines = [[]];
        for (const m of coded.matchAll(/\\([^\\]+)\\([^\\]*)/g)) {
          const color = colorOf(m[1]);
          m[2].split('\n').forEach((part, k) => {
            if (k) lines.push([]);
            if (part) lines[lines.length - 1].push({ text: part, color });
          });
        }
        return lines.filter((l) => l.some((s) => s.text.trim()));
      } catch {
        return null;
      }
    },
  };
}
