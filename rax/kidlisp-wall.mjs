// A wall that runs real KidLisp: AC's own evaluator (kidlisp.mjs) drawing
// through AC's own renderer (graph.mjs) into a small pixel buffer, which the
// view lays onto a wall as a grid of coloured cells. Fades, rainbows, spin,
// zoom, scroll, blur and contrast all come from the same code AC runs.

import { KidLisp } from './ac/lib/kidlisp.mjs';
import * as graph from './ac/lib/graph.mjs';

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

export async function kidlispWall(code, { width = 96, height = 54, source: given = null } = {}) {
  let source = given;
  if (!source) {
    const response = await fetch(new URL(`./kidlisp-source?code=${encodeURIComponent(code)}`, import.meta.url));
    if (!response.ok) throw Error(`no KidLisp for $${code}`);
    ({ source } = await response.json());
  }
  const display = { width, height, pixels: new Uint8ClampedArray(width * height * 4) };
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
    hud: { label() {}, tinyLabel() {}, qr() {} },
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
    wipe(...args) {
      if (args.length) graph.color(...graph.findColor(...args));
      graph.clear();
      return api;
    },
    write() {
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
  return {
    source,
    width,
    height,
    pixels: display.pixels,
    // One frame of the program; errors leave the last good frame.
    step() {
      try {
        graph.setBuffer(display);
        piece?.paint?.(api);
      } catch {}
      return display.pixels;
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
