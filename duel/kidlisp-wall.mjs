// A wall that runs real KidLisp: AC's own evaluator (kidlisp.mjs) drawing
// through AC's own renderer (graph.mjs) into a small pixel buffer, which the
// view lays onto a wall as a grid of coloured cells. Fades, rainbows, spin,
// zoom, scroll, blur and contrast all come from the same code AC runs.

import { KidLisp } from './ac/lib/kidlisp.mjs';
import * as graph from './ac/lib/graph.mjs';

export async function kidlispWall(code, { width = 96, height = 54 } = {}) {
  const response = await fetch(new URL(`./kidlisp-source?code=${encodeURIComponent(code)}`, import.meta.url));
  if (!response.ok) throw Error(`no KidLisp for $${code}`);
  const { source } = await response.json();
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
  };
}
