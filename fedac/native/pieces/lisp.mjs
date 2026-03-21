// lisp.mjs — Generic KidLisp interpreter piece for native runtime
// Evaluates KidLisp source from globalThis.__kidlispSource.
// Uses the bundled KidLisp evaluator (KidLispModule) loaded at init time.

// Built-in source for $roz (fallback when no __kidlispSource is set)
const ROZ_SOURCE = `fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)`;

// CSS color map for resolving color name strings to [r, g, b]
const COLORS = {
  red: [255, 0, 0], green: [0, 128, 0], blue: [0, 0, 255],
  white: [255, 255, 255], black: [0, 0, 0],
  cyan: [0, 255, 255], magenta: [255, 0, 255], yellow: [255, 255, 0],
  orange: [255, 165, 0], pink: [255, 192, 203], purple: [128, 0, 128],
  lime: [0, 255, 0], aqua: [0, 255, 255], navy: [0, 0, 128],
  maroon: [128, 0, 0], olive: [128, 128, 0], teal: [0, 128, 128],
  silver: [192, 192, 192], gray: [128, 128, 128], grey: [128, 128, 128],
  coral: [255, 127, 80], salmon: [250, 128, 114],
  gold: [255, 215, 0], indigo: [75, 0, 130], violet: [238, 130, 238],
  crimson: [220, 20, 60], turquoise: [64, 224, 208],
};

// Resolve color names / "rainbow" to numeric [r, g, b] for native ink/wipe
function resolveColor(...args) {
  const resolved = [];
  for (const a of args) {
    if (typeof a === "string") {
      if (a === "rainbow") {
        const h = ((performance.now() / 4000) % 1);
        const i = Math.floor(h * 6), f = h * 6 - i, q = 1 - f;
        let r = 0, g = 0, b = 0;
        if (i % 6 === 0) { r = 1; g = f; }
        else if (i % 6 === 1) { r = q; g = 1; }
        else if (i % 6 === 2) { g = 1; b = f; }
        else if (i % 6 === 3) { g = q; b = 1; }
        else if (i % 6 === 4) { r = f; b = 1; }
        else { r = 1; b = q; }
        resolved.push(Math.floor(r * 255), Math.floor(g * 255), Math.floor(b * 255));
      } else if (COLORS[a]) {
        resolved.push(...COLORS[a]);
      }
      // else skip unknown strings (fade:, etc.)
    } else if (typeof a === "number") {
      resolved.push(a);
    }
  }
  return resolved;
}

let kl = null;
let ast = null;
let error = null;
let sourceLabel = "";
let originalSource = "";

function boot({ wipe, screen }) {
  wipe(0, 0, 0);

  if (typeof KidLispModule === "undefined" || !KidLispModule.KidLisp) {
    error = "KidLisp engine not loaded";
    return;
  }

  // Read source: prefer globalThis.__kidlispSource, fall back to ROZ_SOURCE
  const source = globalThis.__kidlispSource || ROZ_SOURCE;
  originalSource = source;
  sourceLabel = globalThis.__kidlispLabel || (source === ROZ_SOURCE ? "$roz" : "lisp");
  // Clear global after reading
  globalThis.__kidlispSource = undefined;
  globalThis.__kidlispLabel = undefined;

  try {
    kl = new KidLispModule.KidLisp();
    ast = kl.parse(source);
    if (!ast) {
      error = "Parse failed: " + source.slice(0, 40);
    } else {
      ast = kl.precompileAST(ast);
      kl.ast = ast;
    }
  } catch (e) {
    error = "KidLisp: " + (e.message || e);
  }
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    // Return to prompt with source preserved (cursor at end)
    globalThis.__promptRestore = originalSource;
    system?.jump?.("prompt");
  }
}

function sim() {}

function paint({ wipe, ink, line, box, circle, write, scroll, spin, zoom,
                 contrast, blur, screen, paintCount }) {
  if (error) {
    wipe(20, 0, 0);
    ink(255, 80, 80);
    write(error, { x: 4, y: 4, size: 1, font: "6x10" });
    ink(120, 120, 130);
    write("esc:back", { x: 4, y: screen.height - 14, size: 1, font: "6x10" });
    return;
  }

  if (!kl || !ast) {
    wipe(0, 0, 0);
    return;
  }

  // Wrap ink/wipe to resolve color name strings to numeric RGB
  const wrappedInk = (...args) => {
    const c = resolveColor(...args);
    if (c.length >= 3) ink(c[0], c[1], c[2], c[3] ?? 255);
    else if (c.length === 1) ink(c[0], c[0], c[0]);
  };

  const wrappedWipe = (...args) => {
    if (typeof args[0] === "string" && args[0].startsWith("fade:")) {
      const stops = args[0].replace("fade:", "").split("-")
        .map(name => COLORS[name] || [0, 0, 0]);
      const h = Math.max(1, screen.height - 1);
      for (let y = 0; y < screen.height; y++) {
        const t = y / h;
        const scaled = t * (stops.length - 1);
        const i = Math.floor(scaled);
        const j = Math.min(stops.length - 1, i + 1);
        const f = scaled - i;
        const r = Math.round(stops[i][0] + (stops[j][0] - stops[i][0]) * f);
        const g = Math.round(stops[i][1] + (stops[j][1] - stops[i][1]) * f);
        const b = Math.round(stops[i][2] + (stops[j][2] - stops[i][2]) * f);
        ink(r, g, b);
        line(0, y, screen.width - 1, y);
      }
      return;
    }
    const c = resolveColor(...args);
    if (c.length >= 3) wipe(c[0], c[1], c[2]);
    else if (c.length === 1) wipe(c[0]);
    else wipe(0, 0, 0);
  };

  const api = {
    wipe: wrappedWipe,
    ink: wrappedInk,
    line, box, circle, write, scroll, spin, zoom, contrast, blur,
    screen: { width: screen.width, height: screen.height },
    paintCount,
    help: "",
    needsPaint: () => {},
    backgroundFill: () => {},
    blend: () => {},
    plot: (x, y) => { box(x, y, 1, 1); },
    clock: { time: () => performance.now() / 1000 },
  };

  try {
    kl.frameCount = (kl.frameCount || 0) + 1;
    kl.evaluate(ast, api, kl.localEnv);
  } catch (e) {
    ink(255, 60, 60, 180);
    write("eval: " + (e.message || e).slice(0, 60), { x: 2, y: 2, size: 1, font: "6x10" });
  }

  // No overlay text — avoids compositing source/crosshair into
  // backbuffer effects (scroll/spin/zoom). User knows esc goes back.
}

export { boot, paint, act, sim };
