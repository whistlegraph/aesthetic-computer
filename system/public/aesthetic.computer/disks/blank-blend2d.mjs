// GPU Backend Test - Blend2D
// Testing Blend2D WASM renderer (experimental)

/* 📝 Engineering Notes
  Press UP/DOWN to adjust line count
  Press P to toggle performance overlay
  Press SPACE to toggle animation
  
  Blend2D is a high-performance 2D vector graphics engine.
  Requires blend2d.wasm to be installed in lib/gpu/wasm/
  
  To get the WASM:
  1. Build from source with Emscripten: https://blend2d.com/doc/build-instructions.html
  2. Compile with: emcmake cmake .. -DBLEND2D_STATIC=TRUE
*/

let frame = 0;
let perfEnabled = true;
let animating = true;
let lineCount = 500;
const LINE_STEP = 100;
const MAX_LINES = 10000;
const MIN_LINES = 10;

function boot({ api }) {
  // Use Blend2D backend via the gpu abstraction
  api.gpu = api.gpu || {};
  api.gpu.backend = "blend2d";
  api.webgpu.enabled = true;
  
  // Show DOM stats overlay (always on top)
  api.stats.show({ backend: "BLEND2D", lines: lineCount, extra: "↑↓ adjust  P toggle" });
  
  console.log("🎨 GPU Backend: Blend2D (WASM)");
}

function act({ event: e, api }) {
  if (e.is("keyboard:down:p")) {
    perfEnabled = !perfEnabled;
    if (perfEnabled) {
      api.stats.show({ backend: "BLEND2D", lines: lineCount, extra: "↑↓ adjust  P toggle" });
    } else {
      api.stats.hide();
    }
  }
  if (e.is("keyboard:down:space")) {
    animating = !animating;
  }
  if (e.is("keyboard:down:arrowup")) {
    lineCount = Math.min(MAX_LINES, lineCount + LINE_STEP);
    api.stats.update({ lines: lineCount });
    console.log("Lines:", lineCount);
  }
  if (e.is("keyboard:down:arrowdown")) {
    lineCount = Math.max(MIN_LINES, lineCount - LINE_STEP);
    api.stats.update({ lines: lineCount });
    console.log("Lines:", lineCount);
  }
}

function paint({ wipe, ink, line, screen }) {
  if (animating) frame += 1;
  
  // Dark animated background with magenta tint
  const bgR = Math.floor(25 + Math.sin(frame * 0.005) * 12);
  const bgG = Math.floor(10 + Math.sin(frame * 0.007) * 8);
  const bgB = Math.floor(25 + Math.sin(frame * 0.009) * 12);
  wipe(bgR, bgG, bgB);
  
  const w = screen.width;
  const h = screen.height;
  const cx = w / 2;
  const cy = h / 2;
  const time = frame * 0.02;
  
  // Draw many lines in various patterns
  for (let i = 0; i < lineCount; i++) {
    const t = i / lineCount;
    
    const pattern = i % 4;
    let x1, y1, x2, y2;
    
    if (pattern === 0) {
      const angle = t * Math.PI * 8 + time;
      const len = Math.min(w, h) * 0.45 * (0.3 + 0.7 * Math.sin(t * Math.PI * 4 + time));
      x1 = cx;
      y1 = cy;
      x2 = cx + Math.cos(angle) * len;
      y2 = cy + Math.sin(angle) * len;
    } else if (pattern === 1) {
      const yPos = t * h;
      const wave = Math.sin(yPos * 0.1 + time) * 30;
      x1 = wave;
      y1 = yPos;
      x2 = w + wave;
      y2 = yPos;
    } else if (pattern === 2) {
      const spiral = t * Math.PI * 6 + time * 0.5;
      const r1 = t * Math.min(w, h) * 0.4;
      const r2 = r1 + 20;
      x1 = cx + Math.cos(spiral) * r1;
      y1 = cy + Math.sin(spiral) * r1;
      x2 = cx + Math.cos(spiral + 0.2) * r2;
      y2 = cy + Math.sin(spiral + 0.2) * r2;
    } else {
      const seed = i * 1234.5678;
      const bx = (Math.sin(seed) * 0.5 + 0.5) * w;
      const by = (Math.cos(seed * 1.1) * 0.5 + 0.5) * h;
      const angle = seed + time;
      const len = 20 + Math.sin(seed * 2 + time) * 15;
      x1 = bx;
      y1 = by;
      x2 = bx + Math.cos(angle) * len;
      y2 = by + Math.sin(angle) * len;
    }
    
    // Magenta-pink gradient
    const hue = (0.85 + t * 0.1 + frame * 0.001) % 1;
    const rgb = hslToRgb(hue, 0.9, 0.55);
    ink(rgb[0], rgb[1], rgb[2]);
    line(x1, y1, x2, y2);
  }
}

function leave({ stats, webgpu } = {}) {
  // Hide stats overlay and disable WebGPU when leaving
  stats?.hide();
  webgpu?.disable();
}


function hslToRgb(h, s, l) {
  let r, g, b;
  if (s === 0) {
    r = g = b = l;
  } else {
    const hue2rgb = (p, q, t) => {
      if (t < 0) t += 1;
      if (t > 1) t -= 1;
      if (t < 1/6) return p + (q - p) * 6 * t;
      if (t < 1/2) return q;
      if (t < 2/3) return p + (q - p) * (2/3 - t) * 6;
      return p;
    };
    const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
    const p = 2 * l - q;
    r = hue2rgb(p, q, h + 1/3);
    g = hue2rgb(p, q, h);
    b = hue2rgb(p, q, h - 1/3);
  }
  return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
}

export { boot, act, paint, leave };
