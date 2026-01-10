// GPU Backend Test - Canvas2D
// Testing Canvas2D renderer as universal fallback

/* 📝 Engineering Notes
  Press UP/DOWN to adjust line count
  Press P to toggle performance overlay
  Press SPACE to toggle animation
  
  This uses Canvas2D as the rendering backend (universal support).
  Good baseline for comparing GPU-accelerated backends.
*/

let frame = 0;
let perfEnabled = true;
let animating = true;
let lineCount = 500;
const LINE_STEP = 100;
const MAX_LINES = 10000;
const MIN_LINES = 10;

// FPS tracking
let lastTime = performance.now();
let fps = 0;
let frameCount = 0;
let lastFpsUpdate = 0;

function boot({ api }) {
  // Use Canvas2D backend via the gpu abstraction
  api.gpu = api.gpu || {};
  api.gpu.backend = "canvas2d";
  api.webgpu.enabled = true; // Enable GPU rendering path
  api.webgpu.perf(true);
  console.log("🎨 GPU Backend: Canvas2D");
}

function act({ event: e, api }) {
  if (e.is("keyboard:down:p")) {
    perfEnabled = !perfEnabled;
    api.webgpu.perf(perfEnabled);
  }
  if (e.is("keyboard:down:space")) {
    animating = !animating;
  }
  if (e.is("keyboard:down:arrowup")) {
    lineCount = Math.min(MAX_LINES, lineCount + LINE_STEP);
    console.log("Lines:", lineCount);
  }
  if (e.is("keyboard:down:arrowdown")) {
    lineCount = Math.max(MIN_LINES, lineCount - LINE_STEP);
    console.log("Lines:", lineCount);
  }
}

function paint({ wipe, ink, line, box, write, screen }) {
  if (animating) frame += 1;
  
  // Dark animated background with green tint (to distinguish)
  const bgR = Math.floor(10 + Math.sin(frame * 0.005) * 8);
  const bgG = Math.floor(25 + Math.sin(frame * 0.007) * 12);
  const bgB = Math.floor(15 + Math.sin(frame * 0.009) * 10);
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
    
    // Green-yellow gradient (to distinguish)
    const hue = (0.25 + t * 0.15 + frame * 0.001) % 1;
    const rgb = hslToRgb(hue, 0.85, 0.5);
    ink(rgb[0], rgb[1], rgb[2]);
    line(x1, y1, x2, y2);
  }
  
  // Draw FPS overlay
  drawFps({ ink, box, write, screen });
}

function drawFps({ ink, box, write, screen }) {
  const now = performance.now();
  frameCount++;
  if (now - lastFpsUpdate >= 1000) {
    fps = frameCount;
    frameCount = 0;
    lastFpsUpdate = now;
  }
  
  ink(0, 0, 0, 180).box(4, 4, 140, 52);
  ink(100, 255, 100).write(`CANVAS2D`, { x: 8, y: 8 });
  ink(255).write(`FPS: ${fps}`, { x: 8, y: 20 });
  ink(200).write(`Lines: ${lineCount}`, { x: 8, y: 32 });
  ink(150).write(`↑↓ adjust  P perf`, { x: 8, y: 44 });
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

export { boot, act, paint };
