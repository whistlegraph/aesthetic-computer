// painting.mjs — Nopaint freehand drawing for AC Native
// Persistent canvas with line brush. Touch to draw, lift to bake.
// Commands: "no" clears, scroll adjusts thickness.

let T = __theme.update();

// Canvas state
let canvas = null;   // persistent painting buffer
let buffer = null;   // temporary stroke overlay
let sw = 0, sh = 0;

// Brush state
let thickness = 2;
let color = [255, 255, 255]; // default white (will adapt to theme)
let painting = false;
let points = [];
let frame = 0;
let cursorX = 0, cursorY = 0; // Track cursor for preview
let mkPaintingFn = null; // Stored for "no" command

// Color palette (cycle with middle-click or 'c' key)
const palette = [
  [255, 255, 255], [255, 0, 0], [255, 165, 0], [255, 255, 0],
  [0, 128, 0], [0, 255, 255], [0, 0, 255], [128, 0, 128],
  [128, 128, 128], [0, 0, 0],
];
let colorIdx = 0;

function boot({ screen, wipe, painting: mkPainting }) {
  sw = screen.width;
  sh = screen.height;
  mkPaintingFn = mkPainting;

  // Create persistent canvas and stroke buffer
  canvas = mkPainting(sw, sh, (p) => {
    T = __theme.update();
    p.wipe(T.dark ? 20 : 240, T.dark ? 20 : 238, T.dark ? 25 : 232);
  });
  buffer = mkPainting(sw, sh, (p) => { p.wipe(0, 0, 0, 0); });

  color = T.dark ? [255, 255, 255] : [0, 0, 0];
  cursorX = sw / 2;
  cursorY = sh / 2;
  wipe(T.bg[0], T.bg[1], T.bg[2]);
}

function act({ event: e, screen, sound, system, page }) {
  // Track cursor position from any mouse/touch event
  if (e.x !== undefined) { cursorX = e.x; cursorY = e.y; }

  // Start stroke
  if (e.is("touch")) {
    painting = true;
    points = [{ x: e.x, y: e.y }];
  }

  // Continue stroke
  if (e.is("draw") && painting) {
    const last = points[points.length - 1];
    if (!last || last.x !== e.x || last.y !== e.y) {
      points.push({ x: e.x, y: e.y });
    }
  }

  // End stroke — bake will happen in paint
  if (e.is("lift") && painting) {
    painting = false;
  }

  // Scroll: adjust thickness
  if (e.is("scroll")) {
    const delta = e.y > 0 ? -1 : e.y < 0 ? 1 : 0;
    thickness = Math.max(1, Math.min(thickness + delta, 50));
  }

  // 'c' key: cycle color
  if (e.is("keyboard:down:c")) {
    colorIdx = (colorIdx + 1) % palette.length;
    color = palette[colorIdx];
  }

  // 'n' key: clear canvas (the "no" command)
  if (e.is("keyboard:down:n") && canvas && mkPaintingFn) {
    canvas = mkPaintingFn(sw, sh, (p) => {
      T = __theme.update();
      p.wipe(T.dark ? 20 : 240, T.dark ? 20 : 238, T.dark ? 25 : 232);
    });
  }

  // Escape: jump to prompt
  if (e.is("keyboard:down:escape")) {
    system?.jump?.("prompt");
  }
}

function paint({ wipe, ink, line, circle, paste, page, screen, write }) {
  frame++;
  T = __theme.update();

  // 1. Draw the persistent canvas to screen
  wipe(T.bg[0], T.bg[1], T.bg[2]);
  if (canvas) paste(canvas);

  // 2. If actively painting, draw current stroke into buffer and overlay it
  if (painting && points.length > 1) {
    // Clear buffer
    page(buffer).wipe(0, 0, 0, 0);

    // Draw stroke into buffer
    for (let i = 0; i < points.length - 1; i++) {
      const a = points[i], b = points[i + 1];
      if (thickness === 1) {
        ink(color[0], color[1], color[2]).line(a.x, a.y, b.x, b.y);
      } else {
        ink(color[0], color[1], color[2]).line(a.x, a.y, b.x, b.y, thickness);
      }
    }

    // Switch back to screen
    page(screen);

    // Overlay buffer onto screen
    paste(buffer);
  }

  // 3. If stroke just ended, bake buffer onto canvas
  if (!painting && points.length > 1) {
    // Draw stroke directly onto canvas
    page(canvas);
    for (let i = 0; i < points.length - 1; i++) {
      const a = points[i], b = points[i + 1];
      if (thickness === 1) {
        ink(color[0], color[1], color[2]).line(a.x, a.y, b.x, b.y);
      } else {
        ink(color[0], color[1], color[2]).line(a.x, a.y, b.x, b.y, thickness);
      }
    }
    page(screen);
    points = [];

    // Redraw canvas to screen
    paste(canvas);
  }

  // 4. Brush preview cursor (when not painting)
  if (!painting && thickness > 2) {
    ink(color[0], color[1], color[2], 80).circle(cursorX, cursorY, Math.floor(thickness / 2), false);
  }

  // 5. HUD
  const fg = T.fg;
  ink(fg, fg, fg, 180).write(`${thickness}px`, { x: 4, y: sh - 12, size: 1, font: "font_1" });
  ink(color[0], color[1], color[2]).box(30, sh - 12, 8, 8, true);
  ink(fg, fg, fg, 120).write("c:color n:clear esc:exit", { x: 44, y: sh - 12, size: 1, font: "font_1" });
}

function sim() {}
function leave() {}

export { boot, act, paint, sim, leave };
