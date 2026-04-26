// raylibtest.mjs — Smoke test for the raylib software-rendering bridge.
// Allocates an off-screen painting buffer, asks ac-native to fill it via
// raylib's Image* APIs, and pastes it onto the screen each frame.

let canvas = null;
let cw = 0, ch = 0;
let frame = 0;
let supported = true;
let lastResult = true;

function boot({ screen, painting }) {
  cw = Math.min(screen.width, 320);
  ch = Math.min(screen.height, 240);
  canvas = painting(cw, ch, (p) => p.wipe(0, 0, 0));
  if (typeof globalThis.system?.raylibTest !== "function") {
    supported = false;
  }
}

function paint({ wipe, ink, write, screen, paste, system }) {
  frame++;
  const T = __theme.update();
  wipe(T.bg[0], T.bg[1], T.bg[2]);

  if (!supported) {
    ink(255, 100, 100);
    write("raylib not compiled in", { x: 8, y: 12, size: 1, font: "font_1" });
    ink(T.fgMute);
    write("rebuild ac-native with raylib-devel installed", {
      x: 8, y: 28, size: 1, font: "font_1",
    });
    return;
  }

  if (canvas) {
    lastResult = system?.raylibTest?.(canvas, frame) === true;
  }

  if (!lastResult) {
    ink(255, 160, 80);
    write("raylib call failed", { x: 8, y: 12, size: 1, font: "font_1" });
    return;
  }

  const ox = ((screen.width - cw) / 2) | 0;
  const oy = ((screen.height - ch) / 2) | 0;
  paste(canvas, ox, oy);

  // Tag the output so it's visually distinct from native primitives.
  ink(T.fg, T.fg + 10, T.fg);
  write("raylibtest", { x: 8, y: screen.height - 12, size: 1, font: "font_1" });
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
  }
}

function sim() {}

export { boot, paint, act, sim };
