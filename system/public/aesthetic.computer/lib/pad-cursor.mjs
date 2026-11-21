// 🎮 Pad Cursor
// A game controller's stick moves a cursor over the prompt and its buttons
// click, so someone holding only a pad can press what a mouse can.
//
//   left stick   move            right stick   move slowly
//   A (0)        the mouse button: down on press, up on release (a drag is
//                the stick moving while A is held)
//   B (1)        Escape
//   START (9)    off the prompt: back to it (Escape, as the keyboard has it).
//                On the prompt lib/rax-sheet.mjs hears START itself.
//
// The piece is told nothing new: the presses are PointerEvents sent to the
// element under the cursor, which bubble to the window where lib/pen.mjs
// listens. Pen does not ask whether an event is trusted.
//
// Only the prompt gets the cursor. A piece has no way to say "I read the pad
// myself", and a cursor sliding about under a game's stick would be worse
// than none.

import { cursorCells, REACH } from "./cursor-shape.mjs";

const { hypot, min, max, floor } = Math;

const DEAD = 0.15; // The same dead zone lib/gamepad.mjs uses.
const SLOW = 0.25; // The right stick's share of the left's speed.
const SIZE = REACH * 2 + 1, MID = REACH; // The cursor's canvas in piece pixels, and its middle.

// Pieces known to read the pad: START is theirs (a pause, a rematch).
const PAD_PIECES = /(?:^|\/)(?:1v1|arena|fight|gamepad|gameboy|land|nes|nopaint|notepat|toss|rax)(?:[.:~]|$)/;

export function padCursor({ canvas, piece }) {
  let x, y; // Where the cursor is, in CSS pixels (kept fractional).
  let cell; // The piece pixel last told to the pen, as "x,y".
  let point = { clientX: 0, clientY: 0 }; // That pixel's middle, on the page.
  let shown = false, down = false, target = null;
  let raf = 0, last = 0, held = 0; // `held`: seconds the left stick has been out.
  let mouse; // The real pointer's last place, to tell a move from a re-sent one.
  const was = { a: false, b: false, start: false };

  // The app replaces the system cursor with `html, body, body * { cursor:
  // url(...) !important }`; a class on <html> makes these selectors heavier.
  const style = document.createElement("style");
  style.textContent = `html.pad-cursor, html.pad-cursor body, html.pad-cursor body * { cursor: none !important; }
#pad-cursor { position: fixed; left: 0; top: 0; display: none; pointer-events: none; z-index: 2147483646; image-rendering: pixelated; }
html.pad-cursor #pad-cursor { display: block; }`;
  document.head.append(style);

  // The cursor: the one rax lays on a world panel (lib/cursor-shape.mjs, shared
  // with rax/view.mjs), one piece pixel to its unit: a soft dab of light, and
  // over it a dot and four arms, cyan on a black shadow; closed up and yellow
  // while pressed.
  const face = document.createElement("canvas");
  face.id = "pad-cursor";
  face.width = face.height = SIZE;
  const g = face.getContext("2d");
  document.body.append(face);

  function draw() {
    g.clearRect(0, 0, SIZE, SIZE);
    for (const [dx, dy, [r, gr, b], a] of cursorCells(down)) {
      g.fillStyle = `rgba(${r}, ${gr}, ${b}, ${a})`;
      g.fillRect(MID + dx, MID + dy, 1, 1);
    }
  }
  draw();

  // Lay the cursor on the piece's pixel grid and tell the pen when it has
  // come to a new pixel. The pen floors a point to a pixel, so it is sent the
  // pixel's middle.
  function place(force) {
    const r = canvas.getBoundingClientRect(), s = r.width / canvas.width;
    if (!s) return;
    x = min(max(x ?? r.x + r.width / 2, r.x), r.right - 0.01);
    y = min(max(y ?? r.y + r.height / 2, r.y), r.bottom - 0.01);
    const px = floor((x - r.x) / s), py = floor((y - r.y) / s);
    face.style.width = face.style.height = `${SIZE * s}px`;
    face.style.transform = `translate(${r.x + (px - MID) * s}px, ${r.y + (py - MID) * s}px)`;
    if (!force && cell === `${px},${py}`) return;
    cell = `${px},${py}`;
    point = { clientX: r.x + (px + 0.5) * s, clientY: r.y + (py + 0.5) * s };
    send("pointermove", -1);
  }
  // A mouse's events, at the cursor, to whatever is under it. (The cursor's
  // own canvas takes no pointer events, so it is never the one found.)
  function send(type, button, to) {
    const el = to || document.elementFromPoint(point.clientX, point.clientY) || window;
    const init = { bubbles: true, cancelable: true, composed: true, view: window, ...point, screenX: point.clientX, screenY: point.clientY, button, buttons: down ? 1 : 0 };
    el.dispatchEvent(type.startsWith("pointer")
      ? new PointerEvent(type, { ...init, pointerId: 1, pointerType: "mouse", isPrimary: true, pressure: down ? 0.5 : 0 })
      : new MouseEvent(type, init));
    return el;
  }

  function press() {
    down = true;
    draw();
    target = send("pointerdown", 0);
    send("mousedown", 0, target);
  }

  // `click` follows only a release over what was pressed, as a mouse's does.
  function release(click = true) {
    if (!down) return;
    down = false;
    draw();
    const el = send("pointerup", 0);
    send("mouseup", 0, el);
    if (click && el === target) send("click", 0, el);
    target = null;
  }

  function show() {
    if (shown) return;
    shown = true;
    document.documentElement.classList.add("pad-cursor");
    place(true);
  }

  function hide() {
    release(false);
    if (!shown) return;
    shown = false;
    held = 0;
    document.documentElement.classList.remove("pad-cursor");
  }

  const key = (type) => window.dispatchEvent(new KeyboardEvent(type, { key: "Escape", code: "Escape", keyCode: 27, which: 27, bubbles: true, cancelable: true }));
  const escape = () => (key("keydown"), key("keyup"));

  // A stick's push past the dead zone, rescaled so it starts from nothing.
  function stick(ax = 0, ay = 0) {
    const m = hypot(ax, ay);
    if (m < DEAD) return null;
    const t = min(1, (m - DEAD) / (1 - DEAD));
    return { x: (ax / m) * t, y: (ay / m) * t, t };
  }

  function tick(now) {
    raf = 0;
    let pads = [];
    try { pads = Array.from(navigator.getGamepads?.() || []).filter((p) => p?.connected); } catch {}
    if (!pads.length) return hide(); // Woken again by `gamepadconnected`.
    raf = requestAnimationFrame(tick);
    const dt = min(0.05, (now - last) / 1000); // A stalled frame is not a leap.
    last = now;

    const btn = (i) => pads.some((p) => p.buttons[i]?.pressed);
    const a = btn(0), b = btn(1), start = btn(9);
    const edge = { a: a && !was.a, b: b && !was.b, start: start && !was.start, up: !a && was.a };
    Object.assign(was, { a, b, start });

    // Rax has the screen and the pad while its sheet is up; a window nobody
    // is looking at moves nothing.
    if (document.hidden || !document.hasFocus() || window.acRaxSheet?.up) return hide();

    const name = piece() || "";
    if (!/(?:^|\/)prompt$/.test(name)) {
      hide();
      if (edge.start && !PAD_PIECES.test(name)) escape();
      return;
    }

    // The stick pushed furthest leads. (The 8BitDo Micro's axes past the
    // first two are noise: see lib/gamepad.mjs.)
    let left, right;
    for (const p of pads) {
      const l = stick(p.axes[0], p.axes[1]);
      const r = /2dc8.*9020/.test(p.id) ? null : stick(p.axes[2], p.axes[3]);
      if (l && l.t > (left?.t || 0)) left = l;
      if (r && r.t > (right?.t || 0)) right = r;
    }

    if (left || right) {
      show();
      // Full tilt crosses the screen's longer side in about a second. The
      // square of the push leaves the middle of the stick fine, and the first
      // third of a second eases in, so a flick is a nudge.
      const full = max(innerWidth, innerHeight) * 1.1;
      held = left ? min(1, held + dt / 0.3) : 0;
      const l = left ? full * left.t * (0.4 + 0.6 * held) : 0, r = right ? full * SLOW * right.t : 0;
      x += ((left?.x || 0) * l + (right?.x || 0) * r) * dt;
      y += ((left?.y || 0) * l + (right?.y || 0) * r) * dt;
      place();
    } else held = 0;

    // A first press of A with no cursor up shows where it is; it does not
    // click somewhere unseen.
    if (edge.a) shown ? press() : show();
    if (edge.up) release();
    if (edge.b) escape();
  }

  const wake = () => {
    if (raf) return;
    last = performance.now();
    raf = requestAnimationFrame(tick);
  };

  // The real mouse or a finger takes over, and the cursor starts from there
  // next time. Chrome re-sends a still mouse's move when the page under it
  // changes; that is not the hand.
  const real = (e) => {
    if (!e.isTrusted) return;
    if (e.type === "pointermove" && mouse && mouse.x === e.screenX && mouse.y === e.screenY) return;
    mouse = { x: e.screenX, y: e.screenY };
    x = e.clientX;
    y = e.clientY;
    hide();
  };
  for (const type of ["pointermove", "pointerdown"]) window.addEventListener(type, real, true);

  window.addEventListener("gamepadconnected", wake);
  wake(); // A pad already here (the tick stops itself if there is none).
}
