// 🎮 Pad Cursor
// A game controller's stick moves a cursor over the prompt and its buttons
// click, so someone holding only a pad can press what a mouse can.
//
//   left stick   move
//   A (0)        the mouse button: down on press, up on release (a drag is
//                the stick moving while A is held)
//   R3 (11)      toggle the frame chart
//   View (8)     toggle fullscreen on the prompt
//   B (1)        close the keyboard or mime feed; otherwise Escape
//   START (9)    off the prompt: back to it (Escape, as the keyboard has it).
//                On the prompt lib/rax-sheet.mjs hears START itself.
//
// On the curtain the right stick swipes the mime feed (lib/mime-sheet.mjs):
// down opens it and goes on a card, up comes back a card and then home; the
// d-pad also turns cards inside the feed, and B comes home from anywhere in it.
//
// X starts the prompt typing, and then the pad is a keyboard of rings
// (lib/pad-ring.mjs, drawn by lib/pad-deck.mjs):
//
//   left stick   point at a word: only the words that make a valid command
//                here (a command, then its parameters' values)
//   A (0)        write the word pointed at
//   LB (4)       the next twelve words
//   right stick  point at a petal of the letter wheel; the face buttons then
//                type its four signs (Y top, B right, A bottom, X left)
//   LT (6)       held: the wheel's digits and signs
//   X (2)        Backspace       Y (3)    space
//   RT (7)       Enter           RB (5)   Tab (complete)
//   d-pad        the arrow keys: history up and down, the caret left and right
//   B (1)        stop typing
//
// The deck also draws what each button does, as rax does in its world.
//
// The piece is told nothing new: the presses are PointerEvents sent to the
// element under the cursor, which bubble to the window where lib/pen.mjs
// listens, and the keys are the page's own (a KeyboardEvent, or the letter
// written into the focused text field as a key would write it). Neither pen
// nor keyboard asks whether an event is trusted.
//
// Every piece gets the cursor (A clicks, START goes back to the prompt),
// except one that reads the pad itself: a cursor sliding about under a game's
// stick would be worse than none. Such a piece says so with
// `export const pad = true` (lib/disk.mjs passes it on as
// `window.acPieceReadsPad`); PAD_PIECES names the ones from before that.

import { padDeck } from "./pad-deck.mjs";
import { aim, grammar, offer, more, SLICES, PETALS, SIGNS, FACE } from "./pad-ring.mjs";

const { hypot, min, max, floor, abs, sign } = Math;

const DEAD = 0.15; // The same dead zone lib/gamepad.mjs uses.
const FLICK = 0.6, REST = 0.3; // The right stick swipes past the first and is ready again under the second.
const BADGES = { A: 0, B: 1, X: 2, Y: 3, LB: 4, RB: 5, LT: 6, RT: 7, START: 9 }; // The buttons the deck lights while held.
const SIZE = 3;

// Pieces known to read the pad: START is theirs (a pause, a rematch).
const PAD_PIECES = /(?:^|\/)(?:1v1|arena|fight|gamepad|gameboy|land|nes|toss|rax)(?:[.:~]|$)/;

export function padCursor({ canvas, piece }) {
  let x, y; // Where the cursor is, in CSS pixels (kept fractional).
  let cell; // The piece pixel last told to the pen, as "x,y".
  let point = { clientX: 0, clientY: 0 }; // The visible hotspot, in page coordinates.
  let shown = false, down = false, target = null;
  let raf = 0, last = 0;
  let mouse; // The real pointer's last place, to tell a move from a re-sent one.
  let was = []; // Each button last tick.
  let hand = false; // The pad was the last thing touched: its hints are up.
  const again = []; // When a held button next repeats.
  let flickAt = 0; // When a held swipe of the right stick next repeats (0: at rest).
  let words = null, loading = false; // The grammar the word ring offers from (lib/pad-ring.mjs).
  let page = 0, paged = ""; // Which twelve words are up, and for what line.
  let ringed = false; // X asked for the rings; until then a focused field leaves the stick the cursor.
  let guestDocument;
  const deck = padDeck();

  // The app replaces the system cursor with `html, body, body * { cursor:
  // url(...) !important }`; a class on <html> makes these selectors heavier.
  const style = document.createElement("style");
  style.textContent = `html.prompt-pointer-hidden, html.prompt-pointer-hidden body, html.prompt-pointer-hidden body * { cursor: none !important; }
#pad-cursor { position: fixed; left: 0; top: 0; display: none; pointer-events: none; z-index: 2147483646; image-rendering: pixelated; }
html.pad-cursor #pad-cursor { display: block; }`;
  document.head.append(style);

  // A light point marks the active piece pixel for a pad.
  const face = document.createElement("canvas");
  face.id = "pad-cursor";
  face.width = face.height = SIZE;
  const g = face.getContext("2d");
  document.body.append(face);

  function draw() {
    g.clearRect(0, 0, SIZE, SIZE);
    g.fillStyle = "rgba(0, 0, 0, 0.75)";
    g.fillRect(0, 0, SIZE, SIZE);
    g.fillStyle = down ? "#ffe066" : "#fff";
    g.fillRect(1, 1, 1, 1);
  }
  draw();

  // Draw every stick sample. The piece only needs a move when its pixel changes.
  function place(force = false, dx = 0, dy = 0) {
    const r = canvas.getBoundingClientRect();
    const sx = r.width / canvas.width, sy = r.height / canvas.height;
    if (!sx || !sy) return;
    x = min(max((x ?? r.x + r.width / 2) + dx, r.x), r.right - 0.01);
    y = min(max((y ?? r.y + r.height / 2) + dy, r.y), r.bottom - 0.01);
    face.style.width = `${SIZE * sx}px`;
    face.style.height = `${SIZE * sy}px`;
    face.style.transform = `translate3d(${x - SIZE * sx / 2}px, ${y - SIZE * sy / 2}px, 0)`;
    point = { clientX: x, clientY: y };
    const next = `${floor((x - r.x) / sx)},${floor((y - r.y) / sy)}`;
    if (!force && cell === next) return;
    cell = next;
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
    document.documentElement.classList.remove("pad-cursor");
  }

  // A key, down and up, bubbling from its normal DOM target.
  const CODES = { Escape: 27, Enter: 13, Tab: 9, Backspace: 8, Shift: 16, ArrowLeft: 37, ArrowUp: 38, ArrowRight: 39, ArrowDown: 40 };
  function key(name, to = document.activeElement || document.body) {
    for (const type of ["keydown", "keyup"]) to.dispatchEvent(new KeyboardEvent(type, { key: name, code: name, keyCode: CODES[name], which: CODES[name], bubbles: true, cancelable: true }));
  }
  const escape = () => key("Escape");

  // The text field being typed in, if one is (the prompt's is bios's hidden input).
  const field = () => {
    const el = document.activeElement;
    return el?.id === "software-keyboard-input" ? el : null;
  };
  // Any key but Escape and Tab on the curtain gives the hidden input focus
  // (lib/keyboard.mjs `focusHandler`), and the prompt starts typing.
  const open = () => key("Shift");

  // Type as a keyboard would. A focused field's letters and Backspace reach
  // the piece from the field's own `input` event, so they are written into
  // it; its other keys are key events, as they are from a keyboard.
  function write(el, from, end, text) {
    if (from === end && !text) return;
    el.value = el.value.slice(0, from) + text + el.value.slice(end);
    el.setSelectionRange(from + text.length, from + text.length);
    el.dispatchEvent(new Event("input", { bubbles: true }));
  }
  function type(name) {
    if (!field()) open();
    const el = field();
    if (!el) return;
    const start = el.selectionStart ?? el.value.length, end = el.selectionEnd ?? start;
    if (name.length === 1 || name === "Backspace") {
      const from = name === "Backspace" && start === end ? max(0, start - 1) : start;
      return write(el, from, end, name.length === 1 ? name : "");
    }
    key(name, el);
    // The caret follows an arrow, as the browser moves it after a real one.
    if (name === "ArrowLeft" || name === "ArrowRight") {
      const to = max(0, min(el.value.length, (start === end ? start : name === "ArrowLeft" ? start + 1 : end - 1) + (name === "ArrowLeft" ? -1 : 1)));
      el.setSelectionRange(to, to);
    }
  }

  // A word from the ring takes the place of the part of it already written,
  // and a space follows it when the command takes more.
  function say(word, partial) {
    const el = field();
    if (!el) return;
    const start = el.selectionStart ?? el.value.length, end = el.selectionEnd ?? start;
    const before = el.value.slice(0, start - partial.length) + word;
    write(el, start - partial.length, end, word + (more(words, before, word) ? " " : ""));
  }

  // What can be written, once, the first time the pad types.
  function learn() {
    if (words || loading) return;
    loading = true;
    Promise.all([
      fetch("/docs.json").then((r) => r.json()),
      fetch("/api/piece-hit?top=100").then((r) => r.json()).then((h) => h.pieces || []).catch(() => []),
    ]).then(([docs, hits]) => { words = grammar(docs, hits); })
      .catch(() => {})
      .finally(() => { loading = false; });
  }

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
    if (!pads.length) { was = []; hand = false; deck.hide(); return hide(); } // Woken again by `gamepadconnected`.
    raf = requestAnimationFrame(tick);
    const dt = min(0.05, (now - last) / 1000); // A stalled frame is not a leap.
    last = now;

    const on = Array.from({ length: 16 }, (_, i) => pads.some((p) => p.buttons[i]?.pressed || p.buttons[i]?.value > 0.5));
    const prev = was;
    was = on;
    const pushed = (i) => on[i] && !prev[i];
    // A held button says itself again, as a held key does.
    const held = (i) => {
      if (!on[i]) return false;
      if (prev[i] && now < again[i]) return false;
      again[i] = now + (prev[i] ? 70 : 350);
      return true;
    };
    const edge = { r3: pushed(11), a: pushed(0), b: pushed(1), start: pushed(9), up: !on[0] && prev[0] };

    // Rax has the screen and the pad while its sheet is up; a window nobody
    // is looking at moves nothing.
    if (document.hidden || !document.hasFocus() || window.acRaxSheet?.up) { deck.hide(); return hide(); }

    const name = piece() || "";
    const onPrompt = /(?:^|\/)prompt$/.test(name);
    if (!onPrompt && (window.acPieceReadsPad || PAD_PIECES.test(name))) {
      document.documentElement.classList.remove("prompt-pointer-hidden");
      hide();
      deck.hide();
      return;
    }

    if (pushed(8)) {
      const native = window.webkit?.messageHandlers?.fullscreen;
      if (native) native.postMessage({ on: true });
      else {
        const root = document.documentElement;
        const toggle = document.fullscreenElement || document.webkitFullscreenElement
          ? (document.exitFullscreen || document.webkitExitFullscreen)?.call(document)
          : (root.requestFullscreen || root.webkitRequestFullscreen)?.call(root);
        toggle?.catch(() => {}); // Some browsers require a keyboard or pointer gesture.
      }
    }

    // The Mac app receives the same button directly through GameController.
    if (edge.r3 && !navigator.userAgent.includes("AestheticMac")) {
      window.acFpsHud?.(localStorage.getItem("ac:fps-hud") !== "1");
    }

    // The stick pushed furthest leads. (The 8BitDo Micro's axes past the
    // first two are noise: see lib/gamepad.mjs.)
    let left, right, rawLeft = [0, 0], rawRight = [0, 0], lit;
    for (const p of pads) {
      const l = stick(p.axes[0], p.axes[1]);
      const r = /2dc8.*9020/.test(p.id) ? null : stick(p.axes[2], p.axes[3]);
      if (l && l.t > (left?.t || 0)) { left = l; rawLeft = [p.axes[0], p.axes[1]]; }
      if (r && r.t > (right?.t || 0)) { right = r; rawRight = [p.axes[2], p.axes[3]]; }
    }

    if (left || right || on.some(Boolean)) {
      hand = true;
      document.documentElement.classList.add("prompt-pointer-hidden");
    }
    // Any other piece: the stick moves the cursor, A clicks, START comes home.
    if (!onPrompt) {
      steer(left, dt);
      if (edge.a) shown ? press() : show();
      if (edge.up) release();
      if (edge.start) escape();
      return deck.hide(); // (No hints over a piece's own buttons.)
    }

    const feed = window.acMimeSheet;
    if (!field()) ringed = false;
    const mode = feed?.up ? "mimes" : ringed ? "type" : "curtain";
    // Input in the focused mime iframe doesn't bubble to the parent.
    const guest = feed?.up ? document.activeElement?.contentDocument : null;
    if (guest !== guestDocument) {
      guestDocument?.removeEventListener("pointermove", real, true);
      guestDocument?.removeEventListener("pointerdown", real, true);
      guestDocument?.removeEventListener("keydown", realKey, true);
      guest?.addEventListener("pointermove", real, true);
      guest?.addEventListener("pointerdown", real, true);
      guest?.addEventListener("keydown", realKey, true);
      guestDocument = guest;
    }

    // A swipe: the right stick pushed up or down (and, held, again), or the
    // d-pad. While typing, the sticks point into the rings and the d-pad arrows.
    let swipe = 0;
    if (!right || abs(right.y) < REST) flickAt = 0;
    else if (mode !== "type" && abs(right.y) > FLICK && abs(right.y) > abs(right.x) && (!flickAt || now >= flickAt)) {
      flickAt = now + 600;
      swipe = sign(right.y);
    }
    if (mode === "mimes") swipe ||= pushed(13) ? 1 : pushed(12) ? -1 : 0;

    lit = new Set(Object.keys(BADGES).filter((b) => on[BADGES[b]]));
    if (on[12] || on[13] || on[14] || on[15]) lit.add("D");
    if (mode !== "type" && right && abs(right.y) > FLICK) lit.add("RS");
    let rings = null;
    const draw = () => hand ? deck.draw({ mode, held: lit, point: shown ? point : null, rings }) : deck.hide();

    // The feed has the screen: the pad only turns its cards and comes home.
    if (mode === "mimes") {
      hide();
      if (swipe) feed.step(swipe);
      if (edge.b) feed.home();
      return draw();
    }
    if (swipe) feed?.step(swipe);

    // Typing: the sticks point into the rings and the cursor rests.
    if (mode === "type") {
      hide();
      learn();
      const el = field();
      const line = el.value.slice(0, el.selectionStart ?? el.value.length);
      const { partial, words: all } = offer(words, line);
      if (line !== paged) { paged = line; page = 0; }
      const pages = Math.max(1, Math.ceil(all.length / SLICES));
      if (pushed(4)) page = (page + 1) % pages;
      page = min(page, pages - 1);
      const up = all.slice(page * SLICES, page * SLICES + SLICES);
      const word = left ? aim(rawLeft[0], rawLeft[1], up.length) : -1;
      const petals = on[6] ? SIGNS : PETALS;
      const petal = right ? aim(rawRight[0], rawRight[1], petals.length) : -1;
      if (petal >= 0) {
        // The face buttons spell, each the sign in its place on the petal.
        for (const b of [0, 1, 2, 3]) if (pushed(b)) type(petals[petal][FACE[b]]);
        again[2] = again[3] = Infinity; // Do not run on into delete and space.
      } else {
        if (held(2)) type("Backspace");
        if (held(3)) type(" ");
        if (edge.a && word >= 0) say(up[word], partial);
        if (edge.b) { ringed = false; field()?.blur(); }
      }
      if (pushed(7)) type("Enter");
      if (pushed(5)) type("Tab");
      for (const [i, arrow] of [[12, "ArrowUp"], [13, "ArrowDown"], [14, "ArrowLeft"], [15, "ArrowRight"]]) if (held(i)) type(arrow);
      rings = { words: up, word, partial, page, pages, loading: !words, petals, petal };
      return draw();
    }

    steer(left, dt);

    if (pushed(2)) {
      again[2] = Infinity; // Release the opening X before using it to delete.
      open();
      ringed = true;
    }

    // A first press of A with no cursor up shows where it is; it does not
    // click somewhere unseen.
    if (edge.a) shown ? press() : show();
    if (edge.up) release();
    if (edge.b) escape();
    draw();
  }

  // The left stick moves the cursor, faster the further it is pushed.
  function steer(left, dt) {
    if (!left) return;
    show();
    const full = max(innerWidth, innerHeight) * 1.1;
    const l = full * (0.65 + 0.35 * left.t);
    place(false, left.x * l * dt, left.y * l * dt);
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
    if (e.pointerType === "mouse" && e.type === "pointermove" && mouse && mouse.x === e.screenX && mouse.y === e.screenY) return;
    mouse = e.pointerType === "mouse" ? { x: e.screenX, y: e.screenY } : null;
    x = e.clientX;
    y = e.clientY;
    hand = false;
    document.documentElement.classList.toggle("prompt-pointer-hidden", e.pointerType !== "mouse" && /(?:^|\/)prompt$/.test(piece() || ""));
    hide();
  };
  for (const type of ["pointermove", "pointerdown"]) window.addEventListener(type, real, true);
  const realKey = (e) => { if (e.isTrusted) hand = false; };
  window.addEventListener("keydown", realKey, true);

  window.addEventListener("gamepadconnected", wake);
  wake(); // A pad already here (the tick stops itself if there is none).
}
