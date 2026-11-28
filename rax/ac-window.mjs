// 🪟 AC windows: a screen in the world that is a whole Aesthetic Computer
// page (any piece, the prompt among them), stepped in to and used as it is at
// its own address. ac-piece.mjs hosts a disk on a small api of rax's own; a
// piece that wants the real runtime (the chat, the prompt: disk.mjs, the
// net, the store, `jump`) cannot run there, so here the page itself runs in a
// frame of this document, kept out of sight, and its picture is read off its
// canvases on to a world panel (panel.mjs). The panel's mouse is given to the
// page as pointer events; stepped in, the page has the keyboard itself.
//
// One window is one viewer's: each page has its own frame and its own
// session, and nothing done on it is told to the room (a kiosk's presses are:
// client.mjs sharePanel). The frame shares AC's sign-in, being of its origin.
//
// 🌐 That is also the limit. A frame's canvas is read only from its own
// origin: on aesthetic.computer/rax/ (and in the prompt's own rax guest) the
// window is live; on rax.ac it is a still card, and stepping in opens the
// piece on aesthetic.computer in a tab of its own.
//
// 💾 A whole AC page is heavy, so at most one frame is alive: the window
// stepped in to, or else the nearest within WAKE metres. The other keeps the
// last picture it had. A frame is taken down when its room is left, when
// another window wakes, or when nobody has been near it for REST_MS.
import { HOME_ROOM } from './world.mjs';

const FACE = { w: 3.2, h: 2, cols: 320, rows: 200 };
// 🗺️ The windows there are: the piece as typed at AC's prompt, the rooms it
// stands in, and the middle of its face in the room's metres (yaw 0 faces
// south, as a kiosk's does). One more line here is one more window.
export const AC_WINDOWS = [
  // (behind the $rbc screen that stands at z -1.5 in the studio, facing the same way)
  { id: 'ac-laer-klokken', piece: 'laer-klokken', rooms: ['studio'], x: 0, y: 1.5, z: 2, yaw: 0, ...FACE },
  { id: 'ac-prompt', piece: 'prompt', rooms: ['studio'], x: -5.2, y: 1.5, z: 2, yaw: 0, ...FACE },
];

export const WAKE = 9, REST_MS = 30000;
const HOME = 'https://aesthetic.computer';
const BEHIND = '#0f1118'; // what shows where the page is clear (the prompt's curtain is: lib/rax-sheet.mjs)
const FRAME_MS = { inside: 33, near: 100 };

// 🌐 Whether this document may read an AC page: it is one of AC's own, or a
// local site that serves AC beside rax (`npm run site`; rax's own server does
// not). A test says where its stand-in page is with `window.raxWindowURL`.
let readable = null;
function canRead() {
  if (readable !== null) return readable;
  if (globalThis.raxWindowURL) return (readable = true);
  if (/(^|\.)aesthetic\.computer$/.test(location.hostname)) return (readable = true);
  if (!/^(localhost|127\.|\[::1\])/.test(location.hostname)) return (readable = false);
  readable = false; // (until the local site answers)
  fetch('/aesthetic.computer/boot.mjs', { method: 'HEAD' })
    .then((r) => { readable = r.ok && /javascript/.test(r.headers.get('content-type') || ''); })
    .catch(() => {});
  return readable;
}
const addressOf = (piece) => globalThis.raxWindowURL?.(piece) || `/${piece}`;

// The frame's size on the page: so many CSS pixels that AC, at the density it
// will choose (bios.mjs: the one saved, or 1.875 at a desk and 2 in a hand),
// makes a canvas of the panel's own grid, and its letters come over whole.
function density() {
  let saved = 0;
  try { saved = parseFloat(localStorage.getItem('ac-density')); } catch {}
  const ua = navigator.userAgent, handheld = /Mobi|Android|iPhone|iPad/.test(ua) || (/Macintosh/.test(ua) && navigator.maxTouchPoints > 1);
  return saved > 0 ? saved : handheld ? 2 : 1.875;
}

// 🖼️ The page in a frame, behind everything and clear, taking no pointer. It
// is laid out and painted (a frame that is not is put to sleep by the
// browser), so its loop runs and it can hold the keyboard.
function openFrame(def) {
  const k = density(), frame = document.createElement('iframe');
  frame.dataset.raxWindow = def.id; // 🔁 lib/rax-sheet.mjs reads this: a page in a window opens no world of its own
  frame.title = def.piece;
  frame.tabIndex = -1;
  frame.allow = 'autoplay; clipboard-write';
  Object.assign(frame.style, { position: 'fixed', left: '0', top: '0', width: `${def.cols * k}px`, height: `${def.rows * k}px`,
    border: '0', opacity: '0', zIndex: '-1', pointerEvents: 'none', colorScheme: 'normal' });
  frame.src = addressOf(def.piece);
  document.body.append(frame);
  return frame;
}

// 🔁 A page in a window may not be rax: the world would be in itself.
const isRax = (path) => /^\/(rax|botbop)([/~?]|$)/.test(path);

function createWindow(def, { leave }) {
  const pen = new OffscreenCanvas(def.cols, def.rows).getContext('2d', { willReadFrequently: true });
  let frame = null, shotAt = -Infinity, seen = false, said = [], saidUntil = 0, inside = false, tookKeys = null;
  const down = new Set();

  const page = () => { try { return frame?.contentDocument?.body ? frame.contentWindow : null; } catch { return null; } }; // (null once it has gone to another origin)

  // ⌨️ Stepped in, the page has the keyboard as any page does; Escape alone is
  // kept from it, and is the way back out. Set on every page the frame loads.
  function listen() {
    const w = page();
    if (!w || tookKeys === w) return;
    tookKeys = w;
    w.addEventListener('keydown', (e) => {
      if (e.code !== 'Escape' || !inside) return;
      e.preventDefault();
      e.stopImmediatePropagation();
      window.focus();
      leave();
    }, true);
  }

  function open() {
    if (frame) return;
    frame = openFrame(def);
    frame.inert = !inside; // (a page takes the keyboard for itself as it loads, the prompt for one: not until it is stepped in to)
    frame.addEventListener('load', listen);
    seen = false;
  }
  function close() {
    if (!frame) return;
    if (page()?.document.hasFocus()) window.focus();
    frame.remove();
    frame = tookKeys = null;
    down.clear();
  }

  // The page's picture: every canvas it shows, where the page lays it, brought to the panel's grid. → false if there is none yet.
  function shoot(to) {
    const w = page();
    if (!w) return false;
    const doc = w.document, sx = def.cols / w.innerWidth, sy = def.rows / w.innerHeight;
    pen.fillStyle = BEHIND;
    pen.fillRect(0, 0, def.cols, def.rows);
    let drawn = false;
    for (const c of doc.querySelectorAll('canvas')) {
      if (!c.width || !c.height) continue;
      const r = c.getBoundingClientRect();
      if (!r.width || !r.height) continue;
      const style = w.getComputedStyle(c);
      if (style.visibility === 'hidden' || style.display === 'none' || style.opacity === '0') continue;
      pen.imageSmoothingEnabled = c.width > Math.round(r.width * sx); // (a canvas of the grid's own size is copied as it is)
      try { pen.drawImage(c, r.left * sx, r.top * sy, r.width * sx, r.height * sy); drawn = true; } catch {} // (a canvas with another origin's picture on it)
    }
    if (drawn) to.set(pen.getImageData(0, 0, def.cols, def.rows).data);
    return drawn;
  }

  // The panel's mouse as the page's: a pointer event where the panel's pixel is on the page.
  function point(name, x, y, device, pointer = 1) {
    const w = page();
    if (!w) return;
    if (name === 'move' && x < 0) return; // (off the panel's edge: nothing to say to a page)
    const held = down.has(pointer);
    if (name === 'touch') down.add(pointer);
    if (name === 'lift') { if (!held) return; down.delete(pointer); if (inside) hold(); } // (a click on the world took the keyboard from the page: it has it back)
    const type = name === 'touch' ? 'pointerdown' : name === 'lift' ? 'pointerup' : 'pointermove';
    const cx = clamp((x + 0.5) * w.innerWidth / def.cols, 0, w.innerWidth - 1), cy = clamp((y + 0.5) * w.innerHeight / def.rows, 0, w.innerHeight - 1);
    const target = w.document.elementFromPoint(cx, cy) || w.document.body;
    target.dispatchEvent(new w.PointerEvent(type, { bubbles: true, cancelable: true, composed: true, view: w,
      clientX: cx, clientY: cy, screenX: cx, screenY: cy, pointerId: pointer, isPrimary: pointer === 1,
      pointerType: device === 'touch' ? 'touch' : 'mouse', button: type === 'pointermove' ? -1 : 0,
      buttons: name === 'touch' || name === 'draw' ? 1 : 0, pressure: name === 'touch' || name === 'draw' ? 0.5 : 0 }));
  }

  // ⌨️ A key that came to rax's own document while stepped in (the page had lost the keyboard: a click on the world takes it): the page is given the keyboard back, and this one key by hand, typed into its text box if it has one open.
  function key(way, e) {
    const w = page();
    if (!w) return;
    hold();
    const typed = way === 'down' && e.key?.length === 1 && !e.ctrl && !e.meta;
    const kept = (w.document.activeElement || w.document.body).dispatchEvent(new w.KeyboardEvent(way === 'down' ? 'keydown' : 'keyup', { key: e.key, code: e.code, repeat: !!e.repeat,
      shiftKey: !!e.shift, altKey: !!e.alt, ctrlKey: !!e.ctrl, metaKey: !!e.meta, bubbles: true, cancelable: true, composed: true, view: w }));
    const at = w.document.activeElement; // (AC's text box takes the keyboard on the key itself: bios.mjs keyboard.focusHandler)
    if (!kept || !typed || !at || !('selectionStart' in at) || at.selectionStart === null) return;
    at.setRangeText(e.key, at.selectionStart, at.selectionEnd, 'end');
    at.dispatchEvent(new w.InputEvent('input', { data: e.key, inputType: 'insertText', bubbles: true }));
  }
  function hold() {
    const w = page();
    if (!w || w.document.hasFocus()) return;
    frame.focus({ preventScroll: true });
    w.focus();
  }

  function card(g, lines) {
    g.wipe(15, 17, 24).ink(40, 44, 58).box(0, 0, def.cols, def.rows, 'outline');
    g.ink(255, 255, 255).write(def.piece, { x: 14, y: 14, size: 4 }); // (big: a card is read from across the room, on a coarse grid)
    lines.forEach((line, k) => g.ink(150, 160, 185).write(line, { x: 14, y: 70 + k * 26, size: 2 }));
  }

  const piece = {
    keys: true,
    paint(g) {
      const now = performance.now();
      if (now < saidUntil) return card(g, said);
      if (!canRead()) return card(g, ['opens on aesthetic.computer', 'step in to go there']);
      if (!frame) { if (!seen) card(g, ['walk up to wake it']); return; } // (asleep: the last picture it had stays)
      const w = page();
      if (!w) { say('it left aesthetic.computer', `back to ${def.piece}`); return void reset(); }
      if (isRax(w.location.pathname)) { say('rax is already', 'around you'); return void reset(); }
      if (now - shotAt < (inside ? FRAME_MS.inside : FRAME_MS.near)) return;
      shotAt = now;
      listen();
      if (!inside && w.document.hasFocus()) window.focus();
      if (shoot(g.screen.pixels)) seen = true;
      else if (!seen) card(g, ['loading']);
    },
    act({ event }) {
      if (!frame) return;
      if (event.device === 'keyboard') return key(event.name.split(':')[1], event);
      point(event.name, event.x, event.y, event.device, event.pointer);
    },
    leave: close,
  };
  const say = (...lines) => { said = lines; saidUntil = performance.now() + 2500; };
  const reset = () => { close(); open(); };

  return {
    def, piece,
    get live() { return !!frame; },
    get seen() { return seen; },
    open, close,
    // Stepped in to, or out of.
    enter(on) {
      if (on === inside) return;
      inside = on;
      if (on && !canRead()) return void window.open(`${HOME}/${def.piece}`, '_blank', 'noopener');
      if (frame) frame.inert = !on;
      if (on) hold();
      else if (page()?.document.hasFocus()) window.focus();
    },
  };
}
const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v));

// 🪟 The page's windows, on its panels. `leave` is called when Escape is pressed in a window's page: the eye comes out. Each frame the page says where it is: `tick({ room, at, inside, now })`, `at` the body or the drone, `inside` the id of what the eye has gone in to (`panel:…`) or null.
export function acWindows(panels, { leave = () => {} } = {}) {
  const all = AC_WINDOWS.map((def) => {
    const win = createWindow(def, { leave });
    win.panel = panels.mount({ ...def, label: def.piece, piece: win.piece, deep: 0.08 });
    win.panel.hidden = true;
    win.nearAt = -Infinity;
    return win;
  });
  const windows = {
    all,
    tick({ room, at, inside, now }) {
      let want = null, nearest = Infinity;
      for (const win of all) {
        const { def, panel } = win;
        panel.hidden = !def.rooms.includes(room);
        const within = !panel.hidden && `panel:${def.id}` === inside;
        win.enter(within);
        if (panel.hidden) continue;
        const far = at ? Math.hypot(def.x - at.x, def.z - at.z) : Infinity;
        if (far < WAKE) win.nearAt = now;
        // (The one awake keeps the frame until another is a good step nearer: no flapping between two.)
        const pull = within ? -Infinity : far - (win.live ? 1.5 : 0);
        if ((within || far < WAKE) && pull < nearest) { nearest = pull; want = win; }
      }
      if (!canRead()) return;
      for (const win of all) {
        if (win === want) win.open();
        else if (win.live && (win.panel.hidden || want || now - win.nearAt > REST_MS)) win.close();
      }
    },
  };
  if (new URLSearchParams(location.search).has('debug')) globalThis.raxWindows = windows;
  return windows;
}
