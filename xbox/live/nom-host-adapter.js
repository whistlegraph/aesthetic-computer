// Native Xbox adapter for the shared Nom engine. Appended by bundle-nom.mjs
// inside the same closure as nomBoot/nomSim/nomPaint/nomAct/nomLeave.

let xboxNomSeed = 0x4e4f4d;
let xboxNomScreen = { width: 1920, height: 1080 };
let xboxNomInk = [255, 255, 255, 255];
let xboxNomBackdrop = [0, 0, 0];
let xboxNomPreviousButtons = new Set();

function xboxNomRandom() {
  xboxNomSeed = (Math.imul(xboxNomSeed, 1664525) + 1013904223) >>> 0;
  return xboxNomSeed / 0x100000000;
}

function xboxNomRandInt(maximum) {
  return Math.floor(xboxNomRandom() * (Math.max(0, Number(maximum) || 0) + 1));
}

function xboxNomView() {
  const view = typeof gameView === "function" ? gameView() : null;
  xboxNomScreen = {
    width: Math.max(320, Math.round(Number(view?.width) || 1920)),
    height: Math.max(240, Math.round(Number(view?.height) || 1080)),
  };
}

function xboxNomColor(values) {
  const color = values.length === 1 && Array.isArray(values[0]) ? values[0] : values;
  return [
    Math.max(0, Math.min(255, Math.round(Number(color[0]) || 0))),
    Math.max(0, Math.min(255, Math.round(Number(color[1]) || 0))),
    Math.max(0, Math.min(255, Math.round(Number(color[2]) || 0))),
    color[3] == null ? 255 : Math.max(0, Math.min(255, Math.round(Number(color[3]) || 0))),
  ];
}

function xboxNomOpaque(color) {
  if (color[3] >= 250) return color;
  const alpha = color[3] / 255;
  return [0, 1, 2].map((index) =>
    Math.round(color[index] * alpha + xboxNomBackdrop[index] * (1 - alpha)));
}

function xboxNomRect(x, y, width, height, style) {
  // The current OSKIEWAR host's box primitive is opaque. Dropping translucent
  // overlays preserves the board; drawing their blended color as an opaque
  // rectangle would erase every command beneath it.
  if (xboxNomInk[3] < 250) return;
  const [r, g, b] = xboxNomOpaque(xboxNomInk);
  if (style === "outline") {
    box(x, y, width, 1, r, g, b);
    box(x, y + height - 1, width, 1, r, g, b);
    box(x, y, 1, height, r, g, b);
    box(x + width - 1, y, 1, height, r, g, b);
  } else box(x, y, width, height, r, g, b);
}

function xboxNomLine(x1, y1, x2, y2) {
  const [r, g, b] = xboxNomOpaque(xboxNomInk);
  line(x1, y1, x2, y2, 1, r, g, b);
}

function xboxNomWrite(value, position = {}) {
  const text = String(value || "");
  const size = Number(position.size) || 1;
  const pixelSize = size * 10;
  const x = position.center === "x"
    ? (xboxNomScreen.width - text.length * 6 * size) / 2
    : Number(position.x) || 0;
  const y = Number(position.y) || 0;
  const [r, g, b] = xboxNomOpaque(xboxNomInk);
  if (typeof comicWrite === "function") comicWrite(text, x, y, pixelSize, r, g, b);
  else write(text, x, y, pixelSize, r, g, b);
}

const xboxNomChain = {
  box: xboxNomRect,
  line: xboxNomLine,
  write: xboxNomWrite,
  hd: false,
};

function xboxNomInkFn(...values) {
  xboxNomInk = xboxNomColor(values);
  return xboxNomChain;
}

function xboxNomWipe(...values) {
  xboxNomBackdrop = xboxNomColor(values).slice(0, 3);
  wipe(...xboxNomBackdrop);
}

function xboxNomFrequency(note) {
  const match = String(note).toLowerCase().match(/^([a-g])([#b]?)(-?\d)?$/);
  if (!match) return Number(note) || 440;
  const semitones = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
  const accidental = match[2] === "#" ? 1 : match[2] === "b" ? -1 : 0;
  const octave = match[3] == null ? 4 : Number(match[3]);
  return 440 * Math.pow(2, (semitones[match[1]] + accidental + (octave - 4) * 12 - 9) / 12);
}

const xboxNomSound = {
  freq: xboxNomFrequency,
  synth(options = {}) {
    const tone = Number(options.tone) || 440;
    const volume = Number(options.volume) || .2;
    if (options.duration === "🔁" && typeof oscillator === "function") {
      oscillator(tone, volume);
      return { kill: () => typeof oscillatorStop === "function" && oscillatorStop() };
    }
    xboxNativeSynth(tone, Math.max(.01, Number(options.duration) || .08));
    return { kill() {}, update() {} };
  },
};

class XboxNomHourglass {
  constructor(maximum = 1, options = {}) {
    this.maximum = Math.max(1, Number(maximum) || 1);
    this.options = options;
    this.ticks = 0;
  }
  step() {
    this.ticks += 1;
    if (this.ticks < this.maximum) return;
    this.options.completed?.();
    this.options.flipped?.();
    if (this.options.autoFlip) this.ticks = 0;
  }
}

const xboxNomNum = { randInt: xboxNomRandInt };
const xboxNomGizmo = { Hourglass: XboxNomHourglass };
const xboxNomClock = {
  resync() {},
  time() {
    const snapshot = typeof runtime === "function" ? runtime() : null;
    return new Date(Number(snapshot?.unixMs) || Date.now());
  },
};

function xboxNomEvent(name) {
  return { name, is: (candidate) => candidate === name };
}

function xboxNomDispatch(button, down) {
  const key = {
    ArrowLeft: "arrowleft", ArrowRight: "arrowright",
    ArrowUp: "arrowup", ArrowDown: "arrowdown",
    A: "space", Menu: "enter",
  }[button];
  if (!key) return;
  nomAct({
    event: xboxNomEvent(`keyboard:${down ? "down" : "up"}:${key}`),
    sound: xboxNomSound,
    speak() {}, cursor() {}, net: {}, num: xboxNomNum,
  });
}

function xboxNomSampleInput() {
  const snapshot = typeof gamepad === "function" ? gamepad(0) : { down: [] };
  const current = new Set(Array.isArray(snapshot?.down) ? snapshot.down : []);
  for (const button of current)
    if (!xboxNomPreviousButtons.has(button)) xboxNomDispatch(button, true);
  for (const button of xboxNomPreviousButtons)
    if (!current.has(button)) xboxNomDispatch(button, false);
  xboxNomPreviousButtons = current;
}

const xboxNomPaintApi = {
  get screen() { return xboxNomScreen; },
  get dark() { return true; },
  wipe: xboxNomWipe,
  ink: xboxNomInkFn,
  box: xboxNomRect,
  line: xboxNomLine,
  write: xboxNomWrite,
};

globalThis.boot = function boot() {
  xboxNomView();
  xboxNomPreviousButtons.clear();
  nomBoot({
    params: ["danish"], hud: { label() {} }, clock: xboxNomClock,
    net: {},
    handle: () => typeof nomHandle === "function" ? nomHandle() : null,
    authorize: null, num: xboxNomNum,
  });
  if (typeof telemetry === "function") telemetry("NOM_READY", "dannom shared-engine");
};

globalThis.sim = function sim() {
  xboxNomView();
  xboxNomSampleInput();
  nomSim({
    gizmo: xboxNomGizmo, seconds: (value) => value * 60,
    sound: xboxNomSound, clock: xboxNomClock, num: xboxNomNum,
  });
};

globalThis.paint = function paint() {
  xboxNomView();
  nomPaint(xboxNomPaintApi);
};

// OSKIEWAR samples controller state in its fixed simulation step too. Keeping
// act empty prevents a native edge callback from advancing Nom twice.
globalThis.act = function act() {};
globalThis.leave = function leave() { nomLeave(); };
