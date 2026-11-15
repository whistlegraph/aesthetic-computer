// nes, 26.11.15
// A NES cartridge in AC's own screen. `nes~<id>` follows an Aesel draft:
// it asks /api/easel-live for the newest build and swaps the cartridge in as
// each version lands, so the maker's phone and the watcher's see the same game.

const W = 256, H = 240, FRAME = 1000 / 60;
const A = 0, B = 1, SELECT = 2, START = 3, UP = 4, DOWN = 5, LEFT = 6, RIGHT = 7;
const KEYS = { x: A, z: B, shift: SELECT, enter: START, arrowup: UP, arrowdown: DOWN, arrowleft: LEFT, arrowright: RIGHT };
const PAD = { 0: A, 1: B, 8: SELECT, 9: START, 12: UP, 13: DOWN, 14: LEFT, 15: RIGHT }; // standard gamepad
const BG = [20, 17, 28], TEAL = [122, 231, 206], PINK = [229, 107, 164];

// The core and the header check are the ones the MIME player uses. A piece
// can be loaded from a blob, so the address has to be whole.
const home = /^https?:/.test(globalThis.location?.origin || "") ? globalThis.location.origin : "https://aesthetic.computer";

let core, nes, inspectROM, send, rate = 48000;
let picture, fresh = false, samples = [], debt = 0, last = 0;
let live, note = "loading", bad = false;
const sources = new Map(); // who is holding what: "key:x" → A
const pads = {}; // jsnes key → ui.Button
let lay;

const holding = (key) => [...sources.values()].includes(key);

// Two fingers, a key and a stick can all hold the same button; it lets go
// when the last of them does.
function hold(source, key) {
  const before = new Set(sources.values());
  if (key === undefined) sources.delete(source); else sources.set(source, key);
  const after = new Set(sources.values());
  for (let k = 0; k < 8; k++) if (before.has(k) !== after.has(k)) nes?.[after.has(k) ? "buttonDown" : "buttonUp"](1, k);
}

function insert(bytes) {
  try {
    const info = inspectROM(bytes);
    if (!info.supported) throw new Error(info.console ? "that console is not supported" : `mapper ${info.mapper} is not supported`);
    const next = new core.NES({
      sampleRate: rate,
      onFrame: (buffer) => {
        const px = picture.pixels;
        for (let i = 0, o = 0; i < buffer.length; i++, o += 4) {
          const c = buffer[i];
          px[o] = c & 255; px[o + 1] = (c >> 8) & 255; px[o + 2] = (c >> 16) & 255;
        }
        fresh = true;
      },
      onAudioSample: (l, r) => samples.push(l, r),
    });
    next.loadROM(bytes);
    for (const key of new Set(sources.values())) next.buttonDown(1, key);
    nes = next; bad = false; debt = last = 0; samples = [];
    send({ type: "nes:audio", content: "clear" });
  } catch (err) {
    bad = true; note = err.message.toLowerCase();
  }
}

// One round of the same conversation the watch page has with the relay.
async function follow() {
  if (!live || live.gone) return;
  let wait = 750;
  const ask = (query) => fetch(`${home}/api/easel-live?${query}`, { cache: "no-store", credentials: "omit" });
  try {
    const res = await ask(`${live.id.length === 12 ? "code" : "id"}=${live.id}`);
    const meta = res.ok ? await res.json() : null;
    if (res.status === 404 || res.status === 410 || meta?.status === "stopped") {
      live.sequence = 0; note = nes ? "maker away" : "waiting for the maker"; wait = 3000;
    } else if (!meta) throw new Error();
    else if (meta.kind !== "nes") { bad = true; note = `that draft is a ${meta.kind}`; wait = 3000; }
    else if (meta.sequence > live.sequence) {
      const rom = await ask(`id=${meta.id}&frame=${meta.sequence}`);
      if (rom.status === 409) wait = 250;
      else if (!rom.ok) throw new Error();
      else {
        insert(new Uint8Array(await rom.arrayBuffer()));
        live.sequence = meta.sequence;
        if (!bad) note = `live v${meta.version}`;
      }
    } else if (!bad) note = `live v${meta.version}`;
  } catch {
    note = "reconnecting"; wait = 3000;
  }
  if (!live.gone) live.timer = setTimeout(follow, wait);
}

// The game takes the room the controls leave: beside it when the screen is
// wide, under it when the screen is tall. Whole-number scales where they fit.
function arrange({ width: w, height: h }, geo) {
  const top = 20, gap = 4, wide = w > h * 1.15;
  const u = Math.max(12, Math.min(26, Math.floor(Math.min(w, h) / (wide ? 8 : 9)))); // one d-pad cell
  const side = u * 3 + gap * 2, strip = u * 4 + gap * 2;
  const room = wide ? { x: side, y: top, w: w - side * 2, h: h - top - gap } : { x: gap, y: top, w: w - gap * 2, h: h - top - strip };
  const fit = Math.min(room.w / W, room.h / H);
  const scale = fit >= 1 ? Math.floor(fit) : Math.max(0.25, Math.floor(fit * 8) / 8);
  const gw = Math.round(W * scale), gh = Math.round(H * scale);
  const game = { x: Math.floor(room.x + (room.w - gw) / 2), y: Math.floor(room.y + (room.h - gh) / 2), w: gw, h: gh, scale };

  const cy = wide ? Math.floor(top + (h - top) / 2) : h - gap - u * 2 - Math.floor(u / 2);
  const dx = gap, dy = cy - Math.floor(u * 1.5); // d-pad corner
  const r = Math.floor(u * 1.4), ax = w - gap - r * 2 - Math.floor(r * 0.8); // A and B share a diagonal
  const small = { w: Math.max(22, Math.floor(u * 1.6)), h: Math.max(10, Math.floor(u * 0.7)) };
  const sy = wide ? cy + r + u : h - gap - small.h;
  const sx = wide ? w - gap - small.w * 2 - 2 : Math.floor(w / 2) - small.w - 1;
  const boxes = {
    [UP]: [dx + u, dy, u, u], [DOWN]: [dx + u, dy + u * 2, u, u],
    [LEFT]: [dx, dy + u, u, u], [RIGHT]: [dx + u * 2, dy + u, u, u],
    [B]: [ax, cy - Math.floor(r * 0.3), r, r], [A]: [w - gap - r, cy - Math.floor(r * 0.9), r, r],
    [SELECT]: [sx, sy, small.w, small.h], [START]: [sx + small.w + 2, sy, small.w, small.h],
  };
  // A new Button would drop what a finger is holding, so only the box moves.
  for (const key in boxes) if (pads[key]) pads[key].box = new geo.Box(...boxes[key]);
  return { game, boxes };
}

async function boot({ params, hud, sound, screen, ui, geo, send: s }) {
  send = s;
  nes = null; bad = false; note = "loading"; sources.clear();
  rate = sound?.sampleRate || rate;
  picture = { width: W, height: H, pixels: new Uint8ClampedArray(W * H * 4).fill(255) };
  lay = arrange(screen, geo);
  for (const key in lay.boxes) pads[key] = new ui.Button(...lay.boxes[key]);
  hud.label("nes");

  const id = (params[0] || "").toLowerCase();
  if (!/^(?:[a-f0-9]{12}|[a-f0-9]{32})$/.test(id)) { bad = true; note = "no cartridge"; return; }
  try {
    // A UMD build: as a module it has no exports and hangs itself on the global.
    core = globalThis.jsnes || (await import(`${home}/nes-player/vendor/jsnes-2.1.0.min.js`)).default || globalThis.jsnes;
    ({ inspectROM } = await import(`${home}/nes-player/rom.mjs`));
  } catch {
    bad = true; note = "the nes core did not load"; return;
  }
  live = { id, sequence: 0 };
  follow();
}

function reframed({ screen, geo }) {
  lay = arrange(screen, geo);
}

function sim({ sound }) {
  if (!nes || bad) return;
  if (sound?.sampleRate && sound.sampleRate !== rate) {
    rate = nes.opts.sampleRate = nes.papu.sampleRate = sound.sampleRate;
    nes.setFramerate(60);
  }
  const now = performance.now();
  debt = Math.min(FRAME * 4, debt + (last ? now - last : FRAME)); last = now;
  try {
    while (debt >= FRAME) { nes.frame(); debt -= FRAME; }
  } catch {
    bad = true; note = "the cartridge stopped"; return;
  }
  if (samples.length) {
    send({ type: "nes:audio", content: new Float32Array(samples) });
    samples = [];
  }
}

function paint({ wipe, ink, paste, screen, line }) {
  wipe(...BG);
  if (!lay) return;
  const g = lay.game;
  ink(...TEAL, 60).box(g.x - 2, g.y - 2, g.w + 4, g.h + 4, "outline");
  ink(0).box(g.x, g.y, g.w, g.h);
  if (nes && !bad) paste(picture, g.x, g.y, g.scale);
  else if (!bad) {
    const t = Math.floor(performance.now() / 120) % 8; // a row of lights while the cartridge is on its way
    for (let i = 0; i < 8; i++) ink(...TEAL, i === t ? 255 : 50).box(g.x + g.w / 2 - 30 + i * 8, g.y + g.h / 2 - 2, 4, 4);
  }
  if (bad) ink(...PINK).write(note, { center: "x", y: Math.floor(g.y + g.h / 2 - 5) }, undefined, g.w - 8);
  else ink(...TEAL, 150).write(note, { x: screen.width - note.length * 6 - 5, y: 6 });

  for (const key of [UP, DOWN, LEFT, RIGHT]) {
    const [x, y, w, h] = lay.boxes[key], on = holding(key);
    ink(...TEAL, on ? 255 : 80).box(x + 1, y + 1, w - 2, h - 2, on ? "fill" : "outline");
    const mx = x + w / 2, my = y + h / 2, q = Math.max(2, Math.floor(w / 5));
    ink(...(on ? BG : TEAL));
    if (key === UP) { line(mx - q, my + q / 2, mx, my - q / 2); line(mx, my - q / 2, mx + q, my + q / 2); }
    if (key === DOWN) { line(mx - q, my - q / 2, mx, my + q / 2); line(mx, my + q / 2, mx + q, my - q / 2); }
    if (key === LEFT) { line(mx + q / 2, my - q, mx - q / 2, my); line(mx - q / 2, my, mx + q / 2, my + q); }
    if (key === RIGHT) { line(mx - q / 2, my - q, mx + q / 2, my); line(mx + q / 2, my, mx - q / 2, my + q); }
  }
  for (const key of [A, B]) {
    const [x, y, w] = lay.boxes[key], r = w / 2, on = holding(key);
    ink(...PINK, on ? 255 : 70).circle(x + r, y + r, r - 1, true);
    ink(...PINK).circle(x + r, y + r, r - 1);
    ink(...(on ? BG : [255, 255, 255])).write(key === A ? "A" : "B", { x: Math.floor(x + r - 3), y: Math.floor(y + r - 5) });
  }
  for (const key of [SELECT, START]) {
    const [x, y, w, h] = lay.boxes[key], on = holding(key), word = key === SELECT ? "sel" : "go";
    ink(...TEAL, on ? 255 : 70).box(x, y, w, h, on ? "fill" : "outline");
    if (h >= 12) ink(...(on ? BG : TEAL)).write(word, { x: Math.floor(x + (w - word.length * 6) / 2), y: Math.floor(y + (h - 10) / 2) });
  }
}

function act({ event: e, pens }) {
  for (const name in KEYS) {
    if (e.is(`keyboard:down:${name}`)) hold(`key:${name}`, KEYS[name]);
    if (e.is(`keyboard:up:${name}`)) hold(`key:${name}`);
  }
  if (e.is("gamepad")) {
    if (e.button in PAD) hold(`pad:${e.gamepad}:${e.button}`, e.action === "push" ? PAD[e.button] : undefined);
    if (e.axis === 0 || e.axis === 1) {
      const way = Math.abs(e.value) < 0.5 ? undefined : e.axis === 0 ? (e.value < 0 ? LEFT : RIGHT) : e.value < 0 ? UP : DOWN;
      hold(`stick:${e.gamepad}:${e.axis}`, way);
    }
  }
  for (const key in pads) {
    const k = Number(key), on = () => hold(`touch:${k}`, k), off = () => hold(`touch:${k}`);
    // `over` is a finger sliding back onto a pad it rolled off.
    pads[key].act(e, { down: on, over: on, up: off, push: off, out: off, cancel: off }, pens?.());
  }
}

function leave() {
  if (live) { live.gone = true; clearTimeout(live.timer); }
  sources.clear();
  nes = null;
  send?.({ type: "nes:audio", content: "clear" });
}

export { boot, paint, sim, act, reframed, leave };
