// vertilok, 26.07.17
// A full-screen, vertical-only cancelok feed.

// One instrument fills the viewport. Pull inward from the top or bottom edge to
// move one room at a time; the rest of the screen belongs to the instrument.
// The fixed edge gradients keep the room name legible without putting the piece
// back inside a frame.

const DEADZONE = 8;
const LINGER = 6000;
const WALL = [18, 20, 28];
const NO = [255, 70, 80];

const ORIGIN =
  typeof location !== "undefined" && location.origin && location.origin !== "null"
    ? location.origin
    : "https://aesthetic.computer";
const LOCAL = /localhost|127\.0\.0\.1/.test(ORIGIN);
const API = LOCAL ? "http://localhost:8901/verdict" : `${ORIGIN}/api/cancelok`;
const SESSION = `${Date.now().toString(36)}-${Math.floor(Math.random() * 1e6).toString(36)}`;

let state = "loading";
let error = "";

let deck = [];
let dealt = 0;
let pos = 0;
const world = new Map(); // vertical coordinate -> item
const frozen = new Map(); // vertical coordinate -> last full-screen frame

let host = null;
let hostFailed = false;
let lisp = null;
let stage = null;
const mods = new Map();
const sources = new Map();

let shownAt = 0;
let taps = 0;
let seen = {};
let lost = 0;

let frames = 0;
let fpsAt = 0;
let fps = 0;
let fpsMin = 999;

let grab = null;
let move = null; // { dir, off, lastOff, velocity, live, to, commit, cell, frame }

const now = () => Date.now();
const current = () => world.get(pos);
const edgeSize = (height) => Math.min(48, Math.max(24, Math.round(height * 0.12)));
const edgeAt = (y, height) => {
  const size = edgeSize(height);
  if (y < size) return -1; // top edge: pull down for the previous room
  if (y >= height - size) return 1; // bottom edge: pull up for the next room
  return 0;
};

async function load(api) {
  const asked = (api.params?.[0] || "").replace(/^\^/, "").toLowerCase();
  const res = await fetch("/aesthetic.computer/bags.json");
  if (!res.ok) throw new Error("bags.json HTTP " + res.status);
  const bag = (await res.json()).bags?.[asked || "pads"];
  if (!bag) throw new Error("no bag named ^" + asked);
  deck = (bag.items || [])
    .filter((item) => item.type === "piece" || item.type === "kidlisp")
    .map((item) => ({
      code: item.code,
      name: item.name || item.code,
      lisp: item.type === "kidlisp",
    }));
  if (!deck.length) throw new Error("that bag holds nothing to judge");
  deck.reverse();
  world.set(0, deal());
}

function deal() {
  return deck[dealt++ % deck.length];
}

async function send(api, row) {
  const headers = { "content-type": "application/json" };
  try {
    const token = await Promise.race([
      api.authorize?.() ?? Promise.resolve(null),
      new Promise((resolve) => setTimeout(() => resolve(null), 1200)),
    ]);
    if (token) headers.Authorization = `Bearer ${token}`;
  } catch {}
  try {
    const res = await fetch(API, {
      method: "POST",
      headers,
      body: JSON.stringify(row),
    });
    if (!res.ok) throw new Error("HTTP " + res.status);
  } catch {
    lost += 1;
  }
}

function leaving(api) {
  const item = current();
  if (!item || !shownAt) return;
  const dwellMs = now() - shownAt;
  send(api, {
    session: SESSION,
    code: item.code,
    verdict: taps > 0 || dwellMs >= LINGER ? "ok" : "cancel",
    dwellMs,
    taps,
    fps: Math.round(fps),
    fpsMin: fpsMin === 999 ? 0 : Math.round(fpsMin),
    revisit: (seen[item.code] || 1) - 1,
    failed: hostFailed,
    x: 0,
    y: pos,
    w: api.screen.width,
    h: api.screen.height,
  });
}

function ensureStage(api) {
  const { width, height } = api.screen;
  if (!stage || stage.width !== width || stage.height !== height)
    stage = api.painting(width, height, (p) => p.wipe(...WALL));
  return stage;
}

let padSound = null;
let voiceLog = [];
const sustained = (duration) => duration === "🔁" || duration === Infinity || duration === "infinity";

function wrapSound(sound) {
  if (!sound || typeof sound.synth !== "function") return sound;
  const realSynth = sound.synth.bind(sound);
  const wrapped = Object.create(sound);
  wrapped.synth = (options = {}) => {
    const voice = realSynth(options);
    if (voice && sustained(options.duration)) {
      voiceLog.push(voice);
      if (voiceLog.length > 48) voiceLog.shift()?.kill?.(0.1);
    }
    return voice;
  };
  return wrapped;
}

function killVoices() {
  for (const voice of voiceLog) {
    try {
      voice?.kill?.(0.06);
    } catch {}
  }
  voiceLog = [];
}

function padApi(api, surface, event) {
  if (!padSound) padSound = wrapSound(api.sound);
  const next = { ...api, screen: surface, sound: padSound };
  if (event) next.event = event;
  return next;
}

const SILENT = {
  synth: () => ({ kill() {}, progress: async () => ({ progress: 0 }) }),
  bpm: () => {},
  speaker: { poll() {}, amplitudes: {}, frequencies: {}, beat: {} },
  room: { set() {}, on() {}, off() {}, toggle() {} },
};
const peekApi = (api, surface) => ({ ...api, screen: surface, sound: SILENT });

async function prepare(item) {
  if (item.lisp) {
    if (!sources.has(item.code)) {
      const res = await fetch(`/api/store-kidlisp?code=${encodeURIComponent(item.code)}`);
      if (!res.ok) throw new Error("no $" + item.code);
      const data = await res.json();
      const source = data.source || data.code || data.kidlisp;
      if (!source) throw new Error("$" + item.code + " has no source");
      sources.set(item.code, source);
    }
    return;
  }
  if (!mods.has(item.code))
    mods.set(item.code, await import(`${ORIGIN}/aesthetic.computer/disks/${item.code}.mjs`));
}

async function mount(api) {
  killVoices();
  host = null;
  hostFailed = false;
  lisp = null;
  taps = 0;
  shownAt = now();
  frames = 0;
  fpsAt = 0;
  fps = 0;
  fpsMin = 999;

  const item = current();
  seen[item.code] = (seen[item.code] || 0) + 1;
  const surface = ensureStage(api);
  try {
    await prepare(item);
    if (item.lisp) {
      lisp = sources.get(item.code);
      host = { lisp: true };
    } else {
      host = mods.get(item.code);
      api.page(surface);
      host.boot?.(padApi(api, surface));
      api.page(api.screen);
    }
  } catch (cause) {
    hostFailed = true;
    error = String(cause?.message || cause);
    api.page(api.screen);
  }
}

function restoreCurrent(api) {
  if (!host || host.lisp || !stage) return;
  try {
    api.page(stage);
    host.boot?.(peekApi(api, stage));
    api.page(api.screen);
  } catch {}
}

function shoot(api, item) {
  const { width, height } = api.screen;
  const shot = api.painting(width, height, (p) => p.wipe(...WALL));
  try {
    api.page(shot);
    if (item.lisp) {
      api.kidlisp(0, 0, width, height, sources.get(item.code));
    } else {
      const mod = mods.get(item.code);
      mod.boot?.(peekApi(api, shot));
      mod.sim?.(peekApi(api, shot));
      mod.paint?.(peekApi(api, shot));
    }
  } catch {
    shot.wipe?.(...WALL);
  } finally {
    api.page(api.screen);
  }
  restoreCurrent(api);
  return shot;
}

async function beginMove(api, dir) {
  if (move) return;
  const target = pos + dir;
  if (!world.has(target)) world.set(target, deal());
  const cell = world.get(target);
  try {
    await prepare(cell);
  } catch {}
  if (!grab && !move) return; // a stale async gesture

  let frame = frozen.get(target);
  if (!frame || frame.width !== api.screen.width || frame.height !== api.screen.height)
    frame = shoot(api, cell);
  move = {
    dir,
    off: grab?.synthetic
      ? 0
      : Math.max(-api.screen.height, Math.min(api.screen.height, grab?.dy || 0)),
    lastOff: 0,
    velocity: 0,
    live: true,
    to: 0,
    commit: false,
    cell,
    frame,
  };
}

function releaseMove(api) {
  if (!move) return;
  const height = api.screen.height;
  const fraction = Math.abs(move.off) / height;
  const flick = Math.sign(move.velocity) === Math.sign(-move.dir) && Math.abs(move.velocity) > 4;
  const commit = fraction > 0.18 || flick;
  move.live = false;
  move.to = commit ? -move.dir * height : 0;
  move.commit = commit;
}

function freeze(api) {
  if (!stage) return;
  const copy = api.painting(stage.width, stage.height, () => {});
  copy.pixels.set(stage.pixels);
  frozen.delete(pos);
  frozen.set(pos, copy);
  while (frozen.size > 12) frozen.delete(frozen.keys().next().value);
}

function step(api) {
  leaving(api);
  freeze(api);
  pos += move.dir;
  move = null;
  mount(api);
}

function animateTo(api, dir) {
  if (move) return;
  grab = { sx: 0, sy: 0, begun: true, synthetic: true };
  beginMove(api, dir).then(() => {
    grab = null;
    if (!move) return;
    move.live = false;
    move.to = -dir * api.screen.height;
    move.commit = true;
  });
}

function boot(api) {
  api.wipe(...WALL);
  api.hud?.label?.("");
  api.cursor?.("native");
  load(api)
    .then(() => {
      state = "feeding";
      return mount(api);
    })
    .catch((cause) => {
      state = "error";
      error = String(cause?.message || cause);
    });
}

function sim(api) {
  if (state === "feeding" && host && !hostFailed && !host.lisp) {
    try {
      host.sim?.(padApi(api, stage));
    } catch (cause) {
      hostFailed = true;
      error = String(cause?.message || cause);
    }
  }

  if (move?.live) {
    move.velocity = move.off - move.lastOff;
    move.lastOff = move.off;
  } else if (move) {
    const stiffness = 0.1;
    const damping = 0.64;
    move.velocity = (move.velocity + (move.to - move.off) * stiffness) * damping;
    move.off += move.velocity;
    if (Math.abs(move.to - move.off) < 0.6 && Math.abs(move.velocity) < 0.6) {
      move.off = move.to;
      if (move.commit) step(api);
      else move = null;
    }
  }
}

function edgeGradients(api) {
  const { ink, screen } = api;
  const top = Math.max(28, Math.round(screen.height * 0.18));
  const bottom = Math.max(52, Math.round(screen.height * 0.36));
  const band = 2;

  for (let y = 0; y < top; y += band) {
    const t = 1 - y / top;
    ink(0, 0, 0, Math.round(115 * t * t)).box(0, y, screen.width, band);
  }
  for (let y = 0; y < bottom; y += band) {
    const t = y / bottom;
    ink(0, 0, 0, Math.round(205 * t * t)).box(
      0,
      screen.height - bottom + y,
      screen.width,
      band,
    );
  }
}

function paint(api) {
  const { wipe, ink, screen } = api;
  if (state === "loading") {
    wipe(...WALL);
    ink(150).write("finding the first room…", { center: "xy" });
    return;
  }
  if (state === "error") {
    wipe(...WALL);
    ink(...NO).write(error, { center: "x", y: screen.height / 2 - 4 });
    return;
  }

  ensureStage(api);
  const time = performance?.now?.() ?? now();
  if (fpsAt) {
    const dt = time - fpsAt;
    if (dt > 0 && dt < 1000) {
      const instant = 1000 / dt;
      fps = fps ? fps + (instant - fps) * 0.1 : instant;
      if (++frames > 30 && fps < fpsMin) fpsMin = fps;
    }
  }
  fpsAt = time;

  if (host && !hostFailed) {
    try {
      api.page(stage);
      if (host.lisp) api.kidlisp(0, 0, stage.width, stage.height, lisp);
      else host.paint?.(padApi(api, stage));
    } catch (cause) {
      hostFailed = true;
      error = String(cause?.message || cause);
    } finally {
      api.page(screen);
    }
  }

  wipe(...WALL);
  const offset = Math.round(move?.off || 0);
  if (hostFailed) {
    ink(...WALL).box(0, offset, screen.width, screen.height);
    ink(...NO).write("didn't run", {
      center: "x",
      y: offset + screen.height / 2 - 4,
    });
  } else {
    api.paste(stage, 0, offset);
  }
  if (move?.frame) api.paste(move.frame, 0, offset + move.dir * screen.height);

  edgeGradients(api);
  const item = current();
  ink(0, 0, 0, 150).write(item?.name || item?.code || "", {
    x: 9,
    y: screen.height - 19,
  }, undefined, undefined, false, "MatrixChunky8");
  ink(255).write(item?.name || item?.code || "", {
    x: 8,
    y: screen.height - 20,
  }, undefined, undefined, false, "MatrixChunky8");
}

function forward(api, event) {
  if (!host || hostFailed || host.lisp) return;
  try {
    host.act?.(padApi(api, stage, event));
  } catch (cause) {
    hostFailed = true;
    error = String(cause?.message || cause);
  }
}

function act(api) {
  const e = api.event;
  if (state !== "feeding") return;

  if (e.is("touch")) {
    if (move && !move.live) return;
    const sy = e.y ?? 0;
    grab = {
      sx: e.x ?? 0,
      sy,
      edge: edgeAt(sy, api.screen.height),
      begun: false,
    };
    forward(api, e);
    return;
  }

  if (e.is("draw")) {
    if (!grab || (move && !move.live)) return;
    const dy = (e.y ?? grab.sy) - grab.sy;
    grab.dy = dy;
    const inward =
      (grab.edge === -1 && dy > DEADZONE) ||
      (grab.edge === 1 && dy < -DEADZONE);
    if (!grab.begun && inward) {
      grab.begun = true;
      beginMove(api, grab.edge);
    }
    if (move?.live) {
      const height = api.screen.height;
      move.off = Math.max(-height, Math.min(height, dy));
    } else if (!grab.begun) {
      forward(api, e);
    }
    return;
  }

  if (e.is("lift")) {
    if (move?.live) releaseMove(api);
    else if (grab && !grab.begun) {
      taps += 1;
      forward(api, e);
    }
    grab = null;
    return;
  }

  if (e.is("scroll") || e.is("wheel")) {
    const amount = e.y || e.delta?.y || e.dir || 0;
    if (amount) animateTo(api, amount > 0 ? 1 : -1);
    return;
  }

  if (e.is("keyboard:down:arrowdown") || e.is("keyboard:down:j")) {
    animateTo(api, 1);
    return;
  }
  if (e.is("keyboard:down:arrowup") || e.is("keyboard:down:k")) {
    animateTo(api, -1);
    return;
  }

  if (!grab && !move) forward(api, e);
}

function leave(api) {
  if (state === "feeding") leaving(api);
  killVoices();
  api.overlay?.(null);
}

export { boot, sim, paint, act, leave };
