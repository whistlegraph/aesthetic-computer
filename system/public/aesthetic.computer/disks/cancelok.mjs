// cancelok, 26.07.14
// A machine makes instruments. You decide which ones live — by wandering.
//
// Each pad plays inside a vignetted screen, like a little television set into a
// wall. Drag INSIDE the screen and you play the pad. Push off the EDGE — the
// bezel, outside the glass — and you leave the room: swipe north, south, east or
// west and the set slides to whatever instrument is in that direction.
//
// The rooms are built as you walk them. Go east and a new pad appears; come back
// west and you're in the room you already saw. Your path persists, directionally,
// the way rooms in a building do — so you are drawing a little map as you go, and
// where you wander, and which rooms you return to, IS the verdict. Nobody asks you
// a question. Staying is yes; walking on is no; coming back is the loudest yes of
// all. It's a dérive, not a queue.
//
// Descended from nopaint: "Press Paint if you like what you see or No if you
// don't." Make it first, judge it after — except now judging is a walk.
//
// The pad is HOSTED, not linked: we import its module and drive its lifecycle
// ourselves, into a surface the size of the screen-within-the-screen. Two pads
// can't run at once (lib/pads.mjs is a session singleton), so a room you leave
// becomes a photograph until you return to it.

const LINGER = 6000; // stay this long in a room and you meant it
const NAME_MS = 2600; // the room's name fades — a label, not furniture
const SWIPE = 26; // travel off the bezel before it counts as leaving

const OK = [90, 255, 150];
const NO = [255, 70, 80];
const BEZEL = [10, 10, 14]; // the wall the set is mounted in

const ORIGIN =
  typeof location !== "undefined" && location.origin && location.origin !== "null"
    ? location.origin
    : "https://aesthetic.computer";
const LOCAL = /localhost|127\.0\.0\.1/.test(ORIGIN);
const API = LOCAL ? "http://localhost:8901/verdict" : `${ORIGIN}/api/cancelok`;
const SESSION = `${Date.now().toString(36)}-${Math.floor(Math.random() * 1e6).toString(36)}`;

let state = "loading"; // loading | judging | error
let error = "";

let deck = []; // the supply of unplaced pads
let dealt = 0; // how many have been placed into rooms
const world = new Map(); // "x,y" → { code, name, lisp } — the rooms you've found
const frozen = new Map(); // "x,y" → a photograph of that room, last time you left it
let pos = { x: 0, y: 0 }; // which room you're standing in
const trail = []; // every room you've entered, in order — the psychogeographic map

let host = null;
let hostFailed = false;
let lisp = null; // a $code room's source, straight from the database
let mounting = false;
const mods = new Map(); // imported modules, kept — walking back shouldn't re-fetch

let judge = null;
let shownAt = 0;
let taps = 0;
let seen = {}; // code → how many times you've walked into it
let sent = 0;
let lost = 0;

let frames = 0;
let fpsAt = 0;
let fps = 0;
let fpsMin = 999;

let stage = null; // the live pad's surface — the size of the glass
let out = null; // the room you just left, frozen, sliding away
let outName = "";
let slide = { dx: 0, dy: 0, t: 0 }; // a walk in progress: direction + eased 0→1
let pending = null; // a walk waiting for its room to load: { dx, dy }
let dragv = null; // a bezel drag in progress: { x0, y0, dx, dy }

const EASE = (t) => 1 - Math.pow(1 - t, 3);
const STEP = 0.08;

const now = () => Date.now();
const key = (p) => `${p.x},${p.y}`;
const current = () => world.get(key(pos));

// The screen-within-the-screen. A margin of wall on every side, a little more
// along the bottom so the timer and the map have somewhere to live.
function glass(w, h) {
  const m = Math.max(16, Math.round(Math.min(w, h) * 0.07));
  const chin = m + 18;
  return { x: m, y: m, w: w - 2 * m, h: h - m - chin, m, chin };
}

async function load(api) {
  const asked = (api.params?.[0] || "").replace(/^\^/, "").toLowerCase();
  const res = await fetch("/aesthetic.computer/bags.json");
  if (!res.ok) throw new Error("bags.json HTTP " + res.status);
  const bag = (await res.json()).bags?.[asked || "pads"];
  if (!bag) throw new Error("no bag named ^" + asked);
  deck = (bag.items || [])
    .filter((it) => it.type === "piece" || it.type === "kidlisp")
    .map((it) => ({ code: it.code, name: it.name || it.code, lisp: it.type === "kidlisp" }));
  if (!deck.length) throw new Error("that bag holds nothing to judge");
  deck.reverse(); // newest first — the fresh rooms are the point

  // Stand in the first room.
  world.set(key(pos), deck[dealt++]);
  trail.push({ ...pos });
}

async function send(api, row) {
  const headers = { "content-type": "application/json" };
  try {
    const token = await Promise.race([
      api.authorize?.() ?? Promise.resolve(null),
      new Promise((r) => setTimeout(() => r(null), 1200)),
    ]);
    if (token) headers.Authorization = `Bearer ${token}`;
  } catch {}
  try {
    const res = await fetch(API, { method: "POST", headers, body: JSON.stringify(row) });
    if (!res.ok) throw new Error("HTTP " + res.status);
    sent += 1;
  } catch {
    lost += 1;
  }
}

// You've left a room. What you did while you were in it IS the verdict.
function leaving(api) {
  const c = current();
  if (!c || !shownAt) return;
  const dwellMs = now() - shownAt;
  const kept = taps > 0 || dwellMs >= LINGER;
  send(api, {
    session: SESSION,
    code: c.code,
    verdict: kept ? "ok" : "cancel",
    dwellMs,
    taps,
    fps: Math.round(fps),
    fpsMin: fpsMin === 999 ? 0 : Math.round(fpsMin),
    revisit: (seen[c.code] || 1) - 1, // times you'd already been here before now
    failed: hostFailed,
    x: pos.x,
    y: pos.y,
    w: api.screen.width,
    h: api.screen.height,
  });
}

function ensureStage(api) {
  const g = glass(api.screen.width, api.screen.height);
  if (!stage || stage.width !== g.w || stage.height !== g.h)
    stage = api.painting(g.w, g.h, (p) => p.wipe(0, 0, 0));
  return stage;
}

async function mount(api) {
  host = null;
  hostFailed = false;
  mounting = true;
  taps = 0;
  shownAt = now();
  frames = 0;
  fpsAt = 0;
  fps = 0;
  fpsMin = 999;
  const c = current();
  const { code } = c;
  seen[code] = (seen[code] || 0) + 1;
  lisp = null;
  const surf = ensureStage(api);
  try {
    if (c.lisp) {
      const res = await fetch(`/api/store-kidlisp?code=${encodeURIComponent(code)}`);
      if (!res.ok) throw new Error("no $" + code);
      const data = await res.json();
      lisp = data.source || data.code || data.kidlisp;
      if (!lisp) throw new Error("$" + code + " has no source");
      host = { lisp: true };
    } else {
      let mod = mods.get(code);
      if (!mod) {
        mod = await import(`${ORIGIN}/aesthetic.computer/disks/${code}.mjs`);
        mods.set(code, mod);
      }
      api.page(surf);
      mod.boot?.({ ...api, screen: surf });
      api.page(api.screen);
      host = mod;
    }
  } catch (e) {
    hostFailed = true;
    error = String(e?.message || e);
  } finally {
    mounting = false;
  }
}

// Walk. Leave this room (fire its verdict, keep its photograph), step to the next
// cell, discover or re-enter whatever's there, and start the set sliding that way.
function walk(api, dx, dy) {
  if (slide.t > 0 || pending) return; // one doorway at a time
  leaving(api);

  if (stage) {
    const keep = api.painting(stage.width, stage.height, () => {});
    keep.pixels.set(stage.pixels);
    frozen.set(key(pos), { frame: keep, name: current().name });
    out = keep;
    outName = current().name;
  }

  pos = { x: pos.x + dx, y: pos.y + dy };
  if (!world.has(key(pos))) {
    world.set(key(pos), deck[dealt++ % deck.length]); // a new room, freshly dealt
  }
  trail.push({ ...pos });

  pending = { dx, dy }; // don't slide until the new room has drawn a frame
  mount(api);
}

function boot(api) {
  api.wipe(...BEZEL);
  api.hud?.label?.("");
  judge = api.handle?.() || null;
  load(api)
    .then(() => {
      state = "judging";
      return mount(api);
    })
    .catch((e) => {
      state = "error";
      error = String(e?.message || e);
    });
}

function sim(api) {
  if (state === "judging" && host && !hostFailed && !host.lisp) {
    try {
      host.sim?.({ ...api, screen: stage });
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    }
  }
  if (slide.t > 0) {
    slide.t += STEP;
    if (slide.t >= 1) {
      slide = { dx: 0, dy: 0, t: 0 };
      out = null;
    }
  }
}

function paint(api) {
  const { wipe, ink, screen } = api;
  const w = screen.width;
  const h = screen.height;

  if (state === "loading") {
    wipe(...BEZEL);
    ink(150).write("finding the first room…", { center: "xy" });
    return;
  }
  if (state === "error") {
    wipe(...BEZEL);
    ink(...NO).write(error, { center: "x", y: h / 2 - 4 });
    return;
  }

  const g = glass(w, h);
  ensureStage(api);

  // Frame rate, honest, on the machine you're holding.
  const t = performance?.now?.() ?? now();
  if (fpsAt) {
    const dt = t - fpsAt;
    if (dt > 0 && dt < 1000) {
      const inst = 1000 / dt;
      fps = fps ? fps + (inst - fps) * 0.1 : inst;
      if (++frames > 30 && fps < fpsMin) fpsMin = fps;
    }
  }
  fpsAt = t;

  // Paint the live room into the glass surface.
  let painted = false;
  if (host && !hostFailed) {
    try {
      api.page(stage);
      if (host.lisp) api.kidlisp(0, 0, g.w, g.h, lisp);
      else host.paint?.({ ...api, screen: stage });
      painted = true;
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    } finally {
      api.page(screen);
    }
  }

  // A walk that was waiting on its room: the room has drawn — now the set slides.
  if (pending && (painted || hostFailed)) {
    slide = { dx: pending.dx, dy: pending.dy, t: 0.0001 };
    pending = null;
  }

  wipe(...BEZEL);

  // Where the two rooms sit inside the glass. The one you're leaving slides out
  // in the direction you walked; the one you're entering slides in behind it.
  let ox = 0;
  let oy = 0; // live room offset
  let px = 0;
  let py = 0; // outgoing room offset
  if (slide.t > 0) {
    const e = EASE(Math.min(1, slide.t));
    ox = slide.dx * g.w * (1 - e);
    oy = slide.dy * g.h * (1 - e);
    px = -slide.dx * g.w * e;
    py = -slide.dy * g.h * e;
  } else if (dragv) {
    // Live nudge while you push off the bezel — the room leans toward the door.
    ox = dragv.dx * 0.32;
    oy = dragv.dy * 0.32;
  }

  if (hostFailed && !slide.t) {
    ink(18, 6, 10).box(g.x, g.y, g.w, g.h);
    ink(...NO).write("didn't run", { x: g.x + g.w / 2 - 30, y: g.y + g.h / 2 - 8 });
  } else {
    if (slide.t > 0 && out)
      api.paste(out, Math.round(g.x + px), Math.round(g.y + py));
    if (painted || !slide.t)
      api.paste(stage, Math.round(g.x + ox), Math.round(g.y + oy));
    else if (out) api.paste(out, g.x, g.y); // still loading — hold the last room
  }

  bezel(api, g, w, h); // the wall masks anything the slide pushed past the glass
  vignette(api, g);
  chrome(api, g, w, h);
}

// The wall. Opaque, so it hides the parts of a sliding room that spill past the
// glass — the mask is free clipping.
function bezel({ ink }, g, w, h) {
  ink(...BEZEL).box(0, 0, w, g.y); // top
  ink(...BEZEL).box(0, g.y + g.h, w, h - (g.y + g.h)); // bottom (the chin)
  ink(...BEZEL).box(0, 0, g.x, h); // left
  ink(...BEZEL).box(g.x + g.w, 0, w - (g.x + g.w), h); // right
  // A thin lit edge where the glass meets the wall — sells the set.
  ink(60, 60, 80).box(g.x - 1, g.y - 1, g.w + 2, 1);
  ink(60, 60, 80).box(g.x - 1, g.y + g.h, g.w + 2, 1);
  ink(60, 60, 80).box(g.x - 1, g.y - 1, 1, g.h + 2);
  ink(60, 60, 80).box(g.x + g.w, g.y - 1, 1, g.h + 2);
}

// The vignette. The glass darkens toward its edges, so the pad sits in the set
// instead of being pasted flat against it.
function vignette({ ink }, g) {
  const V = Math.min(28, Math.round(g.h * 0.09));
  for (let i = 0; i < V; i++) {
    const a = Math.round(150 * Math.pow((V - i) / V, 2));
    ink(0, 0, 0, a).box(g.x, g.y + i, g.w, 1);
    ink(0, 0, 0, a).box(g.x, g.y + g.h - 1 - i, g.w, 1);
    ink(0, 0, 0, a).box(g.x + i, g.y, 1, g.h);
    ink(0, 0, 0, a).box(g.x + g.w - 1 - i, g.y, 1, g.h);
  }
}

// Everything of ours lives in the wall, never on the glass.
function chrome(api, g, w, h) {
  const { ink } = api;
  const c = current();
  const age = now() - shownAt;

  // The room's name, in the top bezel, fading after a moment.
  const fade = slide.t > 0 ? 1 : Math.min(1, Math.max(0, (NAME_MS - age) / 700));
  if (fade > 0 && c) {
    ink(0, 0, 0, 150 * fade).write(c.name, { x: g.x + 1, y: g.m / 2 - 3 });
    ink(240, 240, 255, 255 * fade).write(c.name, { x: g.x, y: g.m / 2 - 4 });
  }
  // How many rooms you've walked — the size of your wander.
  const n = `${trail.length}`;
  ink(90, 90, 110).write(n, { x: w - g.x - n.length * 6, y: g.m / 2 - 4 });

  // The timer, in the chin. Staying IS the vote, so you can watch it happen: the
  // bar fills toward the keep threshold and turns green the instant you cross it.
  if (!slide.t && !pending && host) {
    const kept = taps > 0 || age >= LINGER;
    const frac = kept ? 1 : Math.min(1, age / LINGER);
    const by = g.y + g.h + 6;
    const [r, gr, b] = kept ? OK : [120, 120, 140];
    ink(30, 30, 40).box(g.x, by, g.w, 2);
    ink(r, gr, b, 220).box(g.x, by, Math.round(g.w * frac), 2);
    const secs = `${(age / 1000).toFixed(1)}s${taps ? "  " + taps + "▸" : ""}`;
    ink(kept ? OK : [170, 170, 185]).write(secs, { x: g.x, y: by + 6 });
  }

  minimap(api, g, w, h);

  if (lost > 0) {
    const m = `${lost} unsent`;
    ink(...NO).write(m, { x: w - g.x - m.length * 6, y: h - 9 });
  }
}

// Your psychogeographic map: the rooms you've found, laid where you found them,
// and the path you took through them. Bottom-right of the wall.
function minimap({ ink }, g, w, h) {
  const cell = 5;
  const gap = 2;
  const pitch = cell + gap;
  // Bounds of everything walked, centered on where you are.
  const originX = w - g.x - cell;
  const originY = h - 10 - cell;
  const seenKeys = [...world.keys()].map((k) => k.split(",").map(Number));
  for (const [x, y] of seenKeys) {
    const dx = x - pos.x;
    const dy = y - pos.y;
    const sx = originX + dx * pitch;
    const sy = originY + dy * pitch;
    if (sx < g.x || sy < g.y + g.h) continue; // stay in the corner of the wall
    const here = x === pos.x && y === pos.y;
    ink(here ? OK : [70, 70, 90], here ? 255 : 180).box(sx, sy, cell, cell);
  }
}

// Map a screen event into the glass, or return null if it's not on the glass.
function onGlass(e, g) {
  const x = (e.x ?? 0) - g.x;
  const y = (e.y ?? 0) - g.y;
  if (x < 0 || y < 0 || x >= g.w || y >= g.h) return null;
  return Object.assign(Object.create(Object.getPrototypeOf(e)), e, { x, y });
}

function act(api) {
  const { event: e } = api;
  if (state !== "judging") return;
  const g = glass(api.screen.width, api.screen.height);

  // A touch decides its own allegiance by where it lands. On the glass, it plays
  // the pad. On the wall, it's the start of a walk.
  if (e.is("touch")) {
    if (slide.t > 0 || pending) return;
    const on = onGlass(e, g);
    if (on) {
      dragv = null;
      if (host && !hostFailed) {
        taps += 1;
        try {
          if (!host.lisp) host.act?.({ ...api, screen: stage, event: on });
        } catch (err) {
          hostFailed = true;
          error = String(err?.message || err);
        }
      }
    } else {
      dragv = { x0: e.x, y0: e.y, dx: 0, dy: 0 };
    }
    return;
  }

  // A drag on the wall leans the room toward the door you're pushing.
  if (dragv && e.is("draw")) {
    dragv.dx = (e.x ?? dragv.x0) - dragv.x0;
    dragv.dy = (e.y ?? dragv.y0) - dragv.y0;
    return;
  }

  // A drag on the glass is playing the instrument.
  if (!dragv && e.is("draw") && host && !hostFailed && !host.lisp) {
    const on = onGlass(e, g);
    if (on) {
      try {
        host.act?.({ ...api, screen: stage, event: on });
      } catch (err) {
        hostFailed = true;
        error = String(err?.message || err);
      }
    }
    return;
  }

  // Let go of a wall drag: if you pushed far enough, walk that cardinal way.
  if (dragv && e.is("lift")) {
    const { dx, dy } = dragv;
    dragv = null;
    const ax = Math.abs(dx);
    const ay = Math.abs(dy);
    if (Math.max(ax, ay) < SWIPE) return; // a tap on the wall goes nowhere
    if (ax > ay) return walk(api, dx > 0 ? 1 : -1, 0); // east / west
    return walk(api, 0, dy > 0 ? 1 : -1); // south / north
  }

  // Keys walk the four directions too.
  if (e.is("keyboard:down:arrowright")) return walk(api, 1, 0);
  if (e.is("keyboard:down:arrowleft")) return walk(api, -1, 0);
  if (e.is("keyboard:down:arrowdown")) return walk(api, 0, 1);
  if (e.is("keyboard:down:arrowup")) return walk(api, 0, -1);
}

function leave(api) {
  if (state === "judging") leaving(api);
}

export { boot, sim, paint, act, leave };
