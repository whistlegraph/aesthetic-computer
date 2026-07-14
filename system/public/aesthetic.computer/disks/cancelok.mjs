// cancelok, 26.07.14
// A machine makes instruments. You decide which ones live — by wandering.
//
// Each pad plays inside a soft-edged screen, like an old television set deep in a
// dark wall. Drag on the GLASS and you play the pad. Push off the wall around it
// and you leave the room: the set follows your finger and slides — north, south,
// east or west — to whatever instrument is in that direction, and it locks to the
// axis you started moving on, the way a feed does.
//
// The rooms are built as you walk them and they persist directionally: go east and
// a new pad is dealt; come back west and you're in the room you already saw. Where
// you wander, and which rooms you return to, IS the verdict — staying is yes,
// walking on is no, coming back is the loudest yes. A dérive, not a queue.
//
// Two pads can't run at once (lib/pads.mjs is a session singleton), so a room you
// leave is a photograph until you walk back into it — which is the honest metaphor,
// not a workaround: you are remembering rooms, not running them all at once.

const LINGER = 6000; // stay this long in a room and you meant it
const NAME_MS = 2600; // the name fades — a label, not furniture
const DEADZONE = 7; // finger travel before a drag picks its axis
const FEATHER = 14; // the fuzzy width of the screen's edge, in pixels

const OK = [90, 255, 150];
const NO = [255, 70, 80];
const WALL = [8, 8, 11]; // the dark the set is sunk into
const GLOW = [24, 58, 92]; // the wall lights up when you take hold to walk

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
const world = new Map(); // "x,y" → { code, name, lisp }
const frozen = new Map(); // "x,y" → a photograph of that room, last time you left
let pos = { x: 0, y: 0 };
const trail = [];

let host = null;
let hostFailed = false;
let lisp = null;
let mounting = false;
const mods = new Map();

let judge = null;
let shownAt = 0;
let taps = 0;
let seen = {};
let lost = 0;

let frames = 0;
let fpsAt = 0;
let fps = 0;
let fpsMin = 999;

let stage = null; // the live pad's surface — the size of the glass

// A move in progress. `off` is how far the set has slid along its locked axis,
// live under your finger; `anim` eases it home (to full = commit, to 0 = snap
// back) when you let go. `in` is the neighbor room you're sliding toward —
// frozen, because a singleton can't run two pads at once.
let move = null; // { dx, dy, off, live, anim, from, to, frame }

const now = () => Date.now();
const key = (p) => `${p.x},${p.y}`;
const current = () => world.get(key(pos));
const EASE = (t) => 1 - Math.pow(1 - t, 3);

// The screen-within-the-screen. A deep margin of wall on every side.
function glass(w, h) {
  const m = Math.max(24, Math.round(Math.min(w, h) * 0.12));
  return { x: m, y: m, w: w - 2 * m, h: h - 2 * m, m };
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
  deck.reverse();
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
  } catch {
    lost += 1;
  }
}

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
    revisit: (seen[c.code] || 1) - 1,
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

// A photograph of the room in a given direction, so it can slide in under your
// finger. If you've been there, it's your last frame of it; if not, we boot it
// once, steal a frame, and boot the current room back (a singleton can't hold
// two live). If it can't be shot, a wall-dark frame slides in — never black.
function peekFrame(api, cellKey, cell) {
  if (frozen.has(cellKey)) return frozen.get(cellKey).frame;
  const g = glass(api.screen.width, api.screen.height);
  const shot = api.painting(g.w, g.h, (p) => p.wipe(...WALL));
  if (cell && !cell.lisp) {
    try {
      const mod = mods.get(cell.code);
      if (mod) {
        api.page(shot);
        mod.boot?.({ ...api, screen: shot });
        mod.sim?.({ ...api, screen: shot });
        mod.paint?.({ ...api, screen: shot });
        api.page(api.screen);
        // Put the current room back — its boot re-asserts its own config.
        if (host && !host.lisp && stage) {
          api.page(stage);
          host.boot?.({ ...api, screen: stage });
          api.page(api.screen);
        }
      }
    } catch {
      // wall-dark it is.
    }
  }
  return shot;
}

// Begin a walk in a travel direction (dx,dy in grid steps). Find the room that
// way and grab its photograph so it can slide in as you drag.
async function beginMove(api, dx, dy) {
  const target = { x: pos.x + dx, y: pos.y + dy };
  const tk = key(target);
  if (!world.has(tk)) world.set(tk, deck[dealt++ % deck.length]);
  // Cache the target module first (so peek can boot it synchronously next frame).
  const cell = world.get(tk);
  if (cell && !cell.lisp && !mods.get(cell.code)) {
    try {
      mods.set(cell.code, await import(`${ORIGIN}/aesthetic.computer/disks/${cell.code}.mjs`));
    } catch {}
  }
  move = { dx, dy, off: 0, live: true, anim: 0, from: 0, to: 0, frame: null, target: tk, cell };
}

// You let go. If you pushed past the doorway, finish the slide and step through;
// otherwise it eases back and you never left.
function releaseMove(api, dim) {
  if (!move) return;
  const past = Math.abs(move.off) > dim * 0.22;
  move.live = false;
  move.from = move.off;
  move.to = past ? (move.off > 0 ? dim : -dim) : 0;
  move.commit = past;
  move.anim = 0.0001;
}

// The slide finished on a commit: fire the verdict, freeze this room, step into
// the next, and bring it to life.
function step(api) {
  leaving(api);
  if (stage) {
    const keep = api.painting(stage.width, stage.height, () => {});
    keep.pixels.set(stage.pixels);
    frozen.set(key(pos), { frame: keep, name: current().name });
  }
  pos = { x: pos.x + move.dx, y: pos.y + move.dy };
  trail.push({ ...pos });
  move = null;
  mount(api);
}

function boot(api) {
  api.wipe(...WALL);
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
  // Ease a released move home.
  if (move && !move.live) {
    move.anim += 0.09;
    const e = EASE(Math.min(1, move.anim));
    move.off = move.from + (move.to - move.from) * e;
    if (move.anim >= 1) {
      if (move.commit) step(api);
      else move = null;
    }
  }
}

function paint(api) {
  const { wipe, ink, screen } = api;
  const w = screen.width;
  const h = screen.height;

  if (state === "loading") {
    wipe(...WALL);
    ink(150).write("finding the first room…", { center: "xy" });
    return;
  }
  if (state === "error") {
    wipe(...WALL);
    ink(...NO).write(error, { center: "x", y: h / 2 - 4 });
    return;
  }

  const g = glass(w, h);
  ensureStage(api);

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

  // Grab the neighbor's photograph the first frame a move exists.
  if (move && !move.frame) move.frame = peekFrame(api, move.target, move.cell);

  // Paint the live room into the glass surface.
  if (host && !hostFailed) {
    try {
      api.page(stage);
      if (host.lisp) api.kidlisp(0, 0, g.w, g.h, lisp);
      else host.paint?.({ ...api, screen: stage });
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    } finally {
      api.page(screen);
    }
  }

  wipe(...WALL);

  // Where the two rooms sit. The live one slides by `off`; the neighbor rides the
  // opposite side, so they move together like a filmstrip.
  const off = move ? move.off : 0;
  const ax = move && move.dx ? off : 0;
  const ay = move && move.dy ? off : 0;

  if (hostFailed && !move) {
    ink(18, 6, 10).box(g.x, g.y, g.w, g.h);
    ink(...NO).write("didn't run", { x: g.x + g.w / 2 - 30, y: g.y + g.h / 2 - 8 });
  } else {
    api.paste(stage, Math.round(g.x + ax), Math.round(g.y + ay));
    if (move && move.frame) {
      // The neighbor sits on the side you're walking toward and rides in as the
      // current room slides away — a filmstrip of two.
      const nx = ax + move.dx * g.w;
      const ny = ay + move.dy * g.h;
      api.paste(move.frame, Math.round(g.x + nx), Math.round(g.y + ny));
    }
  }

  // Clip everything back into the glass with the wall, then round and fuzz the
  // edge with the baked overlay. The wall lights up the moment you take hold of it
  // to walk — so the whole set tells you it's ready to move.
  const swiping = !!grab || !!move;
  const wc = swiping ? GLOW : WALL;
  ink(...wc).box(0, 0, w, g.y);
  ink(...wc).box(0, g.y + g.h, w, h - (g.y + g.h));
  ink(...wc).box(0, 0, g.x, h);
  ink(...wc).box(g.x + g.w, 0, w - (g.x + g.w), h);
  softEdge(api, g, swiping);

  // The only thing on the wall: the room's name, top-left, fading after a moment.
  const c = current();
  const age = now() - shownAt;
  const fade = move ? 1 : Math.min(1, Math.max(0, (NAME_MS - age) / 700));
  if (fade > 0 && c) {
    ink(0, 0, 0, 150 * fade).write(c.name, { x: g.x + 1, y: g.m / 2 - 3 });
    ink(235, 235, 245, 255 * fade).write(c.name, { x: g.x, y: g.m / 2 - 4 });
  }
}

// The soft CRT edge: rounded corners + a fuzzy border. Writing the display buffer
// directly doesn't composite here (AC copies its own buffer after paint), but a
// `painting()` buffer's pixels DO show when pasted — so we bake the edge into a
// transparent overlay ONCE and paste it every frame. Signed distance to a rounded
// rectangle gives corners (the field is round there) and fuzz (we blend over
// FEATHER pixels instead of cutting) in the same number. We bake two: a dark wall
// and a lit one, and paste the lit version while you're taking hold to walk.
let maskWall = null;
let maskGlow = null;
let maskFor = "";
function bakeMask(api, g, w, h, color) {
  const R = Math.max(8, Math.round(Math.min(g.w, g.h) * 0.16)); // corner radius
  const cx = g.x + g.w / 2;
  const cy = g.y + g.h / 2;
  const hw = g.w / 2 - R;
  const hh = g.h / 2 - R;
  const F = FEATHER;
  const [cr, cg, cb] = color;
  const buf = api.painting(w, h, () => {});
  const px = buf.pixels;
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      const qx = Math.abs(x - cx) - hw;
      const qy = Math.abs(y - cy) - hh;
      const d = Math.min(Math.max(qx, qy), 0) + Math.hypot(Math.max(qx, 0), Math.max(qy, 0)) - R;
      let a = (d + F) / (2 * F); // 0 deep inside → 1 at/after the edge
      a = a < 0 ? 0 : a > 1 ? 1 : a * a * (3 - 2 * a); // smoothstep
      const i = (y * w + x) * 4;
      px[i] = cr;
      px[i + 1] = cg;
      px[i + 2] = cb;
      px[i + 3] = Math.round(a * 255); // transparent center, opaque wall
    }
  }
  return buf;
}
function softEdge(api, g, glow) {
  const w = api.screen.width;
  const h = api.screen.height;
  const sig = `${w}x${h}`;
  if (maskFor !== sig) {
    maskWall = bakeMask(api, g, w, h, WALL);
    maskGlow = bakeMask(api, g, w, h, GLOW);
    maskFor = sig;
  }
  api.paste(glow ? maskGlow : maskWall, 0, 0);
}

function onGlass(e, g) {
  const x = (e.x ?? 0) - g.x;
  const y = (e.y ?? 0) - g.y;
  if (x < 0 || y < 0 || x >= g.w || y >= g.h) return null;
  return Object.assign(Object.create(Object.getPrototypeOf(e)), e, { x, y });
}

let grab = null; // a wall drag before it has picked an axis: { sx, sy }

function act(api) {
  const { event: e } = api;
  if (state !== "judging") return;
  const w = api.screen.width;
  const h = api.screen.height;
  const g = glass(w, h);

  if (e.is("touch")) {
    if (move && !move.live) return; // mid-snap, wait
    const on = onGlass(e, g);
    if (on && !move) {
      grab = null; // on the glass → play the pad
      if (host && !hostFailed) {
        taps += 1;
        try {
          if (!host.lisp) host.act?.({ ...api, screen: stage, event: on });
        } catch (err) {
          hostFailed = true;
          error = String(err?.message || err);
        }
      }
    } else if (!on) {
      grab = { sx: e.x, sy: e.y }; // on the wall → begin a walk
    }
    return;
  }

  if (e.is("draw")) {
    // Still picking an axis?
    if (grab && !move) {
      const dx = (e.x ?? grab.sx) - grab.sx;
      const dy = (e.y ?? grab.sy) - grab.sy;
      if (Math.max(Math.abs(dx), Math.abs(dy)) > DEADZONE) {
        // Travel is OPPOSITE the drag — you're pulling the world, like a map.
        // Drag the room left and you move east; the east room comes in from the
        // right, following your finger.
        const horiz = Math.abs(dx) > Math.abs(dy);
        const tdx = horiz ? (dx > 0 ? -1 : 1) : 0;
        const tdy = horiz ? 0 : dy > 0 ? -1 : 1;
        beginMove(api, tdx, tdy); // async, sets `move` shortly
      }
      return;
    }
    // Following the finger along the locked axis.
    if (move && move.live) {
      const raw = move.dx ? (e.x ?? grab.sx) - grab.sx : (e.y ?? grab.sy) - grab.sy;
      const dim = move.dx ? g.w : g.h;
      move.off = Math.max(-dim, Math.min(dim, raw));
      return;
    }
    // Playing the instrument.
    if (!grab && !move && host && !hostFailed && !host.lisp) {
      const on = onGlass(e, g);
      if (on) {
        try {
          host.act?.({ ...api, screen: stage, event: on });
        } catch (err) {
          hostFailed = true;
          error = String(err?.message || err);
        }
      }
    }
    return;
  }

  if (e.is("lift")) {
    if (move && move.live) releaseMove(api, move.dx ? g.w : g.h);
    grab = null;
    return;
  }

  // Keys walk too — instant, no follow. `to = -dx*dim` lands the neighbor centered
  // (its paste offset is `off + dx*dim`, which is 0 when off = -dx*dim).
  const keyWalk = (dx, dy) => {
    if (move) return;
    beginMove(api, dx, dy).then(() => {
      if (move) {
        move.frame = peekFrame(api, move.target, move.cell);
        move.live = false;
        move.from = 0;
        move.to = dx ? -dx * g.w : -dy * g.h;
        move.commit = true;
        move.anim = 0.0001;
      }
    });
  };
  if (e.is("keyboard:down:arrowright")) return keyWalk(1, 0);
  if (e.is("keyboard:down:arrowleft")) return keyWalk(-1, 0);
  if (e.is("keyboard:down:arrowdown")) return keyWalk(0, 1);
  if (e.is("keyboard:down:arrowup")) return keyWalk(0, -1);
}

function leave(api) {
  if (state === "judging") leaving(api);
}

export { boot, sim, paint, act, leave };
