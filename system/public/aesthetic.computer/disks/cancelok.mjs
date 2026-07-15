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

// The audio leak that killed the sound after many walks: a pad's SUSTAINED voices
// (padChord's `duration: "🔁"`, overtone's Infinity) never stop on their own, and
// swapping the hosted pad never told them to. Room after room, they piled into the
// synth's voice pool until it was full and nothing new could sound.
//
// So we hand each pad a `sound` whose `synth` logs the sustained voices it starts,
// and we kill exactly those when we leave the room. Transient beat-voices decay by
// themselves and are left alone, so the log stays tiny.
let padSound = null;
let voiceLog = [];
const sustained = (d) => d === "🔁" || d === Infinity || d === "infinity";
function wrapSound(sound) {
  if (!sound || typeof sound.synth !== "function") return sound;
  const realSynth = sound.synth.bind(sound);
  const wrapped = Object.create(sound); // inherits speaker, bpm, room, …
  wrapped.synth = (opts = {}) => {
    const h = realSynth(opts);
    if (h && sustained(opts.duration)) {
      voiceLog.push(h);
      if (voiceLog.length > 48) voiceLog.shift()?.kill?.(0.1);
    }
    return h;
  };
  return wrapped;
}
function killVoices() {
  for (const h of voiceLog) {
    try {
      h?.kill?.(0.06);
    } catch {}
  }
  voiceLog = [];
}

// A heartbeat for the stress harness. A piece runs in a worker, so it can't hang
// a handle off the page's window — but its console reaches the page. Once a second
// or so we print the counters that would reveal a leak, and the harness reads them.
let statTick = 0;
function stat() {
  if (++statTick % 90) return;
  try {
    console.log(`[ce] v${voiceLog.length} r${world.size} f${frozen.size} m${mods.size} t${trail.length}`);
  } catch {}
}
// The api a LIVE pad runs against: its own surface + the voice-logging sound.
function padApi(api, surf, event) {
  if (!padSound) padSound = wrapSound(api.sound);
  const a = { ...api, screen: surf, sound: padSound };
  if (event) a.event = event;
  return a;
}
// A dead-silent sound, for the throwaway boots a peek does — a preview must not
// make noise, and must not leak a voice.
const SILENT = {
  synth: () => ({ kill() {}, progress: async () => ({ progress: 0 }) }),
  bpm: () => {},
  speaker: { poll() {}, amplitudes: {}, frequencies: {}, beat: {} },
  room: { set() {}, on() {}, off() {}, toggle() {} },
};
const peekApi = (api, surf) => ({ ...api, screen: surf, sound: SILENT });

async function mount(api) {
  killVoices(); // silence the room we're leaving before the next one speaks
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
  // Announce the room the way AC announces a saved handle — a frontal notice,
  // front and center, instead of a label pinned to the wall.
  api.notice?.(c.name, ["white", "blue"]);
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
      mod.boot?.(padApi(api, surf));
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

// Boot a room silently into a fresh painting — a preview must not sound, and must
// not leak a voice. Does NOT put the current room back; the caller does that once,
// after all the shots, so a diagonal (three neighbors) reboots the current room a
// single time instead of three.
function shootCell(api, cell) {
  const g = glass(api.screen.width, api.screen.height);
  const shot = api.painting(g.w, g.h, (p) => p.wipe(...WALL));
  if (cell && !cell.lisp) {
    try {
      const mod = mods.get(cell.code);
      if (mod) {
        api.page(shot);
        mod.boot?.(peekApi(api, shot));
        mod.sim?.(peekApi(api, shot));
        mod.paint?.(peekApi(api, shot));
        api.page(api.screen);
      }
    } catch {}
  }
  return shot;
}
function restoreCurrent(api) {
  if (host && !host.lisp && stage) {
    try {
      api.page(stage);
      host.boot?.(peekApi(api, stage)); // re-asserts its config; live voices untouched
      api.page(api.screen);
    } catch {}
  }
}
// Fill in any missing neighbor photographs — from the frozen cache if we've been
// there, otherwise by booting once. The current room is rebooted only if we had to
// boot at all, and only once.
function grabFrames(api, cells) {
  let booted = false;
  for (const c of cells) {
    if (c.frame) continue;
    if (frozen.has(c.key)) {
      c.frame = frozen.get(c.key).frame;
      continue;
    }
    c.frame = shootCell(api, c.cell);
    booted = true;
  }
  if (booted) restoreCurrent(api);
}

// Begin a walk. `dx,dy` is the travel step: a single axis for a straight walk, or
// BOTH for a diagonal into a corner. The reveal shows every room the walk exposes —
// the destination plus, on a diagonal, the two orthogonal neighbors that fill the
// edges as the corner opens. All of them are dealt into the world now, so the map
// stays consistent no matter how you later approach them.
async function beginMove(api, dx, dy) {
  const rels = [];
  if (dx) rels.push([dx, 0]);
  if (dy) rels.push([0, dy]);
  if (dx && dy) rels.push([dx, dy]); // the corner room — the destination
  const cells = [];
  for (const [rdx, rdy] of rels) {
    const k = key({ x: pos.x + rdx, y: pos.y + rdy });
    if (!world.has(k)) world.set(k, deck[dealt++ % deck.length]);
    const cell = world.get(k);
    if (cell && !cell.lisp && !mods.get(cell.code)) {
      try {
        mods.set(cell.code, await import(`${ORIGIN}/aesthetic.computer/disks/${cell.code}.mjs`));
      } catch {}
    }
    cells.push({ rdx, rdy, key: k, cell, frame: null });
  }
  move = { dx, dy, ox: 0, oy: 0, live: true, anim: 0, fromx: 0, fromy: 0, tox: 0, toy: 0, commit: false, cells };
}

// You let go. If you pulled past the doorway on either axis, finish the slide and
// step through; otherwise it eases back and you never left.
function releaseMove(api, g) {
  if (!move) return;
  const px = move.dx ? Math.abs(move.ox) / g.w : 0;
  const py = move.dy ? Math.abs(move.oy) / g.h : 0;
  const past = Math.max(px, py) > 0.22;
  move.live = false;
  move.fromx = move.ox;
  move.fromy = move.oy;
  move.tox = past ? -move.dx * g.w : 0;
  move.toy = past ? -move.dy * g.h : 0;
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
    frozen.delete(key(pos)); // move to newest (re-insert), so eviction is LRU-ish
    frozen.set(key(pos), { frame: keep, name: current().name });
    // A photograph is ~300KB. Keep only the rooms you might soon walk back to —
    // an evicted one just re-peeks (boots) when you return. Bounds the memory a
    // long wander would otherwise pile up, one frame per room forever.
    while (frozen.size > 32) frozen.delete(frozen.keys().next().value);
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
      host.sim?.(padApi(api, stage));
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    }
  }
  // Ease a released move home — both axes at once, so a diagonal lands true.
  if (move && !move.live) {
    move.anim += 0.09;
    const e = EASE(Math.min(1, move.anim));
    move.ox = move.fromx + (move.tox - move.fromx) * e;
    move.oy = move.fromy + (move.toy - move.fromy) * e;
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
  stat();

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
  if (host && !hostFailed) {
    try {
      api.page(stage);
      if (host.lisp) api.kidlisp(0, 0, g.w, g.h, lisp);
      else host.paint?.(padApi(api, stage));
    } catch (e) {
      hostFailed = true;
      error = String(e?.message || e);
    } finally {
      api.page(screen);
    }
  }

  wipe(...WALL);

  // Where the rooms sit. The live one slides by (ox,oy); each revealed neighbor
  // rides one cell away in its own direction, so they pan together like a window
  // sliding over a grid. On a diagonal that's four rooms at once — current, the two
  // edges, and the corner you're heading for.
  const ax = move ? move.ox : 0;
  const ay = move ? move.oy : 0;

  if (hostFailed && !move) {
    ink(18, 6, 10).box(g.x, g.y, g.w, g.h);
    ink(...NO).write("didn't run", { x: g.x + g.w / 2 - 30, y: g.y + g.h / 2 - 8 });
  } else {
    api.paste(stage, Math.round(g.x + ax), Math.round(g.y + ay));
    if (move) {
      grabFrames(api, move.cells);
      for (const c of move.cells) {
        if (!c.frame) continue;
        api.paste(c.frame, Math.round(g.x + ax + c.rdx * g.w), Math.round(g.y + ay + c.rdy * g.h));
      }
    }
  }

  // Clip everything back into the glass with the wall, then round and fuzz the
  // edge. The wall lights up (GLOW) the moment you take hold to walk; otherwise it
  // takes a tone chosen to CONTRAST the pad's edge, so the touchable area is always
  // visible — a dark pad never hides a dark wall.
  sampleEdge();
  const swiping = !!grab || !!move;
  const wc = swiping ? GLOW : wallTone();
  ink(...wc).box(0, 0, w, g.y);
  ink(...wc).box(0, g.y + g.h, w, h - (g.y + g.h));
  ink(...wc).box(0, 0, g.x, h);
  ink(...wc).box(g.x + g.w, 0, w - (g.x + g.w), h);
  softEdge(api, g, wc);

  // The name isn't drawn here anymore — it arrives as AC's own frontal notice each
  // time you enter a room (see mount), the same alert that announces a saved handle.
}

// The wall must never vanish into the pad — the touchable area has to stay
// visible, so we pick its tone to CONTRAST whatever the pad is doing at its edge.
// A dark pad gets a light wall; a bright pad keeps the deep dark one. Sampled from
// the pad's own border, a few times a second.
let padLum = 40;
let lumTick = 0;
function sampleEdge() {
  if (!stage?.pixels || ++lumTick % 12) return;
  const px = stage.pixels;
  const W = stage.width;
  const H = stage.height;
  let sum = 0;
  let n = 0;
  const at = (x, y) => {
    const i = (y * W + x) * 4;
    sum += 0.299 * px[i] + 0.587 * px[i + 1] + 0.114 * px[i + 2];
    n++;
  };
  const step = Math.max(4, Math.floor(W / 20));
  for (let x = 0; x < W; x += step) {
    at(x, 2);
    at(x, H - 3);
  }
  for (let y = 0; y < H; y += step) {
    at(2, y);
    at(W - 3, y);
  }
  if (n) padLum += (sum / n - padLum) * 0.4; // ease, so the wall doesn't strobe
}
// The resting wall tone: light slate over a dark pad, deep dark over a bright one —
// either way, a clear step away from the pad's edge.
const wallTone = () => (padLum < 82 ? [66, 72, 88] : [10, 10, 14]);

// The soft CRT edge: rounded corners + a fuzzy border. Writing the display buffer
// directly doesn't composite here (AC copies its own buffer after paint), but a
// `painting()` buffer's pixels DO show when pasted — so we bake the edge into a
// transparent overlay and paste it every frame. Signed distance to a rounded
// rectangle gives corners (the field is round there) and fuzz (we blend over
// FEATHER pixels instead of cutting) in one number. The overlay is baked per wall
// colour and cached, so switching tones is free after the first time.
const maskCache = new Map();
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
function softEdge(api, g, color) {
  const w = api.screen.width;
  const h = api.screen.height;
  const k = `${w}x${h}:${color.join(",")}`;
  let mask = maskCache.get(k);
  if (!mask) {
    mask = bakeMask(api, g, w, h, color);
    maskCache.set(k, mask);
    if (maskCache.size > 8) maskCache.delete(maskCache.keys().next().value); // bound it
  }
  api.paste(mask, 0, 0);
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
          if (!host.lisp) host.act?.(padApi(api, stage, on));
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
    // Still picking a direction?
    if (grab && !grab.started && !move) {
      const dx = (e.x ?? grab.sx) - grab.sx;
      const dy = (e.y ?? grab.sy) - grab.sy;
      const ax = Math.abs(dx);
      const ay = Math.abs(dy);
      if (Math.max(ax, ay) > DEADZONE) {
        // Travel is OPPOSITE the drag — you're pulling the world, like a map. If
        // both axes are meaningfully engaged (within a ~2:1 cone) it's a DIAGONAL,
        // into a corner; otherwise it locks to the dominant axis.
        grab.started = true; // beginMove is async — don't fire it twice
        const diag = Math.min(ax, ay) > Math.max(ax, ay) * 0.5;
        const tdx = diag || ax >= ay ? (dx > 0 ? -1 : 1) : 0;
        const tdy = diag || ay > ax ? (dy > 0 ? -1 : 1) : 0;
        beginMove(api, tdx, tdy);
      }
      return;
    }
    // Following the finger, on whichever axes this move locked.
    if (move && move.live) {
      if (move.dx) move.ox = Math.max(-g.w, Math.min(g.w, (e.x ?? grab.sx) - grab.sx));
      if (move.dy) move.oy = Math.max(-g.h, Math.min(g.h, (e.y ?? grab.sy) - grab.sy));
      return;
    }
    // Playing the instrument.
    if (!grab && !move && host && !hostFailed && !host.lisp) {
      const on = onGlass(e, g);
      if (on) {
        try {
          host.act?.(padApi(api, stage, on));
        } catch (err) {
          hostFailed = true;
          error = String(err?.message || err);
        }
      }
    }
    return;
  }

  if (e.is("lift")) {
    if (move && move.live) releaseMove(api, g);
    grab = null;
    return;
  }

  // Keys walk too — instant, no follow. The destination lands centered when the
  // window slides to (-dx*w, -dy*h).
  const keyWalk = (dx, dy) => {
    if (move) return;
    beginMove(api, dx, dy).then(() => {
      if (!move) return;
      grabFrames(api, move.cells);
      move.live = false;
      move.fromx = 0;
      move.fromy = 0;
      move.tox = -dx * g.w;
      move.toy = -dy * g.h;
      move.commit = true;
      move.anim = 0.0001;
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
