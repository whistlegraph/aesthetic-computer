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
const WALL = [26, 28, 38]; // the dark the set is sunk into — never pure black
const RIM = [150, 158, 180]; // the lit edge line around the glass, always present

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

// The screen-within-the-screen. A deep margin of wall on every side.
function glass(w, h) {
  const m = Math.max(24, Math.round(Math.min(w, h) * 0.12));
  return { x: m, y: m, w: w - 2 * m, h: h - 2 * m, m };
}

// Every pad gets its own signature colour, derived from its name so it's stable —
// a pad is always "the amber one." The frame around a room is that colour, so as
// you wander the whole wall shifts room to room and you learn the map by hue. Dark
// and rich, so it reads as a coloured bezel and the pad still pops, never black.
function hslToRgb(h, s, l) {
  const c = (1 - Math.abs(2 * l - 1)) * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = l - c / 2;
  let r, g, b;
  if (h < 60) [r, g, b] = [c, x, 0];
  else if (h < 120) [r, g, b] = [x, c, 0];
  else if (h < 180) [r, g, b] = [0, c, x];
  else if (h < 240) [r, g, b] = [0, x, c];
  else if (h < 300) [r, g, b] = [x, 0, c];
  else [r, g, b] = [c, 0, x];
  return [Math.round((r + m) * 255), Math.round((g + m) * 255), Math.round((b + m) * 255)];
}
function padColor(code, bright) {
  let h = 0;
  for (let i = 0; i < (code || "").length; i++) h = (h * 31 + code.charCodeAt(i)) >>> 0;
  return hslToRgb(h % 360, 0.52, bright ? 0.34 : 0.24);
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
    stage = api.painting(g.w, g.h, (p) => p.wipe(...WALL)); // never pure black
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
  move = { dx, dy, ox: 0, oy: 0, vx: 0, vy: 0, lastOx: 0, lastOy: 0, live: true, tox: 0, toy: 0, commit: false, cells };
}

// You let go. If you pulled past the doorway on either axis, finish the slide and
// step through; otherwise it eases back and you never left.
function releaseMove(api, g) {
  if (!move) return;
  const pitchX = g.w + 2 * g.m;
  const pitchY = g.h + 2 * g.m;
  const fx = move.dx ? Math.abs(move.ox) / pitchX : 0;
  const fy = move.dy ? Math.abs(move.oy) / pitchY : 0;
  // Commit if you pulled far enough OR flicked — a quick throw carries through even
  // when you didn't drag all the way. This is what makes a flick feel like a flick.
  const flick =
    (move.dx && Math.sign(move.vx) === Math.sign(-move.dx) && Math.abs(move.vx) > 4) ||
    (move.dy && Math.sign(move.vy) === Math.sign(-move.dy) && Math.abs(move.vy) > 4);
  const past = Math.max(fx, fy) > 0.18 || flick;
  move.live = false;
  move.tox = past ? -move.dx * pitchX : 0;
  move.toy = past ? -move.dy * pitchY : 0;
  move.commit = past;
  // vx/vy are carried from the drag — the spring in sim continues that motion
  // instead of restarting, so there's no lurch on release.
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
  if (pressT > 0) pressT -= 0.06; // the tapped label's flash fades

  // While you're dragging, remember how fast the room is moving — that velocity is
  // what a natural release should carry.
  if (move && move.live) {
    move.vx = move.ox - move.lastOx;
    move.vy = move.oy - move.lastOy;
    move.lastOx = move.ox;
    move.lastOy = move.oy;
  }

  // After you let go, a spring pulls the room home — it keeps your finger's
  // momentum, overshoots a hair on a fast throw, and eases in soft on a slow one.
  // Organic, not a fixed-length slide that lurches from wherever you released.
  if (move && !move.live) {
    // A gentle, decelerating glide — you WANDER toward the room and ARRIVE, rather
    // than snapping. Soft stiffness for the long travel, firm damping so it eases
    // in and lands without a bounce.
    const K = 0.09;
    const D = 0.62;
    move.vx = (move.vx + (move.tox - move.ox) * K) * D;
    move.vy = (move.vy + (move.toy - move.oy) * K) * D;
    move.ox += move.vx;
    move.oy += move.vy;
    const settled =
      Math.abs(move.tox - move.ox) < 0.6 &&
      Math.abs(move.toy - move.oy) < 0.6 &&
      Math.hypot(move.vx, move.vy) < 0.6;
    if (settled) {
      move.ox = move.tox;
      move.oy = move.toy;
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

  // The background IS the compass: each of the eight margin zones is dyed its
  // direction's colour, so the wall around you is a colour-coded D-pad — north
  // reads red up top, east yellow to the right, and so on, always in the same
  // place. Dark, so the pad still pops; never pure black.
  const swiping = !!grab || !!move;
  wipe(...WALL);
  for (const d of DIRS) {
    let [zx, zy, zw, zh] = zoneRect(d, g, w, h);
    const gap = 3; // soft neutral seam between tiles, so each reads as its own shape
    zx += gap;
    zy += gap;
    zw -= gap * 2;
    zh -= gap * 2;
    // The WHOLE tile lights, not just its label: dim at rest, brighter while you
    // pull, brightest under the pointer, and a flash the moment it's taken.
    const on = hover === d;
    const lit = pressed === d ? pressT : 0;
    const f = 0.34 + (swiping ? 0.1 : 0) + (on ? 0.36 : 0) + lit * 0.4;
    const col = [Math.min(255, d.col[0] * f), Math.min(255, d.col[1] * f), Math.min(255, d.col[2] * f)];
    // A rounded tile — corners and edges alike get real shape, so nothing butts
    // squarely against the round glass and feels off.
    tile(ink, zx, zy, zw, zh, Math.min(12, zw / 2, zh / 2), col);
  }

  // The web. Every room is its own framed puddle, carrying its frame WITH it — so a
  // drag pulls the whole web across the grid. The frame is neutral so the coloured
  // compass behind it stays the star.
  const ax = move ? move.ox : 0;
  const ay = move ? move.oy : 0;
  const px = g.w + 2 * g.m; // one cell across
  const py = g.h + 2 * g.m;
  const frame = frameFor(api, g, WALL);

  const rooms = [{ rdx: 0, rdy: 0, content: hostFailed ? null : stage }];
  if (move) {
    grabFrames(api, move.cells);
    for (const c of move.cells) rooms.push({ rdx: c.rdx, rdy: c.rdy, content: c.frame });
  }
  for (const r of rooms) {
    const sx = Math.round(g.x + ax + r.rdx * px);
    const sy = Math.round(g.y + ay + r.rdy * py);
    if (r.content) api.paste(r.content, sx, sy);
    else {
      ink(...WALL).box(sx, sy, g.w, g.h);
      ink(...NO).write("didn't run", { x: sx + g.w / 2 - 30, y: sy + g.h / 2 - 8 });
    }
    api.paste(frame, sx, sy); // round the corners, feather the edge, draw the rim
  }

  // The 8-way control surface, on the wall: each direction's key + arrow, lit when
  // you hover or take it. Hidden mid-pull — you're already navigating then.
  if (!move) controls(api, g, w, h);

  // The name isn't drawn here anymore — it arrives as AC's own frontal notice each
  // time you enter a room (see mount), the same alert that announces a saved handle.
}

// A filled rounded rectangle from primitives — a cross of two boxes plus a disc at
// each corner. Gives the direction tiles real shape so nothing butts squarely
// against the round glass.
function tile(ink, x, y, wd, ht, r, col) {
  r = Math.max(0, Math.min(r, wd / 2, ht / 2));
  ink(...col).box(x + r, y, wd - 2 * r, ht);
  ink(...col).box(x, y + r, wd, ht - 2 * r);
  ink(...col).circle(x + r, y + r, r, true);
  ink(...col).circle(x + wd - r, y + r, r, true);
  ink(...col).circle(x + r, y + ht - r, r, true);
  ink(...col).circle(x + wd - r, y + ht - r, r, true);
}

// A little arrow, drawn (not typed — the font has no arrow glyphs). A shaft from
// the zone center outward, and a two-line head at the tip.
function arrow(ink, cx, cy, dx, dy, s, col, a) {
  const L = Math.hypot(dx, dy) || 1;
  const nx = dx / L;
  const ny = dy / L;
  const tx = cx + nx * s;
  const ty = cy + ny * s;
  ink(...col, a).line(cx - nx * s, cy - ny * s, tx, ty);
  const hx = nx * 4;
  const hy = ny * 4;
  const pxp = -ny * 3;
  const pyp = nx * 3;
  ink(...col, a).line(tx, ty, tx - hx + pxp, ty - hy + pyp);
  ink(...col, a).line(tx, ty, tx - hx - pxp, ty - hy - pyp);
}

// The wall's D-pad. Eight small labels — an arrow and the QWERTY key — one per
// border zone. Brightens under the pointer (hover) and flashes when taken.
function controls({ ink, write }, g, w, h) {
  for (const d of DIRS) {
    const [cx, cy] = zoneCenter(d, g, w, h);
    const on = hover === d;
    const lit = pressed === d ? pressT : 0;
    const a = Math.round(150 + on * 90 + lit * 90);
    // The direction's own colour, brightened when hovered or just taken.
    const b = on || lit ? 55 : 0;
    const col = [Math.min(255, d.col[0] + b), Math.min(255, d.col[1] + b), Math.min(255, d.col[2] + b)];
    arrow(ink, cx, cy - 3, d.dx, d.dy, 6, col, a);
    ink(0, 0, 0, a * 0.5).write(d.k.toUpperCase(), { x: cx - 2, y: cy + 4 });
    ink(...col, a).write(d.k.toUpperCase(), { x: cx - 3, y: cy + 3 });
  }
}

// The soft CRT edge: rounded corners + a fuzzy border. Writing the display buffer
// directly doesn't composite here (AC copies its own buffer after paint), but a
// `painting()` buffer's pixels DO show when pasted — so we bake the edge into a
// transparent overlay and paste it every frame. Signed distance to a rounded
// rectangle gives corners (the field is round there) and fuzz (we blend over
// FEATHER pixels instead of cutting) in one number. The overlay is baked per wall
// colour and cached, so switching tones is free after the first time.
// A single puddle's frame — GLASS-SIZED, so it rides WITH its room instead of
// sitting fixed over the whole screen. That's what turns the set from one window
// into a web: every room carries its own rounded frame, and dragging pulls the
// whole web of them across the grid. Transparent in the middle (the pad shows),
// rounded + feathered at the edge (a soft CRT border), wall in the corners, and a
// lit RIM line hugging the boundary so the sides are always drawn. Baked per wall
// colour and cached.
const maskCache = new Map();
function bakeFrame(api, g, color) {
  const gw = g.w;
  const gh = g.h;
  const R = Math.max(8, Math.round(Math.min(gw, gh) * 0.16)); // corner radius
  const cx = gw / 2;
  const cy = gh / 2;
  const hw = gw / 2 - R;
  const hh = gh / 2 - R;
  const F = FEATHER;
  const [cr, cg, cb] = color;
  const buf = api.painting(gw, gh, () => {});
  const px = buf.pixels;
  for (let y = 0; y < gh; y++) {
    for (let x = 0; x < gw; x++) {
      const qx = Math.abs(x - cx) - hw;
      const qy = Math.abs(y - cy) - hh;
      const d = Math.min(Math.max(qx, qy), 0) + Math.hypot(Math.max(qx, 0), Math.max(qy, 0)) - R;
      let a = (d + F) / (2 * F); // 0 deep inside → 1 at/after the edge
      a = a < 0 ? 0 : a > 1 ? 1 : a * a * (3 - 2 * a); // smoothstep to wall
      let rr = cr;
      let rg = cg;
      let rb = cb;
      let A = a;
      // The lit edge line — a bright band a couple pixels inside the boundary, so
      // there's always a visible line on every side, whatever the pad is doing.
      const rim = Math.max(0, 1 - Math.abs(d + 2) / 2);
      if (rim > 0) {
        rr = rr * (1 - rim) + RIM[0] * rim;
        rg = rg * (1 - rim) + RIM[1] * rim;
        rb = rb * (1 - rim) + RIM[2] * rim;
        A = Math.max(A, rim);
      }
      const i = (y * gw + x) * 4;
      px[i] = rr;
      px[i + 1] = rg;
      px[i + 2] = rb;
      px[i + 3] = Math.round(A * 255);
    }
  }
  return buf;
}
function frameFor(api, g, color) {
  const k = `${g.w}x${g.h}:${color.join(",")}`;
  let f = maskCache.get(k);
  if (!f) {
    f = bakeFrame(api, g, color);
    maskCache.set(k, f);
    if (maskCache.size > 20) maskCache.delete(maskCache.keys().next().value);
  }
  return f;
}

function onGlass(e, g) {
  const x = (e.x ?? 0) - g.x;
  const y = (e.y ?? 0) - g.y;
  if (x < 0 || y < 0 || x >= g.w || y >= g.h) return null;
  return Object.assign(Object.create(Object.getPrototypeOf(e)), e, { x, y });
}

let grab = null; // a wall drag before it has picked an axis: { sx, sy }
let hover = null; // the direction zone under the pointer, for the hover glow
let pressed = null; // the direction just taken, briefly lit as feedback
let pressT = 0;

// The eight ways out, as a 9-patch on the QWERTY keys — the cluster around S, so
// your hand never leaves home row. Each also names the wall zone (which third of
// the border) you tap to go that way, and the arrow that points there.
// Each direction wears a colour, a compass wheel — the way notepat gives every
// note its own. Warm at the top, cooling clockwise around, so the eight ways out
// read as a spectrum you can learn by hue.
const DIRS = [
  { k: "w", dx: 0, dy: -1, zx: 0, zy: -1, col: [255, 50, 50] }, //   ↑ N   red
  { k: "e", dx: 1, dy: -1, zx: 1, zy: -1, col: [255, 150, 0] }, //   ↗ NE  orange
  { k: "d", dx: 1, dy: 0, zx: 1, zy: 0, col: [235, 220, 0] }, //     → E   yellow
  { k: "c", dx: 1, dy: 1, zx: 1, zy: 1, col: [60, 210, 70] }, //     ↘ SE  green
  { k: "x", dx: 0, dy: 1, zx: 0, zy: 1, col: [0, 200, 210] }, //     ↓ S   cyan
  { k: "z", dx: -1, dy: 1, zx: -1, zy: 1, col: [60, 120, 255] }, //  ↙ SW  blue
  { k: "a", dx: -1, dy: 0, zx: -1, zy: 0, col: [150, 70, 235] }, //  ← W   purple
  { k: "q", dx: -1, dy: -1, zx: -1, zy: -1, col: [230, 80, 235] }, // ↖ NW magenta
];
const zoneAt = (x, y, g) => {
  const zx = x < g.x ? -1 : x >= g.x + g.w ? 1 : 0;
  const zy = y < g.y ? -1 : y >= g.y + g.h ? 1 : 0;
  if (!zx && !zy) return null; // on the glass — that's the pad's
  return DIRS.find((d) => d.zx === zx && d.zy === zy);
};
const zoneCenter = (d, g, w, h) => [
  d.zx < 0 ? g.x / 2 : d.zx > 0 ? (g.x + g.w + w) / 2 : g.x + g.w / 2,
  d.zy < 0 ? g.y / 2 : d.zy > 0 ? (g.y + g.h + h) / 2 : g.y + g.h / 2,
];
// The wall rectangle for a zone — the third of the border on that side/corner.
const zoneRect = (d, g, w, h) => {
  const x0 = d.zx < 0 ? 0 : d.zx > 0 ? g.x + g.w : g.x;
  const x1 = d.zx < 0 ? g.x : d.zx > 0 ? w : g.x + g.w;
  const y0 = d.zy < 0 ? 0 : d.zy > 0 ? g.y + g.h : g.y;
  const y1 = d.zy < 0 ? g.y : d.zy > 0 ? h : g.y + g.h;
  return [x0, y0, x1 - x0, y1 - y0];
};
// A direction's colour dimmed to a background bezel — rich but not blinding, lit a
// touch brighter while you're pulling.
const bezelTone = (c, bright) => {
  const f = bright ? 0.5 : 0.36;
  return [Math.round(c[0] * f), Math.round(c[1] * f), Math.round(c[2] * f)];
};

function act(api) {
  const { event: e } = api;
  if (state !== "judging") return;
  const w = api.screen.width;
  const h = api.screen.height;
  const g = glass(w, h);

  // Hover: the pointer moving over a wall zone lights that direction's label.
  if (e.is("move")) {
    hover = grab || move ? null : zoneAt(e.x ?? -1, e.y ?? -1, g);
    return;
  }

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
      const pitchX = g.w + 2 * g.m;
      const pitchY = g.h + 2 * g.m;
      if (move.dx) move.ox = Math.max(-pitchX, Math.min(pitchX, (e.x ?? grab.sx) - grab.sx));
      if (move.dy) move.oy = Math.max(-pitchY, Math.min(pitchY, (e.y ?? grab.sy) - grab.sy));
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
    if (move && move.live) {
      releaseMove(api, g);
    } else if (grab && !grab.started) {
      // A tap on the wall that never became a drag: walk the zone you tapped.
      const d = zoneAt(grab.sx, grab.sy, g);
      if (d) tap(api, d, g);
    }
    grab = null;
    return;
  }

  // The QWERTY 9-patch and the arrow keys — same eight walks.
  for (const d of DIRS) {
    if (e.is(`keyboard:down:${d.k}`)) return tap(api, d, g);
  }
  const dir = (dx, dy) => DIRS.find((d) => d.dx === dx && d.dy === dy);
  if (e.is("keyboard:down:arrowright")) return tap(api, dir(1, 0), g);
  if (e.is("keyboard:down:arrowleft")) return tap(api, dir(-1, 0), g);
  if (e.is("keyboard:down:arrowdown")) return tap(api, dir(0, 1), g);
  if (e.is("keyboard:down:arrowup")) return tap(api, dir(0, -1), g);
}

// Take a direction by tap or key — instant, no finger-follow. The spring animates
// it from rest, so it eases in organically, and the label flashes as feedback.
function tap(api, d, g) {
  if (move) return;
  pressed = d;
  pressT = 1;
  beginMove(api, d.dx, d.dy).then(() => {
    if (!move) return;
    grabFrames(api, move.cells);
    move.live = false;
    move.tox = -d.dx * (g.w + 2 * g.m);
    move.toy = -d.dy * (g.h + 2 * g.m);
    move.commit = true;
  });
}

function leave(api) {
  if (state === "judging") leaving(api);
}

export { boot, sim, paint, act, leave };
