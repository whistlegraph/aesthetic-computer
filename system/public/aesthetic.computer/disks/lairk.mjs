// lairk, 2026.9.23
// The Laer Klokken chat as a place: everyone who speaks there stands
// somewhere around the clock tower.

/* 📝 Notes
  lairk shares the `clock` chat with `laklok` (DB `chat-clock`), so the two
  are one conversation — lairk only adds bodies and a place to put them.

  Everything is drawn in the raster stack: the world with CPU `form`s and a
  hand-built camera matrix (so name tags and speech bubbles can be projected
  with the exact math the renderer uses), the chat with `chat.mjs` in its
  `embedded` mode over the top.

  A spot is earned: only a handle that has spoken in Laer Klokken AND been
  @mentioned there by someone else stands in lairk. The server reads the
  whole of `chat-clock` for that (/api/lairk-roster); this piece only draws
  the answer. Each handle stands where it last stood (or a default spot
  around the tower), lit when online and dimmed when not.

  Walking: the session server (session-server/lairk-manager.mjs) lets a
  verified handle on that roster move, keeps it on the ground and at a run,
  remembers where every handle stood, and relays positions to everyone
  watching. Your own body moves the frame you press; everyone else glides to
  where the server last saw them — never guessed ahead.
 */

/* #region 🏁 TODO
  + Done
  - [x] Walking — position relay ~10 Hz, remotes glide.
  - [x] Server-side move gate (spoke + mentioned by someone else).
  - [x] Remember each handle's position within lairk.
#endregion */

import { Chat } from "../lib/chat.mjs";
import * as chat from "./chat.mjs";
import {
  LAK_THEMES,
  pickTema,
  restoreTema,
  realtimeTick,
  lakChatOptions,
} from "./common/laklok-tema.mjs";

const { sin, cos, tan, sqrt, min, max, floor, PI } = Math;

let client; // The `clock` chat connection (the same room as laklok).
let lakTheme = "ler"; // The visitor's laklok tema, so the chat wears it here too.
let get; // `$.get`, kept for loading paintings as handles appear.

// 🎥 Orbit camera around the tower.
const FOV = 60;
const NEAR = 0.1;
const FAR = 400;
const focus = [0, 2.2, 0]; // What the camera orbits: the tower, or you.
let orbitAngle = 0.6; // radians around the tower
let orbitHeight = 6; // eye height
let orbitRadius = 17;
let lastLookAt = -Infinity; // ms of the last manual orbit; pauses the drift
let camMatrix = null; // P·V — projects world points for labels
const cam = { matrix: null, resize() {} }; // What `form` reads.

// 🌍 World forms.
let ground, tower, faces;
let hands = null; // Rebuilt when the minute changes.
let handsMinute = -1;

// 🧍 One character per handle:
//   { handle, x, z, online, colors, texture, base, torso, dirty }
// `colors` are the handle's own per-letter colors (null until set), `texture`
// their most recent painting ({ lit, dim }), and `dirty` asks paint to
// rebuild the forms when either lands or the handle comes online.
const characters = new Map();
let rosterKey = ""; // Rebuilt when the handles in view change.
let eligible = null; // Set of handles with a spot, from /api/lairk-roster.
let eligibleAt = 0; // When it was last asked for, ms.
const ROSTER_MS = 120_000; // The server caches it for two minutes too.
const lookQueue = []; // Characters waiting for their colors and painting.
let looking = 0; // Look-ups in flight.
const LOOKS_AT_ONCE = 4;
const rosterColors = new Map(); // handle -> per-letter colors from @handles.

// 🚶 Walking.
let server = null; // The session socket, for lairk:* messages.
let walker = null; // Your handle, once the server lets you walk.
let walkNo = null; // Why not ("login", "mention", "unavailable").
const placed = new Map(); // handle -> { x, z, facing } the server remembers.
const held = new Set(); // Walk keys held down.
let walkTo = null; // { x, z } — where a tap asked to go.
let tap = null; // { travel } — a touch that hasn't become an orbit drag yet.
let lastSent = 0; // When a move last went out, ms.
let unsent = false; // Moved since then.
let lastSim = 0; // For a real-time step.
const WALK_SPEED = 5; // Units per second; the server allows up to 9.
const SEND_MS = 100; // ~10 Hz while moving.
const WALK_KEYS = {
  w: "forward", arrowup: "forward",
  s: "back", arrowdown: "back",
  a: "left", arrowleft: "left",
  d: "right", arrowright: "right",
};

// 💬 Speech bubbles: handle -> { text, until }.
const bubbles = new Map();
const BUBBLE_MS = 7000;
const seen = new WeakSet(); // Messages already accounted for.
let historySettled = false; // History arrives in a bulk; don't bubble it.

// 🪟 Chat overlay state.
let chatOpen = false;
let typing = false; // Tracks keyboard:open / keyboard:close.
let readoutBox = null; // Tap target for the top readout.
let closeBox = null; // Tap target for "world" while the chat is open.
let notice = null; // { text, until } — the walking gate courtesy note.

function boot({ api, Form, debug, send, hud, store, colon, params, get: getter, net, handle, authorize }) {
  get = getter;
  const tema = pickTema([...(colon || []), ...(params || [])], store);
  lakTheme = tema.name;
  restoreTema(store, tema.pinned, (saved) => {
    if (saved) {
      lakTheme = saved;
      chat.refresh(client.system);
    }
  });

  client = new Chat(debug, send);
  client.connect("clock"); // Same room as laklok. (DB stays `chat-clock`.)
  chat.boot(api, client.system);
  hud.qr(null); // chat.boot stamps a prompt.ac/chat QR; lairk doesn't want it.
  hud.label("lairk");

  chatOpen = false;
  typing = false;
  historySettled = false;
  characters.clear();
  bubbles.clear();
  rosterKey = "";
  eligible = null;
  eligibleAt = 0;
  lookQueue.length = 0;
  rosterColors.clear();
  walker = null;
  walkNo = null;
  placed.clear();
  held.clear();
  walkTo = null;
  focus[0] = 0;
  focus[2] = 0;

  // 🚶 Watch everyone's positions; ask to walk when signed in. The server
  // checks the token and the roster — this piece never claims a handle.
  server = net?.socket?.((_id, type, content) => {
    if (type.startsWith("connected")) {
      server.send("lairk:hello", {});
      if (handle?.() && authorize) {
        Promise.resolve(authorize())
          .then((token) => token && server.send("lairk:auth", { token }))
          .catch(() => (walkNo = "login"));
      }
      return;
    }
    if (type.startsWith("lairk:")) receiveLairk(type, parse(content));
  });

  ground = buildGround(Form);
  tower = buildTower(Form);
  faces = buildFaces(Form);
}

function paint($) {
  const { wipe, ink, screen, form } = $;

  // Sky, darkening upward. 2D boxes never touch the depth buffer, so the
  // world's forms paint over them.
  wipe(34, 30, 58);
  const bands = 6;
  for (let i = 0; i < bands; i += 1) {
    const t = i / bands;
    ink(34 + t * 60, 30 + t * 40, 58 + t * 50).box(
      0,
      floor(screen.height * 0.12 + (screen.height * 0.4 * i) / bands),
      screen.width,
      screen.height,
    );
  }

  updateCamera(screen);

  ink(255);
  form([ground, tower, faces, hands], cam, { cpu: true });
  for (const c of characters.values()) {
    if (c.dirty) dress($.Form, c);
    for (const part of [c.base, c.torso]) {
      part.position[0] = c.x;
      part.position[1] = 0;
      part.position[2] = c.z;
      part.rotation[1] = c.facing || 0;
    }
    form([c.base, c.torso], cam, { cpu: true });
  }

  paintTags($);

  if (chatOpen) {
    ink(0, 0, 0, 150).box(0, 0, screen.width, screen.height);
    // The same widgets as laklok (lakChatOptions), minus the wipe so the
    // world stays behind them.
    chat.paint($, lakChatOptions(lakTheme, {
      otherChat: client.system,
      embedded: true,
      presenceRightInset: 52, // Room for the "world" button.
    }));
    paintCloseButton($);
  } else {
    paintReadout($);
    paintHint($);
  }

  paintNotice($);
}

function act($) {
  const { event: e, jump, send } = $;
  const hit = (box) =>
    box && e.x >= box.x && e.x < box.x + box.w && e.y >= box.y && e.y < box.y + box.h;

  if (e.is("keyboard:open")) typing = true;
  if (e.is("keyboard:close")) typing = false;
  if (e.is("reframed")) client && chat.refresh?.(client.system);

  if (chatOpen) {
    // Escape with nothing being typed goes back to the world, where chat.mjs
    // alone would leave for the prompt.
    const leaving =
      (!typing && (e.is("keyboard:down:escape") || e.is("keyboard:down:`"))) ||
      (e.is("touch") && hit(closeBox));
    if (leaving) {
      if (typing) send({ type: "keyboard:close" });
      chatOpen = false;
      return;
    }
    chat.act($, client.system, { allowDelete: true });
    return;
  }

  // 🌍 World mode.
  if (e.is("keyboard:down:enter") || (e.is("touch") && hit(readoutBox))) {
    chatOpen = true;
    held.clear(); // Typing "wasd" into the chat shouldn't walk you.
    tap = null;
    // Signed in, go straight to typing. Signed out, chat.mjs reads an open
    // keyboard as "log in", so leave that to its own Log in button.
    if ($.handle?.()) send({ type: "keyboard:open" });
    return;
  }

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:`")) {
    jump("prompt");
    return;
  }

  // A touch that barely travels is a tap: walk there. One that drags orbits.
  if (e.is("touch")) tap = { travel: 0 };
  if (e.is("draw")) {
    if (tap) tap.travel += Math.abs(e.delta.x) + Math.abs(e.delta.y);
    orbitAngle -= e.delta.x * 0.012;
    orbitHeight = clamp(orbitHeight + e.delta.y * 0.05, 1.5, 16);
    lastLookAt = performance.now();
  }
  if (e.is("lift") && tap) {
    if (tap.travel < 6) {
      if (walker) walkTo = groundPoint($.screen, e.x, e.y);
      else notice = { text: walkNotice($), until: performance.now() + 4000 };
    }
    tap = null;
  }

  if (e.is("scroll")) {
    orbitRadius = clamp(orbitRadius + e.y * 0.02, 8, 34);
    lastLookAt = performance.now();
  }

  for (const key of Object.keys(WALK_KEYS)) {
    if (e.is(`keyboard:down:${key}`)) {
      if (walker) held.add(key);
      else notice = { text: walkNotice($), until: performance.now() + 4000 };
    }
    if (e.is(`keyboard:up:${key}`)) held.delete(key);
  }
}

function sim($) {
  if (lakTheme === "realtime" && realtimeTick()) chat.refresh(client.system);
  chat.sim($);

  const now = performance.now();
  const dt = lastSim ? min(0.1, (now - lastSim) / 1000) : 0;
  lastSim = now;

  if (!walker && now - lastLookAt > 5000) orbitAngle += 0.0012; // Drift.
  walk(dt, now);
  glide(dt);

  noticeNewMessages();
  if (Date.now() - eligibleAt > ROSTER_MS) askRoster();
  updateRoster();

  const minute = new Date().getMinutes() + new Date().getHours() * 60;
  if (minute !== handsMinute) {
    handsMinute = minute;
    hands = buildHands($.Form, new Date());
  }

  const wall = Date.now();
  for (const [handle, b] of bubbles) if (b.until < wall) bubbles.delete(handle);
}

function leave() {
  client?.kill();
}

export { boot, paint, act, sim, leave };

// 📚 Library

// 🎥 Camera

// Builds `cam.matrix` so that the renderer, which feeds each vertex in as
// (-x, y, -z) (see Vertex.transform in lib/graph.mjs), lands it where P·V
// puts the plain world point. Also keeps P·V for projecting labels.
function updateCamera(screen) {
  const eye = [
    focus[0] + sin(orbitAngle) * orbitRadius,
    orbitHeight,
    focus[2] + cos(orbitAngle) * orbitRadius,
  ];
  const view = lookAt(eye, focus);
  const proj = perspective(FOV, screen.width / screen.height, NEAR, FAR);
  camMatrix = mul(proj, view);
  cam.matrix = mul(camMatrix, [-1, 0, 0, 0, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1]);
}

// The renderer's own perspective (Camera.#perspective in lib/graph.mjs):
// +z is forward and clip W is view Z.
function perspective(fov, aspect, near, far) {
  const f = 1 / tan((fov * PI) / 360);
  const range = near - far;
  const m = new Array(16).fill(0);
  m[0] = f / aspect;
  m[5] = f;
  m[10] = (-near - far) / range;
  m[11] = 1;
  m[14] = (2 * far * near) / range;
  return m;
}

// A view matrix with +x right, +y up and +z toward the target.
function lookAt(eye, target) {
  const f = normalize(sub(target, eye));
  const r = normalize(cross([0, 1, 0], f));
  const u = cross(f, r);
  return [
    r[0], u[0], f[0], 0,
    r[1], u[1], f[1], 0,
    r[2], u[2], f[2], 0,
    -dot(r, eye), -dot(u, eye), -dot(f, eye), 1,
  ];
}

// World point -> screen pixel, or null when it's behind the camera.
function project(screen, x, y, z) {
  const m = camMatrix;
  if (!m) return null;
  const cx = m[0] * x + m[4] * y + m[8] * z + m[12];
  const cy = m[1] * x + m[5] * y + m[9] * z + m[13];
  const cw = m[3] * x + m[7] * y + m[11] * z + m[15];
  if (cw < NEAR) return null;
  return {
    x: ((cx / cw + 1) / 2) * screen.width,
    y: ((-cy / cw + 1) / 2) * screen.height,
    depth: cw,
  };
}

// Column-major 4×4 multiply, the gl-matrix layout the renderer uses.
function mul(a, b) {
  const out = new Array(16).fill(0);
  for (let c = 0; c < 4; c += 1) {
    for (let r = 0; r < 4; r += 1) {
      let s = 0;
      for (let k = 0; k < 4; k += 1) s += a[k * 4 + r] * b[c * 4 + k];
      out[c * 4 + r] = s;
    }
  }
  return out;
}

const sub = (a, b) => [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
const cross = (a, b) => [
  a[1] * b[2] - a[2] * b[1],
  a[2] * b[0] - a[0] * b[2],
  a[0] * b[1] - a[1] * b[0],
];
const normalize = (a) => {
  const l = sqrt(dot(a, a)) || 1;
  return [a[0] / l, a[1] / l, a[2] / l];
};
const clamp = (n, lo, hi) => max(lo, min(hi, n));

// 🚶 Walking

// Messages from the session server (session-server/lairk-manager.mjs).
function receiveLairk(type, data) {
  if (!data) return;
  if (type === "lairk:state") {
    for (const [h, p] of Object.entries(data.positions || {})) place(h, p, true);
  } else if (type === "lairk:pos") {
    place(data.handle, data, false);
  } else if (type === "lairk:auth:ok") {
    walker = data.handle;
    walkNo = null;
    if (data.at) place(walker, data.at, true);
    lastLookAt = performance.now(); // Stop the drift; the camera is yours now.
  } else if (type === "lairk:auth:no") {
    walkNo = data.reason || "mention";
  }
}

function parse(content) {
  if (typeof content !== "string") return content;
  try { return JSON.parse(content); } catch { return null; }
}

// Where the server says a handle is. `snap` jumps there (a first sight);
// otherwise the body glides to it.
function place(handle, p, snap) {
  if (!handle || !Number.isFinite(p?.x) || !Number.isFinite(p?.z)) return;
  const h = handle.toLowerCase();
  if (h === walker && !snap) return; // Yours moves locally, not by echo.
  placed.set(h, { x: p.x, z: p.z, facing: p.facing || 0 });
  const c = characters.get(h);
  if (!c) return;
  c.tx = p.x;
  c.tz = p.z;
  c.facing = p.facing || 0;
  if (snap) {
    c.x = p.x;
    c.z = p.z;
  }
}

// Your own body moves the frame you press, then tells the server (~10 Hz).
function walk(dt, now) {
  const me = walker && characters.get(walker);
  if (!me || dt === 0) return;

  // Keys walk relative to the camera; a tap walks to a spot.
  const forward = [-sin(orbitAngle), -cos(orbitAngle)];
  const right = [-cos(orbitAngle), sin(orbitAngle)];
  let dx = 0;
  let dz = 0;
  for (const key of held) {
    const way = WALK_KEYS[key];
    if (way === "forward") { dx += forward[0]; dz += forward[1]; }
    if (way === "back") { dx -= forward[0]; dz -= forward[1]; }
    if (way === "right") { dx += right[0]; dz += right[1]; }
    if (way === "left") { dx -= right[0]; dz -= right[1]; }
  }
  if (held.size > 0) {
    walkTo = null;
  } else if (walkTo) {
    dx = walkTo.x - me.x;
    dz = walkTo.z - me.z;
    if (Math.hypot(dx, dz) < 0.15) walkTo = null;
  }

  const d = Math.hypot(dx, dz);
  if (d > 0.001) {
    const step = min(WALK_SPEED * dt, walkTo ? d : Infinity);
    const next = keepOnGround(me.x + (dx / d) * step, me.z + (dz / d) * step);
    me.x = me.tx = next.x;
    me.z = me.tz = next.z;
    me.facing = (Math.atan2(dx, dz) * 180) / PI;
    unsent = true;
  }

  if (unsent && now - lastSent >= SEND_MS) {
    server?.send("lairk:move", { x: me.x, z: me.z, facing: me.facing });
    lastSent = now;
    unsent = false;
  }

  // The camera follows you.
  const k = 1 - Math.exp(-dt * 6);
  focus[0] += (me.x - focus[0]) * k;
  focus[2] += (me.z - focus[2]) * k;
}

// Everyone else eases toward where the server last saw them — never past it.
function glide(dt) {
  const k = 1 - Math.exp(-dt * 8);
  for (const c of characters.values()) {
    if (c.handle === walker || c.tx === undefined) continue;
    c.x += (c.tx - c.x) * k;
    c.z += (c.tz - c.z) * k;
  }
}

// Mirrors keepOnGround in session-server/lairk-manager.mjs, so what you see
// is what the server will accept.
const BOUNDS = 26;
const TOWER_KEEP = 1.6;
function keepOnGround(x, z) {
  const d = Math.hypot(x, z);
  if (d > BOUNDS) return { x: (x / d) * BOUNDS, z: (z / d) * BOUNDS };
  if (Math.abs(x) < TOWER_KEEP && Math.abs(z) < TOWER_KEEP) {
    if (Math.abs(x) > Math.abs(z)) return { x: Math.sign(x || 1) * TOWER_KEEP, z };
    return { x, z: Math.sign(z || 1) * TOWER_KEEP };
  }
  return { x, z };
}

// The spot on the ground under a screen pixel, or null for the sky.
function groundPoint(screen, sx, sy) {
  const inv = camMatrix && invert(camMatrix);
  if (!inv) return null;
  const nx = (2 * sx) / screen.width - 1;
  const ny = 1 - (2 * sy) / screen.height;
  const unproject = (nz) => {
    const v = [nx, ny, nz, 1];
    const out = [0, 1, 2, 3].map((r) =>
      inv[r] * v[0] + inv[4 + r] * v[1] + inv[8 + r] * v[2] + inv[12 + r] * v[3]);
    return [out[0] / out[3], out[1] / out[3], out[2] / out[3]];
  };
  const a = unproject(-1);
  const b = unproject(1);
  if (a[1] <= b[1]) return null; // Looking up, never reaching the ground.
  const t = a[1] / (a[1] - b[1]);
  return keepOnGround(a[0] + (b[0] - a[0]) * t, a[2] + (b[2] - a[2]) * t);
}

// General 4×4 inverse, column-major.
function invert(m) {
  const [a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23, a30, a31, a32, a33] = m;
  const b00 = a00 * a11 - a01 * a10, b01 = a00 * a12 - a02 * a10;
  const b02 = a00 * a13 - a03 * a10, b03 = a01 * a12 - a02 * a11;
  const b04 = a01 * a13 - a03 * a11, b05 = a02 * a13 - a03 * a12;
  const b06 = a20 * a31 - a21 * a30, b07 = a20 * a32 - a22 * a30;
  const b08 = a20 * a33 - a23 * a30, b09 = a21 * a32 - a22 * a31;
  const b10 = a21 * a33 - a23 * a31, b11 = a22 * a33 - a23 * a32;
  const det = b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06;
  if (!det) return null;
  const i = 1 / det;
  return [
    (a11 * b11 - a12 * b10 + a13 * b09) * i, (a02 * b10 - a01 * b11 - a03 * b09) * i,
    (a31 * b05 - a32 * b04 + a33 * b03) * i, (a22 * b04 - a21 * b05 - a23 * b03) * i,
    (a12 * b08 - a10 * b11 - a13 * b07) * i, (a00 * b11 - a02 * b08 + a03 * b07) * i,
    (a32 * b02 - a30 * b05 - a33 * b01) * i, (a20 * b05 - a22 * b02 + a23 * b01) * i,
    (a10 * b10 - a11 * b08 + a13 * b06) * i, (a01 * b08 - a00 * b10 - a03 * b06) * i,
    (a30 * b04 - a31 * b02 + a33 * b00) * i, (a21 * b02 - a20 * b04 - a23 * b00) * i,
    (a11 * b07 - a10 * b09 - a12 * b06) * i, (a00 * b09 - a01 * b07 + a02 * b06) * i,
    (a31 * b01 - a30 * b03 - a32 * b00) * i, (a20 * b03 - a21 * b01 + a22 * b00) * i,
  ];
}

// 🧱 Geometry — every form is triangles with per-vertex colors (0–1).

function triangles(Form, positions, colors) {
  const f = new Form(
    { type: "triangle", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  f.noFade = true;
  return f;
}

// A box from (x0, y0, z0) to (x1, y1, z1), sides shaded so edges read.
function pushBox(positions, colors, x0, y0, z0, x1, y1, z1, color) {
  const shade = (s) => [color[0] * s, color[1] * s, color[2] * s, 1];
  const quad = (a, b, c, d, s) => {
    const col = shade(s);
    positions.push([...a, 1], [...b, 1], [...c, 1], [...a, 1], [...c, 1], [...d, 1]);
    colors.push(col, col, col, col, col, col);
  };
  quad([x0, y1, z0], [x1, y1, z0], [x1, y1, z1], [x0, y1, z1], 1.0); // top
  quad([x0, y0, z1], [x1, y0, z1], [x1, y1, z1], [x0, y1, z1], 0.85); // +z
  quad([x1, y0, z0], [x0, y0, z0], [x0, y1, z0], [x1, y1, z0], 0.6); // -z
  quad([x1, y0, z1], [x1, y0, z0], [x1, y1, z0], [x1, y1, z1], 0.72); // +x
  quad([x0, y0, z0], [x0, y0, z1], [x0, y1, z1], [x0, y1, z0], 0.5); // -x
}

// A checkered disc of tiles, so depth reads without lighting.
function buildGround(Form) {
  const positions = [];
  const colors = [];
  const size = 2;
  const reach = 14; // tiles from the center
  for (let i = -reach; i < reach; i += 1) {
    for (let j = -reach; j < reach; j += 1) {
      const cx = (i + 0.5) * size;
      const cz = (j + 0.5) * size;
      if (sqrt(cx * cx + cz * cz) > reach * size) continue;
      const odd = (i + j) & 1;
      const col = odd ? [0.36, 0.52, 0.34, 1] : [0.31, 0.46, 0.3, 1];
      const x0 = i * size;
      const z0 = j * size;
      const x1 = x0 + size;
      const z1 = z0 + size;
      positions.push(
        [x0, 0, z0, 1], [x1, 0, z0, 1], [x1, 0, z1, 1],
        [x0, 0, z0, 1], [x1, 0, z1, 1], [x0, 0, z1, 1],
      );
      colors.push(col, col, col, col, col, col);
    }
  }
  return triangles(Form, positions, colors);
}

const TOWER_HALF = 1.1;
const TOWER_TOP = 7;
const FACE_Y = 5.4; // Clock center height.
const FACE_R = 0.8;

function buildTower(Form) {
  const positions = [];
  const colors = [];
  const h = TOWER_HALF;
  pushBox(positions, colors, -h - 0.3, 0, -h - 0.3, h + 0.3, 0.5, h + 0.3, [0.55, 0.5, 0.48]);
  pushBox(positions, colors, -h, 0.5, -h, h, TOWER_TOP, h, [0.78, 0.66, 0.5]);
  pushBox(positions, colors, -h - 0.15, TOWER_TOP, -h - 0.15, h + 0.15, TOWER_TOP + 0.3, h + 0.15, [0.5, 0.3, 0.26]);
  // A stepped roof.
  pushBox(positions, colors, -0.8, TOWER_TOP + 0.3, -0.8, 0.8, TOWER_TOP + 1.0, 0.8, [0.62, 0.22, 0.2]);
  pushBox(positions, colors, -0.4, TOWER_TOP + 1.0, -0.4, 0.4, TOWER_TOP + 1.6, 0.4, [0.7, 0.26, 0.22]);
  pushBox(positions, colors, -0.1, TOWER_TOP + 1.6, -0.1, 0.1, TOWER_TOP + 2.2, 0.1, [0.95, 0.8, 0.3]);
  return triangles(Form, positions, colors);
}

// The four sides the clock faces sit on: outward normal of each.
const SIDES = [[0, 0, 1], [0, 0, -1], [1, 0, 0], [-1, 0, 0]];

// A point on a face: `a` along the face's right, `b` up, `out` off the wall.
function facePoint(n, a, b, out) {
  const right = [-n[2], 0, n[0]]; // up × (−n): right as seen from outside.
  const d = TOWER_HALF + out;
  return [n[0] * d + right[0] * a, FACE_Y + b, n[2] * d + right[2] * a, 1];
}

function buildFaces(Form) {
  const positions = [];
  const colors = [];
  const segs = 16;
  const rim = [0.3, 0.22, 0.18, 1];
  const paper = [0.96, 0.93, 0.84, 1];
  for (const n of SIDES) {
    for (let s = 0; s < segs; s += 1) {
      const a0 = (s / segs) * PI * 2;
      const a1 = ((s + 1) / segs) * PI * 2;
      const c = facePoint(n, 0, 0, 0.02);
      positions.push(
        c,
        facePoint(n, cos(a0) * (FACE_R + 0.1), sin(a0) * (FACE_R + 0.1), 0.02),
        facePoint(n, cos(a1) * (FACE_R + 0.1), sin(a1) * (FACE_R + 0.1), 0.02),
      );
      colors.push(rim, rim, rim);
      positions.push(
        facePoint(n, 0, 0, 0.04),
        facePoint(n, cos(a0) * FACE_R, sin(a0) * FACE_R, 0.04),
        facePoint(n, cos(a1) * FACE_R, sin(a1) * FACE_R, 0.04),
      );
      colors.push(paper, paper, paper);
    }
  }
  return triangles(Form, positions, colors);
}

// Hour and minute hands on every face, showing the viewer's own time.
function buildHands(Form, date) {
  const positions = [];
  const colors = [];
  const minutes = date.getMinutes();
  const hours = (date.getHours() % 12) + minutes / 60;
  const ink = [0.12, 0.1, 0.1, 1];
  const hand = (n, turn, length, width) => {
    const t = turn * PI * 2; // Clockwise from twelve.
    const dx = sin(t);
    const dy = cos(t);
    const px = dy * width; // Perpendicular, in the face's plane.
    const py = -dx * width;
    const a = facePoint(n, -px, -py, 0.06);
    const b = facePoint(n, px, py, 0.06);
    const c = facePoint(n, dx * length + px, dy * length + py, 0.06);
    const d = facePoint(n, dx * length - px, dy * length - py, 0.06);
    positions.push(a, b, c, a, c, d);
    colors.push(ink, ink, ink, ink, ink, ink);
  };
  for (const n of SIDES) {
    hand(n, hours / 12, FACE_R * 0.5, 0.06);
    hand(n, minutes / 60, FACE_R * 0.8, 0.04);
  }
  return triangles(Form, positions, colors);
}

// A small blocky person, feet at the origin, in two forms: head and legs in
// the handle's own colors, and a torso wearing their latest painting.
function dress(Form, c) {
  c.dirty = false;
  const dim = c.online ? 1 : 0.4;
  const [head, legs, shirt] = bodyColors(c);
  const tone = (col, k) => [(col[0] / 255) * k * dim, (col[1] / 255) * k * dim, (col[2] / 255) * k * dim];

  const positions = [];
  const colors = [];
  pushBox(positions, colors, -0.22, 0, -0.14, -0.04, 0.6, 0.14, tone(legs, 0.8)); // legs
  pushBox(positions, colors, 0.04, 0, -0.14, 0.22, 0.6, 0.14, tone(legs, 0.8));
  pushBox(positions, colors, -0.2, 1.36, -0.2, 0.2, 1.76, 0.2, tone(head, 1.1)); // head
  c.base = triangles(Form, positions, colors);

  const tex = c.texture ? (c.online ? c.texture.lit : c.texture.dim) : null;
  c.torso = tex
    ? paintedBox(Form, -0.3, 0.6, -0.18, 0.3, 1.3, 0.18, tex)
    : (() => {
        const p = [];
        const k = [];
        pushBox(p, k, -0.3, 0.6, -0.18, 0.3, 1.3, 0.18, tone(shirt, 1));
        return triangles(Form, p, k);
      })();
}

// Head, legs and shirt all wear the color of the handle's "@" (the first of
// its per-letter colors, set in the `handle` piece), or the tema's handle
// color when none are set — the same color chat draws the "@" in.
function bodyColors(c) {
  const at = c.colors?.[0];
  const color = at
    ? [at.r, at.g, at.b]
    : (LAK_THEMES[lakTheme] || LAK_THEMES.ler).chat.handle || [255, 160, 120];
  return [color, color, color];
}

// A box whose four sides and top all show the painting, right way up.
function paintedBox(Form, x0, y0, z0, x1, y1, z1, texture) {
  const positions = [];
  const texCoords = [];
  const quad = (a, b, c, d) => {
    // a/b are the top corners (left, right), c/d the bottom (right, left).
    positions.push([...a, 1], [...b, 1], [...c, 1], [...a, 1], [...c, 1], [...d, 1]);
    texCoords.push([0, 0], [1, 0], [1, 1], [0, 0], [1, 1], [0, 1]);
  };
  quad([x0, y1, z1], [x1, y1, z1], [x1, y0, z1], [x0, y0, z1]); // +z
  quad([x1, y1, z0], [x0, y1, z0], [x0, y0, z0], [x1, y0, z0]); // -z
  quad([x1, y1, z1], [x1, y1, z0], [x1, y0, z0], [x1, y0, z1]); // +x
  quad([x0, y1, z0], [x0, y1, z1], [x0, y0, z1], [x0, y0, z0]); // -x
  quad([x0, y1, z0], [x1, y1, z0], [x1, y1, z1], [x0, y1, z1]); // top
  const f = new Form(
    { type: "triangle", positions, texCoords },
    { tex: texture },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  f.noFade = true;
  return f;
}

// 🧍 Roster

// Who has a spot: the server's answer over the whole chat history.
function askRoster() {
  eligibleAt = Date.now();
  fetch("/api/lairk-roster")
    .then((res) => (res.ok ? res.json() : null))
    .then((data) => {
      if (!Array.isArray(data?.handles)) return;
      eligible = new Set(data.handles);
      // Per-letter handle colors, straight from @handles.
      for (const [h, colors] of Object.entries(data.colors || {})) {
        rosterColors.set(h, colors);
        const c = characters.get(h);
        if (c && c.colors !== colors) {
          c.colors = colors;
          c.dirty = true;
        }
      }
    })
    .catch(() => {});
}

// Everyone on the roster stands in lairk; the online ones are lit.
function updateRoster() {
  if (!eligible) return; // Nobody stands until the roster arrives.
  const online = new Set((client.system.onlineHandles || []).map(cleanHandle));
  const handles = eligible;

  const key = [...handles].map((h) => (online.has(h) ? "+" : "-") + h).join(",");
  if (key === rosterKey) return;
  rosterKey = key;

  for (const h of [...characters.keys()]) if (!handles.has(h)) characters.delete(h);
  for (const h of handles) {
    const isOnline = online.has(h);
    const existing = characters.get(h);
    if (existing) {
      if (existing.online !== isOnline) {
        existing.online = isOnline;
        existing.dirty = true;
      }
      continue;
    }
    const spot = placed.get(h) || homeSpot(h, handles.size);
    const c = {
      handle: h, x: spot.x, z: spot.z, tx: spot.x, tz: spot.z, facing: spot.facing || 0,
      online: isOnline, colors: rosterColors.get(h) || null, texture: null, dirty: true,
    };
    characters.set(h, c);
    lookQueue.push(c);
  }
  // Online handles first, so the people in the room dress before the rest.
  lookQueue.sort((a, b) => b.online - a.online);
  pumpLooks();
}

// Works through lookQueue a few at a time.
function pumpLooks() {
  while (looking < LOOKS_AT_ONCE && lookQueue.length > 0) {
    const c = lookQueue.shift();
    if (!characters.has(c.handle)) continue;
    looking += 1;
    lookUp(c).finally(() => {
      looking -= 1;
      pumpLooks();
    });
  }
}

// Ask for a handle's most recent painting; it redresses the body when it
// lands. (Colors come with the roster.) A miss leaves the plain shirt.
function lookUp(c) {
  const painting = fetch(`/media-collection?for=${encodeURIComponent(`@${c.handle}/painting`)}`)
    .then((res) => (res.ok ? res.json() : null))
    .then((data) => {
      const latest = data?.files?.at?.(-1); // Oldest first; the last is newest.
      if (!latest) return null;
      // The listed URL carries the owner's auth0 sub, which isn't public;
      // `/media/@handle/painting/<slug>.png` is (the route get.painting uses).
      const slug = latest.split("/").pop().replace(/\.png$/, "");
      return get.picture(`/media/@${c.handle}/painting/${slug}.png`);
    })
    .then((got) => {
      const img = got?.img; // get.picture resolves { url, img }.
      if (!img?.pixels) return;
      c.texture = { lit: shrink(img, 1), dim: shrink(img, 0.4) };
      c.dirty = true;
    })
    .catch(() => {});

  return painting;
}

// A painting at most 96 pixels on a side (a torso is a few pixels tall on
// screen), darkened by `k` for handles that aren't online.
function shrink(img, k) {
  const scale = min(1, 96 / max(img.width, img.height));
  const width = max(1, floor(img.width * scale));
  const height = max(1, floor(img.height * scale));
  const pixels = new Uint8ClampedArray(width * height * 4);
  for (let y = 0; y < height; y += 1) {
    for (let x = 0; x < width; x += 1) {
      const from = (floor(y / scale) * img.width + floor(x / scale)) * 4;
      const to = (y * width + x) * 4;
      pixels[to] = img.pixels[from] * k;
      pixels[to + 1] = img.pixels[from + 1] * k;
      pixels[to + 2] = img.pixels[from + 2] * k;
      pixels[to + 3] = 255;
    }
  }
  return { width, height, pixels };
}

// Where a handle stands until it walks somewhere: a disc around the tower
// that widens with the roster so a big room doesn't pile up (square-root
// spacing keeps the density even out to the rim).
function homeSpot(handle, count = 1) {
  const hash = handleHash(handle);
  const angle = ((hash % 3600) / 3600) * PI * 2;
  const spread = clamp(sqrt(count) * 1.6, 6, 24);
  const radius = 3.5 + sqrt(((hash >>> 12) % 1000) / 1000) * spread;
  return { x: sin(angle) * radius, z: cos(angle) * radius };
}

function handleHash(handle) {
  let h = 2166136261;
  for (let i = 0; i < handle.length; i += 1) {
    h ^= handle.charCodeAt(i);
    h = Math.imul(h, 16777619);
  }
  return h >>> 0;
}

// Writes "@handle" the way the chat does: letter by letter in the handle's
// own colors when it has them, otherwise in the tema's handle color.
function writeHandle(ink, handle, colors, x, y, cw, k = 1) {
  const text = "@" + handle;
  const fallback = (LAK_THEMES[lakTheme] || LAK_THEMES.ler).chat.handle || [255, 160, 120];
  for (let i = 0; i < text.length; i += 1) {
    const o = colors?.[i];
    const col = o ? [o.r, o.g, o.b] : fallback;
    ink(col[0] * k, col[1] * k, col[2] * k).write(text[i], { x: x + i * cw, y });
  }
}

function cleanHandle(from) {
  if (!from || typeof from !== "string") return null;
  return (from.startsWith("@") ? from.slice(1) : from).toLowerCase();
}

// 💬 Messages

// The first bulk of messages is history; anything after it speaks aloud.
function noticeNewMessages() {
  const messages = client.system.messages;
  if (!historySettled) {
    if (messages.length === 0) return;
    for (const m of messages) seen.add(m);
    historySettled = true;
    return;
  }
  for (const m of messages) {
    if (seen.has(m)) continue;
    seen.add(m);
    const h = cleanHandle(m.from);
    if (!h || h === "log") continue;
    bubbles.set(h, { text: plainText(m.text), until: Date.now() + BUBBLE_MS });
  }
}

// Drops chat.mjs's inline `\r,g,b\` color codes.
function plainText(text) {
  return String(text || "").replace(/\\[^\\]*\\/g, "").replace(/\s+/g, " ").trim();
}

function fit(text, chars) {
  return text.length > chars ? text.slice(0, max(0, chars - 3)) + "..." : text;
}

// 🚶 Why you can't walk (yet): the server's answer, or the roster's.
function walkNotice($) {
  const me = cleanHandle($.handle?.());
  if (!me) return "log in to take your spot in lairk";
  if (!eligible) return "asking laer klokken who is here...";
  if (walkNo === "mention" || !eligible.has(me)) return "you must be mentioned in laer klokken to have a spot";
  if (walkNo === "unavailable") return "lairk can't check the roster right now";
  if (walkNo === "login") return "your login couldn't be verified - try logging in again";
  return "joining lairk...";
}

// 🏷️ Name tags and bubbles, far to near.
function paintTags($) {
  const { ink, screen, typeface } = $;
  const cw = typeface?.blockWidth || 6;
  const tags = [];
  const crowded = characters.size > 24;
  for (const c of characters.values()) {
    const bubble = bubbles.get(c.handle);
    if (crowded && !c.online && !bubble && c.handle !== walker) continue;
    const p = project(screen, c.x, 2.05, c.z);
    if (p) tags.push({ c, p, bubble });
  }
  tags.sort((a, b) => b.p.depth - a.p.depth);

  for (const { c, p, bubble } of tags) {
    const name = "@" + c.handle;
    const nx = floor(p.x - (name.length * cw) / 2);
    const ny = floor(p.y - 10);
    ink(0, 0, 0, 110).box(nx - 1, ny - 1, name.length * cw + 2, 11);
    writeHandle(ink, c.handle, c.colors, nx, ny, cw, c.online ? 1 : 0.7);

    if (bubble) {
      const text = fit(bubble.text, max(8, floor((screen.width - 16) / cw)));
      const w = text.length * cw + 8;
      const bx = floor(clamp(p.x - w / 2, 2, screen.width - w - 2));
      const by = ny - 18;
      ink(250, 248, 238).box(bx, by, w, 14);
      ink(250, 248, 238).box(floor(p.x) - 1, by + 14, 3, 3);
      ink(20, 16, 24).write(text, { x: bx + 4, y: by + 2 });
    }
  }
}

// 📜 The last few lines of the room, under the corner label.
function paintReadout($) {
  const { ink, screen, typeface } = $;
  const cw = typeface?.blockWidth || 6;
  const lines = client.system.messages
    .filter((m) => cleanHandle(m.from) && cleanHandle(m.from) !== "log")
    .slice(-3);
  const top = 22;
  const rowH = 11;
  const h = max(1, lines.length) * rowH + 4;
  readoutBox = { x: 0, y: top, w: screen.width, h };
  ink(0, 0, 0, 120).box(0, top, screen.width, h);
  if (lines.length === 0) {
    ink(200, 200, 220).write("connecting to laer klokken...", { x: 6, y: top + 2 });
    return;
  }
  const chars = floor((screen.width - 12) / cw);
  lines.forEach((m, i) => {
    const h = cleanHandle(m.from);
    const name = "@" + h + " ";
    const y = top + 2 + i * rowH;
    writeHandle(ink, h, characters.get(h)?.colors, 6, y, cw);
    ink(235, 232, 245).write(fit(plainText(m.text), chars - name.length), {
      x: 6 + name.length * cw,
      y,
    });
  });
}

function paintHint($) {
  const { ink, screen, typeface } = $;
  const cw = typeface?.blockWidth || 6;
  const text = walker
    ? "wasd or tap the ground to walk - enter to chat"
    : "tap the top or press enter to chat";
  ink(0, 0, 0, 90).box(0, screen.height - 16, screen.width, 16);
  ink(210, 205, 230).write(text, {
    x: max(4, floor((screen.width - text.length * cw) / 2)),
    y: screen.height - 13,
  });
}

function paintCloseButton($) {
  const { ink, screen, pen, typeface } = $;
  const cw = typeface?.blockWidth || 6;
  const label = "world";
  const w = label.length * cw + 8;
  closeBox = { x: screen.width - w - 4, y: 8, w, h: 16 };
  const hot = pen && pen.x >= closeBox.x && pen.x < closeBox.x + w &&
    pen.y >= closeBox.y && pen.y < closeBox.y + closeBox.h;
  ink(...(hot ? [255, 230, 120] : [70, 62, 110])).box(closeBox.x, closeBox.y, w, closeBox.h);
  ink(...(hot ? [20, 16, 24] : [240, 236, 255])).write(label, { x: closeBox.x + 4, y: closeBox.y + 3 });
}

function paintNotice($) {
  if (!notice || notice.until < performance.now()) return;
  const { ink, screen, typeface } = $;
  const cw = typeface?.blockWidth || 6;
  const text = fit(notice.text, floor((screen.width - 16) / cw));
  const w = text.length * cw + 12;
  const x = floor((screen.width - w) / 2);
  const y = floor(screen.height * 0.62);
  ink(20, 16, 30, 220).box(x, y, w, 18);
  ink(255, 220, 120).write(text, { x: x + 6, y: y + 4 });
}
