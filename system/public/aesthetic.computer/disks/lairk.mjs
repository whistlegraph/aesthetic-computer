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

  Walking is arena's (Quake pmove, jump, a platform with thickness) with
  oskiewar's netcode — see lib/lairk-world.mjs. Your body is predicted from
  your inputs at a fixed 60 Hz and sent to the session server
  (session-server/lairk-manager.mjs) by tick; the server runs the same step
  and is the truth, and when it answers we snap to it and replay what it
  hasn't seen. Everyone else is drawn ~100 ms behind, between snapshots.
  Cameras: third person (behind you) or first (your eyes), `v` to switch;
  watchers who can't walk get an orbit around the tower.
 */

/* #region 🏁 TODO
  + Done
  - [x] Walking — position relay ~10 Hz, remotes glide.
  - [x] Server-side move gate (spoke + mentioned by someone else).
  - [x] Remember each handle's position within lairk.
  - [x] Arena controls, jump, bodies and platform; first/third person.
  - [x] Oskiewar-style netcode: inputs by tick, prediction + replay.
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
import { humanoid, HUMANOID_TORSO } from "../lib/humanoid.mjs";
import {
  lairkSpawn,
  lairkStep,
  packInput,
  unpackState,
  BTN,
  LAIRK_DT,
  LAIRK_HALF,
  LAIRK_THICKNESS,
  LAIRK_TOWER_HALF,
  LAIRK_TOWER_TOP,
} from "../lib/lairk-world.mjs";

const { sin, cos, tan, sqrt, min, max, floor, PI } = Math;

let client; // The `clock` chat connection (the same room as laklok).
let lakTheme = "ler"; // The visitor's laklok tema, so the chat wears it here too.
let get; // `$.get`, kept for loading paintings as handles appear.

// 🎥 Cameras: "orbit" around the tower (watchers), or — once you walk —
// "third" (behind you) and "first" (your eyes).
let view = "orbit";
let look = { yaw: 180, pitch: -8 }; // Where you face, degrees; +pitch is up.
let penLocked = false; // Mouse captured for looking (desktop).
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
let ground, tower, faces, shadow;
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
let socketReady = false; // The session socket has said "connected".
let authAsked = false; // A walk request has gone out for this socket.
const placed = new Map(); // handle -> { x, z, facing } the server remembers.

// Prediction (oskiewar §5): your body runs locally at a fixed tick, and the
// inputs the server hasn't acknowledged wait here to be replayed.
let me = null; // { state, home } — your predicted body.
let tick = 0; // Your last simulated input tick.
let stepClock = 0; // Leftover time toward the next tick, seconds.
let lastSim = 0; // performance.now() of the previous sim.
const pending = []; // [{ tick, cmd }] not yet acknowledged.
const SEND_EVERY = 2; // Ticks between input packets (30 Hz)...
const REDUNDANCY = 6; // ...each carrying the last few inputs again.
const correction = [0, 0, 0]; // Visual offset after a snap, eased away.

// Everyone else, drawn in the past between snapshots, never ahead.
const remotes = new Map(); // handle -> [{ at, s }] (at = server ms)
let clockOffset = null; // Estimated server ms minus local ms.
const INTERP_MS = 100;

// Input.
const held = new Set(); // Movement keys held down.
let walkTo = null; // { x, z } — a tap on the ground steers you there.
let tap = null; // { travel } — a touch that hasn't become a look drag yet.
const MOVE_KEYS = {
  w: "fwd", arrowup: "fwd",
  s: "back", arrowdown: "back",
  a: "left", arrowleft: "left",
  d: "right", arrowright: "right",
  space: "jump",
  shift: "crouch",
};
let viewChip = null; // Tap target for the 1st / 3rd toggle.

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

function boot({ api, Form, debug, send, hud, store, colon, params, get: getter }) {
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
  // 🚶 chat.boot owns the session socket (a piece gets one receiver), so
  // lairk listens through it: watch everyone's positions, and ask to walk
  // once signed in. The server checks the token and the roster.
  socketReady = false;
  authAsked = false;
  server = null;
  chat.boot(api, client.system, {
    onSocket: (_id, type, content, socket) => {
      server = socket;
      if (type.startsWith("connected")) {
        socketReady = true;
        authAsked = false; // A fresh socket needs its own walk request.
        server.send("lairk:hello", {});
      } else if (type.startsWith("lairk:")) {
        receiveLairk(type, parse(content));
      }
    },
  });
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
  me = null;
  tick = 0;
  stepClock = 0;
  pending.length = 0;
  remotes.clear();
  clockOffset = null;
  correction.fill(0);
  view = "orbit";
  focus[0] = 0;
  focus[2] = 0;

  ground = buildGround(Form);
  shadow = buildShadow(Form);
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
    const p = placeFor(c);
    c.rx = p.x; c.ry = p.y; c.rz = p.z; // Where it's drawn (tags use it).
    if (c.handle === walker && view === "first") continue; // You are the camera.
    for (const part of [c.base, c.torso]) {
      if (!part) continue;
      part.position[0] = p.x;
      part.position[1] = p.y; // Eye height: the humanoid hangs below it.
      part.position[2] = p.z;
      part.rotation[1] = p.yaw;
    }
    form([c.base, c.torso], cam, { cpu: true });
    // A drop shadow on the platform, smaller the higher you are.
    if (Math.abs(p.x) < LAIRK_HALF && Math.abs(p.z) < LAIRK_HALF) {
      const k = clamp(1 - (p.y - 2) / 6, 0.35, 1);
      shadow.position[0] = p.x;
      shadow.position[1] = 0.02;
      shadow.position[2] = p.z;
      shadow.scale = [k, 1, k];
      form(shadow, cam, { cpu: true });
    }
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
    paintViewChip($);
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
    walkTo = null;
    // Signed in, go straight to typing. Signed out, chat.mjs reads an open
    // keyboard as "log in", so leave that to its own Log in button.
    if ($.handle?.()) send({ type: "keyboard:open" });
    return;
  }

  if (e.is("keyboard:down:escape") || e.is("keyboard:down:`")) {
    jump("prompt");
    return;
  }

  if (e.is("pen:locked")) penLocked = true;
  if (e.is("pen:unlocked")) penLocked = false;

  // 👀 Walking: look with the mouse (click to capture it) or by dragging;
  // `v` or the chip switches between third and first person.
  if (walker) {
    if (e.is("touch") && hit(viewChip)) {
      toggleView();
      return;
    }
    if (e.is("keyboard:down:v")) toggleView();
    if (e.is("touch") && e.device === "mouse" && !penLocked) $.penLock();
    if (penLocked && e.is("move")) turn(e.delta.x * 0.2, e.delta.y * 0.2);
  }

  // A touch that barely travels is a tap (walk there); one that drags looks.
  if (e.is("touch")) tap = { travel: 0 };
  if (e.is("draw")) {
    if (tap) tap.travel += Math.abs(e.delta.x) + Math.abs(e.delta.y);
    if (walker) {
      if (!penLocked) turn(e.delta.x * 0.35, e.delta.y * 0.35);
    } else {
      orbitAngle -= e.delta.x * 0.012;
      orbitHeight = clamp(orbitHeight + e.delta.y * 0.05, 1.5, 16);
      lastLookAt = performance.now();
    }
  }
  if (e.is("lift") && tap) {
    if (tap.travel < 6 && !penLocked) {
      if (walker) walkTo = groundPoint($.screen, e.x, e.y);
      else notice = { text: walkNotice($), until: performance.now() + 4000 };
    }
    tap = null;
  }

  if (e.is("scroll") && !walker) {
    orbitRadius = clamp(orbitRadius + e.y * 0.02, 8, 34);
    lastLookAt = performance.now();
  }

  for (const key of Object.keys(MOVE_KEYS)) {
    if (e.is(`keyboard:down:${key}`)) {
      if (walker) held.add(MOVE_KEYS[key]);
      else if (key !== "shift") notice = { text: walkNotice($), until: performance.now() + 4000 };
    }
    if (e.is(`keyboard:up:${key}`)) held.delete(MOVE_KEYS[key]);
  }
}

function sim($) {
  if (lakTheme === "realtime" && realtimeTick()) chat.refresh(client.system);
  chat.sim($);

  // The socket often connects before sign-in settles, so ask to walk as soon
  // as both are ready — whichever comes last.
  if (socketReady && !authAsked && $.handle?.()) askToWalk($.authorize);

  const now = performance.now();
  const dt = lastSim ? min(0.25, (now - lastSim) / 1000) : 0;
  lastSim = now;

  if (!walker && now - lastLookAt > 5000) orbitAngle += 0.0012; // Drift.
  predict(dt);
  const ease = Math.exp(-dt * 12); // Snap corrections fade over ~100 ms.
  for (let i = 0; i < 3; i++) correction[i] *= ease;

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
  let eye;
  let target;
  if (walker && me) {
    const p = {
      x: me.state.x + correction[0],
      y: me.state.y + correction[1],
      z: me.state.z + correction[2],
    };
    const yr = (look.yaw * PI) / 180;
    const pr = (look.pitch * PI) / 180;
    const ahead = [sin(yr) * cos(pr), sin(pr), cos(yr) * cos(pr)];
    if (view === "first") {
      eye = [p.x, p.y, p.z];
      target = [p.x + ahead[0], p.y + ahead[1], p.z + ahead[2]];
    } else {
      // Third person: behind and a little above, looking past your head.
      const back = 5;
      eye = [p.x - ahead[0] * back, p.y + 0.8 - ahead[1] * back, p.z - ahead[2] * back];
      eye[1] = max(eye[1], 0.4); // Not under the platform.
      target = [p.x + ahead[0] * 2, p.y + 0.3 + ahead[1] * 2, p.z + ahead[2] * 2];
    }
  } else {
    eye = [
      focus[0] + sin(orbitAngle) * orbitRadius,
      orbitHeight,
      focus[2] + cos(orbitAngle) * orbitRadius,
    ];
    target = focus;
  }
  const viewMatrix = lookAt(eye, target);
  const proj = perspective(FOV, screen.width / screen.height, NEAR, FAR);
  camMatrix = mul(proj, viewMatrix);
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

// Hand the server a token; it checks it and the roster, and answers.
function askToWalk(authorize) {
  authAsked = true;
  if (!authorize) return (walkNo = "login");
  Promise.resolve(authorize())
    .then((token) => {
      if (!token) return (walkNo = "login"); // Signed in, but no token to show.
      server.send("lairk:auth", { token });
      // An answer comes back in well under a second; silence means trouble.
      setTimeout(() => {
        if (!walker && !walkNo) walkNo = "unavailable";
      }, 8000);
    })
    .catch(() => (walkNo = "login"));
}

// Messages from the session server (session-server/lairk-manager.mjs).
function receiveLairk(type, data) {
  if (!data) return;
  if (type === "lairk:state") {
    for (const [h, p] of Object.entries(data.positions || {})) place(h, p);
  } else if (type === "lairk:auth:ok") {
    walker = data.handle;
    walkNo = null;
    me = { state: unpackState(data.state), home: data.home };
    tick = 0;
    pending.length = 0;
    correction.fill(0);
    look.yaw = me.state.yaw;
    look.pitch = -8;
    view = "third";
  } else if (type === "lairk:auth:no") {
    walkNo = data.reason || "mention";
  } else if (type === "lairk:snap") {
    snapshot(data);
  } else if (type === "lairk:still") {
    remotes.delete(data.handle);
    place(data.handle, data);
  }
}

function parse(content) {
  if (typeof content !== "string") return content;
  try { return JSON.parse(content); } catch { return null; }
}

// Where the server remembers a handle standing (not walking right now).
function place(handle, p) {
  if (!handle || !Number.isFinite(p?.x) || !Number.isFinite(p?.z)) return;
  const h = handle.toLowerCase();
  placed.set(h, { x: p.x, z: p.z, facing: p.facing || 0 });
  const c = characters.get(h);
  if (!c || h === walker) return;
  c.x = p.x;
  c.z = p.z;
  c.facing = p.facing || 0;
}

// Your input this tick, from keys (relative to where you face) or a tap.
function currentInput() {
  let fwd = (held.has("fwd") ? 1 : 0) - (held.has("back") ? 1 : 0);
  let right = (held.has("right") ? 1 : 0) - (held.has("left") ? 1 : 0);
  let yaw = look.yaw;
  if (fwd || right) walkTo = null;
  else if (walkTo && me) {
    const dx = walkTo.x - me.state.x;
    const dz = walkTo.z - me.state.z;
    if (Math.hypot(dx, dz) < 0.4) walkTo = null;
    else {
      yaw = (Math.atan2(dx, dz) * 180) / PI; // pmove: yaw 0 faces +z.
      look.yaw = yaw;
      fwd = 1;
    }
  }
  const buttons = (held.has("jump") ? BTN.JUMP : 0) | (held.has("crouch") ? BTN.CROUCH : 0);
  return { fwd, right, yaw, pitch: look.pitch, buttons };
}

// Fixed-step prediction: one lairkStep per 1/60 s of real time, each input
// kept until the server acknowledges it, sent a few times over for loss.
function predict(dt) {
  if (!me || !server) return;
  stepClock = min(stepClock + dt, 0.25); // After a stall, don't sprint to catch up.
  while (stepClock >= LAIRK_DT) {
    stepClock -= LAIRK_DT;
    tick += 1;
    const cmd = currentInput();
    me.state = lairkStep(me.state, cmd, me.home);
    pending.push({ tick, cmd });
    if (tick % SEND_EVERY === 0) {
      const inputs = pending.slice(-REDUNDANCY).map((p) => packInput(p.tick, p.cmd));
      server.send("lairk:input", { inputs });
    }
  }
  if (pending.length > 240) pending.splice(0, pending.length - 240); // 4 s cap.
}

// A snapshot: adopt the server's truth for you and replay what's in flight
// (oskiewar reconciliation); buffer everyone else for interpolation.
function snapshot(data) {
  const local = performance.now();
  if (Number.isFinite(data.ms)) {
    // Max-filter the offset: the least-delayed packet is the best estimate.
    const sample = data.ms - local;
    clockOffset = clockOffset === null ? sample : max(sample, clockOffset - 1);
  }
  for (const { h, s } of data.players || []) {
    if (h === walker) {
      if (me && Number.isFinite(data.ack)) reconcile(unpackState(s), data.ack);
      continue;
    }
    const buf = remotes.get(h) || [];
    buf.push({ at: data.ms, s: unpackState(s) });
    while (buf.length > 20) buf.shift();
    remotes.set(h, buf);
  }
}

function reconcile(truth, ack) {
  while (pending.length && pending[0].tick <= ack) pending.shift();
  const before = me.state;
  let s = truth;
  for (const p of pending) s = lairkStep(s, p.cmd, me.home);
  // Small differences ease out visually; a big one (a real correction) snaps.
  const dx = before.x - s.x, dy = before.y - s.y, dz = before.z - s.z;
  if (Math.hypot(dx, dy, dz) < 2) {
    correction[0] += dx;
    correction[1] += dy;
    correction[2] += dz;
  } else {
    correction.fill(0);
  }
  me.state = s;
}

// Where to draw a character this frame: you (predicted), a walker (in the
// past, between snapshots), or someone standing where they last stood.
function placeFor(c) {
  if (c.handle === walker && me) {
    return {
      x: me.state.x + correction[0],
      y: me.state.y + correction[1],
      z: me.state.z + correction[2],
      yaw: me.state.yaw,
    };
  }
  const buf = remotes.get(c.handle);
  if (buf?.length && clockOffset !== null) {
    const t = performance.now() + clockOffset - INTERP_MS;
    let a = buf[0];
    let b = buf[0];
    for (const snap of buf) {
      if (snap.at <= t) a = snap;
      if (snap.at >= t) { b = snap; break; }
      b = snap; // Past the newest: hold it (never extrapolate).
    }
    const span = b.at - a.at;
    const k = span > 0 ? clamp((t - a.at) / span, 0, 1) : 1;
    return {
      x: a.s.x + (b.s.x - a.s.x) * k,
      y: a.s.y + (b.s.y - a.s.y) * k,
      z: a.s.z + (b.s.z - a.s.z) * k,
      yaw: lerpAngle(a.s.yaw, b.s.yaw, k),
    };
  }
  return { x: c.x, y: 2, z: c.z, yaw: c.facing || 0 }; // Eye height standing.
}

function lerpAngle(a, b, k) {
  const d = ((((b - a) % 360) + 540) % 360) - 180;
  return a + d * k;
}

function turn(dx, dy) {
  look.yaw += dx;
  look.pitch = clamp(look.pitch - dy, -80, 80);
}

function toggleView() {
  view = view === "first" ? "third" : "first";
}

// The spot on the platform under a screen pixel, or null for the sky.
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
  const x = a[0] + (b[0] - a[0]) * t;
  const z = a[2] + (b[2] - a[2]) * t;
  if (Math.abs(x) > LAIRK_HALF || Math.abs(z) > LAIRK_HALF) return null;
  return { x, z };
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

// A square platform of checkered tiles with a thick skirt below its edge,
// like arena's — so depth reads without lighting and the edge reads as a
// drop.
function buildGround(Form) {
  const positions = [];
  const colors = [];
  const size = 2;
  const n = LAIRK_HALF / size;
  for (let i = -n; i < n; i += 1) {
    for (let j = -n; j < n; j += 1) {
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
  // The skirt: four sides from the top edge down, earth under turf.
  const h = LAIRK_HALF;
  const lip = [0.24, 0.36, 0.22, 1];
  const earth = [0.3, 0.22, 0.17, 1];
  const deep = [0.16, 0.12, 0.1, 1];
  const side = (a, b) => {
    const [ax, az] = a;
    const [bx, bz] = b;
    const y1 = -0.25;
    const y2 = -LAIRK_THICKNESS;
    positions.push(
      [ax, 0, az, 1], [bx, 0, bz, 1], [bx, y1, bz, 1],
      [ax, 0, az, 1], [bx, y1, bz, 1], [ax, y1, az, 1],
      [ax, y1, az, 1], [bx, y1, bz, 1], [bx, y2, bz, 1],
      [ax, y1, az, 1], [bx, y2, bz, 1], [ax, y2, az, 1],
    );
    colors.push(lip, lip, lip, lip, lip, lip, earth, earth, deep, earth, deep, deep);
  };
  side([-h, h], [h, h]);
  side([h, -h], [-h, -h]);
  side([h, h], [h, -h]);
  side([-h, -h], [-h, h]);
  return triangles(Form, positions, colors);
}

// A soft dark disc laid on the platform under a body.
function buildShadow(Form) {
  const positions = [];
  const colors = [];
  const segs = 12;
  const r = 0.55;
  const dark = [0, 0, 0, 0.35];
  for (let i = 0; i < segs; i += 1) {
    const a0 = (i / segs) * PI * 2;
    const a1 = ((i + 1) / segs) * PI * 2;
    positions.push([0, 0, 0, 1], [cos(a0) * r, 0, sin(a0) * r, 1], [cos(a1) * r, 0, sin(a1) * r, 1]);
    colors.push(dark, dark, dark);
  }
  return triangles(Form, positions, colors);
}

const TOWER_HALF = LAIRK_TOWER_HALF;
const TOWER_TOP = LAIRK_TOWER_TOP;
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

// Arena's person (lib/humanoid.mjs), hung from its eye at the origin, in two
// forms: the body in the handle's "@" color, and a torso wearing their latest
// painting. Handles that aren't online are dimmed.
function dress(Form, c) {
  c.dirty = false;
  const dim = c.online ? 1 : 0.45;
  const [color] = bodyColors(c);
  const tex = c.texture ? (c.online ? c.texture.lit : c.texture.dim) : null;
  const { positions, colors } = humanoid(color.map((v) => v * dim), { torso: !tex });
  c.base = triangles(Form, positions, colors);
  // Without a painting the humanoid already wears a plain torso.
  c.torso = tex ? paintedBox(Form, ...HUMANOID_TORSO, tex) : null;
}

// A body wears the color of the handle's "@" (the first of its per-letter
// colors, set in the `handle` piece), or the tema's handle color when none
// are set — the same color chat draws the "@" in.
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
  const spread = clamp(sqrt(count) * 1.6, 6, LAIRK_HALF - 5);
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
  if (walkNo === "unavailable") return "lairk isn't answering right now - try again soon";
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
    if (c.handle === walker && view === "first") continue; // Your own eyes.
    if (c.rx === undefined) continue;
    const p = project(screen, c.rx, c.ry + 0.55, c.rz);
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
    ? fit("wasd walk  space jump  v view  enter chat", floor((screen.width - 60) / cw))
    : "tap the top or press enter to chat";
  ink(0, 0, 0, 90).box(0, screen.height - 16, screen.width, 16);
  ink(210, 205, 230).write(text, {
    x: max(4, floor((screen.width - text.length * cw) / 2)),
    y: screen.height - 13,
  });
}

// The 1st / 3rd person chip, bottom right while walking.
function paintViewChip($) {
  viewChip = null;
  if (!walker) return;
  const { ink, screen, typeface } = $;
  const cw = typeface?.blockWidth || 6;
  const label = view === "first" ? "1st" : "3rd";
  const w = label.length * cw + 8;
  viewChip = { x: screen.width - w - 3, y: screen.height - 16, w, h: 16 };
  ink(70, 62, 110).box(viewChip.x, viewChip.y + 1, w, 14);
  ink(240, 236, 255).write(label, { x: viewChip.x + 4, y: viewChip.y + 3 });
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
