import { HZ, DT, PROTOCOL, move, spawn, clamp } from './world.mjs';
import { reconcile, interpolate } from './net.mjs';
import { LocalPresentation, SnapshotClock } from './presentation.mjs';
import { Prompt } from './prompt.mjs';
import { objectBoxes } from './objects.mjs';
import { direction, rayBox, wallDistance, along, SCREEN, WORLD } from './world.mjs';
import { createView } from './view.mjs';
import { Connection } from './connection.mjs';
import { ClientDiagnostics } from './diagnostics-client.mjs';
import { Stalls } from './stalls.mjs';
import { accountToken, accountExpected } from './account.mjs';
import { Controller } from './controller.mjs';
import { moveFreecam } from './freecam.mjs';
import { Radial, HOLD_SECONDS, ringGeometry } from './radial.mjs';
import { TouchPad, touchButtons } from './touch.mjs';
import { Sfx, SpatialTones } from './sfx.mjs';
import { kidlispWall } from './kidlisp-wall.mjs';
import { sourceTones, beatAt } from './object-language.mjs';

const view = await createView(document.getElementById('game'));
const canvas = view.canvas,
  keys = new Set();
const query = new URLSearchParams(location.search);
const requestedRoom = query.get('room');
// 🎯 Hosted under the AC prompt (lib/dool-sheet.mjs on aesthetic.computer):
// the paper over this frame drops on a tap and rises on Escape, Back or a tap
// on the corner block. Behind the curtain the world keeps drawing, bare of
// its HUD, makes no sound and holds no seat.
const hosted = query.get('embed') === 'prompt' && window.parent !== window;
let sheetOpen = !hosted,
  unlockedAt = -1e9;
const toHost = (message) => {
  if (hosted) window.parent.postMessage(message, location.origin);
};
function leave() {
  if (hosted) {
    releaseInput();
    if (locked()) document.exitPointerLock();
    toHost({ type: 'dool:sheet:leave' });
  } else location.href = '/';
}
// 🏠 A signed-in handle arrives in its own block (the server says which the
// first time; this browser remembers). A `room` in the address wins, and so
// does anywhere travelled to since.
let home = null;
try {
  const kept = JSON.parse(localStorage.getItem('dool-home') || 'null');
  if (kept && /^[a-z0-9-]{1,32}$/.test(kept.room)) home = { room: kept.room, lot: kept.lot };
} catch {}
let roamed = !!requestedRoom;
let room =
  requestedRoom && /^[a-z0-9-]{1,32}$/.test(requestedRoom)
    ? requestedRoom
    : (accountExpected() && home?.room) || 'lobby';
// The block's lots and who settled each ({ rects, homes }), or null in the lobby.
let lots = null;
// Under the prompt the host may not have said who is signed in when this
// loads: look again shortly, so the room behind the curtain is home too.
if (!roamed && home && room !== home.room)
  for (const ms of [800, 2500, 6000])
    setTimeout(() => {
      if (!roamed && home && room !== home.room && accountExpected()) travel(home.room, false);
    }, ms);
let ws,
  seat = -1,
  player,
  snapshot,
  prediction = null,
  resetPrediction = false,
  history = [],
  pending = [],
  seq = 0;
let yaw = 0,
  pitch = 0,
  third = false,
  firing = false,
  latestAt = 0,
  eventId = 0;
let chalkStrokes = [[], []],
  chalkVersion = 0,
  chalkLocal = [],
  // Draw mode inside a build: { text, target } while you chalk the shape.
  chalkMode = null,
  chalkMouse = false;
let useQueued = false,
  dropQueued = false;
let jumpQueued = false,
  fireQueued = false,
  audio,
  notice = 'CONNECTING';
let last = performance.now(),
  debt = 0;
let resting = false; // The host window is in the background (lib/dool-sheet.mjs).
const BACKGROUND_FPS = 60; // The room's top rate behind a background window's paper.
let lastBackgroundFrameAt = 0;
const presentation = new LocalPresentation(),
  playback = new SnapshotClock();
const prompt = new Prompt();
// 💬 Room chat: T opens a line (Quake's say key), Enter says it, Escape drops
// it. The same text entry as the build prompt, with a flat cursor.
const chat = new Prompt();
const CHAT_MAX = 140, CHAT_SHOWN_MS = 14000;
let chatLog = []; // { at, handle, name, colors, text }
const connection = new Connection();
let selfMember = null;
const diagnostics = new ClientDiagnostics((sample) => {
  if (ws?.readyState === WebSocket.OPEN && ws.bufferedAmount < 2048) send(sample);
});
let longTaskObserver;
function configureDiagnostics(enabled) {
  diagnostics.enabled = enabled;
  diagnostics.reset();
  longTaskObserver?.disconnect();
  if (enabled && typeof PerformanceObserver !== 'undefined' && PerformanceObserver.supportedEntryTypes?.includes('longtask')) {
    longTaskObserver = new PerformanceObserver((list) => {
      if (!document.hidden && sheetOpen)
        for (const entry of list.getEntries()) diagnostics.longTask(entry.duration);
    });
    longTaskObserver.observe({ type: 'longtask' });
  }
}
// The connection's own log, for the offline panel: what the socket and the
// server's health said, when. Kept short.
const netLog = [];
function logNet(text, flags = {}) {
  const d = new Date();
  const when = [d.getHours(), d.getMinutes(), d.getSeconds()].map((n) => String(n).padStart(2, '0')).join(':');
  netLog.push({ when, text, ...flags });
  if (netLog.length > 40) netLog.shift();
}
let parked = false; // The client closed the room's socket itself (back on the curtain).
let lastSeat = -1; // The seat held last time, for the next flight in.
let cornerScale = 1, shownCornerScale = 1; // The curtain crosshair's breadth, which the room follows.
let connectAttempts = 0,
  healthPoll = null,
  silentSince = null,
  down = false,
  dustedAt = 0,
  mouseAt = null, // The free mouse's last place on the canvas (CSS px), and when.
  cursorAim = false; // This frame the aim ray went through the mouse.
const CURSOR_AIM = false; // A free mouse does not drag the aim around (2026-11-03).
// Down: the socket is closed, or it is open but the server has said nothing
// for three seconds (a restart in progress, or the network gone).
function netDown(now) {
  if (parked) return false; // Left on purpose: not an outage.
  if (ws && ws.readyState === WebSocket.CLOSED) return true;
  const open = ws?.readyState === WebSocket.OPEN;
  const silent = open && connection.lastSnapshot !== null && now - connection.lastSnapshot > 3000;
  if (silent && silentSince === null) {
    silentSince = now;
    logNet('no word from the server for 3 s', { bad: true });
  } else if (!silent) silentSince = null;
  return silent;
}
// While the socket is down, ask the server's health every two seconds and
// say what it answered; when it is back, reload (new code lands) or rejoin.
function watchHealth(hadSnapshot) {
  if (healthPoll) return;
  const ask = async () => {
    healthPoll = null;
    if (ws && ws.readyState !== WebSocket.CLOSED) return;
    try {
      const r = await fetch(new URL('./health', location.href), { cache: 'no-store' });
      if (r.ok) {
        const h = await r.json().catch(() => ({}));
        logNet(`server up: ${String(h.revision || '').slice(0, 10)}, building ${h.building ?? '?'}`, { good: true });
        if (hadSnapshot) {
          logNet('reloading to rejoin');
          return window.doolRig?.restart ? window.doolRig.restart() : location.reload();
        }
        connect();
        return;
      }
      logNet(`server answered ${r.status} (restarting?)`, { bad: true });
    } catch (error) {
      logNet(`server unreachable: ${String(error?.message || error).slice(0, 40)}`, { bad: true });
    }
    healthPoll = setTimeout(ask, 2000);
  };
  healthPoll = setTimeout(ask, 1000);
}
const controller = new Controller();
const radial = new Radial();
const touch = new TouchPad();
// A phone shows its buttons before the first touch.
touch.active = !!window.matchMedia?.('(pointer: coarse)').matches && !window.matchMedia?.('(any-pointer: fine)').matches;
const sfx = new Sfx();
// Two walls run real KidLisp: $roz on the far wall, $bop on the left.
// The room says which $code is on each wall; a change loads the new one.
const walls = [];
const wallCodes = {};
let wallRes = { north: 64, west: 64 };
function setWalls(spec = {}, res = wallRes) {
  for (const [face, code] of Object.entries(spec)) {
    const cols = res[face] || 64;
    if (wallCodes[face] === `${code}@${cols}`) continue;
    wallCodes[face] = `${code}@${cols}`;
    kidlispWall(code, { width: cols, height: Math.round((cols * 9) / 16) })
      .then((w) => {
        if (wallCodes[face] !== `${code}@${cols}`) return;
        const i = walls.findIndex((x) => x.face === face);
        const entry = { ...w, code, face };
        if (i >= 0) walls[i] = entry;
        else walls.push(entry);
      })
      .catch((e) => console.info(`KidLisp wall $${code}:`, e.message));
  }
}
// The walls and the floating cube of KidLisp are retired for now: the room
// is bare, the walls stay plain, and the cube is gone. The wall machinery
// above stays for when a room asks for them again.
const cube = [];
// Standing KidLisp screens: a `$code` prompt stands that piece on the field.
// The room lists them; each code's program runs here, once, for all of them.
let screens = [];
const screenPrograms = new Map();
function setScreens(list) {
  for (const s of list) {
    if (screenPrograms.has(s.code)) continue;
    screenPrograms.set(s.code, null);
    kidlispWall(s.code, { width: 64, height: 36 })
      .then((w) => screenPrograms.set(s.code, w))
      .catch((e) => console.info(`KidLisp screen $${s.code}:`, e.message));
  }
  // Keep the same objects for screens that stay, so their frame caches hold.
  screens = list.map((s) => Object.assign(screens.find((o) => o.id === s.id) || {}, s));
}
// What the face cam reacts to: when you were hurt and from which side, when
// to grin.
const face = { hurtAt: -1e9, hurtSide: 0, grinAt: -1e9 };
// The server's wall clock, learned from snapshots (the fastest arrival is the
// best estimate), so every player hears the beat land together.
let clockOffset = null;
const tones = new SpatialTones(sfx);
const toneCache = new Map();
// Items are carried and run over, so only the rest collide.
let solidList = [],
  solidFrom = null;
function solids() {
  if (solidFrom !== objects) {
    solidFrom = objects;
    solidList = objects.filter((o) => !o.item);
  }
  return solidList;
}
// Every object's oscillators, placed in the room.
function toneSources() {
  const out = [];
  for (const o of objects) {
    const key = `${o.id}:${o.version}`;
    if (!toneCache.has(key)) {
      if (toneCache.size > 64) toneCache.clear();
      try {
        toneCache.set(key, sourceTones(o.geometry));
      } catch {
        toneCache.set(key, []);
      }
    }
    toneCache.get(key).forEach((t, i) =>
      out.push({ ...t, key: `${key}:${i}`, x: o.x + t.at[0], y: t.at[1], z: o.z + t.at[2] }),
    );
  }
  return out;
}
let claimedAt = 0;
let fps = 0,
  fpsFrames = 0,
  fpsAt = performance.now();
let holdTime = 0,
  holdSpent = false,
  devLog = [];
// Behind the curtain the camera looks into a corner, so two walls, the
// ceiling and the dotted floor all show and the room reads as a room.
let freecam = { x: -3, y: 2.1, z: -WORLD + 2, yaw: 28, pitch: -7 };
let freecamPlaced = false;
// Just arrived and not seated yet (the seat waits for a signed-in handle):
// that is joining, not watching, so none of the spectator's mode shows.
let joiningUntil = 0;
const joining = () => seat < 0 && performance.now() < joiningUntil;
let gamepad;
let joinURL = ['localhost', '127.0.0.1', '[::1]'].includes(location.hostname)
  ? null : new URL(`?room=${room}`, location.href).href;
let members = [];
// Start opens the room menu; Back opens the $code history.
// Short notices for finished or failed builds, newest last.
let notices = [];
let braincells = null,
  shownBraincells = null;
let tiles = {},
  floorAim = null;
let handleHistory = [],
  historyPending = false;
// Motion blur: a post layer that blends each frame over the last ones,
// more as the view turns faster. Toggled in the room menu.
let motionBlur = true;
try {
  motionBlur = localStorage.getItem('dool-blur') !== 'off';
} catch {}
let post, postCtx, lastYaw = 0, lastPitch = 0, turnSpeed = 0;
function postProcess(elapsed) {
  const src = view.canvas;
  if (!motionBlur || orbitMix > 0) { // No smear while the camera flies in or out.
    if (post) post.style.display = 'none';
    return;
  }
  if (!post) {
    post = document.createElement('canvas');
    post.setAttribute('aria-hidden', 'true');
    Object.assign(post.style, { position: 'fixed', inset: '0', width: '100%', height: '100%', pointerEvents: 'none', imageRendering: 'pixelated', cursor: 'none' });
    document.body.appendChild(post);
    postCtx = post.getContext('2d');
  }
  post.style.display = 'block';
  if (post.width !== src.width || post.height !== src.height) {
    post.width = src.width;
    post.height = src.height;
  }
  // Degrees per second of view rotation, eased.
  const turned = Math.abs(((yaw - lastYaw + 540) % 360) - 180) + Math.abs(pitch - lastPitch);
  lastYaw = yaw;
  lastPitch = pitch;
  turnSpeed += ((turned / Math.max(1 / 240, elapsed)) - turnSpeed) * 0.3;
  const trail = Math.min(0.65, turnSpeed / 600);
  postCtx.globalAlpha = 1 - trail;
  postCtx.drawImage(src, 0, 0);
  postCtx.globalAlpha = 1;
}
// Lock-on: the view eases to keep this object centred, in third person;
// null when free. The d-pad moves the lock around you.
let lockOn = null,
  thirdBeforeLock = false;
function spotOf(id) {
  const o = objects.find((x) => x.id === id);
  if (o) {
    const it = snapshot?.items?.find((x) => x.id === id);
    return { x: it ? it.x : o.x, y: (it ? it.y : 0) + 1, z: it ? it.z : o.z };
  }
  const w = walls.find((x) => `wall:${x.code}` === id);
  if (w) return w.face === 'north' ? { x: 0, y: 10, z: SCREEN } : { x: -SCREEN, y: 10, z: 0 };
  return null;
}
function selectables() {
  return [...objects.map((o) => o.id), ...walls.map((w) => `wall:${w.code}`)];
}
function setLock(id) {
  if (id && !lockOn) {
    thirdBeforeLock = third;
    third = true;
  }
  if (!id && lockOn) third = thirdBeforeLock;
  lockOn = id ? { id, wall: id.startsWith('wall:') } : null;
}
// Step the lock to the next selectable clockwise (dir 1) or anticlockwise.
function cycleLock(dir) {
  if (!player || !lockOn) return;
  const bearing = (id) => {
    const p = spotOf(id);
    return p ? (((Math.atan2(p.x - player.x, p.z - player.z) * 180) / Math.PI - yaw + 540) % 360) - 180 : null;
  };
  const list = selectables()
    .map((id) => ({ id, b: bearing(id) }))
    .filter((x) => x.b !== null)
    .sort((a, b) => a.b - b.b);
  const at = list.findIndex((x) => x.id === lockOn.id);
  if (list.length < 2 || at < 0) return;
  setLock(list[(at - dir + list.length) % list.length].id);
  sfx.ringMove(at);
}
// Removing takes two presses: the first arms it for two seconds.
let removeArmed = null;
function removeAimed() {
  if (!focusObject || focusObject.wall) return;
  const now = performance.now();
  if (removeArmed?.id === focusObject.id && now - removeArmed.at < 2000) {
    send({ type: 'remove', objectId: focusObject.id });
    removeArmed = null;
    sfx.ringBack();
  } else {
    removeArmed = { id: focusObject.id, at: now };
    sfx.refuse();
  }
}
let menu = null,
  codeHistory = [],
  menuStickAt = 0;
let objects = [],
  owner = null,
  builder = false,
  builderNames = [],
  jobs = [],
  focusObject = null;
const locked = () => document.pointerLockElement === canvas;
// Keyboard-only play: the first control key engages the keys without a pointer
// capture, and the arrows turn and tilt; no mouse is needed.
let keyboardMode = false;
const engaged = () => locked() || keyboardMode;
const CONTROL_KEYS = [
  'KeyW', 'KeyA', 'KeyS', 'KeyD', 'Space', 'ShiftLeft', 'ShiftRight', 'KeyV', 'KeyX',
  'ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight',
  'KeyZ', 'BracketLeft', 'BracketRight', 'Delete', 'Backspace',
];
let activeSeconds = 0; // Seconds of play this visit.
// The controls stay up whenever you are in the room, pad or keys
// (2026-11-08: jeffrey wants every action always in view; no hide key).
const legendShown = () => seat >= 0;
const send = (data) => {
  if (
    ws?.readyState === WebSocket.OPEN &&
    ws.bufferedAmount < (data.type === 'input' ? 2048 : 32768)
  ) {
    ws.send(JSON.stringify(data));
    return true;
  }
  return false;
};

function tone(frequency, duration, volume = 0.04) {
  sfx.voice(frequency, duration, { volume });
}

async function capture() {
  if (locked() || prompt.open) return;
  const resumeAudio = sfx.wake();
  if (!ws || ws.readyState === WebSocket.CLOSED) connect();
  if (touch.active) {
    await resumeAudio.catch(() => {});
    notice = '';
    return;
  }
  try {
    await canvas.requestPointerLock();
    await resumeAudio;
    notice = '';
  } catch (error) {
    console.info('Pointer capture:', error.message);
    notice = 'CLICK TO CAPTURE MOUSE';
  }
}

// Behind the curtain nothing is connected yet, but the room is persistent:
// fetch what stands in it so the world shows it from the first frame
// (2026-11-03). The socket's state replaces this when it joins.
async function peekRoom() {
  try {
    const r = await fetch(new URL(`./room?room=${encodeURIComponent(room)}`, import.meta.url), { cache: 'no-store' });
    if (!r.ok) return;
    const data = await r.json();
    if (ws && ws.readyState !== WebSocket.CLOSED) return; // Joined meanwhile: the live state wins.
    if (Array.isArray(data.objects)) objects = data.objects;
    if (data.tiles && typeof data.tiles === 'object') tiles = data.tiles;
    if (Array.isArray(data.screens)) setScreens(data.screens);
    if (Array.isArray(data.chalk)) {
      chalkStrokes = data.chalk;
      chalkVersion++;
    }
  } catch {}
}
if (hosted) void peekRoom();

// A read-only window into the aim, for end-to-end checks (?debug only).
if (query.has('debug'))
  window.doolDebug = () => ({
    seat,
    player: player ? { x: player.x, y: player.y, z: player.z } : null,
    yaw,
    pitch,
    locked: locked(),
    origin: view.aimOrigin(),
    forward: view.aimForward(),
    floorAim,
    corrections: { ...corrections },
    pending: pending.length,
    rtt: connection.rtt,
    focus: focusObject?.id || null,
    cursorAim,
    canvas: { w: canvas.width, h: canvas.height, cw: canvas.clientWidth, ch: canvas.clientHeight },
  });

// 🚪 To another room of the one world: the same page, a new socket. What
// stood in the old room is put away first so it never shows in the new one.
function travel(to, chosen = true) {
  if (!/^[a-z0-9-]{1,32}$/.test(to) || to === room) return;
  if (chosen) roamed = true;
  room = to;
  lots = null;
  objects = [];
  tiles = {};
  setScreens([]);
  chalkStrokes = [];
  chalkVersion++;
  if (joinURL) joinURL = new URL(`?room=${room}`, location.href).href;
  try {
    const address = new URL(location.href);
    address.searchParams.set('room', room);
    window.history.replaceState(window.history.state, '', address);
  } catch {}
  if (ws && ws.readyState !== WebSocket.CLOSED) {
    const old = ws;
    ws = null; // its close is ours, not an outage
    old.close();
    connect();
  } else void peekRoom();
}
function connect() {
  const url = new URL('./play', import.meta.url);
  url.protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
  // 🔐 Signed in on this page: fetch the token while the socket opens, and
  // ask the server to keep the seat for the handle (no guest name first).
  const account = accountExpected() ? accountToken() : null;
  url.search = new URLSearchParams({ room, protocol: PROTOCOL, ...(account ? { account: '1' } : {}) });
  history = [];
  pending = [];
  player = null;
  snapshot = null;
  prediction = null;
  resetPrediction = false;
  seq = 0;
  eventId = 0;
  debt = 0;
  presentation.reset(null);
  playback.reset();
  notice = 'CONNECTING';
  connection.reset();
  members = [];
  selfMember = null;
  freecamPlaced = false;
  joiningUntil = performance.now() + 7000;
  connectAttempts++;
  logNet(`connecting to room ${room} (try ${connectAttempts})`);
  ws = new WebSocket(url);
  const socket = ws;
  socket.onopen = () => {
    if (socket === ws) logNet('socket open', { good: true });
  };
  socket.onmessage = (e) => {
    if (socket !== ws) return;
    const messageAt = performance.now();
    const data = JSON.parse(e.data);
    if (data.type === 'welcome' || data.type === 'role') {
      const old = player;
      seat = data.seat;
      if (seat >= 0) lastSeat = seat;
      if (seat < 0) {
        freecam = old
          ? { x: old.x + 2, y: old.y + 2, z: old.z - 3, yaw, pitch }
          : freecam;
        yaw = freecam.yaw;
        pitch = freecam.pitch;
        freecamPlaced = false;
      }
      player = null;
      prediction = null;
      resetPrediction = false;
      pending = [];
      seq = 0;
      debt = 0;
      prompt.open = false;
      prompt.busy = false;
      radial.close();
      releaseInput();
      presentation.reset(null);
    }
    if (data.type === 'welcome') {
      configureDiagnostics(data.diagnostics === true);
      logNet(seat >= 0 ? `joined as player ${seat + 1}` : account ? 'signing in' : 'joined as a spectator', { good: true });
      if (data.joinURL) joinURL = data.joinURL;
      let token;
      try {
        token = localStorage.getItem('dool-owner');
      } catch {}
      send({ type: 'identity', token });
      void (async () => {
        let accessToken = await (account || accountToken());
        // Signed in here but no token yet (a slow link times the sign-in out):
        // seat as a guest for now and keep asking; the server names the seat
        // the moment the handle verifies, so the login is never dropped.
        for (let i = 0; !accessToken && account && i < 4 && ws === socket; i++) {
          if (i === 0) send({ type: 'account-none' });
          await new Promise((r) => setTimeout(r, 2500 * (i + 1)));
          if (ws === socket) accessToken = await accountToken();
        }
        if (ws !== socket) return;
        if (accessToken) send({ type: 'account', accessToken });
        else if (account) send({ type: 'account-none' }); // Nobody after all: seat the guest now.
      })();
    }
    if (data.type === 'home' && /^[a-z0-9-]{1,32}$/.test(data.room || '')) {
      home = { room: data.room, lot: data.lot };
      try {
        localStorage.setItem('dool-home', JSON.stringify(home));
      } catch {}
      if (!roamed && home.room !== room) travel(home.room, false);
    }
    if (data.type === 'lots' && data.room === room && Array.isArray(data.rects)) lots = { rects: data.rects, homes: data.homes || [] };
    if (data.type === 'presence') {
      members = data.members;
      selfMember = data.self || null;
    }
    if (data.type === 'handle-history' && Array.isArray(data.items)) {
      handleHistory = data.items;
      historyPending = false;
      if (menu) {
        menu.items = menuItems();
        menu.selected = Math.min(menu.selected, menu.items.length - 1);
      }
    }
    if (data.type === 'braincells' && Number.isFinite(data.remaining)) {
      braincells = data.remaining;
      if (shownBraincells === null) shownBraincells = braincells;
    }
    if (data.type === 'chalk' && Array.isArray(data.strokes)) {
      // Whole: both seats on join, or one seat after a clear or trim.
      if (Number.isInteger(data.seat)) chalkStrokes[data.seat] = data.strokes;
      else chalkStrokes = data.strokes;
      chalkVersion++;
      if (data.seat === seat || !Number.isInteger(data.seat)) chalkLocal = [];
    }
    if (data.type === 'chalk-ops' && Array.isArray(data.ops)) {
      for (const [s, k, x, y, z] of data.ops) {
        const strokes = (chalkStrokes[s] ||= []);
        (strokes[k] ||= []).push({ x, y, z });
        if (s === seat) chalkLocal = [];
      }
      chalkVersion++;
    }
    if (data.type === 'dev-log')
      for (const d of data.entries) {
        // A finished step updates the line it belongs to rather than adding one.
        if (d.kind === 'done') {
          const running = devLog.findLast((x) => x.kind === 'do' && !x.done);
          if (running) running.done = true;
          continue;
        }
        devLog = [...devLog, { ...d, at: performance.now() }].slice(-8);
      }
    if (data.type === 'dev-log')
      for (const d of data.entries.slice(-1)) if (d.kind !== 'done') sfx.dev(d.kind);
    if (data.type === 'pong') connection.reply(data.at, performance.now());
    if (data.type === 'said' && typeof data.text === 'string') {
      chatLog = [...chatLog, { at: performance.now(), handle: data.handle || null, name: data.name || null,
        colors: data.colors || null, seat: data.seat, text: data.text.slice(0, CHAT_MAX) }].slice(-8);
      sfx.ringMove(data.seat === seat ? 5 : 8);
    }
    if (data.type === 'identity') {
      owner = data.owner;
      prompt.busy = seat >= 0 && jobs.filter((j) => j.owner === owner).length >= 3;
      try {
        localStorage.setItem('dool-owner', data.token);
      } catch {}
    }
    if (data.type === 'objects') {
      diagnostics.jobs(data.jobs.map((j) => j.id));
      for (const job of data.jobs) sfx.buildProgress(job.id, job.preview?.boxes?.length || 0);
      objects = data.objects;
      builder = data.builder;
      builderNames = Array.isArray(data.builders) ? data.builders : [];
      if (Array.isArray(data.history)) codeHistory = data.history;
      if (data.tiles && typeof data.tiles === 'object') tiles = data.tiles;
      if (data.wallRes && typeof data.wallRes === 'object') wallRes = data.wallRes;
      // Room wall specs are ignored while the walls are plain (setWalls above).
      if (Array.isArray(data.screens)) setScreens(data.screens);
      jobs = data.jobs;
      prompt.busy = seat >= 0 && jobs.filter((j) => j.owner === owner).length >= 3;
    }
    if (data.type === 'build-accepted') {
      sfx.buildStart();
      if (player) view.effect({ type: 'think', at: { x: player.x, y: player.y + 0.35, z: player.z } });
      prompt.busy = true;
      prompt.message = 'AESEL IS BUILDING';
      prompt.open = false;
    }
    if (data.type === 'build-result') {
      if (data.error) sfx.buildFail(data.id);
      else if (data.cancelled) sfx.ringBack();
      else sfx.buildDone(data.id);
      prompt.busy = false;
      prompt.message = data.cancelled
        ? 'CANCELLED'
        : data.error || (data.kidlisp ? `$${data.kidlisp} IS ON THE WALL` : `V${data.version} READY`);
      // Several builds can run: a result is a notice, never a takeover.
      notices = [
        ...notices,
        {
          at: performance.now(),
          ok: !data.error && !data.cancelled,
          text: data.cancelled ? 'cancelled' : data.error ? data.error : data.kidlisp ? `$${data.kidlisp} is on the wall` : `v${data.version} ready`,
        },
      ].slice(-3);
      if (!data.error) prompt.text = '';
      canvas.setAttribute(
        'aria-label',
        data.cancelled
          ? 'Build cancelled. Enter to prompt Aesel.'
          : data.error ||
              `Object version ${data.version} ready. Enter to prompt Aesel.`,
      );
    }
    if (data.type === 'snapshot') stalls.heard();
    if (data.type === 'snapshot' && !document.hidden && sheetOpen) diagnostics.snapshot();
    if (data.type !== 'snapshot' || (snapshot && data.tick <= snapshot.tick)) {
      diagnostics.max('messageMaxMs', performance.now() - messageAt);
      stalls.handled(data.type, performance.now() - messageAt);
      return;
    }
    const own = data.players[seat];
    if (seat < 0 && !freecamPlaced) {
      const target = data.players.find(Boolean);
      if (target) {
        freecam = {
          x: clamp(target.x + 4, -WORLD + 1, WORLD - 1),
          y: target.y + 3,
          z: clamp(target.z - 5, -WORLD + 1, WORLD - 1),
        };
        yaw =
          (Math.atan2(target.x - freecam.x, target.z - freecam.z) * 180) /
          Math.PI;
        pitch =
          (Math.atan2(
            target.y - freecam.y,
            Math.hypot(target.x - freecam.x, target.z - freecam.z),
          ) *
            180) /
          Math.PI;
        freecamPlaced = true;
      }
    }
    if (seat >= 0 && !own) return;
    // Socket callbacks only retain the newest authority. Replaying prediction
    // for every queued snapshot can starve input, rendering and even pongs.
    if (own) {
      if (prediction && diagnostics.enabled) diagnostics.values.snapshotsCoalesced++;
      resetPrediction ||= !!snapshot && data.phase !== snapshot.phase;
      prediction = data;
    }
    if (Number.isFinite(data.epoch)) {
      const offset = data.epoch - Date.now();
      clockOffset = clockOffset === null ? offset : Math.max(offset, clockOffset - 2);
    }
    snapshot = data;
    latestAt = performance.now();
    connection.snapshot(data.tick, latestAt, HZ);
    history.push(data);
    history = history.slice(-32);
    playback.receive(data.tick, latestAt);
    if (notice === 'CONNECTING') notice = '';
    for (const event of data.events) {
      if (event.id <= eventId) continue;
      eventId = event.id;
      if (data.tick - event.tick > 15) continue;
      view.effect(event);
      if (event.type === 'launch') sfx.launch();
      if (event.type === 'blast') sfx.blast();
      if (event.type === 'frag') sfx.frag(event.attacker === seat);
      if (event.type === 'shot') sfx.shot();
      if (event.type === 'poke') sfx.poke();
      if (event.type === 'throw') sfx.toss();
      if (event.type === 'hit') sfx.hit(event.seat === seat);
      if (event.type === 'hit' && event.seat === seat && player) {
        face.hurtAt = performance.now();
        const from = data.players[event.attacker];
        if (from && event.attacker !== seat) {
          // Which side the hit came from, relative to where you face.
          const to = (Math.atan2(from.x - player.x, from.z - player.z) * 180) / Math.PI;
          const rel = ((to - yaw + 540) % 360) - 180;
          face.hurtSide = Math.abs(rel) < 30 ? 0 : rel > 0 ? -1 : 1;
        } else face.hurtSide = 0;
      }
      if ((event.type === 'pickup' && event.seat === seat) || (event.type === 'frag' && event.attacker === seat && event.seat !== seat))
        face.grinAt = performance.now();
      if (event.type === 'pickup') sfx.pickup(event.seat === seat);
      if (event.type === 'drop') sfx.ringBack();
      if (event.type === 'bounce') sfx.bounce();
      if (event.type === 'removed') sfx.ringBack();
      if (event.type === 'ricochet') sfx.ricochet();
      if (event.type === 'cancel') sfx.ricochet(1.5);
      if (event.type === 'headshot') sfx.headshot();
    }
    diagnostics.max('messageMaxMs', performance.now() - messageAt);
    stalls.handled(data.type, performance.now() - messageAt);
  };
  socket.onclose = (e) => {
    if (socket !== ws) return;
    if (parked) {
      // Our own leave: drop the seat quietly; the room stays as it was seen.
      seat = -1;
      player = null;
      snapshot = null;
      prediction = null;
      resetPrediction = false;
      members = [];
      selfMember = null;
      prompt.busy = false;
      void peekRoom();
      return;
    }
    logNet(`socket closed ${e.code}${e.reason ? ' ' + e.reason : ''}${e.wasClean ? '' : ' (not clean)'}`, { bad: true });
    // When the server goes away (a restart or an update), watch its health
    // and reload once it is back, so new code lands without anyone touching
    // the screen; without a snapshot yet, just rejoin.
    watchHealth(!!snapshot);
    if (locked()) document.exitPointerLock();
    notice = e.reason || 'DISCONNECTED — CLICK TO REJOIN';
    prompt.busy = false;
    owner = null;
    builder = false;
    objects = [];
    jobs = [];
    members = [];
    selfMember = null;
    player = null;
    snapshot = null;
    prediction = null;
    resetPrediction = false;
    pending = [];
    history = [];
    presentation.reset(null);
    playback.reset();
  };
  socket.onerror = () => {
    if (socket === ws) logNet('socket error', { bad: true });
    notice = 'SERVER UNAVAILABLE — CLICK TO RETRY';
  };
}

canvas.addEventListener('click', (e) => {
  if (!locked() && view.cornerHit(e.offsetX, e.offsetY)) return leave();
  // The code tag in the bottom-right is a button: it opens the $code's page.
  if (!locked() && view.qrURL && view.qrHit(e.offsetX, e.offsetY)) return void window.open(view.qrURL, '_blank', 'noopener');
  void capture();
});
canvas.addEventListener('paste', (e) => {
  if (chat.open) {
    e.preventDefault();
    chat.insert(e.clipboardData?.getData('text') || '');
    chat.text = chat.text.slice(0, CHAT_MAX);
    chat.cursor = Math.min(chat.cursor, chat.text.length);
    return;
  }
  if (!prompt.open) return;
  e.preventDefault();
  prompt.insert(e.clipboardData?.getData('text') || '');
});
// Keyboard Enter, the controller's X and the ring's centre all land here.
// Where chalk lands: the aimed surface, or 3 m ahead in the air.
function chalkPoint() {
  const held = (gamepad?.boost || chalkMouse || keys.has('KeyC')) && seat >= 0 && !prompt.open;
  if (!held || !player) return null;
  const dir = direction(yaw, pitch);
  const reach = Math.min(3, wallDistance(player, dir, 3, solids(), 0) - 0.05);
  return along(player, dir, Math.max(0.6, reach));
}
// Send the drawing as this build's turn.
function submitChalk() {
  if (!chalkMode || prompt.busy) return;
  if (!owner || !builder) {
    prompt.message = !owner ? 'CONNECTING' : 'AESEL IS OFFLINE';
    return;
  }
  prompt.busy = true;
  send({ type: 'prompt', prompt: chalkMode.text, objectId: chalkMode.target?.id, baseVersion: chalkMode.target?.version, chalk: true });
  chalkMode = null;
}
// The one menu's tabs: where to go, what you made, how it looks. The tab
// last open is where it opens next.
const MENU_TABS = ['go', '$', 'view'];
let menuTab = 0;
function menuItems() {
  // 🚪 (The old room menu's rows live here since 2026-11-13; "clear the
  // room" is retired.) Both places always show; the one you stand in says so.
  const here = (id) => (room === id ? ' · here' : '');
  const places = [
    home
      ? { label: `home · ${home.room} lot ${home.lot}${here(home.room)}`, act: () => travel(home.room) }
      : { label: 'home · log in for a lot', act: () => {} },
    { label: `lobby${here('lobby')}`, act: () => travel('lobby') },
  ];
  const switches = [
    { label: third ? 'first person' : 'third person', stay: true, act: () => (third = !third) },
    {
      stay: true,
      label: motionBlur ? 'motion blur off' : 'motion blur on',
      act: () => {
        motionBlur = !motionBlur;
        try {
          localStorage.setItem('dool-blur', motionBlur ? 'on' : 'off');
        } catch {}
      },
    },
    {
      label: 'fullscreen',
      act: () => (window.doolRig ? window.doolRig.toggleFullscreen() : canvas.requestFullscreen?.().catch(() => {})),
    },
    { label: 'erase my chalk', act: () => send({ type: 'chalk-clear' }) },
  ];
  // Everything your handle has made that fits the space, from any session,
  // with what this room remembers for you; newest first, a code once.
  const time = (at) => (typeof at === 'number' ? at : Date.parse(at) || 0);
  const thumbs = new Map(handleHistory.map((h) => [h.code, h.thumb]));
  const seen = new Set();
  const list = [
    ...codeHistory.map((h) => ({ code: h.code, at: time(h.at), label: `$${h.code} ${h.name.toLowerCase()} v${h.version}${h.kind && h.kind !== 'object' ? ' · ' + h.kind : ''}` })),
    ...handleHistory.map((h) => ({ code: h.code, at: time(h.at), label: `$${h.code} ${h.name.toLowerCase()}` })),
  ]
    .sort((a, b) => b.at - a.at)
    .filter((h) => !seen.has(h.code) && seen.add(h.code))
    .map((h) => ({ ...h, thumb: thumbs.get(h.code) || null }));
  return [
    places,
    list.length
      ? list.map((h) => ({ ...h, act: () => send({ type: 'restore', code: h.code }) }))
      : [{ label: historyPending ? 'loading' : 'nothing made yet', act: () => {} }],
    switches,
  ][menuTab];
}
// Left and right turn the tabs; the list starts at its top.
function turnMenu(step, to = null) {
  if (!menu) return;
  menuTab = to ?? (menuTab + step + MENU_TABS.length) % MENU_TABS.length;
  menu.tab = menuTab;
  menu.items = menuItems();
  menu.selected = 0;
  sfx.ringMove(menuTab);
}
function toggleMenu() {
  if (menu) return closeMenu();
  historyPending = !handleHistory.length;
  send({ type: 'handle-history' });
  menu = { kind: 'history', tabs: MENU_TABS, tab: menuTab, items: menuItems(), selected: 0 };
  closeBuild();
  releaseInput();
  sfx.ringOpen();
}
function closeMenu() {
  menu = null;
  sfx.ringBack();
}
function chooseMenu() {
  const item = menu?.items[menu.selected];
  sfx.ringPick(0);
  if (item?.stay) {
    // A switch flips where it stands and reads its new state.
    item.act();
    menu.items = menuItems();
    return;
  }
  menu = null;
  item?.act();
}
function holding() {
  return !!snapshot?.items?.some((it) => it.holder === seat && seat >= 0);
}
function submitPrompt(gesture = true) {
  if (!prompt.text.trim() || prompt.busy) return false;
  // A bare $code stands that KidLisp piece on the field, a few steps ahead
  // and facing you. The room keeps it; Aesel is not involved.
  const code = /^\$([a-z0-9]{2,16})$/i.exec(prompt.text.trim());
  if (code && seat >= 0 && player && !radial.open) {
    const dir = direction(yaw, 0);
    send({ type: 'screen', code: code[1], x: player.x + dir.x * 3.5, z: player.z + dir.z * 3.5, yaw });
    prompt.text = '';
    closeBuild();
    if (gesture) void capture();
    return true;
  }
  const item = radial.open ? radial.item : null;
  const chalk = radial.open && radial.chalk;
  if (radial.open && radial.target?.wall) {
    const face = walls.find((w) => w.code === radial.target.code)?.face;
    if (/^(sharper|chunkier)$/.test(prompt.text.trim())) {
      send({ type: 'wall-res', face, dir: prompt.text.trim() === 'sharper' ? 1 : -1 });
      closeBuild();
      return true;
    }
    send({ type: 'kidlisp-mod', face, prompt: prompt.text });
    prompt.busy = true;
    closeBuild();
    return true;
  }
  if (chalk && /erase my chalk/.test(prompt.text)) {
    send({ type: 'chalk-clear' });
    closeBuild();
    return true;
  }
  if (!owner || !builder) {
    prompt.message = !owner ? 'CONNECTING' : 'AESEL IS OFFLINE';
    return false;
  }
  prompt.busy = true;
  prompt.open = false;
  radial.close();
  releaseInput();
  // Pointer capture must start inside the gesture, not the later server reply.
  if (gesture) void capture();
  send({
    type: 'prompt',
    prompt: prompt.text,
    objectId: prompt.target?.id,
    baseVersion: prompt.target?.version,
    item,
    chalk,
  });
  return true;
}
function openRadial(wall = null) {
  if (seat < 0 || prompt.busy || radial.open) return;
  const target = wall?.wall ? wall : focusObject?.owner === owner && !focusObject?.wall ? focusObject : null;
  releaseInput();
  document.exitPointerLock();
  canvas.focus();
  prompt.text = '';
  prompt.begin(target);
  radial.begin(target);
  sfx.ringOpen();
}
function closeBuild() {
  prompt.open = false;
  radial.close();
  releaseInput();
}
function pickRadial(index) {
  if (!radial.pick(index)) return;
  // Chalk category: erase, or step out of the ring to draw the shape.
  if (radial.chalk && radial.picks.length === 1) {
    const thing = radial.picks[0];
    if (thing === 'erase my chalk') send({ type: 'chalk-clear' });
    else {
      chalkMode = { text: thing, target: radial.target };
      send({ type: 'chalk-clear' });
    }
    sfx.ringPick(index);
    closeBuild();
    return;
  }
  prompt.text = radial.text();
  prompt.cursor = prompt.text.length;
  prompt.message = '';
  sfx.ringPick(index);
}
// B or a tap outside the ring: undo the last choice, then close.
function backRadial() {
  sfx.ringBack();
  if (radial.back()) {
    prompt.text = radial.text();
    prompt.cursor = prompt.text.length;
  } else closeBuild();
}
function touchRing() {
  const width = canvas.clientWidth,
    height = canvas.clientHeight;
  const { cy, r } = ringGeometry(width, height);
  return { cx: width / 2, cy: height / 2 - cy, r };
}
const touchy = (e) => e.pointerType === 'touch' || e.pointerType === 'pen';
// 📱 The phone's buttons (touch.mjs) press the keys: one path for every
// action, and a held button is a held key. A hidden field raises the phone's
// keyboard for the build prompt and chat; what it types arrives as keys too.
const synthetic = new WeakSet();
function pressKey(type, code, key) {
  const e = new KeyboardEvent(type, { code, key, bubbles: true, cancelable: true });
  synthetic.add(e);
  document.dispatchEvent(e);
}
const typing = () => prompt.open || chat.open;
const touchMode = () => (seat < 0 || radial.open || chalkMode || menu ? null : typing() ? 'typing' : 'play');
const shownButtons = () => (touch.active ? touchButtons(touchMode(), canvas.clientWidth, canvas.clientHeight) : []);
const softKeys = Object.assign(document.createElement('input'), {
  type: 'text', autocomplete: 'off', autocapitalize: 'off', spellcheck: false, enterKeyHint: 'send', tabIndex: -1,
});
softKeys.setAttribute('autocorrect', 'off');
softKeys.setAttribute('aria-hidden', 'true');
// Sixteen pixels so iOS never zooms to it; unseen and untouchable.
softKeys.style.cssText = 'position:fixed;left:0;bottom:0;width:1px;height:1px;opacity:0;font-size:16px;border:0;padding:0;pointer-events:none;';
document.body.append(softKeys);
const SOFT_FILL = '  '; // Something for backspace to delete.
const syncSoftKeys = () => {
  softKeys.value = SOFT_FILL;
  if (!typing() && document.activeElement === softKeys) softKeys.blur();
};
softKeys.addEventListener('keydown', (e) => {
  e.stopPropagation(); // Letters and backspace come by `input`, where every phone agrees.
  if (['Enter', 'Escape', 'ArrowLeft', 'ArrowRight', 'Home', 'End'].includes(e.key)) {
    e.preventDefault();
    pressKey('keydown', e.key, e.key);
    syncSoftKeys();
  }
});
softKeys.addEventListener('keyup', (e) => e.stopPropagation());
softKeys.addEventListener('input', (e) => {
  if (e.inputType === 'deleteContentBackward') pressKey('keydown', 'Backspace', 'Backspace');
  else if (e.inputType === 'insertLineBreak') pressKey('keydown', 'Enter', 'Enter');
  else for (const ch of e.data || '') pressKey('keydown', '', ch);
  syncSoftKeys();
});
function touchButton(button, isDown) {
  if (!button) return;
  pressKey(isDown ? 'keydown' : 'keyup', button.code, button.key);
  if (isDown) return;
  // On the lift, inside the gesture, so the phone lets its keyboard up.
  if (typing()) {
    softKeys.value = SOFT_FILL;
    softKeys.focus({ preventScroll: true });
  } else syncSoftKeys();
}
canvas.addEventListener('pointerdown', (e) => {
  if (!touchy(e)) return;
  e.preventDefault();
  if (view.cornerHit(e.offsetX, e.offsetY)) return leave();
  // An open menu takes the tap: a row is chosen, anywhere off the panel closes it.
  if (menu) {
    const row = view.menuHit(e.offsetX, e.offsetY);
    if (row === null) closeMenu();
    else if (row <= -10) turnMenu(0, -10 - row); // a tab
    else if (row >= 0) {
      menu.selected = row;
      chooseMenu();
    }
    return;
  }
  canvas.setPointerCapture?.(e.pointerId);
  const first = !touch.active;
  touchButton(touch.down(e, canvas.clientWidth, canvas.clientHeight, performance.now(), radial.open, shownButtons()), true);
  if (first) void capture();
});
canvas.addEventListener('pointermove', (e) => {
  if (touchy(e)) touch.move(e, canvas.clientWidth, canvas.clientHeight);
});
canvas.addEventListener('pointerup', (e) => {
  if (touchy(e)) touchButton(touch.up(e, performance.now()), false);
});
canvas.addEventListener('pointercancel', (e) => {
  if (!touchy(e)) return;
  const held = touch.cancel(e);
  if (held) pressKey('keyup', held.code, held.key);
});
function releaseInput() {
  keys.clear();
  firing = jumpQueued = fireQueued = false;
}
document.addEventListener('pointerlockchange', () => {
  releaseInput();
  if (!locked()) unlockedAt = performance.now();
});
document.addEventListener('mousemove', (e) => {
  if (!locked()) {
    // A free mouse: remember where it is; the aim ray goes through it.
    const r = canvas.getBoundingClientRect();
    mouseAt = { x: e.clientX - r.left, y: e.clientY - r.top, at: performance.now() };
    return;
  }
  if (prompt.open) return;
  yaw = (yaw - e.movementX * 0.12 + 360) % 360;
  pitch = clamp(pitch - e.movementY * 0.12, -85, 85);
});
document.addEventListener('keydown', (e) => {
  void sfx.wake();
  // ⌘+ / ⌘− / ⌘0 under the prompt: its pixel density is ours (we lay our
  // pixels on its grid), so hand them up and the new grid comes back down.
  if (hosted && (e.metaKey || e.ctrlKey) && !e.altKey && ['=', '+', '-', '0'].includes(e.key)) {
    e.preventDefault();
    toHost({ type: 'dool:sheet:density', step: e.key === '0' ? 0 : e.key === '-' ? -1 : 1 });
    return;
  }
  if (down && e.code !== 'Escape') {
    e.preventDefault();
    return;
  }
  // 💬 Chat takes the keys while its line is open.
  if (chat.open) {
    if (e.code === 'Enter' && !e.repeat) {
      const text = chat.text.trim();
      if (text) send({ type: 'say', text });
      chat.text = '';
      chat.cursor = 0;
      chat.open = false;
    } else {
      chat.key(e);
      chat.text = chat.text.slice(0, CHAT_MAX);
      chat.cursor = Math.min(chat.cursor, chat.text.length);
    }
    if (!(e.metaKey || e.ctrlKey)) e.preventDefault();
    releaseInput();
    return;
  }
  if (e.code === 'KeyT' && !e.repeat && !e.metaKey && !e.ctrlKey && !e.altKey && seat >= 0 &&
    !prompt.open && !menu && !radial.open && !chalkMode) {
    e.preventDefault();
    releaseInput();
    chat.begin();
    return;
  }
  // Tab: back to the prompt, as Tab there comes in (2026-11-10). $ (shift
  // 4): the one menu — travel, the $code list, the view's switches — as the
  // prompt's $ starts a code; not while typing, where $ is a letter
  // (2026-11-08). The room menu on R and ` is retired (2026-11-13).
  if (e.code === 'Tab') {
    e.preventDefault();
    if (!e.repeat && !prompt.open) leave();
    return;
  }
  const dollar = e.key === '$' && !prompt.open;
  if (!e.repeat && dollar) {
    e.preventDefault();
    toggleMenu();
    return;
  }
  if (menu) {
    e.preventDefault();
    if (e.code === 'ArrowDown' || e.code === 'KeyS') menu.selected = (menu.selected + 1) % menu.items.length;
    if (e.code === 'ArrowUp' || e.code === 'KeyW') menu.selected = (menu.selected - 1 + menu.items.length) % menu.items.length;
    if (!e.repeat && (e.code === 'ArrowLeft' || e.code === 'KeyA')) turnMenu(-1);
    if (!e.repeat && (e.code === 'ArrowRight' || e.code === 'KeyD')) turnMenu(1);
    if (e.code === 'Enter') chooseMenu();
    if (e.code === 'Escape') closeMenu();
    return;
  }
  if (e.code === 'Escape' && radial.open && !e.repeat) {
    e.preventDefault();
    closeBuild();
    return;
  }
  if (e.code === 'Escape' && prompt.busy && !e.repeat) {
    e.preventDefault();
    send({ type: 'cancel-build' });
    prompt.open = false;
    releaseInput();
    return;
  }
  if (chalkMode && !e.repeat && (e.code === 'Enter' || e.code === 'Escape')) {
    e.preventDefault();
    if (e.code === 'Enter') submitChalk();
    else chalkMode = null;
    return;
  }
  // Hosted, Escape first frees the mouse (the browser usually keeps that
  // press for itself); then, with nothing open and the mouse already free,
  // Escape brings the prompt's paper back.
  // Hosted, one Escape with nothing open does both (2026-11-03): it frees
  // the mouse and brings the prompt's paper back. (A browser that eats the
  // first Escape to end its own pointer lock still needs a second press.)
  if (hosted && e.code === 'Escape' && !e.repeat && !prompt.open) {
    e.preventDefault();
    if (locked()) document.exitPointerLock();
    unlockedAt = performance.now();
    leave();
    return;
  }
  if (e.code === 'Enter' && !e.repeat) {
    e.preventDefault();
    if (seat < 0) return;
    if (!prompt.open) {
      releaseInput();
      document.exitPointerLock();
      canvas.focus();
      prompt.begin(focusObject?.owner === owner ? focusObject : null);
    } else submitPrompt();
    return;
  }
  if (prompt.open && radial.open && e.key.length === 1) radial.close();
  if (prompt.key(e)) {
    if (!prompt.open) radial.close();
    if (!(e.metaKey || e.ctrlKey)) e.preventDefault();
    releaseInput();
    return;
  }
  if (!locked() && !keyboardMode && !synthetic.has(e)) {
    if (touch.active || !CONTROL_KEYS.includes(e.code)) return;
    keyboardMode = true;
    if (!ws || ws.readyState === WebSocket.CLOSED) connect();
    notice = '';
  }
  if (CONTROL_KEYS.includes(e.code)) e.preventDefault();
  keys.add(e.code);
  if (seat >= 0 && e.code === 'Space' && !e.repeat) {
    jumpQueued = true;
    sfx.jump();
  }
  if (seat >= 0 && e.code === 'KeyV' && !e.repeat) third = !third;
  if (seat >= 0 && e.code === 'KeyF' && !e.repeat) {
    if (focusObject?.wall && !holding()) openRadial(focusObject);
    else useQueued = true;
  }
  if (seat >= 0 && e.code === 'KeyQ' && !e.repeat) dropQueued = true;
  // Lock on, as the pad's LB does (2026-11-08): Z toggles it, [ and ]
  // change it like the d-pad, and delete removes the locked object at once
  // (the lock is the confirmation, as B on the pad).
  if (seat >= 0 && e.code === 'KeyZ' && !e.repeat) {
    setLock(lockOn ? null : focusObject?.id || null);
    sfx[lockOn ? 'ringOpen' : 'ringBack']();
  }
  if (lockOn && !e.repeat && (e.code === 'BracketLeft' || e.code === 'BracketRight')) cycleLock(e.code === 'BracketRight' ? 1 : -1);
  if (seat >= 0 && (e.code === 'Delete' || e.code === 'Backspace') && !e.repeat) {
    if (lockOn && !lockOn.wall) {
      send({ type: 'remove', objectId: lockOn.id });
      setLock(null);
      sfx.ringBack();
    } else if (focusObject && !focusObject.wall) removeAimed();
  }
  if (seat >= 0 && e.code === 'KeyX' && !e.repeat && !holding() && !chalkMode) paintSquare();
});
// Paint the floor square the aim ray lands on (the highlighted one), never
// the one underfoot (2026-11-03).
// A press steps the square to its next colour and loads the brush with it;
// held, the brush paints every square the aim crosses (2026-11-10), so a
// press picks the colour and a drag lays it down.
let brush = null; // { colour, key }: the colour in hand and the last square it touched.
function paintSquare() {
  const at = floorAim && !focusObject ? floorAim : null;
  if (!at) return;
  const key = `${at.i},${at.j}`;
  brush = { colour: ((tiles[key] || 0) + 1) % 7, key };
  layTile(at, brush.colour);
}
function layTile(at, colour) {
  const key = `${at.i},${at.j}`;
  // Shown at once; the server's next word on the floor confirms it.
  tiles = { ...tiles };
  if (colour) tiles[key] = colour;
  else delete tiles[key];
  send({ type: 'paint', i: at.i, j: at.j, c: colour });
  sfx.ringPick(at.i + at.j);
}
function dragBrush() {
  if (!brush) return;
  if (!keys.has('KeyX') || seat < 0 || chalkMode) return void (brush = null);
  const at = floorAim && !focusObject ? floorAim : null;
  if (!at) return;
  const key = `${at.i},${at.j}`;
  if (key === brush.key) return;
  brush.key = key;
  if ((tiles[key] || 0) !== brush.colour) layTile(at, brush.colour);
}
document.addEventListener('keyup', (e) => keys.delete(e.code));
document.addEventListener('mousedown', (e) => {
  if (seat >= 0 && locked() && e.button === 0 && holding()) {
    e.preventDefault();
    useQueued = true;
    return;
  }
  // A click no longer opens the build prompt (jeffrey, 2026-11-08): Enter,
  // or the pad's build button, does.
});
document.addEventListener('mouseup', (e) => {
  firing = false;
  if (e.button === 2) chalkMouse = false;
});
document.addEventListener('mousedown', (e) => {
  if (e.button === 2 && locked()) chalkMouse = true;
});
canvas.addEventListener('contextmenu', (e) => e.preventDefault());
// 🎥 In third person the wheel (or two fingers on a trackpad) moves the camera
// in and out along its boom: up is closer. The distance is kept for next time.
const BOOM_MIN = 1.5, BOOM_MAX = 14;
let boomLength = 4;
try {
  const kept = parseFloat(localStorage.getItem('dool-boom'));
  if (Number.isFinite(kept)) boomLength = Math.min(BOOM_MAX, Math.max(BOOM_MIN, kept));
} catch {}
let boomSaveTimer = 0;
addEventListener('wheel', (e) => {
  if (!sheetOpen) return;
  e.preventDefault(); // The page never scrolls or zooms under the world.
  if (seat < 0 || !third || typing() || menu) return;
  const lines = e.deltaMode === 1 ? e.deltaY * 16 : e.deltaY;
  boomLength = Math.min(BOOM_MAX, Math.max(BOOM_MIN, boomLength * Math.exp(Math.max(-120, Math.min(120, lines)) * 0.004)));
  clearTimeout(boomSaveTimer);
  boomSaveTimer = setTimeout(() => { try { localStorage.setItem('dool-boom', boomLength.toFixed(2)); } catch {} }, 400);
}, { passive: false });
// A held thumb is a control, never a selection: iOS starts its loupe and
// selection from touchstart whatever the CSS says, so the touch is refused
// there (pointer events still arrive) and nothing can be selected (2026-11-10).
canvas.addEventListener('touchstart', (e) => { e.preventDefault(); if (!typing()) canvas.focus({ preventScroll: true }); }, { passive: false });
for (const type of ['selectstart', 'dragstart', 'gesturestart'])
  document.addEventListener(type, (e) => e.preventDefault());
window.addEventListener('blur', releaseInput);
document.addEventListener('visibilitychange', () => {
  diagnostics.reset();
  releaseInput();
  debt = 0;
  last = performance.now();
  presentation.reset(player);
});

// Behind the curtain the world is a backdrop under a veil: it steps at
// 20 fps there (2026-11-03) so the prompt's own drawing keeps the frame
// budget, and comes back to full rate the moment the paper drops.
let bareAt = 0, sheetClosedAt = 0, sheetShown = !hosted, closingAt = 0;
// The far, turning view behind the curtain: 1 there, easing to 0 over 1.4 s
// once the paper drops, and back out as it returns.
let orbitMix = hosted ? 1 : 0, orbitAt = performance.now();
function orbitStep(now) {
  const want = hosted && (!sheetOpen || closingAt) ? 1 : 0;
  const dt = Math.min(0.1, (now - orbitAt) / 1000);
  orbitAt = now;
  const rate = want ? 1 / 0.36 : 1 / 1.4; // back out on the paper's own beat
  orbitMix = want > orbitMix ? Math.min(want, orbitMix + rate * dt) : Math.max(want, orbitMix - rate * dt);
  return orbitMix;
}
// On the way home the reticle flies to the prompt's crosshair corner on the
// prompt's own path and clock (prompt.mjs doolFlight "back": 360 ms, ease-out,
// to 14 px in from the bottom-right), in buffer pixels from the centre, y up.
function reticleShift(now) {
  if (!closingAt) return null;
  const t = Math.min(1, (now - closingAt) / 360), ease = 1 - (1 - t) * (1 - t);
  const px = canvas.width / Math.max(1, canvas.clientWidth);
  const hx = canvas.width / 2 - 22 * px, hy = -(canvas.height / 2 - 22 * px);
  return { x: Math.round(hx * ease), y: Math.round(hy * ease) };
}
// 🧊 Stalls (stalls.mjs): gaps in the picture and what filled them, reported
// as numbers to /dool/stalls and shown beside the fps readout.
const AGENT = window.webkit?.messageHandlers?.iOSApp || window.parent?.webkit?.messageHandlers?.iOSApp ? 'app'
  : /firefox/i.test(navigator.userAgent) ? 'firefox'
  : /chrome|chromium|crios/i.test(navigator.userAgent) ? 'chrome'
  : /safari/i.test(navigator.userAgent) ? 'safari' : 'other';
const stalls = new Stalls({ send: (m) => send(m) });
const corrections = { count: 0, sum: 0, max: 0 };
function applyPrediction() {
  if (!prediction) return;
  const began = performance.now(), data = prediction, own = data.players[seat];
  prediction = null;
  const changedLife = !player || own.life !== player.life;
  const result = reconcile(own, pending, data.acks[seat], solids(), data.tick);
  // How far the server's word moved us from where we thought we were: zero
  // when prediction holds (?debug reads it).
  if (player && !changedLife) {
    const d = Math.hypot(result.player.x - player.x, result.player.y - player.y, result.player.z - player.z);
    if (d > 0.002) {
      corrections.count++;
      corrections.sum += d;
      corrections.max = Math.max(corrections.max, d);
    }
  }
  pending = result.pending;
  player = data.phase === 'playing' || data.phase === 'waiting' ? result.player : { ...own };
  presentation.correct(player, changedLife || resetPrediction);
  resetPrediction = false;
  if (changedLife) { yaw = own.yaw; pitch = own.pitch; }
  diagnostics.max('reconcileMaxMs', performance.now() - began);
  diagnostics.max('pendingCommandsMax', pending.length);
}
function frame(now) {
  // Schedule first: one bad frame must never stop the loop.
  requestAnimationFrame(frame);
  // 🧊 A gap since the last frame, while someone is in the room looking.
  stalls.begin(now, !document.hidden && document.hasFocus() && (hosted ? sheetShown : true) && !resting, {
    building: jobs.length, objects: objects.length, pending: pending.length,
    rtt: Number.isFinite(connection.rtt) ? connection.rtt : undefined,
    backend: canvas.doolStats?.backend, agent: AGENT, hosted,
  });
  // With the host window in the background and the paper down, the room
  // still turns smoothly but draws at most 60 times a second, half the work
  // on a 120 Hz display (2026-11-08).
  if (resting && !sheetOpen) {
    if (now - lastBackgroundFrameAt < 1000 / BACKGROUND_FPS - 2) return;
    lastBackgroundFrameAt = now;
  }
  // Full rate for a beat after the paper is back, so the return never stutters.
  // (The corner world runs at full rate: stepping it at 20 fps made the
  // turning room's edges flicker, 2026-11-03.)
  const began = performance.now();
  let predicted = 0;
  try {
    applyPrediction();
    predicted = performance.now() - began;
    step(now);
  } catch (error) {
    if (diagnostics.enabled) diagnostics.values.frameErrors++;
    console.error('dool frame', error);
  }
  stalls.end({ work: performance.now() - began, render: canvas.doolStats?.cpuMs || 0, predict: predicted });
  const gpu = canvas.doolStats?.gpu;
  if (gpu?.max) {
    stalls.gpu(gpu.max, gpu.queued);
    gpu.max = 0;
  }
  if (diagnostics.enabled) diagnostics.frame({ visible: !document.hidden && sheetOpen,
    workMs: performance.now() - began, renderMs: canvas.doolStats?.cpuMs,
    backend: canvas.doolStats?.backend, objects: objects.length, triangles: canvas.doolStats?.triangles,
    rttMs: connection.rtt, jitterMs: connection.jitter, queuedBytes: ws?.bufferedAmount || 0,
    buildIds: jobs.map((j) => j.id) });
}
function step(now) {
  const elapsed = Math.max(0, (now - last) / 1000);
  const pads = navigator.getGamepads?.();
  // A controller should just work: if it's touched while the game is in the
  // background, ask the rig to bring the game to the front.
  if (!document.hasFocus() && window.doolRig?.claimFocus && now - claimedAt > 1000) {
    const touched = Array.from(pads || []).some(
      (p) => p?.connected && (p.buttons.some((b) => b.pressed) || p.axes.some((a) => Math.abs(a) > 0.5)),
    );
    if (touched) {
      claimedAt = now;
      window.doolRig.claimFocus();
    }
  }
  // Down (socket dead or the server silent): every menu closes, the pad is
  // ignored and the HUD rests; only the way back to the paper stays open.
  down = netDown(now);
  if (down) {
    if (menu) closeMenu();
    if (radial.open) radial.close();
    if (prompt.open) prompt.open = false;
    chat.open = false;
    chalkMode = null;
    releaseInput();
  }
  gamepad = controller.sample(down ? [] : pads, document.hasFocus());
  // Walking through chalk dusts it: the points within reach fall as motes
  // here at once, and the server cuts the strokes for everyone.
  if (player && seat >= 0 && now - dustedAt > 120 && Math.hypot(player.vx || 0, player.vz || 0) > 0.5) {
    let fell = 0;
    chalkStrokes.forEach((strokes, s) => {
      (strokes || []).forEach((stroke) => {
        for (const p of stroke) {
          if (p.y < 2.2 && Math.hypot(p.x - player.x, p.z - player.z) < 0.5) {
            if (fell++ < 6) view.effect({ type: 'dust', at: p });
          }
        }
      });
    });
    if (fell) {
      dustedAt = now;
      send({ type: 'chalk-dust' });
    }
  }
  if (gamepad.ring || gamepad.select || gamepad.jump || gamepad.prompt || gamepad.cancel || gamepad.start) void sfx.wake();
  dragBrush();
  fpsFrames++;
  if (now - fpsAt >= 500) {
    fps = Math.round((fpsFrames * 1000) / (now - fpsAt));
    fpsFrames = 0;
    fpsAt = now;
  }
  if (gamepad.start || gamepad.swap) toggleMenu(); // START, with BACK as before
  if (menu) {
    // Move with the d-pad or the stick, choose with A, close with B.
    let step = (gamepad.ddown ? 1 : 0) - (gamepad.dup ? 1 : 0);
    if (!step && Math.abs(gamepad.fwd) > 0.6 && now - menuStickAt > 220) {
      step = gamepad.fwd > 0 ? -1 : 1;
      menuStickAt = now;
    }
    if (step) {
      menu.selected = (menu.selected + step + menu.items.length) % menu.items.length;
      sfx.ringMove(menu.selected);
    }
    // Tabs: d-pad left and right, the bumpers, or the stick.
    let turn = (gamepad.dright || gamepad.use ? 1 : 0) - (gamepad.dleft || gamepad.ring ? 1 : 0);
    if (!turn && !step && Math.abs(gamepad.right) > 0.6 && now - menuStickAt > 260) {
      turn = gamepad.right > 0 ? 1 : -1;
      menuStickAt = now;
    }
    if (turn) turnMenu(turn);
    if (gamepad.select) chooseMenu();
    if (gamepad.cancel) closeMenu();
    gamepad = { ...gamepad, select: false, cancel: false, jump: false, use: false, ring: false, prompt: false, fwd: 0, right: 0, lookX: 0, lookY: 0 };
  }
  const pad = touch.sample(canvas.clientWidth, canvas.clientHeight, now);
  for (const event of pad.events) {
    if (event.type === 'hold' && !prompt.open) openRadial();
    else if (event.type === 'tap' && radial.open && event.menu) {
      const ring = touchRing();
      const x = (event.x - ring.cx) / ring.r,
        y = (ring.cy - event.y) / ring.r,
        d = Math.hypot(x, y);
      if (d < 0.4) {
        if (!submitPrompt(false)) sfx.refuse();
      } else if (d < 1.6) {
        radial.point(x, y, 0);
        pickRadial(radial.selected);
      } else backRadial();
    } else if (event.type === 'tap' && !prompt.open && seat >= 0) {
      jumpQueued = true;
      sfx.jump();
    }
  }
  // LB toggles a lock on the aimed object; while locked, B removes it (the
  // lock is the confirmation) and RB changes it.
  if (gamepad.ring && seat >= 0 && !prompt.open) {
    setLock(lockOn ? null : focusObject?.id || null);
    sfx[lockOn ? 'ringOpen' : 'ringBack']();
  }
  if (lockOn && !menu && (gamepad.dleft || gamepad.dright)) cycleLock(gamepad.dright ? 1 : -1);
  if (lockOn && gamepad.cancel && !prompt.open && !radial.open && !lockOn.wall) {
    send({ type: 'remove', objectId: lockOn.id });
    setLock(null);
    gamepad.cancel = false;
    sfx.ringBack();
  }
  if (radial.open) {
    const left = Math.hypot(gamepad.right, gamepad.fwd),
      right = Math.hypot(gamepad.lookX, gamepad.lookY);
    if (controller.id) {
      const before = radial.selected;
      if (left >= right) radial.point(gamepad.right, gamepad.fwd);
      else radial.point(gamepad.lookX, -gamepad.lookY);
      if (radial.selected !== before && radial.selected >= 0) sfx.ringMove(radial.selected);
    }
    if (gamepad.select && radial.selected >= 0) pickRadial(radial.selected);
    if (gamepad.prompt && !submitPrompt(false)) sfx.refuse();
    if (gamepad.cancel) backRadial();
    gamepad.jump = gamepad.select = gamepad.prompt = gamepad.cancel = false;
  }
  if (gamepad.cancel) {
    // B cancels your build while its square or object is highlighted.
    const highlighted = jobs.some((j) => j.owner === owner && ((focusObject && focusObject.id === j.objectId) || (floorAim && Math.hypot(-18 + floorAim.i * 2 + 1 - j.x, -18 + floorAim.j * 2 + 1 - j.z) < 2.5)));
    if (seat >= 0 && (prompt.busy || highlighted)) send({ type: 'cancel-build' });
    closeBuild();
    document.exitPointerLock();
  }
  if (chalkMode) {
    if (gamepad.prompt) {
      submitChalk();
      gamepad.prompt = false;
    }
    if (gamepad.cancel) {
      chalkMode = null;
      gamepad.cancel = false;
      sfx.ringBack();
    }
  }
  // RB adds: with nothing held it builds (or changes what you aim at, or
  // rewrites a KidLisp wall); with an item in hand it uses the item.
  if (gamepad.use && seat >= 0 && !prompt.open && !holding()) {
    openRadial(focusObject?.wall ? focusObject : null);
    gamepad.use = false;
  }
  if (gamepad.use && seat >= 0 && !prompt.open) useQueued = true;
  // X paints (as the legend says); RB builds. With an item in hand X drops it.
  if (gamepad.prompt && seat >= 0 && !prompt.open && holding()) {
    dropQueued = true;
    gamepad.prompt = false;
  }
  if (gamepad.prompt && seat >= 0 && !prompt.open && !chalkMode) {
    paintSquare();
    gamepad.prompt = false;
  }
  if (!prompt.open) {
    yaw = (yaw - pad.dx * 0.25 + 360) % 360;
    pitch = clamp(pitch - pad.dy * 0.25, -85, 85);
    yaw = (yaw - gamepad.lookX * 160 * Math.min(0.1, elapsed) + 360) % 360;
    pitch = clamp(
      pitch - gamepad.lookY * 120 * Math.min(0.1, elapsed),
      -85,
      85,
    );
    // Arrow keys turn and tilt at a steady rate for mouse-free play.
    const turnX = Number(keys.has('ArrowRight')) - Number(keys.has('ArrowLeft')),
      turnY = Number(keys.has('ArrowDown')) - Number(keys.has('ArrowUp'));
    yaw = (yaw - turnX * 130 * Math.min(0.1, elapsed) + 360) % 360;
    pitch = clamp(pitch - turnY * 90 * Math.min(0.1, elapsed), -85, 85);
    if (seat >= 0 && gamepad.jump) {
      jumpQueued = true;
      sfx.jump();
    }
    if (seat >= 0 && gamepad.camera) third = !third;
  }
  if (lockOn && player) {
    const target = spotOf(lockOn.id);
    if (!target) setLock(null);
    else {
      const dx = target.x - player.x, dz = target.z - player.z, dy = target.y - player.y;
      const wantYaw = (Math.atan2(dx, dz) * 180) / Math.PI;
      const wantPitch = (Math.atan2(dy, Math.hypot(dx, dz)) * 180) / Math.PI;
      const dyaw = ((wantYaw - yaw + 540) % 360) - 180;
      const k = 1 - Math.exp(-elapsed * 8);
      yaw = (yaw + dyaw * k + 360) % 360;
      pitch = clamp(pitch + (wantPitch - pitch) * k, -85, 85);
    }
  }
  if (elapsed > 0.25) presentation.reset(player);
  debt += Math.min(0.1, elapsed);
  last = now;
  if (seat >= 0 && (keys.size || gamepad.fwd || gamepad.right || gamepad.lookX || gamepad.lookY || pad.fwd || pad.right || pad.dx || pad.dy))
    activeSeconds += Math.min(0.1, elapsed);
  const connected = ws?.readyState === WebSocket.OPEN && snapshot;
  if (seat < 0 && connected && !joining())
    freecam = moveFreecam(
      { ...freecam, yaw, pitch },
      {
        fwd: clamp(
          Number(keys.has('KeyW')) - Number(keys.has('KeyS')) + gamepad.fwd + pad.fwd,
          -1,
          1,
        ),
        right: clamp(
          Number(keys.has('KeyD')) - Number(keys.has('KeyA')) + gamepad.right + pad.right,
          -1,
          1,
        ),
        up: clamp(
          Number(keys.has('Space')) -
            Number(keys.has('ShiftLeft') || keys.has('ShiftRight')) +
            gamepad.up,
          -1,
          1,
        ),
        boost: gamepad.boost,
      },
      elapsed,
    );
  const probe = connection.probe(
    performance.now(),
    ws?.readyState === WebSocket.OPEN && ws.bufferedAmount < 2048,
  );
  if (probe) send(probe);
  const firstSequence = seq;
  while (debt >= DT) {
    debt -= DT;
    if (
      !connected ||
      seat < 0 ||
      !player ||
      pending.length >= 120 ||
      now - latestAt > 1000
    ) {
      if (player) presentation.step(player);
      continue;
    }
    const c = {
      seq: ++seq,
      yaw,
      pitch,
      fwd: clamp(
        Number(keys.has('KeyW')) -
          Number(keys.has('KeyS')) +
          (prompt.open ? 0 : gamepad.fwd + pad.fwd),
        -1,
        1,
      ),
      right: clamp(
        Number(keys.has('KeyD')) -
          Number(keys.has('KeyA')) +
          (prompt.open ? 0 : gamepad.right + pad.right),
        -1,
        1,
      ),
      buttons: keys.has('Space') || jumpQueued ? 1 : 0,
      use: useQueued,
      drop: dropQueued,
      chalk: (() => {
        const at = chalkPoint();
        if (at) chalkLocal.push(at);
        else chalkLocal = [];
        return at;
      })(),
      fire: (firing || fireQueued) && locked(),
    };
    jumpQueued = fireQueued = useQueued = dropQueued = false;
    pending.push(c);
    if (snapshot.phase === 'playing' || snapshot.phase === 'waiting')
      player = move(player, c, solids(), (snapshot.tick + pending.length) / HZ);
    presentation.step(player);
  }
  // One newest batch per display frame: catch-up steps don't flood a slow link.
  if (seq !== firstSequence)
    send({ type: 'input', commands: pending.slice(-12) }); // match.mjs INPUT_BATCH
  const seconds = snapshot
    ? Math.max(1, Math.ceil((snapshot.until - snapshot.tick) / HZ))
    : 0;
  const status = !connected
    ? notice
    : now - latestAt > 1000
      ? 'CONNECTION STALLED'
      : notice ||
        (snapshot.phase === 'countdown'
          ? `${seconds}`
          : snapshot.phase === 'finished'
            ? `${snapshot.winner === seat ? 'YOU WIN' : 'OPPONENT WINS'} · ${seconds}`
            : player && !player.alive
              ? 'RESPAWNING'
              : !engaged() && !touch.active && !controller.id
                ? 'CLICK OR PRESS A KEY TO PLAY'
                : snapshot.phase === 'waiting'
                  ? 'WAITING FOR OPPONENT'
                  : '');
  // Animate objects on the smoothed playback clock, not the raw snapshot tick:
  // snapshots land unevenly, and snapping to each one made motion stutter.
  const tick = playback.sample(now);
  const worldTime = tick / HZ;
  focusObject = null;
  // Off (2026-11-03): the aim is the crosshair's normal, with pointer lock for
  // mouse look; a free mouse does not drag the aim around.
  cursorAim = CURSOR_AIM && !locked() && !!mouseAt && now - mouseAt.at < 6000 && !touch.active && !prompt.open;
  if (player) {
    // The aim ray: from the camera, through the free mouse or along the
    // camera's own centre (the crosshair's normal), never from the player's
    // position with the raw yaw and pitch.
    const ray = cursorAim ? view.aimRay(mouseAt.x, mouseAt.y) : view.aimForward();
    const origin = ray ? view.aimOrigin() || player : player;
    const dir = ray || direction(yaw, pitch);
    let nearest = wallDistance(origin, dir, 60);
    for (const box of objectBoxes(solids(), worldTime)) {
      const distance = rayBox(origin, dir, box, nearest);
      if (distance < nearest) {
        nearest = distance;
        focusObject = objects.find((o) => o.id === box.objectId);
      }
    }
    // Items can be aimed at (and so edited) where they really are: loose on
    // the floor, or in your own hand.
    for (const it of snapshot?.items || []) {
      if (it.holder >= 0 && it.holder !== seat) continue;
      const r = 0.6;
      const box = { xMin: it.x - r, xMax: it.x + r, yMin: it.y - 0.1, yMax: it.y + 1.2, zMin: it.z - r, zMax: it.z + r };
      const distance = it.holder === seat ? 0.5 : rayBox(origin, dir, box, nearest);
      if (distance < nearest && it.holder !== seat) {
        nearest = distance;
        focusObject = objects.find((o) => o.id === it.id);
      }
    }
    // The KidLisp walls are objects too: aim at one to see its code.
    for (const w of walls) {
      const along = w.face === 'north' ? dir.z : -dir.x;
      if (along <= 0.01) continue;
      const t = w.face === 'north' ? (SCREEN - 0.03 - origin.z) / dir.z : (-SCREEN + 0.03 - origin.x) / dir.x;
      if (!(t > 0) || t >= nearest) continue;
      const hit = { x: origin.x + dir.x * t, y: origin.y + dir.y * t, z: origin.z + dir.z * t };
      const u = w.face === 'north' ? hit.x : hit.z;
      if (Math.abs(u) <= 16 && hit.y >= 1.5 && hit.y <= 1.5 + 18) {
        nearest = t;
        focusObject = { id: `wall:${w.code}`, wall: true, code: w.code, source: w.source, colored: w.colored, size: `${w.width}x${w.height}`, version: 0, owner: null, geometry: { name: `$${w.code}` } };
      }
    }
    // The floor square under the crosshair, within reach, if nothing nearer.
    floorAim = null;
    if (dir.y < -0.02) {
      const t = -origin.y / dir.y;
      // The floor is one of the walls the ray can meet: when nothing stands
      // before it, the two distances are the same, so the floor must not
      // lose to itself (2026-11-03: this is why no square ever lit).
      if (t > 0 && t < Math.min(60, nearest + 0.05)) {
        const x = origin.x + dir.x * t, z = origin.z + dir.z * t;
        const i = Math.floor((x + 18) / 2), j = Math.floor((z + 18) / 2);
        if (i >= 0 && i < 18 && j >= 0 && j < 18) floorAim = { i, j };
      }
    }
    if (lockOn) {
      const locked = objects.find((o) => o.id === lockOn.id);
      const w = walls.find((x) => `wall:${x.code}` === lockOn.id);
      if (locked) focusObject = locked;
      else if (w)
        focusObject = { id: `wall:${w.code}`, wall: true, code: w.code, source: w.source, colored: w.colored, size: `${w.width}x${w.height}`, version: 0, owner: null, geometry: { name: `$${w.code}` } };
    }
    // Holding something with nothing else aimed at: the held item is the one to edit.
    if (!focusObject) {
      const mine = snapshot?.items?.find((it) => it.holder === seat);
      if (mine) focusObject = objects.find((o) => o.id === mine.id) || null;
    }
  }
  const displayed = presentation.sample(debt / DT, elapsed);
  // The listener is wherever the camera is, facing where it looks.
  {
    const at = seat < 0 ? freecam : displayed || player;
    if (at) {
      const f = direction(yaw, pitch);
      const beat = beatAt(Date.now() + (clockOffset || 0));
      tones.update(toneSources(), { x: at.x, y: at.y, z: at.z, fx: f.x, fy: f.y, fz: f.z }, worldTime, beat);
    }
  }
  view.render({
    // Every seat the room holds, not only the original duel's two.
    players: connected ? Array.from({ length: snapshot?.players?.length || 0 }, (_, i) => interpolate(history, tick, i)) : [],
    me: seat < 0 ? null : player ? { ...displayed, yaw, pitch } : spawn(seat),
    camera: seat < 0 ? { ...freecam, yaw, pitch } : null,
    spectator: seat < 0 && !joining(),
    controller: controller.id,
    seat,
    third,
    boomLength,
    rockets: snapshot?.rockets || [],
    items: snapshot?.items || [],
    bullets: snapshot?.bullets || [],
    since: (now - latestAt) / 1000,
    holding: seat >= 0 ? objects.find((o) => snapshot?.items?.some((it) => it.id === o.id && it.holder === seat)) || null : null,
    status: sheetOpen ? status : '',
    members: sheetOpen ? members : [],
    self: sheetOpen ? selfMember : null,
    joinURL,
    connection: sheetOpen
      ? connection.state(now, ws?.readyState === WebSocket.OPEN, ws?.bufferedAmount || 0)
      : null,
    objects,
    prompt,
    chat: {
      open: chat.open,
      text: chat.text,
      cursor: chat.cursor,
      lines: chatLog.filter((l) => chat.open || now - l.at < CHAT_SHOWN_MS),
    },
    radial: radial.open
      ? {
          items: radial.items,
          selected: radial.selected,
          title: radial.title,
          ready: !!prompt.text.trim(),
        }
      : null,
    touch: touch.state(),
    touchButtons: shownButtons().map((b) => ({ ...b, held: [...touch.pressed.values()].some((p) => p.name === b.name) })),
    input: touch.active ? 'touch' : controller.id ? 'pad' : 'keys',
    fps: sheetOpen ? fps : 0,
    stallNote: stalls.note(),
    walls,
    cube,
    menu,
    lockedOn: !!lockOn,
    removeArmed:
      removeArmed && focusObject?.id === removeArmed.id && performance.now() - removeArmed.at < 2000 ? removeArmed.id : null,
    notices: [], // The "ready v0" build notices are retired (2026-11-03); the prompt block tells the story.
    braincells: !sheetOpen ? null : (() => {
      // Ease the shown count toward the real one; each drop is spent.
      if (braincells === null) return null;
      const gap = shownBraincells - braincells;
      if (Math.abs(gap) < 1) shownBraincells = braincells;
      else shownBraincells -= Math.sign(gap) * Math.max(1, Math.ceil(Math.abs(gap) * 0.06));
      return Math.round(shownBraincells);
    })(),
    spending: braincells !== null && shownBraincells > braincells + 0.5,
    tiles,
    lots,
    home: home && home.room === room ? home.lot : null,
    floorAim: focusObject || !sheetOpen ? null : floorAim,
    chalk: chalkStrokes,
    chalkVersion,
    chalkLocal,
    chalkMode,
    face: seat >= 0 && player ? { ...face, health: player.health, alive: player.alive, seat, pitch, turn: gamepad.lookX + (keys.has('KeyA') || keys.has('ArrowLeft') ? -0.5 : 0) + (keys.has('KeyD') || keys.has('ArrowRight') ? 0.5 : 0) } : null,
    devLog: sheetOpen ? devLog.filter((d) => now - d.at < 120000).map((d) => ({ ...d, age: (now - d.at) / 1000 })) : [],
    owner,
    focusObject,
    builder,
    builderNames,
    netLog,
    connected: !down, // connecting counts; a dead or long-silent socket shows the log
    offline: sheetOpen && down,
    cursor: cursorAim ? mouseAt : null,
    reticle: hosted && sheetOpen && !down,
    reticleShift: reticleShift(now),
    orbit: orbitStep(now),
    cornerScale,
    flyTo: seat < 0 && hosted && sheetOpen ? spawn(lastSeat >= 0 ? lastSeat : 0) : null,
    jobs,
    worldTime,
    locked: locked(),
    legend: sheetOpen && sheetShown && seat >= 0 && legendShown(),
    corner: sheetOpen && !down,
    screens,
    screenPrograms,
    bare: !sheetOpen || !sheetShown || down,
    held: {
      keys: [...keys],
      pad: [
        ...['A', 'B', 'X', 'Y', 'LB', 'RB', 'LT', 'RT'].filter((_, i) => controller.previous[i]),
        ...(gamepad.fwd || gamepad.right ? ['LS'] : []),
        ...(gamepad.lookX || gamepad.lookY ? ['RS'] : []),
      ],
      touch: [...(touch.stick ? ['left'] : []), ...(touch.look ? ['right'] : [])],
    },
  });
  postProcess(elapsed);
}
// 📊 AC's fps switch, shared through localStorage with the prompt (and every
// other AC window), live through the storage event.
{
  const fpsOn = () => { try { return localStorage.getItem('ac:fps-hud') === '1'; } catch { return false; } };
  view.showFps?.(fpsOn());
  window.addEventListener('storage', (e) => { if (e.key === 'ac:fps-hud') view.showFps?.(fpsOn()); });
}
let hushTimer = 0;
if (hosted) {
  window.addEventListener('message', (e) => {
    if (e.source !== window.parent || e.origin !== location.origin || e.data?.type !== 'dool:sheet') return;
    // The prompt's pixel grid rides along: our pixels become its pixels.
    if ('grid' in e.data) view.grid(e.data.grid);
    // And its theme: a light prompt sits over the day room, a dark one over the night.
    if ('dark' in e.data) view.theme(e.data.dark);
    if ('rest' in e.data && resting !== (e.data.rest === true)) {
      resting = e.data.rest === true;
      if (!resting) {
        last = performance.now(); // No catch-up for the time spent resting.
        debt = 0;
      }
    }
    // (A resting loop keeps running, slower; the next frame picks up the rate.)
    // The HUD shows only between the paper's full drop and the start of its
    // return, so nothing pops in or out mid-fade (2026-11-03).
    // (The room no longer scales with the crosshair's hover blink.)
    if (e.data.phase === 'opened') sheetShown = true;
    if (e.data.phase === 'closing') {
      sheetShown = false;
      closingAt = performance.now();
      sfx.exit(); // As the paper starts back up.
    }
    if (e.data.phase === 'opened') closingAt = 0;
    if (!('open' in e.data)) return;
    sheetOpen = e.data.open === true;
    // Back on the curtain you are not in the room (2026-11-03): the seat is
    // given up at once, as if teleported out, and taken fresh on the next drop.
    if (!sheetOpen && ws && ws.readyState !== WebSocket.CLOSED) {
      parked = true;
      logNet('left the room (back on the curtain)');
      ws.close(1000, 'left');
    }
    if (sheetOpen) parked = false;
    if (!sheetOpen) sheetShown = false;
    if (sheetOpen) closingAt = 0; // A fresh drop starts the reticle at the centre.
    if (!sheetOpen) sheetClosedAt = performance.now();
    if (sheetOpen) {
      last = performance.now();
      debt = 0;
      if (!ws || ws.readyState === WebSocket.CLOSED) connect();
      canvas.focus();
      clearTimeout(hushTimer);
      void sfx.wake().then(() => sheetOpen && sfx.enter());
      // The tap that dropped the paper is a fresh gesture: take the mouse now,
      // so no cursor flickers over the world. A refusal leaves click-to-capture.
      if (!touch.active) {
        try {
          Promise.resolve(canvas.requestPointerLock()).catch(() => {});
        } catch {}
      }
    } else {
      releaseInput();
      if (locked()) document.exitPointerLock();
      closeBuild();
      // Let the exit chord ring out before the room goes quiet.
      clearTimeout(hushTimer);
      hushTimer = setTimeout(() => {
        if (!sheetOpen) sfx.audio?.suspend?.().catch?.(() => {});
      }, 1200);
    }
  });
  toHost({ type: 'dool:sheet:hello' });
} else connect();
requestAnimationFrame(frame);
