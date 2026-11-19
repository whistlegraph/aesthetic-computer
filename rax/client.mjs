import { edgeAt, SPAN, groundsOf, cellOf } from './park.mjs';
import { airAt, sun } from './air.mjs';
import { phase as skyPhase } from './sky.mjs';
import { NAME as BOP_NAME } from './bop.mjs';
import { carWalls, lobbyWalls, inCar, clearOfCar, altitude, floorAt, rideSeconds, rideEase } from './elevator.mjs';
import { gameFor } from './games.mjs';
import { HZ, DT, PROTOCOL, HORIZON, move, spawn, clamp } from './world.mjs';
import { reconcile, interpolate } from './net.mjs';
import { LocalPresentation, SnapshotClock } from './presentation.mjs';
import { Prompt } from './prompt.mjs';
import { objectBoxes } from './objects.mjs';
import { direction, rayBox, wallDistance, along, SCREEN, WORLD, halfOf } from './world.mjs';
import { createView } from './view.mjs';
import { Connection } from './connection.mjs';
import { ClientDiagnostics } from './diagnostics-client.mjs';
import { Stalls } from './stalls.mjs';
import { accountToken, accountExpected } from './account.mjs';
import { Controller } from './controller.mjs';
import { moveFreecam } from './freecam.mjs';
import { Radial, HOLD_SECONDS, ringGeometry } from './radial.mjs';
import { TouchPad, touchButtons, touchZones, CONTEXT } from './touch.mjs';
import { Sfx, SpatialTones } from './sfx.mjs';
// 🐌 KidLisp is some thirty modules deep (its runtime and AC's graphics): it
// was fetched before the room could start at all, about 0.4 s of the way in
// on a cold cache (measured 2026-11-19). Now it comes when the first screen
// or wall asks for a program, and the room is up before it.
const kidlispWall = (...given) => import('./kidlisp-wall.mjs').then((m) => m.kidlispWall(...given));
import { screenRows, carryPixels } from './screen-lod.mjs';
import { ballAt } from './ball.mjs';
import { SPAN as BLOCK_SPAN } from './park.mjs';
import { sourceTones, beatAt } from './object-language.mjs';

// ⏱️ How long the way in takes, once: when this module ran, when the view
// was ready, the first connect, the socket open, the welcome, the seat and
// the first frame, in ms since the page began (window.raxBoot; one line in
// the console and the connection's log when the seat is had). Each time the
// paper is put away, how long until the body is there is logged too.
const boot = {};
const bootMark = (name) => { if (!(name in boot)) boot[name] = Math.round(performance.now()); };
try { window.raxBoot = boot; } catch {}
bootMark('module');
const view = await createView(document.getElementById('game'));
bootMark('view');
const canvas = view.canvas,
  keys = new Set();
const query = new URLSearchParams(location.search);
const requestedRoom = query.get('room');
// 🏷️ What this browser kept under the world's old name (dool, until
// 2026-11-18) comes along: who you are, where you live, where you stood.
try {
  for (const key of ['owner', 'home', 'place', 'world', 'tower-top', 'boom', 'blur']) {
    const was = localStorage.getItem(`dool-${key}`);
    if (was !== null && localStorage.getItem(`rax-${key}`) === null) localStorage.setItem(`rax-${key}`, was);
  }
} catch {}
// 🎯 Hosted under the AC prompt (lib/rax-sheet.mjs on aesthetic.computer):
// the paper over this frame drops on a tap and rises on Escape, Back or a tap
// on the corner block. Behind the curtain the world keeps drawing, bare of
// its HUD, makes no sound and holds no seat.
const hosted = query.get('embed') === 'prompt' && window.parent !== window;
let sheetOpen = !hosted,
  unlockedAt = -1e9;
// (A prompt page loaded before the rename to rax listens for the old names
// until it reloads, so each message goes up under both.)
const toHost = (message) => {
  if (!hosted) return;
  window.parent.postMessage(message, location.origin);
  window.parent.postMessage({ ...message, type: message.type.replace(/^rax:/, 'dool:') }, location.origin);
};
function leave() {
  if (hosted) {
    releaseInput();
    if (locked()) document.exitPointerLock();
    toHost({ type: 'rax:sheet:leave' });
  } else location.href = '/';
}
// 🌍 rax is one persistent world: you come back where you last stood. This
// browser remembers the place so the right room is up before the socket is;
// the server, which keeps where every handle rests, has the last word. A
// `room` in the address wins, and so does anywhere travelled to since.
// (`home` is the handle's floor of the tower.)
let home = null, lastPlace = null;
try {
  const kept = localStorage.getItem('rax-place');
  if (kept && /^[a-z0-9-]{1,32}$/.test(kept)) lastPlace = kept;
} catch {}
try {
  const kept = JSON.parse(localStorage.getItem('rax-home') || 'null');
  if (kept && /^[a-z0-9-]{1,32}$/.test(kept.room)) home = { room: kept.room, floor: kept.floor };
} catch {}
let roamed = !!requestedRoom;
let room =
  requestedRoom && /^[a-z0-9-]{1,32}$/.test(requestedRoom)
    ? requestedRoom
    : (accountExpected() && lastPlace) || 'lobby';
let park = null, passage = null, walkAt = 0, walkEdge = null; // walkEdge: the side last walked off
const BACK = { east: 'west', west: 'east', north: 'south', south: 'north' };
// 🚶 The body between two rooms: where the passage sets it down, shown until
// the new room seats it (a moment; twenty seconds at most), so the eye never
// leaves the head for the watcher's camera.
let held = null;
// 🫧 The walk over a seam now under way, and when the last one ended: how
// long the body was between two rooms goes in the connection's log.
let crossing = null, crossedAt = -Infinity;
// ⚽ The park's ball as last heard, in this room's metres, and when.
let ball = null;
let sentSeq = 0; // The newest command handed to the socket.
// 🏢 The tower as the server tells it here ({ count, top, here, yours }), the
// ride in progress ({ from, to, handle, at }), and whether the top line is
// the elevator's (typed in the car) rather than chat.
let tower = null, ride = null, liftLine = false;
// 🔢 In the car the number keys are its buttons: press a floor's digits and,
// a moment after the last one, the car goes. Pressed while it runs, it
// stops where it is and goes to the new floor from there.
let shownAt = null; // when the paper was last put away, until the body is there
let liftWas = null; // the car's height a frame ago, for its speed
const DIAL_MS = 900;
let dial = { text: '', at: 0 };
// The tower's height in floors, as last heard: it stands on every skyline.
let towerTop = 0;
try { towerTop = Number(localStorage.getItem('rax-tower-top')) || 0; } catch {}
// 🛗 While the car runs, how far it still is from the floor it is going to
// (metres; below it going up, above it coming down), and whether it moves.
const rideMoving = (now) => !!ride && now - ride.at < ride.seconds * 1000;
const liftAt = (now) => (ride ? ride.span * (1 - rideEase((now - ride.at) / 1000 / ride.seconds)) : 0);
// 📟 Who lives on which floor, for the car's screen: asked of the directory
// when a car is first near and again whenever the ledger changes (a floor
// added, a number taken or traded). `pencilled` is the numbers set aside for
// a handle that has not come to rax yet: those can still be taken.
let directory = new Map(), pencilled = new Set(), directoryFor = -1;
async function readDirectory(revision) {
  if (directoryFor === revision) return;
  directoryFor = revision;
  try {
    const r = await fetch(new URL('./tower', import.meta.url), { cache: 'no-store' });
    const data = r.ok ? await r.json() : null;
    if (Array.isArray(data?.floors)) {
      directory = new Map(data.floors.map(([n, handle]) => [n, handle]));
      pencilled = new Set(data.floors.filter(([, , held]) => !held).map(([n]) => n));
    }
  } catch { directoryFor = -1; }
}
// What the typed line would reach, said on the screen before Enter.
function floorAsked(text) {
  const q = text.trim().toLowerCase().replace(/^(?:floor|go)\s+/, '');
  if (!q) return 'number, @handle or take N';
  if (/^(take|claim)\b/.test(q)) return /^(?:take|claim)\s+#?\d{1,4}$/.test(q) ? 'enter: make it yours' : 'take which number?';
  if (['g', '0', 'ground', 'plaza', 'lobby'].includes(q)) return 'the plaza';
  if (q === 'home') return home?.floor ? `your floor, ${home.floor}` : 'sign in for a floor';
  if (/^#?\d{1,4}$/.test(q)) {
    const n = Number(q.replace('#', '')), who = directory.get(n);
    if (who && pencilled.has(n) && n !== home?.floor) return `@${who}, not here yet: take ${n}`;
    return who ? `@${who}` : `nobody lives there: take ${n}`;
  }
  const name = q.replace(/^@/, '');
  for (const [n, handle] of directory) if (handle === name) return `floor ${n}`;
  for (const [n, handle] of directory) if (name && handle.startsWith(name)) return `@${handle}? floor ${n}`;
  return 'nobody by that name';
}
// 🪧 The lobby's board: your floor and the floors with someone on them now.
const lobbyBoard = () => ({ yours: tower?.yours?.number || home?.floor || 0, in: tower?.inside || [] });
function carScreen(rideNow) {
  // Asked where to: the line being typed is on the screen itself.
  if (dial.text) return { number: dial.text, moving: true, handle: floorAsked(dial.text), note: 'going in a moment', entry: true };
  if (liftLine && chat.open) {
    const blink = Math.floor(performance.now() / 400) % 2 ? '_' : ' ';
    return { number: '?', moving: true, handle: `${chat.text.slice(0, 22)}${blink}`, note: floorAsked(chat.text), entry: true };
  }
  const moving = !!rideNow?.moving, n = moving ? rideNow.floor : tower?.here?.number ?? 0;
  // The floor you stand on is named by the server's last word, not the directory.
  const who = n ? (n === tower?.here?.number && tower.here.handle) || directory.get(n) : null;
  return {
    number: n ? `${n}` : 'G',
    moving,
    handle: n ? (who ? `@${who}` : 'empty') : 'plaza',
    note: moving ? `${rideNow.to > rideNow.from ? 'up' : 'down'} to ${rideNow.to || 'G'}${rideNow.handle ? ` @${rideNow.handle}` : ''}`
      : n ? `${altitude(n)} m up` : tower?.count ? `${tower.count} floors` : '',
  };
}
// Where the plaza (and the tower on it) stands from this room, in metres.
// 🌧️ What falls over Los Angeles falls here: asked of the server now and
// every five minutes (weather.mjs).
const placeOf = (id) => { const c = cellOf(id); return c ? { x: c.i * SPAN, z: c.j * SPAN } : null; };
let weather = null;
// 👀 The people in the rooms around this one (the server's `beyond`), eased
// from where each was last drawn to where they were last heard of.
let beyond = new Map(), drones = new Map(), myEye = null, droneView = false; // droneView: looking on, from behind your own drone
// The eye a little behind and above your own drone, along the way it looks.
function droneBack() {
  const d = direction(yaw, pitch);
  return { x: freecam.x - d.x * 1.6, y: Math.max(0.2, freecam.y - d.y * 1.6 + 0.35), z: freecam.z - d.z * 1.6 };
}
function droneAt(d, now) {
  const k = Math.min(1, (now - d.at) / 110), f = d.from;
  if (!f) return d;
  let turn = d.yaw - f.yaw;
  turn -= Math.round(turn / 360) * 360;
  return { ...d, x: f.x + (d.x - f.x) * k, y: f.y + (d.y - f.y) * k, z: f.z + (d.z - f.z) * k, yaw: f.yaw + turn * k, from: null };
}
function beyondAt(b, now) {
  const k = Math.min(1, (now - b.at) / 70), f = b.from;
  if (!f) return b;
  let turn = b.yaw - f.yaw;
  turn -= Math.round(turn / 360) * 360;
  return { ...b, x: f.x + (b.x - f.x) * k, y: f.y + (b.y - f.y) * k, z: f.z + (b.z - f.z) * k, yaw: f.yaw + turn * k, pitch: f.pitch + (b.pitch - f.pitch) * k, from: null };
}
const askWeather = () => fetch(new URL('./weather', import.meta.url)).then((r) => (r.ok ? r.json() : null)).then((w) => { if (w) weather = w; }).catch(() => {});
void askWeather();
setInterval(askWeather, 5 * 60 * 1000);
function plazaFrom() {
  if (room === 'lobby') return { dx: 0, dy: 0, dz: 0 };
  if (room.startsWith('floor-')) return tower?.here?.number ? { dx: 0, dy: -altitude(tower.here.number), dz: 0 } : null;
  const m = /^([ew])(\d{1,4})([ns])(\d{1,4})$/.exec(room);
  return m ? { dx: (m[1] === 'w' ? 1 : -1) * Number(m[2]) * SPAN, dy: 0, dz: (m[3] === 's' ? 1 : -1) * Number(m[4]) * SPAN } : null;
}
// 🎬 Filming (?film, served only by film/shoot.mjs): this page watches without
// a body and the director's camera is the view. Null everywhere else.
let film = null;
if (query.has('film'))
  void import('./film/stage.mjs').then((m) => {
    film = m.stage({ canvas, travel: (to, riding) => travel(to, true, null, riding || null),
      world: () => ({ room, members, players: filmPlayers, connected: ws?.readyState === WebSocket.OPEN, ride: filmRide }) });
  }).catch(() => {});
let filmPlayers = [], filmRide = null;
const hasCar = () => room === 'lobby' || room.startsWith('floor-');
// 📐 This room's half-width (world.mjs): the same number the server moves us by.
const half = () => halfOf(room);
const CAR_BOXES = carWalls(), LOBBY_BOXES = lobbyWalls();
// 🌍 The world around this block, streamed from the server and kept between
// visits: the last answer of /world for each block it was asked about.
let world = null, worldAt = 0;
const worlds = new Map();
try {
  for (const [id, w] of Object.entries(JSON.parse(localStorage.getItem('rax-world') || '{}')))
    if (Array.isArray(w?.blocks)) worlds.set(id, w);
} catch {}
// The room's own solid boxes: the car, and the lobby on the plaza.
let carBoxes = null, carBoxesFor = null;
// 🍄 In botbop's arena the view is side-on: left and right walk, the body
// faces the way it last went, and the ledges are the room's fixed boxes.
// 🎮 The room's gametype (games.mjs), or null in rax's own rooms.
let gameOf = null, gameRoom = null;
function game() {
  if (gameRoom !== room) { gameRoom = room; gameOf = gameFor(room); gameCam = null; }
  return gameOf;
}
const flat = () => !!game()?.flat;
let flatYaw = 90, flatJumpHeld = false;
let debugBoxes = false; // 🔲 ` in a game: hitboxes and numbers
// Its camera, eased toward where the game wants it so framing never snaps.
let gameCam = null;
function gameCamera(me, players, elapsed) {
  const want = game().camera(me, players, canvas.width / Math.max(1, canvas.height));
  if (!gameCam) return (gameCam = want);
  const k = 1 - Math.exp(-Math.min(0.1, elapsed) * 5);
  for (const key of ['x', 'y', 'zoom']) gameCam[key] += (want[key] - gameCam[key]) * k;
  return (gameCam = { ...want, x: gameCam.x, y: gameCam.y, zoom: gameCam.zoom });
}
// The sides of this place that lead to a neighbour: no wall stands there.
let openFor = null, openSides = null;
function openEdges() {
  if (openFor !== park) {
    openFor = park;
    openSides = park?.neighbours ? Object.keys(park.neighbours).filter((k) => park.neighbours[k]) : null;
  }
  return openSides;
}
const NO_BOXES = [];
function fixedBoxes() {
  if (game()?.ledges) return game().ledges;
  if (!hasCar()) return NO_BOXES;
  // (Rebuilt when the room changes: only the plaza has the lobby.)
  if (carBoxesFor !== room) { carBoxesFor = room; carBoxes = null; }
  return (carBoxes ||= [...CAR_BOXES, ...(room === 'lobby' ? LOBBY_BOXES : [])]);
}
// The blocks beside the one underfoot, placed in metres from its middle.
function around() {
  if (!world) return [];
  // 🏢 From a floor of the tower the plaza and its blocks lie below.
  if (room.startsWith('floor-')) {
    const at = plazaFrom();
    return at ? world.blocks.map((b) => ({ ...b, dx: b.di * SPAN, dz: b.dj * SPAN, dy: at.dy,
      home: null })) : [];
  }
  const here = world.blocks.find((b) => b.id === room);
  if (!here) return [];
  return world.blocks
    .filter((b) => b !== here && Math.abs(b.di - here.di) <= 1 && Math.abs(b.dj - here.dj) <= 1)
    .map((b) => ({ ...b, dx: (b.di - here.di) * SPAN, dz: (b.dj - here.dj) * SPAN,
      home: home && home.room === b.id ? home.lot : null }));
}
async function streamWorld() {
  const asked = room;
  worldAt = performance.now();
  // A floor of the tower looks down on the plaza's neighbourhood.
  const about = asked.startsWith('floor-') ? 'lobby' : asked;
  try {
    const r = await fetch(new URL(`./world?room=${encodeURIComponent(about)}`, import.meta.url), { cache: 'no-store' });
    if (!r.ok || room !== asked) return;
    const data = await r.json();
    if (!Array.isArray(data.blocks)) return;
    world = data;
    worlds.delete(about);
    worlds.set(about, data);
    while (worlds.size > 6) worlds.delete(worlds.keys().next().value);
    try {
      localStorage.setItem('rax-world', JSON.stringify(Object.fromEntries(worlds)));
    } catch {}
  } catch {}
}
world = worlds.get(room.startsWith('floor-') ? 'lobby' : room) || null;
void streamWorld();
// Under the prompt the host may not have said who is signed in when this
// loads: look again shortly, so the room behind the curtain is theirs too.
if (!roamed && lastPlace && room !== lastPlace)
  for (const ms of [800, 2500, 6000])
    setTimeout(() => {
      if (!roamed && room !== lastPlace && accountExpected()) travel(lastPlace, false);
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
let resting = false; // The host window is in the background (lib/rax-sheet.mjs).
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
// 📟 Just loaded again because the server came back: say so for a few seconds.
let rebooted = null;
try {
  const onto = sessionStorage.getItem('rax-rebooted');
  if (onto !== null) { sessionStorage.removeItem('rax-rebooted'); rebooted = { text: `the server restarted: back on ${onto || 'new code'}`, until: performance.now() + 9000 }; }
} catch {}
let statsShown = false;
try { statsShown = localStorage.getItem('rax-stats') === '1'; } catch {}
const extrasSeen = new Set(); // a controller's buttons past the standard ones, as they are first pressed
// 🔄 New code reaches a page that is up over the room (parked) too: it asks
// the server's revision every half minute and loads again, unseen, when
// that is not the one it started with. (In the room, the socket's fall
// does it: watchHealth.)
let bootRevision = null;
const askRevision = () => fetch(new URL('./health', import.meta.url), { cache: 'no-store' }).then((r) => (r.ok ? r.json() : null)).then((h) => h?.revision || null).catch(() => null);
void askRevision().then((r) => { bootRevision = r; });
setInterval(async () => {
  if (!hosted || sheetOpen || !bootRevision) return;
  const now = await askRevision();
  if (now && now !== bootRevision && !sheetOpen) location.reload();
}, 30000);
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
          try { sessionStorage.setItem('rax-rebooted', String(h.revision || '').slice(0, 10)); } catch {}
          return window.raxRig?.restart ? window.raxRig.restart() : location.reload();
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
// The room lists them. A screen's program runs here at the pixel grid the
// view asks for (`want`, from how far the eye is: screen-lod.mjs), one
// program for each code at each grid, shared by the screens that show it.
let screens = [];
const screenPrograms = new Map(); // `${code}@${cols}` → program, or null while it loads
const screenSources = new Map(); // code → source, fetched once
function screenProgram(code, cols, from = null) {
  const key = `${code}@${cols}`;
  if (screenPrograms.has(key)) return;
  screenPrograms.set(key, null);
  kidlispWall(code, { width: cols, height: screenRows(cols), source: screenSources.get(code) || null })
    .then((w) => {
      screenSources.set(code, w.source);
      // Start from the picture the coarser (or finer) grid was showing.
      const old = from && screenPrograms.get(`${code}@${from}`);
      if (old) carryPixels(old.pixels, old.width, old.height, w.pixels, w.width, w.height);
      w.usedAt = performance.now();
      screenPrograms.set(key, w);
    })
    .catch((e) => console.info(`KidLisp screen $${code}:`, e.message));
}
function setScreens(list) {
  // Keep the same objects for screens that stay, so their frame caches hold.
  // A screen stood on the car before 2026-11-18 is shown beside it.
  screens = list.map((s) => Object.assign(screens.find((o) => o.id === s.id) || {}, s, hasCar() ? clearOfCar(s.x, s.z) : null));
}
// Once a frame: load the grid each screen wants, and let go of a program no
// screen has drawn from for a while.
function tendScreens(now) {
  for (const s of screens) screenProgram(s.code, s.want || 64, s.cols || null);
  for (const [key, w] of screenPrograms) if (w && now - w.usedAt > 8000) screenPrograms.delete(key);
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
let lookAt = 0, lookSaid = ''; // When and what this eye last told the room of itself.
// Just arrived and not seated yet (the seat waits for a signed-in handle):
// that is joining, not watching, so none of the spectator's mode shows.
let joiningUntil = 0;
const joining = () => seat < 0 && performance.now() < joiningUntil;
// 🪪 What the seat waits for: 'account' (nobody is signed in) or 'handle'
// (signed in, no handle yet). Until then the visitor flies over and looks.
let gate = null;
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
// more as the view turns faster. Off: its menu switch is retired (2026-11-18),
// and only a browser that turned it on before still has it.
let motionBlur = false;
try {
  motionBlur = localStorage.getItem('rax-blur') === 'on';
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
    ws.bufferedAmount < (data.type === 'input' ? 65536 : 32768)
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
    const peeking = room;
    const r = await fetch(new URL(`./room?room=${encodeURIComponent(room)}`, import.meta.url), { cache: 'no-store' });
    if (!r.ok) return;
    const data = await r.json();
    if (ws && ws.readyState !== WebSocket.CLOSED) return; // Joined meanwhile: the live state wins.
    if (room !== peeking) return;
    park = data.park || null;
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
  window.raxDebug = () => ({
    seat,
    room, park, home, tower, ride,
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
// A place in the one world (the plaza, a block of the park, a floor of the
// tower), as against an instance: a game's arena or a named room.
const inWorld = (id) => id === 'lobby' || id.startsWith('floor-') || /^[ew]\d{1,4}[ns]\d{1,4}$/.test(id);
// 🧭 This place's AC address, for the host's address bar: a game's own word
// (botbop, botbop~name), else rax~room; the lobby and your home are rax.
function placePath() {
  const g = game();
  if (g?.at(room)) return room.startsWith(`${g.name}-`) ? `/${g.name}~${room.slice(g.name.length + 1)}` : `/${g.name}`;
  // The world has one address: the plaza, a block and a floor of the tower
  // are all rax. Only a room outside the world has a word of its own.
  return inWorld(room) ? '/rax/' : `/rax~${room}`;
}
function travel(to, chosen = true, receipt = null, riding = null, at = null, ack = null) {
  if (!/^[a-z0-9-]{1,32}$/.test(to) || to === room) return;
  // 🚶 The body goes on between the two rooms (held): it keeps walking by its
  // own commands, which the next room runs when it seats it. A walk across
  // a block's edge keeps its motion and the commands the old room never ran.
  const walk = Number.isInteger(ack), from = room;
  if (walk) crossing = { from, to, at: performance.now() };
  // The ball stays where it is in the world: over a seam it is a block
  // away in the new room's metres; by any other way it is out of sight.
  if (walk && ball && player && at) {
    ball = { ...ball, x: ball.x + Math.round((at.x - player.x) / BLOCK_SPAN) * BLOCK_SPAN, z: ball.z + Math.round((at.z - player.z) / BLOCK_SPAN) * BLOCK_SPAN };
  } else ball = null;
  held = receipt && at && player && seat >= 0
    ? { ...player, vx: 0, vz: 0, ...(walk ? at : { x: at.x, z: at.z }), walk, until: performance.now() + 20000 } : null;
  const owed = held && walk ? pending.filter((c) => c.seq > ack).slice(-HORIZON) : [];
  if (chosen) roamed = true;
  room = to;
  if (riding) {
    // 🛗 The car leaves at once and the next floor is already known: the
    // ride is the distance between the two, travelled for real.
    // Asked while the car ran: it starts from where it is, not from the
    // floor it was on its way to.
    const now = performance.now(), running = rideMoving(now) ? liftAt(now) : 0;
    const span = altitude(riding.from) - altitude(riding.to) + running;
    ride = { ...riding, ...(running ? { from: floorAt(altitude(riding.from) + running + 0.5) } : {}), at: now, span, seconds: rideSeconds(span), rung: false };
    tower = { ...(tower || {}), here: riding.to ? { number: riding.to, handle: riding.handle, colors: null } : { room: 'lobby', number: 0 }, owner: false };
    sfx.ringMove(ride.to > ride.from ? 9 : 4);
  } else {
    tower = null;
    ride = null;
  }
  passage = receipt;
  beyond = new Map(); // (the next room tells them in its own metres)
  drones = new Map();
  park = null;
  walkAt = performance.now() + 1000;
  objects = [];
  tiles = {};
  // What the world stream already knows of the place stands there at once:
  // the ground and things were on screen a block away a moment ago.
  const known = world?.blocks.find((b) => b.id === to);
  if (known) {
    park = { neighbours: known.neighbours, plaza: known.plaza };
    objects = known.objects || [];
    tiles = known.tiles || {};
  } else if (walk && walkEdge) {
    // Nothing known of the place yet but the way in: that side leads back.
    park = { neighbours: { [BACK[walkEdge]]: from }, plaza: false };
  }
  void streamWorld();
  setScreens([]);
  chalkStrokes = [];
  chalkVersion++;
  if (joinURL) joinURL = new URL(`?room=${room}`, location.href).href;
  try {
    const address = new URL(location.href);
    address.searchParams.set('room', room);
    window.history.replaceState(window.history.state, '', address);
  } catch {}
  toHost({ type: 'rax:sheet:place', path: placePath() });
  if (inWorld(room)) {
    lastPlace = room;
    try { localStorage.setItem('rax-place', room); } catch {}
  }
  if (ws && ws.readyState !== WebSocket.CLOSED) {
    const old = ws;
    ws = null; // its close is ours, not an outage
    old.close();
    // 🚪 The next room's door is already open: step through it.
    const door = warm.get(to);
    if (door) warm.delete(to);
    coolDown(walk ? from : null); // (the room just left keeps its door: the way straight back)
    if (walk && door?.ready && door.socket.readyState === WebSocket.OPEN) connect(door);
    else {
      if (door) { door.socket.onclose = door.socket.onerror = door.socket.onmessage = null; try { door.socket.close(); } catch {} }
      connect();
    }
  } else void peekRoom();
  if (held && owed.length) {
    pending = owed.map((c, i) => ({ ...c, seq: i + 1 }));
    seq = pending.length;
    for (const c of pending) held = { ...move(held, c, solids(), 0, fixedBoxes(), flat(), openEdges(), half()), walk, until: held.until };
  }
}
// 🚪 The rooms next door, held open while you are near their seam: each
// has answered and knows who you are, and waits (wait=1). Stepping over,
// the passage is handed to the one already open, so there is no new
// connection to make. Its room's words are kept to be heard on stepping in.
const WARM_NEAR = 6, WARM_FAR = 7; // metres from a block's line: open within, let go beyond
const warm = new Map(); // room → { socket, words, ready }
function warmUp(to) {
  if (warm.has(to) || warm.size >= 3) return;
  const url = new URL('./play', import.meta.url);
  url.protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
  url.search = new URLSearchParams({ room: to, protocol: PROTOCOL, wait: '1', ...(accountExpected() ? { account: '1' } : {}) });
  const entry = { socket: new WebSocket(url), words: [], ready: false };
  warm.set(to, entry);
  entry.socket.onmessage = (e) => {
    if (typeof e.data !== 'string' || e.data.startsWith('{"type":"snapshot"')) return; // the room's motion is not kept
    let type = '';
    try { type = JSON.parse(e.data).type; } catch { return; }
    // (What only happened once is not heard again: a line said, a pong, an answer to an asking.)
    if (['whereabouts', 'ball', 'gate', 'said', 'pong', 'dev-log', 'input', 'travel', 'build-result', 'build-accepted', 'elevator-result'].includes(type)) return;
    if (entry.words.length < 256) entry.words.push(e.data);
    if (type === 'welcome') {
      // Say who is at the door now, so the step in has nothing to wait for.
      entry.ready = true;
      let token;
      try { token = localStorage.getItem('rax-owner'); } catch {}
      entry.socket.send(JSON.stringify({ type: 'identity', token }));
      if (accountExpected()) void accountToken().then((accessToken) => {
        if (accessToken && warm.get(to) === entry && entry.socket.readyState === WebSocket.OPEN) entry.socket.send(JSON.stringify({ type: 'account', accessToken }));
      });
    }
  };
  entry.socket.onclose = entry.socket.onerror = () => { if (warm.get(to) === entry) warm.delete(to); };
}
function coolDown(keep = null) {
  for (const [to, entry] of warm) {
    if (to === keep) continue;
    warm.delete(to);
    entry.socket.onclose = entry.socket.onerror = entry.socket.onmessage = null;
    try { entry.socket.close(1000, 'walked away'); } catch {}
  }
}
// Each frame: which neighbours' lines are you near?
function tendWarm() {
  if (!(park?.neighbours && player && seat >= 0 && sheetOpen && !held)) { if (!held) coolDown(); return; }
  const h = half(), near = [];
  for (const [side, d] of [['east', h - player.x], ['west', h + player.x], ['north', h - player.z], ['south', h + player.z]]) {
    const to = park.neighbours[side];
    if (!to) continue;
    if (d < (warm.has(to) ? WARM_FAR : WARM_NEAR)) near.push([d, to]);
  }
  near.sort((a, b) => a[0] - b[0]);
  const wanted = near.slice(0, 2).map(([, to]) => to);
  // 🔙 And this room's own door, for turning straight back once over the line.
  if (wanted.length) wanted.push(room);
  for (const to of [...warm.keys()]) if (!wanted.includes(to)) { const e = warm.get(to); warm.delete(to); e.socket.onclose = e.socket.onerror = e.socket.onmessage = null; try { e.socket.close(1000, 'walked away'); } catch {} }
  for (const to of wanted) warmUp(to);
}
function connect(adopt = null) {
  bootMark('connect');
  const url = new URL('./play', import.meta.url);
  url.protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
  // 🔐 Signed in on this page: fetch the token while the socket opens, and
  // ask the server to keep the seat for the handle (no guest name first).
  const account = accountExpected() ? accountToken() : null;
  url.search = new URLSearchParams({ room, protocol: PROTOCOL, ...(passage ? { passage } : {}), ...(account ? { account: '1' } : {}), ...(query.has('film') ? { watch: '1' } : {}) });
  history = [];
  pending = [];
  player = null;
  snapshot = null;
  prediction = null;
  resetPrediction = false;
  seq = sentSeq = 0;
  eventId = 0;
  debt = 0;
  presentation.reset(null);
  playback.reset();
  notice = held ? '' : 'CONNECTING';
  // 🚶 Walking over a seam you are still you and the line is still the line:
  // your face, your ping and the roster stay up until the next room speaks.
  if (held?.walk) { connection.pending.clear(); connection.next = 0; }
  else {
    connection.reset();
    members = [];
    selfMember = null;
  }
  freecamPlaced = false;
  gate = null;
  joiningUntil = performance.now() + 7000;
  connectAttempts++;
  logNet(adopt ? `stepping into room ${room} (its door was open)` : `connecting to room ${room} (try ${connectAttempts})`);
  ws = adopt ? adopt.socket : new WebSocket(url);
  const socket = ws;
  socket.onclose = socket.onerror = null;
  // The door was open: hear what the room said while it waited, then hand
  // in the passage (after this function has set the socket's ears).
  if (adopt) queueMicrotask(() => {
    if (socket !== ws) return;
    for (const word of adopt.words) socket.onmessage({ data: word });
    if (socket === ws && socket.readyState === WebSocket.OPEN) socket.send(JSON.stringify({ type: 'arrive', passage }));
  });
  socket.onopen = () => {
    if (socket === ws) { bootMark('open'); logNet('socket open', { good: true }); }
  };
  socket.onmessage = (e) => {
    if (socket !== ws) return;
    const messageAt = performance.now();
    const data = JSON.parse(e.data);
    if (data.type === 'welcome' || data.type === 'role') {
      if (data.eye) myEye = data.eye;
      const old = player;
      seat = data.seat;
      if (seat >= 0) {
        lastSeat = seat;
        gate = null;
      }
      if (seat < 0 && !held) {
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
      sentSeq = 0;
      // Between two rooms the body is still walking: its keys stay down and
      // its commands wait for the seat.
      if (!held) {
        pending = [];
        seq = 0;
        debt = 0;
        prompt.open = false;
        prompt.busy = false;
        radial.close();
        releaseInput();
        presentation.reset(null);
      }
    }
    if (data.type === 'gate' && seat < 0) {
      gate = data.needs === 'handle' ? 'handle' : 'account';
      joiningUntil = 0; // Not joining: looking.
    }
    if (data.type === 'welcome') {
      bootMark('welcome');
      // Nobody signed in on this page and the room seats handles only.
      if (data.guests === false && seat < 0 && !account && !query.has('film')) {
        gate = 'account';
        joiningUntil = 0;
      }
      configureDiagnostics(data.diagnostics === true);
      logNet(seat >= 0 ? `joined as player ${seat + 1}` : account ? 'signing in' : 'joined as a spectator', { good: true });
      if (data.joinURL) joinURL = data.joinURL;
      let token;
      try {
        token = localStorage.getItem('rax-owner');
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
    if (data.type === 'ball') { ball = { ...data, at: performance.now() }; return; }
    // 👀 Who walks the rooms around this one, in this room's metres.
    if (data.type === 'beyond') {
      const now = performance.now(), next = new Map();
      for (const [id, name, seat, x, y, z, yaw, pitch, vx, vz] of data.people) {
        const was = beyond.get(id), from = was ? beyondAt(was, now) : null;
        next.set(id, { id, name, seat, x, y, z, yaw, pitch, vx, vz, alive: true, from, at: now });
      }
      beyond = next;
      // 🚁 And who looks on from the air, each a drone (your own is not drawn).
      const eyes = new Map();
      for (const [id, name, x, y, z, yaw] of data.drones || []) {
        if (id === myEye) continue;
        const was = drones.get(id), from = was ? droneAt(was, now) : null;
        // (How fast it goes, eased: what its lean is drawn from.)
        const vx = was ? (was.vx || 0) * 0.6 + ((x - was.x) / Math.max(0.03, (now - was.at) / 1000)) * 0.4 : 0;
        const vz = was ? (was.vz || 0) * 0.6 + ((z - was.z) / Math.max(0.03, (now - was.at) / 1000)) * 0.4 : 0;
        eyes.set(id, { id, name, x, y, z, yaw, vx, vz, from, at: now });
      }
      drones = eyes;
      return;
    }
    if (data.type === 'park') park = { neighbours: data.neighbours, plaza: data.plaza };
    if (data.type === 'travel') travel(data.room, true, data.passage, data.ride || null, data.at || null, data.ack);
    if (data.type === 'tower') {
      tower = data;
      if (hasCar()) void readDirectory(data.revision ?? data.count);
      if (data.top) {
        towerTop = data.top;
        try { localStorage.setItem('rax-tower-top', String(towerTop)); } catch {}
      }
      toHost({ type: 'rax:sheet:place', path: placePath() });
    }
    // The elevator answers in the room's own lines.
    if (data.type === 'elevator-result')
      chatLog = [...chatLog, { at: performance.now(), handle: null, name: 'elevator', colors: null, seat: -1,
        text: String(data.error || data.note || '').slice(0, CHAT_MAX) }].slice(-8);
    if (data.type === 'arrival') {
      // A walker looks where they were looking; a door or the car sets the facing.
      if (!held?.walk) { yaw = data.yaw; pitch = data.pitch || 0; }
      passage = null;
    }
    if (data.type === 'home' && /^[a-z0-9-]{1,32}$/.test(data.room || '')) {
      home = { room: data.room, floor: data.floor };
      try {
        localStorage.setItem('rax-home', JSON.stringify(home));
      } catch {}
    }
    // 🌍 Where this handle last stood in the world: go there, unless this
    // visit has already gone somewhere of its own. Heard once a visit: the
    // room it leads to says so again, and following that too never ends.
    if (data.type === 'whereabouts' && /^[a-z0-9-]{1,32}$/.test(data.room || '') && !roamed) {
      roamed = true;
      if (data.room !== room) travel(data.room, false);
    }
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
        localStorage.setItem('rax-owner', data.token);
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
    if (seat < 0 && !freecamPlaced && !held) {
      const target = data.players.find(Boolean);
      if (target) {
        freecam = {
          x: clamp(target.x + 4, -half() + 1, half() - 1),
          y: target.y + 3,
          z: clamp(target.z - 5, -half() + 1, half() - 1),
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
    // (With a passage in hand the seat's first place is not ours: wait to be set down.)
    if (own && !(passage && held)) {
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
    history = history.slice(-64);
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
      if (event.type === 'bank' && event.seat === seat) sfx.bank(event.combo);
      if (event.type === 'bop') {
        sfx.bop(event.attacker === seat, event.combo);
        if (event.attacker === seat) face.grinAt = performance.now();
        if (event.drone !== undefined && event.attacker === seat) sfx.ricochet(0.6);
        if (event.seat === seat) { face.hurtAt = performance.now(); face.hurtSide = 0; }
      }
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
  const reach = Math.min(3, wallDistance(player, dir, 3, solids(), 0, half()) - 0.05);
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
// The $ menu is one list: what your handle has made (2026-11-18). Its go
// and view tabs are retired: places are walked and ridden to, V and [Y]
// switch the camera, and chalk is erased from the build ring.
function menuItems() {
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
  return list.length
    ? list.map((h) => ({ ...h, act: () => send({ type: 'restore', code: h.code }) }))
    : [{ label: historyPending ? 'loading' : 'nothing made yet', act: () => {} }];
}
function toggleMenu() {
  if (menu) return closeMenu();
  historyPending = !handleHistory.length;
  send({ type: 'handle-history' });
  menu = { kind: 'history', tabs: ['$'], tab: 0, items: menuItems(), selected: 0 };
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
const touchMode = () => (seat < 0 || down || radial.open || menu ? null : chalkMode ? 'chalk' : typing() ? 'typing' : 'play');
// The home bar's inset, measured once from a probe (env() has no script API).
const insetProbe = Object.assign(document.createElement('div'), { ariaHidden: 'true' });
insetProbe.style.cssText = 'position:fixed;left:0;bottom:0;width:0;height:env(safe-area-inset-bottom,0px);visibility:hidden;pointer-events:none;';
document.body.append(insetProbe);
const bottomInset = () => insetProbe.offsetHeight || 0;
// What the phone's keyboard leaves in view.
const visibleHeight = () => {
  const v = window.visualViewport;
  return v ? Math.min(canvas.clientHeight, v.height + v.offsetTop) : canvas.clientHeight;
};
// 📱 The caps above the icons: what the thing in view offers, and nothing else.
function touchContext() {
  if (holding()) {
    const item = objects.find((o) => snapshot?.items?.some((it) => it.id === o.id && it.holder === seat));
    return [CONTEXT.use({ shoot: 'shoot', poke: 'poke', throw: 'throw' }[item?.item] || 'use'), CONTEXT.drop];
  }
  // Locked on, a remove is at once (the lock is the confirmation).
  if (lockOn) return [CONTEXT.unlock, lockOn.wall ? CONTEXT.change : CONTEXT.remove];
  if (focusObject?.wall) return [CONTEXT.change, CONTEXT.lock];
  if (focusObject) {
    const armed = removeArmed?.id === focusObject.id && performance.now() - removeArmed.at < 2000;
    return [armed ? { ...CONTEXT.remove, name: 'again?' } : CONTEXT.remove, CONTEXT.lock];
  }
  // The floor takes paint; the open air takes chalk.
  return [floorAim ? CONTEXT.paint : CONTEXT.chalk];
}
// The build ring, for a build without typing: a cap beside send and cancel.
const ringCap = { ...CONTEXT.ring, act: () => { softKeys.blur(); openRadial(); } };
const shownButtons = () => {
  if (!touch.active) return [];
  const mode = touchMode(), keyboardUp = mode === 'typing';
  return touchButtons(mode, canvas.clientWidth, keyboardUp ? visibleHeight() : canvas.clientHeight, {
    context: mode === 'play' ? touchContext() : mode === 'typing' && prompt.open && !prompt.busy ? [ringCap] : [],
    inset: keyboardUp && visibleHeight() < canvas.clientHeight ? 0 : bottomInset(),
  });
};
const shownZones = () => (touch.active && ['play', 'chalk'].includes(touchMode()) ? touchZones(canvas.clientWidth, canvas.clientHeight, bottomInset()) : []);
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
  if (button.act) return void (isDown || button.act());
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
  if (!touchy(e)) return;
  touchButton(touch.up(e, performance.now()), false);
});
canvas.addEventListener('pointercancel', (e) => {
  if (!touchy(e)) return;
  const held = touch.cancel(e);
  if (held && !held.act) pressKey('keyup', held.code, held.key);
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
    toHost({ type: 'rax:sheet:density', step: e.key === '0' ? 0 : e.key === '-' ? -1 : 1 });
    return;
  }
  if (down && e.code !== 'Escape') {
    e.preventDefault();
    return;
  }
  if (!chat.open && !prompt.open && !menu && !radial.open && seat >= 0 && player && hasCar() && inCar(player)
    && /^(Digit|Numpad)\d$/.test(e.code) && !e.metaKey && !e.ctrlKey && !e.altKey) {
    if (!e.repeat) dial = { text: (dial.text + e.code.slice(-1)).slice(-4), at: performance.now() };
    e.preventDefault();
    return;
  }
  // 💬 Chat takes the keys while its line is open.
  if (chat.open) {
    // 🛗 In the car the line is the elevator's: a number, a handle, home, g,
    // or "take 89" to make a number yours; up and down ride one floor.
    const step = liftLine && !chat.text && !e.repeat && (e.code === 'ArrowUp' ? 'up' : e.code === 'ArrowDown' ? 'down' : null);
    if (step) {
      send({ type: 'elevator', to: step });
      chat.open = false;
    } else if (e.code === 'Enter' && !e.repeat) {
      const text = chat.text.trim();
      const taking = liftLine && /^(?:take|claim)\s+#?(\d{1,6})$/i.exec(text);
      if (taking) send({ type: 'take-floor', number: Number(taking[1]) });
      else if (text) send(liftLine ? { type: 'elevator', to: text.replace(/^(?:floor|go)\s+/i, '') } : { type: 'say', text });
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
    liftLine = false;
    chat.begin();
    return;
  }
  // Tab: back to the prompt, as Tab there comes in (2026-11-10). $ (shift
  // 4): the menu, the list of what you made, as the prompt's $ starts a code; not while typing, where $ is a letter
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
  // Hosted, Escape takes two steps (2026-11-18): the first only frees the
  // mouse, and the next, with the mouse free and nothing open, brings the
  // prompt's paper back. Tab is the one-press way out. A browser may end the
  // lock itself and then hand over the same press; that press has done its
  // work, so the moment just after an unlock does not leave.
  if (hosted && e.code === 'Escape' && !e.repeat && !prompt.open) {
    e.preventDefault();
    if (locked()) document.exitPointerLock();
    else if (performance.now() - unlockedAt > 250) leave();
    return;
  }
  if (e.code === 'Enter' && !e.repeat) {
    e.preventDefault();
    if (seat < 0) return; // Looking on: Enter asks for nothing.
    // 🛗 Standing in the car, Enter asks where to.
    if (!prompt.open && hasCar() && inCar(player) && !menu && !radial.open && !chalkMode) {
      releaseInput();
      liftLine = true;
      chat.text = '';
      chat.begin();
      // Turn to the screen over the door: that is where the floor is typed.
      yaw = 180;
      pitch = 24;
      return;
    }
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
  // 🔲 ` in a game shows its hitboxes, whether or not the keys are engaged.
  if (game() && e.code === 'Backquote' && !e.repeat) {
    debugBoxes = !debugBoxes;
    e.preventDefault();
    return;
  }
  // 🔬 Standing alone, [ and ] make the picture chunkier or sharper (as L4 and R4 do).
  if (!hosted && !lockOn && (e.code === 'BracketLeft' || e.code === 'BracketRight') && !typing() && !e.metaKey && !e.ctrlKey) {
    view.density?.(e.code === 'BracketRight' ? 1 : -1);
    e.preventDefault();
    return;
  }
  // 🧰 ` anywhere else opens the debugging panel, bottom right (not while typing).
  if (e.code === 'Backquote' && !e.repeat && !typing()) {
    statsShown = !statsShown;
    try { localStorage.setItem('rax-stats', statsShown ? '1' : ''); } catch {}
    e.preventDefault();
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
  // 🚁 Looking on, V steps back to see your own drone.
  if (seat < 0 && e.code === 'KeyV' && !e.repeat && !typing()) droneView = !droneView;
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
  const kept = parseFloat(localStorage.getItem('rax-boom'));
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
  boomSaveTimer = setTimeout(() => { try { localStorage.setItem('rax-boom', boomLength.toFixed(2)); } catch {} }, 400);
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
// prompt's own path and clock (prompt.mjs raxFlight "back": 360 ms, ease-out,
// to 14 px in from the bottom-right), in buffer pixels from the centre, y up.
function reticleShift(now) {
  if (!closingAt) return null;
  const t = Math.min(1, (now - closingAt) / 360), ease = 1 - (1 - t) * (1 - t);
  const px = canvas.width / Math.max(1, canvas.clientWidth);
  const hx = canvas.width / 2 - 22 * px, hy = -(canvas.height / 2 - 22 * px);
  return { x: Math.round(hx * ease), y: Math.round(hy * ease) };
}
// 🧊 Stalls (stalls.mjs): gaps in the picture and what filled them, reported
// as numbers to /rax/stalls and shown beside the fps readout.
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
  const carried = !player && !!held;
  const changedLife = !player || own.life !== player.life;
  const result = reconcile(own, pending, data.acks[seat], solids(), data.tick, fixedBoxes(), flat(), openEdges(), half());
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
  if (changedLife && !carried) { yaw = own.yaw; pitch = own.pitch; }
  diagnostics.max('reconcileMaxMs', performance.now() - began);
  diagnostics.max('pendingCommandsMax', pending.length);
}
function frame(now) {
  bootMark('frame');
  // Schedule first: one bad frame must never stop the loop.
  requestAnimationFrame(frame);
  // 🧊 A gap since the last frame, while someone is in the room looking.
  stalls.begin(now, !document.hidden && document.hasFocus() && (hosted ? sheetShown : true) && !resting, {
    building: jobs.length, objects: objects.length, pending: pending.length,
    rtt: Number.isFinite(connection.rtt) ? connection.rtt : undefined,
    backend: canvas.raxStats?.backend, agent: AGENT, hosted,
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
    console.error('rax frame', error);
  }
  stalls.end({ work: performance.now() - began, render: canvas.raxStats?.cpuMs || 0, predict: predicted });
  const gpu = canvas.raxStats?.gpu;
  if (gpu?.max) {
    stalls.gpu(gpu.max, gpu.queued);
    gpu.max = 0;
  }
  if (diagnostics.enabled) diagnostics.frame({ visible: !document.hidden && sheetOpen,
    workMs: performance.now() - began, renderMs: canvas.raxStats?.cpuMs,
    backend: canvas.raxStats?.backend, objects: objects.length, triangles: canvas.raxStats?.triangles,
    rttMs: connection.rtt, jitterMs: connection.jitter, queuedBytes: ws?.bufferedAmount || 0,
    buildIds: jobs.map((j) => j.id) });
}
function step(now) {
  const elapsed = Math.max(0, (now - last) / 1000);
  const pads = navigator.getGamepads?.();
  // A controller should just work: if it's touched while the game is in the
  // background, ask the rig to bring the game to the front.
  if (!document.hasFocus() && window.raxRig?.claimFocus && now - claimedAt > 1000) {
    const touched = Array.from(pads || []).some(
      (p) => p?.connected && (p.buttons.some((b) => b.pressed) || p.axes.some((a) => Math.abs(a) > 0.5)),
    );
    if (touched) {
      claimedAt = now;
      window.raxRig.claimFocus();
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
  // 🎮 L4 and R4 (a pad's extra buttons) change the pixel density, as ⌘− and
  // ⌘+ do: of the extras this pad has shown, the lowest is chunkier and any
  // other sharper. Each is named in the log the first time, to map them by.
  for (const i of gamepad.extras || []) {
    if (!extrasSeen.has(i)) { extrasSeen.add(i); logNet(`controller: extra button ${i}`); console.info(`🎮 rax extra button ${i}`); }
    const step = extrasSeen.size > 1 && i === Math.min(...extrasSeen) ? -1 : 1;
    if (hosted) toHost({ type: 'rax:sheet:density', step });
    else view.density?.(step);
  }
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
    if (gamepad.select) chooseMenu();
    if (gamepad.cancel) closeMenu();
    gamepad = { ...gamepad, select: false, cancel: false, jump: false, use: false, ring: false, prompt: false, fwd: 0, right: 0, lookX: 0, lookY: 0 };
  }
  // 🛗 In the car the d-pad rides a floor up or down.
  if (!menu && seat >= 0 && hasCar() && inCar(player) && (gamepad.dup || gamepad.ddown)) send({ type: 'elevator', to: gamepad.dup ? 'up' : 'down' });
  const pad = touch.sample(canvas.clientWidth, canvas.clientHeight, now);
  for (const event of pad.events) {
    if (event.type === 'tap' && radial.open && event.menu) {
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
    if (seat < 0 && gamepad.camera) droneView = !droneView;
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
        boost: gamepad.boost || gamepad.hold, // the right trigger, or A held
      },
      elapsed,
      // 🌬️ The air where the eye is, in the world's metres at this moment.
      weather ? airAt(weather, freecam.x + (placeOf(room)?.x || 0), freecam.y, freecam.z + (placeOf(room)?.z || 0), Date.now() / 1000, { day: sun(skyPhase()) }) : null,
    );
  // 👀 Someone looking on tells the room where their eye is, a few times a
  // second and only when it has moved, so maps can show who is overhead.
  if (seat < 0 && connected && !joining() && sheetOpen && now - lookAt > 100) { // (ten times a second: the room draws it as a drone)
    const said = `${freecam.x.toFixed(1)} ${freecam.y.toFixed(1)} ${freecam.z.toFixed(1)} ${Math.round(yaw / 5)}`;
    if (said !== lookSaid) {
      lookSaid = said;
      lookAt = now;
      send({ type: 'look', x: freecam.x, y: freecam.y, z: freecam.z, yaw });
    }
  }
  const probe = connection.probe(
    performance.now(),
    ws?.readyState === WebSocket.OPEN && ws.bufferedAmount < 2048,
  );
  if (probe) send(probe);
  const firstSequence = seq;
  while (debt >= DT) {
    debt -= DT;
    // 🏃 The body answers its own keys at once, whatever the link is doing:
    // it runs ahead of the server's word by up to HORIZON commands, through
    // a quiet link and between two rooms (carrying), and stops only there.
    const carrying = !!held && !player;
    if (carrying ? pending.length >= HORIZON : !connected || seat < 0 || !player || pending.length >= HORIZON) {
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
    // 🛗 The car is moving: stand still and look out.
    if (rideMoving(performance.now())) Object.assign(c, { fwd: 0, right: 0, buttons: 0 });
    if (flat()) {
      // Side-on: A/D (or the stick) walk along the stage; the body turns to face its way.
      // Up jumps and down ducks (the arrows as well as WASD; the stick too).
      const typingNow = prompt.open || chat.open;
      const arrows = typingNow ? 0 : Number(keys.has('ArrowRight')) - Number(keys.has('ArrowLeft'));
      const along = clamp((c.right || 0) + arrows, -1, 1), lift = c.fwd || 0;
      if (along) flatYaw = along > 0 ? 90 : 270;
      yaw = flatYaw;
      pitch = 0;
      const up = !typingNow && (keys.has('ArrowUp') || keys.has('KeyW')), down = !typingNow && (keys.has('ArrowDown') || keys.has('KeyS'));
      // A held key jumps once: it must come up before it jumps again, as space does.
      const jump = (up || lift > 0.6) && !flatJumpHeld;
      flatJumpHeld = up || lift > 0.6;
      Object.assign(c, { yaw, pitch, fwd: Math.abs(along), right: 0, fire: false, buttons: c.buttons || jump ? 1 : 0, duck: down || lift < -0.6 });
    }
    jumpQueued = fireQueued = useQueued = dropQueued = false;
    pending.push(c);
    if (carrying) {
      held = { ...move(held, c, solids(), 0, fixedBoxes(), flat(), openEdges(), half()), walk: held.walk, until: held.until };
      presentation.step(held);
      continue;
    }
    if (snapshot.phase === 'playing' || snapshot.phase === 'waiting')
      player = move(player, c, solids(), (snapshot.tick + pending.length) / HZ, fixedBoxes(), flat(), openEdges(), half());
    presentation.step(player);
  }
  // Every command reaches the server once, in order, in batches of twelve
  // (match.mjs INPUT_BATCH): what a slow link held back goes when it clears.
  if (seat >= 0 && player && pending.length && pending.at(-1).seq > sentSeq) {
    const due = pending.filter((c) => c.seq > sentSeq);
    for (let i = 0; i < due.length; i += 12) {
      const part = due.slice(i, i + 12);
      if (!send({ type: 'input', commands: part })) break;
      sentSeq = part.at(-1).seq;
    }
  }
  if (performance.now() - worldAt > 6000 && !document.hidden) void streamWorld();
  tendWarm();
  if (park?.neighbours && player && seq !== firstSequence) {
    // A home is its lot: walking over a front is only walking. Off a side
    // with a neighbour, the next block takes the walker.
    if (now > walkAt) {
      const edge = edgeAt(player);
      if (edge && park.neighbours[edge]) { walkEdge = edge; send({ type: 'walk', edge }); walkAt = now + 2500; } // (the server keeps the request and answers when the body is there)
    }
  }
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
    let nearest = wallDistance(origin, dir, 60, [], 0, half());
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
  const shownPlayers = connected ? Array.from({ length: snapshot?.players?.length || 0 }, (_, i) => interpolate(history, tick, i)) : [];
  // 🤖 Drones glide between the two snapshots around the playback tick.
  const shownDrones = (() => {
    if (!connected || !snapshot.drones?.length) return [];
    let a = history[0], b = history.at(-1);
    for (const snap of history) {
      if (snap.tick <= tick) a = snap;
      if (snap.tick >= tick) { b = snap; break; }
    }
    const t = a.tick === b.tick ? 0 : clamp((tick - a.tick) / (b.tick - a.tick), 0, 1);
    return (b.drones || []).map((d) => {
      const p = a.drones?.find((o) => o.id === d.id);
      return p && Math.abs(p.x - d.x) < 1 ? { ...d, x: p.x + (d.x - p.x) * t } : d;
    });
  })();
  // 🛗 The car's height over (or under) its floor this frame; a bell on arrival.
  if (seat >= 0 && player && !('seat' in boot)) {
    bootMark('seat');
    const line = `way in: module ${boot.module}, view +${boot.view - boot.module}, connect ${boot.connect}, open +${boot.open - boot.connect}, welcome +${boot.welcome - boot.open}, seat +${boot.seat - boot.welcome} (ms)`;
    logNet(line, { good: true });
    console.info(`⏱️ rax ${line}`);
  }
  if (shownAt !== null && seat >= 0 && player) {
    const ms = Math.round(now - shownAt);
    shownAt = null;
    logNet(`paper away → body there in ${ms} ms`, { good: true });
    console.info(`⏱️ rax tab in: ${ms} ms`);
  }
  const lift = liftAt(now), sky = plazaFrom();
  // 🛗 The car is heard: its hum and the shaft's wind follow how fast it goes.
  sfx.lift(ride && liftWas ? Math.min(1, Math.abs(lift - liftWas.lift) / Math.max(0.001, (now - liftWas.at) / 1000) / 14) : 0);
  liftWas = { lift, at: now };
  if (held && ((seat >= 0 && player) || now > held.until)) {
    if (crossing) {
      const ms = Math.round(now - crossing.at);
      logNet(player ? `crossed ${crossing.from} → ${crossing.to} in ${ms} ms` : `crossing ${crossing.from} → ${crossing.to} gave up after ${ms} ms`, player ? { good: true } : { bad: true });
      console.info(`🫧 rax seam ${crossing.from} → ${crossing.to}: ${ms} ms${player ? '' : ' (no seat)'}`);
      if (player) send({ type: 'seam', from: crossing.from, ms });
      crossing = null;
      crossedAt = now;
    }
    if (!player) passage = null;
    held = null;
  }
  if (ride && !ride.rung && !rideMoving(now)) { ride.rung = true; sfx.ringMove(12); }
  // (Between two rooms the digits wait for the body to be set down.)
  if (dial.text && now - dial.at > DIAL_MS && !held) {
    if (seat >= 0 && player && hasCar() && inCar(player)) send({ type: 'elevator', to: dial.text });
    dial = { text: '', at: 0 };
  }
  const rideNow = ride && now - ride.at < ride.seconds * 1000 + 2500
    ? { ...ride, moving: rideMoving(now), floor: floorAt(altitude(ride.to) + lift + 0.5) } : null;
  // 🎬 The director's camera, when a film is being shot. A shot that `rides`
  // is in the car's frame: it goes up and down with it.
  filmPlayers = shownPlayers;
  filmRide = rideNow;
  const filmShot = film?.camera(now) || null;
  if (filmShot) { freecam = { ...freecam, x: filmShot.x, y: filmShot.y + (filmShot.rides ? lift : 0), z: filmShot.z }; yaw = filmShot.yaw; pitch = filmShot.pitch; }
  tendScreens(now);
  view.render({
    half: half(),
    seam: { crossing: !!held || now - crossedAt < 400, shown: query.has('seams') }, // the lines between rooms are walked through unseen; ?seams draws them
    weather,
    eyes: [...(sheetOpen && drones.size ? [...drones.values()].filter((d) => now - d.at < 1500).map((d) => droneAt(d, now)) : []),
      ...(droneView && seat < 0 && !held && !filmShot ? [{ name: '', x: freecam.x, y: freecam.y, z: freecam.z, yaw, vx: freecam.vx || 0, vz: freecam.vz || 0 }] : [])],
    place: placeOf(room), // 📍 this room's corner of the world, so where you stand is told in the world's metres
    beyond: sheetOpen && beyond.size ? [...beyond.values()].filter((b) => now - b.at < 1500).map((b) => beyondAt(b, now)) : [],
    ball: ball && now - ball.at < 4000 ? { ...ballAt(ball, (now - ball.at) / 1000), vx: ball.vx, vz: ball.vz } : null,
    drones: shownDrones,
    // Every seat the room holds, not only the original duel's two. Whoever
    // stands in the car while it runs rides with it.
    players: lift ? shownPlayers.map((p) => (p && inCar(p) ? { ...p, y: p.y + lift } : p)) : shownPlayers,
    me: seat >= 0 && player ? { ...displayed, y: displayed.y + lift, yaw, pitch, duck: !!player.duck, onGround: player.onGround, vy: player.vy }
      : held ? { ...held, ...(displayed || {}), y: (displayed || held).y + lift, yaw, pitch } : seat < 0 ? null : spawn(seat, 1, half()),
    debug: !!game() && debugBoxes,
    camera: game()?.camera ? gameCamera(displayed || player, shownPlayers, elapsed) : seat < 0 && !held ? { ...freecam, ...(droneView && !filmShot ? droneBack() : {}), yaw, pitch, ...(filmShot ? { zoom: filmShot.zoom } : {}) } : null,
    spectator: seat < 0 && !joining(),
    gate: seat < 0 ? gate : null,
    controller: controller.id,
    stats: statsShown,
    sticks: controller.id && seat < 0 && !held && gamepad ? { move: [gamepad.right || 0, -(gamepad.fwd || 0)], look: [gamepad.lookX || 0, gamepad.lookY || 0], buttons: controller.previous || [] } : null,
    seat,
    third: third || !!game()?.camera,
    game: game(),
    bops: snapshot?.bops || [],
    combos: snapshot?.combos || [],
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
      // The elevator's line is typed on the car's screen, not the top line.
      // The elevator's line shows up here too: the car's screen can be out of frame.
      open: chat.open,
      text: chat.text,
      cursor: chat.cursor,
      lines: chatLog.filter((l) => chat.open || now - l.at < CHAT_SHOWN_MS),
      lift: liftLine && chat.open,
    },
    tower: hasCar() ? { ...(tower || { here: null }), lift, screen: carScreen(rideNow) } : null,
    skyline: sky && towerTop ? { top: towerTop, ...sky, count: tower?.count || towerTop, board: room === 'lobby' ? lobbyBoard() : null } : null,
    car: hasCar() && seat >= 0 && inCar(player) && !rideMoving(now),
    plainTags: !!film,
    ride: rideNow,
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
    touchZones: shownZones(),
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
    park,
    around: around(),
    field: groundsOf(room),
    home: home && home.room === room ? home.lot : null,
    floorAim: focusObject || !sheetOpen || game()?.hud?.aim === false ? null : floorAim,
    chalk: chalkStrokes,
    chalkVersion,
    chalkLocal,
    chalkMode,
    face: seat >= 0 && player && game()?.hud?.face !== false ? { ...face, health: player.health, alive: player.alive, seat, pitch, turn: gamepad.lookX + (keys.has('KeyA') || keys.has('ArrowLeft') ? -0.5 : 0) + (keys.has('KeyD') || keys.has('ArrowRight') ? 0.5 : 0) } : null,
    devLog: sheetOpen ? devLog.filter((d) => now - d.at < 120000).map((d) => ({ ...d, age: (now - d.at) / 1000 })) : [],
    owner,
    focusObject,
    builder,
    builderNames,
    netLog,
    connected: !down, // connecting counts; a dead or long-silent socket shows the log
    offline: sheetOpen && down,
    // 📟 The server quiet for a moment (not yet an outage), or the page just back on new code.
    wire: !sheetOpen || down ? null
      : ws?.readyState === WebSocket.OPEN && connection.lastSnapshot !== null && now - connection.lastSnapshot > 400 && !held
        ? { text: `no word from the server ${((now - connection.lastSnapshot) / 1000).toFixed(1)} s`, color: 0xffe066 }
        : rebooted && now < rebooted.until ? { text: rebooted.text, color: 0x3fb950 } : null,
    cursor: cursorAim ? mouseAt : null,
    reticle: hosted && sheetOpen && !down,
    reticleShift: reticleShift(now),
    orbit: orbitStep(now),
    cornerScale,
    flyTo: seat < 0 && hosted && sheetOpen ? gate ? { ...freecam, yaw } : spawn(lastSeat >= 0 ? lastSeat : 0, 1, half()) : null,
    jobs,
    worldTime,
    locked: locked(),
    legend: sheetOpen && sheetShown && seat >= 0 && legendShown(),
    corner: sheetOpen && !down && !film,
    screens,
    screenPrograms,
    bare: !sheetOpen || !sheetShown || down || !!film,
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
  film?.frame(now);
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
    if (e.source !== window.parent || e.origin !== location.origin || !['rax:sheet', 'dool:sheet'].includes(e.data?.type)) return;
    // The prompt's pixel grid rides along: our pixels become its pixels.
    if ('grid' in e.data) view.grid(e.data.grid);
    // And its theme: a light prompt sits over the day room, a dark one over the night.
    if ('dark' in e.data) view.theme(e.data.dark);
    // A room asked for by address (aesthetic.computer/botbop, rax~room).
    if (typeof e.data.room === 'string') travel(e.data.room);
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
    if (sheetOpen && !(seat >= 0 && player)) shownAt = performance.now();
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
  toHost({ type: 'rax:sheet:hello' });
  toHost({ type: 'rax:sheet:place', path: placePath() });
} else connect();
requestAnimationFrame(frame);
