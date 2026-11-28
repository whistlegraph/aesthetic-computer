import { BASEMENT, DEPTH, cellarFixtures } from './basement.mjs';
import { edgeAt, SPAN, groundsOf, cellOf, wrap } from './park.mjs';
import { airAt, sun } from './air.mjs';
import { flyover } from './flyover.mjs';
import { PLANET } from './planet.mjs';
import { phase as skyPhase, sunTimes } from './sky.mjs';
import { rideable } from './bike.mjs';
import { hungFor } from './orbits.mjs';
import { NAME as BOP_NAME } from './bop.mjs';
import { towerSides, storeyWindows, carWalls, lobbyWalls, inCar, clearOfCar, altitude, floorAt, listFloors, rideSeconds, rideEase, CAR_PAD, roofOf, shutTicks, storeyOf, ROOF, aloft, roofWalls, atriumRoof } from './elevator.mjs';
import { createNotepatScreen } from './notepat-screen.mjs';
import { kioskPlan, kioskSigns, KIOSK_DEEP } from './kiosks-plan.mjs';
import { createPanels, panelHit, fingertipHit, PANEL_REACH } from './panel.mjs';
import { dockPose, boxStandoff, boxMiddle, bearingFrom, glide, smooth, cursorStep, rates } from './dock.mjs';
import { PLANTERS } from './elevator.mjs'; // (the courtyard's trees, for where an eye may stand)
import * as keypad from './pieces/keypad.mjs';
import { gameFor } from './games.mjs';
import { HZ, DT, PROTOCOL, HORIZON, move, spawn, clamp, CHALK, playURL, ONE_ROOM, placeFor } from './world.mjs';
import { reconcile, interpolate } from './net.mjs';
import { Rounds } from './rounds.mjs';
import { LocalPresentation, SnapshotClock, RemoteEase } from './presentation.mjs';
import { Prompt } from './prompt.mjs';
import { objectBoxes } from './objects.mjs';
import { direction, rayBox, wallDistance, along, SCREEN, WORLD, halfOf } from './world.mjs';
import { createView } from './view.mjs';
import { Connection } from './connection.mjs';
import { ClientDiagnostics } from './diagnostics-client.mjs';
import { Stalls } from './stalls.mjs';
import { accountToken, accountExpected, askAccount } from './account.mjs';
import { createPairDesk } from './pair-desk.mjs';
import { immersive } from './immersive.mjs';
import { Controller, neutralPad } from './controller.mjs';
import { createBuilds } from './build-line.mjs';
import { moveFreecam } from './freecam.mjs';
import { createAgentSurface } from './agent.mjs';
import { Radial, HOLD_SECONDS, ringGeometry } from './radial.mjs';
import { TouchPad, touchButtons, bikeButtons } from './touch.mjs';
import { gyroLook } from './gyro.mjs';
import { Sfx, SpatialTones } from './sfx.mjs';
// 🐌 KidLisp is some thirty modules deep (its runtime and AC's graphics): it
// was fetched before the room could start at all, about 0.4 s of the way in
// on a cold cache (measured 2026-11-19). Now it comes when the first screen
// or wall asks for a program, and the room is up before it.
const kidlispWall = (...given) => import('./kidlisp-wall.mjs').then((m) => m.kidlispWall(...given));
import { MAX_FOCUS_ZOOM, screenRows, carryPixels } from './screen-lod.mjs';
import { ballAt, DISPENSER_WALL } from './ball.mjs';
import { sourceTones, beatAt } from './object-language.mjs';
let farShots = []; // 🌐 rounds in the rooms around (beyond)
const rounds = new Rounds(); // 🔫 Rounds in flight, flown on between snapshots and fired at once (rounds.mjs).

// ⏱️ How long the way in takes, once: when this module ran, when the view
// was ready, the first connect, the socket open, the welcome, the seat and
// the first frame, in ms since the page began (window.raxBoot; one line in
// the console and the connection's log when the seat is had). Each time the
// paper is put away, how long until the body is there is logged too.
const boot = {};
const bootMark = (name) => { if (!(name in boot)) boot[name] = Math.round(performance.now()); };
try { window.raxBoot = boot; } catch {}
bootMark('module');
// 🍎 Standing alone in AC's native app (no prompt page around it), rax says it
// is alive and then that it is there, as AC's own boot does. The app watches
// every page it loads for that word and loads the page again if 25 seconds
// pass without it: until 2026-11-20 rax never said it, so the app took a rax
// opened on its own for a page that had hung and loaded it again and again,
// and whoever was in it froze and was set down afresh each time.
const tellApp = (type) => { try { if (window.parent === window) window.webkit?.messageHandlers?.iOSApp?.postMessage(JSON.stringify({ type })); } catch {} };
let appTold = false;
tellApp('boot:heartbeat');
const view = await createView(document.getElementById('game'));
bootMark('view');
const canvas = view.canvas,
  keys = new Set();
const query = new URLSearchParams(location.search);
// 🏷️ A room is asked for the way AC asks for anything: a word after `~`
// (rax.ac/~studio, as aesthetic.computer/rax~studio). `?room=` is still read
// (the prompt's paper loads the page that way) but never written.
const requestedRoom = decodeURIComponent(location.pathname).match(/\/~([a-z0-9-]{1,32})$/)?.[1] || query.get('room');
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
// ⎋ The step of Escape just taken, { color, at }: the view flashes the screen border.
let escFlash = null;
function leave() {
  // Escape's second step, to the prompt: a pink border and two notes falling.
  escFlash = { color: 0xff3296, at: performance.now() };
  sfx.escStep(2);
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
// (🏠 One room, world.mjs ONE_ROOM: any place in the world is the one room; a game or a named room is still itself.)
let room = placeFor(
  requestedRoom && /^[a-z0-9-]{1,32}$/.test(requestedRoom)
    ? requestedRoom
    : (accountExpected() && lastPlace) || 'lobby');
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
let balls = [];
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
// 🖼️ World panels (panel.mjs): pieces on screens in the room, worked with the
// crosshair as their mouse. The one there is: the pad at the back of the car
// (pieces/keypad.mjs). Its digits are the same `dial` the number keys press,
// but a key aimed at takes longer than a key under a finger, so the car waits
// PAD_MS after one of those (or goes at once on `go`).
const PAD_MS = 5000;
const panels = createPanels();
const carPad = panels.mount({
  id: 'car-pad', label: 'elevator keypad', piece: keypad, ...CAR_PAD,
  host: {
    car: {
      typed: () => dial.text,
      key: (digit) => { dial = { text: (dial.text + digit).slice(-4), at: performance.now(), wait: PAD_MS }; sfx.ringMove(Number(digit)); },
      clear: () => { dial = { text: '', at: 0 }; sfx.ringBack(); },
      go: selectFloor,
      floor: () => floorName(ride && rideMoving(performance.now()) ? floorAt(altitude(ride.to) + liftAt(performance.now()) + 0.5, [ride.to, ride.from]) : tower?.here?.number ?? 0), // (a number, or 'roof')
      home: () => home?.floor || 0,
      moving: () => rideMoving(performance.now()),
    },
  },
});
// 🧩 Real AC pieces in the yard, to try them in the world: two `bleep`s side
// by side on the lobby's front, west of the door, each under a sign that says
// what it is. The disks are AC's own (ac-piece.mjs); they are kiosks: a
// press is told to the room (`panel`), so everyone there sees the pads go
// down and hears the tone from where the kiosk stands, and each piece rolls
// its dice from its place's name so the pads are the same on every page.
// Every held tone stops when the room is left.
// Inside, the song hall: the songs AC knows (disks/song.mjs), one to a kiosk in a row along the back of the lobby's front glass, under a sign that says `songs`: tap a song's big button through its syllables, or `play` it as a guide on the room's clock.
// 🗺️ Which kiosks stand where is the world's to say, not this page's: kiosks-plan.mjs works their places out from the tower's measurements, and the server takes a press only on one of them.
const yardPanels = [], yardVoices = new Set();
// 🧩 The server keeps what has been done to each piece (kiosks.mjs): `yardKept` is its last word, and each panel counts the presses and lets-go its own copy has had (`done`). The difference is done silently, so a page that comes later stands where everyone's stands.
let yardKept = null, yardMuted = false;
const QUIET = { kill() {}, update() {} };
// 🔇 A note cannot be left sounding. Every voice a kiosk's piece starts is kept with whose pointer it was started under, and that pointer's letting go ends it whatever the piece made of the event (a lift that came from outside the panel, or from someone who left the room, used to leave a pad droning: one did for eight minutes, 2026-11-20). And a kiosk nobody has touched for a quarter of a minute is hushed: a held note needs a hand on it.
let yardActing = null; // { id, pointer } while a panel's piece is being told of a pointer
const yardTouched = new Map(); // panel id → when it was last worked
const KIOSK_HUSH_MS = 15000;
const yardPointers = new Map();
function kioskAudible(id, duration) {
  return (Number.isFinite(duration) || yardPointers.get(id)?.size > 0) &&
    room === 'lobby' && sheetOpen && !document.hidden && !yardMuted &&
    performance.now() - (yardTouched.get(id) ?? -Infinity) < KIOSK_HUSH_MS;
}
function yardVoice(id, held, duration) {
  const hands = yardPointers.get(id);
  const pointer = yardActing?.id === id ? yardActing.pointer : hands?.size === 1 ? hands.values().next().value : null;
  const voice = { id, pointer, held: !Number.isFinite(duration), update: (to) => held.update?.(to), kill: (fade) => { yardVoices.delete(voice); held.kill(fade); } };
  yardVoices.add(voice);
  return voice;
}
function tendKiosk(panel) {
  const { id } = panel, act = panel.act, hear = panel.hear;
  const hands = new Set();
  yardPointers.set(id, hands);
  const under = (pointer, name, tell) => {
    if (!yardMuted && name === 'touch') hands.add(pointer);
    if (name === 'lift') hands.delete(pointer);
    yardActing = { id, pointer };
    if (!yardMuted && name !== 'move') yardTouched.set(id, performance.now());
    try { tell(); } finally { yardActing = null; }
    if (name === 'lift') for (const v of [...yardVoices]) if (v.id === id && v.held && (v.pointer === pointer || !hands.size)) v.kill(0.06);
  };
  panel.act = (name, px, py, device, pointer = 1) => under(pointer, name, () => act(name, px, py, device, pointer));
  panel.hear = (name, px, py, pointer) => under(pointer, name, () => hear(name, px, py, pointer));
  // ⌨️ A key held on it is a hand on it, as a pointer is: its note is heard while it is down and ended when it comes up.
  const key = panel.key;
  panel.key = (way, e) => under(`key:${e.code}`, way === 'down' ? 'touch' : 'lift', () => key.call(panel, way, e));
  return panel;
}
// ↻ A kiosk's piece begun again from nothing: a fresh load of its disk boots on the same screen, its voices are ended, and its count of what has been done to it starts over. Asked for by pressing its name (`tell`: the room is told, and the server forgets what was done to it), heard from someone else who did, or found on coming in (the server's `epoch` for it is past this page's).
async function resetKiosk(id, tell = false, epoch = null) {
  const panel = yardPanels.find((p) => p.id === id);
  if (!panel?.fresh || panel.renewing) return;
  panel.renewing = true;
  yardPointers.get(id)?.clear();
  yardTouched.delete(id);
  for (const v of [...yardVoices]) if (v.id === id) v.kill(0.05);
  try {
    const next = await panel.fresh();
    panel.renew(next);
    await next.ready?.();
  } catch (e) { console.warn(`rax: ${id} did not begin again:`, e?.message || e); }
  panel.done = 0;
  panel.epoch = epoch ?? (panel.epoch || 0) + 1;
  panel.renewing = false;
  sfx.ringBack();
  if (tell && seat >= 0 && room === 'lobby') send({ type: 'panel', id, name: 'reset', x: 0, y: 0 });
  else catchUp(); // (what has been done to it since, if this page was behind)
}
function hushIdleKiosks(now) {
  for (const v of [...yardVoices]) if (now - (yardTouched.get(v.id) ?? -Infinity) > KIOSK_HUSH_MS) v.kill(0.4);
}
function catchUp() {
  if (!yardKept) return;
  yardMuted = true;
  try {
    for (const panel of yardPanels) {
      const kept = yardKept[panel.id];
      if (!kept || !panel.hear || panel.id === 'yard-notepat') continue;
      // (Begun again by someone while this page was away: begin it again here first, then do what has been done since.)
      if ((kept.epoch || 0) > (panel.epoch || 0)) { if (panel.fresh) { void resetKiosk(panel.id, false, kept.epoch); continue; } panel.epoch = kept.epoch; }
      const todo = kept.total > (panel.done || 0) ? kept.events.slice(-Math.min(kept.events.length, kept.total - (panel.done || 0))) : [];
      for (const [name, x, y, seat, finger] of todo) panel.hear(name, x, y, remotePointer(seat, finger));
      panel.done = Math.max(panel.done || 0, kept.total);
    }
  } finally { yardMuted = false; }
}
let yardAsked = false;
const yardQuiet = () => { yardTouched.clear(); for (const hands of yardPointers.values()) hands.clear(); for (const v of [...yardVoices]) v.kill(0.05); };
function sharePanel(panel) {
  const act = panel.act, toldAt = new Map();
  panel.act = (name, x, y, device, finger = 1) => {
    act(name, x, y, device, finger);
    if (name === 'move' || seat < 0 || room !== 'lobby') return;
    const now = performance.now();
    if (name === 'draw' && now - (toldAt.get(finger) ?? -Infinity) < 50) return;
    if (name === 'draw') toldAt.set(finger, now);
    if (name === 'lift') toldAt.delete(finger);
    if (name !== 'draw') panel.done = (panel.done || 0) + 1;
    send({ type: 'panel', id: panel.id, name, finger, x: Math.round(x), y: Math.round(y) });
  };
}
const remotePointer = (seat, finger = 1) => 16 * (seat + 1) + finger;
async function mountYard() {
  if (yardAsked) return;
  yardAsked = true;
  try {
    const { acPiece, signPiece } = await import('./ac-piece.mjs');
    const plan = kioskPlan('lobby');
    for (const { id, piece: slug, x, y, z, yaw, sign, ...face } of plan) {
      if (id === 'yard-notepat') continue; // Its screen comes from the server.
      const sound = { synth: ({ type, tone, duration }) => {
        if (!kioskAudible(id, duration)) return QUIET;
        return yardVoice(id, sfx.hold(Number(tone), { type, at: { x, y, z } }), duration);
      } };
      const fresh = () => acPiece(slug, { sound, seed: id, clock: () => Date.now() + (clockOffset || 0) });
      const { piece, host } = await fresh();
      const panel = tendKiosk(panels.mount({ id, label: slug, completes: true, piece, host, x, y, z, yaw, ...face, deep: KIOSK_DEEP }));
      // 👥 What this page's pointer does on it is told to the room: a press, a slide while down (twenty a second at the most), a letting go.
      sharePanel(panel);
      panel.fresh = fresh;
      yardPanels.push(panel);
      if (sign) yardPanels.push(panels.mount({ id: `${id}-sign`, piece: signPiece(slug, () => void resetKiosk(id, true)), x, y: y + face.h / 2 + 0.17, z, yaw, w: face.w, h: 0.2, cols: 96, rows: 16, deep: 0.06 }));
    }
    // 🪧 And over a group of them, what they are (the song hall's `songs`).
    for (const { text, ...at } of kioskSigns('lobby')) yardPanels.push(panels.mount({ piece: signPiece(text), ...at, deep: 0.06 }));
    const boardSpec = plan.find(p => p.id === 'yard-notepat');
    const board = panels.mount({ ...boardSpec, label: 'notepat', piece: { keys: true }, completes: true });
    notepatScreen.mount(board);
    sharePanel(board);
    yardPanels.push(board);
    const reset = () => send({ type: 'panel', id: board.id, name: 'reset', x: 0, y: 0 });
    yardPanels.push(panels.mount({ id: board.id + '-sign', piece: signPiece('notepat', reset),
      x: board.x - (board.w / 2 - 0.6), y: board.y + board.h / 2 + 0.17, z: board.z,
      yaw: board.yaw, w: 1.2, h: 0.2, cols: 96, rows: 16, deep: 0.06 }));
    catchUp();
  } catch (e) { console.warn('rax: the yard\'s pieces did not load:', e?.message || e); }
}
const screenFingers = new Map(); // Browser pointer id → captured screen and piece pointer.
// 🤖 Agents linked to this handle (the presence relay AC's own pages join: lib/agent-presence.mjs). rax takes a place in the handle's room itself and draws the mark after the name, on its own grid: no page element over the picture.
const agentLink = { room: null, socket: null, agents: 0, tryAt: 0 };
function linkAgents(handle, now) {
  const room = String(handle || '').toLowerCase().replace(/^@/, '');
  const fits = /^[a-z0-9_-]{1,32}$/.test(room) && !/^(localhost|127\.)/.test(location.hostname);
  if (agentLink.room !== (fits ? room : null)) {
    try { agentLink.socket?.close(); } catch {}
    Object.assign(agentLink, { room: fits ? room : null, socket: null, agents: 0, tryAt: 0 });
  }
  if (!agentLink.room || agentLink.socket || now < agentLink.tryAt) return;
  agentLink.tryAt = now + 30000;
  let socket;
  try { socket = new WebSocket(`wss://session-server.aesthetic.computer/agent-presence?room=${agentLink.room}&role=surface`); } catch { return; }
  agentLink.socket = socket;
  socket.onmessage = (e) => {
    try { const said = JSON.parse(e.data); if (said.type === 'agent-presence:status' && agentLink.socket === socket) agentLink.agents = (said.content?.agents || []).length; } catch {}
  };
  socket.onclose = () => { if (agentLink.socket === socket) { agentLink.socket = null; agentLink.agents = 0; } };
  socket.onerror = () => { try { socket.close(); } catch {} };
}
// LT sets the optical zoom. A click of the left stick doubles it for this screen.
let focusZoom = 1;
function focusStep(elapsed) {
  const active = !typing() && !menu && !radial.open && !chalkMode;
  if (active && dock?.way > 0 && gamepad.zoomClick) dock.extended = !dock.extended;
  const pressure = Math.max(gamepad.lt > 0.06 ? gamepad.lt : 0, Number(keys.has('KeyC')));
  const want = active ? (1 + (MAX_FOCUS_ZOOM - 1) * pressure) * (dock?.way > 0 && dock.extended ? 2 : 1) : 1;
  const was = focusZoom;
  focusZoom += (want - focusZoom) * (1 - Math.exp(-Math.min(0.1, elapsed) * 9));
  if (Math.abs(focusZoom - want) < 0.002) focusZoom = want;
  return elapsed > 0 ? Math.min(1, Math.abs(Math.log2(focusZoom / was)) / Math.min(0.1, elapsed)) : 0; // how fast the lens moves, for its whirr
}
let padHolds = false, keyHolds = false; // a panel held by A or RT on a pad, or by Enter
let lastMotionLook = null;
let sticksMoving = false;
// The tower's height in floors, as last heard: it stands on every skyline.
let towerTop = 0;
try { towerTop = Number(localStorage.getItem('rax-tower-top')) || 0; } catch {}
// 🏗️ How many floors stand, for the car's sign and its pad: the storeys built, which while the tower is short is fewer than the numbers given out.
const standing = () => storeyOf(tower?.top || towerTop);
// 🏬 And which of its floors have a storey of their own (elevator.mjs SHORT),
// kept beside it so the skyline is the right height before the server speaks.
try { listFloors(JSON.parse(localStorage.getItem('rax-tower-listed') || '[]')); } catch {}
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
// A short pause commits a run of taps. Holding either arrow keeps changing
// this same destination until the rider lets go.
let padFloorRepeat = 0, padFloorStart = 0;
function selectFloor(direction, heldMs = 0) {
  if (seat < 0 || !inCar(player)) return;
  const floors = [-1, 0, ...directory.keys(), ...(towerTop ? [towerTop + 1] : [])].sort((a, b) => a - b);
  const here = Number((dial.text === 'roof' ? towerTop + 1 : dial.text) || ride?.to || tower?.here?.number || 0);
  const next = heldMs >= 2000 ? (direction === 'up' ? floors.at(-1) : -1) : direction === 'up' ? floors.find(n => n > here) : floors.findLast(n => n < here);
  if (next === undefined || next === here) {
    if (dial.text) dial.at = performance.now();
    return;
  }
  dial = { text: next === towerTop + 1 && towerTop ? 'roof' : String(next), at: performance.now(), wait: 360 };
  sfx.ringMove(direction === 'up' ? 6 : 3);
}
// What the typed line would reach, said on the screen before Enter.
function floorAsked(text) {
  const q = text.trim().toLowerCase().replace(/^(?:floor|go)\s+/, '');
  if (!q) return 'number, @handle or take N';
  if (/^(take|claim)\b/.test(q)) return /^(?:take|claim)\s+#?\d{1,4}$/.test(q) ? 'enter: make it yours' : 'take which number?';
  if (['g', '0', 'ground', 'plaza', 'lobby'].includes(q)) return 'the plaza';
  if (['top', 'roof', 'the top', 'the roof', 'penthouse'].includes(q)) return 'the roof';
  if (['b1', 'basement', '-1'].includes(q)) return 'the basement';
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
// 🏙️ A floor's number as it is said: a number over the highest floor there is, is the roof's (elevator.mjs roofNumber).
const floorName = (n) => (n > 0 && tower?.top > 0 && n > tower.top ? 'roof' : n);
function carScreen(rideNow) {
  // Asked where to: the line being typed is on the screen itself.
  if (dial.text) return { number: dial.text, moving: true, handle: floorAsked(dial.text), note: 'going in a moment', entry: true };
  if (liftLine && chat.open) {
    const blink = Math.floor(performance.now() / 400) % 2 ? '_' : ' ';
    return { number: '?', moving: true, handle: `${chat.text.slice(0, 22)}${blink}`, note: floorAsked(chat.text), entry: true };
  }
  const moving = !!rideNow?.moving, n = moving ? rideNow.floor : tower?.here?.number ?? 0;
  // The floor you stand on is named by the server's last word, not the directory.
  const who = n ? (n === tower?.here?.number && tower.here.handle) || directory.get(n) : null, roof = floorName(n) === 'roof';
  return {
    number: n === -1 ? 'B1' : n ? `${floorName(n)}` : 'G',
    moving,
    handle: n === -1 ? 'basement' : roof ? 'open air' : n ? (who ? `@${who}` : 'empty') : 'plaza',
    note: moving ? `${rideNow.to > rideNow.from ? 'up' : 'down'} to ${floorName(rideNow.to) || 'G'}${rideNow.handle ? ` @${rideNow.handle}` : ''}`
      : n ? `${altitude(n)} m up` : standing() ? `${standing()} floors` : '',
  };
}
// Where the plaza (and the tower on it) stands from this room, in metres.
// 🌧️ What falls over Los Angeles falls here: asked of the server now and
// every five minutes (weather.mjs).
const placeOf = (id) => { const c = cellOf(id); return c ? { x: c.i * SPAN, z: c.j * SPAN } : null; };
let weather = null;
// 👀 The people in the rooms around this one (the server's `beyond`), eased
// from where each was last drawn to where they were last heard of.
// 🛫 The idle flyover (flyover.mjs): the tour, where on it this drone is, and when anyone last touched a control.
const IDLE_MS = 25000;
let tour = null, touring = null, inputAt = performance.now();
let idleEntry = null, droneIdleWeight = 0;
for (const event of ['keydown', 'pointerdown', 'pointermove', 'wheel', 'focus', 'visibilitychange'])
  window.addEventListener(event, () => { inputAt = performance.now(); }, { passive: true, capture: true });

function idlePlayerCamera(now, body) {
  if (!body?.alive || !sheetOpen || down || film || held || (idleEntry && idleEntry.room !== room)) {
    idleEntry = null;
    return null;
  }
  const idle = now - inputAt >= IDLE_MS && !document.hidden && document.hasFocus() &&
    !joining() && !dock && !lockOn && !menu && !typing() && !chalkMode && !rideMoving(now) && !game();
  if (!idle) {
    if (!idleEntry?.shot) return null;
    idleEntry.exitAt ??= now;
    const weight = idleEntry.shot.weight * (1 - smooth((now - idleEntry.exitAt) / 850));
    if (weight > 0) return { ...idleEntry.shot, weight };
    idleEntry = null;
    return null;
  }
  idleEntry ??= { room, yaw, at: now };
  const age = (now - idleEntry.at) / 1000;
  // A wide tracking arc, rising and easing back in over successive sweeps.
  const turn = idleEntry.yaw * Math.PI / 180 + age * 0.12;
  const distance = 7 + 5 * (1 - Math.cos(age * 0.13));
  const rise = 0.22 + 0.16 * (1 + Math.sin(age * 0.19));
  const len = Math.hypot(1, rise);
  const out = { x: -Math.sin(turn) / len, y: rise / len, z: -Math.cos(turn) / len };
  const clear = Math.max(2, Math.min(distance, airFrom(body, out, distance + 0.3) - 0.3));
  const target = {x: body.x + out.x * clear, y: body.y + out.y * clear, z: body.z + out.z * clear,
    yaw: turn * 180 / Math.PI, pitch: -Math.atan2(out.y * clear + 0.35, clear / len) * 180 / Math.PI};
  return (idleEntry.shot = { ...target, weight: smooth(age / 4), cinematic: true });
}
const roofNow = () => { try { return roofOf(towerTop) || 33; } catch { return 33; } };
// 🔭 What is worth a look in this room now, for the tour to find: the pieces on the panels, the screens, what people built, the ball, and whoever is about (those that move are followed as they go).
function sightsNow() {
  const out = [];
  for (const p of yardPanels) if (p.label && !p.hidden) out.push({ x: p.x, y: p.y, z: p.z, r: Math.max(p.w, p.h) / 2 });
  for (const s of screens) out.push({ x: s.x, y: 1.5, z: s.z, r: 2 });
  for (const o of objects) if (!o.item && Number.isFinite(o.x)) out.push({ x: o.x, y: 1, z: o.z, r: 1 });
  (snapshot?.players || []).forEach((p, i) => {
    if (p) out.push({ x: p.x, y: p.y + 1.2, z: p.z, r: 1, at: () => { const q = snapshot?.players?.[i]; return q ? { x: q.x, y: q.y + 1.2, z: q.z } : null; } });
  });
  for (const b of balls) out.push({ ...b, r: 0.6, at: () => balls.find(other => other.id === b.id) || null });
  return out;
}
// Where on the tour is nearest a place: it is joined there, not at its start.
function nearestOnTour(t, at) {
  let best = 0, least = Infinity;
  for (let s = 0; s < t.seconds; s += 0.5) { const p = t.at(s), d = Math.hypot(p.x - at.x, p.y - at.y, p.z - at.z); if (d < least) { least = d; best = s; } }
  return best;
}
let rolledAt = -Infinity;
let droneRoll = 0; // radians your own drone is rolled over
let beyond = new Map(), drones = new Map(), myEye = null, droneView = false; // droneView: looking on, from behind your own drone
// The eye a little behind and above your own drone, along the way it looks.
function droneBack() {
  const d = direction(yaw, pitch);
  const y = freecam.y - d.y * 1.6 + 0.35;
  return { x: freecam.x - d.x * 1.6, y: freecam.y < 0 ? Math.min(-0.2, y) : Math.max(0.2, y), z: freecam.z - d.z * 1.6 }; // (the eye keeps to the drone's side of the land: over it, or under it inside the planet)
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
const askWeather = () => fetch(new URL('./weather', import.meta.url)).then((r) => (r.ok ? r.json() : null)).then((w) => { if (w) { weather = w; sunTimes(w.rise, w.set); } }).catch(() => {});
void askWeather();
setInterval(askWeather, 5 * 60 * 1000);
function plazaFrom() {
  if (room === BASEMENT) return { dx: 0, dy: DEPTH, dz: 0 };
  if (room === 'lobby') return { dx: 0, dy: 0, dz: 0 };
  if (aloft(room)) return tower?.here?.number ? { dx: 0, dy: -altitude(tower.here.number), dz: 0 } : null; // (a floor, or the roof: its number is its height)
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
const hasCar = () => room === BASEMENT || room === 'lobby' || aloft(room);
// 📐 This room's half-width (world.mjs): the same number the server moves us by.
const half = () => halfOf(room);
const CAR_BOXES = carWalls(), LOBBY_BOXES = [...lobbyWalls(), DISPENSER_WALL, atriumRoof()], ROOF_BOXES = roofWalls(), WINDOW_BOXES = storeyWindows();
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
  if (aloft(room)) return towerSides;
  if (openFor !== park) {
    openFor = park;
    openSides = park?.neighbours ? Object.keys(park.neighbours).filter((k) => park.neighbours[k]) : null;
  }
  return openSides;
}
const NO_BOXES = [];
// 🪐 The planets hung over this room (orbits.mjs), as the server has them: over the plaza and the blocks, what a body there meets.
let hungFor_ = null, hungKept = NO_BOXES;
function hungHere() {
  if (hungFor_ !== room) { hungFor_ = room; const cell = cellOf(room); hungKept = cell ? hungFor(cell) : NO_BOXES; if (!hungKept.length) hungKept = NO_BOXES; }
  return hungKept;
}
function fixedBoxes() {
  if (game()?.ledges) return game().ledges;
  if (!hasCar()) return hungHere();
  // (Rebuilt when the room changes: only the plaza has the lobby, only the roof its parapet.)
  if (carBoxesFor !== room) { carBoxesFor = room; carBoxes = null; }
  return (carBoxes ||= [...hungHere(), ...CAR_BOXES, ...(room === BASEMENT ? cellarFixtures : []), ...(room === 'lobby' ? LOBBY_BOXES : room === ROOF ? ROOF_BOXES : aloft(room) ? WINDOW_BOXES : [])]);
}
// The blocks beside the one underfoot, placed in metres from its middle.
function around() {
  const sky = plazaFrom();
  if (!world || !sky) return [];
  const here = cellOf(room) || { i: 0, j: 0 };
  return world.blocks.filter(b => b.id !== room).map(b => {
    const cell = cellOf(b.id);
    const di = wrap(cell.i - here.i), dj = wrap(cell.j - here.j);
    return { ...b, dx: di * SPAN, dz: dj * SPAN, dy: sky.dy,
      sceneryOnly: b.sceneryOnly || Math.abs(di) > 1 || Math.abs(dj) > 1,
      home: home && home.room === b.id ? home.lot : null };
  }).concat(floorsAround(sky.dy).map(f => ({ ...f, dx: sky.dx, dz: sky.dz })));
}
// 🏢 What stands on the tower's floors near the eye, each where its floor is
// (the floor you are on is the room itself, and is not drawn twice).
function floorsAround(dy) {
  return (world?.floors || []).filter((f) => f.id !== room)
    .map((f) => ({ id: f.id, floor: true, dx: 0, dz: 0, dy: f.y + dy, objects: f.objects, screens: f.screens, tiles: f.tiles, neighbours: {}, plaza: false, home: null }));
}
// 🏢 How high the eye is in the world: on a floor of the tower, that floor's
// height; flying over the plaza, the drone's. The world is asked again when
// it has moved a storey, for the floors now beside it.
let worldY = 0, worldRequest = 0, worldZoomed = false;
function eyeHeight() {
  const up = -(plazaFrom()?.dy || 0);
  return up + (seat < 0 && !held ? freecam?.y || 0 : player?.y || 0);
}
async function streamWorld() {
  const asked = room, request = ++worldRequest;
  worldAt = performance.now();
  const about = aloft(asked) || asked === BASEMENT ? 'lobby' : asked;
  try {
    worldZoomed = focusZoom > 1;
    worldY = eyeHeight();
    const r = await fetch(new URL(`./world?room=${encodeURIComponent(about)}&y=${Math.round(worldY)}${worldZoomed ? '&detail=zoom' : ''}`, import.meta.url), { cache: 'no-store' });
    if (!r.ok || room !== asked) return;
    const data = await r.json();
    if (room !== asked || request !== worldRequest || !Array.isArray(data.blocks)) return;
    world = data;
    if (!sheetOpen) {
      const here = data.blocks.find(block => block.id === room);
      if (here) { park = { neighbours: here.neighbours, plaza: here.plaza }; objects = here.objects; tiles = here.tiles; setScreens(here.screens || []); }
    }
    worlds.delete(about);
    worlds.set(about, data);
    while (worlds.size > 6) worlds.delete(worlds.keys().next().value);
    try {
      localStorage.setItem('rax-world', JSON.stringify(Object.fromEntries(worlds)));
    } catch {}
  } catch {}
}
world = worlds.get(aloft(room) || room === BASEMENT ? 'lobby' : room) || null;
void streamWorld();
// Under the prompt the host may not have said who is signed in when this
// loads: look again shortly, so the room behind the curtain is theirs too.
if (!ONE_ROOM && !roamed && lastPlace && room !== lastPlace)
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
  dropQueued = false,
  bikeQueued = false; // 🛵 B was pressed: on or off the bike with the next command
let jumpQueued = false,
  fireQueued = false,
  audio,
  notice = 'CONNECTING';
let last = performance.now(),
  debt = 0;
let resting = false; // The host window is in the background (lib/rax-sheet.mjs).
let away = false; // Another piece has the host's stage: nobody can see the room, so it draws nothing.
// 💧 With the host page's ?lcd (bios sets window.acLcd), the room behind a
// background window's paper draws 12 times a second and each frame fades in
// over the last like a slow liquid-crystal pixel — the turn reads as motion
// without the edge flicker a plain 20 fps had (2026-11-03), at a fifth of
// the work. Without it, 60 as before.
const LCD = (() => { try { return window.parent !== window && window.parent.acLcd === true; } catch { return false; } })();
const BACKGROUND_FPS = LCD ? 12 : 60; // The room's top rate behind a background window's paper.
const lcd = LCD ? (() => {
  const show = document.createElement('canvas'); // the outgoing frame, fading away
  const saved = document.createElement('canvas'); // the frame on screen, kept for the next fade
  show.style.cssText = 'position:fixed;inset:0;width:100vw;height:100vh;pointer-events:none;opacity:0';
  let have = false, fading = false;
  const fit = (c) => { if (c.width !== canvas.width || c.height !== canvas.height) { c.width = canvas.width; c.height = canvas.height; } };
  return {
    // Right after a paced frame is drawn (same task, so the 3D canvas still holds it).
    frame() {
      if (!show.isConnected) canvas.after(show);
      fit(show); fit(saved);
      if (have) {
        const g = show.getContext('2d');
        g.clearRect(0, 0, show.width, show.height);
        g.drawImage(saved, 0, 0);
        show.style.transition = 'none';
        show.style.opacity = '1';
        void show.offsetWidth; // commit 1 before the fade
        show.style.transition = `opacity ${Math.round(1000 / BACKGROUND_FPS)}ms linear`;
        show.style.opacity = '0';
        fading = true;
      }
      const s = saved.getContext('2d');
      s.clearRect(0, 0, saved.width, saved.height);
      s.drawImage(canvas, 0, 0);
      have = true;
    },
    // Full rate again: nothing held, nothing stale for the next rest.
    off() {
      have = false;
      if (!fading) return;
      show.style.transition = 'none';
      show.style.opacity = '0';
      fading = false;
    },
  };
})() : null;
let lastBackgroundFrameAt = 0;
const presentation = new LocalPresentation(),
  playback = new SnapshotClock(),
  remoteEase = new RemoteEase();
const prompt = new Prompt();
// 💬 Room chat: T opens a line (Quake's say key), Enter says it, Escape drops
// it. The same text entry as the build prompt, with a flat cursor.
const chat = new Prompt();
const CHAT_MAX = 140, CHAT_SHOWN_MS = 14000;
let chatLog = []; // { at, handle, name, colors, text }
const connection = new Connection();
let selfMember = null;
// 🪞 A drone signed in as someone who walks here on another screen follows that body and looks where it looks, from just behind it. Its own stick, keys or look take it back; let go for a few seconds, it follows again.
const FOLLOW_AFTER_MS = 6000;
let following = false, followSaid = null, followLook = null, flownAt = -Infinity, followFlight = null, eyeCarried = false;
const ownBodySeat = () => {
  const mine = selfMember?.handle?.toLowerCase();
  return mine && seat < 0 ? members.find((m) => m.seat >= 0 && m.handle?.toLowerCase() === mine)?.seat ?? -1 : -1;
};
const diagnostics = new ClientDiagnostics((sample) => {
  if (query.get('debug') === 'motion') Object.assign(sample, gyro.status());
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
let downSince = null; // when this outage began, for the picture's decay
// 📉 Going down, the picture comes apart: how far gone, 0 to 1, over the outage's first five seconds.
function decayOf(now) {
  if (!(sheetOpen && down)) { downSince = null; return 0; }
  downSince ??= now;
  return Math.min(1, (now - downSince) / 5000);
}
// 🧱 Chunking in: from a drop (or a rejoin) until the room's first snapshot
// the picture is coarse blocks with pieces still missing; when the room
// speaks it fills in over half a second, from wherever it got to.
let chunkAt = null, chunkPeak = 0, landedAt = null;
function chunkOf(now) {
  if (!sheetOpen || down) { chunkAt = landedAt = null; chunkPeak = 0; return 0; }
  if (connection.lastSnapshot === null && !held) {
    landedAt = null;
    chunkAt ??= now;
    return (chunkPeak = Math.min(0.85, (now - chunkAt) / 180)); // (a quick join barely shows)
  }
  if (chunkAt === null) return 0;
  landedAt ??= now;
  const left = chunkPeak * (1 - (now - landedAt) / 500);
  if (left <= 0) { chunkAt = landedAt = null; chunkPeak = 0; return 0; }
  return left;
}
let statsShown = false;
const chartKey = 'ac:fps-hud';
try {
  const saved = localStorage.getItem(chartKey);
  statsShown = (saved ?? localStorage.getItem('rax-stats')) === '1';
  if (saved === null && statsShown) localStorage.setItem(chartKey, '1');
} catch {}
addEventListener('storage', event => {
  if (event.key === chartKey || event.key === null) statsShown = event.newValue === '1';
});
function toggleStats() {
  statsShown = !statsShown;
  try { localStorage.setItem(chartKey, statsShown ? '1' : '0'); } catch {}
}
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
  if (parked || ws?.left) return false; // Left on purpose: not an outage.
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
let bumperStart = null;
const touch = new TouchPad();
// Use the touch HUD before the first gesture on a phone.
touch.active = !!window.matchMedia?.('(pointer: coarse)').matches && !window.matchMedia?.('(any-pointer: fine)').matches;
let pointerSource = touch.active ? 'touch' : 'mouse';
for (const type of ['pointerdown', 'pointermove']) {
  canvas.addEventListener(type, e => { pointerSource = e.pointerType === 'mouse' ? 'mouse' : 'touch'; });
}
const sfx = new Sfx();
const notepatScreen = createNotepatScreen({
  at: kioskPlan('lobby').find(p => p.id === 'yard-notepat'), sfx,
  audible: () => room === 'lobby' && sheetOpen && !document.hidden,
  clock: () => Date.now() + (clockOffset || 0) + (connection.rtt || 0) / 2, send: message => send(message),
});
// 🍎 AC's native app starts a page's sound itself once the page has booted (it may play with no gesture), by the word AC's own pages answer to. Standing alone, rax answers to it too, so its sound comes back after the app loads it afresh. (Under the prompt the prompt's page has the word.)
try { if (window.parent === window && !window.AC) window.AC = { startAudio: () => { void sfx.wake(); return { state: sfx.audio?.state || 'suspended' }; } }; } catch {}
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
// program per screen and grid so each screen receives its own input.
let screens = [];
const screenPrograms = new Map(); // per-screen grid → program, or null while it loads
const screenSources = new Map(); // code → source, fetched once
let screenLoads = 0;
const screenRetry = new Map();
function screenProgram(s) {
  const { code, want: cols } = s, key = `${s.programKey}@${cols}`;
  if (screenPrograms.has(key) || screenLoads >= 4 || performance.now() < (screenRetry.get(key) || 0)) return;
  screenLoads++;
  screenRetry.delete(key);
  screenPrograms.set(key, null);
  kidlispWall(code, { width: cols, height: screenRows(cols), source: screenSources.get(code) || null })
    .then((w) => {
      screenSources.set(code, w.source);
      // Start from the picture the coarser (or finer) grid was showing.
      const old = screenPrograms.get(`${s.programKey}@${s.cols}`);
      if (old) carryPixels(old.pixels, old.width, old.height, w.pixels, w.width, w.height);
      w.usedAt = performance.now();
      screenPrograms.set(key, w);
    })
    .catch((e) => {
      screenPrograms.delete(key);
      screenRetry.set(key, performance.now() + 15000);
      console.info(`KidLisp screen $${code}:`, e.message);
    }).finally(() => { screenLoads--; });
}
function setScreens(list) {
  // Keep the same objects for screens that stay, so their frame caches hold.
  // A screen stood on the car before 2026-11-18 is shown beside it.
  screens = list.map((s) => Object.assign(screens.find((o) => o.id === s.id) || {}, s, hasCar() ? clearOfCar(s.x, s.z) : null));
}
// Render instances keep their last grid when the same screen changes from
// local to neighbouring scenery. Simulation and input still use room-local positions.
const screenInstances = new Map();
function visibleScreens(blocks) {
  const list = [], seen = new Set();
  function place(id, source, dx, dy, dz) {
    for (const s of source || []) {
      const key = id + '/' + s.id;
      seen.add(key);
      let instance = screenInstances.get(key);
      if (!instance || instance.code !== s.code) {
        instance = { id: key, code: s.code, programKey: key + "/" + s.code };
        screenInstances.set(key, instance);
      }
      const at = id === 'lobby' || aloft(id) || id === BASEMENT ? clearOfCar(s.x, s.z) : s;
      Object.assign(instance, { x: at.x + dx, y: dy, z: at.z + dz, yaw: s.yaw });
      list.push(instance);
    }
  }
  place(room, screens, 0, 0, 0);
  for (const b of blocks) place(b.id, b.screens, b.dx, b.dy || 0, b.dz);
  for (const id of screenInstances.keys()) if (!seen.has(id)) screenInstances.delete(id);
  return list;
}
function tendScreens(now, list) {
  for (const s of list) if (s.want) screenProgram(s);
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

let holdTime = 0,
  holdSpent = false,
  devLog = [];
// Behind the curtain the camera looks into a corner, so two walls, the
// ceiling and the dotted floor all show and the room reads as a room.
const DRONE_HOME = { x: -3, y: 2.1, z: -WORLD + 2, yaw: 28, pitch: -7 };
let freecam = { ...DRONE_HOME };
// 🌀 Warp: a drone, however far it has flown, jumps back to where drones begin (B, on the keys and on a pad). The picture comes apart on the way out and together again on the way in; it is moved at the middle, when least can be seen.
const WARP_MS = 700;
let warp = null; // { at, moved } while one is under way
function startWarp() {
  if (seat >= 0 || warp || game()) return;
  warp = { at: performance.now(), moved: false };
  sfx.warp();
}
function stepWarp(now) {
  if (!warp) return 0;
  const t = (now - warp.at) / WARP_MS;
  if (t >= 0.5 && !warp.moved) {
    warp.moved = true;
    selectTarget(null);
    freecam = { x: DRONE_HOME.x, y: DRONE_HOME.y, z: DRONE_HOME.z };
    yaw = DRONE_HOME.yaw; pitch = DRONE_HOME.pitch;
    droneRoll = 0;
  }
  if (t >= 1) return (warp = null), 0;
  return 0.9 * Math.sin(Math.PI * t);
}
let freecamPlaced = false;
let lookAt = 0, lookSaid = ''; // When and what this eye last told the room of itself.
// Just arrived and not seated yet (the seat waits for a signed-in handle):
// that is joining, not watching, so none of the spectator's mode shows.
let joiningUntil = 0;
const joining = () => seat < 0 && performance.now() < joiningUntil;
// 🪪 What the seat waits for: 'account' (nobody is signed in) or 'handle'
// (signed in, no handle yet). Until then the visitor flies over and looks.
let gate = null;
// 📱 Signed out and looking on, a phone can sign this tab in (pair-desk.mjs).
const pairDesk = createPairDesk();
let gamepad;
let joinURL = null; // (set once the world's shape is known, below: addressOf)
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
// 🖥️ Fill the screen with the room, or give it back: the browser is asked, and
// a host (the Mac app, the prompt's page) is told so it can do it itself.
function fullScreen() {
  const full = document.fullscreenElement || document.webkitFullscreenElement;
  const done = full
    ? (document.exitFullscreen || document.webkitExitFullscreen)?.call(document)
    : (document.documentElement.requestFullscreen || document.documentElement.webkitRequestFullscreen)?.call(document.documentElement);
  // (A browser that will not take a pad's button for it says so in the log; a host is asked as well.)
  Promise.resolve(done).then(() => logNet(full ? 'left full screen' : 'full screen'), (error) => logNet(`full screen refused: ${error?.message || error}`, { bad: true }));
  toHost({ type: 'rax:sheet:fullscreen', on: !full });
  try { window.webkit?.messageHandlers?.fullscreen?.postMessage({ on: !full }); } catch {}
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
  const s = screens.find((x) => `screen:${x.id}` === id);
  if (s) return { x: s.x, y: 1.5, z: s.z };
  const panel = panelOf(id);
  if (panel) return { x: panel.x, y: panel.y, z: panel.z };
  const far = farThings.get(id)?.box;
  if (far) return { x: (far.xMin + far.xMax) / 2, y: (far.yMin + far.yMax) / 2, z: (far.zMin + far.zMax) / 2 };
  return null;
}
const screenBox = (s) => { const wide = direction((s.yaw || 0) - 90), hx = Math.abs(wide.x) * 2 + 0.08, hz = Math.abs(wide.z) * 2 + 0.08; return { xMin: s.x - hx, xMax: s.x + hx, yMin: 0.4, yMax: 2.65, zMin: s.z - hz, zMax: s.z + hz }; };
const wallFocus = (w) => ({ id: `wall:${w.code}`, wall: true, screen: true, code: w.code, source: w.source, colored: w.colored, size: `${w.width}x${w.height}`, version: 0, owner: null, geometry: { name: `$${w.code}` } });
const screenFocus = (s) => ({ id: `screen:${s.id}`, wall: true, screen: true, code: s.code, version: 0, owner: null, geometry: { name: `$${s.code}` }, box: screenBox(s) });
// 🧩 A piece on a world panel is a thing in the world like any other: aimed at from any distance, named, and entered (within reach the aim is also its mouse). A label marks an enterable panel.
const panelOf = (id) => (id?.startsWith('panel:') ? panels.list.find((p) => !p.hidden && p.label && `panel:${p.id}` === id) || null : null);
const panelBox = (p) => { const wide = direction((p.yaw || 0) - 90), hx = Math.abs(wide.x) * (p.w / 2) + 0.08, hz = Math.abs(wide.z) * (p.w / 2) + 0.08; return { xMin: p.x - hx, xMax: p.x + hx, yMin: p.y - p.h / 2, yMax: p.y + p.h / 2, zMin: p.z - hz, zMax: p.z + hz }; };
const panelFocus = (p) => ({ id: `panel:${p.id}`, wall: true, panel: true, version: 0, owner: null, geometry: { name: p.label }, box: panelBox(p) });
// 🔭 Things that stand in the rooms around (another block, a floor of the tower): where each is in this room's metres, as last aimed at.
const farThings = new Map();
function selectables() {
  return [...objects.map((o) => o.id), ...walls.map((w) => `wall:${w.code}`), ...screens.map((s) => `screen:${s.id}`), ...panels.list.filter((p) => !p.hidden && p.label).map((p) => `panel:${p.id}`)];
}
function selectTarget(id) {
  if (dock && (padHolds || keyHolds)) { padHolds = keyHolds = false; panels.release(); }
  const face = id && faceOf(id);
  if (face && !goIn(id, face)) return;
  if (!face) dock = null;
  if (id && !face && !lockOn) {
    thirdBeforeLock = third;
    if (seat >= 0) third = true;
  }
  if ((!id || face) && lockOn) third = thirdBeforeLock;
  lockOn = id && !face ? { id, wall: farThings.has(id), cursor: { x: 0.5, y: 0.5 } } : null;
  lockedAt = performance.now();
  nearing = lockOn && seat < 0 ? approach(id) : null;
}
// A screen carries the eye square to its face. The body can walk independently.
let dock = null,
  nearing = null, // 🚁 { from, to, at, ms }: a drone on its way up to what it has just locked on to
  lockedAt = -Infinity,
  whirr = { lens: 0, travel: 0, turn: 0 }; // what the servos were last asked for (raxDebug)
const faceOf = (id) => {
  const s = screens.find((x) => `screen:${x.id}` === id);
  if (s) return { x: s.x, y: 1.525, z: s.z, yaw: s.yaw || 0, w: 4, h: 2.25, cols: 160, rows: 90 };
  const wall = walls.find((w) => `wall:${w.code}` === id);
  if (wall) {
    const north = wall.face === 'north', d = SCREEN - 0.03, h = 32 * wall.height / wall.width;
    return { x: north ? 0 : -d, y: 1.5 + h / 2, z: north ? d : 0, yaw: north ? 0 : 270, w: 32, h, cols: wall.width, rows: wall.height };
  }
  return panelOf(id);
};
// (How far the air is open from a place along a way, to `most`: the room's walls, what is built in it but for `but`, and the tower.)
function airFrom(origin, dir, most, but = null) {
  let clear = wallDistance(origin, dir, most, but ? solids().filter((o) => o.id !== but) : solids(), (snapshot?.tick || 0) / HZ, half());
  for (const box of [...fixedBoxes(), ...(room === 'lobby' ? YARD_TREES : [])]) clear = Math.min(clear, rayBox(origin, dir, box, clear));
  return clear;
}
// 🌳 The courtyard's two trees (view.mjs draws them on their planters): nothing walks into one, but an eye is not stood behind one.
const YARD_TREES = PLANTERS.flatMap((p) => [
  { xMin: p.x - 0.14, xMax: p.x + 0.14, yMin: 0.55, yMax: 2.45, zMin: p.z - 0.14, zMax: p.z + 0.14 },
  { xMin: p.x - 0.95, xMax: p.x + 0.95, yMin: 2.35, yMax: 3.85, zMin: p.z - 0.95, zMax: p.z + 0.95 },
]);
// (Before a face: straight out from its middle and from a third of the way to either side, the least of the three.)
const airBefore = (face, most) => {
  const out = direction((face.yaw || 0) + 180), side = direction((face.yaw || 0) + 90);
  return Math.min(...[-0.3, 0, 0.3].map((k) => airFrom({ x: face.x + out.x * 0.12 + side.x * face.w * k, y: face.y, z: face.z + out.z * 0.12 + side.z * face.w * k }, out, most))) + 0.12;
};
function goIn(id, face) {
  const zoomFrom = dock?.eye.zoom || 1;
  const scale = view.scale(), at = view.aimOrigin(), f = view.aimForward();
  if (!scale || !at || !f || game()?.camera || held) return false;
  const to = dockPose(face, scale, { clear: airBefore(face, 14) });
  if (!to) return false;
  const from = { x: at.x, y: at.y, z: at.z, yaw: ((Math.atan2(f.x, f.z) * 180) / Math.PI + 360) % 360, pitch: (Math.asin(clamp(f.y, -1, 1)) * 180) / Math.PI, zoom: zoomFrom };
  // (One of the mouse's pixels is one of the picture's: so many of the face's pixels to a degree of look.)
  const perDegree = (1 / 0.12) / (((face.h / face.rows) / to.back) * scale.sy * to.zoom * (canvas.clientHeight / 2));
  const bodyAt = dock?.bodyAt || (player && { x: player.x, y: player.y, z: player.z });
  dock = { id, face, from, to, eye: from, returnEye: dock?.returnEye || from, bodyAt,
    home: seat < 0 ? { x: freecam.x, y: freecam.y, z: freecam.z } : null, t: 0, way: 1,
    seconds: clamp(0.6 + Math.hypot(to.x - from.x, to.y - from.y, to.z - from.z) * 0.06, 0.8, 2.2),
    peekX: 0, peekY: 0, extended: false, cursor: { x: face.cols / 2, y: face.rows / 2 }, perDegree, headYaw: yaw, headPitch: pitch };
  nearing = null;
  return true;
}
// ⌨️ The panel this page has stepped in to, if its piece hears a keyboard; the keys it holds there (let go when the eye comes out, so no note is left sounding); and the keys rax keeps while inside.
const INSIDE_KEYS = new Set(['Escape', 'Tab', 'KeyZ']), pieceKeys = new Map();
const keyPanel = () => {
  if (!dock || dock.way < 0 || prompt.open || chat.open || menu || radial.open || chalkMode || typing()) return null;
  const panel = panels.list.find((p) => !p.hidden && `panel:${p.id}` === dock.id);
  return panel?.takesKeys ? panel : null;
};
function liftPieceKeys() {
  for (const [code, held] of pieceKeys) held.panel.key('up', { key: held.key, code });
  pieceKeys.clear();
}
function comeOut() {
  if (!dock) return;
  liftPieceKeys();
  dock.way = -1;
  if (padHolds || keyHolds) { padHolds = keyHolds = false; panels.release(); }
}
let screenPress = null;
function pressScreen() {
  if (!dock || dock.way < 0 || dock.id.startsWith('panel:')) return false;
  const instance = screenInstances.get(room + '/' + dock.id.slice(7));
  const program = instance?.program || walls.find(w => 'wall:' + w.code === dock.id);
  if (program) {
    if (instance) instance.pressed = program;
    screenPress = { dock, instance, program, x: -1, y: -1 };
    pointScreen('touch');
  }
  return true;
}
function pointScreen(name) {
  if (!screenPress) return;
  const p = screenPress, { cursor, face } = p.dock;
  const x = Math.floor(cursor.x * p.program.width / face.cols), y = Math.floor(cursor.y * p.program.height / face.rows);
  if (name !== 'draw' || p.x !== x || p.y !== y) p.program.point(name, x, y);
  p.x = x;
  p.y = y;
  if (name === 'lift') {
    if (p.instance && ![...screenFingers.values()].some(f => f.instance === p.instance)) p.instance.pressed = null;
    screenPress = null;
  }
}
// RB on foot, X in a drone, or Z on the keyboard.
function pressLock() {
  if (dock) comeOut();
  else selectTarget(lockOn ? null : focusObject?.id || null);
  sfx[lockOn || dock?.way > 0 ? 'ringOpen' : 'ringBack']();
}
// A drone flies up to an object it locks onto.
function approach(id) {
  const o = objects.find((x) => x.id === id);
  let to = null;
  if (o) {
    const at = spotOf(id), boxes = o.item ? [] : objectBoxes([o], (snapshot?.tick || 0) / HZ);
    const box = boxes.reduce((w, q) => (w ? { xMin: Math.min(w.xMin, q.xMin), xMax: Math.max(w.xMax, q.xMax), yMin: Math.min(w.yMin, q.yMin), yMax: Math.max(w.yMax, q.yMax), zMin: Math.min(w.zMin, q.zMin), zMax: Math.max(w.zMax, q.zMax) } : { ...q }), null)
      || { xMin: at.x - 0.6, xMax: at.x + 0.6, yMin: at.y - 1, yMax: at.y + 0.2, zMin: at.z - 0.6, zMax: at.z + 0.6 };
    to = boxStandoff(box, freecam, airFrom(boxMiddle(box), bearingFrom(box, freecam), 60, id));
  }
  const far = to ? Math.hypot(to.x - freecam.x, to.y - freecam.y, to.z - freecam.z) : 0;
  return far > 0.3 ? { from: { x: freecam.x, y: freecam.y, z: freecam.z }, to, at: performance.now(), ms: clamp(500 + far * 70, 700, 2600) } : null;
}
// Mouse and arrow look become motion across a docked face. A stick supplies
// velocity, so releasing it leaves the reticle on its current pixel.
function moveTargetCursor(elapsed) {
  const dt = Math.min(0.1, elapsed);
  const sx = gamepad.lookX, sy = gamepad.lookY;
  if (dock) {
    const dx = ((yaw - dock.headYaw + 540) % 360) - 180, dy = pitch - dock.headPitch;
    yaw = dock.headYaw;
    pitch = dock.headPitch;
    if (dock.way < 0 || typing() || menu || radial.open) return;
    const rate = Math.max(dock.face.cols, dock.face.rows) / 1.1;
    if (dx || dy || sx || sy) dock.cursor = cursorStep(dock.cursor, dock.face,
      -dx * dock.perDegree + sx * rate * dt, -dy * dock.perDegree + sy * rate * dt);
  } else if (lockOn && !typing() && !menu && !radial.open) {
    lockOn.cursor.x = clamp(lockOn.cursor.x + sx * dt / 1.1, 0, 1);
    lockOn.cursor.y = clamp(lockOn.cursor.y + sy * dt / 1.1, 0, 1);
  }
}
// And after it: the eye is carried along its glide (a drone with it), and a
// drone on its way up to a thing is flown there. What comes back is how fast
// the eye is changing each way (dock.mjs `rates`), for the servos' whirr; all
// nothing when it is not being carried.
function carry(elapsed, now, turned) {
  const dt = Math.min(0.1, Math.max(0, elapsed));
  let heard = { lens: 0, travel: 0, turn: 0 };
  if (dock && !faceOf(dock.id)) selectTarget(null); // (the screen itself went)
  if (dock) {
    const was = dock.eye;
    dock.t = clamp(dock.t + (dock.way * dt) / (dock.way > 0 ? dock.seconds : dock.seconds * 0.8), 0, 1);
    const e = smooth(dock.t);
    const returning = dock.way < 0 && dock.bodyAt && player;
    const from = returning ? { ...dock.returnEye,
      x: dock.returnEye.x + player.x - dock.bodyAt.x, y: dock.returnEye.y + player.y - dock.bodyAt.y,
      z: dock.returnEye.z + player.z - dock.bodyAt.z } : dock.from;
    dock.eye = glide(from, dock.to, e);
    const looking = dock.way > 0 && !typing() && !menu && !radial.open && !chalkMode;
    const spring = 1 - Math.exp(-dt * 12);
    dock.peekX += ((looking ? gamepad.right : 0) - dock.peekX) * spring;
    dock.peekY += ((looking ? gamepad.fwd : 0) - dock.peekY) * spring;
    // Aim towards the edges of this face. Releasing the stick restores the square-on view.
    dock.eye.yaw -= e * dock.peekX * Math.atan2(dock.face.w / 2, dock.to.back) * 180 / Math.PI;
    dock.eye.pitch += e * dock.peekY * Math.atan2(dock.face.h / 2, dock.to.back) * 180 / Math.PI;
    if (dt) heard = rates(was, dock.eye, dt);
    if (seat < 0) {
      freecam = { ...freecam, x: dock.home.x + (dock.to.x - dock.home.x) * e, y: dock.home.y + (dock.to.y - dock.home.y) * e, z: dock.home.z + (dock.to.z - dock.home.z) * e, vx: 0, vy: 0, vz: 0 };
      yaw = dock.eye.yaw;
      pitch = dock.eye.pitch;
      droneRoll -= Math.round(droneRoll / (2 * Math.PI)) * 2 * Math.PI;
      droneRoll *= Math.exp(-dt * 6);
    }
    dock.headYaw = yaw;
    dock.headPitch = pitch;
    if (dock.way < 0 && dock.t <= 0) dock = null;
  } else if (nearing && seat < 0) {
    const e = smooth((now - nearing.at) / nearing.ms), mix = (k) => nearing.from[k] + (nearing.to[k] - nearing.from[k]) * e;
    const to = { x: mix('x'), y: mix('y'), z: mix('z') };
    if (dt) heard.travel = Math.min(1, Math.hypot(to.x - freecam.x, to.y - freecam.y, to.z - freecam.z) / dt / 8);
    freecam = { ...freecam, ...to, vx: 0, vy: 0, vz: 0 };
    if (now - nearing.at >= nearing.ms) nearing = null;
  } else nearing = null;
  // (The lock's own turn to its thing is heard as it swings round, for a moment after it is set: not for as long as the thing is walked about.)
  if (!dock && lockOn && now - lockedAt < 1500 && dt) heard.turn = Math.max(heard.turn, Math.min(1, turned / dt / 120));
  return heard;
}
// Step the lock to the next selectable clockwise (dir 1) or anticlockwise.
function cycleLock(dir) {
  const from = player || (seat < 0 ? freecam : null);
  const current = dock || lockOn;
  if (!from || !current || dock?.way < 0) return;
  // 🖥️ In at a screen, the step goes on to the next thing all the same: to another screen the eye glides straight across, lens as it is; to anything else it comes back out to look at it.
  const head = dock ? dock.headYaw : yaw;
  const bearing = (id) => {
    const p = spotOf(id);
    return p ? (((Math.atan2(p.x - from.x, p.z - from.z) * 180) / Math.PI - head + 540) % 360) - 180 : null;
  };
  const list = selectables()
    .map((id) => ({ id, b: bearing(id) }))
    .filter((x) => x.b !== null)
    .sort((a, b) => a.b - b.b);
  const at = list.findIndex((x) => x.id === current.id);
  if (list.length < 2 || at < 0) return;
  selectTarget(list[(at - dir + list.length) % list.length].id);
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
  // 🧵 What is said of the body is said of a room: the socket stays when the body goes to the next, and a word for the room left is not the next one's.
  if (data.type === 'notepat-watch' || data.type === 'notepat-key' || data.type === 'input' || data.type === 'walk' || data.type === 'elevator' || data.type === 'panel') data = { ...data, in: room };
  if (
    ws?.readyState === WebSocket.OPEN &&
    (ws.bufferedAmount < (data.type === 'input' ? 65536 : 32768) || data.type === 'notepat-watch' ||
      (data.type === 'notepat-key' && data.way === 'up') || (data.type === 'panel' && data.name === 'lift'))
  ) {
    ws.send(JSON.stringify(data));
    return true;
  }
  return false;
};
// 🧱 What this page has asked Aesel for, each with its one state from the press to the end (build-line.mjs): the view draws them as a line at the top of the screen, wherever you look. An asking that cannot be sent is put there too with its reason: no control here is ever simply dead.
const builds = createBuilds({
  send,
  ready: (askedRoom) => !!owner && askedRoom === room,
  // (An asking not yet answered outlives the page loading itself again after a dropped socket: the tab's own storage.)
  kept: { read: () => { try { return sessionStorage.getItem('rax-asked'); } catch { return null; } }, write: (text) => { try { sessionStorage.setItem('rax-asked', text); } catch {} } },
});
const mineJob = (j) => !!owner && (j.asker || j.owner) === owner;
// 🧩 The view keeps each object's model under its geometry (view.mjs), so an
// object the room sends again at the same version keeps the geometry it
// already had: a build starting or ending re-made every model in the room,
// a stop for each, where only the new one needs making.
// The server sends each shape once per version to a page that keeps them
// (`shapes-kept`); later sends mark it `kept`, without geometry. One this page
// no longer has is asked for again, and stays out of the room till it comes.
const shapes = new Map(); // `id@version` → geometry
function keepShapes(before, after) {
  let lost = false;
  const list = [];
  for (const o of after) {
    const key = `${o.id}@${o.version}`;
    if (o.kept) {
      const geometry = shapes.get(key);
      if (geometry) list.push({ ...o, geometry });
      else lost = true;
      continue;
    }
    if (o.geometry) {
      if (shapes.has(key)) o.geometry = shapes.get(key);
      else {
        if (shapes.size > 4000) shapes.clear();
        shapes.set(key, o.geometry);
      }
    }
    list.push(o);
  }
  if (lost) send({ type: 'shapes-lost' });
  return list;
}
function heardJobs(list) {
  diagnostics.jobs(list.map((j) => j.id));
  for (const job of list) sfx.buildProgress(job.id, job.preview?.boxes?.length || 0);
  jobs = list;
  prompt.busy = seat >= 0 && jobs.filter((j) => j.owner === owner).length >= 3;
  // 🧱 The line follows the room's word; a build of ours moving on a step (generating, validating…) is a small tick, each higher.
  for (const b of builds.room(room, jobs, objects, mineJob)) {
    const step = ['generating', 'correcting', 'validating', 'waiting'].indexOf(b.phase);
    if (step >= 0) sfx.ringMove(5 + step);
  }
}
let heardRoom = null; // the room whose `objects` this socket last said (the line is only checked against a room that has spoken)
function refuseBuild(text, why) {
  builds.refuse(text, why);
  prompt.message = why.toUpperCase();
}
// The line's rows with what the room knows of each: a finished thing's name and where it stands from here.
function buildRows() {
  return builds.rows().map((b) => {
    const o = b.state === 'done' && b.objectId ? objects.find((x) => x.id === b.objectId) : null;
    if (!o) return b;
    let where = '';
    if (player) {
      const dx = o.x - player.x, dz = o.z - player.z, turn = ((((Math.atan2(dx, dz) * 180) / Math.PI - yaw) % 360) + 540) % 360 - 180;
      where = `${Math.round(Math.hypot(dx, dz))} m ${Math.abs(turn) < 45 ? 'ahead' : Math.abs(turn) > 135 ? 'behind you' : turn > 0 ? 'to your left' : 'to your right'}`;
    }
    return { ...b, name: o.geometry?.name || '', code: o.code || '', where };
  });
}

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
    if (data.tower) { towerTop = data.tower.top; listFloors(data.tower.listed); }
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
    gyro: gyro.status(),
    holding: snapshot?.items?.find(it => it.holder === seat && seat >= 0)?.id,
    seat,
    builds: builds.rows(),
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
    lock: lockOn?.id || null,
    lockCursor: lockOn?.cursor || null,
    dock: dock ? { id: dock.id, pixel: screenInstances.get(room + '/' + dock.id.slice(7))?.program?.pixels?.slice(0, 3), t: dock.t, way: dock.way, cursor: dock.cursor, to: dock.to, hot: panels.hot ? { id: panels.hot.panel.id, x: panels.hot.x, y: panels.hot.y } : null } : null,
    fov: (dock?.eye.zoom || 1) * focusZoom,
    nearing: !!nearing,
    inside: (() => { const p = keyPanel(); return p ? { id: p.id, keys: [...pieceKeys.keys()], ink: p.pixels.reduce((sum, v, i) => (sum + v * ((i % 251) + 1)) % 1000003, 0) } : null; })(), // ⌨️ the piece that has the keyboard, the keys it holds, and a sum of its picture
    whirr: { ...whirr, gains: sfx.servos ? sfx.servos.map((s) => s.out.gain.value) : null }, // (the servos' rates asked for, and how loud each stands)
    eye: seat < 0 ? { x: freecam.x, y: freecam.y, z: freecam.z } : null,
    notepatVoices: notepatScreen.voices,
    panels: panels.list.filter((p) => !p.hidden).map((p) => p.id),
    cursorAim,
    canvas: { w: canvas.width, h: canvas.height, cw: canvas.clientWidth, ch: canvas.clientHeight },
  });
// 🎥 And a way to set the eye down for a picture (?debug only): where a drone hovers, and which way anyone looks.
if (query.has('debug'))
  window.raxLook = (at = {}) => {
    if (seat < 0) freecam = { ...freecam, ...Object.fromEntries(['x', 'y', 'z'].filter((k) => Number.isFinite(at[k])).map((k) => [k, at[k]])), vx: 0, vy: 0, vz: 0 };
    if (Number.isFinite(at.yaw)) yaw = at.yaw;
    if (Number.isFinite(at.pitch)) pitch = at.pitch;
    inputAt = performance.now(); // (not left to the idle tour)
  };

// 🚪 To another room of the one world: the same page, a new socket. What
// stood in the old room is put away first so it never shows in the new one.
// A place in the one world (the plaza, a block of the park, a floor of the
// tower), as against an instance: a game's arena or a named room.
const inWorld = (id) => id === BASEMENT || id === 'lobby' || aloft(id) || /^[ew]\d{1,4}[ns]\d{1,4}$/.test(id);
// 🧭 This place's AC address, for the host's address bar: a game's own word
// (botbop, botbop~name), else rax~room; the lobby and your home are rax.
// 🏷️ This page's own address for a room. The world has one: the plaza, a
// block and a floor are all the bare address, and where you stand is kept for
// you, not written in it. Only a room outside the world has a word, `~room`.
const addressOf = (id) => new URL(inWorld(id) ? './' : `./~${id}`, import.meta.url);
function tidyAddress() {
  if (!['localhost', '127.0.0.1', '[::1]'].includes(location.hostname)) joinURL = addressOf(room).href;
  if (window.parent !== window) return; // (under the prompt the host keeps the address)
  try {
    const address = addressOf(room);
    address.search = location.search.replace(/([?&])room=[^&]*&?/, '$1').replace(/[?&]$/, ''); // (the rest as written: ?debug stays ?debug)
    window.history.replaceState(window.history.state, '', address);
  } catch {}
}
tidyAddress();
// 🦾 rax in words and calls (agent.mjs): a screen reader, a keyboard and an
// agent read the same state the eye does, and press the same inputs.
const placeName = (id) =>
  id === 'lobby' ? 'The plaza, at the foot of the tower'
  : id === BASEMENT ? 'The basement'
  : aloft(id) ? (tower?.here?.number ? `Floor ${tower.here.number} of the tower${tower.here.handle ? `, @${tower.here.handle}'s` : ''}` : 'Up the tower')
  : inWorld(id) ? `Block ${id} of the park`
  : `The room ${id}`;
const agentSurface = createAgentSurface({
  canvas,
  read: () => {
    const me = selfMember || members.find((m) => (seat >= 0 ? m.seat === seat : false)) || null;
    const named = (m) => (m?.handle ? `@${m.handle}` : m?.name || 'someone');
    return {
      room, place: placeName(room), connected: ws?.readyState === WebSocket.OPEN && !!snapshot,
      seat, player, eye: seat < 0 ? freecam : null, yaw, pitch,
      name: me ? named(me) : null, handle: me?.handle || null,
      people: (snapshot?.players || []).flatMap((p, i) => {
        if (!p || i === seat) return [];
        const m = members.find((x) => x.seat === i);
        return [{ id: `seat:${i}`, x: p.x, z: p.z, name: named(m), handle: m?.handle || null }];
      }),
      objects: objects.filter((o) => Number.isFinite(o.x)).map((o) => {
        const it = snapshot?.items?.find((x) => x.id === o.id);
        return { id: o.id, x: it ? it.x : o.x, z: it ? it.z : o.z, name: o.geometry?.name || o.name || (o.item ? 'something to pick up' : 'a made thing') };
      }),
      screens: [
        ...screens.map((s) => ({ id: `screen:${s.id}`, x: s.x, z: s.z, name: `the screen showing $${s.code}` })),
        ...panels.list.filter((p) => !p.hidden && p.label).map((p) => ({ id: `panel:${p.id}`, x: p.x, z: p.z, name: `${p.label}, a piece to step into` })),
      ],
      half: half(), edges: aloft(room) ? [] : openEdges() || [], neighbours: park?.neighbours || null,
      said: chatLog.slice(-8).map((c) => ({ at: c.at, name: c.handle ? `@${c.handle}` : c.name || 'someone', text: c.text })),
      notice,
    };
  },
  act: {
    look: (at) => { if (Number.isFinite(at.yaw)) yaw = at.yaw; if (Number.isFinite(at.pitch)) pitch = at.pitch; inputAt = performance.now(); },
    press: (what) => { if (what === 'use') useQueued = true; else if (what === 'drop') dropQueued = true; else if (what === 'jump') jumpQueued = true; },
    say: (text) => send({ type: 'say', text: text.slice(0, CHAT_MAX) }),
    elevator: (to) => send({ type: 'elevator', to }),
    signIn: () => askAccount('account'),
  },
});
function placePath() {
  const g = game();
  if (g?.at(room)) return room.startsWith(`${g.name}-`) ? `/${g.name}~${room.slice(g.name.length + 1)}` : `/${g.name}`;
  // The world has one address: the plaza, a block and a floor of the tower
  // are all rax. Only a room outside the world has a word of its own.
  return inWorld(room) ? '/rax/' : `/rax~${room}`;
}
function travel(to, chosen = true, receipt = null, riding = null, at = null, ack = null, here = false) {
  if (typeof to === 'string') to = placeFor(to); // (🏠 one room: nowhere else in the world to go)
  if (!/^[a-z0-9-]{1,32}$/.test(to) || to === room) return;
  // 🚶 The body goes on between the two rooms (held): it keeps walking by its
  // own commands, which the next room runs when it seats it. A walk across
  // a block's edge keeps its motion and the commands the old room never ran.
  cancelBumper();
  inputAt = performance.now();
  const walk = Number.isInteger(ack), from = room;
  if (walk) crossing = { from, to, at: performance.now() };
  // The ball stays where it is in the world: over a seam it is a block
  // away in the new room's metres; by any other way it is out of sight.
  const leavingTower = walk && aloft(from) && !!cellOf(to);
  const oldCell = walk && cellOf(leavingTower ? 'lobby' : from), newCell = walk && cellOf(to);
  const floorHeight = leavingTower ? altitude(tower?.here?.number || 0) : 0;
  const continuous = here && player && oldCell && newCell;
  const dx = oldCell && newCell ? wrap(oldCell.i - newCell.i) * SPAN : 0;
  const dz = oldCell && newCell ? wrap(oldCell.j - newCell.j) * SPAN : 0;
  balls = walk ? balls.map(b => ({ ...b, x: b.x + dx, z: b.z + dz })) : [];
  // 🪞 A drone brought after its body over a block's line is where it was in the world: it flies on from there, it is not set down again.
  if (seat < 0 && here) {
    const a = cellOf(from), b = cellOf(to);
    if (a && b) { freecam = { ...freecam, x: freecam.x + wrap(a.i - b.i) * SPAN, z: freecam.z + wrap(a.j - b.j) * SPAN }; eyeCarried = true; followFlight = null; }
  }
  if (continuous) presentation.translate(dx, dz, floorHeight);
  held = (receipt || here) && at && player && seat >= 0
    ? { ...player, vx: 0, vz: 0, ...(walk ? at : { x: at.x, z: at.z }), walk, until: performance.now() + 20000 } : null;
  const owed = held && walk ? pending.filter((c) => c.seq > ack).slice(-HORIZON) : [];
  if (chosen) roamed = true;
  selectTarget(null);
  yardQuiet(); // (a tone held on a piece in the yard does not follow into the next room)
  // The live room may have changed since the neighbourhood was fetched.
  if (cellOf(from) && heardRoom === from) {
    world ||= { blocks: [], floors: [] };
    let departing = world.blocks.find(b => b.id === from);
    if (!departing) {
      departing = { id: from, plaza: from === 'lobby', neighbours: park?.neighbours || {} };
      world.blocks.push(departing);
    }
    const poses = new Map((snapshot?.items || []).map(it => [it.id, it]));
    departing.objects = objects.map(o => {
      const it = poses.get(o.id);
      return it ? { ...o, x: it.x, y: it.y, z: it.z } : o;
    });
    departing.screens = screens.map(({ id, x, z, yaw, code }) => ({ id, x, z, yaw, code }));
    departing.tiles = tiles;
  }
  for (const id of screenFingers.keys()) endScreenFinger(id, null);
  liftPieceKeys();
  notepatScreen.leave();
  room = to;
  if (riding) {
    // 🛗 The car leaves at once and the next floor is already known: the
    // ride is the distance between the two, travelled for real.
    // Asked while the car ran: it starts from where it is, not from the
    // floor it was on its way to.
    const now = performance.now(), running = rideMoving(now) ? liftAt(now) : 0;
    const span = altitude(riding.from) - altitude(riding.to) + running;
    ride = { ...riding, ...(running ? { from: floorAt(altitude(riding.from) + running + 0.5, [riding.from, ride?.from]) } : {}), at: now, span, seconds: rideSeconds(span), rung: false };
    tower = { ...(tower || {}), here: riding.to ? { number: riding.to, handle: riding.handle, colors: null, ...(to === ROOF ? { room: ROOF, roof: true } : {}) } : { room: 'lobby', number: 0 }, owner: false };
    // 🚪 The door is shut while it runs: the rider walks about inside (world.mjs `shut`); the next room says the same when it seats them.
    if (held) held.shut = shutTicks(ride.seconds, HZ);
    sfx.ringMove(ride.to > ride.from ? 9 : 4);
  } else {
    tower = null;
    ride = null;
  }
  passage = receipt;
  beyond = new Map(); // (the next room tells them in its own metres)
  drones = new Map();
  park = null;
  walkAt = walk ? 0 : performance.now() + 1000;
  objects = [];
  tiles = {};
  // What the world stream already knows of the place stands there at once:
  // the ground and things were on screen a block away a moment ago.
  const known = world?.blocks.find(b => b.id === to) || world?.floors?.find(f => f.id === to);
  if (known) {
    park = known.neighbours ? { neighbours: known.neighbours, plaza: known.plaza } : null;
    objects = known.objects || [];
    tiles = known.tiles || {};
  } else if (walk && walkEdge && oldCell && newCell) {
    // Nothing known of the place yet but the way in: that side leads back.
    park = { neighbours: { [BACK[walkEdge]]: from }, plaza: false };
  }
  setScreens(known?.screens || []);
  void streamWorld();
  chalkStrokes = [];
  chalkVersion++;
  tidyAddress();
  toHost({ type: 'rax:sheet:place', path: placePath() });
  if (inWorld(room)) {
    lastPlace = room;
    try { localStorage.setItem('rax-place', room); } catch {}
  }
  if (here && ws?.readyState === WebSocket.OPEN) {
    // 🧵 The socket stays: the server has moved the body, and the next room's
    // words are next on this wire. Only what was known of the room left goes.
    coolDown();
    unseat(continuous);
  } else if (ws && ws.readyState !== WebSocket.CLOSED) {
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
  if (continuous && held) presentation.correct(held);
}
// 🚪 The rooms next door, held open while you are near their seam: each
// has answered and knows who you are, and waits (wait=1). Stepping over,
// the passage is handed to the one already open, so there is no new
// connection to make. Its room's words are kept to be heard on stepping in.
const STAY = true;
const WARM_NEAR = 6, WARM_FAR = 7; // metres from a block's line: open within, let go beyond
const warm = new Map(); // room → { socket, words, ready }
function warmUp(to) {
  if (warm.has(to) || warm.size >= 3) return;
  const url = playURL(to, import.meta.url);
  url.search = new URLSearchParams({ room: to, protocol: PROTOCOL, wait: '1', ...(accountExpected() ? { account: '1' } : {}) });
  const entry = { socket: new WebSocket(url), words: [], ready: false };
  warm.set(to, entry);
  entry.socket.onmessage = (e) => {
    if (typeof e.data !== 'string' || e.data.startsWith('{"type":"snapshot"')) return; // the room's motion is not kept
    let type = '';
    try { type = JSON.parse(e.data).type; } catch { return; }
    // (What only happened once is not heard again: a line said, a pong, an answer to an asking.)
    if (['whereabouts', 'ball', 'balls-away', 'gate', 'said', 'pong', 'dev-log', 'input', 'travel', 'jobs', 'build-result', 'build-accepted', 'elevator-result'].includes(type)) return;
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
  // 🧵 The socket stays from room to room, so no door is held open ahead of a step. (STAY off: the doors again, for a server that hands out passages.)
  if (STAY) return;
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
// What was known of a room's match goes when the body leaves it: its
// snapshots, its count of commands, what was predicted in it.
function unseat(keepMotion = false) {
  history = [];
  pending = [];
  player = null;
  snapshot = null;
  rounds.clear();
  view.clearGore?.();
  prediction = null;
  resetPrediction = false;
  seq = sentSeq = 0;
  eventId = 0;
  if (!keepMotion) {
    debt = 0;
    presentation.reset(null);
  }
  playback.reset();
  remoteEase.reset();
  freecamPlaced = false;
  gate = null;
  joiningUntil = performance.now() + 7000;
}
function connect(adopt = null) {
  bootMark('connect');
  const url = playURL(room, import.meta.url); // (a room kept on another box opens there: world.mjs ROOM_HOSTS)
  // 🔐 Signed in on this page: fetch the token while the socket opens, and
  // ask the server to keep the seat for the handle (no guest name first).
  const account = accountExpected() ? accountToken() : null;
  url.search = new URLSearchParams({ room, protocol: PROTOCOL, stay: '1', ...(passage ? { passage } : {}), ...(account ? { account: '1' } : {}), ...(query.has('film') ? { watch: '1' } : {}) }); // (stay: one socket from room to room)
  unseat();
  notice = held ? '' : 'CONNECTING';
  // 🚶 Walking over a seam you are still you and the line is still the line:
  // your face, your ping and the roster stay up until the next room speaks.
  if (held?.walk) { connection.pending.clear(); connection.next = 0; }
  else {
    connection.reset();
    members = [];
    selfMember = null;
  }
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
      if (!appTold) { appTold = true; tellApp('boot:ready'); } // 🍎 in the room: the native app stops watching the load
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
        freecamPlaced = eyeCarried;
        eyeCarried = false;
        followLook = null;
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
    if (data.type === 'pair') return void pairDesk.hear(socket, data);
    if (data.type === 'gate' && seat < 0) {
      gate = data.needs === 'handle' ? 'handle' : 'account';
      joiningUntil = 0; // Not joining: looking.
    }
    if (data.type === 'welcome') {
      bootMark('welcome');
      send({ type: 'shapes-kept' });
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
    if (data.type === 'balls-away') { balls = []; return; }
    if (data.type === 'ball') {
      const at = performance.now();
      balls = [data, ...(data.others || [])].map(b => ({ ...b, at }));
      return;
    }
    // 👥 Someone else's hand on a kiosk: its piece hears their pointer, numbered by their seat.
    if (data.type === 'notepat-sound' || data.type === 'notepat-frame') {
      if (room === 'lobby') notepatScreen.receive(data);
      return;
    }
    if (data.type === 'panel') {
      if (data.id === 'yard-notepat') return;
      const panel = yardPanels.find((p) => p.id === data.id);
      if (data.name === 'reset') { if (yardKept?.[data.id]) yardKept[data.id] = { total: 0, events: [], epoch: (panel?.epoch || 0) + 1 }; return void resetKiosk(data.id); }
      panel?.hear?.(data.name, data.x, data.y, remotePointer(data.seat, data.finger));
      if (panel && data.name !== 'draw') panel.done = (panel.done || 0) + 1;
      return;
    }
    // 🧩 Where the room's pieces stand: what this page's copies have not had done to them is done now, silently.
    if (data.type === 'panels') { if (room === 'lobby') { yardKept = data.kept || {}; catchUp(); send({ type: 'notepat-watch' }); } return; }
    // 👀 Who walks the rooms around this one, in this room's metres.
    if (data.type === 'beyond') {
      const now = performance.now(), next = new Map();
      // 🌐 Rounds flying in the rooms around (a shot carried on over a seam), flown on here between words.
      farShots = (data.shots || []).map(([id, x, y, z, vx, vy, vz]) => ({ id: `far-${id}`, x, y, z, vx, vy, vz, at: now }));
      for (const [id, name, seat, x, y, z, yaw, pitch, vx, vz, boat, bike] of data.people) {
        const was = beyond.get(id), from = was ? beyondAt(was, now) : null;
        next.set(id, { id, name, seat, x, y, z, yaw, pitch, vx, vz, boat, bike, alive: true, from, at: now });
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
    if (data.type === 'travel') travel(data.room, true, data.passage, data.ride || null, data.at || null, data.ack, data.here === true);
    if (data.type === 'tower') {
      tower = data;
      if (Array.isArray(data.listed)) {
        listFloors(data.listed);
        try { localStorage.setItem('rax-tower-listed', JSON.stringify(data.listed)); } catch {}
      }
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
      if (!held?.walk && Number.isFinite(data.yaw)) { yaw = data.yaw; pitch = data.pitch || 0; } followLook = null; // (a rider is given none: they look where they looked)
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
      if (selfMember?.canTakeBody) joiningUntil = 0; // 🪞 its body walks on another screen: this one looks on, it is not waiting for a seat (so a drone brought to the next room flies on at once)
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
      // 🧱 Known to the room again (a new socket): a build of ours it already spoke of joins the line, and an asking it never answered is said again under its own name.
      if (heardRoom === room) builds.room(room, jobs, objects, mineJob);
      builds.rejoined();
      // 🌐 Only home's identity is kept: a satellite (a room kept on another box)
      // hands out its own, which would leave this page a stranger to what it built at home.
      if (playURL(room, import.meta.url).host === location.host || /^(localhost|127\.)/.test(location.hostname))
        try {
          localStorage.setItem('rax-owner', data.token);
        } catch {}
    }
    // 🧵 A build's progress alone (builders.broadcastJobs): the line of jobs, no room.
    if (data.type === 'jobs' && heardRoom === room) heardJobs(data.jobs);
    if (data.type === 'objects') {
      objects = keepShapes(objects, data.objects);
      builder = data.builder;
      builderNames = Array.isArray(data.builders) ? data.builders : [];
      if (Array.isArray(data.history)) codeHistory = data.history;
      if (data.tiles && typeof data.tiles === 'object') tiles = data.tiles;
      if (data.wallRes && typeof data.wallRes === 'object') wallRes = data.wallRes;
      // Room wall specs are ignored while the walls are plain (setWalls above).
      if (Array.isArray(data.screens)) setScreens(data.screens);
      heardRoom = room;
      heardJobs(data.jobs);
    }
    if (data.type === 'build-accepted') {
      builds.hear(data);
      sfx.buildStart();
      if (player) view.effect({ type: 'think', at: { x: player.x, y: player.y + 0.35, z: player.z } });
      prompt.busy = true;
      prompt.message = 'AESEL IS BUILDING';
      prompt.open = false;
    }
    if (data.type === 'build-result') {
      builds.hear(data);
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
    // 📍 A page loaded again (a restart of the server, a reload) puts a drone back where it was, looking where it looked.
    if (seat < 0 && !freecamPlaced && !held) {
      try {
        const was = JSON.parse(sessionStorage.getItem('rax-eye') || 'null');
        if (was && was.room === room && Date.now() - was.at < 30 * 60000 && [was.x, was.y, was.z, was.yaw, was.pitch].every(Number.isFinite)) {
          freecam = { x: was.x, y: was.y, z: was.z };
          yaw = was.yaw; pitch = was.pitch;
          freecamPlaced = true;
        }
      } catch {}
    }
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
    rounds.heard(data.bullets, seat, latestAt);
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
      // 🏙️ Instagib: a body comes apart (view.mjs 'gib'); 🚁 a drone shot down is set back at its perch.
      if (event.type === 'gib') sfx.blast();
      if (event.type === 'downed' && event.eye === myEye && seat < 0) {
        freecam = { ...freecam, x: DRONE_HOME.x, y: DRONE_HOME.y, z: DRONE_HOME.z, vx: 0, vy: 0, vz: 0 };
        notice = 'SHOT DOWN';
        setTimeout(() => { if (notice === 'SHOT DOWN') notice = ''; }, 2000);
      }
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
    notepatScreen.leave();
    if (parked || socket.left) {
      // Our own leave: drop the seat quietly; the room stays as it was seen.
      seat = -1;
      player = null;
      snapshot = null;
      rounds.clear();
      view.clearGore?.();
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
    heardRoom = null;
    prompt.busy = false;
    owner = null;
    builder = false;
    objects = [];
    jobs = [];
    members = [];
    selfMember = null;
    player = null;
    snapshot = null;
    rounds.clear();
    view.clearGore?.();
    prediction = null;
    resetPrediction = false;
    pending = [];
    history = [];
    presentation.reset(null);
    playback.reset();
    remoteEase.reset();
  };
  socket.onerror = () => {
    if (socket === ws) logNet('socket error', { bad: true });
    notice = 'SERVER UNAVAILABLE — CLICK TO RETRY';
  };
}

let headsetTap = null, headsetTappedAt = -Infinity;
canvas.addEventListener('click', (e) => {
  if (performance.now() - headsetTappedAt < 450) return;
  const mode = !locked() && view.headsetHit(e.offsetX, e.offsetY);
  if (mode) return void headset.enter(mode);
  if (!locked() && view.cornerHit(e.offsetX, e.offsetY)) return leave();
  if (e.pointerType !== 'touch' && !locked() && selfMember?.canTakeBody && view.handleHit(e.offsetX, e.offsetY)) return send({ type: 'take-body' });
  // The code tag in the bottom-right is a button: it opens the $code's page.
  if (!locked() && view.qrURL && view.qrHit(e.offsetX, e.offsetY)) return void window.open(view.qrURL, '_blank', 'noopener');
  // 📱 The sign-in code is a button as well: sign in here, the ordinary way.
  if (!locked() && view.pairHit(e.offsetX, e.offsetY)) return askAccount('account');
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
  if (!CHALK) return null;
  const held = ((chalkMode && gamepad?.triggerHeld) || chalkMouse || keys.has('KeyC')) && seat >= 0 && !prompt.open;
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
    ? list.map((h) => ({ ...h, act: () => { builds.ask({ type: 'restore', code: h.code }, `$${h.code}`, room); sfx.buildAsk(); } }))
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
// What the held item is: 'shoot', 'poke', 'throw' (or null).
function heldKind() {
  return (seat >= 0 && snapshot?.items?.find((it) => it.holder === seat)?.kind) || null;
}
function submitPrompt(gesture = true) {
  if (!prompt.text.trim()) return false;
  if (prompt.busy) {
    refuseBuild(prompt.text, builds.live >= 3 ? 'aesel is already making three things for you' : 'still waiting for word of the last one');
    return false;
  }
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
    builds.ask({ type: 'kidlisp-mod', face, prompt: prompt.text }, prompt.text, room);
    sfx.buildAsk();
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
    // Not sent, and said so on the build line (the prompt's own message is one small row, top left, gone when the ring closes).
    refuseBuild(prompt.text, !owner ? 'the room does not know you yet: try again in a moment' : 'aesel is offline: nothing can be built just now');
    return false;
  }
  prompt.busy = true;
  prompt.open = false;
  radial.close();
  releaseInput();
  // Pointer capture must start inside the gesture, not the later server reply.
  if (gesture) void capture();
  // 🧱 Asked through the build line: it names the asking, says it again if no answer comes, and shows where it stands until it ends.
  builds.ask({
    type: 'prompt',
    prompt: prompt.text,
    objectId: prompt.target?.id,
    baseVersion: prompt.target?.version,
    item,
    chalk,
  }, prompt.text, room);
  sfx.buildAsk();
  return true;
}
// 🧱 Only a KidLisp wall of this room is changed as one. A standing screen, a piece on a panel and a thing in a room beside this one are marked `wall` as well (looked at, never removed), and used to take this ring for themselves: it offered a wall's changes, its [X] asked for a change to a wall that is not there, and no build was asked for at all. Aimed at one of those, the ring is for a new thing.
const wallOf = (o) => (o?.wall && walls.some((w) => `wall:${w.code}` === o.id) ? o : null);
function openRadial(wall = null, bumper = false) {
  if (seat < 0 || radial.open) return;
  if (prompt.busy) {
    refuseBuild('', builds.live >= 3 ? 'aesel is already making three things for you' : 'still waiting for word of the last one');
    sfx.refuse();
    return;
  }
  const target = wallOf(wall) || (focusObject?.owner === owner && !focusObject?.wall ? focusObject : null);
  releaseInput();
  document.exitPointerLock();
  canvas.focus();
  prompt.text = '';
  prompt.begin(target);
  radial.begin(target, bumper);
  sfx.ringOpen();
}
function closeBuild() {
  if (radial.bumper) {
    prompt.text = '';
    prompt.cursor = 0;
  }
  prompt.open = false;
  radial.close();
  releaseInput();
}
function pickRadial(index) {
  if (!radial.pick(index)) return;
  // Chalk category: erase, or step out of the ring to draw the shape.
  if (!radial.bumper && radial.chalk && radial.picks.length === 1) {
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
const touchMode = () => (seat < 0 || down || radial.open || menu ? null : chalkMode ? 'chalk' : typing() ? 'typing' : canBike() ? 'play' : null);
// 🛵 Where a bike may be ridden (bike.mjs), so its touch cap shows only there.
const canBike = () => !!player && rideable(fixedBoxes(), flat(), player);
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
const ringCap = { name: 'ring', color: 0x9be7a0, act: () => { softKeys.blur(); openRadial(); } };
const shownButtons = () => {
  const mode = touch.active && touchMode();
  if (!mode) return [];
  const keyboardUp = mode === 'typing';
  return touchButtons(mode, canvas.clientWidth, keyboardUp ? visibleHeight() : canvas.clientHeight, {
    context: mode === 'typing' && prompt.open && !prompt.busy ? [ringCap] : mode === 'play' ? bikeButtons(!!player?.bike) : [],
    inset: keyboardUp && visibleHeight() < canvas.clientHeight ? 0 : bottomInset(),
  });
};
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
function screenTargets(only = null) {
  const targets = [];
  for (const panel of panels.list) {
    if (!panel.hidden && (!only || only === 'panel:' + panel.id)) targets.push({ id: 'panel:' + panel.id, panel, face: panel });
  }
  for (const id of only ? [only] : [...screens.map(s => 'screen:' + s.id), ...walls.map(w => 'wall:' + w.code)]) {
    if (id.startsWith('panel:')) continue;
    const face = faceOf(id);
    const instance = id.startsWith('screen:') ? screenInstances.get(room + '/' + id.slice(7)) : null;
    const program = instance?.pressed || instance?.program || walls.find(w => 'wall:' + w.code === id);
    if (face && program?.point) targets.push({ id, face, program, instance });
  }
  return targets;
}
function nearestScreen(targets, intersect) {
  let best = null;
  for (const target of targets) {
    const hit = intersect(target.face);
    if (hit && (!best || hit.t < best.hit.t)) best = { target, hit };
  }
  return best;
}
function startScreenContact(id, { target, hit }) {
  const used = new Set([...screenFingers.values()].map(f => f.pointer));
  let pointer = padHolds || keyHolds || screenPress ? 2 : 1;
  while (used.has(pointer)) pointer++;
  if (pointer > 16) return;
  const finger = { ...target, pointer, x: hit.x, y: hit.y };
  if (target.instance) target.instance.pressed = target.program;
  screenFingers.set(id, finger);
  tellScreenFinger(finger, 'touch', hit);
}
function beginScreenFinger(e) {
  if (dock && view.overHud(e.offsetX, e.offsetY)) return false;
  const eye = view.aimOrigin(), ray = view.aimRay(e.offsetX, e.offsetY);
  if (!eye || !ray || dock?.way < 0) return false;
  const target = nearestScreen(screenTargets(dock?.id), face => panelHit(face, eye, ray, dock ? Infinity : PANEL_REACH));
  if (!target) return false;
  startScreenContact(e.pointerId, target);
  canvas.setPointerCapture(e.pointerId);
  return true;
}
const xrTriggers = new Set();
function releaseXRContacts() {
  for (const [id, f] of screenFingers) if (typeof id === 'string') endScreenFinger(id, f);
  xrTriggers.clear();
}
function updateXRContacts(xr, base) {
  if (!xr.hands.length && !xr.rays.length) { releaseXRContacts(); return; }
  const eye = { x: base.x + xr.offset[0], y: base.y + xr.offset[1], z: base.z + xr.offset[2] };
  const world = p => ({ x: eye.x + p.x, y: eye.y + p.y, z: eye.z + p.z, radius: p.radius });
  const targets = screenTargets(), seen = new Set();
  const currentTargets = was => was ? targets.filter(t => t.id === was.id && t.program === was.program && t.panel === was.panel) : targets;
  const update = (id, was, hit) => {
    if (!hit) { if (was) endScreenFinger(id, was); return; }
    if (!was) startScreenContact(id, hit);
    else if (Math.floor(was.x) !== Math.floor(hit.hit.x) || Math.floor(was.y) !== Math.floor(hit.hit.y)) tellScreenFinger(was, 'draw', hit.hit);
  };
  for (const hand of xr.hands) hand.fingers.forEach((chain, i) => {
    const tip = chain.at(-1);
    if (!tip) return;
    const id = hand.side + i, was = screenFingers.get(id);
    seen.add(id);
    const at = world(tip);
    const hit = nearestScreen(currentTargets(was), face => fingertipHit(face, at, eye, !!was));
    update(id, was, hit);
  });
  for (const ray of xr.rays) {
    const id = ray.side + '-trigger', was = screenFingers.get(id);
    seen.add(id);
    const at = world(ray.origin);
    const hit = nearestScreen(currentTargets(was), face => panelHit(face, at, ray.dir, PANEL_REACH));
    if (hit) ray.distance = hit.hit.t;
    if (was || !xrTriggers.has(id)) update(id, was, ray.down ? hit : null);
    if (ray.down) xrTriggers.add(id); else xrTriggers.delete(id);
  }
  for (const [id, f] of screenFingers) if (typeof id === 'string' && !seen.has(id)) endScreenFinger(id, f);
  for (const id of xrTriggers) if (!seen.has(id)) xrTriggers.delete(id);
}
function tellScreenFinger(f, name, hit) {
  f.x = hit?.x ?? -1; f.y = hit?.y ?? -1;
  if (f.panel) {
    f.panel.act(name, f.x, f.y, 'touch', f.pointer);
  } else {
    const x = Math.floor(f.x * f.program.width / f.face.cols), y = Math.floor(f.y * f.program.height / f.face.rows);
    f.program.point(name, x, y, f.pointer, 'touch');
  }
}
function endScreenFinger(id, hit) {
  const f = screenFingers.get(id);
  if (!f) return;
  screenFingers.delete(id);
  tellScreenFinger(f, 'lift', hit);
  if (f.instance && screenPress?.instance !== f.instance && ![...screenFingers.values()].some(other => other.instance === f.instance))
    f.instance.pressed = null;
}
function trackScreenFinger(e, lift = false) {
  const f = screenFingers.get(e.pointerId);
  if (!f) return false;
  const eye = view.aimOrigin(), ray = view.aimRay(e.offsetX, e.offsetY);
  const hit = eye && ray && panelHit(f.face, eye, ray);
  if (lift) endScreenFinger(e.pointerId, hit);
  else if (Math.floor(f.x) !== Math.floor(hit?.x ?? -1) || Math.floor(f.y) !== Math.floor(hit?.y ?? -1)) tellScreenFinger(f, 'draw', hit);
  return true;
}
canvas.addEventListener('pointerdown', (e) => {
  if (!touchy(e)) return;
  const mode = view.headsetHit(e.offsetX, e.offsetY);
  if (mode) { headsetTap = { id: e.pointerId, mode }; canvas.setPointerCapture(e.pointerId); return; }
  e.preventDefault();
  if (view.cornerHit(e.offsetX, e.offsetY)) return leave();
  if (view.pairHit(e.offsetX, e.offsetY)) return askAccount('account');
  if (selfMember?.canTakeBody && view.handleHit(e.offsetX, e.offsetY)) return send({ type: 'take-body' });
  if (!typing() && !chalkMode && !menu && !radial.open && beginScreenFinger(e)) {
    touch.active = true;
    void sfx.wake();
    return;
  }
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
  if (touchy(e) && !trackScreenFinger(e)) touch.move(e, canvas.clientWidth, canvas.clientHeight);
});
// The prompt and its room share an origin. Subscribe in the visible top-level page on phones.
const motionHost = hosted && document.referrer && new URL(document.referrer).origin === location.origin ? window.parent : window;
const gyro = gyroLook(motionHost,
  () => touch.active && sheetOpen && !document.hidden && !down &&
    !typing() && !menu && !radial.open && !dock && !lockOn && !screenFingers.size && !game()?.camera && !away && !resting,
  (dx, dy) => {
    yaw = (yaw + dx / focusZoom + 360) % 360;
    pitch = clamp(pitch + dy / focusZoom, -85, 85);
    if (Math.hypot(dx, dy) > 0.08) inputAt = performance.now();
  });
window.addEventListener('pagehide', e => { if (!e.persisted) gyro.dispose(); else gyro.reset(); });
function enablePhoneLook() {
  if (sheetOpen && !typing() && !menu && !radial.open) void gyro.enable();
}
canvas.addEventListener('touchend', enablePhoneLook, { passive: true });
canvas.addEventListener('pointerup', (e) => {
  if (headsetTap?.id === e.pointerId) {
    const { mode } = headsetTap;
    headsetTap = null; headsetTappedAt = performance.now();
    if (view.headsetHit(e.offsetX, e.offsetY) === mode) void headset.enter(mode);
    return;
  }
  if (!touchy(e)) return;
  if (e.pointerType === 'touch') enablePhoneLook();
  if (trackScreenFinger(e, true)) return;
  touchButton(touch.up(e, performance.now()), false);
});
canvas.addEventListener('pointercancel', (e) => {
  if (headsetTap?.id === e.pointerId) headsetTap = null;
  if (!touchy(e)) return;
  endScreenFinger(e.pointerId, null);
  const held = touch.cancel(e);
  if (held && !held.act) pressKey('keyup', held.code, held.key);
});
canvas.addEventListener('lostpointercapture', e => endScreenFinger(e.pointerId, null));
function releaseInput({ keepKeys = false } = {}) {
  headsetTap = null;
  releaseXRContacts();
  for (const id of screenFingers.keys()) endScreenFinger(id, null);
  gyro.reset();
  lastMotionLook = null;
  sfx.motion();
  panels.release();
  yardQuiet();
  if (keepKeys) return;
  liftPieceKeys();
  keys.clear();
  firing = jumpQueued = fireQueued = false;
}
// 🖱️ The mouse taken again (a build asked for re-captures it) keeps the keys
// already held: a walk begun on Enter goes on. Only letting go clears them.
// The first moves after taking it can carry the cursor's whole jump to the
// centre, so the view stays put for a moment instead of snapping round.
let mouseTakenAt = -1e9;
document.addEventListener('pointerlockchange', () => {
  releaseInput({ keepKeys: locked() });
  if (locked()) mouseTakenAt = performance.now();
  if (!locked()) {
    unlockedAt = performance.now();
    // ⎋ Escape's first step, the mouse let go: a yellow border and a ding. (Not over the second step's, when leaving let it go.)
    if (!escFlash || unlockedAt - escFlash.at > 400) { escFlash = { color: 0xffe066, at: unlockedAt }; sfx.escStep(1); }
  }
});
document.addEventListener('mousemove', (e) => {
  if (e.movementX || e.movementY) inputAt = performance.now(); // (a hand on the mouse: not idle)
  if (!locked()) {
    // A free mouse: remember where it is; the aim ray goes through it.
    const r = canvas.getBoundingClientRect();
    mouseAt = { x: e.clientX - r.left, y: e.clientY - r.top, at: performance.now() };
    return;
  }
  if (prompt.open) return;
  if (performance.now() - mouseTakenAt < 120 || Math.abs(e.movementX) > 400 || Math.abs(e.movementY) > 400) return;
  yaw = (yaw - e.movementX * 0.12 + 360) % 360;
  pitch = clamp(pitch - e.movementY * 0.12, -85, 85);
});
document.addEventListener('keydown', (e) => {
  if (!typing() && !e.repeat && e.shiftKey && !e.metaKey && !e.ctrlKey && !e.altKey && (e.code === 'KeyV' || e.code === 'KeyM')) {
    const mode = e.code === 'KeyV' ? 'immersive-vr' : 'immersive-ar';
    if (headset.controls?.modes.includes(mode)) { e.preventDefault(); void headset.enter(mode); return; }
  }
  void sfx.wake();
  // ⌘+ / ⌘− / ⌘0 under the prompt: its pixel density is ours (we lay our
  // pixels on its grid), so hand them up and the new grid comes back down.
  if (hosted && (e.metaKey || e.ctrlKey) && !e.altKey && ['=', '+', '-', '0'].includes(e.key)) {
    e.preventDefault();
    toHost({ type: 'rax:sheet:density', step: e.key === '0' ? 0 : e.key === '-' ? -1 : 1 });
    return;
  }
  if (radial.bumper && e.code !== 'Escape') { e.preventDefault(); return; }
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
  // Z toggles screen focus. Leave modified Z to the browser's undo shortcut.
  if (e.code === 'KeyZ' && (e.metaKey || e.ctrlKey || e.altKey)) return;
  // The focused piece receives the other keys. Escape leaves and Tab opens the prompt.
  const inside = keyPanel();
  if (inside && !INSIDE_KEYS.has(e.code) && !e.metaKey && !e.ctrlKey) {
    e.preventDefault();
    pieceKeys.set(e.code, { panel: inside, key: e.key });
    inside.key('down', e);
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
  if (e.code === 'Escape' && !e.repeat && (prompt.busy || (!prompt.open && jobs.some(mineJob)))) {
    e.preventDefault();
    send({ type: 'cancel-build', in: room });
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
  // 🖥️ Gone in to a screen, Escape brings the eye back.
  if (dock && e.code === 'Escape' && !e.repeat && !prompt.open && !chat.open) {
    e.preventDefault();
    comeOut();
    sfx.ringBack();
    return;
  }
  if (hosted && e.code === 'Escape' && !e.repeat && !prompt.open) {
    e.preventDefault();
    if (locked()) document.exitPointerLock();
    else if (performance.now() - unlockedAt > 250) leave();
    return;
  }
  if (e.code === 'Enter' && !e.repeat) {
    e.preventDefault();
    // 🖼️ Aimed at a world panel, Enter presses it.
    if (!prompt.open && !menu && !radial.open && !chalkMode && panels.hot) return void (e.repeat || (keyHolds = panels.press())); // (held while the key is down)
    if (seat < 0) return; // Looking on: Enter asks for nothing.
    // 🛗 Standing in the car, Enter asks where to.
    if (!prompt.open && hasCar() && inCar(player) && !menu && !radial.open && !chalkMode) {
      releaseInput();
      liftLine = true;
      chat.text = '';
      chat.begin();
      // Turn to the pad on the back wall: that is where the floor shows (the typed line is on the page's top line too).
      yaw = 0;
      pitch = -3;
      return;
    }
    if (!prompt.open) {
      releaseInput();
      document.exitPointerLock();
      canvas.focus();
      prompt.begin(focusObject?.owner === owner && !focusObject.wall ? focusObject : null); // (a thing of yours seen in the room beside this one is not changed from here: a new thing is asked for)
    } else if (!submitPrompt() && prompt.text.trim()) sfx.refuse();
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
  if (!hosted && !dock && !lockOn && (e.code === 'BracketLeft' || e.code === 'BracketRight') && !typing() && !e.metaKey && !e.ctrlKey) {
    view.density?.(e.code === 'BracketRight' ? 1 : -1);
    e.preventDefault();
    return;
  }
  // 🧰 ` anywhere else toggles the shared frame chart (not while typing).
  if (e.code === 'Backquote' && !e.repeat && !typing()) {
    toggleStats();
    e.preventDefault();
    return;
  }
  if (!locked() && !keyboardMode && !synthetic.has(e)) {
    if (touch.active || !CONTROL_KEYS.includes(e.code)) return;
    keyboardMode = true;
    if (!ws || ws.readyState === WebSocket.CLOSED) connect();
    notice = '';
  }
  // (A letter is rax's too — C's lens, F, Q — and one left to the page is an error ding on every repeat in AC's Mac app.)
  if (CONTROL_KEYS.includes(e.code) || (e.key.length === 1 && !e.metaKey && !e.ctrlKey)) e.preventDefault();
  keys.add(e.code);
  if (seat >= 0 && e.code === 'Space' && !e.repeat) {
    jumpQueued = true;
    sfx.jump();
  }
  if (seat >= 0 && e.code === 'KeyV' && !e.repeat) third = !third;
  // 🚁 Looking on, F fills the screen (as VIEW does on a pad) and Escape lets a lock go (as B does).
  // (F is the focus key: with a thing to lock on to, or a lock to let go, it does that; with neither it fills the screen.)
  if (seat < 0 && e.code === 'KeyF' && !e.repeat && !typing()) { if (lockOn || dock || focusObject) pressLock(); else fullScreen(); }
  if (seat < 0 && lockOn && e.code === 'Escape') { selectTarget(null); e.preventDefault(); return; }
  if (seat < 0 && e.code === 'KeyB' && !e.repeat && !typing() && !e.metaKey && !e.ctrlKey) startWarp(); // 🌀 (as B does on a pad)
  // 🚁 Looking on, V steps back to see your own drone.
  if (seat < 0 && selfMember?.canTakeBody && e.code === 'KeyP' && !e.repeat && !typing() && !menu && !dock) send({ type: 'take-body' });
  if (seat < 0 && e.code === 'KeyV' && !e.repeat && !typing()) droneView = !droneView;
  // 🎯 F is the focus key, beside WASD (2026-11-27; it was Z, which still
  // works): looking at a screen it goes in, gone in or locked on it comes
  // out. Anywhere else it is `use`, as before. Inside a piece that hears the
  // keyboard F is the piece's own key (a note, on the Notepat) and never
  // reaches here: Escape or Z comes out of those.
  if (seat >= 0 && e.code === 'KeyF' && !e.repeat) {
    if (dock || lockOn || focusObject?.panel) pressLock();
    else if (focusObject?.wall && !holding()) openRadial(focusObject);
    else useQueued = true;
  }
  if (seat >= 0 && e.code === 'KeyQ' && !e.repeat) dropQueued = true;
  // 🛵 B: on or off your cabin bike (bike.mjs). Space lifts it, shift lets it sink.
  if (seat >= 0 && e.code === 'KeyB' && !e.repeat && !typing() && !e.metaKey && !e.ctrlKey) bikeQueued = true;
  // Lock on, as the pad's RB does (2026-11-08): Z toggles it, [ and ]
  // change it like the d-pad, and delete removes the locked object at once
  // (the lock is the confirmation, as B on the pad).
  if (e.code === 'KeyZ' && !e.repeat && (seat >= 0 || !typing())) pressLock();
  // (the arrows step it too: a lock holds the eye, so they have no turning to do; gone in to a screen they move its mouse)
  // (In at a screen the arrows are its mouse; the brackets still step.)
  if ((lockOn || dock) && !e.repeat && !typing() && (dock ? ['BracketLeft', 'BracketRight'] : ['BracketLeft', 'BracketRight', 'ArrowLeft', 'ArrowRight']).includes(e.code)) cycleLock(e.code === 'BracketRight' || e.code === 'ArrowRight' ? 1 : -1);
  if (seat >= 0 && (e.code === 'Delete' || e.code === 'Backspace') && !e.repeat) {
    if (lockOn && !lockOn.wall) {
      send({ type: 'remove', objectId: lockOn.id });
      selectTarget(null);
      sfx.ringBack();
    } else if (focusObject && !focusObject.wall) removeAimed();
  }
  if (seat >= 0 && e.code === 'KeyX' && !e.repeat && !holding() && !chalkMode) paintSquare();
});
// Paint the floor square the aim ray lands on, never
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
document.addEventListener('keyup', (e) => {
  // ⌨️ A key the piece was given goes up in the piece (wherever the eye is by then).
  const given = pieceKeys.get(e.code);
  if (given) { pieceKeys.delete(e.code); given.panel.key('up', e); return; }
  keys.delete(e.code);
  if (keyHolds && (e.code === 'Enter' || e.code === 'NumpadEnter')) { keyHolds = false; panels.release(); }
});
document.addEventListener('mousedown', (e) => {
  // 🖼️ A click on a world panel is that panel's (panel.mjs): down now, up on the mouse's release.
  if (locked() && e.button === 0 && panels.hot && !menu && !radial.open && !prompt.open) {
    e.preventDefault();
    panels.press();
    return;
  }
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
  if (e.button === 0 && !panels.pinned) panels.release();
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
function cancelBumper() {
  bumperStart = null;
  if (radial.bumper) closeBuild();
}
window.addEventListener('blur', () => { cancelBumper(); releaseInput(); liftPieceKeys(); });
document.addEventListener('visibilitychange', () => {
  notepatScreen.visibility();
  cancelBumper();
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
let lastBody = null; // { room, x, y, z, yaw, pitch }: where this page's body last stood, and how it looked
// The far, turning view behind the curtain: 1 there, easing to 0 over 1.4 s
// once the paper drops, and back out as it returns.
let orbitMix = hosted ? 1 : 0, orbitAt = performance.now();
function orbitStep(now) {
  const want = hosted && (!sheetOpen || closingAt) ? 1 : 0;
  const dt = Math.min(0.1, (now - orbitAt) / 1000);
  orbitAt = now;
  const rate = want ? 1 / 0.64 : 1 / 1.4; // back out on the paper's own beat home (lib/rax-sheet.mjs HOME_BEAT)
  orbitMix = want > orbitMix ? Math.min(want, orbitMix + rate * dt) : Math.max(want, orbitMix - rate * dt);
  return orbitMix;
}
// On the way home the reticle flies to the prompt's crosshair corner on the
// prompt's own path and clock (prompt.mjs raxFlight "back": 360 ms, ease-out,
// to 14 px in from the bottom-right), in buffer pixels from the centre, y up.
function reticleShift(now) {
  if (!closingAt) return null;
  const t = Math.min(1, (now - closingAt) / 640), ease = 1 - (1 - t) * (1 - t);
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
  presentation.correct(player, (changedLife && !held?.walk) || resetPrediction);
  resetPrediction = false;
  // 👀 A body set down where it was left looks where it was looking (the eye is this page's own; the server's word on facing is for a body it has put somewhere new).
  const woke = changedLife && !carried && lastBody?.room === room && Math.hypot(own.x - lastBody.x, own.z - lastBody.z) < 1.5;
  if (woke) { yaw = lastBody.yaw; pitch = lastBody.pitch; }
  else if (changedLife && !carried) { yaw = own.yaw; pitch = own.pitch; }
  if (player) lastBody = { room, x: player.x, y: player.y, z: player.z, yaw, pitch };
  diagnostics.max('reconcileMaxMs', performance.now() - began);
  diagnostics.max('pendingCommandsMax', pending.length);
}
function frame(now, xr) {
  bootMark('frame');
  // 🧊 A gap since the last frame, while someone is in the room looking.
  stalls.begin(now, !document.hidden && document.hasFocus() && (hosted ? sheetShown : true) && !resting && !away, {
    building: jobs.length, objects: objects.length, pending: pending.length,
    rtt: Number.isFinite(connection.rtt) ? connection.rtt : undefined,
    backend: canvas.raxStats?.backend, agent: AGENT, hosted,
  });
  if (away && !sheetOpen) return; // Unseen under another piece: no step, no draw.
  // With the host window in the background and the paper down, the room
  // still turns smoothly but draws at most 60 times a second, half the work
  // on a 120 Hz display (2026-11-08).
  const paced = resting && !sheetOpen;
  if (paced) {
    if (now - lastBackgroundFrameAt < 1000 / BACKGROUND_FPS - 2) return;
    lastBackgroundFrameAt = now;
  }
  if (!paced) lcd?.off();
  // Full rate for a beat after the paper is back, so the return never stutters.
  // (The corner world runs at full rate: stepping it at 20 fps made the
  // turning room's edges flicker, 2026-11-03.)
  const began = performance.now();
  let predicted = 0;
  try {
    applyPrediction();
    predicted = performance.now() - began;
    step(now, xr);
  } catch (error) {
    if (diagnostics.enabled) diagnostics.values.frameErrors++;
    console.error('rax frame', error);
  }
  if (paced) lcd?.frame();
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
function step(now, xr) {
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
  gamepad = controller.sample(down || xr ? [] : pads, document.hasFocus());
  if (xr) {
    yaw = xr.yaw; pitch = xr.pitch;
    inputAt = flownAt = now;
    if (!down) Object.assign(gamepad, { right: xr.right, fwd: xr.fwd || (xr.walkPinch && !screenFingers.size ? 0.42 : 0), jump: xr.jump });
  }
  if (gamepad.fwd || gamepad.right || gamepad.lookX || gamepad.lookY ||
      Array.from(pads || []).some(p => p?.connected && p.buttons.some(b => b.pressed || b.value > 0.08))) {
    pointerSource = 'pad';
    inputAt = now;
  } else if (keys.size) inputAt = now;
  if (seat < 0 && selfMember?.canTakeBody && gamepad.zoomClick && !dock && !menu && !typing()) send({ type: 'take-body' });
  const canBuild = sheetOpen && seat >= 0 && !down && !joining() && !menu && !chat.open && !chalkMode;
  if (gamepad.buildPress && canBuild && !typing() && !radial.open) bumperStart = now;
  if (bumperStart !== null && (!canBuild || !gamepad.buildHeld || typing())) bumperStart = null;
  if (bumperStart !== null && now - bumperStart >= HOLD_SECONDS * 1000) {
    bumperStart = null;
    openRadial(focusObject, true);
  }
  if (radial.bumper) {
    const { lookX, lookY } = gamepad;
    radial.centered = Math.hypot(lookX, lookY) < 0.22;
    const prev = radial.selected;
    radial.point(lookX, -lookY);
    if (!canBuild || !gamepad.buildHeld) {
      if (canBuild && gamepad.buildRelease && radial.centered && prompt.text.trim()) {
        if (radial.chalk && radial.picks[0] !== 'erase my chalk') {
          chalkMode = { text: radial.picks[0], target: radial.target };
          send({ type: 'chalk-clear' });
        } else if (!submitPrompt(false)) sfx.refuse();
      }
      closeBuild();
    } else {
      if (radial.selected >= 0 && radial.selected !== prev) sfx.ringMove(radial.selected);
      if (gamepad.select && radial.selected >= 0 && !(radial.chalk && radial.picks.length)) pickRadial(radial.selected);
      if (gamepad.cancel) backRadial();
    }
    gamepad = neutralPad();
  }
  // 🧱 The build line's clock: an asking unheard is said once more (the same note, lower), then given up with the failing two notes and its row saying so.
  for (const e of builds.tick()) {
    if (e.type === 'again') sfx.buildAsk(true);
    else {
      sfx.buildFail(e.build.id);
      prompt.busy = seat >= 0 && jobs.filter((j) => j.owner === owner).length >= 3;
    }
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
  const sticksNow = sheetOpen && !!(gamepad.fwd || gamepad.right || gamepad.lookX || gamepad.lookY);
  if ((!sfx.ready && sticksNow && !sticksMoving) || gamepad.focus || gamepad.zoomClick || gamepad.use || gamepad.select || gamepad.jump || gamepad.prompt || gamepad.cancel || gamepad.start) void sfx.wake();
  sticksMoving = sticksNow;
  dragBrush();
  // START goes to the prompt, as the corner says: under the prompt it turns the paper (the prompt's page hears the pad itself, so it works from both sides: lib/rax-sheet.mjs); standing alone it leaves for the prompt, as Escape does. (The $ is on the d-pad.)
  if (gamepad.start && !hosted) leave();
  // ➡️ Right on the d-pad opens the $ (not while locked on, where it steps the lock, nor while typing); once open the d-pad moves in it.
  let opened = false;
  const riding = seat >= 0 && hasCar() && inCar(player);
  if (gamepad.dright && !dock && !lockOn && !menu && !prompt.open && !radial.open) { toggleMenu(); opened = true; }
  // 🛵 Left on the d-pad: on or off the cabin bike (as B is on the keys). A held lifts it, B held lets it sink.
  if (gamepad.dleft && seat >= 0 && !dock && !lockOn && !menu && !prompt.open && !radial.open) bikeQueued = true;
  // 🔬 Up and down on the d-pad make the picture sharper or chunkier, live (in the car they ride a floor, and in the $ they move): under the prompt the density is the page's (lib/rax-sheet.mjs), which clicks like a shutter as it changes.
  if ((gamepad.dup || gamepad.ddown) && !menu && !opened && !riding && !prompt.open && !radial.open) {
    const step = gamepad.dup ? 1 : -1;
    if (hosted) toHost({ type: 'rax:sheet:density', step });
    else { view.density?.(step); sfx.shutter?.(step); }
  }
  // VIEW (back) fills the screen with the room, and gives it back.
  if (gamepad.swap) fullScreen();
  // 🧰 The right stick, clicked, shows and hides the debugging layer, as ` does: the frame meter, where you stand in metres, and the fences on the lines between rooms.
  if (gamepad.debug) {
    toggleStats();
    sfx.ringMove(statsShown ? 7 : 2);
  }
  if (menu) {
    // Move with the d-pad or the stick, choose with A, close with B.
    let step = opened ? 0 : (gamepad.ddown ? 1 : 0) - (gamepad.dup ? 1 : 0);
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
    gamepad = { ...gamepad, select: false, cancel: false, jump: false, use: false, focus: false, zoomClick: false, prompt: false, fwd: 0, right: 0, lookX: 0, lookY: 0 };
  }
  // A and RT both hold a panel at its cursor until the last button lifts.
  if (!menu && !radial.open && !typing() && !chalkMode && panels.hot && (gamepad.select || gamepad.use)) {
    if (!padHolds) padHolds = panels.press();
    gamepad = { ...gamepad, select: false, use: false, jump: false };
  }
  if (!menu && !radial.open && !typing() && (gamepad.select || gamepad.use) && (screenPress || pressScreen()))
    gamepad = { ...gamepad, select: false, use: false, jump: false };
  if (screenPress && (dock !== screenPress.dock || dock.way < 0 || !(gamepad.hold || gamepad.triggerHeld))) pointScreen('lift');
  if (padHolds && !gamepad.hold && !gamepad.triggerHeld) { padHolds = false; panels.release(); }
  // 🛗 In the car the d-pad rides a floor up or down.
  if (!menu && riding && gamepad.floorDirection && (gamepad.dup || gamepad.ddown || (padFloorRepeat && now >= padFloorRepeat))) {
    if (!padFloorRepeat || gamepad.dup || gamepad.ddown) padFloorStart = now;
    selectFloor(gamepad.floorDirection > 0 ? 'up' : 'down', now - padFloorStart);
    padFloorRepeat = now + (gamepad.dup || gamepad.ddown ? 300 : 140);
  } else if (!gamepad.floorDirection || !riding || menu) padFloorRepeat = 0;
  const pad = touch.sample(canvas.clientWidth, canvas.clientHeight, now);
  for (const event of pad.events) {
    if (event.type === 'tap' && radial.open && !radial.bumper && event.menu) {
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
  // RB toggles a lock on the aimed object; while locked, B removes it (the
  // lock is the confirmation).
  // (Looking on, the bumpers roll, so the lock is on X; B lets go.)
  if (dock && gamepad.cancel && !radial.open && !prompt.open && !chalkMode) {
    comeOut();
    gamepad.cancel = false;
    sfx.ringBack();
  }
  if (seat < 0 && gamepad.cancel && !radial.open && !prompt.open && !chalkMode) {
    if (lockOn) selectTarget(null);
    else { startWarp(); gamepad.cancel = false; } // 🌀 with nothing to let go, B warps it home
  }
  if ((seat >= 0 ? gamepad.focus : gamepad.prompt) && !typing() && !radial.open && !menu) pressLock();
  if ((lockOn || dock) && !menu && (gamepad.dleft || gamepad.dright)) cycleLock(gamepad.dright ? 1 : -1); // (in at a screen too: the d-pad steps on to the next)
  if (lockOn && seat >= 0 && gamepad.cancel && !prompt.open && !radial.open && !lockOn.wall) {
    send({ type: 'remove', objectId: lockOn.id });
    selectTarget(null);
    gamepad.cancel = false;
    sfx.ringBack();
  }
  if (radial.open && !radial.bumper) {
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
  if (gamepad.use && seat >= 0 && !typing() && !radial.open && !menu && !chalkMode && holding())
    useQueued = true;
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
    if (!dock && !lockOn) {
      const dt = Math.min(0.1, elapsed) / focusZoom;
      yaw = (yaw - gamepad.lookX * 160 * dt + 360) % 360;
      pitch = clamp(pitch - gamepad.lookY * 120 * dt, -85, 85);
    }
    // Arrow keys turn and tilt at a steady rate for mouse-free play.
    const turnX = Number(keys.has('ArrowRight')) - Number(keys.has('ArrowLeft')),
      turnY = Number(keys.has('ArrowDown')) - Number(keys.has('ArrowUp'));
    yaw = (yaw - turnX * 130 * Math.min(0.1, elapsed) + 360) % 360;
    pitch = clamp(pitch - turnY * 90 * Math.min(0.1, elapsed), -85, 85);
    if (seat >= 0 && gamepad.jump && !dock && !radial.open) {
      jumpQueued = true;
      sfx.jump();
    }
    if (seat >= 0 && gamepad.camera) third = !third;
    if (seat < 0 && gamepad.camera) droneView = !droneView;
  }
  moveTargetCursor(elapsed);
  if (screenPress) pointScreen('draw');
  const yawBefore = yaw, pitchBefore = pitch;
  if (lockOn && (player || seat < 0)) {
    const target = spotOf(lockOn.id), from = player || freecam; // (a drone turns its eye to it)
    if (!target) selectTarget(null);
    else {
      const dx = target.x - from.x, dz = target.z - from.z, dy = target.y - from.y;
      const wantYaw = (Math.atan2(dx, dz) * 180) / Math.PI;
      const wantPitch = (Math.atan2(dy, Math.hypot(dx, dz)) * 180) / Math.PI;
      const dyaw = ((wantYaw - yaw + 540) % 360) - 180;
      const k = 1 - Math.exp(-elapsed * 8);
      yaw = (yaw + dyaw * k + 360) % 360;
      pitch = clamp(pitch + (wantPitch - pitch) * k, -85, 85);
    }
  }
  // 🎥 The eye carried to a screen and back, and a drone up to what it locked on to; its servos are heard only while it is.
  {
    const heard = carry(elapsed, now, Math.abs(((yaw - yawBefore + 540) % 360) - 180) + Math.abs(pitch - pitchBefore));
    sfx.servo(Math.max(heard.lens, focusStep(elapsed)), heard.travel, heard.turn);
    whirr = heard;
  }
  if (elapsed > 0.25) presentation.reset(player);
  debt += Math.min(0.1, elapsed);
  last = now;
  if (seat >= 0 && (keys.size || gamepad.fwd || gamepad.right || gamepad.lookX || gamepad.lookY || pad.fwd || pad.right || pad.dx || pad.dy))
    activeSeconds += Math.min(0.1, elapsed);
  const connected = ws?.readyState === WebSocket.OPEN && snapshot;
  if (seat < 0 && connected && !joining()) {
    // 🛫 Nobody at the stick for a while, over the plaza: the drone takes itself on its tour (flyover.mjs). Any key, stick or look gives it back.
    const moving = keys.size || gamepad.fwd || gamepad.right || gamepad.lookX || gamepad.lookY || gamepad.roll || gamepad.lt > 0.06 || gamepad.rt > 0.06 || gamepad.hold || pad.fwd || pad.right || pad.dx || pad.dy;
    if (moving) inputAt = now;
    // (A look given by hand, mouse or finger, shows as a turn the follow did not make.)
    if (moving || (followLook && (Math.abs(yaw - followLook.yaw) > 0.01 || Math.abs(pitch - followLook.pitch) > 0.01))) flownAt = now;
    followLook = null;
    following = !!selfMember?.canTakeBody && !dock && !lockOn && !nearing && !query.has('film') && !held && !warp && now - flownAt > FOLLOW_AFTER_MS;
    // The room is told, so the drone is brought along when the body goes to another room (server.mjs `follow`).
    if (followSaid?.ws !== ws || followSaid.on !== following) { followSaid = { ws, on: following }; send({ type: 'follow', on: following }); }
    const followed = following ? interpolate(history, playback.tick ?? snapshot.tick, ownBodySeat(), playback.ahead || 0) : null;
    if (!followed?.alive) followFlight = null;
    const idle = sheetOpen && room === 'lobby' && !dock && !lockOn && !query.has('film') && !held && !game() && !prompt.open && !chat.open && !following && now - inputAt > IDLE_MS;
    if (followed?.alive) {
      touring = null;
      // 🚁 It flies after the body as a drone does, never set down beside it: behind it and a little over it while it walks, looking where it looks; and when it has stood a moment, round it in a slow circle, looking at it.
      const dt = Math.min(0.1, elapsed), was = followFlight;
      const pace = was && dt ? Math.hypot(followed.x - was.x, followed.z - was.z) / dt : 0;
      const stoodAt = pace > 0.4 || !was ? now : was.stoodAt;
      const round = smooth((now - stoodAt - 1500) / 2500); // 0 walking → 1 circling
      const behind = ((followed.yaw + 180) * Math.PI) / 180;
      // (The circle is taken up from wherever the drone already is about the body.)
      let angle = round > 0 ? (was?.round > 0 ? was.angle + dt * 0.3 : Math.atan2(freecam.x - followed.x, freecam.z - followed.z)) : behind;
      const rise = 0.32 + 0.1 * round, len = Math.hypot(1, rise);
      const out = { x: Math.sin(angle) / len, y: rise / len, z: Math.cos(angle) / len };
      const origin = { x: followed.x, y: followed.y + 0.15, z: followed.z };
      const far = Math.max(0.5, Math.min(3.4 + 0.8 * round, airFrom(origin, out, 4.5) - 0.3)); // (never the far side of a wall)
      const to = { x: origin.x + out.x * far, y: origin.y + out.y * far, z: origin.z + out.z * far };
      followFlight = { x: followed.x, z: followed.z, stoodAt, round, angle };
      // Flown there: it leans into the way it must go and takes a moment to get up to speed, faster the further it has to come.
      const gx = to.x - freecam.x, gy = to.y - freecam.y, gz = to.z - freecam.z, gap = Math.hypot(gx, gy, gz);
      const top = clamp(gap * 1.5, 8, 70), want = Math.min(top, gap * 3.5) / (gap || 1), kv = 1 - Math.exp(-dt * 6);
      const vx = (freecam.vx || 0) + (gx * want - (freecam.vx || 0)) * kv, vy = (freecam.vy || 0) + (gy * want - (freecam.vy || 0)) * kv, vz = (freecam.vz || 0) + (gz * want - (freecam.vz || 0)) * kv;
      // The eye: where the body looks while it walks, at the body while circling (and while still a way off).
      const head = { x: followed.x - freecam.x, y: followed.y - freecam.y, z: followed.z - freecam.z };
      const at = Math.max(round, smooth((gap - 4) / 6));
      const atYaw = (Math.atan2(head.x, head.z) * 180) / Math.PI, atPitch = (Math.atan2(head.y, Math.hypot(head.x, head.z)) * 180) / Math.PI;
      const wantYaw = followed.yaw + ((((atYaw - followed.yaw + 540) % 360) - 180) * at);
      const wantPitch = clamp(followed.pitch - 8 + (atPitch - (followed.pitch - 8)) * at, -85, 85);
      const turn = ((wantYaw - yaw + 540) % 360) - 180, kl = 1 - Math.exp(-dt * 7);
      yaw = (yaw + turn * kl + 360) % 360;
      pitch = clamp(pitch + (wantPitch - pitch) * kl, -85, 85);
      freecam = { ...freecam, x: freecam.x + vx * dt, y: freecam.y + vy * dt, z: freecam.z + vz * dt, vx, vy, vz, yaw, pitch };
      droneRoll -= Math.round(droneRoll / (2 * Math.PI)) * 2 * Math.PI;
      droneRoll += (clamp(-turn * 0.012, -0.3, 0.3) - droneRoll) * (1 - Math.exp(-dt * 3));
      followLook = { yaw, pitch };
    } else if (idle) {
      // The tour is worked out from what there is to see now (flyover.mjs), when the drone is let go and again at the end of each lap: what was built, or who came in, since is on the next one.
      if (!touring || now > touring.until) {
        const again = !!touring;
        tour = flyover(roofNow(), { sights: sightsNow(), planet: PLANET });
        // (Taken up gently: for four seconds it eases from where it was to where the tour is. A new lap begins where the last one ended.)
        const clock = again ? 0 : nearestOnTour(tour, freecam);
        touring = { at: now, from: { ...freecam }, clock, until: now + (tour.seconds - clock) * 1000 };
      }
      const dt = Math.min(0.1, elapsed), t = touring.clock + (now - touring.at) / 1000, to = tour.at(t);
      const ease = Math.min(1, (now - touring.at) / 4000), e = ease * ease * (3 - 2 * ease);
      const x = touring.from.x + (to.x - touring.from.x) * e, y = touring.from.y + (to.y - touring.from.y) * e, z = touring.from.z + (to.z - touring.from.z) * e;
      const vx = dt ? (x - freecam.x) / dt : 0, vy = dt ? (y - freecam.y) / dt : 0, vz = dt ? (z - freecam.z) / dt : 0;
      freecam = { ...freecam, x, y, z, vx: (freecam.vx || 0) * 0.8 + vx * 0.2, vy: (freecam.vy || 0) * 0.8 + vy * 0.2, vz: (freecam.vz || 0) * 0.8 + vz * 0.2 };
      // The eye turns to what the tour looks at, the short way, unhurried; and banks a little into its turn.
      const wantYaw = (Math.atan2(to.lx - x, to.lz - z) * 180) / Math.PI, wantPitch = (Math.atan2(to.ly - y, Math.hypot(to.lx - x, to.lz - z)) * 180) / Math.PI;
      const dyaw = ((wantYaw - yaw + 540) % 360) - 180, k = 1 - Math.exp(-dt * 2.2);
      yaw = (yaw + dyaw * k + 360) % 360;
      pitch = clamp(pitch + (wantPitch - pitch) * k, -85, 85);
      droneRoll += (clamp(-dyaw * 0.012, -0.35, 0.35) - droneRoll) * (1 - Math.exp(-dt * 2));
    } else if (dock || (nearing && !(keys.has('KeyW') || keys.has('KeyA') || keys.has('KeyS') || keys.has('KeyD') || gamepad.fwd || gamepad.right || gamepad.roll || pad.fwd || pad.right))) {
      touring = null; // 🖥️ carried (`carry`): in to a screen, or up to what it has locked on to, until its own stick asks otherwise
    } else {
      touring = nearing = null;
      // Hold a bumper to roll one turn in four seconds. The gyro levels the drone after release.
      {
        const keyRoll = 0.6 * (Number(keys.has('KeyQ')) - Number(keys.has('KeyR')));
        const turn = clamp(gamepad.roll + keyRoll, -1, 1);
        if (turn) droneRoll += turn * (Math.PI / 2) * Math.min(0.1, elapsed);
        // (Held where it was left for two seconds, then brought level slowly.)
        else if (now - rolledAt > 2000) { droneRoll -= Math.round(droneRoll / (2 * Math.PI)) * 2 * Math.PI; droneRoll *= Math.exp(-Math.min(0.1, elapsed) * 1.2); }
        if (turn) rolledAt = now;
      }
      // 🦾 Looking on, the words-and-calls surface flies the drone as it would walk a body (agent.mjs).
      const flown = seat < 0 ? agentSurface.drive(freecam, now, room) : null;
      if (flown) inputAt = now; // (not left to the idle tour)
      if (Number.isFinite(flown?.yaw)) yaw = flown.yaw;
      freecam = moveFreecam(
        { ...freecam, yaw, pitch },
        {
          fwd: clamp(
            Number(keys.has('KeyW')) - Number(keys.has('KeyS')) + gamepad.fwd + pad.fwd + (flown?.fwd || 0),
            -1,
            1,
          ),
          right: clamp(
            Number(keys.has('KeyD')) - Number(keys.has('KeyA')) + gamepad.right + pad.right + (flown?.right || 0),
            -1,
            1,
          ),
          up: 0,
          boost: gamepad.hold || keys.has('KeyE'),
        },
        elapsed,
        // 🌬️ The air where the eye is, in the world's metres at this moment.
        weather && freecam.y >= 0 ? airAt(weather, freecam.x + (placeOf(room)?.x || 0), freecam.y, freecam.z + (placeOf(room)?.z || 0), Date.now() / 1000, { day: sun(skyPhase()) }) : null, // (no wind inside the planet)
        // 🕳️ Under the land the planet is hollow (freecam.mjs); a floor of the tower, and a game's arena, are only floors.
        !aloft(room) && !game(),
      );
    }
  }
  // 👀 Someone looking on tells the room where their eye is, a few times a
  // second and only when it has moved, so maps can show who is overhead.
  if (seat < 0 && connected && !joining() && sheetOpen && now - lookAt > 100) { // (ten times a second: the room draws it as a drone)
    const said = `${freecam.x.toFixed(1)} ${freecam.y.toFixed(1)} ${freecam.z.toFixed(1)} ${Math.round(yaw / 5)}`;
    if (said !== lookSaid) {
      lookSaid = said;
      lookAt = now;
      send({ type: 'look', x: freecam.x, y: freecam.y, z: freecam.z, yaw });
      try { sessionStorage.setItem('rax-eye', JSON.stringify({ room, x: freecam.x, y: freecam.y, z: freecam.z, yaw, pitch, at: Date.now() })); } catch {}
    }
  }
  const probe = connection.probe(
    performance.now(),
    ws?.readyState === WebSocket.OPEN && ws.bufferedAmount < 2048,
  );
  if (probe) send(probe);
  // 🦾 What the words-and-calls surface holds (agent.mjs): a stick, and a yaw while it walks somewhere.
  const driven = agentSurface.drive(player, performance.now(), room);
  if (Number.isFinite(driven?.yaw)) { yaw = driven.yaw; inputAt = performance.now(); }
  const walkFwd = prompt.open ? 0 : (dock ? 0 : gamepad.fwd) + pad.fwd + (driven?.fwd || 0);
  const walkRight = prompt.open ? 0 : (dock ? 0 : gamepad.right) + pad.right + (driven?.right || 0);
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
          walkFwd,
        -1,
        1,
      ),
      right: clamp(
        Number(keys.has('KeyD')) -
          Number(keys.has('KeyA')) +
          walkRight,
        -1,
        1,
      ),
      buttons: keys.has('Space') || jumpQueued || (player?.bike && gamepad.hold) ? 1 : 0,
      use: useQueued,
      drop: dropQueued,
      chalk: (() => {
        const at = chalkPoint();
        if (at) chalkLocal.push(at);
        else chalkLocal = [];
        return at;
      })(),
      fire: (firing || fireQueued) && locked(),
      bike: bikeQueued,
      duck: !!player?.bike && (keys.has('ShiftLeft') || keys.has('ShiftRight') || gamepad.backHeld),
    };
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
    jumpQueued = fireQueued = useQueued = dropQueued = bikeQueued = false;
    pending.push(c);
    // 🔫 A shot leaves the hand now, not a round trip from now (rounds.mjs).
    if (c.use && player && player.alive !== false && heldKind() === 'shoot') rounds.fire({ ...player, yaw: c.yaw, pitch: c.pitch });
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
  const worldAge = now - worldAt;
  const needZoomModels = focusZoom > 1 && !worldZoomed;
  if (!document.hidden && (needZoomModels || worldAge > 6000 || (worldAge > 700 && Math.abs(eyeHeight() - worldY) > 7))) void streamWorld();
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
  // 🚁 Someone looking on aims too (their drone's eye): to scan what stands there and lock on to it.
  const aimer = player || (seat < 0 && !held && snapshot && sheetOpen ? freecam : null), hand = seat >= 0 ? seat : -2;
  if (!aimer && !screenFingers.size) panels.point(null);
  if (aimer) {
    // The aim ray: from the camera, through the free mouse or along the
    // camera's own centre (the crosshair's normal), never from the player's
    // position with the raw yaw and pitch.
    const ray = cursorAim ? view.aimRay(mouseAt.x, mouseAt.y) : view.aimForward();
    const origin = ray ? view.aimOrigin() || aimer : aimer;
    const dir = ray || direction(yaw, pitch);
    let nearest = wallDistance(origin, dir, 60, [], 0, half());
    for (const box of objectBoxes(solids(), worldTime)) {
      const distance = rayBox(origin, dir, box, nearest);
      if (distance < nearest) {
        nearest = distance;
        focusObject = objects.find((o) => o.id === box.objectId);
      }
    }
    // (A loose item, a gun on the floor, is not aimed at: it is picked up, not focused or edited from the floor.)
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
        focusObject = wallFocus(w);
      }
    }
    // 🔭 What stands in the rooms around is aimed at too, where it is seen.
    for (const b of around()) {
      if (b.sceneryOnly) continue;
      const things = (b.objects || []).filter((o) => !o.item && o.geometry);
      if (!things.length) continue;
      const dy = b.dy || 0;
      for (const box of objectBoxes(things, worldTime)) {
        const shifted = { xMin: box.xMin + b.dx, xMax: box.xMax + b.dx, yMin: box.yMin + dy, yMax: box.yMax + dy, zMin: box.zMin + b.dz, zMax: box.zMax + b.dz };
        const distance = rayBox(origin, dir, shifted, nearest);
        if (distance >= nearest) continue;
        nearest = distance;
        const o = things.find((x) => x.id === box.objectId);
        if (!o) continue;
        // (Its whole box, for the lock's corners.)
        const whole = objectBoxes([o], worldTime).reduce((w, q) => (w ? { xMin: Math.min(w.xMin, q.xMin), xMax: Math.max(w.xMax, q.xMax), yMin: Math.min(w.yMin, q.yMin), yMax: Math.max(w.yMax, q.yMax), zMin: Math.min(w.zMin, q.zMin), zMax: Math.max(w.zMax, q.zMax) } : { ...q }), null);
        focusObject = { ...o, far: true, wall: true, box: { xMin: whole.xMin + b.dx, xMax: whole.xMax + b.dx, yMin: whole.yMin + dy, yMax: whole.yMax + dy, zMin: whole.zMin + b.dz, zMax: whole.zMax + b.dz } };
        farThings.set(o.id, focusObject);
        if (farThings.size > 64) farThings.delete(farThings.keys().next().value);
      }
    }
    // 📺 A standing KidLisp screen is aimed at too (four metres wide, facing its yaw).
    for (const s of screens) {
      const distance = rayBox(origin, dir, screenBox(s), nearest);
      if (distance < nearest) { nearest = distance; focusObject = screenFocus(s); }
    }
    // 🖼️ A world panel within reach, with nothing before it, takes the aim
    // as its mouse (panel.mjs): the primary action presses there.
    const seen = panels.aim(origin, dir, nearest), onPanel = seen && seen.t <= PANEL_REACH ? seen : null;
    // 🖥️ Gone in to one, its mouse is the one moved over its whole face (`dockLook`), however far off the eye stands.
    const inside = dock?.cursor && dock.way > 0 ? panelOf(dock.id) : null;
    if (!screenFingers.size) panels.point(inside ? { panel: inside, x: dock.cursor.x, y: dock.cursor.y, t: dock.to.back } : dock || lockOn ? null : onPanel);
    // 🧩 A piece on a panel is aimed at from any distance, as anything that stands here is.
    if (seen) { nearest = seen.t; focusObject = seen.panel.label ? panelFocus(seen.panel) : onPanel ? null : focusObject; }
    // The floor square under the crosshair, within reach, if nothing nearer.
    floorAim = null;
    if (dir.y < -0.02 && player) {
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
    if (dock || lockOn) {
      const id = (dock || lockOn).id;
      const panel = panelOf(id);
      const screen = screens.find((x) => `screen:${x.id}` === id);
      if (panel) focusObject = panelFocus(panel);
      else if (screen) focusObject = screenFocus(screen);
      else if (lockOn) focusObject = objects.find((o) => o.id === id) || farThings.get(id) || null;
      else {
        const w = walls.find((x) => `wall:${x.code}` === id);
        if (w) focusObject = wallFocus(w);
      }
    }
  }
  const displayed = presentation.sample(debt / DT, elapsed);
  // Sound the local player's motion. Docking has its own servos and the
  // unattended tour has its own flight path.
  const movingBody = seat < 0 ? freecam : player?.alive ? player : null;
  if (movingBody && connected && !down && sheetOpen && !document.hidden && document.hasFocus() &&
      !typing() && !menu && !radial.open && !dock && !nearing && !touring && !warp && !held && !joining()) {
    const flyingInput = gamepad.fwd || gamepad.right || pad.fwd || pad.right ||
      ['KeyW', 'KeyA', 'KeyS', 'KeyD'].some(key => keys.has(key));
    const speed = seat < 0 ? (flyingInput ? Math.hypot(movingBody.vx, movingBody.vy, movingBody.vz) : 0)
      : movingBody.onGround && !movingBody.swimming && !movingBody.boat ? Math.hypot(movingBody.vx, movingBody.vz) : 0;
    const prev = lastMotionLook;
    const continuous = prev && prev.room === room && prev.seat === seat && prev.life === movingBody.life && elapsed > 0 && elapsed < 0.2;
    const angle = continuous && !lockOn ? Math.hypot(((yaw - prev.yaw + 540) % 360) - 180, pitch - prev.pitch) : 0;
    const travel = clamp((speed - 0.1) / 10, 0, 1);
    const turn = clamp(angle / Math.max(elapsed, 1 / 240) / 145, 0, 1);
    sfx.motion(travel, turn, seat >= 0);
    // 🦶 A landing is heard: off the ground for a moment and falling, then on it (a step off a kerb is not one).
    const aloft = seat >= 0 && !movingBody.onGround && !movingBody.swimming && !movingBody.boat;
    if (continuous && prev.aloftAt != null && !aloft && movingBody.onGround && performance.now() - prev.aloftAt > 180 && prev.vy < -1.5) sfx.land(clamp(-prev.vy / 14, 0, 1));
    lastMotionLook = { room, seat, life: movingBody.life, yaw, pitch, vy: movingBody.vy || 0, aloftAt: aloft ? (continuous && prev.aloftAt != null ? prev.aloftAt : performance.now()) : null };
  } else if (lastMotionLook) {
    lastMotionLook = null;
    sfx.motion();
  }
  // The listener is wherever the camera is, facing where it looks.
  {
    const at = seat < 0 ? freecam : displayed || player;
    if (at) {
      const f = direction(yaw, pitch);
      const beat = beatAt(Date.now() + (clockOffset || 0));
      tones.update(toneSources(), { x: at.x, y: at.y, z: at.z, fx: f.x, fy: f.y, fz: f.z }, worldTime, beat);
    }
  }
  // (Everyone but this page's own body is carried on through a quiet wire and eased back: presentation.mjs.)
  const guessed = (playback.ahead || 0) > 0;
  const shownPlayers = connected ? Array.from({ length: snapshot?.players?.length || 0 }, (_, i) =>
    i === seat ? interpolate(history, tick, i) : remoteEase.sample(i, interpolate(history, tick, i, playback.ahead || 0), guessed, elapsed)) : [];
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
  const identity = selfMember || members.find(m => seat >= 0 ? m.seat === seat : owner && m.owner === owner);
  linkAgents(identity?.handle || (identity?.name && seat < 0 ? 'rax-drone-' + identity.name : null), now);
  const lift = liftAt(now), sky = plazaFrom();
  // 🖼️ The car's pad rides with the car; every panel's piece steps and paints.
  carPad.hidden = !hasCar() || !!game()?.ledges;
  // 🧩 The yard's pieces stand on the plaza only, and are asked for when it is first seen: by a body standing there or by a drone over it.
  const yardDistance = sky ? Math.hypot(sky.dx - (displayed || freecam).x, sky.dy - (displayed || freecam).y, sky.dz - (displayed || freecam).z) / focusZoom : Infinity;
  if (sky) void mountYard();
  for (const p of yardPanels) p.hidden = room !== 'lobby';
  hushIdleKiosks(now);
  carPad.y = CAR_PAD.y + lift;
  const orbit = orbitStep(now);
  const miniature = orbit >= 0.94;
  if (xr && sheetOpen && !down && !held && !typing() && !menu && !radial.open) {
    const body = seat >= 0 ? displayed : freecam;
    if (body) updateXRContacts(xr, { ...body, y: body.y + (seat >= 0 ? lift : 0) });
    else releaseXRContacts();
  } else if (xr) releaseXRContacts();
  if (!miniature) panels.frame(now);
  // Away from the plaza the kiosks are scenery. Their pixels and frame cache
  // survive the crossing, while their input and sound stay in the plaza.
  const sceneryPanels = [];
  if (sky && room !== 'lobby') {
    for (const p of yardPanels) {
      if (!p.frame || (!miniature && yardDistance < 90 && now >= (p.nextSceneryFrame || 0))) {
        p.step();
        p.nextSceneryFrame = now + (yardDistance < 28 ? 80 : 400);
      }
      sceneryPanels.push({ ...p, x: p.x + sky.dx, y: p.y + sky.dy, z: p.z + sky.dz, cursor: null });
    }
    if (!hasCar()) {
      if (!carPad.frame) carPad.step();
      sceneryPanels.push({ ...carPad, x: CAR_PAD.x + sky.dx, y: CAR_PAD.y + sky.dy, z: CAR_PAD.z + sky.dz, cursor: null });
    }
  }
  // 🛗 The car is heard: its hum and the shaft's wind follow how fast it goes.
  sfx.lift(ride && liftWas ? Math.min(1, Math.abs(lift - liftWas.lift) / Math.max(0.001, (now - liftWas.at) / 1000) / 14) : 0);
  liftWas = { lift, at: now };
  // 🚁 Your own drone is heard while you look on; anyone else's when it is near.
  {
    const eye = seat < 0 && !held ? freecam : displayed || player, flying = seat < 0 && !held && sheetOpen && snapshot;
    let near = flying ? 0.8 : 0, work = flying ? Math.min(1, Math.hypot(freecam.vx || 0, freecam.vy || 0, freecam.vz || 0) / 16) : 0;
    if (eye && sheetOpen) for (const d of drones.values()) {
      const close = Math.max(0, 1 - Math.hypot(d.x - eye.x, d.y - eye.y, d.z - eye.z) / 18);
      if (close > near) { near = close; work = Math.min(1, Math.hypot(d.vx || 0, d.vz || 0) / 16); }
    }
    sfx.drone(near, work);
  }
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
  if (dial.text && now - dial.at > (dial.wait || DIAL_MS) && !held) {
    if (seat >= 0 && player && hasCar() && inCar(player)) send({ type: 'elevator', to: dial.text });
    dial = { text: '', at: 0 };
  }
  const rideNow = ride && now - ride.at < ride.seconds * 1000 + 2500
    ? { ...ride, moving: rideMoving(now), floor: floorAt(altitude(ride.to) + lift + 0.5, [ride.to, ride.from]) } : null;
  // 🎬 The director's camera, when a film is being shot. A shot that `rides`
  // is in the car's frame: it goes up and down with it.
  filmPlayers = shownPlayers;
  filmRide = rideNow;
  const idleShot = seat >= 0 ? idlePlayerCamera(now, displayed || player) : (idleEntry = null);
  const droneTour = seat < 0 && touring && sheetOpen && !film && !held;
  const fadeStep = Math.min(0.1, elapsed) / (droneTour ? 4 : 0.85);
  droneIdleWeight = droneTour ? Math.min(1, droneIdleWeight + fadeStep) : Math.max(0, droneIdleWeight - fadeStep);
  const filmShot = film?.camera(now) || null;
  if (filmShot) { freecam = { ...freecam, x: filmShot.x, y: filmShot.y + (filmShot.rides ? lift : 0), z: filmShot.z }; yaw = filmShot.yaw; pitch = filmShot.pitch; }
  const blocks = around(), sceneScreens = visibleScreens(blocks);
  if (!miniature) tendScreens(now, sceneScreens);
  view.render({
    xr, headset: headset.supported, headsetControls: xr ? null : headset.controls,
    inspectScenery: query.has('debug'),
    basement: room === BASEMENT,
    half: half(),
    seam: { crossing: !!held || now - crossedAt < 400, shown: statsShown && query.get('seams') === '1' }, // 🫧 the fences on the lines between rooms are drawn (cyan, amber while between two rooms); ?seams=1 opts in
    weather,
    eyes: [...(sheetOpen && drones.size ? [...drones.values()].filter((d) => now - d.at < 1500).map((d) => droneAt(d, now)) : []),
      ...(droneView && !dock && seat < 0 && !held && !filmShot ? [{ name: '', x: freecam.x, y: freecam.y, z: freecam.z, yaw, vx: freecam.vx || 0, vz: freecam.vz || 0, roll: droneRoll }] : [])],
    place: placeOf(room), // 📍 this room's corner of the world, so where you stand is told in the world's metres
    beyond: sheetOpen && beyond.size ? [...beyond.values()].filter((b) => now - b.at < 1500).map((b) => beyondAt(b, now)) : [],
    // ⚽ A ball in the car you ride keeps to the car as this page counts it (the server's count is a moment behind).
    balls: balls.filter(b => now - b.at < 4000).map(b => {
      const at = ballAt(b, (now - b.at) / 1000), own = lift && b.lift !== undefined && Math.abs((b.g || 0) - b.lift) < 0.01;
      return { ...b, ...at, y: own ? at.y - b.lift + lift : at.y };
    }),
    courtyard: sky,
    drones: shownDrones,
    // Every seat the room holds, not only the original duel's two. Whoever
    // stands in the car while it runs rides with it.
    players: lift ? shownPlayers.map((p) => (p && inCar(p) ? { ...p, y: p.y + lift } : p)) : shownPlayers,
    me: seat >= 0 && player ? { ...displayed, y: displayed.y + lift, yaw, pitch, duck: !!player.duck, onGround: player.onGround, vy: player.vy }
      : held ? { ...held, ...(displayed || {}), y: (displayed || held).y + lift, yaw, pitch } : seat < 0 ? null : spawn(seat, 1, half()),
    debug: !!game() && debugBoxes,
    idleCamera: xr ? null : idleShot,
    idleEffect: idleShot?.weight ?? (seat < 0 ? smooth(droneIdleWeight) : 0),
    camera: xr ? (seat < 0 && !held ? { ...freecam, yaw, pitch } : null) : (game()?.camera ? gameCamera(displayed || player, shownPlayers, elapsed) : dock && !filmShot ? { ...dock.eye, face: {w: dock.face.w, h: dock.face.h}, back: dock.to.back, dockZoom: dock.to.zoom, flat: smooth(dock.t), roll: seat < 0 ? droneRoll : 0 } : seat < 0 && !held ? { ...freecam, ...(droneView && !filmShot ? droneBack() : { roll: droneRoll }), yaw, pitch, ...(filmShot ? { zoom: filmShot.zoom } : {}) } : null),
    spectator: seat < 0 && !joining(),
    gate: seat < 0 ? gate : null,
    pair: pairDesk.want(ws, seat < 0 && gate === 'account' && !held && !film && sheetOpen),
    controller: controller.id,
    stats: statsShown && sheetOpen && sheetShown, // (not over the prompt's paper)
    agents: agentLink.agents,
    // 🚁 The drone's dials, bottom centre: miles an hour, the rotors' turns a minute, feet up.
    gauge: seat < 0 && !held && sheetOpen && snapshot ? (() => {
      const v = Math.hypot(freecam.vx || 0, freecam.vy || 0, freecam.vz || 0);
      // (A hover turns them at 5,400; climbing and speed ask for more, sinking for less.)
      return { mph: v * 2.23694, rpm: Math.max(2400, 5400 + v * 190 + (freecam.vy || 0) * 260), feet: (freecam.y || 0) * 3.28084 };
    })() : null,
    sticks: null, // (the whole pad drawn top right was too much: the controls' own caps light instead, LS and RS among them)
    seat,
    third: !xr && (third || !!game()?.camera),
    game: game(),
    bops: snapshot?.bops || [],
    combos: snapshot?.combos || [],
    boomLength,
    rockets: snapshot?.rockets || [],
    items: snapshot?.items,
    bullets: [...rounds.step(now, solids(), (snapshot?.tick || 0) / HZ, half(), openEdges()),
      ...farShots.map((b) => { const t = Math.min(0.12, (now - b.at) / 1000); return { id: b.id, x: b.x + b.vx * t, y: b.y + b.vy * t, z: b.z + b.vz * t, vx: b.vx, vy: b.vy, vz: b.vz, flown: true }; })],
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
    tower: hasCar() ? { ...(tower || { here: null }), count: standing(), lift, screen: carScreen(rideNow) } : null,
    skyline: sky ? { top: towerTop, ...sky } : null,
    hung: sky ? hungFor({ i: -sky.dx / SPAN, j: -sky.dz / SPAN }, -sky.dy, Infinity) : [], // 🪐 the planets hung over home, from wherever in it this room is
    car: hasCar() && seat >= 0 && inCar(player) && !rideMoving(now),
    plainTags: !!film,
    ride: rideNow,
    radial: radial.open
      ? {
          bumper: radial.bumper, centered: radial.centered,
          items: radial.items,
          selected: radial.selected,
          title: radial.title,
          ready: !!prompt.text.trim(),
        }
      : null,
    touch: touch.state(),
    touchButtons: shownButtons().map((b) => ({ ...b, held: [...touch.pressed.values()].some((p) => p.name === b.name) })),
    input: touch.active ? 'touch' : controller.id ? 'pad' : 'keys',
    stallNote: stalls.note(),
    walls,
    cube: miniature ? [] : cube,
    menu,
    lockedOn: !!lockOn,
    lockCursor: lockOn?.cursor,
    screenCursor: dock && !dock.id.startsWith('panel:') ? dock : null,
    docked: !!dock,
    nearZoom: focusZoom,
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
    around: blocks,
    field: groundsOf(room),
    home: home && home.room === room ? home.lot : null,
    floorAim: focusObject || !sheetOpen || game()?.hud?.aim === false ? null : floorAim,
    chalk: CHALK ? chalkStrokes : [],
    chalkVersion,
    chalkLocal,
    chalkMode,
    // (Between two rooms the body is `held`: the face stays up over the hop, where it used to blink out when the car's pad was pressed.)
    face: seat >= 0 && (player || held) && game()?.hud?.face !== false ? { ...face, health: (player || held).health, alive: (player || held).alive, seat, pitch, turn: gamepad.lookX + (keys.has('KeyA') || keys.has('ArrowLeft') ? -0.5 : 0) + (keys.has('KeyD') || keys.has('ArrowRight') ? 0.5 : 0) } : null,
    devLog: sheetOpen ? devLog.filter((d) => now - d.at < 120000).map((d) => ({ ...d, age: (now - d.at) / 1000 })) : [],
    owner,
    focusObject,
    builder,
    builderNames,
    netLog,
    connected: !down, // connecting counts; a dead or long-silent socket shows the log
    offline: sheetOpen && down,
    decay: (() => { const g = decayOf(now); sfx.down(g); return Math.max(stepWarp(now), g); })(),
    chunk: chunkOf(now),
    // 📶 Not down but shoddy (late, uneven or slow word from the server): the picture tears and the line crackles.
    fuzz: (() => { const f = sheetOpen && !down && !document.hidden ? connection.shoddy(now) : 0; sfx.shoddy(f); return f; })(),
    // 📟 The server quiet for a moment (not yet an outage), or the page just back on new code.
    wire: !sheetOpen || down ? null
      : ws?.readyState === WebSocket.OPEN && connection.lastSnapshot !== null && now - connection.lastSnapshot > 400 && !held
        ? { text: `no word from the server ${((now - connection.lastSnapshot) / 1000).toFixed(1)} s`, color: 0xffe066 }
        : rebooted && now < rebooted.until ? { text: rebooted.text, color: 0x3fb950 } : null,
    cursor: cursorAim ? mouseAt : null,
    pointerSource,
    handlePoint: pointerSource === 'mouse' && !locked() ? mouseAt : null,
    reticle: !xr && hosted && sheetOpen && !down && !idleShot,
    reticleShift: reticleShift(now),
    orbit: xr ? 0 : orbit,
    cornerScale,
    // (Before the seat is given back, the flight in aims at where this body last stood here, looking where it looked: it used to aim at the room's spawn and jump to the real place, and the real facing, as it landed.)
    flyTo: seat < 0 && hosted && sheetOpen ? gate ? { ...freecam, yaw } : lastBody?.room === room ? { ...lastBody } : spawn(lastSeat >= 0 ? lastSeat : 0, 1, half()) : null,
    jobs,
    builds: sheetOpen ? buildRows() : [],
    worldTime,
    locked: locked(),
    flash: escFlash,
    legend: sheetOpen && sheetShown && seat >= 0 && legendShown(),
    corner: sheetOpen && !down && !film,
    screens: sceneScreens,
    screenPrograms,
    notepatVoices: notepatScreen.voices,
    panels: panels.list.filter((p) => !p.hidden).concat(sceneryPanels),
    bare: !!xr || !sheetOpen || !sheetShown || down || !!film || !!idleShot,
    held: {
      keys: [...keys],
      pad: [
        ...['A', 'B', 'X', 'Y', 'LB', 'RB', 'LT', 'RT', 'BACK', 'START', 'L3'].filter((_, i) => controller.previous[i]),
        ...(gamepad.fwd || gamepad.right ? ['LS'] : []),
        ...(gamepad.lookX || gamepad.lookY ? ['RS'] : []),
      ],
      touch: [...(touch.stick ? ['left'] : []), ...(touch.look ? ['right'] : [])],
    },
  });
  if (!xr) postProcess(elapsed);
  film?.frame(now);
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
    if ('away' in e.data && away !== (e.data.away === true)) {
      away = e.data.away === true;
      if (!away) {
        last = performance.now(); // No catch-up for the time spent unseen.
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
    if (!('open' in e.data)) { headset.show(sheetOpen && sheetShown); return; }
    sheetOpen = e.data.open === true;
    // Back on the curtain you are not in the room (2026-11-03): the seat is
    // given up at once, as if teleported out, and taken fresh on the next drop.
    if (!sheetOpen && ws && ws.readyState !== WebSocket.CLOSED) {
      parked = true;
      ws.left = true;
      logNet('left the room (back on the curtain)');
      ws.close(1000, 'left');
    }
    if (sheetOpen) parked = false;
    if (sheetOpen && !(seat >= 0 && player)) shownAt = performance.now();
    if (!sheetOpen) sheetShown = false;
    headset.show(sheetOpen && sheetShown);
    if (sheetOpen) closingAt = 0; // A fresh drop starts the reticle at the centre.
    if (sheetOpen && e.data.straight) orbitMix = 0; // 🔁 a page loaded again while in the world: no fly-in from the far view
    if (!sheetOpen) sheetClosedAt = performance.now();
    if (sheetOpen) {
      last = performance.now();
      debt = 0;
      // (A socket still closing from the last trip to the curtain is not the room going down: a new one at once.)
      if (!ws || ws.readyState === WebSocket.CLOSED || ws.left) connect();
      canvas.focus();
      clearTimeout(hushTimer);
      void sfx.wake().then(() => sheetOpen && sfx.enter());
      // The tap that dropped the paper is a fresh gesture: take the mouse now,
      // so no cursor flickers over the world. A refusal leaves click-to-capture.
      if (!touch.active && !headset.supported) {
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
const headset = immersive({ frame, release: releaseXRContacts, getYaw: () => yaw, transition(entering) {
  releaseInput();
  debt = 0; last = inputAt = performance.now();
  if (entering) {
    if (locked()) document.exitPointerLock();
    if (dock) comeOut();
    if (lockOn) selectTarget(null);
    closeBuild();
    if (menu) closeMenu();
    void sfx.wake();
  } else canvas.focus({ preventScroll: true });
} });
headset.show(sheetOpen && sheetShown);
