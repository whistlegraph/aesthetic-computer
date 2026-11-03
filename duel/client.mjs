import { HZ, DT, PROTOCOL, move, spawn, clamp } from './world.mjs';
import { reconcile, interpolate } from './net.mjs';
import { LocalPresentation, SnapshotClock } from './presentation.mjs';
import { Prompt } from './prompt.mjs';
import { objectBoxes } from './objects.mjs';
import { direction, rayBox, wallDistance, along } from './world.mjs';
import { createView } from './view.mjs';
import { Connection } from './connection.mjs';
import { accountToken } from './account.mjs';
import { Controller } from './controller.mjs';
import { moveFreecam } from './freecam.mjs';
import { Radial, HOLD_SECONDS, ringGeometry } from './radial.mjs';
import { TouchPad } from './touch.mjs';
import { Sfx, SpatialTones } from './sfx.mjs';
import { kidlispWall } from './kidlisp-wall.mjs';
import { sourceTones, beatAt } from './object-language.mjs';

const view = await createView(document.getElementById('game'));
const canvas = view.canvas,
  keys = new Set();
const requestedRoom = new URLSearchParams(location.search).get('room');
const room =
  requestedRoom && /^[a-z0-9-]{1,32}$/.test(requestedRoom)
    ? requestedRoom
    : 'lobby';
let ws,
  seat = -1,
  player,
  snapshot,
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
const presentation = new LocalPresentation(),
  playback = new SnapshotClock();
const prompt = new Prompt();
const connection = new Connection();
const controller = new Controller();
const radial = new Radial();
const touch = new TouchPad();
const sfx = new Sfx();
// Two walls run real KidLisp: $roz on the far wall, $bop on the left.
const walls = [];
for (const [code, face] of [['roz', 'north'], ['bop', 'west']])
  kidlispWall(code, { width: 64, height: 36 })
    .then((w) => walls.push({ ...w, code, face }))
    .catch((e) => console.info(`KidLisp wall $${code}:`, e.message));
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
let freecam = { x: 4, y: 4, z: -17, yaw: -38, pitch: -20 };
let freecamPlaced = false;
let gamepad;
let joinURL = ['localhost', '127.0.0.1', '[::1]'].includes(location.hostname)
  ? null : new URL(`?room=${room}`, location.href).href;
let members = [];
let objects = [],
  owner = null,
  builder = false,
  jobs = [],
  focusObject = null;
const locked = () => document.pointerLockElement === canvas;
const send = (data) => {
  if (
    ws?.readyState === WebSocket.OPEN &&
    ws.bufferedAmount < (data.type === 'input' ? 2048 : 32768)
  )
    ws.send(JSON.stringify(data));
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

function connect() {
  const url = new URL('./play', import.meta.url);
  url.protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
  url.search = new URLSearchParams({ room, protocol: PROTOCOL });
  history = [];
  pending = [];
  player = null;
  snapshot = null;
  seq = 0;
  eventId = 0;
  debt = 0;
  presentation.reset(null);
  playback.reset();
  notice = 'CONNECTING';
  connection.reset();
  members = [];
  freecamPlaced = false;
  ws = new WebSocket(url);
  const socket = ws;
  socket.onmessage = (e) => {
    if (socket !== ws) return;
    const data = JSON.parse(e.data);
    if (data.type === 'welcome' || data.type === 'role') {
      const old = player;
      seat = data.seat;
      if (seat < 0) {
        freecam = old
          ? { x: old.x + 2, y: old.y + 2, z: old.z - 3, yaw, pitch }
          : freecam;
        yaw = freecam.yaw;
        pitch = freecam.pitch;
        freecamPlaced = false;
      }
      player = null;
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
      if (data.joinURL) joinURL = data.joinURL;
      let token;
      try {
        token = localStorage.getItem('dool-owner');
      } catch {}
      send({ type: 'identity', token });
      void accountToken().then((accessToken) => {
        if (accessToken && ws === socket)
          send({ type: 'account', accessToken });
      });
    }
    if (data.type === 'presence') members = data.members;
    if (data.type === 'chalk' && Array.isArray(data.strokes)) chalkStrokes = data.strokes;
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
    if (data.type === 'identity') {
      owner = data.owner;
      prompt.busy = seat >= 0 && jobs.some((j) => j.owner === owner);
      try {
        localStorage.setItem('dool-owner', data.token);
      } catch {}
    }
    if (data.type === 'objects') {
      for (const job of data.jobs) sfx.buildProgress(job.id, job.preview?.boxes?.length || 0);
      objects = data.objects;
      builder = data.builder;
      jobs = data.jobs;
      prompt.busy = seat >= 0 && jobs.some((j) => j.owner === owner);
    }
    if (data.type === 'build-accepted') {
      sfx.buildStart();
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
        : data.error || `V${data.version} READY`;
      if (data.error) {
        prompt.open = true;
        releaseInput();
        document.exitPointerLock();
      } else prompt.text = '';
      canvas.setAttribute(
        'aria-label',
        data.cancelled
          ? 'Build cancelled. Enter to prompt Aesel.'
          : data.error ||
              `Object version ${data.version} ready. Enter to prompt Aesel.`,
      );
    }
    if (data.type !== 'snapshot' || (snapshot && data.tick <= snapshot.tick))
      return;
    const own = data.players[seat];
    if (seat < 0 && !freecamPlaced) {
      const target = data.players.find(Boolean);
      if (target) {
        freecam = {
          x: clamp(target.x + 4, -17, 17),
          y: target.y + 3,
          z: clamp(target.z - 5, -17, 17),
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
    if (own) {
      const changedLife = !player || own.life !== player.life;
      const changedPhase = snapshot && data.phase !== snapshot.phase;
      const result = reconcile(
        own,
        pending,
        data.acks[seat],
        solids(),
        data.tick,
      );
      pending = result.pending;
      player =
        data.phase === 'playing' || data.phase === 'waiting'
          ? result.player
          : { ...own };
      presentation.correct(player, changedLife || changedPhase);
      if (changedLife) {
        yaw = own.yaw;
        pitch = own.pitch;
      }
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
      if (event.type === 'ricochet') sfx.ricochet();
      if (event.type === 'cancel') sfx.ricochet(1.5);
      if (event.type === 'headshot') sfx.headshot();
    }
  };
  socket.onclose = (e) => {
    if (socket !== ws) return;
    // When the server goes away (a restart or an update), wait for it to come
    // back and reload, so new code lands without anyone touching the screen.
    if (snapshot) {
      const back = async () => {
        try {
          const r = await fetch(new URL('./health', location.href), { cache: 'no-store' });
          if (r.ok) return location.reload();
        } catch {}
        setTimeout(back, 1000);
      };
      setTimeout(back, 1000);
    }
    if (locked()) document.exitPointerLock();
    notice = e.reason || 'DISCONNECTED — CLICK TO REJOIN';
    prompt.busy = false;
    owner = null;
    builder = false;
    objects = [];
    jobs = [];
    members = [];
    player = null;
    snapshot = null;
    pending = [];
    history = [];
    presentation.reset(null);
    playback.reset();
  };
  socket.onerror = () => {
    notice = 'SERVER UNAVAILABLE — CLICK TO RETRY';
  };
}

canvas.addEventListener('click', capture);
canvas.addEventListener('paste', (e) => {
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
function holding() {
  return !!snapshot?.items?.some((it) => it.holder === seat && seat >= 0);
}
function submitPrompt(gesture = true) {
  if (!prompt.text.trim() || prompt.busy) return false;
  const item = radial.open ? radial.item : null;
  const chalk = radial.open && radial.chalk;
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
function openRadial() {
  if (seat < 0 || prompt.busy || radial.open) return;
  const target = focusObject?.owner === owner ? focusObject : null;
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
canvas.addEventListener('pointerdown', (e) => {
  if (!touchy(e)) return;
  e.preventDefault();
  canvas.setPointerCapture?.(e.pointerId);
  const first = !touch.active;
  touch.down(e, canvas.clientWidth, canvas.clientHeight, performance.now(), radial.open);
  if (first) void capture();
});
canvas.addEventListener('pointermove', (e) => {
  if (touchy(e)) touch.move(e, canvas.clientWidth, canvas.clientHeight);
});
canvas.addEventListener('pointerup', (e) => {
  if (touchy(e)) touch.up(e, performance.now());
});
canvas.addEventListener('pointercancel', (e) => {
  if (touchy(e)) touch.cancel(e);
});
function releaseInput() {
  keys.clear();
  firing = jumpQueued = fireQueued = false;
}
document.addEventListener('pointerlockchange', releaseInput);
document.addEventListener('mousemove', (e) => {
  if (!locked() || prompt.open) return;
  yaw = (yaw - e.movementX * 0.12 + 360) % 360;
  pitch = clamp(pitch - e.movementY * 0.12, -85, 85);
});
document.addEventListener('keydown', (e) => {
  void sfx.wake();
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
  if (!locked()) return;
  if (
    [
      'KeyW',
      'KeyA',
      'KeyS',
      'KeyD',
      'Space',
      'ShiftLeft',
      'ShiftRight',
      'KeyV',
    ].includes(e.code)
  )
    e.preventDefault();
  keys.add(e.code);
  if (seat >= 0 && e.code === 'Space' && !e.repeat) {
    jumpQueued = true;
    sfx.jump();
  }
  if (seat >= 0 && e.code === 'KeyV' && !e.repeat) third = !third;
  if (seat >= 0 && e.code === 'KeyF' && !e.repeat) useQueued = true;
  if (seat >= 0 && e.code === 'KeyQ' && !e.repeat) dropQueued = true;
});
document.addEventListener('keyup', (e) => keys.delete(e.code));
document.addEventListener('mousedown', (e) => {
  if (seat >= 0 && locked() && e.button === 0 && holding()) {
    e.preventDefault();
    useQueued = true;
    return;
  }
  if (seat >= 0 && locked() && e.button === 0 && !prompt.busy) {
    e.preventDefault();
    releaseInput();
    document.exitPointerLock();
    prompt.begin(focusObject?.owner === owner ? focusObject : null);
  }
});
document.addEventListener('mouseup', (e) => {
  firing = false;
  if (e.button === 2) chalkMouse = false;
});
document.addEventListener('mousedown', (e) => {
  if (e.button === 2 && locked()) chalkMouse = true;
});
canvas.addEventListener('contextmenu', (e) => e.preventDefault());
window.addEventListener('blur', releaseInput);
document.addEventListener('visibilitychange', () => {
  releaseInput();
  debt = 0;
  last = performance.now();
  presentation.reset(player);
});

function frame(now) {
  // Schedule first: one bad frame must never stop the loop.
  requestAnimationFrame(frame);
  try {
    step(now);
  } catch (error) {
    console.error('dool frame', error);
  }
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
  gamepad = controller.sample(pads, document.hasFocus());
  if (gamepad.ring || gamepad.select || gamepad.jump || gamepad.prompt || gamepad.cancel || gamepad.start) void sfx.wake();
  fpsFrames++;
  if (now - fpsAt >= 500) {
    fps = Math.round((fpsFrames * 1000) / (now - fpsAt));
    fpsFrames = 0;
    fpsAt = now;
  }
  if (gamepad.start) {
    if (window.doolRig) window.doolRig.toggleFullscreen();
    else canvas.requestFullscreen?.().catch(() => {});
  }
  if (gamepad.swap) window.doolRig?.focusOther();
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
  // LB selects what you aim at for building and opens the ring.
  if (gamepad.ring && seat >= 0 && !prompt.open) openRadial();
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
    if (seat >= 0 && prompt.busy) send({ type: 'cancel-build' });
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
  if (gamepad.use && seat >= 0 && !prompt.open) useQueued = true;
  if (gamepad.prompt && seat >= 0 && !prompt.open && holding()) {
    dropQueued = true;
    gamepad.prompt = false;
  }
  if (gamepad.prompt && seat >= 0 && !prompt.open && !prompt.busy) {
    releaseInput();
    document.exitPointerLock();
    canvas.focus();
    prompt.begin(focusObject?.owner === owner ? focusObject : null);
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
    if (seat >= 0 && gamepad.jump) {
      jumpQueued = true;
      sfx.jump();
    }
    if (seat >= 0 && gamepad.camera) third = !third;
  }
  if (elapsed > 0.25) presentation.reset(player);
  debt += Math.min(0.1, elapsed);
  last = now;
  const connected = ws?.readyState === WebSocket.OPEN && snapshot;
  if (seat < 0 && connected)
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
    now,
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
      chalk: chalkPoint(),
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
    send({ type: 'input', commands: pending.slice(-6) });
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
              : !locked() && !touch.active && !controller.id
                ? 'CLICK TO PLAY'
                : snapshot.phase === 'waiting'
                  ? 'WAITING FOR OPPONENT'
                  : '');
  // Animate objects on the smoothed playback clock, not the raw snapshot tick:
  // snapshots land unevenly, and snapping to each one made motion stutter.
  const tick = playback.sample(now);
  const worldTime = tick / HZ;
  focusObject = null;
  if (player) {
    const dir = direction(yaw, pitch);
    let nearest = wallDistance(player, dir, 30);
    for (const box of objectBoxes(solids(), worldTime)) {
      const distance = rayBox(player, dir, box, nearest);
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
      const distance = it.holder === seat ? 0.5 : rayBox(player, dir, box, nearest);
      if (distance < nearest && it.holder !== seat) {
        nearest = distance;
        focusObject = objects.find((o) => o.id === it.id);
      }
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
    players: connected ? [0, 1].map((i) => interpolate(history, tick, i)) : [],
    me: seat < 0 ? null : player ? { ...displayed, yaw, pitch } : spawn(seat),
    camera: seat < 0 ? { ...freecam, yaw, pitch } : null,
    spectator: seat < 0,
    controller: controller.id,
    seat,
    third,
    rockets: snapshot?.rockets || [],
    items: snapshot?.items || [],
    bullets: snapshot?.bullets || [],
    since: (now - latestAt) / 1000,
    holding: seat >= 0 ? objects.find((o) => snapshot?.items?.some((it) => it.id === o.id && it.holder === seat)) || null : null,
    status,
    members,
    joinURL,
    connection: connection.state(
      now,
      ws?.readyState === WebSocket.OPEN,
      ws?.bufferedAmount || 0,
    ),
    objects,
    prompt,
    radial: radial.open
      ? {
          items: radial.items,
          selected: radial.selected,
          title: radial.title,
          ready: !!prompt.text.trim(),
        }
      : null,
    touch: touch.state(),
    input: touch.active ? 'touch' : controller.id ? 'pad' : 'keys',
    fps,
    walls,
    chalk: chalkStrokes,
    chalkMode,
    face: seat >= 0 && player ? { ...face, health: player.health, alive: player.alive, seat, turn: gamepad.lookX + (keys.has('KeyA') ? -0.5 : 0) + (keys.has('KeyD') ? 0.5 : 0) } : null,
    devLog: devLog.filter((d) => now - d.at < 120000).map((d) => ({ ...d, age: (now - d.at) / 1000 })),
    owner,
    focusObject,
    builder,
    jobs,
    worldTime,
    locked: locked(),
  });
}
connect();
requestAnimationFrame(frame);
