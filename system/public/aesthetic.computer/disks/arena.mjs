// arena, 2025.4.7
// Quake-style arena with large tessellated ground, player shadow, and speedometer.

/* #region 🏁 TODO
  + Done
  - [x] Fork from fps.mjs
  - [x] Large pre-tessellated ground plane
  - [x] Speed meter HUD + FPS counter
  - [x] Gravity + space-to-jump + shift-to-crouch (Quake-style)
  - [x] Multiuser: WS+UDP wiring to session-server/arena-manager.mjs
  - [x] Walls + pillars (solid meshes, shared client/server collision)
  - [x] Strafe-jumping / bunny-hop acceleration (Q3 friction + air-accel)
  - [x] Solid humanoid meshes for remote players
#endregion */

// ---------------------------------------------------------------------------
// 🏟️ Multiuser networking (Q3-style: server-authoritative, UDP cmds + snaps).
// See plans/arena-multiplayer.md §5 for the full design.
// ---------------------------------------------------------------------------

import { BTN, packCmd, pmove } from "../lib/pmove.mjs";
import { ARENA_OBSTACLES, ARENA_OBSTACLE_COLORS, ARENA_PHYSICS } from "../lib/arena-world.mjs";

let myHandle = "guest";
let netServer = null;       // WebSocket (reliable)
let netUdp = null;          // geckos.io channel (unreliable, low-latency)
let netSendFn = null;
let netConnectedAt = 0;

let nextCmdSeq = 0;         // monotonic per-cmd seq (implicit on wire via firstSeq)
let lastSnapAck = 0;        // highest server messageNum we've seen
let serverClockOffset = 0;  // add to Date.now() → server time estimate
let clockInit = false;      // first sample seeds the offset directly
const clockSamples = [];    // [{ at, off }] — sliding window for max-filter
let lastRenderTime = -Infinity; // monotonic floor for renderTimeNow()
let lastPingSent = 0;
let ping = 0;
let netSpectator = false;       // true if another tab took over this handle
let netTakeoverAt = 0;
// Free-fly spectator cam (Quake noclip). Initialised on entering spectator.
let specPos = null;             // { x, y, z } in world coords
const SPEC_SPEED = 20;          // units/sec (faster than runSpeed; no collision)
const SPEC_FAST_MUL = 3;        // when no other modifier: ctrl / etc

const CMD_RATE = 60;        // cmd sends per sec
const CMD_BACKUP = 3;       // how many past cmds to include in each packet
const SNAP_INTERP_MS = 100; // render remotes this far in the past

// 🕰️ Clock estimation. Each snap yields one raw offset sample = serverMs −
// localElapsed. A snap delayed by one-way latency `d` reads `O − d` (O =
// true offset), so the *largest* sample over a recent window is the
// least-delayed packet — the best estimate of O. We max-filter over a
// window, then slew the committed offset toward it (never jump). Before
// this, the offset was overwritten raw every snap, so render time bounced
// with UDP jitter and remotes stuttered even with a full buffer.
const CLOCK_WINDOW_MS = 4000;  // max-filter horizon
const CLOCK_SLEW_UP = 16;      // ms/snap when correcting forward (t-safe)
const CLOCK_SLEW_DOWN = 4;     // ms/snap when easing back (could pull t back)
// 🛰️ Extrapolation horizon for starved remote buffers (lost/late snaps).
// Coast a remote along its last velocity for up to this long instead of
// freezing on the last sample then snapping when packets resume.
const EXTRAP_MAX_MS = 250;
const pendingCmds = [];     // unacked cmds [{ seq, cmd }], oldest first
const cmdOutbox = [];       // last CMD_BACKUP cmds only (wire-level backup window)

// Soft reconciliation tuning.
// cam-doll (local) and pmove (server) use slightly different integrators,
// so they diverge by a small amount during sustained motion. Keep the dead
// zone wide enough to ignore that physics-model jitter — otherwise the
// 30Hz snap loop yanks the cam every frame and forward movement feels choppy.
// Reconcile tuning — informed by arena-probe.mjs runs. A 5-unit prediction
// drift (from a ~250ms network stall) previously triggered a 1-frame hard
// snap that felt like a teleport. Raise the snap threshold and quadruple
// the soft-K so 2–4 unit drifts catch up in ~5 frames instead of 20+ or a
// visible teleport.
const RECONCILE_SNAP_THRESHOLD = 4.0;   // > this = hard snap (true teleport)
const RECONCILE_SOFT_K = 0.18;          // ~90% convergence over ~12 frames
const RECONCILE_DEAD_ZONE = 0.5;        // < this = ignore (model-jitter noise)

// Arena world cfg — MUST match ARENA_CFG in session-server/arena-manager.mjs.
// Duplicated (not imported) because lib code shouldn't depend on server code.
const ARENA_CFG = Object.freeze({
  runSpeed: 10, walkSpeed: 5, jumpVelocity: 8, gravity: 50,
  groundY: -1.5, eyeHeight: 2.0, crouchEyeHeight: 1.2, crouchLerp: 0.25,
  groundBounds: { xMin: -14, xMax: 14, zMin: -14, zMax: 14 },
  deathFloorY: -30, deathFloorClearance: 0.3,
  simHz: 60, hVelDecay: 0.9,
  // 🏃 Q3 air-physics (mirrors server) — enables strafe-jump / bunny-hop.
  airAccel: ARENA_PHYSICS.airAccel,
  groundAccel: ARENA_PHYSICS.groundAccel,
  airCapSpeed: ARENA_PHYSICS.airCapSpeed,
  groundFriction: ARENA_PHYSICS.groundFriction,
  // 🧱 Static walls + pillars (shared geometry).
  obstacles: ARENA_OBSTACLES,
  playerRadius: ARENA_PHYSICS.playerRadius,
});

// Remote players (everyone except me)
const others = {};          // handle -> { buffer: [{serverMs,x,y,z,yaw,...}], bodyFeet, bodyArms }

// M9: delta-snapshot bases. messageNum -> players[] (as reconstructed).
const snapBases = new Map();
const SNAP_BASE_RING = 32;
function rememberBase(messageNum, playersArr) {
  snapBases.set(messageNum, playersArr);
  // Prune oldest when over ring budget.
  if (snapBases.size > SNAP_BASE_RING) {
    const oldest = Math.min(...snapBases.keys());
    snapBases.delete(oldest);
  }
}
function applyDelta(base, delta, removed) {
  // Start from a copy of base by handle, then overlay delta entries.
  const byH = new Map();
  for (const p of base) byH.set(p.h, { ...p });
  for (const d of delta) {
    if (d.__new) { byH.set(d.h, { ...d.__new }); continue; }
    const keys = Object.keys(d);
    if (keys.length === 1) continue; // { h } only → unchanged
    const cur = byH.get(d.h);
    if (!cur) { byH.set(d.h, { ...d }); continue; }
    for (const k of keys) if (k !== "h") cur[k] = d[k];
  }
  if (removed?.length) for (const h of removed) byH.delete(h);
  return [...byH.values()];
}

// My own server-authoritative state (from snaps) — used for soft correction.
let myServerState = null;
let myServerStateMs = 0;
let myServerAckCmdMs = 0;
// Cam reference captured in netSim; used by reconciler on snap arrival.
let reconCamRef = null;
let reconCorrectionMs = 0; // monotonic debug counter for HUD

// 🔬 Perf ring — arena.mjs runs inside the disk.mjs Web Worker, so we cannot
// touch `window` directly. Instead we batch-send the ring to bios.mjs every
// few frames; bios mirrors it onto window.__arena_perfStats for external
// probes (artery/arena-probe.mjs).
const ARENA_PERF_RING_SIZE = 240;         // ~4s @ 60fps
const ARENA_PERF_SEND_EVERY = 15;         // batch every ~250ms @ 60fps
const arenaPerfRing = new Array(ARENA_PERF_RING_SIZE);
let arenaPerfRingIdx = 0;
let arenaPerfRingLen = 0;
let arenaPerfPaintsSinceSend = 0;
let arenaPerfSend = null;                 // captured from boot's `send` arg
let lastReconDist = 0;                    // |predicted − local| from last reconcile
let lastReconKind = "none";               // "none" | "soft" | "snap" | "skip"

// Tunables exposed on screen.
let netStats = {
  snapsRx: 0,
  cmdsTx: 0,
  lastSnapMs: 0,
  lastCmdMs: 0,
};

// Key input state captured for usercmd composition. Hooked into the
// existing keyboardState that arena already tracks for button highlights.
const netInput = {
  fwd: 0, right: 0,
  jumping: false, crouching: false,
};

function currentButtons() {
  let b = 0;
  if (netInput.jumping)  b |= BTN.JUMP;
  if (netInput.crouching) b |= BTN.CROUCH;
  return b;
}

function enqueueCmd(cam) {
  if (!cam) return;
  const ms = Date.now() - netConnectedAt;
  const cmd = packCmd({
    ms,
    fwd: netInput.fwd,
    right: netInput.right,
    yaw: cam.rotY,
    pitch: cam.rotX,
    buttons: currentButtons(),
  });
  const seq = ++nextCmdSeq;
  pendingCmds.push({ seq, cmd });
  cmdOutbox.push({ seq, cmd });
  while (cmdOutbox.length > CMD_BACKUP) cmdOutbox.shift();
  // Cap pending queue defensively (at 60Hz cmd rate + 1s RTT ceiling ≈ 60).
  while (pendingCmds.length > 120) pendingCmds.shift();
}

function flushCmds() {
  if (cmdOutbox.length === 0) return;
  const frame = {
    handle: myHandle,
    firstSeq: cmdOutbox[0].seq,
    ack: lastSnapAck,
    cmds: cmdOutbox.map((e) => e.cmd),
  };
  if (netUdp?.connected) {
    netUdp.send("arena:cmd", frame);
    // 🚑 UDP can die silently (channel still reports connected). If snaps
    // have gone quiet while the WS is alive, mirror cmds over WS too so the
    // server keeps seeing our acks and its stall watchdog can demote us to
    // WS snapshots instead of letting remotes freeze.
    const snapAge = netStats.lastSnapMs ? Date.now() - netStats.lastSnapMs : 0;
    if (snapAge > 1500) netServer?.send("arena:cmd", frame);
  } else {
    netServer?.send("arena:cmd", frame);
  }
  netStats.cmdsTx++;
  netStats.lastCmdMs = Date.now();
}

// 🐛 mp-debug — log first snap + roster changes so we can see what's
// actually arriving on the wire. Drop once mutual-visibility is solid.
let _mpFirstSnap = false;
let _mpLastRoster = "";
function _mpDebug(snap, blobs) {
  if (!_mpFirstSnap) {
    _mpFirstSnap = true;
    console.log(
      `🏟️  first snap: msg=${snap.messageNum} you=${snap.you} delta=${snap.deltaNum} players=[${blobs.map((p) => p.h).join(",")}]`,
    );
  }
  const roster = Object.keys(others).sort().join(",");
  if (roster !== _mpLastRoster) {
    _mpLastRoster = roster;
    console.log(`🏟️  others roster (me=${myHandle}): [${roster || "—"}]`);
  }
}

function onSnap(snap) {
  netStats.snapsRx++;
  netStats.lastSnapMs = Date.now();
  if (snap.messageNum > lastSnapAck) lastSnapAck = snap.messageNum;

  // Clock sync: max-filter the raw offset over a sliding window, then slew
  // the committed offset toward it. renderTimeNow() additionally clamps to
  // a monotonic floor, so the interpolation clock can never run backward
  // even if a faster path briefly lowers the estimate.
  const localMs = Date.now();
  const rawOff = snap.serverMs - (localMs - netConnectedAt);
  clockSamples.push({ at: localMs, off: rawOff });
  while (clockSamples.length && localMs - clockSamples[0].at > CLOCK_WINDOW_MS) {
    clockSamples.shift();
  }
  let targetOff = clockSamples[0].off;
  for (let i = 1; i < clockSamples.length; i++) {
    if (clockSamples[i].off > targetOff) targetOff = clockSamples[i].off;
  }
  if (!clockInit) {
    serverClockOffset = targetOff;
    clockInit = true;
  } else {
    const dOff = targetOff - serverClockOffset;
    serverClockOffset += dOff > 0
      ? Math.min(dOff, CLOCK_SLEW_UP)
      : Math.max(dOff, -CLOCK_SLEW_DOWN);
  }

  // M10: drop cmds the server has acked (seq-based; firstSeq implicit).
  if (typeof snap.ackCmdSeq === "number") {
    while (pendingCmds.length && pendingCmds[0].seq <= snap.ackCmdSeq) {
      pendingCmds.shift();
    }
  }

  // M9: reconstruct full player list from either full or delta snap.
  let blobs;
  if (snap.deltaNum && snap.delta) {
    const base = snapBases.get(snap.deltaNum);
    if (!base) {
      // Base expired / never saw it. Server will send a full snap on the
      // next tick because our ack will re-anchor. Skip this one.
      return;
    }
    blobs = applyDelta(base, snap.delta, snap.removed);
  } else {
    blobs = snap.players || [];
  }
  // Commit as a base for future delta decoding.
  if (typeof snap.messageNum === "number" && snap.messageNum > 0) {
    rememberBase(snap.messageNum, blobs);
  }
  const seen = new Set();
  for (const p of blobs) {
    const isMe = p.h === myHandle;
    if (isMe) {
      myServerState = p;
      myServerStateMs = snap.serverMs;
      myServerAckCmdMs = typeof snap.ackCmdMs === "number" ? snap.ackCmdMs : myServerAckCmdMs;
      // While spectating, our "me" entry is being driven by another tab —
      // render it as just another remote stick figure so we can watch.
      // Otherwise skip (cam-doll is already drawing us locally).
      if (!netSpectator) continue;
    }
    seen.add(p.h);
    let o = others[p.h];
    if (!o) {
      o = others[p.h] = { buffer: [], bodyFeet: null, bodyArms: null, lastSeenMs: snap.serverMs };
    }
    o.lastSeenMs = snap.serverMs;
    // Append to interpolation buffer (keep ~500ms of history).
    o.buffer.push({
      ms: snap.serverMs,
      x: p.x, y: p.y, z: p.z,
      yaw: p.yaw, pitch: p.pitch,
      crouchT: p.c,
      onGround: !!p.g,
      alive: !!p.a,
    });
    while (o.buffer.length > 32) o.buffer.shift();
  }
  // If we just left spectator mode, drop the self entry we had been tracking.
  if (!netSpectator && others[myHandle]) delete others[myHandle];
  // Prune others not in this snap for >2s (graceful drop).
  for (const h of Object.keys(others)) {
    if (seen.has(h)) continue;
    if (snap.serverMs - others[h].lastSeenMs > 2000) delete others[h];
  }

  _mpDebug(snap, blobs);

  // M7: client-side prediction reconciliation.
  reconcileLocal();
}

// Starting from the server's authoritative state for me, replay every
// still-unacked cmd → this is where the server WILL arrive once the rest
// of our in-flight cmds reach it. Compare to cam-doll's current local
// position; if divergent, soft-correct (small drift) or snap (big desync).
function reconcileLocal() {
  if (!myServerState || !reconCamRef) return;
  // While spectating, the "me" state in snapshots is being driven by another
  // tab — if we reconciled cam-doll against it, our cam would keep getting
  // yanked to wherever they are. So just skip local pmove reconciliation
  // (we still update cam to follow them below if we want spectator-follow).
  if (netSpectator) return;
  // 🎆 Knockback grace: a grenade explosion just punted us via applyImpulse.
  // The server doesn't know about that impulse (it sees only WASD cmds), so
  // its predicted state will diverge by several units. Skip reconciliation
  // briefly so the blast actually launches us instead of being snapped back.
  if (Date.now() < knockbackGraceUntil) return;
  const cam = reconCamRef;

  // Build a pmove-compatible state from the wire blob.
  let predicted = {
    x: myServerState.x, y: myServerState.y, z: myServerState.z,
    vx: myServerState.vx || 0, vy: myServerState.vy || 0, vz: myServerState.vz || 0,
    yaw: myServerState.yaw || 0, pitch: myServerState.pitch || 0,
    crouchT: myServerState.c || 0,
    onGround: !!myServerState.g,
    frozen: false,
    alive: !!myServerState.a,
  };

  // Replay each pending cmd in order, using its ms delta for dt. The "base
  // ms" for the first pending cmd is the server's last-applied cmd ms.
  let prevMs = myServerAckCmdMs;
  for (const { cmd } of pendingCmds) {
    const dt = prevMs > 0 ? Math.min((cmd.ms - prevMs) / 1000, 0.25) : 1 / 60;
    predicted = pmove(predicted, { ...cmd, dt }, ARENA_CFG);
    prevMs = cmd.ms;
  }

  // Compare cam-doll's current world position to predicted.
  //   cam.x/y/z store negated world coords (see cam-doll.mjs).
  const localX = -cam.x, localY = -cam.y, localZ = -cam.z;
  const dx = predicted.x - localX;
  const dy = predicted.y - localY;
  const dz = predicted.z - localZ;
  const dist = Math.sqrt(dx * dx + dy * dy + dz * dz);

  lastReconDist = dist;

  if (dist < RECONCILE_DEAD_ZONE) {
    lastReconKind = "skip";
    return;
  }

  if (dist > RECONCILE_SNAP_THRESHOLD) {
    // Large desync (teleport / forced respawn / long stall) — hard snap.
    cam.x = -predicted.x;
    cam.y = -predicted.y;
    cam.z = -predicted.z;
    reconCorrectionMs++;
    lastReconKind = "snap";
    return;
  }

  // Small drift — blend cam toward predicted over ~5 frames.
  cam.x += -dx * RECONCILE_SOFT_K;
  cam.y += -dy * RECONCILE_SOFT_K;
  cam.z += -dz * RECONCILE_SOFT_K;
  reconCorrectionMs++;
  lastReconKind = "soft";
}

function netBoot({ net, handle, send, debug }) {
  netSendFn = send;
  myHandle = handle?.() || "guest_" + Math.floor(Math.random() * 9999);
  netConnectedAt = Date.now();

  // 🔄 Live-reload on new deploy — DEV ONLY.
  // The `while (true)` poll loop is detached from piece lifecycle, so in
  // production it persists after navigating away (e.g. into chat) and
  // would force a reload there too. Gate behind `debug` so live-reload
  // only fires on local dev checkouts.
  if (debug) {
    (async () => {
      try {
        const res = await fetch("/api/version");
        if (!res.ok) return;
        const info = await res.json();
        const current = info.deployed;
        while (true) {
          try {
            const r = await fetch(`/api/version?current=${current}`);
            if (!r.ok) break;
            const data = await r.json();
            if (data.changed !== false) { netSendFn?.({ type: "window:reload" }); break; }
          } catch { break; }
        }
      } catch {}
    })();
  }

  if (!net) return;

  const { socket, udp } = net;

  netUdp = udp?.((type, content) => {
    if (type !== "arena:snap") return;
    const s = typeof content === "string" ? JSON.parse(content) : content;
    onSnap(s);
  });

  netServer = socket?.((id, type, content) => {
    if (type.startsWith("connected")) {
      netServer.send("arena:hello", { handle: myHandle });
      return;
    }
    const msg = typeof content === "string" ? JSON.parse(content) : content;
    if (type === "arena:welcome") {
      const roster = msg.roster || [];
      const peers = roster.filter((h) => h !== msg.you);
      console.log(
        peers.length
          ? `🏟️  welcome → ${msg.you}; peers: ${peers.join(", ")}`
          : `🏟️  welcome → ${msg.you} (arena empty)`,
      );
      return;
    }
    if (type === "arena:snap") { onSnap(msg); return; }          // WS fallback
    if (type === "arena:join") {
      if (msg.handle !== myHandle && !others[msg.handle]) {
        others[msg.handle] = { buffer: [], bodyFeet: null, bodyArms: null, lastSeenMs: Date.now() };
      }
      return;
    }
    if (type === "arena:leave") { delete others[msg.handle]; return; }
    if (type === "arena:pong") { ping = Date.now() - msg.ts; return; }
    if (type === "arena:takeover") {
      // Another tab under the same handle displaced us. Flip to spectator
      // mode: stop sending cmds, keep receiving snaps, let the user watch
      // the other tab drive their avatar.
      netSpectator = true;
      netTakeoverAt = Date.now();
      console.log(`🪑 takeover: ${msg.handle} is now controlled from another tab — spectating.`);
      return;
    }
  });
}

function netSim(cam) {
  reconCamRef = cam; // kept across frames so reconcileLocal can correct.

  // 🪑 Spectator free-fly camera (no gravity, no collision, no avatar).
  // cam-doll has already run its physics for this frame; we overwrite
  // cam.x/y/z and let cam-doll keep handling rotation (mouselook, etc).
  if (netSpectator) {
    if (!specPos) {
      // Seed from the current cam position, lifted a bit so we start with a
      // nice angle on the arena instead of clipping into the ground.
      specPos = { x: -cam.x, y: -cam.y + 5, z: -cam.z };
    }
    const dt = 1 / SIM_HZ;
    // Read the same keyboard + gamepad state arena already tracks.
    const fwd = (keyboardState.w || keyboardState.arrowup)    ?  1
              : (keyboardState.s || keyboardState.arrowdown) ? -1 : 0;
    const right = (keyboardState.d || keyboardState.arrowright) ?  1
                 : (keyboardState.a || keyboardState.arrowleft)  ? -1 : 0;
    const up = (keyboardState.space ? 1 : 0) - (keyboardState.shift ? 1 : 0);
    // Direction vectors from cam rotation.
    const yr = cam.rotY * Math.PI / 180;
    const pr = cam.rotX * Math.PI / 180;
    const cy = Math.cos(yr), sy = Math.sin(yr);
    const cp = Math.cos(pr);
    // Forward in world: include pitch so look-down-and-W flies you down.
    const fx = sy * cp, fy = Math.sin(pr), fz = cy * cp;
    const rx = cy,      rz = -sy;  // strafe right (horizontal only)
    const speed = SPEC_SPEED;
    specPos.x += (fwd * fx + right * rx) * speed * dt;
    specPos.y += (fwd * fy + up        ) * speed * dt;
    specPos.z += (fwd * fz + right * rz) * speed * dt;
    // Commit to cam (stored as negated world coords).
    cam.x = -specPos.x;
    cam.y = -specPos.y;
    cam.z = -specPos.z;
    // Skip the usercmd path entirely — spectator sends nothing.
    // Still ping so the HUD latency value stays live.
    if (Date.now() - lastPingSent > 2000) {
      lastPingSent = Date.now();
      netServer?.send("arena:ping", { handle: myHandle, ts: Date.now() });
    }
    return;
  }
  specPos = null; // reset when re-entering spectator later.

  // Poll input state → usercmd each sim tick (120 Hz). Send at CMD_RATE.
  // Mirrors EVERY input source that drives cam-doll: keyboard, gamepad, AND
  // the on-screen mobile buttons. Mobile buttons used to feed only cam-doll
  // (setMovement) — the server never heard them, so reconciliation dragged
  // touch players back toward their last keyboard position. Rubber-banding.
  // pmove convention: fwd=+1 moves along facing (forward), right=+1 strafes right.
  netInput.fwd   = (keyboardState.w || keyboardState.arrowup || mobileButtonStates.up)    ?  1
                  : (keyboardState.s || keyboardState.arrowdown || mobileButtonStates.down) ? -1 : 0;
  netInput.right = (keyboardState.d || keyboardState.arrowright || mobileButtonStates.right) ?  1
                  : (keyboardState.a || keyboardState.arrowleft || mobileButtonStates.left) ? -1 : 0;
  // Gamepad left-stick: stick-up (gy<0) is forward, stick-right (gx>0) is strafe right.
  if (gamepadState.connected) {
    const gx = gamepadState.axes[0] || 0, gy = gamepadState.axes[1] || 0;
    if (Math.abs(gx) > 0.3) netInput.right = gx > 0 ?  1 : -1;
    if (Math.abs(gy) > 0.3) netInput.fwd   = gy > 0 ? -1 :  1;
  }
  netInput.jumping  = !!keyboardState.space || !!gamepadState.buttons?.[0] || !!mobileButtonStates.jump;
  netInput.crouching = !!keyboardState.shift || !!gamepadState.buttons?.[1] || !!mobileButtonStates.crouch;

  // Non-spectator: produce + send usercmds.
  enqueueCmd(cam);
  if (!netSim._acc) netSim._acc = 0;
  netSim._acc++;
  if (netSim._acc >= 120 / CMD_RATE) { netSim._acc = 0; flushCmds(); }

  // Periodic ping for latency HUD.
  if (Date.now() - lastPingSent > 2000) {
    lastPingSent = Date.now();
    netServer?.send("arena:ping", { handle: myHandle, ts: Date.now() });
  }
}

function renderTimeNow() {
  let t = (Date.now() - netConnectedAt) + serverClockOffset - SNAP_INTERP_MS;
  // Never let the interp clock step backward (slew bias + Date.now NTP
  // hiccups). Multiple calls in one frame return the same monotone value,
  // so the world and minimap stay in agreement.
  if (t < lastRenderTime) return lastRenderTime;
  lastRenderTime = t;
  return t;
}

function sampleOther(o, t) {
  const buf = o.buffer;
  if (buf.length === 0) return null;
  if (t <= buf[0].ms) return buf[0];
  const last = buf[buf.length - 1];
  if (t >= last.ms) {
    // Buffer starved (lost / late snaps). Coast along the last observed
    // velocity for up to EXTRAP_MAX_MS instead of freezing then snapping.
    // If the remote was stationary, last === prev ⇒ zero drift (no idle
    // jitter); only actually-moving remotes get extrapolated.
    if (buf.length < 2) return last;
    const prev = buf[buf.length - 2];
    const span = Math.max(1, last.ms - prev.ms);
    const over = Math.min(t - last.ms, EXTRAP_MAX_MS);
    const k = over / span; // 0..(EXTRAP_MAX_MS/span) past `last`
    return {
      x: last.x + (last.x - prev.x) * k,
      y: last.y + (last.y - prev.y) * k,
      z: last.z + (last.z - prev.z) * k,
      yaw: lerpAngle(prev.yaw, last.yaw, 1 + k), // keep turning past last
      pitch: last.pitch + (last.pitch - prev.pitch) * k,
      crouchT: last.crouchT, // don't extrapolate stance
      onGround: last.onGround,
      alive: last.alive,
    };
  }
  // Find bracketing entries.
  for (let i = 0; i < buf.length - 1; i++) {
    const a = buf[i], b = buf[i + 1];
    if (t >= a.ms && t <= b.ms) {
      const k = (t - a.ms) / Math.max(1, b.ms - a.ms);
      return {
        x: a.x + (b.x - a.x) * k,
        y: a.y + (b.y - a.y) * k,
        z: a.z + (b.z - a.z) * k,
        yaw: lerpAngle(a.yaw, b.yaw, k),
        pitch: a.pitch + (b.pitch - a.pitch) * k,
        crouchT: a.crouchT + (b.crouchT - a.crouchT) * k,
        onGround: b.onGround,
        alive: b.alive,
      };
    }
  }
  return buf[buf.length - 1];
}

function lerpAngle(a, b, k) {
  let d = ((b - a + 540) % 360) - 180; // shortest arc
  return a + d * k;
}

// Anonymous tabs (guest_xxxx or swarm_xxx from the load-test CLI) get a
// "watcher" treatment — gray, translucent, tiny on the minimap — so the
// visual prominence in the arena is reserved for people who claimed a handle.
function isGuestHandle(h) {
  return typeof h === "string" && (h.startsWith("guest_") || h.startsWith("swarm_"));
}

// Build a solid boxy humanoid mesh (head, torso, arms, legs) as a single
// triangle Form so remote players read as bodies, not line drawings. The
// origin sits at eye height so paintRemotes can place body.position at the
// remote's eye coords without offset gymnastics. Face-side (+Z in body-local)
// gets a darker visor tone so you can tell which way the player is looking.
function buildRemoteBody(Form, colorRGB, watcher = false) {
  const [R, G, B] = colorRGB;
  const a = watcher ? 0.55 : 1.0;
  const r = R / 255, g = G / 255, b = B / 255;
  // Tone tiers from the deterministic per-handle hue.
  const main = [r, g, b, a];
  const dark = [r * 0.55, g * 0.55, b * 0.55, a];
  const light = [Math.min(1, r * 1.18), Math.min(1, g * 1.18), Math.min(1, b * 1.18), a];
  const skinHead = [
    Math.min(1, r * 0.4 + 0.5),
    Math.min(1, g * 0.4 + 0.45),
    Math.min(1, b * 0.4 + 0.4),
    a,
  ];
  const visor = watcher
    ? [0.55, 0.6, 0.7, a]
    : [0.08, 0.08, 0.12, a];

  const positions = [];
  const colors = [];
  // Push one axis-aligned box (in body-local coords). `faces` can override
  // each face individually; missing faces fall back to `main`.
  const pushBox = (xMin, yMin, zMin, xMax, yMax, zMax, faces) => {
    const top = faces.top ?? faces.main;
    const bot = faces.bot ?? faces.main;
    const north = faces.north ?? faces.side ?? faces.main; // -Z
    const south = faces.south ?? faces.side ?? faces.main; // +Z (face/forward)
    const east  = faces.east  ?? faces.side ?? faces.main; // +X
    const west  = faces.west  ?? faces.side ?? faces.main; // -X
    const v = (x, y, z) => [x, y, z, 1];
    const quad = (A, B, C, D, c) => {
      positions.push(A, B, C, A, C, D);
      for (let i = 0; i < 6; i++) colors.push(c);
    };
    quad(v(xMin,yMax,zMin), v(xMin,yMax,zMax), v(xMax,yMax,zMax), v(xMax,yMax,zMin), top);
    quad(v(xMin,yMin,zMin), v(xMax,yMin,zMin), v(xMax,yMin,zMax), v(xMin,yMin,zMax), bot);
    quad(v(xMin,yMin,zMin), v(xMin,yMax,zMin), v(xMax,yMax,zMin), v(xMax,yMin,zMin), north);
    quad(v(xMax,yMin,zMax), v(xMax,yMax,zMax), v(xMin,yMax,zMax), v(xMin,yMin,zMax), south);
    quad(v(xMax,yMin,zMin), v(xMax,yMax,zMin), v(xMax,yMax,zMax), v(xMax,yMin,zMax), east);
    quad(v(xMin,yMin,zMax), v(xMin,yMax,zMax), v(xMin,yMax,zMin), v(xMin,yMin,zMin), west);
  };

  // Body coordinate convention (matches the prior stick figure):
  //   y=+0.35 head top, y=0 shoulder/eye, y=-1.1 hip, y=-2.0 feet.
  // +Z is the player's forward direction (they face +Z at yaw=0).

  // Torso — primary handle color.
  pushBox(-0.30, -1.10, -0.18, 0.30, -0.40, 0.18, {
    main, top: light, bot: dark, north: dark, south: main,
  });

  // Head — neutral skin tone with a darker visor on the front (+Z) so the
  // facing direction reads at a glance.
  pushBox(-0.22, -0.05, -0.20, 0.22, 0.35, 0.20, {
    main: skinHead,
    top: [
      Math.min(1, skinHead[0] * 1.05),
      Math.min(1, skinHead[1] * 1.05),
      Math.min(1, skinHead[2] * 1.05),
      a,
    ],
    bot: dark,
    south: visor, // +Z = face
  });

  // Arms — thin boxes on either side of the torso.
  pushBox(-0.50, -1.05, -0.12, -0.32, -0.40, 0.12, {
    main: dark, top: main, bot: dark,
  });
  pushBox( 0.32, -1.05, -0.12,  0.50, -0.40, 0.12, {
    main: dark, top: main, bot: dark,
  });

  // Legs — squarer boxes from hip to feet.
  pushBox(-0.22, -2.00, -0.14, -0.04, -1.10, 0.14, {
    main: dark, top: main, bot: [0, 0, 0, a],
  });
  pushBox( 0.04, -2.00, -0.14,  0.22, -1.10, 0.14, {
    main: dark, top: main, bot: [0, 0, 0, a],
  });

  const f = new Form(
    { type: "triangle", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  f.noFade = true;
  return f;
}

function handleHash(handle) {
  let h = 0; for (let i = 0; i < handle.length; i++) h = (h * 31 + handle.charCodeAt(i)) | 0;
  return h >>> 0;
}

function hslToRgb(hue, sat, light) {
  const c = sat * (1 - Math.abs(2 * light - 1));
  const x = c * (1 - Math.abs(((hue / 60) % 2) - 1));
  const m = light - c / 2;
  let r = 0, g = 0, b = 0;
  if (hue < 60)       [r, g, b] = [c, x, 0];
  else if (hue < 120) [r, g, b] = [x, c, 0];
  else if (hue < 180) [r, g, b] = [0, c, x];
  else if (hue < 240) [r, g, b] = [0, x, c];
  else if (hue < 300) [r, g, b] = [x, 0, c];
  else                [r, g, b] = [c, 0, x];
  return [Math.round((r + m) * 255), Math.round((g + m) * 255), Math.round((b + m) * 255)];
}

// Deterministic per-handle color for named players: full hue range, vivid.
function handleColor(handle) {
  const hue = handleHash(handle) % 360;
  return hslToRgb(hue, 0.7, 0.6);
}

// Spectators (specs) — anon guest_/swarm_ tabs. Constrained to the cool half
// of the spectrum (cyan→blue→violet→pink) with bright lightness so they read
// as ghostly/icy rather than just "dim gray".
function specColor(handle) {
  const hue = 170 + (handleHash(handle) % 150); // 170..319
  return hslToRgb(hue, 0.55, 0.72);
}

// Called from paint() once per frame.
function paintRemotes(ink, form, Form) {
  const t = renderTimeNow();
  for (const [handle, o] of Object.entries(others)) {
    const sample = sampleOther(o, t);
    if (!sample) continue;
    if (!o.body) {
      const watcher = isGuestHandle(handle);
      const color = watcher ? specColor(handle) : handleColor(handle);
      o.body = buildRemoteBody(Form, color, watcher);
    }
    // Mirror local body positioning: Form.position uses (-x, y, -z).
    o.body.position[0] = -sample.x;
    o.body.position[1] = sample.y;
    o.body.position[2] = -sample.z;
    o.body.rotation[1] = sample.yaw;
    ink(255).form(o.body);
  }
}

// ---------------------------------------------------------------------------


let groundPlane;
let groundSkirt;   // solid opaque plate just under the ground that blocks
                   // any lava bleed-through between ground tile seams
let platformBlock;  // bottom and side faces for platform volume
let shadowGround;  // ring + cross — standing on the ground
let shadowAir;     // diagonal X — airborne
let shadowCrouch;  // dense inner dot + outer ring — crouched
let plumbLine;     // vertical line from ground to the player's feet
let bodyFeet;      // two foot wireframes, anchored to ground + yaw (1P only)
let bodyArms;      // two arm wireframes, anchored to eye + yaw (1P only)
let myBody;        // solid humanoid mesh (3P only — same shape as remotes)
let platformEdge;  // bright outline at the ground's perimeter
let obstacleForms = []; // solid pillar + wall meshes (one Form per shape)
let obstacleEdges = []; // bright top-rim line forms paired with obstacleForms
let FormRef;       // captured at boot so sim/paint can build transient forms
let penLocked = false;

// 📱 Mobile control buttons using TextButton UI component
let mobileButtons = {}; // { up, down, left, right, jump, crouch }
let mobileButtonStates = {}; // track which buttons are pressed
let buttonBuffers = {}; // { jump, crouch } - pre-baked custom pixel graphics
let paintingRef = null; // Capture painting function for button graphics

// ⌨️ Keyboard state tracking (for lighting up buttons when keys are held)
let keyboardState = {
  w: false, a: false, s: false, d: false,
  arrowup: false, arrowdown: false, arrowleft: false, arrowright: false,
  space: false, shift: false,
};


// Walk-cycle phase (advanced in sim while moving) for gentle arm/foot bob.
let walkPhase = 0;

// 🐛 Debug: dump a snapshot of scene state once per second so it can be
// pasted back verbatim when something looks off.
let debugDumpTimer = 0;
const DEBUG_DUMP_INTERVAL = 120; // sim ticks (= 1 s at SIM_HZ=120)

// 💀 Death / respawn state
let playerAlive = true;
let deathTickAge = 0; // how long we've been dead (sim ticks, for UI fade-in)

// 🎆 Grenade SFX — synthesis informed by lib/percussion.mjs's TR-808 kick:
// `gonk` is a wood-tom-like thunk for the launcher (high noise tick + tonal
// mid-low pop); `boom` is a stacked noise/sine burst for the detonation.
// Voiced through sound.synth (same surface notepat.mjs uses).
function playGonk(sound) {
  if (!sound?.synth) return;
  const tone = 420 + Math.random() * 80;
  sound.synth({ type: "noise",    tone: 4500,    duration: 0.005, volume: 0.45, attack: 0.0001, decay: 0.0049, pan: 0 });
  sound.synth({ type: "square",   tone,          duration: 0.025, volume: 0.55, attack: 0.0005, decay: 0.024,  pan: 0 });
  sound.synth({ type: "sine",     tone: tone*0.5,duration: 0.060, volume: 0.95, attack: 0.001,  decay: 0.058,  pan: 0 });
  sound.synth({ type: "sine",     tone: 90,      duration: 0.18,  volume: 0.55, attack: 0.002,  decay: 0.175,  pan: 0 });
}
function playBoom(sound) {
  if (!sound?.synth) return;
  sound.synth({ type: "noise",    tone: 1800, duration: 0.18,  volume: 1.0, attack: 0.001, decay: 0.18,  pan: 0 });
  sound.synth({ type: "noise",    tone: 600,  duration: 0.45,  volume: 0.7, attack: 0.005, decay: 0.44,  pan: 0 });
  sound.synth({ type: "sine",     tone: 75,   duration: 0.55,  volume: 1.1, attack: 0.003, decay: 0.54,  pan: 0 });
  sound.synth({ type: "triangle", tone: 45,   duration: 0.40,  volume: 0.6, attack: 0.005, decay: 0.39,  pan: 0 });
}
// "Ouch/ow" — a brief layered yelp. Vocal-ish: noise breath + sine "ah" body
// + triangle undertone, randomised per hit so back-to-back blasts don't read
// as a sample loop. Volume scales with damage so a graze sounds smaller.
function playOuch(sound, severity = 1) {
  if (!sound?.synth) return;
  const t = 300 + Math.random() * 90;
  const v = Math.max(0.25, Math.min(1, severity));
  sound.synth({ type: "noise",    tone: 900,    duration: 0.04, volume: 0.35 * v, attack: 0.005, decay: 0.035, pan: 0 });
  sound.synth({ type: "sine",     tone: t,      duration: 0.10, volume: 0.65 * v, attack: 0.005, decay: 0.092, pan: 0 });
  sound.synth({ type: "sine",     tone: t * 1.5,duration: 0.07, volume: 0.30 * v, attack: 0.005, decay: 0.064, pan: 0 });
  sound.synth({ type: "triangle", tone: t * 0.7,duration: 0.22, volume: 0.55 * v, attack: 0.010, decay: 0.205, pan: 0 });
}

function damagePlayer(amount, doll) {
  if (!playerAlive || amount <= 0) return;
  playerHP = Math.max(0, playerHP - amount);
  lastDamageMs = Date.now();
  playOuch(soundRef, amount / BLAST_DAMAGE_MAX);
  if (playerHP <= 0) {
    // HP-zero death stays on the platform — no lava plunge required.
    playerAlive = false;
    deathTickAge = 0;
    doll?.setFrozen?.(true);
    doll?.clearHeldKeys?.();
  }
}

function fireGrenade(cam, doll) {
  if (!cam || myGrenade || !playerAlive || netSpectator) return;
  // Spawn from the LOGICAL player position, not the chase-camera position.
  // In 3P, cam.x/y/z include the 3P offset, so a naive `cam.x` would launch
  // the grenade from behind/above the avatar — and as the blast knocks you
  // around, the camera follows, making the explosion look pinned to you.
  const phys = doll?.physics;
  const cx = phys?.playerCamX ?? cam.x;
  const cy = -(phys?.playerCamY ?? cam.y);
  const cz = phys?.playerCamZ ?? cam.z;
  const yaw = cam.rotY * Math.PI / 180;
  const pit = cam.rotX * Math.PI / 180;
  const cyR = Math.cos(yaw), syR = Math.sin(yaw);
  const cpR = Math.cos(pit), spR = Math.sin(pit);
  const flipX = (hoverFlipMode & 1) !== 0;
  const flipZ = (hoverFlipMode & 2) !== 0;
  let fx = syR * cpR;
  let fy = spR;
  let fz = cyR * cpR;
  if (flipX) fx = -fx;
  if (flipZ) fz = -fz;
  myGrenade = {
    x: cx + fx * 0.6,
    y: cy + fy * 0.6 - 0.25,           // muzzle just below eye
    z: cz + fz * 0.6,
    vx: fx * GRENADE_SPEED,
    vy: fy * GRENADE_SPEED + 2.0,      // gentle upward arc
    vz: fz * GRENADE_SPEED,
    t: 0,
  };
  playGonk(soundRef);
}

// Bounce a live grenade off the static walls + pillars. Restitution +
// friction match the floor bounce so behaviour is consistent: hit a wall
// hard, lose ~45% of inward speed; hit at a glancing angle, mostly slide.
function resolveGrenadeObstacles(g) {
  const r = GRENADE_RADIUS;
  for (const o of ARENA_OBSTACLES) {
    // Vertical filter — let the grenade arc above a low wall.
    if (typeof o.yMax === "number" && g.y - r > o.yMax) continue;
    if (typeof o.yMin === "number" && g.y + r < o.yMin) continue;
    if (o.type === "cylinder") {
      const dx = g.x - o.x;
      const dz = g.z - o.z;
      const d  = Math.hypot(dx, dz);
      const minD = (o.r ?? 1) + r;
      if (d >= minD) continue;
      if (d < 1e-4) { g.x += minD; continue; }
      const nx = dx / d, nz = dz / d;
      g.x += nx * (minD - d);
      g.z += nz * (minD - d);
      const vd = g.vx * nx + g.vz * nz;
      if (vd < 0) {
        // Reflect with restitution, plus a touch of tangential friction.
        g.vx -= (1 + GRENADE_BOUNCE_RESTITUTION) * vd * nx;
        g.vz -= (1 + GRENADE_BOUNCE_RESTITUTION) * vd * nz;
        g.vx *= GRENADE_BOUNCE_FRICTION;
        g.vz *= GRENADE_BOUNCE_FRICTION;
      }
    } else if (o.type === "box") {
      const cx = (o.xMin + o.xMax) * 0.5;
      const cz = (o.zMin + o.zMax) * 0.5;
      const hx = (o.xMax - o.xMin) * 0.5 + r;
      const hz = (o.zMax - o.zMin) * 0.5 + r;
      const dx = g.x - cx;
      const dz = g.z - cz;
      const ox = hx - Math.abs(dx);
      const oz = hz - Math.abs(dz);
      if (ox <= 0 || oz <= 0) continue;
      if (ox < oz) {
        g.x += dx >= 0 ? ox : -ox;
        const inward = (dx >= 0 && g.vx < 0) || (dx < 0 && g.vx > 0);
        if (inward) {
          g.vx = -g.vx * GRENADE_BOUNCE_RESTITUTION;
          g.vz *= GRENADE_BOUNCE_FRICTION;
        }
      } else {
        g.z += dz >= 0 ? oz : -oz;
        const inward = (dz >= 0 && g.vz < 0) || (dz < 0 && g.vz > 0);
        if (inward) {
          g.vz = -g.vz * GRENADE_BOUNCE_RESTITUTION;
          g.vx *= GRENADE_BOUNCE_FRICTION;
        }
      }
    }
  }
}

function simGrenade(dt, doll, cam) {
  if (!myGrenade) return;
  myGrenade.t += dt;
  myGrenade.vy -= GRENADE_GRAVITY * dt;
  myGrenade.x += myGrenade.vx * dt;
  myGrenade.y += myGrenade.vy * dt;
  myGrenade.z += myGrenade.vz * dt;

  // 🧱 Wall + pillar bounces — same obstacle list the player collides with.
  // Done before floor bounce so a grenade that hits a wall low can still
  // settle on the platform afterwards.
  resolveGrenadeObstacles(myGrenade);

  // Floor bounce — only on top of the platform; outside groundBounds the
  // grenade flies off the edge and falls toward the lava (no edge wall).
  const b = ARENA_CFG.groundBounds;
  const onPlatform = myGrenade.x >= b.xMin && myGrenade.x <= b.xMax &&
                     myGrenade.z >= b.zMin && myGrenade.z <= b.zMax;
  const floorY = ARENA_CFG.groundY + GRENADE_RADIUS;
  if (onPlatform && myGrenade.y <= floorY && myGrenade.vy < 0) {
    myGrenade.y = floorY;
    const incomingVy = myGrenade.vy;
    myGrenade.vy = -incomingVy * GRENADE_BOUNCE_RESTITUTION;
    // Apply ground friction only on real bounces. Otherwise gravity pulling
    // a settled grenade barely below floorY each frame counts as a "bounce"
    // and friction-kills horizontal momentum — the grenade dies on the spot
    // instead of rolling to the platform edge.
    if (Math.abs(incomingVy) > GRENADE_REST_VY * 2) {
      myGrenade.vx *= GRENADE_BOUNCE_FRICTION;
      myGrenade.vz *= GRENADE_BOUNCE_FRICTION;
    }
    if (Math.abs(myGrenade.vy) < GRENADE_REST_VY) myGrenade.vy = 0;
  }

  if (myGrenade.t >= GRENADE_FUSE) {
    detonate(myGrenade.x, myGrenade.y, myGrenade.z, doll, cam);
    myGrenade = null;
    return;
  }

  // Past the lava floor without having detonated — fizzle so we don't render
  // a grenade falling forever into the abyss.
  if (myGrenade.y < ARENA_CFG.deathFloorY - 5) {
    myGrenade = null;
  }
}

function detonate(x, y, z, doll, cam) {
  activeExplosion = { x, y, z, age: 0 };
  playBoom(soundRef);
  if (!doll || !cam) return;
  // Player position in arena's logical world (same coord as the grenade).
  const phys = doll.physics;
  const ax = phys?.playerCamX ?? cam.x;
  const ay = -(phys?.playerCamY ?? cam.y);
  const az = phys?.playerCamZ ?? cam.z;
  const dx = ax - x, dy = ay - y, dz = az - z;
  const dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
  if (dist >= EXPLOSION_RADIUS) return;
  const falloff = 1 - dist / EXPLOSION_RADIUS;   // 1 at center → 0 at edge
  const minD = Math.max(dist, 0.4);
  const nx = dx / minD;
  const ny = dy / minD;
  const nz = dz / minD;
  const horForce = EXPLOSION_KICK_HOR * (0.4 + 0.6 * falloff);
  // Vertical = radial component scaled by falloff + a flat upward bias so
  // ground-level blasts still launch you (ny ≈ 0 when grenade and feet share Y).
  const vertImpulse = ny * EXPLOSION_KICK_VERT * (0.3 + 0.7 * falloff)
                    + EXPLOSION_VERT_BIAS * falloff;
  // cam-doll's applyImpulse takes "world" velocity in its own convention where
  // cam.x = -worldX. Arena's logical X is just cam.x — so to push the player
  // in arena's +X we feed the negated value here.
  doll.applyImpulse?.({
    x: -nx * horForce,
    y: vertImpulse,
    z: -nz * horForce,
  });
  // 💖 Splash damage — linear falloff with a small floor so even a graze stings.
  const dmg = BLAST_DAMAGE_FLOOR + (BLAST_DAMAGE_MAX - BLAST_DAMAGE_FLOOR) * falloff;
  damagePlayer(dmg, doll);
  // Suspend pmove reconciliation briefly so the local knockback isn't yanked
  // back to the server's prediction (server doesn't know about the blast).
  knockbackGraceUntil = Date.now() + 1200;
}

function buildGrenadeForm(g) {
  if (!FormRef) return null;
  const r = GRENADE_RADIUS;
  const heat = Math.min(1, g.t / GRENADE_FUSE);
  const blink = (Math.sin(g.t * 36) * 0.5 + 0.5) * heat;
  const col = [
    1.0,
    0.85 - heat * 0.55 + blink * 0.2,
    0.25 - heat * 0.2 + blink * 0.05,
    1.0,
  ];
  // Octahedron — 6 verts, 8 triangles. Reads as a small spinning shell.
  // Bake spin + world position straight into vertex coords (matches the
  // lava/ground/platform pattern); fresh forms with `pos: [...]` were not
  // sticking to their world position when the player WASD'd.
  const yaw = (g.t * 240) * Math.PI / 180;
  const cy = Math.cos(yaw), sy = Math.sin(yaw);
  const place = (lx, ly, lz) => [
    g.x + (lx * cy - lz * sy),
    g.y + ly,
    g.z + (lx * sy + lz * cy),
    1,
  ];
  const v = [
    place( 0,  r, 0), place( 0, -r, 0),
    place( r,  0, 0), place(-r,  0, 0),
    place( 0,  0, r), place( 0,  0,-r),
  ];
  const tris = [
    [0,2,4],[0,4,3],[0,3,5],[0,5,2],
    [1,4,2],[1,3,4],[1,5,3],[1,2,5],
  ];
  const positions = [], colors = [];
  for (const [a, b, c] of tris) {
    positions.push(v[a], v[b], v[c]);
    colors.push(col, col, col);
  }
  const f = new FormRef(
    { type: "triangle", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  f.noFade = true;
  return f;
}

function buildExplosionPill(ex) {
  if (!FormRef) return null;
  const t = ex.age / EXPLOSION_DURATION;
  if (t >= 1) return null;
  const ease = 1 - (1 - t) * (1 - t); // ease-out grow
  // Sphere whose radius matches the physics blast radius — visual lines up
  // with what actually pushes / damages you.
  const R = EXPLOSION_RADIUS * (0.35 + ease * 0.65);
  const H = 0;                          // pill → sphere
  const alpha = Math.max(0, 1 - t);
  const ringSegs = 16;
  const hemiSegs = 5;

  // Bake the world position straight into each vertex (use pos:[0,0,0])
  // — fresh per-frame forms with non-zero `pos` weren't tracking the
  // detonation point when the player moved, and absolute coords also let
  // the platform's depth-buffer occlude wireframe behind it.
  const ringPoints = (y, r) => {
    const pts = [];
    for (let i = 0; i < ringSegs; i++) {
      const a = (i / ringSegs) * Math.PI * 2;
      pts.push([
        ex.x + Math.cos(a) * r,
        ex.y + y,
        ex.z + Math.sin(a) * r,
        1,
      ]);
    }
    return pts;
  };
  const allRings = [];
  for (let i = 0; i <= hemiSegs; i++) {
    const phi = -Math.PI / 2 + (i / hemiSegs) * (Math.PI / 2);
    allRings.push(ringPoints(-H / 2 + R * Math.sin(phi), R * Math.cos(phi)));
  }
  for (let i = 1; i <= hemiSegs; i++) {
    const phi = (i / hemiSegs) * (Math.PI / 2);
    allRings.push(ringPoints( H / 2 + R * Math.sin(phi), R * Math.cos(phi)));
  }
  const positions = [], colors = [];
  const orange = [1.0, 0.65, 0.2, alpha];
  const yellow = [1.0, 0.95, 0.55, alpha * 0.85];
  for (const ring of allRings) {
    for (let i = 0; i < ringSegs; i++) {
      positions.push(ring[i], ring[(i + 1) % ringSegs]);
      colors.push(orange, orange);
    }
  }
  for (let i = 0; i < ringSegs; i += 2) {
    for (let r = 0; r < allRings.length - 1; r++) {
      positions.push(allRings[r][i], allRings[r + 1][i]);
      colors.push(yellow, yellow);
    }
  }
  const f = new FormRef(
    { type: "line", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  f.noFade = true;
  return f;
}

// Shared respawn trigger — touch, gamepad A, gamepad Start, and Space all
// route through this so the "TAP TO RESPAWN" prompt accepts every input.
function tryRespawn(system) {
  if (playerAlive || deathTickAge <= 30) return false;
  playerAlive = true;
  deathTickAge = 0;
  walkedTiles.clear();
  prevPlayerTile = null;
  // 🎆 Drop any in-flight grenade and resume server reconciliation right away,
  // otherwise the suspended reconcile from a fatal blast keeps the camera
  // glued to the lava view even after cam-doll teleported back to spawn.
  myGrenade = null;
  activeExplosion = null;
  knockbackGraceUntil = 0;
  playerHP = MAX_HP;
  lastDamageMs = 0;
  const doll = system?.fps?.doll;
  doll?.respawn?.(0, 0);
  // Force the 3P lerp to re-snap. Otherwise tpCurrent (which last lerped
  // toward the falling-into-lava camera position) keeps the visual camera
  // floating near the death floor for ~half a second after respawn — looks
  // like the player is "still falling" even though logical pos teleported.
  if (doll && zoomLevel > 0) {
    doll.setThirdPerson(false);
    doll.setThirdPerson(true, ZOOM_DISTANCES[zoomLevel]);
  }
  return true;
}

// 🎮 Gamepad / Xbox controller state.
// Populated lazily on the first gamepad event, then driven by act() each
// frame. Connection persists for the session — we don't get disconnect events
// surfaced through the disk event stream, so we treat "ever seen" as "still
// here" for UI purposes.
let gamepadState = {
  connected: false,
  id: null,
  index: 0,
  buttons: {}, // { 0: true, 1: false, ... } indexed by Standard Gamepad button
  axes: { 0: 0, 1: 0, 2: 0, 3: 0 },
  // Mirrors what we last told doll.setMovement so we only emit on transitions.
  movement: { forward: false, back: false, left: false, right: false },
};
const GP_DEADZONE = 0.3;          // movement threshold (gamepad.mjs already pre-filters at 0.15)
const GP_LOOK_DEG_PER_SEC = 180;  // right-stick look speed at full deflection
const GP_PITCH_LIMIT = 89;

// 🟨 Per-tile highlight state.
// hoverTile: { row, col } of tile under the crosshair, or null.
// walkedTiles: Map<tileKey, ageTicks>. A tile just stepped onto starts at
// WALK_AGE_TICKS and decays each sim tick; drawn with alpha = age/TICKS.
const WALK_AGE_TICKS = 90; // ≈ 0.75 s at 120 Hz
let hoverTile = null;
let prevPlayerTile = null;
const walkedTiles = new Map();

// 🔍 Diagnostic: stash the last hit point + pen coords so paint() can draw a
// visible crosshair and the snapshot can log exactly where the ray landed.
let lastHitWorld = null;   // [x, z] or null
let lastPenScreen = null;  // [x, y] or null

// Axis-sign experiment toggle. Press F to cycle. See the hover raycast for
// what each bit flips.
//   0 = baseline          1 = flipX
//   2 = flipZ             3 = flip both
let hoverFlipMode = 3; // flip both X and Z per recent experiments

// ⚡ Adaptive-quality flags driven by measured render FPS. Auto-toggle in
// paint() based on the rolling frame-time average. Pieces can override via the
// HUD labels (future: click to pin). "LOW" = coarser tile, static lava, skip
// body wireframes. "MED" = static lava only. "HIGH" = everything on.
let perfLowMode = false;
let perfMedMode = false;
const PERF_LOW_MS = 25;   // below ~40fps → drop to LOW
const PERF_MED_MS = 18;   // below ~55fps → drop to MED
const PERF_HIGH_MS = 14;  // above ~70fps → return to HIGH
let perfSamplesSinceSwitch = 0;

// 🎆 Grenade launcher — Q3-style projectile, click to fire one at a time.
// One in-flight grenade per player; fuse-detonates after a beat into a pill-
// shaped blast that knocks the firer (and the cam-doll) outward. The blast
// can punt the player off the platform and into the lava — that's the trick.
const GRENADE_RADIUS = 0.18;
const GRENADE_SPEED = 8;              // initial m/s — slow enough that a forward
                                      // shot still detonates within EXPLOSION_RADIUS
                                      // of the firer (the arena is only ±14u wide)
const GRENADE_GRAVITY = 22;           // m/s² (lighter than player gravity → arcs)
const GRENADE_BOUNCE_RESTITUTION = 0.55;
const GRENADE_BOUNCE_FRICTION = 0.78; // horizontal vel scaling per bounce
const GRENADE_FUSE = 0.9;             // seconds before detonation
const GRENADE_REST_VY = 1.0;          // settle threshold (avoid eternal jitter)
const EXPLOSION_RADIUS = 7.5;         // world units; generous so forward shots
                                      // still rocket-jump the firer
const EXPLOSION_DURATION = 0.45;      // seconds the pill stays visible
const EXPLOSION_KICK_HOR = 1.3;       // dolly impulse → ~12u horizontal flight
const EXPLOSION_KICK_VERT = 14;       // worldYVel impulse (+ is up), units/sec
const EXPLOSION_VERT_BIAS = 5;        // baseline upward bump even at edge

let myGrenade = null;        // { x, y, z, vx, vy, vz, t } — local single-shot
let activeExplosion = null;  // { x, y, z, age } — local visual effect
let knockbackGraceUntil = 0; // ms timestamp; while < now() pmove reconcile is suspended
let soundRef = null;         // captured in boot for synth access

// 💖 Player HP — blast splash damage with linear falloff to the radius edge.
// Center hits (~feet-stomp) score lethal damage, edge hits sting but survive,
// so multiple self-blasts on the platform can kill you without ever touching
// the lava. Reset on respawn.
const MAX_HP = 100;
const BLAST_DAMAGE_MAX = 90;        // direct-hit damage at falloff = 1
const BLAST_DAMAGE_FLOOR = 6;       // edge-of-radius minimum (so a graze hurts)
let playerHP = MAX_HP;
let lastDamageMs = 0;               // wall-clock; drives the red flash overlay
const HP_FLASH_DURATION = 0.35;     // seconds the screen tints red after a hit

// 🔎 Camera zoom — wheel scroll steps between 1P and 3P at discrete distances.
// Level 0 = first person. Levels 1..N = third person, pulling the camera back
// further each click. More close-in steps for shoulder-camera views.
// Middle-mouse still toggles between 1P and a default 3P.
const ZOOM_DISTANCES = [0, 0.5, 1, 1.5, 2, 3, 4.5, 6, 9, 12, 16, 24, 32];
let zoomLevel = 2; // Start at 1 unit back (shoulder camera)

// 🎥 Player facing direction (decoupled from camera rotation)
let playerFacing = 0; // Player body Y rotation (degrees), independent from camera

// 🎥 Right-click camera orbit (3P mode only): rotate camera around player
// without changing player body rotation. Orbits by modifying XZ offset.
let orbitAngle = 0;   // extra Y rotation for camera only (degrees)
let orbiting = false; // currently dragging with right button
let orbitDistance = 0; // captured XZ distance when orbit starts, stays constant
let appliedOrbitOffset = [0, 0]; // track X,Z offset we applied so we can undo it
let baseRotY = 0;     // cam.rotY when orbit started, to prevent player spinning
let orbitSnapped = false; // true if orbit was released; reset on next left-click

function applyZoom(doll) {
  if (!doll) return;
  const d = ZOOM_DISTANCES[zoomLevel];
  if (zoomLevel === 0) doll.setThirdPerson(false);
  else doll.setThirdPerson(true, d);
  // Drop any leftover orbit state when zoom changes. Otherwise a previous
  // right-drag's orbitAngle + orbitDistance keep the orbit branch (in sim)
  // displacing the camera around the player every frame, which reads as the
  // character "flying" sideways the next time you scroll into 3P.
  orbiting = false;
  orbitAngle = 0;
  orbitDistance = 0;
  orbitSnapped = false;
}

function tileKey(row, col) { return row * GRID + col; }
function tileFromKey(k) { return { row: Math.floor(k / GRID), col: k % GRID }; }

// World XZ → tile (row, col) or null if outside the GRID bounds.
function tileAt(worldX, worldZ) {
  const step = (GROUND_SIZE * 2) / GRID;
  const col = Math.floor((worldX + GROUND_SIZE) / step);
  const row = Math.floor((worldZ + GROUND_SIZE) / step);
  if (col < 0 || col >= GRID || row < 0 || row >= GRID) return null;
  return { row, col };
}

// AC sim runs at a fixed 120 Hz (see lib/loop.mjs updateFps). All physics and
// state updates happen in sim(), so the game runs at constant speed regardless
// of paint FPS — the CPU rasterizer dropping to 30 fps while looking at the
// ground will slow the *visual* update but not the simulation.
const SIM_HZ = 120;

// Speed tracking
let prevX = 0, prevY = 0, prevZ = 0;
let speedSmoothed = 0;
const SPEED_SMOOTH = 0.15;

// 🕐 Sim-driven clock (seconds). Incremented each sim tick so every
// gameplay-visible animation advances at a constant rate regardless of
// paint FPS. Paint reads this instead of performance.now() for anything
// that should feel tied to the simulation (lava flow, body bob timing in
// the future, etc.).
let simTime = 0;

// FPS tracking
let frameTimes = [];
let lastFrameTime = 0;

// Ground config. GRID is odd so there's a centre tile under the player at the
// origin (even grids put the player on a 4-way corner junction, which makes
// tile highlights look offset by half a tile).
const GROUND_SIZE = 14;
const GRID = 15;
const GROUND_Y = -1.5;
const FOG_START_SQ = 7 * 7;
const FOG_END_SQ = 14 * 14;

// 🏃 Quake-style movement tuning (AC-scaled ≈ Quake ÷ 32).
//   Canonical Quake: FOV 90°, run 320 u/s, jump 270 u/s, gravity 800 u/s².
// Here AC units ~ 32 Quake units, so numbers shrink proportionally.
const FOV = 90;
const RUN_SPEED = 10;      // u/s (≈ 320 Quake u/s)
const WALK_SPEED = 5;      // u/s crouched
const JUMP_VELOCITY = 8;   // u/s initial → peak jump = jv²/(2g) AC units
const GRAVITY = 50;        // u/s² (heavier / less lofty than Quake's 800-scaled ≈25)
const EYE_HEIGHT = 2.0;    // AC units above ground (matches prior default cam.y=-0.5)
const CROUCH_EYE = 1.2;

// 💀 Death pit — declared before fpsOpts so it can be passed as deathFloorY.
const DEATH_FLOOR_Y = -30;
const DEATH_PAD = 60;        // half-size of the lava floor (generous)
const DEATH_TILES = 14;      // (unused since donut build; kept for reference)
const DEATH_STRIP_FREQ = 0.22;
const DEATH_FLOW_SPEED = 1.8;

export const fpsOpts = {
  fov: FOV,
  y: 0,
  z: 0,
  sensitivity: 0.002,
  runSpeed: RUN_SPEED,
  walkSpeed: WALK_SPEED,
  jumpVelocity: JUMP_VELOCITY,
  gravity: GRAVITY,
  groundY: GROUND_Y,
  eyeHeight: EYE_HEIGHT,
  crouchEyeHeight: CROUCH_EYE,
  // Disable built-in camdoll touch controls; arena handles custom mobile UI instead
  disableTouchControls: true,
  // Outside this XZ rectangle the floor clamp is disabled so the player
  // falls into the pit below. Matches the ground plane's half-size.
  groundBounds: {
    xMin: -GROUND_SIZE,
    xMax: GROUND_SIZE,
    zMin: -GROUND_SIZE,
    zMax: GROUND_SIZE,
  },
  // Dead players stop on the lava instead of falling forever.
  deathFloorY: DEATH_FLOOR_Y,
  // 🏃 Quake-style local prediction: cam-doll runs the same friction +
  // air-accel model as pmove on the server, so strafe-jump speed survives
  // reconciliation.
  airAccel: ARENA_PHYSICS.airAccel,
  groundAccel: ARENA_PHYSICS.groundAccel,
  airCapSpeed: ARENA_PHYSICS.airCapSpeed,
  groundFriction: ARENA_PHYSICS.groundFriction,
  // 🧱 Solid obstacle list resolved client-side too (matches server) so
  // local motion stays in sync with pmove without depending on the
  // reconciler to push the player back out of walls.
  obstacles: ARENA_OBSTACLES,
  playerRadius: ARENA_PHYSICS.playerRadius,
};

const BG = [45 / 255, 48 / 255, 55 / 255];
const COLOR_A = [0.38, 0.35, 0.30, 1.0];
const COLOR_B = [0.22, 0.20, 0.19, 1.0];

// Build a fresh lava floor Form with time-animated stripe colors. Now that
// the Z-buffer correctly occludes, the lava is a FULL plane covering the
// entire pit area — no donut cut-out needed. The ground depth-tests above it.
// When perfLowMode is on, we build the lava once and cache it (no animation).
let lavaCache = null;
let lavaCacheFrame = -1;

function buildLavaFloor(t) {
  if (!FormRef) return null;
  // When perf mode is low, return a cached static lava (skip stripe animation).
  if (perfLowMode && lavaCache) return lavaCache;

  // 🌋 Bound `t` before passing to Math.sin. On iOS Safari, paint `now` can
  // arrive as an epoch-ms value (billions of seconds once divided by 1000),
  // and Math.sin precision collapses at that magnitude — all triangles get
  // the same value and the lava renders flat black. Mod by 2π*scale so the
  // animation is continuous and arguments stay tiny.
  const TWOPI = Math.PI * 2;
  const tFlow = ((t * DEATH_FLOW_SPEED) % TWOPI);
  const tSlow = ((t * 0.9) % TWOPI);

  const positions = [];
  const colors = [];
  const deathY = DEATH_FLOOR_Y;
  const STEP = perfLowMode ? 8 : 4; // coarser tessellation in low-perf mode
  for (let z = -DEATH_PAD; z < DEATH_PAD; z += STEP) {
    for (let x = -DEATH_PAD; x < DEATH_PAD; x += STEP) {
      const x0 = x, z0 = z;
      const x1 = Math.min(x + STEP, DEATH_PAD);
      const z1 = Math.min(z + STEP, DEATH_PAD);
      const cx = (x0 + x1) / 2;
      const cz = (z0 + z1) / 2;
      const w1 = Math.sin(cx * DEATH_STRIP_FREQ - tFlow + cz * 0.08);
      const w2 = Math.sin(cx * 0.09 + cz * 0.27 - tSlow);
      const glow = (w1 + w2 * 0.6) * 0.5;
      const hot = 0.35 + Math.max(0, glow) * 0.65;
      const r = 0.45 + hot * 0.55;
      const g = hot * hot * 0.45;
      const b = hot * hot * hot * 0.08;
      const c = [r, g, b, 1.0];
      positions.push(
        [x0, deathY, z0, 1], [x0, deathY, z1, 1], [x1, deathY, z1, 1],
        [x0, deathY, z0, 1], [x1, deathY, z1, 1], [x1, deathY, z0, 1],
      );
      for (let i = 0; i < 6; i++) colors.push(c);
    }
  }
  const f = new FormRef(
    { type: "triangle", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  f.noFade = true;
  if (perfLowMode) lavaCache = f;
  return f;
}

// Ground fog: RGB lerps toward BG with distance so the arena "dissolves" into
// the sky, but alpha stays a full 1.0. Dropping alpha lets the red death floor
// show through the ground (no depth buffer → painter's order only), and the
// RGB lerp already gives the visual fade.
function fogColor(base, distSq) {
  if (distSq <= FOG_START_SQ) return base;
  if (distSq >= FOG_END_SQ) return [BG[0], BG[1], BG[2], 1.0];
  const t = (distSq - FOG_START_SQ) / (FOG_END_SQ - FOG_START_SQ);
  return [
    base[0] + (BG[0] - base[0]) * t,
    base[1] + (BG[1] - base[1]) * t,
    base[2] + (BG[2] - base[2]) * t,
    1.0,
  ];
}

function boot({ Form, penLock, system, screen, ui, api, painting, net, handle, send, debug, sound, glaze }) {
  penLock();
  FormRef = Form;
  paintingRef = painting;
  soundRef = sound;

  // ✨ Volumetric glaze post-effect (defaults to the `prompt` shader set).
  glaze?.({ on: true });

  const cam = system?.fps?.doll?.cam;
  if (cam) { prevX = cam.x; prevY = cam.y; prevZ = cam.z; }
  lastFrameTime = performance.now();

  // 🔬 Capture `send` so paint() can batch the perf ring to bios.mjs (which
  // mirrors it onto window.__arena_perfStats for the external probe).
  arenaPerfSend = send;

  // 🏟️ Multiplayer: open WS + UDP, send arena:hello.
  netBoot({ net, handle, send, debug });

  // 🎯 Set initial cursor style
  if (api?.cursor) {
    api.cursor('crosshair');
  }

  // 🎨 Create button graphics using painting buffers
  if (painting) {
    console.log("✓ Creating button buffers in boot...");
    // Jump button: relaxed pose (normal state)
    buttonBuffers.jump_normal = painting(56, 28, (api) => {
      const { wipe, ink, box, line } = api;
      wipe(50, 200, 100, 255); // Green background
      ink(255, 255, 200); // Skin color

      // Head with cute face
      box(22, 2, 12, 10); // Head

      // Eyes (small boxes instead of plot)
      ink(50, 50, 50); // Dark eyes
      box(26, 5, 1, 1);
      box(27, 5, 1, 1);
      box(34, 5, 1, 1);
      box(35, 5, 1, 1);

      // Smile (line instead of plot)
      ink(255, 100, 100); // Pink smile
      line(28, 8, 32, 8);

      // Body
      ink(255, 255, 200);
      line(28, 12, 28, 18);

      // Arms (relaxed)
      line(22, 14, 18, 16); // Left arm
      line(34, 14, 38, 16); // Right arm

      // Legs
      line(26, 18, 24, 24); // Left leg
      line(30, 18, 32, 24); // Right leg
    });

    // Jump button: excited jumping pose (pressed state)
    buttonBuffers.jump_active = painting(56, 28, (api) => {
      const { wipe, ink, box, line } = api;
      wipe(80, 220, 120, 255); // Brighter green
      ink(255, 255, 200);

      // Head with happy face
      box(22, 1, 12, 10);

      // Happy eyes
      ink(50, 50, 50);
      box(25, 4, 1, 1);
      box(26, 4, 1, 1);
      box(27, 4, 1, 1);
      box(33, 4, 1, 1);
      box(34, 4, 1, 1);
      box(35, 4, 1, 1);

      // Big smile
      ink(255, 100, 100);
      line(27, 7, 33, 7);
      box(27, 8, 1, 1);
      box(33, 8, 1, 1);

      // Body
      ink(255, 255, 200);
      line(28, 11, 28, 16);

      // Arms raised high (jumping)
      line(22, 12, 16, 4); // Left arm
      line(34, 12, 40, 4); // Right arm

      // Legs bent (jumping)
      line(26, 16, 24, 20);
      line(30, 16, 32, 20);
    });

    // Crouch button: relaxed standing pose (normal state)
    buttonBuffers.crouch_normal = painting(56, 28, (api) => {
      const { wipe, ink, box, line } = api;
      wipe(220, 150, 40, 255); // Orange background
      ink(255, 255, 200);

      // Head with cute face
      box(22, 4, 12, 10);

      // Eyes
      ink(50, 50, 50);
      box(26, 7, 1, 1);
      box(27, 7, 1, 1);
      box(34, 7, 1, 1);
      box(35, 7, 1, 1);

      // Smile
      ink(255, 100, 100);
      line(28, 10, 32, 10);

      // Body
      ink(255, 255, 200);
      line(28, 14, 28, 19);

      // Arms relaxed
      line(22, 16, 18, 18);
      line(34, 16, 38, 18);

      // Legs normal
      line(26, 19, 24, 24);
      line(30, 19, 32, 24);
    });

    // Crouch button: deep crouch pose (pressed state)
    buttonBuffers.crouch_active = painting(56, 28, (api) => {
      const { wipe, ink, box, line } = api;
      wipe(240, 170, 60, 255); // Brighter orange
      ink(255, 255, 200);

      // Head lower
      box(22, 8, 12, 10);

      // Happy eyes
      ink(50, 50, 50);
      box(25, 11, 1, 1);
      box(26, 11, 1, 1);
      box(27, 11, 1, 1);
      box(33, 11, 1, 1);
      box(34, 11, 1, 1);
      box(35, 11, 1, 1);

      // Big smile
      ink(255, 100, 100);
      line(27, 14, 33, 14);
      box(27, 15, 1, 1);
      box(33, 15, 1, 1);

      // Body very bent
      ink(255, 255, 200);
      line(28, 18, 28, 20);

      // Arms tucked
      line(24, 19, 20, 20);
      line(32, 19, 36, 20);

      // Legs very bent
      line(26, 20, 24, 23);
      line(30, 20, 32, 23);
    });

    // Up arrow button - normal state
    buttonBuffers.up_normal = painting(28, 28, (api) => {
      const { wipe, ink, line, box } = api;
      wipe(60, 75, 95, 255); // Blue background
      ink(200, 220, 255);

      // Arrow pointing up
      line(14, 22, 14, 8); // Stem
      line(10, 14, 14, 6); // Left point
      line(18, 14, 14, 6); // Right point

      // Decorative dots
      ink(150, 200, 255);
      box(10, 22, 1, 1);
      box(18, 22, 1, 1);
    });

    // Up arrow button - active state (brightened)
    buttonBuffers.up_active = painting(28, 28, (api) => {
      const { wipe, ink, line, box } = api;
      wipe(100, 130, 170, 255); // Brighter blue
      ink(255, 255, 255);

      // Arrow pointing up (thicker with extra lines)
      line(14, 22, 14, 6); // Stem
      line(13, 22, 13, 6);
      line(15, 22, 15, 6);
      line(10, 14, 14, 4); // Left point extended
      line(18, 14, 14, 4); // Right point extended
      line(10, 15, 14, 5);
      line(18, 15, 14, 5);

      // Decorative dots highlighted
      ink(255, 255, 200);
      box(10, 22, 1, 1);
      box(18, 22, 1, 1);
      box(10, 23, 1, 1);
      box(18, 23, 1, 1);
    });

    // Down arrow button - normal state
    buttonBuffers.down_normal = painting(28, 28, (api) => {
      const { wipe, ink, line, box } = api;
      wipe(60, 75, 95, 255);
      ink(200, 220, 255);

      // Arrow pointing down
      line(14, 6, 14, 20); // Stem
      line(10, 14, 14, 22); // Left point
      line(18, 14, 14, 22); // Right point

      // Decorative dots
      ink(150, 200, 255);
      box(10, 6, 1, 1);
      box(18, 6, 1, 1);
    });

    // Down arrow button - active state (brightened)
    buttonBuffers.down_active = painting(28, 28, (api) => {
      const { wipe, ink, line, box } = api;
      wipe(100, 130, 170, 255); // Brighter blue
      ink(255, 255, 255);

      // Arrow pointing down (thicker with extra lines)
      line(14, 6, 14, 24); // Stem extended
      line(13, 6, 13, 24);
      line(15, 6, 15, 24);
      line(10, 14, 14, 24); // Left point extended
      line(18, 14, 14, 24); // Right point extended
      line(10, 13, 14, 23);
      line(18, 13, 14, 23);

      // Decorative dots highlighted
      ink(255, 255, 200);
      box(10, 6, 1, 1);
      box(18, 6, 1, 1);
      box(10, 5, 1, 1);
      box(18, 5, 1, 1);
    });

    // Left arrow button - normal state
    buttonBuffers.left_normal = painting(28, 28, (api) => {
      const { wipe, ink, line, box } = api;
      wipe(60, 75, 95, 255);
      ink(200, 220, 255);

      // Arrow pointing left
      line(22, 14, 6, 14); // Stem
      line(14, 10, 6, 14); // Top point
      line(14, 18, 6, 14); // Bottom point

      // Decorative dots
      ink(150, 200, 255);
      box(22, 10, 1, 1);
      box(22, 18, 1, 1);
    });

    // Left arrow button - active state (brightened)
    buttonBuffers.left_active = painting(28, 28, (api) => {
      const { wipe, ink, line, box } = api;
      wipe(100, 130, 170, 255); // Brighter blue
      ink(255, 255, 255);

      // Arrow pointing left (thicker with extra lines)
      line(22, 14, 4, 14); // Stem extended
      line(22, 13, 4, 13);
      line(22, 15, 4, 15);
      line(14, 10, 4, 14); // Top point extended
      line(14, 18, 4, 14); // Bottom point extended
      line(15, 10, 5, 14);
      line(15, 18, 5, 14);

      // Decorative dots highlighted
      ink(255, 255, 200);
      box(22, 10, 1, 1);
      box(22, 18, 1, 1);
      box(23, 10, 1, 1);
      box(23, 18, 1, 1);
    });

    // Right arrow button - normal state
    buttonBuffers.right_normal = painting(28, 28, (api) => {
      const { wipe, ink, line, box } = api;
      wipe(60, 75, 95, 255);
      ink(200, 220, 255);

      // Arrow pointing right
      line(6, 14, 22, 14); // Stem
      line(14, 10, 22, 14); // Top point
      line(14, 18, 22, 14); // Bottom point

      // Decorative dots
      ink(150, 200, 255);
      box(6, 10, 1, 1);
      box(6, 18, 1, 1);
    });

    // Right arrow button - active state (brightened)
    buttonBuffers.right_active = painting(28, 28, (api) => {
      const { wipe, ink, line, box } = api;
      wipe(100, 130, 170, 255); // Brighter blue
      ink(255, 255, 255);

      // Arrow pointing right (thicker with extra lines)
      line(6, 14, 24, 14); // Stem extended
      line(6, 13, 24, 13);
      line(6, 15, 24, 15);
      line(14, 10, 24, 14); // Top point extended
      line(14, 18, 24, 14); // Bottom point extended
      line(13, 10, 23, 14);
      line(13, 18, 23, 14);

      // Decorative dots highlighted
      ink(255, 255, 200);
      box(6, 10, 1, 1);
      box(6, 18, 1, 1);
      box(5, 10, 1, 1);
      box(5, 18, 1, 1);
    });

    // 💣 Grenade button — fuse unlit (idle)
    buttonBuffers.grenade_normal = painting(28, 28, (api) => {
      const { wipe, ink, box, line } = api;
      wipe(140, 50, 40, 255); // Deep TNT red
      // Pineapple body (olive drab)
      ink(70, 95, 70);
      box(9, 12, 10, 11);
      // Body shading (right side darker)
      ink(50, 75, 50);
      box(17, 12, 2, 11);
      // Cross-hatch texture
      ink(40, 65, 40);
      line(9, 15, 18, 15);
      line(9, 19, 18, 19);
      line(12, 12, 12, 22);
      line(15, 12, 15, 22);
      // Highlight glint on body
      ink(120, 150, 120);
      box(10, 13, 1, 1);
      // Top cap (steel)
      ink(120, 120, 130);
      box(11, 9, 6, 3);
      ink(160, 160, 170);
      box(11, 9, 6, 1);
      // Pin ring (yellow)
      ink(220, 200, 80);
      line(17, 10, 22, 7);
      box(21, 5, 3, 3);
      ink(180, 160, 60);
      box(22, 6, 1, 1);
    });

    // 💣 Grenade button — fuse lit (pressed/launching)
    buttonBuffers.grenade_active = painting(28, 28, (api) => {
      const { wipe, ink, box, line } = api;
      wipe(220, 80, 50, 255); // Hot warning red
      // Body
      ink(90, 125, 90);
      box(9, 12, 10, 11);
      ink(60, 90, 60);
      box(17, 12, 2, 11);
      // Cross-hatch
      ink(50, 80, 50);
      line(9, 15, 18, 15);
      line(9, 19, 18, 19);
      line(12, 12, 12, 22);
      line(15, 12, 15, 22);
      // Brighter highlight
      ink(160, 200, 160);
      box(10, 13, 1, 1);
      box(11, 13, 1, 1);
      // Top cap (brighter)
      ink(180, 180, 190);
      box(11, 9, 6, 3);
      // Spark trail rising from cap
      ink(255, 220, 60);
      line(14, 9, 14, 5);
      box(13, 6, 1, 1);
      box(15, 6, 1, 1);
      // Flame burst
      ink(255, 140, 40);
      box(12, 2, 5, 3);
      ink(255, 240, 120);
      box(13, 3, 3, 1);
      box(14, 1, 1, 1);
      // Sparks flying outward
      ink(255, 200, 100);
      box(7, 7, 1, 1);
      box(20, 9, 1, 1);
      box(22, 4, 1, 1);
    });

    // 🎥 View toggle — 1st person (default normal state)
    buttonBuffers.view_normal = painting(56, 28, (api) => {
      const { wipe, ink, box, line } = api;
      wipe(50, 105, 140, 255); // Deep teal — matches "1st" HUD family
      ink(255, 255, 200); // Skin

      // Big close-up head (filling the strip — "you ARE this character")
      box(20, 4, 16, 14);

      // Eyes — wide & looking right at the viewer (the camera IS the eyes)
      ink(50, 50, 50);
      box(24, 9, 2, 2);
      box(30, 9, 2, 2);

      // Cyan glint pupils — POV signal
      ink(120, 220, 230);
      box(24, 9, 1, 1);
      box(30, 9, 1, 1);

      // Smile
      ink(255, 100, 100);
      line(25, 14, 31, 14);
      box(25, 13, 1, 1);
      box(31, 13, 1, 1);

      // Cheek blush
      ink(255, 180, 180);
      box(21, 12, 1, 1);
      box(34, 12, 1, 1);

      // Forward gaze brackets — chevron radiating right (POV cone)
      ink(180, 230, 240);
      line(40, 11, 47, 8);
      line(40, 11, 47, 14);
      line(40, 16, 47, 13);
      line(40, 16, 47, 19);

      // Tiny "1" badge bottom-left
      ink(255, 255, 255);
      box(7, 19, 1, 5);
      box(8, 19, 1, 1);

      // Hint of shoulders (peeking up)
      ink(255, 255, 200);
      line(20, 18, 17, 22);
      line(36, 18, 39, 22);
    });

    // 🎥 View toggle — 3rd person (active state)
    buttonBuffers.view_active = painting(56, 28, (api) => {
      const { wipe, ink, box, line } = api;
      wipe(140, 95, 180, 255); // Lavender — matches "3rd" HUD family
      ink(255, 255, 200); // Skin

      // Full-body cute character on the right (seen from behind/side)
      // Head
      box(34, 4, 8, 7);

      // Tiny back-of-head dots (no facing eyes — facing away)
      ink(50, 50, 50);
      box(36, 7, 1, 1);
      box(39, 7, 1, 1);

      // Body
      ink(255, 255, 200);
      line(38, 11, 38, 17);

      // Arms
      line(34, 13, 31, 16);
      line(42, 13, 45, 16);

      // Legs
      line(36, 17, 34, 23);
      line(40, 17, 42, 23);

      // Floating camera behind the character (left side)
      ink(40, 40, 50);
      box(7, 9, 10, 8); // body
      ink(80, 80, 95);
      box(7, 9, 10, 1); // top edge highlight
      // Lens
      ink(50, 50, 60);
      box(13, 11, 4, 4);
      ink(120, 220, 230);
      box(14, 12, 2, 2);
      ink(255, 255, 255);
      box(14, 12, 1, 1);
      // Tally light (recording)
      ink(255, 80, 80);
      box(8, 10, 1, 1);

      // Sight line camera → character
      ink(220, 200, 240, 200);
      line(17, 13, 33, 13);

      // Tiny "3" badge bottom-right (3 stacked dots)
      ink(255, 255, 255);
      box(50, 19, 2, 1);
      box(50, 21, 2, 1);
      box(50, 23, 2, 1);
    });

    console.log("✓ Button buffers created successfully");
  }


  // 📱 Create mobile control buttons using ui.Button (always enabled for testing/development)
  if (screen && ui?.Button) {
    initMobileButtons(screen, ui);
  }

  // --- Vertex-colored checkerboard ground with fog ---
  const positions = [];
  const colors = [];
  const step = (GROUND_SIZE * 2) / GRID;

  for (let row = 0; row < GRID; row++) {
    for (let col = 0; col < GRID; col++) {
      const x0 = -GROUND_SIZE + col * step;
      const z0 = -GROUND_SIZE + row * step;
      const x1 = x0 + step;
      const z1 = z0 + step;

      const cx = (x0 + x1) / 2;
      const cz = (z0 + z1) / 2;
      const dSq = cx * cx + cz * cz;

      const base = (row + col) % 2 === 0 ? COLOR_A : COLOR_B;
      const c = fogColor(base, dSq);

      positions.push(
        [x0, GROUND_Y, z0, 1], [x0, GROUND_Y, z1, 1], [x1, GROUND_Y, z1, 1],
        [x0, GROUND_Y, z0, 1], [x1, GROUND_Y, z1, 1], [x1, GROUND_Y, z0, 1],
      );
      colors.push(c, c, c, c, c, c);
    }
  }

  groundPlane = new Form(
    { type: "triangle", positions, colors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  groundPlane.noFade = true;

  // 🔲 Ground skirt — a single opaque dark quad at the EXACT SAME Y as the
  // main ground plane, covering a slightly oversized XZ footprint. Drawn
  // BEFORE the ground so any rasterizer seams between ground tiles (painter's
  // order, no depth buffer) reveal the dark skirt instead of the lava far
  // below. The +0.5 AC-unit pad ensures the skirt also catches seams at the
  // platform outer edge.
  // Skirt sits 0.02 below the ground so painter's order places ground clearly
  // on top; oversized by 0.5 AC units on each side so the outer edge of the
  // arena also has a backstop.
  const skirtY = GROUND_Y - 0.02;
  const skirtR = GROUND_SIZE + 0.5;
  const skirtColor = [0.06, 0.06, 0.08, 1.0];
  groundSkirt = new Form(
    {
      type: "triangle",
      positions: [
        [-skirtR, skirtY, -skirtR, 1],
        [-skirtR, skirtY,  skirtR, 1],
        [ skirtR, skirtY,  skirtR, 1],
        [-skirtR, skirtY, -skirtR, 1],
        [ skirtR, skirtY,  skirtR, 1],
        [ skirtR, skirtY, -skirtR, 1],
      ],
      colors: [skirtColor, skirtColor, skirtColor, skirtColor, skirtColor, skirtColor],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  groundSkirt.noFade = true;

  // 🧱 Platform block — bottom and side faces to give the arena volume
  const platformDepth = 2.0; // thickness of the platform block
  const bottomY = GROUND_Y - platformDepth;

  // Different colors for each side
  const bottomColor = [0.08, 0.08, 0.12, 1.0];     // Dark blue-black texture
  const northColor = [0.22, 0.18, 0.14, 1.0];      // Brown
  const southColor = [0.16, 0.20, 0.18, 1.0];      // Dark teal
  const eastColor = [0.20, 0.16, 0.16, 1.0];       // Dark red-brown
  const westColor = [0.18, 0.20, 0.14, 1.0];       // Dark olive

  const platformGs = GROUND_SIZE;

  const platformPositions = [];
  const platformColors = [];

  // Bottom face (two triangles, full area) - textured
  platformPositions.push(
    [-platformGs, bottomY, -platformGs, 1], [-platformGs, bottomY, platformGs, 1], [platformGs, bottomY, platformGs, 1],
    [-platformGs, bottomY, -platformGs, 1], [platformGs, bottomY, platformGs, 1], [platformGs, bottomY, -platformGs, 1],
  );
  for (let i = 0; i < 6; i++) platformColors.push(bottomColor);

  // Side faces - solid colors per side (no stripes)
  const sideStep = (platformGs * 2) / 8;

  // North side (-Z direction) - brown
  for (let i = 0; i < 8; i++) {
    const x0 = -platformGs + i * sideStep;
    const x1 = x0 + sideStep;
    platformPositions.push(
      [x0, GROUND_Y, -platformGs, 1], [x0, bottomY, -platformGs, 1], [x1, bottomY, -platformGs, 1],
      [x0, GROUND_Y, -platformGs, 1], [x1, bottomY, -platformGs, 1], [x1, GROUND_Y, -platformGs, 1],
    );
    for (let j = 0; j < 6; j++) platformColors.push(northColor);
  }

  // South side (+Z direction) - teal
  for (let i = 0; i < 8; i++) {
    const x0 = -platformGs + i * sideStep;
    const x1 = x0 + sideStep;
    platformPositions.push(
      [x0, GROUND_Y, platformGs, 1], [x1, bottomY, platformGs, 1], [x1, GROUND_Y, platformGs, 1],
      [x0, GROUND_Y, platformGs, 1], [x0, bottomY, platformGs, 1], [x1, bottomY, platformGs, 1],
    );
    for (let j = 0; j < 6; j++) platformColors.push(southColor);
  }

  // East side (+X direction) - red-brown
  for (let i = 0; i < 8; i++) {
    const z0 = -platformGs + i * sideStep;
    const z1 = z0 + sideStep;
    platformPositions.push(
      [platformGs, GROUND_Y, z0, 1], [platformGs, bottomY, z0, 1], [platformGs, bottomY, z1, 1],
      [platformGs, GROUND_Y, z0, 1], [platformGs, bottomY, z1, 1], [platformGs, GROUND_Y, z1, 1],
    );
    for (let j = 0; j < 6; j++) platformColors.push(eastColor);
  }

  // West side (-X direction) - olive
  for (let i = 0; i < 8; i++) {
    const z0 = -platformGs + i * sideStep;
    const z1 = z0 + sideStep;
    platformPositions.push(
      [-platformGs, GROUND_Y, z0, 1], [-platformGs, bottomY, z1, 1], [-platformGs, GROUND_Y, z1, 1],
      [-platformGs, GROUND_Y, z0, 1], [-platformGs, bottomY, z0, 1], [-platformGs, bottomY, z1, 1],
    );
    for (let j = 0; j < 6; j++) platformColors.push(westColor);
  }

  platformBlock = new Form(
    { type: "triangle", positions: platformPositions, colors: platformColors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  platformBlock.noFade = true;

  // 🧱 Platform edge — a glowing rim so you can see where the floor ends.
  // Four line segments slightly above the ground, plus short drops at each
  // corner so the edge reads in depth even when approaching at a shallow
  // angle.
  const edgeY = GROUND_Y + 0.04;
  const edgeDrop = GROUND_Y - 0.2;
  const gs = GROUND_SIZE;
  const rim = [1, 0.9, 0.45, 0.95];
  const drop = [1, 0.35, 0.15, 0.85];
  platformEdge = new Form(
    {
      type: "line",
      positions: [
        // Perimeter
        [-gs, edgeY, -gs, 1], [ gs, edgeY, -gs, 1],
        [ gs, edgeY, -gs, 1], [ gs, edgeY,  gs, 1],
        [ gs, edgeY,  gs, 1], [-gs, edgeY,  gs, 1],
        [-gs, edgeY,  gs, 1], [-gs, edgeY, -gs, 1],
        // Corner drops (short lines falling into the pit)
        [-gs, edgeY, -gs, 1], [-gs, edgeDrop, -gs, 1],
        [ gs, edgeY, -gs, 1], [ gs, edgeDrop, -gs, 1],
        [ gs, edgeY,  gs, 1], [ gs, edgeDrop,  gs, 1],
        [-gs, edgeY,  gs, 1], [-gs, edgeDrop,  gs, 1],
      ],
      colors: [
        rim, rim, rim, rim, rim, rim, rim, rim,
        rim, drop, rim, drop, rim, drop, rim, drop,
      ],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  platformEdge.noFade = true;

  // 🧱 Pillars + walls — solid triangle meshes built from ARENA_OBSTACLES.
  // The same list drives client/server collision (see lib/arena-world.mjs),
  // so what you see is what blocks you.
  obstacleForms = [];
  obstacleEdges = [];
  for (let i = 0; i < ARENA_OBSTACLES.length; i++) {
    const o = ARENA_OBSTACLES[i];
    const baseColor = ARENA_OBSTACLE_COLORS[i] ?? [0.4, 0.4, 0.45, 1.0];
    if (o.type === "cylinder") {
      const segs = 14;
      const positions = [];
      const colors = [];
      const yLo = o.yMin, yHi = o.yMax;
      // Lighter top, darker bottom for depth.
      const top = baseColor;
      const bot = [baseColor[0] * 0.55, baseColor[1] * 0.55, baseColor[2] * 0.55, 1.0];
      for (let s = 0; s < segs; s++) {
        const a0 = (s / segs) * Math.PI * 2;
        const a1 = ((s + 1) / segs) * Math.PI * 2;
        const x0 = o.x + Math.cos(a0) * o.r, z0 = o.z + Math.sin(a0) * o.r;
        const x1 = o.x + Math.cos(a1) * o.r, z1 = o.z + Math.sin(a1) * o.r;
        // Side quad as two triangles. Slight per-segment shade variation
        // so the curvature reads instead of looking like a flat hexagon.
        const shade = 0.85 + 0.15 * Math.cos(a0 * 1.5);
        const sCol = [top[0] * shade, top[1] * shade, top[2] * shade, 1.0];
        const sBot = [bot[0] * shade, bot[1] * shade, bot[2] * shade, 1.0];
        positions.push(
          [x0, yLo, z0, 1], [x1, yLo, z1, 1], [x1, yHi, z1, 1],
          [x0, yLo, z0, 1], [x1, yHi, z1, 1], [x0, yHi, z0, 1],
        );
        colors.push(sBot, sBot, sCol, sBot, sCol, sCol);
        // Top cap fan triangle.
        positions.push([o.x, yHi, o.z, 1], [x0, yHi, z0, 1], [x1, yHi, z1, 1]);
        colors.push(top, sCol, sCol);
      }
      const f = new Form(
        { type: "triangle", positions, colors },
        { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
      );
      f.noFade = true;
      obstacleForms.push(f);

      // Top rim outline so the pillar reads in depth even in fog.
      const rimPos = [];
      const rimCol = [];
      const rim = [1, 0.85, 0.5, 0.85];
      for (let s = 0; s < segs; s++) {
        const a0 = (s / segs) * Math.PI * 2;
        const a1 = ((s + 1) / segs) * Math.PI * 2;
        rimPos.push(
          [o.x + Math.cos(a0) * o.r, yHi + 0.01, o.z + Math.sin(a0) * o.r, 1],
          [o.x + Math.cos(a1) * o.r, yHi + 0.01, o.z + Math.sin(a1) * o.r, 1],
        );
        rimCol.push(rim, rim);
      }
      const fe = new Form(
        { type: "line", positions: rimPos, colors: rimCol },
        { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
      );
      fe.noFade = true;
      obstacleEdges.push(fe);
    } else if (o.type === "box") {
      const yLo = o.yMin, yHi = o.yMax;
      const x0 = o.xMin, x1 = o.xMax;
      const z0 = o.zMin, z1 = o.zMax;
      const top = baseColor;
      const sideN = [baseColor[0] * 0.78, baseColor[1] * 0.78, baseColor[2] * 0.78, 1.0];
      const sideE = [baseColor[0] * 0.92, baseColor[1] * 0.92, baseColor[2] * 0.92, 1.0];
      const sideS = sideN;
      const sideW = sideE;
      const bot = [baseColor[0] * 0.5, baseColor[1] * 0.5, baseColor[2] * 0.5, 1.0];
      const positions = [];
      const colors = [];
      const quad = (a, b, c, d, col) => {
        positions.push(a, b, c, a, c, d);
        for (let k = 0; k < 6; k++) colors.push(col);
      };
      // Top
      quad(
        [x0, yHi, z0, 1], [x0, yHi, z1, 1], [x1, yHi, z1, 1], [x1, yHi, z0, 1],
        top,
      );
      // Bottom (visible if you peek under, also shields lava bleed)
      quad(
        [x0, yLo, z0, 1], [x1, yLo, z0, 1], [x1, yLo, z1, 1], [x0, yLo, z1, 1],
        bot,
      );
      // North (-Z)
      quad(
        [x0, yLo, z0, 1], [x0, yHi, z0, 1], [x1, yHi, z0, 1], [x1, yLo, z0, 1],
        sideN,
      );
      // South (+Z)
      quad(
        [x1, yLo, z1, 1], [x1, yHi, z1, 1], [x0, yHi, z1, 1], [x0, yLo, z1, 1],
        sideS,
      );
      // East (+X)
      quad(
        [x1, yLo, z0, 1], [x1, yHi, z0, 1], [x1, yHi, z1, 1], [x1, yLo, z1, 1],
        sideE,
      );
      // West (-X)
      quad(
        [x0, yLo, z1, 1], [x0, yHi, z1, 1], [x0, yHi, z0, 1], [x0, yLo, z0, 1],
        sideW,
      );
      const f = new Form(
        { type: "triangle", positions, colors },
        { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
      );
      f.noFade = true;
      obstacleForms.push(f);

      // Top edge outline — four lines around the wall's roof.
      const rim = [1, 0.85, 0.5, 0.85];
      const ey = yHi + 0.01;
      const fe = new Form(
        {
          type: "line",
          positions: [
            [x0, ey, z0, 1], [x1, ey, z0, 1],
            [x1, ey, z0, 1], [x1, ey, z1, 1],
            [x1, ey, z1, 1], [x0, ey, z1, 1],
            [x0, ey, z1, 1], [x0, ey, z0, 1],
          ],
          colors: [rim, rim, rim, rim, rim, rim, rim, rim],
        },
        { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
      );
      fe.noFade = true;
      obstacleEdges.push(fe);
    }
  }

  // 🫥 Feet reference — three shadow symbols swapped based on physics state,
  // plus a vertical plumb line from the ring up to the eye.
  const ringR = 0.35;
  const ringSegs = 16;

  const mkRing = (r, segs, color) => {
    const pos = [], col = [];
    for (let i = 0; i < segs; i++) {
      const a0 = (i / segs) * Math.PI * 2;
      const a1 = ((i + 1) / segs) * Math.PI * 2;
      pos.push(
        [Math.cos(a0) * r, 0, Math.sin(a0) * r, 1],
        [Math.cos(a1) * r, 0, Math.sin(a1) * r, 1],
      );
      col.push(color, color);
    }
    return { pos, col };
  };

  // Ground: ring + small cross inside.
  const g = mkRing(ringR, ringSegs, [1, 1, 1, 0.75]);
  const crossArm = ringR * 0.6;
  g.pos.push(
    [-crossArm, 0, 0, 1], [crossArm, 0, 0, 1],
    [0, 0, -crossArm, 1], [0, 0, crossArm, 1],
  );
  const gx = [1, 1, 1, 0.6];
  g.col.push(gx, gx, gx, gx);
  shadowGround = new FormRef(
    { type: "line", positions: g.pos, colors: g.col },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  shadowGround.noFade = true;

  // Air: diagonals only (bright X at feet so you can track your fall point).
  const xArm = ringR * 0.9;
  const airColor = [1, 0.9, 0.4, 0.85];
  shadowAir = new FormRef(
    {
      type: "line",
      positions: [
        [-xArm, 0, -xArm, 1], [xArm, 0, xArm, 1],
        [-xArm, 0, xArm, 1], [xArm, 0, -xArm, 1],
      ],
      colors: [airColor, airColor, airColor, airColor],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  shadowAir.noFade = true;

  // Crouch: smaller dense ring with a centre dot — reads as "compressed".
  const c = mkRing(ringR * 0.75, ringSegs, [1, 0.7, 0.3, 0.85]);
  const dotR = 0.06;
  const dot = [1, 0.85, 0.5, 0.95];
  c.pos.push(
    [-dotR, 0, 0, 1], [dotR, 0, 0, 1],
    [0, 0, -dotR, 1], [0, 0, dotR, 1],
  );
  c.col.push(dot, dot, dot, dot);
  shadowCrouch = new FormRef(
    { type: "line", positions: c.pos, colors: c.col },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  shadowCrouch.noFade = true;

  // Plumb line: unit-length along +Y at local origin. Scale.y each frame to
  // the current eye-above-ground distance. Fade alpha bottom→top so the line
  // doesn't poke into the pupil as a stark white segment.
  plumbLine = new Form(
    {
      type: "line",
      positions: [[0, 0, 0, 1], [0, 1, 0, 1]],
      colors: [[1, 1, 1, 0.45], [1, 1, 1, 0.0]],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: [1, 1, 1] },
  );
  plumbLine.noFade = true;

  // (Death floor is rebuilt each paint — see buildLavaFloor.)

  // 🦶 Feet — two stubby boxes (top + side edges as lines) anchored under the
  // player. Yaw rotates the whole form each frame so they face where you look.
  const footBox = (dx, ySpan, color) => {
    // Rectangle outline on top of the foot (local y = ySpan.max).
    const xL = dx - 0.08, xR = dx + 0.08;
    const zT = 0.35, zB = 0.05; // toe + heel in local forward (+Z)
    const yT = ySpan; // boot height
    const pts = [
      // top rectangle
      [xL, yT, zB, 1], [xR, yT, zB, 1],
      [xR, yT, zB, 1], [xR, yT, zT, 1],
      [xR, yT, zT, 1], [xL, yT, zT, 1],
      [xL, yT, zT, 1], [xL, yT, zB, 1],
      // four corners dropping to ground
      [xL, 0, zB, 1], [xL, yT, zB, 1],
      [xR, 0, zB, 1], [xR, yT, zB, 1],
      [xR, 0, zT, 1], [xR, yT, zT, 1],
      [xL, 0, zT, 1], [xL, yT, zT, 1],
    ];
    const cols = pts.map(() => color);
    return { pts, cols };
  };
  const footColor = [0.9, 0.9, 1.0, 0.85];
  const leftFoot = footBox(-0.18, 0.15, footColor);
  const rightFoot = footBox(0.18, 0.15, footColor);
  bodyFeet = new Form(
    {
      type: "line",
      positions: [...leftFoot.pts, ...rightFoot.pts],
      colors: [...leftFoot.cols, ...rightFoot.cols],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  bodyFeet.noFade = true;

  // 🤲 Arms — two angled line segments from shoulder to wrist, extending
  // forward and down from the eye. Color fades darker at the wrists so they
  // don't scream at the reader.
  const armLocal = (side) => {
    const sx = 0.28 * side;
    const shoulder = [sx, -0.35, 0.05, 1];
    const elbow = [sx * 0.9, -0.55, 0.45, 1];
    const wrist = [sx * 0.75, -0.65, 0.75, 1];
    const shoulderCol = [1, 1, 1, 0.7];
    const wristCol = [1, 0.9, 0.7, 0.9];
    return {
      pts: [shoulder, elbow, elbow, wrist],
      cols: [shoulderCol, wristCol, wristCol, wristCol],
    };
  };
  const leftArm = armLocal(-1);
  const rightArm = armLocal(+1);
  bodyArms = new Form(
    {
      type: "line",
      positions: [...leftArm.pts, ...rightArm.pts],
      colors: [...leftArm.cols, ...rightArm.cols],
    },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
  );
  bodyArms.noFade = true;

  // 🧍 Solid humanoid for 3rd-person. Same builder as remote players so the
  // local avatar matches what other tabs see. Only painted when zoomLevel > 0
  // (1P uses the bodyFeet/bodyArms wireframe helpers instead).
  const myColor = isGuestHandle(myHandle) ? specColor(myHandle) : handleColor(myHandle);
  myBody = buildRemoteBody(Form, myColor, isGuestHandle(myHandle));
}

function sim({ system, pen, screen }) {
  const doll = system?.fps?.doll;
  const cam = doll?.cam;
  if (!cam) return;

  // Advance the sim clock first so any logic below that wants elapsed
  // time sees the fresh value.
  simTime += 1 / SIM_HZ;

  // 🏟️ Multiplayer: compose usercmd, batch & flush at CMD_RATE.
  netSim(cam);

  // 🎆 Grenade physics — integrate position, bounce, detonate after fuse.
  // Runs at SIM_HZ so the trajectory is identical regardless of paint FPS.
  simGrenade(1 / SIM_HZ, doll, cam);
  if (activeExplosion) {
    activeExplosion.age += 1 / SIM_HZ;
    if (activeExplosion.age >= EXPLOSION_DURATION) activeExplosion = null;
  }

  // 📱 Update mobile button states (movement only — grenade/view fire from
  // act() so we don't get double triggers).
  if (mobileButtons && doll) {
    for (const [name, btnData] of Object.entries(mobileButtons)) {
      if (btnData.isGrenade || btnData.isView) continue;
      const isPressed = btnData.btn?.down ?? false;
      const wasPressed = mobileButtonStates[name] ?? false;

      if (isPressed && !wasPressed) {
        doll.setMovement(btnData.key, true);
        mobileButtonStates[name] = true;
      } else if (!isPressed && wasPressed) {
        doll.setMovement(btnData.key, false);
        mobileButtonStates[name] = false;
      }
    }
  }

  // 🎮 Right-stick → camera look. Integrate continuously in sim so the look
  // speed is framerate-independent. Skip while orbiting (right-mouse drag) so
  // the two camera-control schemes don't fight.
  if (gamepadState.connected && !orbiting) {
    const rx = gamepadState.axes[2] || 0;
    const ry = gamepadState.axes[3] || 0;
    if (rx !== 0 || ry !== 0) {
      const dt = 1 / SIM_HZ;
      cam.rotY += rx * GP_LOOK_DEG_PER_SEC * dt;
      cam.rotX = Math.max(
        -GP_PITCH_LIMIT,
        Math.min(GP_PITCH_LIMIT, cam.rotX + ry * GP_LOOK_DEG_PER_SEC * dt),
      );
    }
  }

  // Undo any orbit offset we applied last frame so it doesn't affect physics
  cam.x -= appliedOrbitOffset[0];
  cam.z -= appliedOrbitOffset[1];
  appliedOrbitOffset[0] = 0;
  appliedOrbitOffset[1] = 0;

  // Always use the *logical* player position (not the render camera) for
  // gameplay state. In 3P mode cam.x/z is offset behind the player.
  const phys = doll?.physics;
  const playerCamX = phys?.playerCamX ?? cam.x;
  const playerCamY = phys?.playerCamY ?? cam.y;
  const playerCamZ = phys?.playerCamZ ?? cam.z;

  // Horizontal speed only (ignore vertical so jump bursts don't spike the bar).
  const dx = playerCamX - prevX;
  const dz = playerCamZ - prevZ;
  const speed = Math.sqrt(dx * dx + dz * dz);
  speedSmoothed += (speed - speedSmoothed) * SPEED_SMOOTH;
  prevX = playerCamX; prevY = playerCamY; prevZ = playerCamZ;

  // Walk cycle phase advances with horizontal speed; fallback to slow drift.
  walkPhase += Math.max(0.002, speedSmoothed) * 8;

  // Player world position — cam stores negated world coords (see Camera
  // #transform). cam.y is the one exception and uses opposite inversion.
  const pWorldX = -playerCamX;
  const pWorldZ = -playerCamZ;
  const pWorldY = -playerCamY;

  // --- 💀 Death detection: hitting the red floor kills the player. ---
  if (playerAlive && pWorldY <= DEATH_FLOOR_Y + EYE_HEIGHT + 0.05) {
    playerAlive = false;
    deathTickAge = 0;
    doll.setFrozen?.(true);
    doll.clearHeldKeys?.();
  }
  if (!playerAlive) deathTickAge += 1;

  // --- Walked-tile trail: when the player's current tile changes, stamp it. ---
  // Apply the same hoverFlipMode flip to player position so walked tiles match raycast.
  const flipX = (hoverFlipMode & 1) !== 0;
  const flipZ = (hoverFlipMode & 2) !== 0;
  const playerX = flipX ? -pWorldX : pWorldX;
  const playerZ = flipZ ? -pWorldZ : pWorldZ;
  const curTile = tileAt(playerX, playerZ);
  if (curTile) {
    const key = tileKey(curTile.row, curTile.col);
    if (!prevPlayerTile || prevPlayerTile !== key) {
      walkedTiles.set(key, WALK_AGE_TICKS);
      prevPlayerTile = key;
    } else {
      // Also refresh the age while standing still so the glow lingers under you.
      walkedTiles.set(key, WALK_AGE_TICKS);
    }
  }
  // Age + prune.
  for (const [k, age] of walkedTiles) {
    const next = age - 1;
    if (next <= 0) walkedTiles.delete(k);
    else walkedTiles.set(k, next);
  }

  // --- Hover tile: fire a proper mouse ray from the pen's screen pixel into
  // the 3D scene, then intersect with the ground plane. When pen-locked the
  // pen stops updating so we fall back to screen-centre (the crosshair). ---
  //
  // Pipeline: screen (px,py) → NDC → camera-space ray dir (u,v,1) → rotate
  // by camera orientation → world ray → plane intersect.
  const rx = cam.rotX * Math.PI / 180;
  const ry = cam.rotY * Math.PI / 180;
  const sinRotX = Math.sin(rx), cosRotX = Math.cos(rx);
  const sinRotY = Math.sin(ry), cosRotY = Math.cos(ry);

  // Use pen position when available, otherwise centre of screen.
  const sw = screen?.width ?? 1, sh = screen?.height ?? 1;
  const mx = penLocked ? sw / 2 : (pen?.x ?? sw / 2);
  const my = penLocked ? sh / 2 : (pen?.y ?? sh / 2);
  const ndcX = (2 * mx) / sw - 1;
  const ndcY = 1 - (2 * my) / sh;
  const tanHalfFov = Math.tan((FOV * Math.PI / 180) / 2);
  const aspect = sw / sh;
  // Camera-space ray direction at this screen pixel (before rotating).
  const u = ndcX * aspect * tanHalfFov;
  const v = ndcY * tanHalfFov;
  // Rotate camera-space (u, v, 1) to world using (rotY(+rotY) ∘ rotX(-rotX)).
  const fx = u * cosRotY - v * sinRotY * sinRotX + sinRotY * cosRotX;
  const fy = v * cosRotX + sinRotX;
  const fz = -u * sinRotY - v * cosRotY * sinRotX + cosRotY * cosRotX;

  hoverTile = null;
  lastHitWorld = null;
  lastPenScreen = penLocked ? null : [mx, my];
  if (fy < -0.001) {
    // Ray origin + direction. The hoverFlipMode toggle lets us A/B all four
    // axis-sign combinations without editing source — press F to cycle.
    //   0 = no flip              (baseline derivation)
    //   1 = flip X (hit.x)
    //   2 = flip Z (hit.z)
    //   3 = flip both
    const flipX = (hoverFlipMode & 1) !== 0;
    const flipZ = (hoverFlipMode & 2) !== 0;
    const camWorldX = flipX ? cam.x  : -cam.x;
    const camWorldY = -cam.y;
    const camWorldZ = flipZ ? cam.z  : -cam.z;
    const fxAdj     = flipX ? -fx    : fx;
    const fzAdj     = flipZ ? -fz    : fz;
    const t = (GROUND_Y - camWorldY) / fy;
    if (t > 0 && t < 200) {
      const hitX = camWorldX + t * fxAdj;
      const hitZ = camWorldZ + t * fzAdj;
      hoverTile = tileAt(hitX, hitZ);
      lastHitWorld = [hitX, hitZ];
    }
  }

  // --- Anchor shadow / plumb / body at the logical player position (see top
  //     of sim). Reusing playerCam* captured above. ---
  const playerWorldY = pWorldY;
  const feetY = GROUND_Y + 0.01;

  const shadows = [shadowGround, shadowAir, shadowCrouch];
  for (const s of shadows) {
    if (!s) continue;
    s.position[0] = playerCamX;
    s.position[1] = feetY;
    s.position[2] = playerCamZ;
  }
  if (plumbLine) {
    plumbLine.position[0] = playerCamX;
    plumbLine.position[1] = feetY;
    plumbLine.position[2] = playerCamZ;
    plumbLine.scale[1] = Math.max(0, playerWorldY - GROUND_Y - 0.02);
  }

  // Update player facing direction (camera rotY when not orbiting)
  if (!orbiting) {
    playerFacing = cam.rotY;
  }

  if (bodyFeet) {
    // Feet move with player when airborne, stay planted when on ground
    const phys = system?.fps?.doll?.physics;
    let footY;
    if (playerAlive) {
      // When on ground, feet are at ground level
      // When in air, feet move with player (showing the jump height)
      footY = (phys?.onGround) ? GROUND_Y : playerWorldY;
    } else {
      footY = playerWorldY - EYE_HEIGHT;
    }
    bodyFeet.position[0] = playerCamX;
    bodyFeet.position[1] = footY;
    bodyFeet.position[2] = playerCamZ;
    bodyFeet.rotation[1] = playerFacing;
  }
  if (bodyArms) {
    const crouchDrop = (phys?.crouch ?? 0) * 0.2;
    const bob = Math.sin(walkPhase) * 0.03 * Math.min(1, speedSmoothed * 40);
    bodyArms.position[0] = playerCamX;
    bodyArms.position[1] = playerWorldY - crouchDrop + bob;
    bodyArms.position[2] = playerCamZ;
    bodyArms.rotation[1] = playerFacing;
  }
  if (myBody) {
    myBody.position[0] = playerCamX;
    myBody.position[1] = playerWorldY;
    myBody.position[2] = playerCamZ;
    myBody.rotation[1] = playerFacing;
  }

  // 🐛 Once-per-second debug dump. Paste this back into chat to diagnose.
  debugDumpTimer += 1;
  if (debugDumpTimer >= DEBUG_DUMP_INTERVAL) {
    debugDumpTimer = 0;
    const f2 = (n) => (typeof n === "number" ? n.toFixed(2) : String(n));
    const hoverStr = hoverTile
      ? `${hoverTile.row},${hoverTile.col}`
      : "none";
    const penStr = pen
      ? `pen=(${f2(pen.x)}, ${f2(pen.y)})`
      : "pen=null";
    const hitStr = lastHitWorld
      ? `(${f2(lastHitWorld[0])}, ${f2(lastHitWorld[1])})`
      : "none";
    // Debug snapshot (commented out due to framework message handling issue)
    // console.log("🏟️ arena snapshot:", { player, cam, phys, state, tiles, mouse, rayDir, speed });
  }

  // 🎥 Camera orbit effect (3P mode only): rotate camera around player by
  // modifying the XZ offset without changing cam.rotY (so player body stays put).
  // Orbit angle persists after release until left-click is pressed.

  if (zoomLevel > 0 && Math.abs(orbitAngle) > 0.01 && orbitDistance > 0) {
    const pCamX = phys?.playerCamX ?? cam.x;
    const pCamZ = phys?.playerCamZ ?? cam.z;
    // Use captured orbit distance (constant throughout orbit) to maintain radius
    const orbitRad = orbitAngle * Math.PI / 180;
    const newX = pCamX + orbitDistance * Math.sin(orbitRad);
    const newZ = pCamZ + orbitDistance * Math.cos(orbitRad);
    // Track the total change we're making so we can undo it next frame
    appliedOrbitOffset[0] = newX - cam.x;
    appliedOrbitOffset[1] = newZ - cam.z;
    cam.x = newX;
    cam.z = newZ;
  }
}

function paint({ wipe, ink, screen, write, box, system, pen, canvas, api, painting, paste }) {
  // 🎯 Switch cursor based on pen lock state (FPS mode vs UI mode)
  if (api?.cursor && penLocked) {
    // FPS mode (locked): yellow crosshair cursor
    const yellowCrosshair = `data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='32' height='32' viewBox='0 0 32 32'%3E%3Cline x1='16' y1='4' x2='16' y2='28' stroke='%23FFFF00' stroke-width='2'/%3E%3Cline x1='4' y1='16' x2='28' y2='16' stroke='%23FFFF00' stroke-width='2'/%3E%3C/svg%3E`;
    api.cursor(`url('${yellowCrosshair}') 16 16, auto`);
  }
  // UI mode (unlocked): keep default cyan cursor (don't override)

  // FPS calc — `now` is NOT a paint-API parameter (destructuring gave us
  // `undefined`, which made dt = NaN → fps = NaN → lava colors NaN → black
  // rendering). Pull from performance.now() directly instead.
  const now = performance.now();
  const dt = now - lastFrameTime;
  lastFrameTime = now;
  frameTimes.push(dt);
  if (frameTimes.length > 60) frameTimes.shift();
  const avgDt = frameTimes.reduce((a, b) => a + b, 0) / frameTimes.length;
  const fps = Math.round(1000 / avgDt);

  // 🔬 Per-frame perf sample — batch-sent to bios.mjs every ARENA_PERF_SEND_EVERY
  // frames where it's mirrored onto window.__arena_perfStats for external probes.
  arenaPerfRing[arenaPerfRingIdx] = {
    t: now,
    dt,
    reconDist: lastReconDist,
    reconKind: lastReconKind,
    pending: pendingCmds.length,
    ping,
    snapAge: netStats.lastSnapMs ? Date.now() - netStats.lastSnapMs : -1,
    reconCorrMs: reconCorrectionMs,
  };
  arenaPerfRingIdx = (arenaPerfRingIdx + 1) % ARENA_PERF_RING_SIZE;
  if (arenaPerfRingLen < ARENA_PERF_RING_SIZE) arenaPerfRingLen++;
  // Per-frame reset of the reconcile classifier so each sample reflects
  // *its own* frame, not the last non-skip reconcile that happened.
  lastReconKind = "none";

  if (arenaPerfSend) {
    arenaPerfPaintsSinceSend++;
    if (arenaPerfPaintsSinceSend >= ARENA_PERF_SEND_EVERY) {
      arenaPerfPaintsSinceSend = 0;
      const samples = new Array(arenaPerfRingLen);
      for (let i = 0; i < arenaPerfRingLen; i++) {
        const idx =
          (arenaPerfRingIdx - arenaPerfRingLen + i + ARENA_PERF_RING_SIZE) %
          ARENA_PERF_RING_SIZE;
        samples[i] = arenaPerfRing[idx];
      }
      arenaPerfSend({
        type: "perf:arena",
        content: {
          samples,
          meta: {
            size: arenaPerfRingLen,
            netSpectator,
            myHandle,
            ping,
            pendingCmds: pendingCmds.length,
            reconCorrTotal: reconCorrectionMs,
            snapsRx: netStats.snapsRx,
            cmdsTx: netStats.cmdsTx,
            collectedAt: now,
            position: reconCamRef
              ? { x: -reconCamRef.x, y: -reconCamRef.y, z: -reconCamRef.z }
              : null,
          },
        },
      });
    }
  }

  // ⚡ Adaptive quality — switch modes with hysteresis so we don't flip every
  // frame. Require several samples of sustained FPS before changing state.
  perfSamplesSinceSwitch += 1;
  if (perfSamplesSinceSwitch > 30) {
    if (avgDt > PERF_LOW_MS && !perfLowMode) {
      perfLowMode = true; perfMedMode = true; perfSamplesSinceSwitch = 0;
      lavaCache = null; // force rebuild at low tessellation
    } else if (avgDt > PERF_MED_MS && !perfMedMode) {
      perfMedMode = true; perfSamplesSinceSwitch = 0;
    } else if (avgDt < PERF_HIGH_MS && perfLowMode) {
      perfLowMode = false; perfSamplesSinceSwitch = 0;
      lavaCache = null; // rebuild each frame again
    } else if (avgDt < PERF_HIGH_MS && perfMedMode) {
      perfMedMode = false; perfSamplesSinceSwitch = 0;
    }
  }

  // --- Tile highlights: build a single transient Form containing one quad
  // per visible highlight (hover + walked trail). Drawn *after* the ground,
  // just above floor-Y, with additive-ish semi-transparent tint.
  const step = (GROUND_SIZE * 2) / GRID;
  const hiPos = [];
  const hiCol = [];
  const pushQuad = (row, col, color) => {
    const x0 = -GROUND_SIZE + col * step;
    const z0 = -GROUND_SIZE + row * step;
    const x1 = x0 + step;
    const z1 = z0 + step;
    const y = GROUND_Y + 0.015;
    hiPos.push(
      [x0, y, z0, 1], [x0, y, z1, 1], [x1, y, z1, 1],
      [x0, y, z0, 1], [x1, y, z1, 1], [x1, y, z0, 1],
    );
    // Per-tile dithering: a single deterministic noise value based on
    // (row,col) applied uniformly to all 6 vertices. Per-vertex random
    // noise caused shared-edge flickering between the two triangles (each
    // vertex got a fresh random value every frame, so the diagonal seam
    // shimmered). Hashing on tile coords means the noise is stable across
    // frames and consistent within a tile — breaks up solid fields without
    // introducing motion.
    const h = Math.sin(row * 12.9898 + col * 78.233) * 43758.5453;
    const noise = ((h - Math.floor(h)) - 0.5) * 0.08;
    const tinted = [
      Math.max(0, Math.min(1, color[0] + noise)),
      Math.max(0, Math.min(1, color[1] + noise)),
      Math.max(0, Math.min(1, color[2] + noise)),
      color[3],
    ];
    for (let i = 0; i < 6; i++) hiCol.push(tinted);
  };
  // Walked trail (bright yellow, fades with age — kept highly visible).
  // Skip the tile under the player's feet so the active standing tile reads as
  // "present" rather than already part of the trail.
  for (const [k, age] of walkedTiles) {
    if (k === prevPlayerTile) continue;
    const { row, col } = tileFromKey(k);
    const alpha = (age / WALK_AGE_TICKS) * 0.35; // More subtle fade
    pushQuad(row, col, [0.95, 0.85, 0.4, alpha]); // Muted warm gold
  }
  // Hover (muted cyan, subtle).
  if (hoverTile) {
    pushQuad(hoverTile.row, hoverTile.col, [0.5, 0.8, 0.9, 0.25]);
  }
  // Current standing tile (very subtle white glow).
  if (prevPlayerTile !== null) {
    const { row, col } = tileFromKey(prevPlayerTile);
    pushQuad(row, col, [0.95, 0.95, 0.95, 0.15]); // Barely visible
  }

  // Render scene — lava donut first (never under the main ground), then the
  // dark skirt that seals any tile-seam gaps, then the ground, its glowing
  // edge, tile highlights, feet shadow + body.
  wipe(45, 48, 55);
  // Drive lava animation from the sim clock, not paint time — decouples
  // the flow speed from render FPS so the look is identical on 30fps and
  // 120fps devices.
  const lava = buildLavaFloor(simTime);
  if (lava) ink(255).form(lava);
  if (platformBlock) ink(255).form(platformBlock);  // bottom and side faces
  if (groundSkirt) ink(255).form(groundSkirt);
  ink(255).form(groundPlane);
  // 🧱 Walls + pillars — solid bodies first, glowing rims on top so the
  // silhouette reads even when fog dims the body color.
  for (const f of obstacleForms) ink(255).form(f);
  for (const f of obstacleEdges) ink(255).form(f);
  if (hiPos.length > 0 && FormRef) {
    const hi = new FormRef(
      { type: "triangle", positions: hiPos, colors: hiCol },
      { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 },
    );
    hi.noFade = true;
    ink(255, 255, 255).form(hi);
  }

  // Pick the correct shadow symbol for the current physics state.
  const phys = system?.fps?.doll?.physics;
  let activeShadow = shadowGround;
  if (phys) {
    if (!phys.onGround) activeShadow = shadowAir;
    else if (phys.crouch > 0.5) activeShadow = shadowCrouch;
  }
  // Only draw ground-anchored shadow/plumb while on solid ground AND we're
  // playing (spectators are flying around without a body — no shadow).
  const onSolidGround = phys?.onGround;
  if (!netSpectator) {
    if (activeShadow && onSolidGround) ink(255, 255, 255).form(activeShadow);
    if (plumbLine && onSolidGround && plumbLine.scale[1] > 0.05) {
      ink(255, 255, 255).form(plumbLine);
    }
    // 1P: thin wireframes for hands/feet (your own POV references).
    // 3P: solid humanoid mesh — same builder as remote players, so what
    // other tabs see is what you see of yourself.
    if (zoomLevel > 0 && myBody) {
      ink(255).form(myBody);
    } else if (!perfLowMode) {
      if (bodyFeet) ink(255).form(bodyFeet);
      if (bodyArms) ink(255).form(bodyArms);
    }
  }

  // 🏟️ Remote players (interpolated from server snapshots, rendered ~100ms behind).
  paintRemotes(ink, undefined, FormRef);

  // 🎆 Local grenade + pill-shaped explosion. Rebuilt per-frame because color
  // alpha is baked into the geometry (matches the lava-floor approach above).
  if (myGrenade) {
    const gf = buildGrenadeForm(myGrenade);
    if (gf) ink(255).form(gf);
  }
  if (activeExplosion) {
    const ef = buildExplosionPill(activeExplosion);
    if (ef) ink(255).form(ef);
  }

  // --- HUD (top-right) ---
  // Layout, top to bottom:
  //   1. minimap (square showing live player positions)
  //   2. plain-English status: who, where, how the connection is doing
  //   3. local-perf info (fps + quality tier) at the bottom
  const font = "MatrixChunky8";
  const margin = 4;
  const lineH = 10;
  const rX = screen.width - margin; // right edge
  const dim = [140, 140, 150];      // shared label color
  const rightLabel = (txt, y, color) => {
    if (color) (Array.isArray(color) ? ink(...color) : ink(color));
    write(txt, { x: rX - txt.length * 4, y }, undefined, undefined, false, font);
  };
  // Right-align a row built from [color, text] segments so labels and values
  // can be tinted independently on the same line.
  const rightLabelMulti = (parts, y) => {
    let total = 0;
    for (const [, t] of parts) total += t.length;
    let x = rX - total * 4;
    for (const [c, t] of parts) {
      if (Array.isArray(c)) ink(...c); else ink(c);
      write(t, { x, y }, undefined, undefined, false, font);
      x += t.length * 4;
    }
  };

  // 1. Minimap — top-right, shows arena floor + everyone's position.
  const mapSize = 64;
  const mapX = rX - mapSize;
  const mapY = margin;
  ink(20, 22, 28, 200).box(mapX, mapY, mapSize, mapSize);
  ink(80, 82, 96).box(mapX, mapY, mapSize, 1);
  ink(80, 82, 96).box(mapX, mapY + mapSize - 1, mapSize, 1);
  ink(80, 82, 96).box(mapX, mapY, 1, mapSize);
  ink(80, 82, 96).box(mapX + mapSize - 1, mapY, 1, mapSize);
  {
    const { xMin, xMax, zMin, zMax } = ARENA_CFG.groundBounds;
    const wWorld = xMax - xMin;
    const hWorld = zMax - zMin;
    const inner = mapSize - 4;
    const sx = inner / wWorld;
    const sz = inner / hWorld;
    const ox = mapX + 2;
    const oy = mapY + 2;
    const project = (wx, wz) => ({
      px: Math.round(ox + (wx - xMin) * sx),
      py: Math.round(oy + (wz - zMin) * sz),
    });
    // Remote players (interpolated like the 3D bodies). Watchers (anon
    // guest_/swarm_) render as a small dim 1px dot; named players get a
    // bright 3×3 colored square.
    const tNow = renderTimeNow();
    for (const [handle, o] of Object.entries(others)) {
      const s = sampleOther(o, tNow);
      if (!s) continue;
      const { px, py } = project(s.x, s.z);
      if (isGuestHandle(handle)) {
        const [r, g, b] = specColor(handle);
        ink(r, g, b, 200).box(px, py, 2, 2);
      } else {
        const [r, g, b] = handleColor(handle);
        ink(r, g, b).box(px - 1, py - 1, 3, 3);
      }
    }
    // Self — white if you have a real handle, dim if anonymous, plus a
    // yellow yaw notch so the map is orientation-readable.
    const camRef = system?.fps?.doll?.cam;
    if (camRef) {
      const myX = -camRef.x;
      const myZ = -camRef.z;
      const { px, py } = project(myX, myZ);
      const meWatching = isGuestHandle(myHandle);
      if (meWatching) {
        const [r, g, b] = specColor(myHandle);
        ink(r, g, b).box(px - 1, py - 1, 3, 3);
      } else {
        ink(255, 255, 255).box(px - 2, py - 2, 5, 5);
      }
      const yawR = camRef.rotY * Math.PI / 180;
      const tipX = px + Math.round(Math.sin(yawR) * 6);
      const tipY = py + Math.round(Math.cos(yawR) * 6);
      ink(255, 220, 60).box(tipX - 1, tipY - 1, 2, 2);
    }
  }

  // 2. Plain-English status — what an average reader actually wants.
  let lineY = margin + mapSize + 4;
  const advance = () => { lineY += lineH; };

  // Players + watchers — split named handles from anon guests so the
  // count tells you who's *here* versus who's just looking around.
  let namedRemotes = 0;
  let watcherRemotes = 0;
  for (const h of Object.keys(others)) {
    if (isGuestHandle(h)) watcherRemotes++; else namedRemotes++;
  }
  const meIsWatcher = isGuestHandle(myHandle);
  const namedTotal = namedRemotes + (netSpectator || meIsWatcher ? 0 : 1);
  const watcherTotal = watcherRemotes + (netSpectator || meIsWatcher ? 1 : 0);
  rightLabelMulti(
    [[dim, "players "], [namedTotal > 0 ? [180, 230, 180] : [180, 180, 190], `${namedTotal}`]],
    lineY,
  );
  advance();
  if (watcherTotal > 0) {
    rightLabelMulti([[dim, "specs "], [[170, 200, 235], `${watcherTotal}`]], lineY);
    advance();
  }

  // 💖 HP bar — number tinted by remaining health (green → yellow → red).
  {
    const pct = Math.max(0, Math.min(1, playerHP / MAX_HP));
    const hpClr = pct > 0.5 ? [180, 230, 180]
                : pct > 0.25 ? [230, 200, 110]
                : [230, 110, 100];
    rightLabelMulti([[dim, "hp "], [hpClr, `${Math.ceil(playerHP)}`]], lineY);
    advance();
  }

  // Connection: transport + how stale snaps are + lag.
  {
    const wsOk = !!netServer;
    const udpOk = !!netUdp?.connected;
    const nowMs = Date.now();
    const snapAgeMs = netStats.lastSnapMs ? nowMs - netStats.lastSnapMs : Infinity;
    let label, color;
    if (!wsOk) { label = "offline"; color = [220, 90, 90]; }
    else if (snapAgeMs < 500) { label = udpOk ? "live" : "live (slow)"; color = [120, 230, 120]; }
    else if (snapAgeMs < 2000) { label = "lagging"; color = [230, 200, 80]; }
    else { label = "stalled"; color = [230, 140, 90]; }
    rightLabelMulti([[dim, "online "], [color, label]], lineY);
    advance();
    if (wsOk) {
      const lagClr = ping < 80 ? [180, 230, 180]
                   : ping < 200 ? [230, 220, 120]
                   : [230, 140, 120];
      rightLabelMulti([[dim, "lag "], [lagClr, `${ping}ms`]], lineY);
      advance();
    }
  }

  // Live state from physics: stance + camera mode + speed.
  if (phys) {
    const stance = !phys.onGround ? "jumping"
                 : phys.crouch > 0.5 ? "crouched"
                 : "standing";
    const stanceClr = !phys.onGround ? [240, 220, 100]
                    : phys.crouch > 0.5 ? [240, 170, 90]
                    : [180, 230, 180];
    rightLabelMulti([[dim, "stance "], [stanceClr, stance]], lineY);
    advance();

    const view = phys.thirdPerson ? `3rd · x${ZOOM_DISTANCES[zoomLevel]}` : "1st";
    const viewClr = phys.thirdPerson ? [220, 160, 240] : [120, 220, 230];
    rightLabelMulti([[dim, "view "], [viewClr, view]], lineY);
    advance();
  }

  {
    const upsNow = speedSmoothed * SIM_HZ;
    const barMaxUPSNow = RUN_SPEED * 1.2;
    const fillNow = Math.min(1, upsNow / barMaxUPSNow);
    const spR = fillNow > 0.5 ? Math.floor(255 * ((fillNow - 0.5) * 2)) : 0;
    const spG = fillNow < 0.5 ? 255 : Math.floor(255 * (1 - (fillNow - 0.5) * 2));
    rightLabelMulti([[dim, "speed "], [[spR, spG, 50], `${upsNow.toFixed(1)}`]], lineY);
    advance();
  }

  if (netSpectator) {
    rightLabel("watching", lineY, [255, 200, 80]);
    advance();
  }

  // 3. Quiet local-perf info at the bottom of the column.
  lineY += 4;
  const fpsColor = fps >= 30 ? [180, 230, 180]
                 : fps >= 15 ? [230, 220, 120]
                 : [230, 140, 120];
  rightLabelMulti([[dim, "fps "], [fpsColor, `${fps}`]], lineY);
  advance();
  {
    const tier = perfLowMode ? "low" : perfMedMode ? "medium" : "high";
    const tierClr = perfLowMode ? [220, 130, 130]
                  : perfMedMode ? [230, 200, 120]
                  : [180, 220, 180];
    rightLabelMulti([[dim, "quality "], [tierClr, tier]], lineY);
  }

  // 🪑 Full-width spectator overlay (only renders when we were kicked from
  // our own avatar by another tab). Center of screen, easy to notice.
  if (netSpectator) {
    const bannerY = Math.floor(screen.height / 2 - 8);
    const msg1 = "SPECTATING";
    const msg2 = `@${myHandle.replace(/^@/, "")} is controlled`;
    const msg3 = "from another tab";
    ink(0, 0, 0, 180).box(0, bannerY - 2, screen.width, 30);
    ink(255, 220, 100);
    write(msg1, { x: Math.floor(screen.width / 2 - msg1.length * 2), y: bannerY }, undefined, undefined, false, "MatrixChunky8");
    ink(200, 200, 200);
    write(msg2, { x: Math.floor(screen.width / 2 - msg2.length * 2), y: bannerY + 10 }, undefined, undefined, false, "MatrixChunky8");
    write(msg3, { x: Math.floor(screen.width / 2 - msg3.length * 2), y: bannerY + 18 }, undefined, undefined, false, "MatrixChunky8");
  }

  // 🎯 Debug crosshair at the current pen position (unlocked mode only) so
  // we can visually compare it against the highlighted hover tile.
  if (pen && !penLocked) {
    const px = Math.floor(pen.x);
    const py = Math.floor(pen.y);
    ink("magenta");
    box(px - 6, py, 13, 1);
    box(px, py - 6, 1, 13);
  }

  // 💢 Damage flash — quick red vignette on the most recent hit so the player
  // gets a hit-feedback pulse beyond the HP number ticking down.
  if (lastDamageMs > 0 && playerAlive) {
    const ageS = (Date.now() - lastDamageMs) / 1000;
    if (ageS < HP_FLASH_DURATION) {
      const k = 1 - ageS / HP_FLASH_DURATION;
      ink(220, 40, 30, Math.floor(k * 90));
      box(0, 0, screen.width, screen.height);
    }
  }

  // --- 💀 Death screen overlay (fade-in) ---
  if (!playerAlive) {
    const fade = Math.min(1, deathTickAge / 24); // ~0.2 s ramp
    ink(60, 0, 0, Math.floor(fade * 160));
    box(0, 0, screen.width, screen.height);

    const cx = screen.width / 2;
    const cy = screen.height / 2;
    ink(255, 80, 80, Math.floor(fade * 255));
    const died = "YOU DIED";
    // MatrixChunky8 ≈ 4px/char; centre roughly.
    write(died, { x: Math.floor(cx - died.length * 4), y: Math.floor(cy - 12) }, undefined, undefined, false, font);
    if (deathTickAge > 30) {
      ink(230, 230, 230, 220);
      const prompt = "TAP TO RESPAWN";
      write(prompt, { x: Math.floor(cx - prompt.length * 4), y: Math.floor(cy + 6) }, undefined, undefined, false, font);
    }
  }

  // 🎮 Controller minimap — appears top-left whenever a gamepad has been
  // detected this session. Schematic Xbox-style layout with live stick + button
  // state so the player can verify input is reaching the piece.
  if (gamepadState.connected) {
    const px = 4, py = 4, w = 90, h = 56;
    const btn = (i) => !!gamepadState.buttons[i];

    // Body
    ink(20, 25, 35, 210).box(px, py, w, h, "fill");
    ink(80, 100, 130, 230).box(px, py, w, h, "outline");

    // Triggers (LT=6, RT=7) — short bars across the top edge
    ink(btn(6) ? "yellow" : [70, 80, 100]).box(px + 4,        py + 2, 18, 3, "fill");
    ink(btn(7) ? "yellow" : [70, 80, 100]).box(px + w - 22,   py + 2, 18, 3, "fill");
    // Bumpers (LB=4, RB=5) — bars just below the triggers
    ink(btn(4) ? "white"  : [70, 80, 100]).box(px + 4,        py + 7, 18, 3, "fill");
    ink(btn(5) ? "white"  : [70, 80, 100]).box(px + w - 22,   py + 7, 18, 3, "fill");

    // Left stick well + dot (axes 0, 1; LS press = button 10)
    const lsx = px + 14, lsy = py + 26, lsr = 8;
    ink(40, 50, 70, 230).circle(lsx, lsy, lsr, true);
    ink(110, 130, 160).circle(lsx, lsy, lsr);
    const lsDx = (gamepadState.axes[0] || 0) * (lsr - 2);
    const lsDy = (gamepadState.axes[1] || 0) * (lsr - 2);
    ink(btn(10) ? "yellow" : "white").circle(lsx + lsDx, lsy + lsDy, 2, true);

    // Right stick (axes 2, 3; RS press = button 11)
    const rsx = px + w - 14, rsy = py + 26, rsr = 8;
    ink(40, 50, 70, 230).circle(rsx, rsy, rsr, true);
    ink(110, 130, 160).circle(rsx, rsy, rsr);
    const rsDx = (gamepadState.axes[2] || 0) * (rsr - 2);
    const rsDy = (gamepadState.axes[3] || 0) * (rsr - 2);
    ink(btn(11) ? "yellow" : "white").circle(rsx + rsDx, rsy + rsDy, 2, true);

    // D-pad cross (12=up, 13=down, 14=left, 15=right)
    const dpx = px + 30, dpy = py + 38;
    const dpOff = [70, 80, 100], dpOn = [255, 255, 255];
    ink(...(btn(12) ? dpOn : dpOff)).box(dpx,     dpy - 4, 4, 4, "fill");
    ink(...(btn(13) ? dpOn : dpOff)).box(dpx,     dpy + 4, 4, 4, "fill");
    ink(...(btn(14) ? dpOn : dpOff)).box(dpx - 4, dpy,     4, 4, "fill");
    ink(...(btn(15) ? dpOn : dpOff)).box(dpx + 4, dpy,     4, 4, "fill");

    // Face buttons diamond (Y top, X left, B right, A bottom — Xbox colors)
    const fbx = px + w - 30, fby = py + 38;
    const face = (cx, cy, color, on) => {
      ink(color[0], color[1], color[2], on ? 255 : 90).circle(cx, cy, 3, true);
      ink(255, 255, 255, on ? 255 : 80).circle(cx, cy, 3);
    };
    face(fbx,     fby + 5, [60, 200, 80],  btn(0)); // A — green
    face(fbx + 5, fby,     [220, 60, 60],  btn(1)); // B — red
    face(fbx - 5, fby,     [60, 130, 220], btn(2)); // X — blue
    face(fbx,     fby - 5, [240, 220, 60], btn(3)); // Y — yellow

    // Back (8) / Start (9) / Guide (16) — tiny center dots
    const cmx = px + w / 2, cmy = py + 26;
    ink(...(btn(8) ? [255, 255, 255] : [100, 110, 130])).box(cmx - 7, cmy, 3, 3, "fill");
    ink(...(btn(9) ? [255, 255, 255] : [100, 110, 130])).box(cmx + 4, cmy, 3, 3, "fill");
    if (btn(16)) ink("lime").circle(cmx + 1, cmy + 9, 2, true);

    // Controller id (truncated)
    const idShort = (gamepadState.id || "GAMEPAD").slice(0, 18).toUpperCase();
    ink(180, 200, 230, 200);
    write(idShort, { x: px + 3, y: py + h - 9 }, undefined, undefined, false, font);
  }

  // 📱 Draw mobile control buttons
  if (mobileButtons) {
    // Check if keyboard key is held for each button
    const keyHeld = {
      up: keyboardState.w || keyboardState.arrowup,
      down: keyboardState.s || keyboardState.arrowdown,
      left: keyboardState.a || keyboardState.arrowleft,
      right: keyboardState.d || keyboardState.arrowright,
      jump: keyboardState.space,
      crouch: keyboardState.shift,
    };

    for (const [name, btnData] of Object.entries(mobileButtons)) {
      const btn = btnData.btn;
      if (!btn) continue;

      // Draw button with pressed state (button click OR keyboard key held)
      const isPressed = btn.down || keyHeld[name];

      // Color-coded buttons: jump=green, crouch=orange, directionals=blue
      let bgColor, borderColor, textColor;
      if (btnData.color) {
        // Custom color for jump/crouch
        const [r, g, b] = btnData.color;
        bgColor = isPressed ? [r + 40, g + 40, b + 40, 255] : [r - 20, g - 20, b - 20, 200];
        borderColor = isPressed ? [255, 255, 255] : [200, 200, 200];
        textColor = isPressed ? [255, 255, 255] : [240, 240, 240];
      } else {
        // Default blue for directionals
        bgColor = isPressed ? [100, 140, 180, 220] : [60, 75, 95, 150];
        borderColor = isPressed ? [200, 220, 255] : [110, 130, 160];
        textColor = isPressed ? [255, 255, 255] : [180, 200, 230];
      }

      btn.paint((b) => {
        ink(...bgColor).box(b.box, "fill");
        ink(...borderColor).box(b.box, "outline");

        // Determine which buffer to render based on button state
        let bufferName = name;
        if (name === "jump" || name === "crouch" || name === "up" || name === "down" || name === "left" || name === "right" || name === "grenade") {
          // Jump/Crouch/Arrow/Grenade buttons have press-driven animation states
          bufferName = isPressed ? `${name}_active` : `${name}_normal`;
        } else if (name === "view") {
          // View toggle reflects the camera mode itself, not the momentary tap:
          // _active = 3rd-person, _normal = 1st-person.
          bufferName = zoomLevel > 0 ? "view_active" : "view_normal";
        }

        const buffer = buttonBuffers[bufferName];
        if (buffer) {
          if (!paste) {
            console.log(`⚠️  paste not available for button ${name}`);
          } else {
            // Render pixel graphics (centered in button)
            const centerX = b.box.x + (b.box.w - buffer.width) / 2;
            const centerY = b.box.y + (b.box.h - buffer.height) / 2;
            paste(buffer, centerX, centerY);
          }
        } else if (bufferName !== name) {
          console.log(`⚠️  No buffer found for ${name} (looking for ${bufferName})`);
        }

        // 🎮 When a controller is live, hint the corresponding Xbox face
        // button on jump (A — green) and crouch (B — red) so the player
        // can map "tap this on screen" → "press this on the pad."
        if (gamepadState.connected && (name === "jump" || name === "crouch")) {
          const isA = name === "jump";
          const color = isA ? [60, 200, 80] : [220, 60, 60];
          const r = 6;
          const gx = b.box.x + b.box.w - r - 3;
          const gy = b.box.y + r + 3;
          ink(color[0], color[1], color[2], 240).circle(gx, gy, r, true);
          ink(0, 0, 0, 220).circle(gx, gy, r);
          // MatrixChunky8 glyphs are ~4px wide × 7px tall.
          ink(255, 255, 255, 255);
          write(isA ? "A" : "B", { x: gx - 2, y: gy - 3 }, undefined, undefined, false, "MatrixChunky8");
        }
      });
    }
  }
}

function act({ event: e, penLock, system, screen, ui }) {
  if (e.is("pen:locked")) {
    penLocked = true;
    // Reset keyboard state to prevent stuck keys when cursor lock changes
    resetKeyboardState(system);
  }
  if (e.is("pen:unlocked")) {
    penLocked = false;
    // Reset keyboard state to prevent stuck keys when cursor lock changes
    resetKeyboardState(system);
  }

  // 📱 Reposition mobile buttons on screen resize/reframe
  if (e.is("reframed")) {
    if (screen && ui?.Button) {
      initMobileButtons(screen, ui);
    }
  }

  // 🎮 Gamepad — Standard Gamepad mapping (Xbox 360/One/Series, PS, etc.).
  // Events arrive as `gamepad:<idx>:button:<n>:push|release` and
  // `gamepad:<idx>:axis:<n>:move` from lib/gamepad.mjs. Parse the name once
  // and dispatch by kind so we don't have to enumerate every button.
  if (e.name && e.name.startsWith("gamepad:")) {
    const m = e.name.match(/^gamepad:(\d+):(button|axis):(\d+):(push|release|move)$/);
    if (m) {
      const gi = +m[1], kind = m[2], idx = +m[3], action = m[4];
      const doll = system?.fps?.doll;
      if (!gamepadState.connected) {
        gamepadState.connected = true;
        gamepadState.id = e.gamepadId || "Gamepad";
        gamepadState.index = gi;
      }
      if (kind === "button") {
        const pushed = action === "push";
        gamepadState.buttons[idx] = pushed;
        // A button: when dead, respawn; otherwise drive jump.
        if (idx === 0) {
          if (pushed && tryRespawn(system)) {
            // Consumed by respawn — don't also queue a jump on the new life.
          } else if (doll) {
            doll.setMovement("jump", pushed);
          }
        }
        if (idx === 1 && doll) doll.setMovement("crouch", pushed);   // B → crouch
        if (idx === 12 && pushed) {                                  // D-pad up → zoom in
          zoomLevel = Math.max(0, zoomLevel - 1);
          applyZoom(doll);
        }
        if (idx === 13 && pushed) {                                  // D-pad down → zoom out
          zoomLevel = Math.min(ZOOM_DISTANCES.length - 1, zoomLevel + 1);
          applyZoom(doll);
        }
        if (idx === 9 && pushed) tryRespawn(system);                  // Start → respawn
      } else if (kind === "axis") {
        gamepadState.axes[idx] = e.value;
        if ((idx === 0 || idx === 1) && doll) {
          // Left stick → discrete forward/back/left/right with deadzone, so
          // it slots into the same setMovement boolean flags the keyboard /
          // mobile buttons drive. Any move past GP_DEADZONE counts as held.
          const x = gamepadState.axes[0] || 0;
          const y = gamepadState.axes[1] || 0;
          const want = {
            left:    x < -GP_DEADZONE,
            right:   x >  GP_DEADZONE,
            forward: y < -GP_DEADZONE,
            back:    y >  GP_DEADZONE,
          };
          for (const k of ["left", "right", "forward", "back"]) {
            if (want[k] !== gamepadState.movement[k]) {
              doll.setMovement(k, want[k]);
              gamepadState.movement[k] = want[k];
            }
          }
        }
        // Axes 2/3 (right stick) are read in sim() for smooth look integration.
      }
    }
  }

  // ⌨️ Track keyboard state for visual feedback on buttons
  if (e.is("keyboard:down")) {
    if (e.key === "w") keyboardState.w = true;
    if (e.key === "a") keyboardState.a = true;
    if (e.key === "s") keyboardState.s = true;
    if (e.key === "d") keyboardState.d = true;
    if (e.key === "arrowup") keyboardState.arrowup = true;
    if (e.key === "arrowdown") keyboardState.arrowdown = true;
    if (e.key === "arrowleft") keyboardState.arrowleft = true;
    if (e.key === "arrowright") keyboardState.arrowright = true;
    if (e.key === " ") keyboardState.space = true;
    if (e.key === "Shift") keyboardState.shift = true;
  } else if (e.is("keyboard:up")) {
    if (e.key === "w") keyboardState.w = false;
    if (e.key === "a") keyboardState.a = false;
    if (e.key === "s") keyboardState.s = false;
    if (e.key === "d") keyboardState.d = false;
    if (e.key === "arrowup") keyboardState.arrowup = false;
    if (e.key === "arrowdown") keyboardState.arrowdown = false;
    if (e.key === "arrowleft") keyboardState.arrowleft = false;
    if (e.key === "arrowright") keyboardState.arrowright = false;
    if (e.key === " ") keyboardState.space = false;
    if (e.key === "Shift") keyboardState.shift = false;
  }

  // 📱 Trigger button input handling
  let mobileButtonHit = false;
  const doll = system?.fps?.doll;
  if (mobileButtons && doll) {
    for (const [name, btnData] of Object.entries(mobileButtons)) {
      btnData.btn?.act(e, {
        down: () => {
          mobileButtonHit = true;
          if (btnData.isGrenade) {
            // 💣 Mobile grenade trigger — single fire on tap; gated identically
            // to the click-to-fire path so we never double-spawn.
            if (playerAlive && !myGrenade && !netSpectator) {
              fireGrenade(doll.cam, doll);
            }
          } else if (btnData.isView) {
            // 🎥 First/third-person toggle — flip between 1P (level 0) and the
            // default shoulder-cam (level 2).
            zoomLevel = zoomLevel === 0 ? 2 : 0;
            applyZoom(doll);
          } else {
            doll.setMovement(btnData.key, true);
          }
        },
        push: () => {},
        cancel: () => {
          if (!btnData.isGrenade && !btnData.isView) {
            doll.setMovement(btnData.key, false);
          }
        },
      });
    }
    // Also flag as hit if the touch/lift lands inside any button box.
    if ((e.is("touch") || e.is("lift")) && !mobileButtonHit) {
      for (const btnData of Object.values(mobileButtons)) {
        if (btnData.btn?.box?.contains(e)) { mobileButtonHit = true; break; }
      }
    }
  }

  // If a mobile button was touched, don't let it fall through to camera/penLock.
  if (mobileButtonHit) return;

  // F cycles the hover axis-flip experiment (0 = no flip, 1 = X, 2 = Z, 3 = both).
  if (e.is("keyboard:down:f")) {
    hoverFlipMode = (hoverFlipMode + 1) & 3;
    console.log("🔄 hoverFlipMode →", hoverFlipMode,
      ["baseline", "flipX", "flipZ", "flipBoth"][hoverFlipMode]);
  }

  // 🎥 Middle-mouse toggles third-person (press once to enter, press again
  // to exit). Only trigger on touch so the release doesn't also flip.
  if (e.device === "mouse" && e.button === 1 && e.is("touch")) {
    zoomLevel = zoomLevel === 0 ? 2 : 0; // flip between 1P and default 3P
    applyZoom(system?.fps?.doll);
  }

  // 🔎 Scroll wheel — steps through discrete zoom levels (1P → closer 3P →
  // further 3P). dir < 0 = zoom in (toward 1P); dir > 0 = zoom out.
  if (e.is("wheel")) {
    if (e.dir < 0) zoomLevel = Math.max(0, zoomLevel - 1);
    else if (e.dir > 0) zoomLevel = Math.min(ZOOM_DISTANCES.length - 1, zoomLevel + 1);
    applyZoom(system?.fps?.doll);
  }

  // 🎥 Right-click drag to orbit camera around player (3P mode only).
  const cam = system?.fps?.doll?.cam;
  if (e.is("touch") && e.button === 2 && cam && zoomLevel > 0) {
    orbiting = true;
    playerFacing = cam.rotY; // lock player facing to current heading immediately
    baseRotY = cam.rotY;
    orbitSnapped = false; // start orbiting fresh
    // Capture current XZ distance to maintain constant orbit radius
    const phys = system?.fps?.doll?.physics;
    const pCamX = phys?.playerCamX ?? cam.x;
    const pCamZ = phys?.playerCamZ ?? cam.z;
    const dx = cam.x - pCamX;
    const dz = cam.z - pCamZ;
    orbitDistance = Math.sqrt(dx * dx + dz * dz);
  } else if (e.is("lift") && (e.button === 2 || orbiting)) {
    // Lift ends orbit (handle both proper e.button===2 and fallback for button detection issues)
    orbiting = false;
    orbitSnapped = true; // mark that orbit was released; wait for left-click to reset
  } else if (e.is("draw") && orbiting && zoomLevel > 0) {
    // Drag: accumulate orbit angle during right-click drag (3P mode only)
    // This handles cases where the touch API doesn't properly set e.button on drag events
    orbitAngle += e.delta.x * 0.4;
  }

  // Left-click resets orbit angle (only if orbit was previously snapped/released)
  // Also cancel any in-progress orbit if left-click happens
  if (e.is("touch") && e.button === 0) {
    if (orbiting) orbiting = false; // force-stop any active orbit on left-click
    if (orbitSnapped) {
      orbitAngle = 0;
      orbitSnapped = false;
    }
  }

  // While dead, any touch respawns; otherwise the first touch re-locks the pen.
  if (e.is("touch")) {
    if (tryRespawn(system)) return;
    // Don't re-lock on middle-click (1) or right-click (2) — reserved for camera control.
    if (!penLocked && e.button !== 1 && e.button !== 2) penLock();
    // 🎆 Click-to-fire grenade — only after pen is locked, only when alive,
    // and only one in-flight grenade at a time. The first click of a session
    // locks the pen (above) and does not fire; subsequent left-clicks launch.
    else if (e.button === 0 && playerAlive && !myGrenade && !netSpectator) {
      fireGrenade(system?.fps?.doll?.cam, system?.fps?.doll);
    }
  }

  // Space (jump key) also respawns when dead — parallels gamepad A.
  if (e.is("keyboard:down:space") && tryRespawn(system)) return;
}

// ⌨️ Helper: Reset all keyboard state to prevent stuck keys
function resetKeyboardState(system) {
  keyboardState.w = false;
  keyboardState.a = false;
  keyboardState.s = false;
  keyboardState.d = false;
  keyboardState.arrowup = false;
  keyboardState.arrowdown = false;
  keyboardState.arrowleft = false;
  keyboardState.arrowright = false;
  keyboardState.space = false;
  keyboardState.shift = false;

  // Also clear movement from doll to stop any in-progress movement
  const doll = system?.fps?.doll;
  if (doll) {
    doll.setMovement("forward", false);
    doll.setMovement("back", false);
    doll.setMovement("left", false);
    doll.setMovement("right", false);
    doll.setMovement("jump", false);
    doll.setMovement("crouch", false);
  }
}

// 📱 Helper: Initialize or reposition mobile buttons responsively
function initMobileButtons(screen, ui) {
  const btnSize = 28;
  const btnSizeWide = 56;  // wider action buttons
  const gap = 4;  // gap between buttons (no border overlap)
  const padding = 6;
  const bottomMargin = 6;

  // Movement buttons (bottom-left): D-pad style, compact layout
  const moveX = padding;
  const moveY = screen.height - (btnSize * 3 + gap * 2 + bottomMargin);

  // Action buttons (bottom-right): wider layout, three rows: view / jump / crouch.
  const actionX = screen.width - btnSizeWide - padding;
  const actionY = screen.height - (btnSize * 3 + gap * 2 + bottomMargin);

  // D-pad layout (center cell is the grenade trigger):
  //     ↑
  //  ← 💣 →
  //     ↓
  mobileButtons = {
    up: { btn: new ui.Button(moveX + btnSize + gap, moveY, btnSize, btnSize), key: "forward", label: "↑", isArrow: true },
    down: { btn: new ui.Button(moveX + btnSize + gap, moveY + (btnSize + gap) * 2, btnSize, btnSize), key: "back", label: "↓", isArrow: true },
    left: { btn: new ui.Button(moveX, moveY + btnSize + gap, btnSize, btnSize), key: "left", label: "←", isArrow: true },
    right: { btn: new ui.Button(moveX + (btnSize + gap) * 2, moveY + btnSize + gap, btnSize, btnSize), key: "right", label: "→", isArrow: true },
    grenade: { btn: new ui.Button(moveX + btnSize + gap, moveY + btnSize + gap, btnSize, btnSize), key: "grenade", label: "💣", color: [200, 60, 50], isGrenade: true },
    view: { btn: new ui.Button(actionX, actionY, btnSizeWide, btnSize), key: "view", label: "VIEW", color: [150, 110, 200], isView: true },
    jump: { btn: new ui.Button(actionX, actionY + btnSize + gap, btnSizeWide, btnSize), key: "jump", label: "JUMP", color: [50, 200, 100] },
    crouch: { btn: new ui.Button(actionX, actionY + (btnSize + gap) * 2, btnSizeWide, btnSize), key: "crouch", label: "CROUCH", color: [220, 150, 40] },
  };
}

// 🏟️ Lifecycle: tell the server we're leaving so our player record is
// deleted immediately instead of waiting for the 30s stale sweep.
function leave() {
  try { netServer?.send("arena:bye", { handle: myHandle }); } catch {}
  myGrenade = null;
  activeExplosion = null;
  knockbackGraceUntil = 0;
}

export const system = "fps";
export { boot, sim, paint, act, leave };
