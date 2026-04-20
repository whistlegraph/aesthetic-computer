# arena.mjs — Multiplayer Plan

**Goal:** put more than one figure on the arena platform. Preserve the current
single-player feel (Quake-style movement, lava pit, shadow, third-person).
Add presence so other logged-in users appear as walking stick figures.

---

## 1. The networking model you already have

Two transports, one session server (`session-server/session.mjs`):

| Transport | Where defined | Library | Delivery | Used for |
|---|---|---|---|---|
| **WebSocket** | `net.socket(cb)` in disks | `ws` package, `wss` in session.mjs | reliable, ordered | join/leave, roster, chat, scoring, invites |
| **UDP** (WebRTC DataChannel) | `net.udp(cb)` in disks | `@geckos.io/server`, `io` in session.mjs | low-latency, may drop | position/velocity sync, audio data, real-time input |

Both channels carry `{ type, content }` frames (content is a JSON string or
object). On the server a client is a single logical identity with **two**
connection IDs — one in `connections[id]` (WS) and one in `udpChannels[id]`
(UDP). They are linked by the client's `handle`, glued via the `udp:identity`
message that every UDP channel sends right after it opens. Lookup helper:
`resolveUdpForHandle(handle)` scans `clients` to find the matching UDP channel.

### Three relay patterns — pick one per message type

**A. `everyone(str)` — WS broadcast to all**
Used for `roster`, `world:*`, `build:*`, `reload`. Simple broadcast.

**B. `others(str)` / `channel.broadcast.emit(evt, data)` — relay all-except-sender**
Used for `1v1:move`, `squash:move`. Session server is a dumb relay: it does no
state management, no validation. Each client is the source of truth for its
own avatar; others see a lagged copy. This is the pattern to copy for arena
presence.

**C. Server-authoritative (see `duel-manager.mjs`)**
Server owns state. Clients send inputs only (`duel:input { seq, targetX,
targetY }`). Server runs a 60 Hz tick, broadcasts snapshots at ~20 Hz.
Clients predict locally, reconcile when ack'd input `seq` comes back in
`lastInputSeq[myHandle]`. This is what dumduel uses.

### How `duel-manager.mjs` hooks in — the canonical manager pattern

```js
// session.mjs
import { DuelManager } from "./duel-manager.mjs";
const duelManager = new DuelManager();
duelManager.setSendFunctions({ sendUDP, sendWS, broadcastWS, resolveUdpForHandle });

// WS message dispatch:
if (msg.type === "duel:join")  duelManager.playerJoin(handle, wsId);
if (msg.type === "duel:input") duelManager.receiveInput(handle, parsed);

// UDP channel handler:
channel.on("duel:input", (data) => duelManager.receiveInput(handle, parsed));
```

The Manager object is the only place game state lives. `session.mjs` just
shuttles frames. This is the model to extend if/when arena needs
server-authoritative combat.

### Identity — how handle becomes the key

- On WS connect, client sends `{ type: "login" }` (see `chat-manager.mjs`) or
  piece-specific `*:join { handle }`. The session records
  `clients[id].handle = handle`.
- On UDP connect, `geckos.io` issues a channel id. The client sends
  `udp:identity { handle, user }` as its first message; server writes
  `clients[channelId].handle`, and — if a DuelManager/arena manager wants the
  UDP channel — calls `resolveUdpChannel(handle, channelId)`.
- `guest_xxxx` handles are first-class but typically demoted (dumduel puts
  them in `spectators`).

---

## 2. What arena.mjs looks like today

- Single-player. Zero networking. Zero imports from `net`.
- Uses `export const system = "fps"` → framework provides
  `system.fps.doll` (a `Camdoll` from `lib/cam-doll.mjs`) which owns camera,
  physics, crouch/jump, and groundY clamping.
- Player position is `phys.playerCamX/Y/Z` (negated world coords — cam stores
  `-worldX`). `playerFacing` = `cam.rotY` except while orbiting.
- Rendering body parts uses two 3D `Form` objects — `bodyFeet` and `bodyArms`
  — positioned/rotated each sim tick at the local player. The ground
  (`groundPlane`), skirt, platform, lava, and shadow are scene geometry.
- Death = Y fell into the pit. Respawn = `doll.respawn(0,0)`. No concept of
  other players' life state.

So to become multi-user, arena needs:

1. **A network identity** — resolve handle in `boot`, open WS + UDP.
2. **An `others` map** of remote players — pos, rotY (facing), jumping/crouch
   anim state, alive flag.
3. **Outgoing position sync** — throttled UDP send of own state.
4. **Incoming state handling** — buffered + lerped like dumduel's opponent.
5. **Per-remote render** — clone the `bodyFeet`/`bodyArms` Form recipe once
   per other; reposition each paint call.
6. **Presence lifecycle** — `arena:join` on connect, `arena:leave` on
   disconnect, roster updates.

---

## 3. Recommended approach — start with relay-only ("Tier 1")

Mirror `squash.mjs` / `1v1.mjs`: session server is a dumb relay. Each client
owns its own avatar. Keep the single-player `cam-doll` physics intact —
authority for "where am I" stays local. We just paint other people.

This lands the visible win (more figures on the board) with the smallest
diff and no new server state. If we later want shooting, hit detection, or
a death-pit-score shared across players, we add an `ArenaManager` alongside
`DuelManager` (Tier 2 below).

### Message shape (Tier 1)

```js
// Client → server, UDP, ~30Hz
{
  type: "arena:move",
  content: {
    handle,
    x, y, z,           // world coords
    rotY,              // facing (degrees)
    vy,                // vertical velocity (for jump animation on remotes)
    onGround,          // bool
    crouch,            // 0..1
    alive,             // bool
  }
}

// Client → server, WS
{ type: "arena:join",  content: { handle } }
{ type: "arena:leave", content: { handle } }
{ type: "arena:respawn", content: { handle } }   // reliable event
{ type: "arena:died",    content: { handle } }   // reliable event
```

Server-side relay (no state held):

```js
// In session.mjs WS dispatch, next to 1v1:move / squash:move
if (msg.type === "arena:move")   { others(JSON.stringify(msg)); return; } // WS fallback
if (msg.type === "arena:join" || msg.type === "arena:leave"
 || msg.type === "arena:respawn" || msg.type === "arena:died") {
  everyone(JSON.stringify(msg)); return;
}

// In io.onConnection geckos handler, next to squash:move
channel.on("arena:move", (data) => {
  if (channel.webrtcConnection.state === "open") {
    try { channel.broadcast.emit("arena:move", data); } catch {}
  }
});
```

### Client changes (`disks/arena.mjs`)

1. **Boot signature**
   ```js
   function boot({ Form, penLock, system, screen, ui, api, painting,
                   net: { socket, udp }, handle }) { ... }
   ```

2. **New module state**
   ```js
   let myHandle = "guest";
   let server, udpChannel;
   let others = {};          // { [handle]: { x,y,z, rotY, vy, onGround, crouch, alive,
                             //   serverX, serverY, serverZ,   // latest from net
                             //   displayX, displayY, displayZ, // lerped
                             //   bodyFeet, bodyArms            // 3D Forms built lazily
                             // } }
   let lastUdpSend = 0;
   const UDP_SEND_INTERVAL = 4; // sim ticks, = 30Hz at 120Hz sim
   const LERP_SPEED = 0.25;
   ```

3. **Connect in boot**
   ```js
   myHandle = handle?.() || "guest_" + Math.floor(Math.random()*9999);

   udpChannel = udp((type, content) => {
     if (type === "arena:move") {
       const d = typeof content === "string" ? JSON.parse(content) : content;
       if (d.handle === myHandle) return;
       upsertOther(d);
     }
   });

   server = socket((id, type, content) => {
     if (type.startsWith("connected")) {
       server.send("arena:join", { handle: myHandle });
       return;
     }
     const msg = typeof content === "string" ? JSON.parse(content) : content;
     if (type === "arena:move")    upsertOther(msg); // WS fallback
     if (type === "arena:join")    upsertOther({ handle: msg.handle, alive: true });
     if (type === "arena:leave")   delete others[msg.handle];
     if (type === "arena:died")    { if (others[msg.handle]) others[msg.handle].alive = false; }
     if (type === "arena:respawn") { if (others[msg.handle]) others[msg.handle].alive = true; }
   });
   ```

4. **Outgoing state in `sim`** — after the doll physics update, guarded by
   `simTime % (UDP_SEND_INTERVAL/SIM_HZ)`:
   ```js
   lastUdpSend++;
   if (lastUdpSend >= UDP_SEND_INTERVAL) {
     lastUdpSend = 0;
     const payload = {
       handle: myHandle,
       x: -playerCamX, y: -playerCamY, z: -playerCamZ,   // to world
       rotY: playerFacing, vy: phys?.vy ?? 0,
       onGround: phys?.onGround ?? true,
       crouch: phys?.crouch ?? 0,
       alive: playerAlive,
     };
     if (udpChannel?.connected) udpChannel.send("arena:move", payload);
     else server?.send("arena:move", payload);
   }
   ```

5. **Interpolation in `sim`** — for every other, lerp display toward server:
   ```js
   for (const o of Object.values(others)) {
     o.displayX += (o.serverX - o.displayX) * LERP_SPEED;
     o.displayY += (o.serverY - o.displayY) * LERP_SPEED;
     o.displayZ += (o.serverZ - o.displayZ) * LERP_SPEED;
   }
   ```

6. **Rendering in `paint`** — build + reposition a `bodyFeet`/`bodyArms`
   pair per other. Simplest: reuse the same form-building code from boot,
   factored into `makeBody()` that returns `{ feet, arms }`. On first
   snapshot for a new handle, build it; store on the `others[handle]`
   record; each frame update `.position` and `.rotation[1]`. Add to
   `paint`'s Form render list the same way local body parts are.

   Optional polish: fade new joiners in over ~0.5s; pulse on death.

7. **Teardown** — there's no `leave()` currently; add one to emit
   `arena:leave` before the piece unmounts.

### Server changes

Two small patches to `session-server/session.mjs`:

- **WS dispatch** near the `1v1:move` / `squash:move` / `duel:*` section
  (~line 2815): add the four `arena:*` handlers above.
- **UDP handler** near `channel.on("squash:move", ...)` (~line 3607): add
  the `arena:move` broadcast.

No new file, no new manager, no session-server state.

---

## 4. Open design questions

1. **Scope of presence.** Does "arena" mean one shared room across the whole
   session server, or one-per-spawn? Dumduel / squash assume one global room.
   Simplest to start the same way; partition later with `arena:<roomId>`
   message prefixes if needed.
2. **Guests.** Include `guest_xxx` as first-class figures or as ghosts /
   hide them? Dumduel demotes to spectators — that feels wrong for a
   platform game. Recommend: include, but render with 50% alpha + italic
   handle label.
3. **Remote body rendering cost.** Each remote = two Forms. With N players
   that's 2N forms plus shadows and labels. Acceptable up to ~10 remotes;
   above that we'd want a single batched form.
4. **Handle labels over heads.** Need to project world → screen (FPS camera).
   arena already has the inverse ray in `sim` for `hoverTile`; reuse the
   math to draw 2D text above each remote.
5. **Death pit as shared hazard?** Today each client decides its own death
   from local Y. For presence-only that's fine — the `arena:died` message
   is just cosmetic. If we want kills ("push someone into the pit") that
   becomes authority-contested → promote to Tier 2.

---

## 5. Tier 2 — Quake 3-caliber netcode on WS + geckos.io

Target: competitive FPS feel (pro-mode quality) on our existing transports.
No new infra — we **do not** add a raw UDP socket, a packet-level protocol,
or a second server. Everything runs through `socket()` (reliable) and
`udp()` (geckos.io WebRTC DataChannel), plus a new `ArenaManager` class
sitting next to `DuelManager` in `session-server/`.

### 5.1 Q3 concepts → what they map to for us

Q3 invented this pattern; everything below is a direct adaptation. Sources:
[Fabien Sanglard's Q3 network review](https://fabiensanglard.net/quake3/network.php),
[jfedor Q3 wire format](https://www.jfedor.org/quake3/),
[id's `sv_snapshot.c`](https://github.com/id-Software/Quake-III-Arena/blob/master/code/server/sv_snapshot.c),
[SnapNet on snapshot interpolation](https://snapnet.dev/blog/netcode-architectures-part-3-snapshot-interpolation/).

| Q3 concept | Q3 implementation | AC adaptation |
|---|---|---|
| **Transport** | one UDP socket; reliable cmds multiplexed via seq+ack inside UDP | **split**: WS = reliable channel, geckos.io UDP = `cmd`/`snap` |
| **Packet MTU** | fragment at 1400 bytes to avoid router splits | geckos.io handles fragmentation; we still size snaps conservatively (<1 KB target, hard cap 8 KB) |
| **`clc_move` (input)** | ≤8 `usercmd_t` per packet, bit-packed with 1-bit "changed?" per field, timestamps | `arena:cmd` UDP frame: `{ seq, ack, cmds: [last N usercmds], ms }` — JSON for M1, bitpack later |
| **`usercmd_t`** | `{ serverTime, angles[3], forwardmove, rightmove, upmove, buttons, weapon }` | `{ ms, yaw, pitch, fwd, right, up, buttons }` — `buttons` = bitmask (jump\|crouch\|shoot\|dash) |
| **Command backup** | `cl_packetdup` — every packet carries the last N cmds so one drop ≠ lost input | Start at **N=3**, each cmd ~20 bytes, fine under the MTU |
| **`svc_snapshot`** | delta-compressed vs a previously-acked snap; server keeps 32-snap ring per client | `arena:snap` with `{ messageNum, deltaNum, tick, serverMs, entities }`; server keeps 32-snap ring per client |
| **Delta compression** | bit-per-field "changed?" marker, terminate at last-changed index | identical algorithm; field table built once from an entity schema object |
| **Snap ack** | every outgoing client packet includes `serverMessageSequence` = last snap seen | every `arena:cmd` includes `ack: lastSeenMessageNum` |
| **`sv_fps` / `sv_snaps`** | server tick 20–40 Hz (pro: 40–125) | **tickRate = 60 Hz, snapRate = 30 Hz** to start; per-client override possible later |
| **`cl_snaps` / `cl_maxpackets`** | client requests 20–40 snaps, sends 30–125 cmds/s | cmd rate **60 Hz**, snap rate decided by server |
| **Client prediction (`pmove`)** | identical movement code client+server; client replays unacked cmds on latest authoritative state each frame | extract pmove into `shared/pmove.mjs` so disk + session-server run byte-identical simulation |
| **Interpolation (`cl_interp`)** | render remote entities ~100 ms in the past, between two known snaps | `INTERP_DELAY_MS = 100`; never extrapolate |
| **Lag compensation (Unlagged)** | on hitscan, server rewinds other players by `ping + interp` to the attacker's view time | keep 500 ms position history per player; on shoot, rewind & raycast |
| **PVS culling** | only send entities in the player's potentially-visible set | arena is small (±14 units); skip PVS. Send all players always. |
| **Reliable cmds** | chat, disconnect, config strings multiplexed into the UDP stream with per-cmd seq | send over **WS** instead: `arena:hello`, `arena:bye`, `arena:kill`, `arena:config`, `arena:chat` |

### 5.2 Why the split transport is actually *better* for us than Q3's single-socket design

Q3 had to invent in-band reliable command acknowledgment because it only had
UDP. We already have an ordered reliable channel (WS). That lets us:

- Delete the reliable-command retransmit loop entirely.
- Keep `arena:cmd` / `arena:snap` purely unreliable and delete-safe.
- Avoid coupling snapshot loss to chat loss.

The tradeoff: TCP head-of-line blocking on WS could delay a `kill` event
by a few hundred ms during packet loss. That's fine for lifecycle/chat;
it would not be fine for position data, which is why position stays on UDP.

### 5.3 Wire formats

```js
// Client → Server, UDP, ~60Hz
{ type: "arena:cmd",
  content: {
    seq,              // monotonic client cmd seq
    ack,              // last snap messageNum we saw (0 if none)
    handle,           // identity (geckos channel is already bound but include for safety)
    cmds: [           // last N=3 usercmds, oldest first
      { ms, yaw, pitch, fwd, right, up, buttons }, ...
    ]
  }
}

// Server → Client, UDP, 30Hz (per-client)
{ type: "arena:snap",
  content: {
    messageNum,       // this client's monotonic snap seq
    deltaNum,         // messageNum - deltaNum ago is the base; 0 = full snap
    tick,             // server sim tick
    serverMs,         // wall-clock ms, for client->server time offset estimation
    ackCmdSeq,        // highest client cmd seq the server has processed
    players: [        // delta-encoded; only fields that changed vs base
      { h, x, y, z, yaw, pitch, vy, ground, crouch, alive, health, ... }
    ],
    events: [         // fire-and-forget one-shots since last snap (kill, respawn, spawn)
      { t: "kill", by: "a", of: "b", ms }, ...
    ]
  }
}

// WS — reliable sideband
{ type: "arena:hello",   content: { handle } }           // client on connect
{ type: "arena:welcome", content: { yourId, serverConfig, initialSnap } }
{ type: "arena:bye",     content: { handle } }
{ type: "arena:kill",    content: { by, of, ms } }       // redundant with snap events but guaranteed
{ type: "arena:chat",    content: { handle, text } }
```

### 5.4 `session-server/arena-manager.mjs`

```
class ArenaManager {
  players            // Map<handle, PlayerRecord>
  tick, serverMs
  tickInterval

  // Per-client state needed for delta compression:
  //   record.snapHistory : ring buffer [32] of { messageNum, state }
  //   record.lastAckMessageNum : highest snap the client confirmed receiving
  //   record.nextMessageNum    : monotonic counter for this client's snap stream
  //   record.posHistory        : ring buffer [~30] of {ms, x, y, z} for lag comp

  setSendFunctions({ sendUDP, sendWS, broadcastWS, resolveUdpForHandle })

  playerJoin(handle, wsId)         // WS arena:hello
  playerLeave(handle)
  receiveCmd(handle, frame)        // UDP arena:cmd  → applyUsercmd per cmd, update lastAckMessageNum
  receiveShoot(handle, frame)      // optional — lag-comp hit test

  serverTick()                     // 60Hz: pmove each player w/ latest cmd, advance world
  buildSnapshotFor(handle)         // compose state, delta-encode vs snapHistory[lastAckMessageNum]
  broadcastSnapshots()             // 30Hz: for each handle → sendUDP(chan, "arena:snap", delta)
}
```

Key implementation notes from the Q3 source:

1. **Per-client snap history ring is mandatory.** The delta base must be a
   snap we *know* the client has. Client signals this via `ack` in every
   cmd packet. Ring size 32 gives ~1 s of tolerance at 30 Hz before we're
   forced to send a full snap.
2. **Delta encoding uses a schema.** Q3 uses a `netField_t[]` table with
   `name, offset, bits`. We build the equivalent once as an array of field
   specs over a `PlayerState` object. Encode loop: for each field, compare
   to base, emit 1 bit; if changed, emit the value. Early-out after the
   last-changed index (Q3's big win).
3. **usercmd replay uses time deltas, not absolute time.** Server applies
   cmd *i* by computing `dt = cmd[i].ms - cmd[i-1].ms` (clamped to keep
   cheaters from moving faster). First cmd after join uses wall clock.
4. **`ackCmdSeq` in snaps** lets the client drop cmds from its pending
   queue (same as dumduel's `lastInputSeq`, just per-client per-packet).
5. **No need for Q3's `qport`** — geckos.io gives us a stable channel id.

### 5.5 Client (disk-side) rework

#### 5.5.1 Factor out pmove

Today `cam-doll.mjs` owns movement. For prediction parity we need a
**pure function** both sides can call:

```
shared/pmove.mjs
  export function pmove(state, cmd, dt) { return newState; }
```

`state` = `{ x, y, z, vx, vy, vz, yaw, pitch, onGround, crouchT }`.
`cmd` = the usercmd fields. Same code runs in browser (arena.mjs) and
Node (arena-manager.mjs). Keep it dependency-free.

`cam-doll` then becomes a thin wrapper: "read input → produce usercmd →
feed pmove → write back to cam". This is the Carmack pattern: one
function is the source of truth, everything else is input/output plumbing.

#### 5.5.2 Client-side prediction & reconciliation

```js
// On every sim tick locally:
const cmd = makeUsercmd(input);
pendingCmds.push({ seq: ++cmdSeq, cmd });
playerState = pmove(playerState, cmd, 1/SIM_HZ);
// render playerState

// On snap arrival:
function onSnap(snap) {
  // Advance our "authoritative" state to what the server says
  authoritativeState = applySnapDelta(authoritativeState, snap);
  // Drop acked cmds
  pendingCmds = pendingCmds.filter(c => c.seq > snap.ackCmdSeq);
  // Re-run the unacked ones on top of the server's state
  let replayed = authoritativeState;
  for (const c of pendingCmds) replayed = pmove(replayed, c.cmd, 1/SIM_HZ);
  // If replayed is far from our displayed state, smooth over ~100ms instead of snapping
  playerState = smoothCorrect(playerState, replayed);
}
```

#### 5.5.3 Interpolation for remote players

Maintain a per-remote snapshot buffer `[{ ms, state }, ...]`. Render at
`now - INTERP_DELAY_MS` (100 ms). Find the two buffered states bracketing
that time, lerp between them. When the buffer empties (lost packets,
server starved), **freeze** the remote — do not extrapolate. Carmack's
rule: "you only ever show positions the entity actually had."

```js
function renderRemote(handle, now) {
  const renderTime = now - INTERP_DELAY_MS;
  const buf = remotes[handle].buffer;
  // Find i such that buf[i].ms <= renderTime <= buf[i+1].ms
  // If not found, clamp to oldest/newest (freeze)
  // Else lerp between buf[i] and buf[i+1]
}
```

#### 5.5.4 Clock sync

For interp to work, client and server need a shared time. Each snap
carries `serverMs`. Client estimates offset = `serverMs - receivedMs` with
a rolling min filter (ping variance). All interp math uses
`clientMs + offset` as "server time".

### 5.6 Lag compensation (for future combat)

Only needed when we add hitscan shooting. On a shoot usercmd:

1. Server reads `snapMs = snapMsForMessageNum[cmd.ack]` — the moment the
   attacker *thought* they were shooting at.
2. For each other player, find the two entries in `posHistory` bracketing
   `snapMs - INTERP_DELAY_MS` and lerp to rewind their position.
3. Raycast against the rewound positions. Register hit on the player the
   attacker saw on their screen.
4. Restore positions and continue sim.

Budget: `posHistory` = 30 entries × 8 players × ~48 bytes = ~12 KB. Free.

### 5.7 Perf & tuning targets

- Snap size (full): ~120 bytes/player. 8 players × 120 = ~1 KB. Under MTU.
- Snap size (delta, steady state): expect 10–30 bytes/player.
- Uplink per client: 60 Hz × 3 cmds × ~16 bytes = ~3 KB/s.
- Downlink per client: 30 Hz × ~200 bytes = ~6 KB/s.
- Server CPU: 60 Hz pmove × N players. Pmove is <1 µs in JS, so 8 players = ~0.5 ms/tick budget used, leaving plenty for delta encode.

Tunables to expose on `serverConfig`:
`tickRate`, `snapRate`, `cmdRate`, `cmdBackup`, `interpDelayMs`,
`posHistoryMs`, `smoothCorrectMs`.

---

## 6. Suggested milestones

Presence tier (relay-only, builds on squash/1v1 pattern):

- **M1 — Presence skeleton.** Wire WS + UDP in arena.mjs, log incoming
  frames to console, no rendering. Confirm two tabs exchange `arena:move`.
- **M2 — Remote body render.** Build per-remote feet/arms forms, position
  each paint. Ignore interpolation (snap to server pos).
- **M3 — Death/respawn lifecycle + handle labels** (world→screen project).
- **M4 — Naive lerp + polish.** Fade-in on join, alpha for guests, perf
  test with ≥4 players.

Q3-caliber tier (server-authoritative):

- **M5 — Extract `shared/pmove.mjs`** from cam-doll. Unit tests that
  fixed-seed command streams produce identical state in Node and browser.
- **M6 — `ArenaManager` skeleton.** 60 Hz tick, full snapshots (no delta
  yet), 30 Hz broadcast, client drops M1-M4 relay code and uses manager
  snaps as truth. No client prediction yet — just visual lag.
- **M7 — Client prediction + reconciliation.** Pending cmd queue, replay
  on ack, smooth-correct on mispredict.
- **M8 — Interpolation buffer for remotes.** 100 ms delay, freeze on
  starvation, clock sync via snap `serverMs`.
- **M9 — Delta compression.** Per-client snap ring, field schema, bit-per-
  field changed marker, last-changed early-out. Measure bandwidth drop.
- **M10 — Command backup + ack-in-cmd.** Send last 3 cmds per packet,
  include `ack` of last snap seen. Server dedupes by seq.
- **M11 — (optional) Lag-compensated hitscan.** Only if combat lands.

Each is independently shippable. M5–M8 together reach "playable Q3-lite";
M9–M11 are the polish that makes it feel pro.

---

## 7. References

- [Quake 3 Source Code Review: Network Model — Fabien Sanglard](https://fabiensanglard.net/quake3/network.php)
- [Quake 3 Network Protocol (wire format) — jfedor](https://www.jfedor.org/quake3/)
- [`sv_snapshot.c` in id's Quake 3 source](https://github.com/id-Software/Quake-III-Arena/blob/master/code/server/sv_snapshot.c)
- [`sv_client.c` — client/cmd handling](https://github.com/id-Software/Quake-III-Arena/blob/master/code/server/sv_client.c)
- [Netcode Architectures Part 3: Snapshot Interpolation — SnapNet](https://snapnet.dev/blog/netcode-architectures-part-3-snapshot-interpolation/)
- In-repo: [`session-server/duel-manager.mjs`](../session-server/duel-manager.mjs) — the dumbed-down version of this pattern already running in production (no delta, no cmd backup, no interp buffer, no lag comp)

Each milestone is independently shippable.
