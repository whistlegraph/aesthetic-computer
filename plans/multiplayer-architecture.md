# Multiplayer Architecture — analysis & the land/WorldManager refactor

*2026.06.09 — written alongside the `land` (3D meadow) build. Companion to
`plans/arena-multiplayer.md` (the original Q3-style design doc).*

## The stack today

```
piece (land.mjs / arena.mjs, in the disk worker)
  │  usercmds @60Hz (cmd backup ×3, seq-numbered)        snaps @30Hz (delta vs ack)
  ▼                                                       ▲
net.udp  ── geckos.io WebRTC datachannel (unordered/unreliable)
net.socket ─ WebSocket (reliable; hello/bye/join/leave/ping + cmd/snap fallback)
  │
  ▼
session-server (ONE monolith node process for everything:
  chat ×3 instances, duel, worlds, notepat MIDI, DAW, status streams, machines)
  └─ WorldManager per world {arena, land}: 60Hz tick, 30Hz per-client
     delta snapshots, pmove integration on cmd arrival (Q3-style)
```

Client prediction: cam-doll integrates the same pmove model locally;
`reconcileLocal()` replays unacked cmds from the server's authoritative state
and soft-corrects (dead zone 0.5u, soft-K 0.18, hard snap > 4u). Remote
players render ~100ms in the past from an interpolation buffer, with ≤250ms
velocity extrapolation when the buffer starves.

## Why it felt skippy

Ranked by observed impact:

1. **Silent UDP death (found & fixed today).** A WebRTC datachannel can die
   while `webrtcConnection.state` stays `"open"` and `channel.emit()` keeps
   returning success. The server's "prefer UDP, fall back to WS" check never
   falls back, so the client *receives nothing* — remotes freeze, then the
   piece coasts on extrapolation, then teleports. Reproduced 100% in
   headless Chromium (UDP resolves, then rx dies ~8s in); the same failure
   shape exists on flaky NATs/mobile in prod, where it reads as "skippy".
   **Fix (shipped):**
   - server: stall watchdog in `broadcastSnapshots` — a player on UDP whose
     snap-ack hasn't advanced for ~2s of sends is demoted to WS snapshots,
     with a 30s re-promotion block so a dead-but-open channel can't flap;
   - session.mjs re-resolves the UDP channel on *every* UDP cmd (freshest
     liveness signal), gated by that block;
   - client (arena + land): if snaps go quiet >1.5s while UDP claims
     connected, cmds are mirrored over WS so the server keeps seeing acks.
2. **The lobby leak (fixed today).** `broadcastWS` was `everyone()` — every
   arena join/leave hit every client on the monolith, including chat tabs;
   anything not explicitly routed in session.mjs also relays to everyone.
   World events are now member-scoped inside WorldManager (it already knows
   every member wsId), and *all* `<world>:*` verbs are consumed by the world
   router instead of falling through to the relay.
3. **Server integrates only on cmd arrival.** Faithful to Q3, but cmds ride
   an unreliable channel in bursts; a late batch means the victim's snapshot
   position stalls then jumps, which the remote interp buffer reproduces
   faithfully. The cmd-backup window (×3) plus extrapolation mostly covers
   it; if it still shows, the next lever is server-side per-tick coasting
   (integrate held inputs on empty ticks), which trades a little authority
   drift for smoothness.
4. **Event-loop contention on the monolith.** The 60Hz world tick shares one
   node event loop with chat history loads, Mongo round-trips, status
   streams, etc. Timer jitter directly becomes snap jitter (clients
   max-filter + slew the clock to absorb it, but the floor is set by the
   server). Worth measuring before splitting; the clean split is one
   process (or worker thread) per world, same protocol.
5. **JSON wire format.** Fine at ≤8 players (delta snaps are small);
   bit-packing was deliberately deferred — still the right call.

## The lobby model after the refactor

- `session-server/world-manager.mjs` — `WorldManager` class, one instance
  per world; prefix parameterizes the protocol (`arena:*`, `land:*`).
  Everything else (tick, snapshots, takeover/spectate, stale sweep, delta
  compression, lag-comp history) is shared.
- Adding a world = one cfg module in `lib/<name>-world.mjs` + one line in
  session.mjs's `worldManagers` map + a client piece. Routing (hello / bye /
  cmd / ping, WS close, UDP identity/cmd) is generic over the map.
- `land` proves the pattern: its *entire* world cfg lives in
  `lib/land-world.mjs`, imported by both client and server — no duplicated
  MUST-MATCH block (arena still has one; migrate it the same way when
  convenient).

Current scope is **one room per world** (matches the single global chat).
Next steps when a meadow gets crowded: shard key `world:instance`, spawn a
WorldManager per instance, and have hello return the assigned instance —
the protocol already carries everything needed.

## Test harness (all local, no prod risk)

- `npm test` in `session-server/` — vitest; `tests/world.test.mjs` covers
  prefixed events, member-scoped broadcasts, pmove integration, takeover,
  reload races, multi-world isolation.
- `npm run land:probe:local` / `arena:probe:local` — terminal probe
  (`--world` flag) for live snap-rate/RTT/roster against any server.
- `node artery/test-land-multiplayer.mjs` — two headless Chromium players
  on `/land` + a wire-level walker bot; asserts snaps flow to both browsers
  and that the bot appears in both rosters (this is the test that caught
  the silent-UDP-death bug). `--keep` runs headful for manual driving.
