# duel

A working-name scaffold for Aesthetic Computer's 1v1 arena shooter.
Two players, one rocket launcher, one room, first to ten. Movement, aim and the rematch
are the game. No loadouts, pickups, inventory or progression.

```sh
npm run duel
```

Open http://127.0.0.1:8787 in two browser windows. You enter the lobby directly;
click the arena to capture the mouse. One player can move and fire while waiting.
The second starts a three-second countdown. Death respawns after one second;
a finished match resets after five. A third player is refused.

WASD moves, mouse aims, left mouse fires, space jumps, V switches first/third
person, Escape releases the mouse. Click again to resume. Keyboard and mouse only.
There are no DOM menus or HUD overlays: scores, status and instructions are on
arena walls, health is on the launcher and player, and the aim point sits at the
ray's impact in the world. The canvas retains an accessible control description.

Share the page URL to invite another player. Use `?room=your-room` for a separate
room; there is no weapon selection. A localhost invite works only on the same
machine.

For a LAN session, run `HOST=0.0.0.0 npm run duel` and use the host's LAN address
on both machines. `PORT` overrides 8787.

## Game

- **Rockets:** 100 HP, lethal direct hits, distance-based splash with cover
  occlusion, self-damage and rocket-jump impulse; 0.7-second firing interval.
- **Map:** 36 × 36 m, two mirrored cover blocks, opposite spawns, open center.
  No platforms yet. Coral and blue identify the two seats.
- **Camera:** both views aim from the same eye ray. The chase camera retracts
  at walls. Third person still reveals more around corners; ranked camera rules
  need a decision before competitive play.

`arena.mjs` supplies the movement lineage through the existing pure `pmove`
module. Both client and server call the same wrapper and configuration here.
`1v1.mjs` and `dumduel.mjs` are earlier AC precedents; Oskiewar supplies the
compact duel/rematch influence. This project contains no copied private
Oskiewar implementation and does not modify those games.

## Network contract

The local Node server owns movement, hit detection, health, scores, respawns and
match phases. Connections receive seat identities; messages cannot choose a
victim, claim a hit, supply a position or choose their simulation delta.

| Mechanism | Scaffold behavior |
| --- | --- |
| Simulation | 60 Hz, fixed 1/60-second movement step |
| Snapshots | 30 Hz, full state, sequence acknowledgements |
| Prediction | Immediate local movement; reset to authority and replay unacknowledged input |
| Input delivery | Three-command backup, deduplication, sequence ordering, bounded queue |
| Server time budget | At most one command per simulation tick; excess backlog discarded |
| Remote rendering | Six-tick buffer, shortest-angle interpolation, freeze on starvation |
| Rockets | Authoritative swept collision and splash; no projectile rewind |
| Disconnect | Match cancelled, remaining player returns to warmup; vacated seat reusable |
| Stalls | Prediction stops after one second without snapshots; no long catch-up burst |
| Transport | WebSocket; bounded messages/queues, heartbeat and slow-consumer cutoff |

`world.mjs` is shared geometry/movement; `net.mjs` is validation/reconciliation;
`match.mjs` is the transport-independent match; `server.mjs` supplies rooms,
WebSockets and an explicit static-file allowlist. `client.mjs` handles input and
network state; `view.mjs` draws with the repository's vendored Three.js.
`paint.mjs` owns stepped shading, outlines, paper texture and the animated
center surface. Fighters use articulated rounded limbs and faces; launchers
recoil and explosions scatter colored fragments. These are visual effects,
independent of the authoritative simulation.

The renderer exposes `setSurfaceFrame({ width: 256, height: 256, data })`, where
`data` is a 256 × 256 RGBA `Uint8ClampedArray`. The frame replaces the center
surface's procedural animation. This is the future KidLisp worker boundary;
there is no KidLisp evaluator or language-support claim in this scaffold.
Use the canonical evaluator in a separately budgeted worker when integrating
it, and keep program output confined to approved visual surfaces. Collision,
player visibility, inputs and match state remain server-owned.

No CDN or new dependency install is needed in an installed AC checkout. `ws`
comes from the root package. This is a standalone local harness, not an AC disk
or a deployed `aesthetic.computer/duel` route yet.

## Checks

```sh
npm run test:duel
npm run test:duel:browser
```

Simulation tests cover prediction parity, replay, malformed/duplicate/reordered
input, synthetic loss/jitter, cover, rocket travel/cooldown, respawns,
match completion and disconnect. Integration tests connect real WebSockets,
check room isolation/capacity and restrict served files. The browser test runs
two clients through movement, jumping, cameras, rockets and disconnect,
then writes screenshots to ignored `tmp/duel/`. It opens a test browser window
for real mouse capture, using installed Chrome on macOS or Playwright Chromium
elsewhere. `DUEL_HEADLESS=1` is available where headless pointer lock works.

## Next playtest gates

1. Tune movement, firing cadence, silhouettes and sightlines from actual 1v1
   sessions. Test high-refresh displays and real keyboard/mouse play.
2. Add an unordered datagram transport through the existing session stack;
   keep WebSocket control/fallback. TCP head-of-line blocking remains in this
   scaffold. Measure at 30/80/150 ms RTT, jitter, burst loss and server stalls.
3. Measure correction distance and hit fairness. Add clock synchronization,
   visual correction smoothing and projectile interpolation/prediction. Current
   clock estimation uses snapshot arrival; it is not latency-calibrated.
4. Define simultaneous-hit/tie policy, spawn protection/selection, match time
   limits, ranked camera rules and rematch consent. Current hits resolve in seat
   order, spawns are fixed, and matches automatically restart.
5. Connect KidLisp's canonical evaluator to the surface frame input, with
   explicit execution budgets and the language's conformance checks.
6. Integrate the AC entry point, identity, invites, regions, spectators and
   replay evidence; version the physics/map build together before deployment.

The foundations are testable; competitive WAN quality, matchmaking and production
operations are not established by this scaffold. No analytics or account data is
collected.
