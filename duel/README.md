# dool

Aesthetic Computer's 1v1 arena shooter, developed in `duel/`.
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
There are no DOM menus or HUD overlays: scores and status are on
arena walls, health is on the launcher, and the aim point sits at the
ray's impact in the world. The canvas retains an accessible control description.
The renderer follows canvas resizes, adapts portrait camera framing and draws
without antialiasing. Solid triangle faces, geometric bitmap letters and pixelated canvas
scaling keep the pixel edges sharp. Resizing is supported; touch controls are
not implemented yet.

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
| Prediction | Immediate local simulation; reset to authority and replay unacknowledged input |
| Presentation | Interpolate consecutive local ticks at display rate; ease small reconciliation offsets over 70 ms; mouse aim stays immediate |
| Input delivery | Three-command backup, deduplication, sequence ordering, bounded queue |
| Server time budget | At most one command per simulation tick; excess backlog discarded |
| Remote rendering | Six-tick target buffer on a monotonic playback clock; rate adjusts to jitter; freeze on starvation |
| Rockets | Authoritative swept collision and splash; no projectile rewind |
| Disconnect | Match cancelled, remaining player returns to warmup; vacated seat reusable |
| Stalls | Prediction stops after one second without snapshots; no long catch-up burst |
| Transport | WebSocket; bounded messages/queues, heartbeat and slow-consumer cutoff |

`world.mjs` is shared geometry/movement; `net.mjs` is validation/reconciliation;
`match.mjs` is the transport-independent match; `server.mjs` supplies rooms,
WebSockets and an explicit static-file allowlist. `client.mjs` handles input and
network state; `view.mjs` draws through Oskiewar's shared scene contract (`xbox/live/scene3d.mjs`).
The new shared `scene3d-webgpu.mjs` host uses one bounded vertex buffer, one
render pass and one triangle draw per frame, with a real depth buffer.
WebGPU is selected asynchronously; browsers without an adapter use Oskiewar's
existing WebGL 2 host, with antialiasing disabled for this game. Device loss
reloads the renderer. Other Oskiewar hosts keep their existing defaults.

`presentation.mjs` keeps displayed movement separate from physics. Local position
interpolation adds one simulation tick (16.7 ms) of visual delay. Corrections over
one meter, life changes and match transitions snap immediately; smaller offsets
decay in real time. No smoothed position is sent to the server or used for hits.

`paint.mjs` builds solid-color models and geometry from the shared Oskiewar
5x7 block font. `project.mjs` clips faces at the near plane and projects them
into the shared triangle stream. The world uses white, gray, black and seat
colors: no lighting, texture grain, sky gradient, scenery or post effects.
Fighters and launchers are articulated boxes; explosions are brief solid
flashes. Collision and network authority remain independent of rendering.

KidLisp remains a future integration. The unused RGBA surface inlet was removed
with the textured renderer; this host currently accepts solid triangles only.
Use the canonical evaluator in a separately budgeted worker and add a bounded
visual-surface command when integrating it. Program output must not alter
collision, player visibility, inputs or server-owned match state.

No CDN or new dependency install is needed in an installed AC checkout. `ws`
comes from Lith's dependencies (or the root package during local development).
Lith mounts the game at `/dool/`, including `/dool/play` WebSockets and
`/dool/health` for the served protocol/revision. `/dool` redirects while preserving
the room query. This shares AC's HTTPS origin and upgrade router; other WebSocket
routes retain their existing query rules. It is a standalone game entry, not an
AC disk. Deploy committed, pushed `main` with `ac-deploy`, then verify the health
response, browser assets and two-player connection at the public URL.

## Checks

```sh
npm run test:duel
npm run test:duel:browser
npm run test:duel:render
```

Simulation tests cover prediction parity, replay, malformed/duplicate/reordered
input, synthetic loss/jitter, cover, rocket travel/cooldown, respawns,
match completion and disconnect. Integration tests connect real WebSockets,
check room isolation/capacity and restrict served files. The browser test runs
two clients through movement, jumping, cameras, rockets and disconnect,
then writes screenshots to ignored `tmp/duel/`. It opens a test browser window
for real mouse capture, using installed Chrome on macOS or Playwright Chromium
elsewhere. `DUEL_HEADLESS=1` is available where headless pointer lock works.

The render check requires WebGPU-capable Chrome. It verifies the actual backend,
compares identical WebGPU/WebGL frames, checks depth occlusion after resize,
and captures both cameras and portrait/wide layouts. Its local CPU submission
timings exclude GPU completion and display latency; they are not a speedup claim.

## Next playtest gates

1. Tune movement, firing cadence, silhouettes and sightlines from actual 1v1
   sessions. Test high-refresh displays and real keyboard/mouse play.
2. Add an unordered datagram transport through the existing session stack;
   keep WebSocket control/fallback. TCP head-of-line blocking remains in this
   scaffold. Measure at 30/80/150 ms RTT, jitter, burst loss and server stalls.
3. Measure correction distance and hit fairness. Add clock synchronization,
   projectile interpolation/prediction. The remote playback clock smooths snapshot
   arrival jitter; it is not latency-calibrated.
4. Define simultaneous-hit/tie policy, spawn protection/selection, match time
   limits, ranked camera rules and rematch consent. Current hits resolve in seat
   order, spawns are fixed, and matches automatically restart.
5. Connect KidLisp's canonical evaluator to a bounded visual command, with
   explicit execution budgets and the language's conformance checks.
6. Integrate the AC entry point, identity, invites, regions, spectators and
   replay evidence; version the physics/map build together before deployment.

The foundations are testable; competitive WAN quality, matchmaking and production
operations are not established by this scaffold. No analytics or account data is
collected.
