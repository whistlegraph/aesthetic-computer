# dool

Aesthetic Computer's two-player world, developed in `duel/`.
Move through the arena with empty hands and ask Aesel to build or revise objects.
The original rocket match simulation remains underneath. The scoreboard is hidden.

```sh
npm run duel
```

Open http://127.0.0.1:8787 in two browser windows. You enter the lobby directly;
click the arena to capture the mouse. One player can move and build while waiting.
The second starts a three-second countdown. Death respawns after one second;
a finished match resets after five. Additional connections spectate with a free camera.

WASD moves, mouse aims, left mouse opens the object prompt, space jumps, V switches first/third
person, Escape releases the mouse. Click again to resume. No mouse is needed: a control key
engages the keyboard without capturing the pointer, and the arrow keys turn and tilt. A
controls legend for the hand in use (keys, pad or touch) shows for the first eight seconds of
play; H pins or hides it.
Handles float above players. A compact room list and connection meter are
rendered in the engine, with no DOM overlays. The aim point sits at the ray's
impact in the world. The canvas retains an accessible control description.
The renderer follows canvas resizes, adapts portrait camera framing and draws
without antialiasing. Solid triangle faces, geometric bitmap letters and pixelated canvas
scaling keep the pixel edges sharp. Resizing is supported.

**Touch:** the left half is a floating move stick; drag the right half to look,
tap it to jump, and hold it to open the build ring.

**Controller:** left trigger jumps. RB adds: it opens the build ring, or changes the object (or KidLisp wall) you aim at; with an item in hand it uses it. LB removes the object you aim at (it stays in the $ history, Back, to put back). X paints floor squares or drops an item. Start opens the room menu. Point with
a stick, A adds the highlighted choice, X builds, B undoes the last choice and
then closes. Aimed at your own object, the ring offers changes ("make it spin");
otherwise you pick a thing and then traits ("a red spinning tower"). Typing on
a keyboard while the ring is open hands the prompt over to the keyboard.

**Live dev strip:** set `DOOL_DEV_LOG` to the transcript of the agent editing
the game (a Claude Code or Codex `.jsonl`) and the server shows that agent's
latest note and tool call along the top of the view. It only appears in the
rooms named by `DOOL_DEV_LOG_ROOMS` (default `studio`). Home paths, keys and
long tokens are redacted on the server before anything is sent.

Share the page URL to invite another player. Use `?room=your-room` for a separate
room; there is no weapon selection. A localhost invite works only on the same
machine. The corner join code is retired for now, ahead of dool joining the AC
homepage. Guests are named from their identity (two words, the same in every
room) and may start five builds a day; signed-in players spend their own
braincells.

Open two live clients side by side in one desktop window:

```sh
npm run duel:rig
```

This runs the Electron AC app's dedicated test mode against production. Each
pane has its own renderer and persistent player identity, and both join one newly
generated room. Click a pane to control it; Escape releases the mouse so you
can click the other. V switches that client's camera. The native Test menu
reloads both clients (Cmd-R) or focuses the left/right client (Cmd-1/Cmd-2).
Closing the window disconnects both clients and exits the test app.
Slab tiles the pair as one window with Cmd-Option-T; Cmd-Option-arrow moves
focus between the rig and neighboring tiles. Both clients resize within it.

To test local code, run `npm run duel` in another terminal and launch with
`DUEL_URL=http://127.0.0.1:8787/ npm run duel:rig`. The rig uses the Mac's existing AC session to identify players. No debug port
or analytics is enabled by the normal rig launch. The desktop app source also
includes File → Dool Test Rig; installed releases need a rebuild to get it.

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
| Input delivery | Six-command backup, one batch per display frame, deduplication, bounded queue |
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
colors with broad ambient shading: no textures or post effects.
Third person shows a body with hands; first person draws no arms or hands, only the aim point and anything held. Collision and network authority
remain independent of rendering.

New objects use the shared Oskiewar object dialect in `xbox/live/object-lisp.mjs`.
`object-language.mjs` admits a bounded subset, then compiles with that existing
compiler. Retained mesh/sketch handles and placements feed the WebGPU triangle
stream. Static geometry compiles once; animated transforms read shared match time.
This is the object dialect, not the full 2D KidLisp evaluator or a JavaScript sandbox.

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
npm run test:duel:rig
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
5. Extend the admitted object dialect as needed while retaining explicit work,
   geometry and animation bounds.
6. Integrate the AC entry point, identity, invites, regions, spectators and
   replay evidence; version the physics/map build together before deployment.

The foundations are testable; competitive WAN quality, matchmaking and production
operations are not established by this scaffold.

## Operational diagnostics

Set `DOOL_DIAGNOSTICS=1` on the game server to enable private structured journal
records prefixed `dool.diagnostic`. Off by default; nothing goes to PostHog.
The welcome message enables the matching client sampler. Visible clients send
at most one summary every five seconds, covering frame gaps/work, long tasks,
snapshot gaps, message processing, prediction replay cost, maximum pending
commands, coalesced snapshots, rendering cost, RTT/jitter, queue size and
object/triangle counts. Hidden tabs and the closed homepage backdrop reset the
window. Samples are client-reported evidence, not authoritative timing.

Schema 1 has an ephemeral run/session UUID, deployed revision, bounded numeric
fields, fixed outcome/reason/phase enums, and build UUIDs already disclosed to
that connection. It excludes prompts, source, handles, room names, owner tokens,
URLs, raw errors and stacks. No persistent browser tracking ID is created.
Per-connection sampling and a global 120-record/second cap bound ingestion.
Journal storage and retention follow the host's journald policy; no new public
read endpoint or file store is created.

Build records cover acceptance, rejection, phase duration, preview processing,
commit duration and terminal outcomes. A separate publication receipt joins the
build UUID to its public `$code`; KidLisp's ATProto log names that same code on
success or failure. A commit is not proof that downstream publication or client
display succeeded. A browser that never recovers cannot send a final sample.

Inspect with `journalctl -u lith -o cat | rg 'dool.diagnostic|KidLisp ATProto'`.
`node duel/test/diagnostics-browser.mjs` verifies two clients observing one build
and a controlled main-thread stall, without calling a model. The corner label
shows only the local identity; other players' names remain above their heads.

Local load and recovery checks (no model calls or production traffic):

```sh
node duel/test/load-server.mjs          # 16 connections, four animated rooms
node duel/test/load-browser.mjs         # 4× CPU slowdown, packet bursts, movement, reconnect
node duel/test/sheet-recovery-browser.mjs # embedded reload, HUD, meter, prompt resume
```

The load fixture is synthetic. `DOOL_ROOM_FIXTURE=/private/path/objects.json`
can replay a private store locally; never commit that store or its output.

## Aesel objects

Click while moving or press Enter to open a GPU-rendered text prompt. Type a request such as `add a staircase`
and press Enter again. Escape closes it. Aim at your object and press Enter to
revise it. The first build is v0; revisions append v1, v2, etc. Players and spectators see
completed objects and collide with the same geometry. A pending build does not
pause the match. The small label under the aim identifies the object and owner.

Run `npm run duel:studio` to connect a local world to the private remote Aesel
Claude relay using the existing AC login. Open the rig with
`DUEL_URL='http://127.0.0.1:8787/?room=studio' npm run duel:rig`. Inference runs remotely; the
bridge is a separate authenticated WebSocket client. This does not deploy the
feature to the public dool service.

For a remote builder process, run `npm run duel:builder` with `DOOL_SERVER_URL`
(the WSS `/builder` endpoint), a shared server-only `DOOL_BUILDER_TOKEN` of at
least 32 characters, and the operator's existing AC relay login. The game
server must use the same secret. Never put that secret in a client URL or page.
The current private relay admits its configured AC owner; this is not a public
multi-account Aesel service. Different game players can submit independent
requests through the authorized builder, up to two concurrent jobs.

On production, the builder runs on lith beside the server as
`dool-builder.service` (installed by `lith/deploy.fish`). It reaches the server
over loopback and reads `DOOL_BUILDER_TOKEN` from `/etc/aesthetic-computer/dool.env`,
which `lith.service` also loads. Its own AC login lives at
`/var/lib/dool-builder/.ac-token`. Seed both with
`fish lith/scripts/dool-builder-setup.fish` (run it again to renew the login). Until then the unit stays down and the game reads "Aesel is offline".

Ownership is a server-issued player identity saved in that browser/profile;
AC handles are verified separately for display. Each rig pane retains a separate
object-owner identity even when both use the same AC sign-in.
The server assigns owner, object ID and version; client-provided ownership is
ignored. Only the owner can revise an object. New geometry is compact object Lisp, admitted and compiled by the shared
Oskiewar compiler; generated JavaScript is never executed. Legacy JSON boxes remain readable. Invalid results, stale edits, occupied space
and failed generations leave the previous version intact.

Studio stores identities and immutable prompt/version history privately in
`~/.local/share/dool-studio/objects.json`; standalone dool uses
`~/.local/share/dool/objects.json`. `DOOL_STATE_DIR` overrides that directory.
Objects belong to a room. Reopen the same room URL to recover them; each new
rig window normally creates a fresh room unless DUEL_URL specifies a room. Versions persist across process
restarts when a state directory is configured; in-process test servers use
memory. Maximum: 16 objects per room, 32 versions per object, 32 shapes and
512 triangles per new version, 8000 source characters, and 280 prompt characters.
No prompts enter analytics.

`node duel/test/prompt-browser.mjs` tests the UI and two-player protocol with a
controlled builder. Add `--live` to exercise the remote relay and consume real
model usage. `npm run test:duel` checks ownership, persistence, geometry bounds,
stair traversal, prediction and builder authorization.

Builds show a floor footprint immediately, followed by bounded wire previews and
the actual streamed Lisp in the GPU scene. A timer and stage distinguish model
startup, generation, correction and validation. Previews never affect collision.
Submitting resumes mouse capture; Escape cancels your unfinished build. If the
browser refuses capture, the in-engine prompt says `CLICK MOVE`.

The `oskiewar-object-v1` format stores canonical source per immutable version.
Supported shapes are `slab`, `orb`, `capsule`, `tri`, and `quad`; transforms are
`move`, `rotate`, and constant `scale`. `ink`, `def`, `let`, and bounded `repeat`
compose them. `sin`/`cos` of `time` animate wings, feet, and paths without a
separate creature protocol. Shapes bake into retained parts; their transformed
bounds supply the same collision boxes on client and server. Placement uses a
conservative envelope of the whole animation. Wings and curved objects use
box approximations for collision, not triangle-exact collision.

Only transforms animate: dimensions and colors remain constant. The admission
pass limits nesting, expression expansion, loops, shapes, and triangle count
before publication. Complete streamed forms pass the same checks for previews.
Failures now report the validation reason; failed edits preserve the old version.
Older static objects stay static until revised. Legacy walk/fly records still work.

`node duel/test/progress-browser.mjs` checks previews, continued movement,
cancellation and shared animation. `--live --butterfly` tests the remote builder;
`--lisp` replays a real saved result from `tmp/duel/butterfly-lisp.json`.

## Presence and connection

The server verifies AC credentials with the existing account verifier and broadcasts
only handles, seats and object-owner IDs. The desktop rig attaches its native AC
session to the game WebSocket handshake, restricted to loopback or aesthetic.computer;
tokens never enter renderer JavaScript or room state. The public browser entry can
reuse an existing AC browser login. Unsigned players display as GUEST 1 / GUEST 2.

The upper-right bars measure connection health, not physical Wi-Fi signal strength.
The number is a local-clock echo round trip to the game server, sampled once per
second and smoothed; J appears for jitter of at least 10 ms. Missing snapshots,
missing echoes or an outbound backlog show STALLED. Disconnect shows OFFLINE.
A real zero-millisecond sample is distinct from no measurement.

Oskiewar's inspected combat lane uses delayed-input rollback, redundant pad history,
and echoed timestamp measurements. Dool retains its authoritative server and
prediction model. It now sends one latest input batch per display frame, retains
six recent commands, and stops adding movement packets once the WebSocket backlog
reaches 2 KiB. This limits extra queueing; it does not change radio quality or remove
TCP head-of-line blocking. WAN improvement still needs measurements on a remote link.

`node duel/test/rig.mjs --layout-only --accounts` verifies the actual native AC
session, two-client presence and measured ping; it requires the operator's AC login.

## Spectators and controllers

One verified AC handle occupies at most one player slot in a room. A second
connection for that handle becomes a spectator; room presence lists the handle
once. Spectators receive the same scene and builds, but have no body, collision,
player input or build authority. The first connection retains priority even if
account verification finishes out of order. On disconnect, the earliest eligible
spectator takes the open slot. Rooms allow two players and up to eight spectators.

Spectator freecam: WASD and mouse, Space up, Shift down. It starts looking at the
active player. Handles use AC's canonical pixel font, exact casing, and saved
per-character colors from the handle-colors API.

8BitDo/standard gamepad: left stick moves, right stick looks, bottom face button
jumps, left face button opens the prompt, top face button switches first/third
person, right face button closes/cancels the prompt. Typing still uses the keyboard.
In freecam, LB lowers, RB raises and RT moves faster. Select switches the focused
rig pane; Start toggles fullscreen. Only the focused pane consumes controller
input. Deadzones suppress stick drift, actions trigger on press edges, and
unplugging clears movement. Chromium may require a button press before exposing
a newly connected controller.

Launch the live room fullscreen with
`DOOL_FULLSCREEN=1 DUEL_URL='http://127.0.0.1:8788/?room=studio' npm run duel:rig`.
The app opens one player view. Set `DOOL_CLIENTS=2` for the tiled test rig.
The engine renders the room's join QR at top right, with a four-module white
quiet zone. Set `DOOL_PUBLIC_URL` on a local server to the HTTPS address that
reaches that same server; public clients otherwise use their current origin.
HUD text and QR modules stay pixel-aligned, with 5% TV edge insets and responsive
sizes. Localhost links are never offered as join QR codes.
macOS uses fullscreen on the current desktop so switching panes retains focus.
`node duel/test/rig.mjs --layout-only --accounts --controls` checks duplicate-handle
spectating and simulated controller input through the native app.

Rotation bounds now evaluate sine/cosine over each actual angle interval. A small
leg swing no longer reserves a full circle and falsely crosses the floor. The
previously rejected Big friend source is a regression fixture; animations that
really cross the ground remain rejected.

## Under the prompt

dool is the world beneath the aesthetic.computer homepage. The prompt piece
draws dool's square reticle bottom-right of its curtain; a tap flies it to the
centre and `lib/dool-sheet.mjs` (the host, in bios) drops the prompt's paper
to uncover a same-origin frame of `/dool/?embed=prompt` parked beneath the
stage. The curtain is clear, so the world shows through it from the first
frame, bare of its HUD; the frame takes no seat until it is first opened and
is silent until then.

Hosted, the client says `dool:sheet:hello` when its script has run and
`dool:sheet:leave` when the player goes back: Escape with nothing open and
the mouse already free, or a tap on the corner block at the top left (the
system's label on every AC piece, SCREEN.md). The host answers with
`{ type: "dool:sheet", open }` as the paper moves. Back in the browser closes
the world too; the open world is one history entry at `/dool/`. Standalone,
the corner block goes to the homepage.
