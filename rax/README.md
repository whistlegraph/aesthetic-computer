# rax

Aesthetic Computer's shared world, developed in `rax/`.
The [park and pod geography](GEOGRAPHY.md) separates personal pod identities from
their park placements. Walk the public paths and enter tagged pods. Pod interiors are public to visit and only
their owners can arrange them.
Move through the arena with empty hands and ask Aesel to build or revise objects.
The original rocket match simulation remains underneath. The scoreboard is hidden.

```sh
npm run rax
```

Open http://127.0.0.1:8787 in two browser windows. You enter the lobby directly;
click the arena to capture the mouse. One player can move and build while waiting.
The second player starts the match where both stand: an arrival or a departure
never moves or stops anyone. Death respawns after one second; a finished match
resets after five, with a three-second countdown. Additional connections spectate with a free camera.

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

Walk up to the football dispenser in the courtyard to roll out a ball. Step away
and return for another. Everyone nearby can kick the balls across park boundaries.

The east side inside the plaza lobby has stairs marked B1. Walk down into the basement or take the elevator down from G. Tap the car’s up or down arrow once per floor, or hold it to scroll. Hold for two seconds for the roof or B1. The car departs after a short pause. Jump through an open tower window to the block below. B1 opens onto the inner sea beneath the planet’s surface. Jump from the scaffold or take the dock stairs to the water. Look down while swimming to dive. Jump beside a rowboat to board, then jump again to dismount.

**Touch:** the left half is a floating move stick and the right half drags to look; both
are outlined on the lower half of the screen. Four icons sit bottom right: the $ menu, jump, chat (a
message bubble) and build (a cube). What the thing in view offers (paint, chalk, change,
remove, lock on, use, drop) appears as a cap above them; typing a build, a ring cap opens
the build ring. Chat types on the top line, as a build does.

**Controller:** A jumps and the left trigger zooms. LB locks onto a target on foot
(X as a drone). Press it again to focus on a screen. D-pad left/right cycles targets
even while focused. On a kiosk, move the pointer with the left stick and press A to
interact. B backs out. With an object locked on foot, B removes it.
RB opens the build ring or uses the item in hand. X paints floor squares or drops an
item. Start returns to the prompt. D-pad right opens the $ menu when there is no
lock. In the elevator, d-pad up/down selects a floor. In the build ring, point with
a stick, A adds the highlighted choice, X builds, B undoes the last choice and
then closes. Aimed at your own object, the ring offers changes ("make it spin");
otherwise you pick a thing and then traits ("a red spinning tower"). Typing on
a keyboard while the ring is open hands the prompt over to the keyboard.

**Live dev strip:** set `RAX_DEV_LOG` to the transcript of the agent editing
the game (a Claude Code or Codex `.jsonl`) and the server shows that agent's
latest note and tool call along the top of the view. It only appears in the
rooms named by `RAX_DEV_LOG_ROOMS` (default `studio`). Home paths, keys and
long tokens are redacted on the server before anything is sent.

Share the page URL to invite another player. Use `?room=your-room` for a separate
room; there is no weapon selection. A localhost invite works only on the same
machine. The corner join code is retired for now, ahead of rax joining the AC
homepage. Guests are named from their identity (two words, the same in every
room) and may start five builds a day; signed-in players spend their own
braincells.

Open two live clients side by side in one desktop window:

```sh
npm run rax:rig
```

This runs the Electron AC app's dedicated test mode against production. Each
pane has its own renderer and persistent player identity, and both join one newly
generated room. Click a pane to control it; Escape releases the mouse so you
can click the other. V switches that client's camera. The native Test menu
reloads both clients (Cmd-R) or focuses the left/right client (Cmd-1/Cmd-2).
Closing the window disconnects both clients and exits the test app.
Slab tiles the pair as one window with Cmd-Option-T; Cmd-Option-arrow moves
focus between the rig and neighboring tiles. Both clients resize within it.

To test local code, run `npm run rax` in another terminal and launch with
`RAX_URL=http://127.0.0.1:8787/ npm run rax:rig`. The rig uses the Mac's existing AC session to identify players. No debug port
or analytics is enabled by the normal rig launch. The desktop app source also
includes File → Rax Test Rig; installed releases need a rebuild to get it.

For a LAN session, run `HOST=0.0.0.0 npm run rax` and use the host's LAN address
on both machines. `PORT` overrides 8787.

## botbop

**aesthetic.computer/botbop** opens it: the prompt's `botbop`, which drops the
paper into rax's arena. `botbop~name` is an arena of its own, and `rax~room`
reaches any room; the address bar follows you as you move. (Inside, the guest is
still `/rax/?room=…`; that is transport, not an address to hand out.)

Land on a head. Coming down onto another player's crown bops them and bounces
you. The rule (`bop.mjs`, run by the server in `match.mjs`) holds in every room;
the arena is a side-on stage where bodies keep to one plane, ledges are jumped
through from below and stood on from above, play never stops for a countdown,
and a bopped player is set down again a second later somewhere else.

Five drones walk the floor and the lower ledges: dumb robots, not bots. Landing
on one is a bop; another rolls out three seconds later.

The score sits top centre. Bops chained without touching down are a combo: the
first is worth 1, the second 2, the third 3; the chain reads under the score as
`+6 x3` and is counted in when the feet land. Bopped out of the air, it is lost.

Up (W, the up arrow, space, the stick) jumps and down ducks: on a ledge it drops
through, in the air it dives, on the ground the head sits lower and is harder to
land on. The left and right arrows walk as A and D do. `` ` `` shows the hitboxes:
bodies in green, the band a stomper's feet must fall through in pink, drones in
cyan, your feet in yellow, and a line of numbers.

A room like this is a **gametype** (`games.mjs`): one object naming its ledges,
its camera (botbop's frames whoever is near enough to bop), its own legend, and
the parts of rax's interface it does without (the reticle, the face cam, the
floor aim, the build hint). Add a game there and the client and view follow.

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
compact rax/rematch influence. This project contains no copied private
Oskiewar implementation and does not modify those games.

## Network contract

The local Node server owns movement, hit detection, health, scores, respawns and
match phases. Connections receive seat identities; messages cannot choose a
victim, claim a hit, supply a position or choose their simulation delta.

| Mechanism | Scaffold behavior |
| --- | --- |
| Simulation | 60 Hz, fixed 1/60-second movement step |
| Snapshots | 60 Hz, full state, sequence acknowledgements |
| Prediction | Immediate local simulation; reset to authority and replay unacknowledged input |
| Presentation | Interpolate consecutive local ticks at display rate; ease small reconciliation offsets over 70 ms; mouse aim stays immediate |
| Input delivery | Every command once, in order, in batches of twelve; what a slow link held back goes when it clears |
| Server time budget | One command per tick of the player's own time; late commands run in the order sent, up to `HORIZON` (four seconds) |
| Remote rendering | Playback clock six ticks behind the newest snapshot, further on a link that delivers in clumps (up to 36); rate adjusts; freeze on starvation |
| Rockets | Authoritative swept collision and splash; no projectile rewind |
| Arrivals and departures | Never move or stop anyone already in the room; the score clears when one player is left |
| Between rooms | The body keeps walking by its own commands; the next room runs them when it seats it |
| Stalls | Prediction runs `HORIZON` commands ahead of the last acknowledgement, then waits |
| Transport | WebSocket; bounded messages/queues, heartbeat and slow-consumer cutoff |

### Local first

This is the rule every change to movement, rooms and the network is checked
against: **your own body answers your own keys on the same frame, whatever the
link is doing.** The world arrives when it arrives; you never wait for it.

1. **A key moves the body at once.** Input is run locally the tick it is
   read. Nothing a server says is needed first.
2. **The server ends where you predicted.** It runs your commands, all of
   them, in the order sent, however late they come (to `HORIZON`). A
   correction is a bug to find, not a cost of latency: `?debug` counts them,
   and on a clean link the count is zero.
3. **Nothing else moves you.** Someone arriving, someone leaving, a round
   starting next to you, a reload of your own page: none of them sets your
   body down somewhere else or takes your keys away.
4. **A join in the world is not a stop.** Crossing a block or entering a
   door changes rooms under a body that is still walking: keys stay down,
   speed carries over, and the commands given on the way are run on arrival.
5. **Others glide.** Remote bodies are drawn from the past, far enough back
   that the next snapshot is always already here; the delay widens on a
   clumpy link instead of the picture stopping and jumping.
6. **When it must give out, it holds still and says so.** Past `HORIZON`
   the body waits where it is (never rubber-bands) and the status line reads
   CONNECTION STALLED.

`test/presentation.test.mjs` and `test/pods.test.mjs` hold a test for each of
2 to 5; a change that breaks one is breaking the rule.

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
Life changes snap only when the body really was set down elsewhere.

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
Lith mounts the game at `/rax/`, including `/rax/play` WebSockets and
`/rax/health` for the served protocol/revision. `/rax` redirects while preserving
the room query. This shares AC's HTTPS origin and upgrade router; other WebSocket
routes retain their existing query rules. It is a standalone game entry, not an
AC disk. Deploy committed, pushed `main` with `ac-deploy`, then verify the health
response, browser assets and two-player connection at the public URL.

## Checks

```sh
npm run test:rax
npm run test:rax:browser
npm run test:rax:render
npm run test:rax:rig
```

Simulation tests cover prediction parity, replay, malformed/duplicate/reordered
input, synthetic loss/jitter, cover, rocket travel/cooldown, respawns,
match completion and disconnect. Integration tests connect real WebSockets,
check room isolation/capacity and restrict served files. The browser test runs
two clients through movement, jumping, cameras, rockets and disconnect,
then writes screenshots to ignored `tmp/rax/`. It opens a test browser window
for real mouse capture, using installed Chrome on macOS or Playwright Chromium
elsewhere. `RAX_HEADLESS=1` is available where headless pointer lock works.

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

Set `RAX_DIAGNOSTICS=1` on the game server to enable private structured journal
records prefixed `rax.diagnostic`. Off by default; nothing goes to PostHog.
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

Inspect with `journalctl -u lith -o cat | rg 'rax.diagnostic|KidLisp ATProto'`.
`node rax/test/diagnostics-browser.mjs` verifies two clients observing one build
and a controlled main-thread stall, without calling a model. The corner label
shows only the local identity; other players' names remain above their heads.

Local load and recovery checks (no model calls or production traffic):

```sh
node rax/test/load-server.mjs          # 16 connections, four animated rooms
node rax/test/load-browser.mjs         # 4× CPU slowdown, packet bursts, movement, reconnect
node rax/test/sheet-recovery-browser.mjs # embedded reload, HUD, meter, prompt resume
```

The load fixture is synthetic. `RAX_ROOM_FIXTURE=/private/path/objects.json`
can replay a private store locally; never commit that store or its output.

## Aesel objects

Click while moving or press Enter to open a GPU-rendered text prompt. Type a request such as `add a staircase`
and press Enter again. Escape closes it. Aim at your object and press Enter to
revise it. The first build is v0; revisions append v1, v2, etc. Players and spectators see
completed objects and collide with the same geometry. A pending build does not
pause the match. The small label under the aim identifies the object and owner.

Run `npm run rax:studio` to connect a local world to the private remote Aesel
Claude relay using the existing AC login. Open the rig with
`RAX_URL='http://127.0.0.1:8787/?room=studio' npm run rax:rig`. Inference runs remotely; the
bridge is a separate authenticated WebSocket client. This does not deploy the
feature to the public rax service.

For a remote builder process, run `npm run rax:builder` with `RAX_SERVER_URL`
(the WSS `/builder` endpoint), a shared server-only `RAX_BUILDER_TOKEN` of at
least 32 characters, and the operator's existing AC relay login. The game
server must use the same secret. Never put that secret in a client URL or page.
The current private relay admits its configured AC owner; this is not a public
multi-account Aesel service. Different game players can submit independent
requests through the authorized builder, up to two concurrent jobs.

On production, the builder runs on lith beside the server as
`rax-builder.service` (installed by `lith/deploy.fish`). It reaches the server
over loopback and reads `RAX_BUILDER_TOKEN` from `/etc/aesthetic-computer/rax.env`,
which `lith.service` also loads. Its own AC login lives at
`/var/lib/rax-builder/.ac-token`. Seed both with
`fish lith/scripts/rax-builder-setup.fish` (run it again to renew the login). Until then the unit stays down and the game reads "Aesel is offline".

Ownership is a server-issued player identity saved in that browser/profile;
AC handles are verified separately for display. Each rig pane retains a separate
object-owner identity even when both use the same AC sign-in.
The server assigns owner, object ID and version; client-provided ownership is
ignored. Only the owner can revise an object. New geometry is compact object Lisp, admitted and compiled by the shared
Oskiewar compiler; generated JavaScript is never executed. Legacy JSON boxes remain readable. Invalid results, stale edits, occupied space
and failed generations leave the previous version intact.

Studio stores identities and immutable prompt/version history privately in
`~/.local/share/rax-studio/objects.json`; standalone rax uses
`~/.local/share/rax/objects.json`. `RAX_STATE_DIR` overrides that directory.
Objects belong to a room. Reopen the same room URL to recover them; each new
rig window normally creates a fresh room unless RAX_URL specifies a room. Versions persist across process
restarts when a state directory is configured; in-process test servers use
memory. Maximum: 16 objects per room, 32 versions per object, 32 shapes and
512 triangles per new version, 8000 source characters, and 280 prompt characters.
No prompts enter analytics.

`node rax/test/prompt-browser.mjs` tests the UI and two-player protocol with a
controlled builder. Add `--live` to exercise the remote relay and consume real
model usage. `npm run test:rax` checks ownership, persistence, geometry bounds,
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

`node rax/test/progress-browser.mjs` checks previews, continued movement,
cancellation and shared animation. `--live --butterfly` tests the remote builder;
`--lisp` replays a real saved result from `tmp/rax/butterfly-lisp.json`.

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
and echoed timestamp measurements. Rax retains its authoritative server and
prediction model. It now sends one latest input batch per display frame, retains
six recent commands, and stops adding movement packets once the WebSocket backlog
reaches 2 KiB. This limits extra queueing; it does not change radio quality or remove
TCP head-of-line blocking. WAN improvement still needs measurements on a remote link.

`node rax/test/rig.mjs --layout-only --accounts` verifies the actual native AC
session, two-client presence and measured ping; it requires the operator's AC login.

## Spectators and controllers

One verified AC handle occupies at most one player slot in a room. A second
connection for that handle becomes a spectator; room presence lists the handle
once. Spectators receive the same scene and builds, but have no body, collision,
player input or build authority. The first connection retains priority even if
account verification finishes out of order. On disconnect, the earliest eligible
spectator takes the open slot. Rooms hold up to sixteen players (`MAX_PLAYERS`) and as many
spectators again. The second player to arrive starts a match; later arrivals drop into it on a
spawn ring facing the middle, and a departure only stops the match when it leaves one player alone.

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
`RAX_FULLSCREEN=1 RAX_URL='http://127.0.0.1:8788/?room=studio' npm run rax:rig`.
The app opens one player view. Set `RAX_CLIENTS=2` for the tiled test rig.
The engine renders the room's join QR at top right, with a four-module white
quiet zone. Set `RAX_PUBLIC_URL` on a local server to the HTTPS address that
reaches that same server; public clients otherwise use their current origin.
HUD text and QR modules stay pixel-aligned, with 5% TV edge insets and responsive
sizes. Localhost links are never offered as join QR codes.
macOS uses fullscreen on the current desktop so switching panes retains focus.
`node rax/test/rig.mjs --layout-only --accounts --controls` checks duplicate-handle
spectating and simulated controller input through the native app.

### Signing a screen in from a phone

A tab that looks on with nobody signed in shows a QR bottom left, in the face
cam's place, with a four-digit number. A phone already signed in to AC scans
it, sees the same number and its own handle, and on a tap the screen is seated
as that handle. A click on the QR signs in on the device itself instead.

- The tab sends `{ type: 'pair' }` on its socket and gets `{ code, number, ttl }`.
  The code is 128 random bits, lives two minutes, works once, and is kept in
  memory with that socket (`pairing.mjs`). The tab asks again 30 s before it
  runs out; a socket holds the code it shows and the one before.
- The QR is `https://aesthetic.computer/rax/pair#<code>`. The phone's page
  (`pair.html`, `pair.mjs`) takes the code out of the address at once, posts its
  token to `/pair/who` to learn its handle, and on the tap posts
  `{ code, accessToken }` to `/pair`.
- The server verifies the token and signs in the waiting socket only. The tab
  never receives the token and the server does not keep it for that socket, so
  a paired screen builds on a guest's allowance. No credential is minted.
- A reload, or a dropped connection, is a new socket: scan again. Walking
  between rooms keeps the handle (a passage carries it).
- Codes and approvals are limited per socket and per address (`PAIR_LIMITS`);
  five refused approvals discard a code. Codes and tokens are never logged.
- An agent does the phone's part with `acin_pair` (`toolchain/mcp/acin-pair.mjs`):
  it reads the QR out of a Frame capture of the screen and posts the code with
  the shared `~/.ac-token`. `node toolchain/mcp/acin-pair.mjs <picture>` is the
  same from a shell.

Rotation bounds now evaluate sine/cosine over each actual angle interval. A small
leg swing no longer reserves a full circle and falsely crosses the floor. The
previously rejected Big friend source is a regression fixture; animations that
really cross the ground remain rejected.

## Under the prompt

rax is the world beneath the aesthetic.computer homepage. The prompt piece
draws rax's square reticle bottom-right of its curtain; a tap flies it to the
centre and `lib/rax-sheet.mjs` (the host, in bios) drops the prompt's paper
to uncover a same-origin frame of `/rax/?embed=prompt` parked beneath the
stage. The curtain is clear, so the world shows through it from the first
frame, bare of its HUD; the frame takes no seat until it is first opened and
is silent until then.

Hosted, the client says `rax:sheet:hello` when its script has run and
`rax:sheet:leave` when the player goes back: Escape with nothing open and
the mouse already free, or a tap on the corner block at the top left (the
system's label on every AC piece, SCREEN.md). The host answers with
`{ type: "rax:sheet", open }` as the paper moves. Back in the browser closes
the world too; the open world is one history entry at `/rax/`. Standalone,
the corner block goes to the homepage.

## Toward unirax

One room now holds sixteen players. Measured on a local server (2026-11-08): twelve sockets each
held a seat at 61 snapshots a second; a snapshot was about 2.5 KB, so each client receives about
150 KB/s, and the room sends about 1.8 MB/s in total. Snapshots are serialized once per room per tick.

A single shared world for everyone ("unirax") needs four more things, in this order:

1. **Smaller snapshots.** Quantize positions and angles, send only fields that changed, and drop
   to 30 Hz for distant players. Bandwidth per client is the first wall, not CPU.
2. **Interest.** Send each client the players and objects near it, not the whole room.
3. **Cells.** Split the world into adjacent rooms, each its own authority, with players handed
   across an edge carrying their state. A cell is the unit that can move to a host near its players.
4. **Persistence per person.** Keep position, inventory and builds by handle, so leaving and
   returning lands you where you were.

Oskiewar's rollback lane does not carry over: it suits two seats on one engine. rax keeps one
authority per cell. See papers/arxiv-relay-rollback for both designs and the review questions.
