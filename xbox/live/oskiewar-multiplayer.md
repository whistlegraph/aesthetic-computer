# Oskiewar multiplayer: how a versus match moves

Written 2026-09-09. Part one diagnoses the streamed lane as it shipped in v101
against the relay at 3040a4eb26. Part two describes the rollback lane built on
top of it in v102, which is what two current builds now play.

**Read this first.** A versus fight between two current builds is
**rollback netplay** — both machines simulate the whole fight and neither
streams it to the other. Jump to "Part two" for that. The streamed lane below
still exists and still matters: it is what spectators watch, and it is the
fallback whenever one side is an older build or the net channel is blocked.

---

# Part one: the streamed lane, and why the guest lagged

## Verdict

A versus match is one authoritative simulation, on the host's machine, with
the guest attached as a remote controller and a video feed. The guest runs no
simulation of its own fighter, no prediction, no interpolation buffer and no
clock sync. Its presses take two network legs to become pixels, its picture
is a ~30 Hz stream that the relay thins by 25-50%, and its HUD, intro card,
hit sparks and sound are gated behind host-only state it never enters. The
host feels a 60 Hz local game. The guest feels a laggy stream of it. Both
of those are the design working as written, not a fault in the network.

The three things @jeffrey reported map to specific lines:

| Symptom | Cause | Where |
|---|---|---|
| Guest feels slow | Press → relay → host tick → 33 ms publish gate → relay → guest paint. About 116 ms mean on a good link vs about 25 ms for the host, and no local prediction hides it. | `oskiewar.js:3530-3556`, `:2610-2621`, `:6933-6955` |
| Guest feels glitchy | The relay drops the newest frame of any burst (25 ms wall-clock gate, checked before store). The guest extrapolates on last velocity with no ground collision, then snaps. Frames are applied inside the socket callback, mid-render-interpolation. Sim clock drifts behind wall clock, so the 150 ms coast fuse stops tripping. | `oskiewar-live-manager.mjs:406-411`, `oskiewar.js:3492-3520`, `:3475-3481` |
| Not symmetrical | Host simulates locally with full VFX and audio. Guest gets positions, flags and names only: no `hit`, no bullets, no debris, no sound. | `oskiewar.js:1494-1505`, `:3392-3406`, `:6954` |
| No state updates on screen when the game starts | The match HUD block requires `gameplayStarted`, which the viewer boot path never sets. The intro card requires `introAge < roundIntroDurationUs()`, but versus reports `remainingMs = 0`, which pins the guest's round clock 30 s in the past. | `oskiewar.js:3643-3654`, `:11795`, `:1447-1457`, `:3427-3429`, `:11910-11916` |

## Measurements

Two probes were written for this (`xbox/tools/oskiewar-relay-probe.mjs`,
`xbox/tools/oskiewar-challenger-probe.mjs`). All runs hit the production
relay on 2026-09-09.

Relay alone, publisher + challenger + viewer in one Node process, so one clock
on both ends:

| Where run | Publish cadence | State frames dropped by relay | Guest inter-arrival p50 / p90 / p99 ms | Pads dropped by relay |
|---|---|---|---|---|
| jasellite (wired DO droplet) | 30 Hz | 0% | 33 / 34 / 35 | 0% |
| jasellite | 60 Hz | 50% (exactly every other frame) | 33 / 34 / 43 | 1% |
| neo (Wi-Fi laptop) | 30 Hz | 19-20% | 34 / 90 / 117 | 17-18% |
| neo | 60 Hz | 57% | 32 / 80 / 110 | 45% |

Real host: headless Chrome on neo hosting `oskiewar.com/probeh43`, Node
challenger seated with a 250 ms heartbeat, 45 s:

| Metric | Value |
|---|---|
| State payload | 1272 bytes p50, 1324 max |
| Frames received | 776 in 45 s, 28% of seq numbers missing |
| Inter-arrival ms p50 / p90 / p99 / max | 33 / 101 / 129 / 192 |
| Seq gap histogram | 1: 609, 2: 43, 3: 116, 4: 3, 5: 4 |
| Lobby → intro after first pad | 300 ms |
| Intro → fight | 2.9 s |
| Held LEFT for 1 s | fighter[1].x moved 388 units |

The 28% loss reproduces the 27% measured on 2026-09-07 from blueberry. The
wired run proves the relay itself is clean at a steady 30 Hz. The loss is
manufactured by bursts: the host emits two or more frames in one wall-clock
instant during frame-driver catch-up, and the relay keeps the first (oldest)
and discards the rest. On Wi-Fi, link stalls of 80-120 ms batch frames the
same way. Network floor from neo: ICMP 21-70 ms round trip, one-way state
delay p50 28-31 ms.

## Topology

Three programs, one relay hop between any two of them.

- **Host**: `xbox/live/mac-test.html` shell + `oskiewar.js` engine. Owns the
  simulation. Publishes state, receives pads.
- **Relay**: `session-server/oskiewar-live-manager.mjs`, 480 lines,
  synchronous, no timers, no Redis, no auth. One room per match id. Roles by
  query string: `publisher`, `challenger`, `agent`, anything else `viewer`
  (`:158-161`).
- **Guest / spectators**: the same shell + engine, attached through
  `xbox/live/round-room.mjs`. The guest is a spectator that holds the one
  `challenger` seat and is allowed to send pads.

There is no `#guest` mode in the page. Any visitor to `oskiewar.com/<room>`
asks for the challenger chair (`mac-test.html:1312-1316`); a bare visit asks
the matchmaker `GET /oskiewar-open-room` for a live, untimed, chair-free room
published within 10 s, and hosts its own room on a miss or a 2.5 s timeout
(`:1299-1306`, `oskiewar-live-manager.mjs:453-466`). Caddy serves the same
`mac-test.html` for `/` and `/<room>` (`lith/Caddyfile:993-999`).

Role split in the engine (`oskiewar.js:3643-3654`): if a bridge exists, the
engine sets `shellMode = "GAME"`, starts the bridge, and returns from boot
without entering any game mode. Every later sim tick takes the viewer branch
(`:6933-6955`): sample the local pad, toggle debug on View, run
`updateRoundViewer`, `sendChallengerInput`, `updateVersusClaim`, return.
Everything below that return is host-only: player update, camera, bullets,
grenades, impacts, shell, system buttons, publishing, round clock, signals,
drums.

## The wire

**State (host → everyone)** `spectatorState` (`oskiewar.js:1476-1537`).
Envelope `format`, `version`, `seq`, `at` (Date.now, never read by any
client), `phase` in `select|intro|fight|round|match|replay`, `perf`,
`sessionId`, optional `nextRoundId`. Per fighter: `name` (empty chair is
sent as `NOBODY` to pass relay validation), `nation`, `color`, `x y z`,
`vx vy vz`, `facing`, `alive`, `grounded`, `ducking`, `sinking`, `blocking`,
`score`, `roundWins`, `attack`, `removedParts`. Plus `ball`, `balls`, full
camera pose, `wind`, `round {remainingMs, timed, result, cause}`. About
1.27 KB, ~37 KB/s per watcher at 30 Hz. Relay cap 8 KiB.

Not on the wire but read by the guest: `hit`, `blockFlash`, `attackTicks`
(`:3388-3393`). Not on the wire at all: bullets, grenades, detached parts,
impacts, ammo, command stream, stance, hit stun.

**Pads (guest → host)** `sendChallengerInput` (`:3530-3556`), called every
sim tick. Sends `{seq, down[], leftX, leftY, name, colors}` when the pad
changed and at least 33 ms of sim time passed since the last send, otherwise
heartbeats every 250 ms. `View` and `Menu` never leave the guest. The relay
forwards to the publisher only if 15 ms of wall time passed since the last
pad it accepted (`oskiewar-live-manager.mjs:348-355`); anything faster is
dropped silently, and the client marks the frame as sent on socket success
(`:3554`), so a dropped edge is invisible until the next change or the 250 ms
heartbeat. The shell stamps arrival with `Date.now()` into
`globalThis.__oskiewarRemotePad` (`mac-test.html:1030-1035`); the host reads
it fresh each tick with no buffer, no seq check, and treats 2.5 s of silence
as absence (`oskiewar.js:1179`, `:1196-1200`).

**Control frames** `oskiewar:seat`, `oskiewar:status` (`live`, `viewers`,
`agents`, `challenger`, `hasState`), `oskiewar:viewers` (publisher only),
`oskiewar:error`. The publisher is not in `watchers()` (`:428-432`) so it
never receives status; the host learns a guest exists only when a pad
arrives. Handle and colors ride every pad frame for the same reason
(`oskiewar.js:3546-3553`). The generic session-server `joined` / `left`
broadcasts also reach every relay socket (`session.mjs:2205-2216`,
`:3204-3208`); the clients ignore them.

## Host loop

`frame-driver.mjs`: fixed 60 Hz sim, render per display frame, `sample` once
per rAF and per fallback timer tick. `pumpSimulation` (`:132-150`) runs up to
`maxCatchUpTicks = 4` ticks in one call when the display fell behind, then
counts the rest as dropped and re-anchors. A hidden tab runs a 1 s
maintenance timer that pumps up to 240 owed ticks at once (`:158-174`).

`publishVersus` (`oskiewar.js:2610-2621`) runs inside `gameSim`, gated on
`versusSnapshotIntervalUs = 33000` of **sim** time. Two catch-up ticks in one
instant emit two publishes in one instant. The shell's `publishLive`
(`mac-test.html:1004-1085`) parses the payload twice and re-stringifies it
before sending, so each publish costs four serializations of the state on
the host, and it keeps only the latest payload while the socket is opening.

## Relay

`publish()` (`oskiewar-live-manager.mjs:390-422`), in order: size cap, parse,
validate, then

```js
if (now - room.publishedAt < MIN_PUBLISH_INTERVAL_MS) return;   // 25 ms, wall clock
if (room.state && state.seq <= room.state.seq) return;
room.state = state;
for (const watcher of this.watchers(room)) send(watcher, "oskiewar:state", state);
```

Consequences:

- The gate runs before the store and has no trailing flush, so of two frames
  arriving inside 25 ms the **older** one is delivered and the newer one is
  gone. Coalescing adds staleness instead of removing it. The test at
  `oskiewar-live-manager.test.mjs:88-103` pins this as intended.
- The host's gate is sim-clocked and the relay's is wall-clocked. Catch-up
  bursts on the host, or link stalls anywhere, become permanent frame loss
  correlated with exactly the moments the fight is busiest.
- `send()` (`:124-128`) stringifies the whole state once per watcher, in one
  synchronous loop, and yields the challenger **last** after up to 64
  viewers and 4 agents (`:428-432`).
- No `bufferedAmount` check anywhere. A slow guest socket buffers forever
  and never skips to latest.
- Pads: same shape, 15 ms floor, silent drop, per room not per socket.
- Takeover (`:214-217`) nulls `room.state`, so anyone joining between a
  claim and the successor's first frame gets no initial state, and the
  matchmaker never offers that room until a frame lands.
- Liveness is the shared 15 s ping (`session.mjs:1738-1747`); a dead host can
  hold `live: true` for up to 30 s.

## Guest loop

`round-room.mjs` opens one socket per room (`:66-71`) and emits `seat`,
`status`, `state`, `demo`, `round`. `handleRoundViewer` (`oskiewar.js:3475-3481`)
runs in the socket `message` callback: it applies the frame and records
`roundViewerLiveAt = runtime().monotonicUs`. `updateRoundViewer`
(`:3492-3520`), once per sim tick, coasts every alive fighter and active ball
by the last wire velocity while `now - roundViewerLiveAt < 150 ms`
(`liveDeadReckonMaxUs`, `:3490`), then holds still.

What that means for the picture:

- **Extrapolation only, never interpolation.** Each frame is applied the
  instant it lands and then extended on a straight line with no gravity,
  ground or wall. The next frame is a hard correction. At the measured 33 ms
  nominal and 100-130 ms p90-p99 gaps, bodies coast through geometry and
  snap back several times a second.
- **Applied mid-frame.** The callback lands between a sim tick and a paint.
  Render interpolation (`:1038`, `:1061`, captured at `:6918`) captured the pre-frame pose, so the
  next paint lerps partway into the new state by `alpha` and the following
  paint finishes the jump. Every correction is smeared over two paints.
- **Camera snaps.** `cameraDoll.track(..., dt, 1000)` (`:3437-3441`) is an
  effective hard set; nothing moves the camera between frames. The guest sees
  the host's framing as a 30 Hz staircase with the network's jitter baked in.
- **Two clocks.** In the socket callback the shell's `runtime().monotonicUs`
  is `performance.now()` (`mac-test.html:591`, `simulationMonotonicMs` is
  null outside `simulate`/`paint`). In `updateRoundViewer` it is the driver's
  `simulationTime`, which starts equal to `performance.now()`
  (`frame-driver.mjs:241-243`) and then falls behind by every dropped tick.
  So `roundViewerLiveAt` is wall-clocked and compared against sim time: once
  the drift exceeds 150 ms the coast fuse never trips and the guest keeps
  moving bodies on stale velocity through every gap. Drift grows with
  playtime, which matches "sessions degrade" in the perf ledger. Same mixing
  hits `roundStartedAt` (`:3429` vs `:11722`) and `attackStartedAt`.
- **No prediction.** The guest's own fighter is `players[1]`, driven only by
  the echo. The local pad lights the keycap legend and touch discs
  (`:6939-6947`) and nothing else. `drawControlLegend` annotates from
  `players[0].lastButton` (`:8825-8828`), the host's body.

What that means for the HUD and effects:

- **`gameplayStarted` is never true on a guest.** Initial `false` (`:1244`);
  every setter (`:2385`, `:2414`, `:2525`, `:2564`, `:2786`, `:2874`, `:3000`)
  is inside a start/begin function the viewer path never calls. The match HUD
  block (`:11795`) requires it, so the guest has no round clock, no `VS` /
  `WAITING FOR HOST` label, no `YOU ARE <NAME>` seat line, no status tray,
  no update-ready affordance. That is the "no state updates on screen".
- **The intro never plays.** `roundIsTimed()` is false in the versus lane
  (`:1455`), so `remainingMs` is 0 (`:1486-1488`). The guest computes
  `roundElapsedUs = roundDurationUs - 0` and pushes `roundStartedAt` 30 s
  into the past (`:3427-3429`), so `counting` is always false (`:11910`) and
  `drawFightIntro` is skipped (`:11913-11916`). `state.phase` is read for one
  thing only, `matchOver = phase === "match"` (`:3428`); `intro`/`fight`
  never change what the guest draws.
- **No hit VFX in LIVE.** `player.hit = source.hit || 0` is always 0 because
  the wire has no `hit`, so the spark branch (`:3399-3401`) is dead. Limb-loss
  and death impacts are pushed (`:3402-3405`) but `updateResultImpactDebris`
  only runs in DEMO (`:3611`) or host sim (`:7028`, `:7106`), so they never
  grow `debris`, `drawImpacts` (`:11586`) draws nothing for them, and the
  `impacts` array grows for the life of the connection.
- **Silent.** `emitSignal`/`playDrum` are only reached below the viewer
  return. A stored demo is richer than a live match: it carries an impact
  track and hit/block flags (`:3333`, `:3597-3612`).
- **Nameplate palettes are guessed locally** for any non-`.remote` player
  (`:11674-11676`); on the guest neither player is `.remote`.

Lifecycle hazards, guest-only:

- **2.5 s of pad silence resets the match.** `updateVersusSeat`
  (`:2653-2671`): `versusActive() && !fresh` → `beginVersusLobby`; return →
  `startVersusFight(now, true)` with `resetMatch`, `roundWins = 0` (`:3712`).
  A phone switching antennas wipes the score.
- **Reconnect can be dropped.** `RoundRoom.schedule` (`round-room.mjs:177-183`)
  has one timer slot and returns if it is occupied. While the room is not live,
  or after any `phase === "match"` frame (`:116`, `loadDemo(true)`), the
  1800 ms replay-retry loop (`:174`) holds that slot about 95% of the time
  because versus rooms have no stored replay and 404 forever. A socket
  `close` in that window calls `schedule(open, 1200)` and is ignored; the
  bridge never reopens. This is the "reusing one room wedges the guest"
  gotcha from the 2026-09-07 sweep.
- **Seat retake is 4 s + a handshake**, not instant (`:88-101`). The relay
  frees the seat synchronously; the client paces itself.
- **A hidden guest** keeps the seat with a 1 Hz neutral pad from the shell
  (`mac-test.html:1330-1335`, `seq: 0`, does not update
  `versusInputLastSent`), while its engine sender bursts against the 15 ms
  relay floor and mostly loses.

## Latency budget

U = one-way guest↔relay, H = one-way host↔relay. Measured from neo: ~30 ms
one-way p50 to the relay.

| Stage | Guest | Host |
|---|---|---|
| Pad sampled | 0-16.7 ms | 0-16.7 ms |
| `sendChallengerInput` 33 ms floor | 0-33 ms | none |
| Guest → relay → host | U + H | none |
| Relay 15 ms pad gate (drop; 250 ms heartbeat recovers) | 0, tail 250 ms | none |
| Host applies at next tick | 0-16.7 ms | 0-16.7 ms |
| Host publish 33 ms gate, relay 25 ms gate | 0-33 ms | none |
| Host → relay → guest | H + U | none |
| Paint | 0-16.7 ms | 0-16.7 ms |
| **Mean, U = H = 15 ms** | **~116 ms** | **~25 ms** |
| **Mean, U = 70 ms (cellular)** | **~226 ms** | ~25 ms |

Beyond ~150 ms of one-way jitter the coast fuse freezes poses mid-stride.
Nothing on the guest hides any part of this budget.

## What was changed

Items 3, 4, 5 and 8 below are answered by the rollback lane in part two: the
guest now has the HUD and the intro because it runs the round itself, it does
not interpolate a stream because there is no stream to interpolate, it sees
every effect because it spawns them, and its own fighter is locally
simulated. Items 1, 2, 6 and 7 are still open and still apply to spectators,
who continue to watch the streamed lane.

## The original ranking, kept for the spectator lane

1. **Relay: store-then-flush-latest.** In `publish()`, always
   `room.state = state` after the seq check; if inside the 25 ms window,
   arm one trailing `setTimeout` to fan out whatever is newest. Turns 25-57%
   permanent loss into a bounded 25 ms coalesce. Stringify once per fanout.
   Yield the challenger first. About 20 lines, one test to flip
   (`oskiewar-live-manager.test.mjs:88`).
2. **Host: publish on wall time, from paint.** Move `publishVersus` to the
   paint callback or gate it on `run.unixMs`, so catch-up bursts emit one
   frame. Drop the double parse in `publishLive`.
3. **Guest: set `gameplayStarted` and stop trusting `remainingMs`.** Set it in
   the viewer boot branch (`:3643-3654`) and in `updateVersusConflict`. Derive
   `roundStartedAt` from `phase` edges (`intro` → now) instead of a clock the
   versus lane zeroes. This alone restores the clock lane, the seat line, and
   the intro card. Also `matchOver`/`roundResult` should not depend on it.
4. **Guest: apply frames on the tick, interpolate between the last two.**
   Queue arriving frames; in `updateRoundViewer` render ~50-70 ms behind the
   newest frame and lerp between the two bracketing frames, extrapolating
   only past the buffer. Use one clock (`simMonotonicUs`) for
   `roundViewerLiveAt` and `roundStartedAt`. Removes the popping, the
   mid-paint smear, and the drift bug.
5. **Wire: carry the flags the guest already reads.** Add `hit`,
   `blockFlash`, `attackTicks` per fighter (about 30 bytes) and let the guest
   run `updateResultImpactDebris` in LIVE. Emit the same signal names the host
   plays so the guest is not silent.
6. **Pads: send edges immediately, ack or resend.** Bypass the 33 ms floor
   for a changed `down[]`; resend an unchanged frame after 50 ms once instead
   of waiting 250 ms. Relay: accept a pad whose `down` differs from the last
   accepted even inside 15 ms.
7. **Bridge: give `schedule` two slots** (replay retry and reconnect) or make
   `closed()` clear the timer first. Stop `loadDemo(true)` looping on
   versus rooms that have no replay.
8. **Later: local prediction for the guest's own fighter** (run the
   movement sim for `players[1]` locally, reconcile on host frames), and
   eventually a WebRTC data channel so the host and guest are one hop apart
   with the relay as fallback. Both are real projects, not patches.

---

# Part two: the rollback lane (v102)

Two current builds play the fight the way Street Fighter 6 does. Both machines
run the same simulation from the same start, each schedules its own pad two
frames ahead and mails it, and each simulates the current frame with the
rival's last known pad standing in for one that has not arrived yet. When the
real pad lands and differs, the machine rewinds to a snapshot of the frame
before, replays the frames in between in silence, and carries on. Nobody
watches anybody's stream, so there is no host advantage left to feel.

## What it needed from the game

Rollback needs a simulation that is a pure function of (frame, both pads) and
a snapshot of everything that simulation touches. The pieces already present:
a fixed 60 Hz tick (`frame-driver.mjs`), a 14-bit input word (`inputCommand`),
a re-simulation path for replays (`resimPad`, `advanceResimCommands`), seeded
name and bot dice, and terrain seeded from a constant string. What had to be
added or corrected:

- **One clock.** `runtime()` reports the frame's own time while a frame is
  simulated, and the current frame plus render alpha while painting. The sim
  no longer reads the wall clock, so a rewound frame reads the same time it
  read the first time.
- **A shared origin.** The host proposes frame zero's timestamp, rounded up to
  a tick, in the opening deal. Both seats set `startedAt` from it, so the limb
  poses that collide are phased identically. Limb geometry is the hitbox here,
  so this was load-bearing, not cosmetic.
- **Silence on replayed frames.** Drums, signals, telemetry, analytics, frame
  telemetry and publishing are suppressed while re-simulating. A rewind
  changes state and nothing else.
- **A snapshot list.** `netSimScalars` names the ~46 scalars the round depends
  on; `netSimArrays` names the twelve collections (fighters, balls, bullets,
  grenades, impacts, detached parts, pickups, the tile field, and the rest).
  Restore is in place: the live arrays keep their identity, and keys the sim
  grew since the snapshot are deleted rather than left behind.
- **Identity from the deal.** `resetRound` re-dresses fighters from the local
  account, which on the challenger's machine is the challenger. Every reset
  now re-applies the deal's names and colors, so both screens agree on who is
  who across round rollovers.
- **Local hands.** The keycap legend, touch discs and move names follow the
  seat the person is actually holding, which for the challenger is fighter
  two.

## The wire

A new relay message type, `oskiewar:net`, carries a small JSON object from one
seat to the other and to nobody else. The relay checks the size (2 KiB) and
that the sender holds a seat; what the two games say is theirs. It is never
rate limited, because a dropped input packet is a rollback the other seat has
to eat. Pads for the streamed lane still ride the same socket under their own
tighter cap, so nothing about spectators or older builds changed.

Three packet kinds:

| kind | from | carries |
|---|---|---|
| `hello` | challenger, once a second until answered | handle and wardrobe |
| `start` | host, once | frame zero's clock, input delay, ball type, both fighters' names and colors |
| `i` | both, every frame | the last ten input words, the newest frame heard from the rival, this seat's frame, and periodically a state hash |

Every input packet re-sends the last ten frames, so a lost packet costs
nothing unless several vanish in a row. Each seat hashes its own state at the
newest frame both pads are known for and mails it; a mismatch is counted and
reported as `NET_DESYNC` rather than silently drifting.

## Constants

| name | value | why |
|---|---|---|
| `NET_INPUT_DELAY` | 2 frames | 33 ms, about a display's own lag; a rival packet arriving inside it costs no rollback at all |
| `NET_MAX_ROLLBACK` | 8 frames | 133 ms of one-way silence, past the relay's measured p99; beyond it a seat holds instead of guessing |
| `NET_REDUNDANCY` | 10 frames | packet loss recovery without an ack protocol |
| `NET_PEER_LOST_MS` | 4000 | wall-clock silence after which the rival is gone |

A seat that runs more than a delay's worth ahead of where the rival says they
are gives back one tick in four until they are level, so neither machine
accumulates an advantage.

## Opening and falling back

The challenger's bridge says hello on the net channel; the host answers with a
deal and both fall into the same fight, from the lobby or from a streamed
fight one second old. If no hello ever arrives, the host starts the streamed
fight exactly as before. Deployment order does not matter: an old relay drops
the packets, an old game ignores them, and either way both sides land on the
streamed lane. A seat that leaves says so, so the rival does not eat the
four-second timeout.

The host keeps publishing the 30 Hz spectator stream throughout, so the
grandstand is unchanged. The challenger never publishes, so the two seats
never race for the room's one publisher socket.

## Measurements

Unit harness, two seats in one process over a wire with 4 ticks of delay,
3 ticks of jitter and 10% loss, 2400 frames including round rollovers:

| metric | value |
|---|---|
| Desyncs | 0 |
| Rollbacks | 99, deepest 3 frames |
| Snapshot cost | 0.069 ms/frame |
| Re-simulation cost | 0.012 ms/frame |

Two real browsers, the real shell and bridge, a local instance of the real
relay, 14 seconds of two-handed play:

| metric | host | guest |
|---|---|---|
| Frames simulated | 1130 | 1129 |
| Rollbacks (deepest) | 10 (5) | 0 (0) |
| Desyncs | 0 | 0 |
| Snapshot cost | 0.140 ms/frame | 0.098 ms/frame |

Both seats' state hashes agreed at every frame they shared. Screenshots taken
from each browser mid-fight show the same fighters in the same places with the
same nameplates and command streams.

One caveat worth knowing before reading a bad number: give each seat its own
browser window. A background tab's frame driver parks and settles its owed
ticks in one-second bursts, so two tabs in one window spend most of their
ticks holding for each other and read like a netcode fault that is not one.

## Known limits

- **Same engine only.** Transcendental functions are not bit-identical across
  V8, JavaScriptCore and the console's QuickJS, and limb poses collide. Two
  browsers on the same engine family are fine; a browser against the native
  console shell is not yet proven and should be expected to desync.
- **Console budget.** Snapshot cost was measured on desktop browsers. The
  console frame is already raster-bound at 16 to 45 ms, so measure there
  before trusting an 8-frame window.
- **Spectators still watch the streamed lane**, with all of part one's
  limitations.
- **Desyncs are detected, not repaired.** A mismatch is counted and reported;
  there is no resynchronisation path yet.

## Method

- `node xbox/tools/oskiewar-relay-probe.mjs 60 60 8` from any box.
- Host a room: headless Chrome on `https://oskiewar.com/<room>`, room
  matching `^[a-z]{4,7}[0-9]{1,3}$`; then
  `node xbox/tools/oskiewar-challenger-probe.mjs <room> 45 10000`.
- `npm run xbox:test:oskiewar:netplay` for the two-seats-in-one-process suite,
  `npm run xbox:test:oskiewar:netplay:browser` for the two-browser end to end
  (add `-- --shots <dir>` for a frame from each seat, `-- --headful` to watch).
- The relay unit tests run only by hand:
  `node --test session-server/oskiewar-live-manager.test.mjs`.
- The engine suite has 37 pre-existing failures unrelated to any of this
  (`node --test xbox/live/tests/oskiewar.test.mjs`). Diff failure names
  against that baseline rather than trusting the exit code.
- Prior two-window recording method and the 2026-09-07 numbers are in the
  session memory note `oskiewar-multiplayer-e2e`.
