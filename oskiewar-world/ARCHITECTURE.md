# oskiewar-world-1.0

The multiplayer and game-service architecture for OSKIEWAR: an authoritative
tick server with client-side prediction and server reconciliation, a world you
are dropped into rather than a menu you navigate, and a control plane that runs
somewhere else entirely.

Every platform-capability claim below is sourced, with the date it was read.
Every performance number comes from a probe in `tmp/oskiewar-world/`, run on
this machine against `xbox/live/oskiewar.js` at commit `2fcb1b6f4`.

---

## 1. The measurement that came first

The uncertain thing was whether the sim can run headless at a fixed tick,
decoupled from wall clock, and land in the same place twice. If it can, the
server is a re-simulation of the same code the client runs and reconciliation
is cheap. If it can't, the server has to become a separate authority and the
client has to be corrected by interpolation — the thing the replay system
already does with 1-second checkpoints, which is what you build when you can't
trust re-simulation.

It can. The probe loads `oskiewar.js` into a `new Function` sandbox with stub
hosts — the same trick `xbox/live/tests/oskiewar.test.mjs` already uses — and
drives `sim()` by advancing `runtime().monotonicUs` exactly 16,667 µs per call.
No wall clock is consulted anywhere in the loop.

### Results

| Test | Result |
| --- | --- |
| Same inputs, fixed dt, two instances, 30 s | **bit-identical** |
| Same, 90 s, 5 input seeds, `Math.random` unseeded | **2 of 5 diverge** |
| Same, 90 s, 5 input seeds, `Math.random` seeded | **5 of 5 bit-identical** |
| Re-simulation from the 16-bit command mask, 90 s × 5 seeds | **gameplay bit-identical**; one HUD field differs |
| Two separate Node processes, 60 s | **identical SHA-256** (`3d6c0217e449…`) |
| `sim()` + `paint()` vs `sim()` alone | diverges at t=3.97 s — **camera only** |
| Wall-clock dt vs fixed dt | diverges at tick 0 |

Reproduce: `node tmp/oskiewar-world/wire.mjs 5400 seeded`,
`node tmp/oskiewar-world/budget.mjs hash`,
`node tmp/oskiewar-world/nondeterminism.mjs 1 5400`.

### The single divergence, and where it enters

Seeds 1 and 7777777 diverged at tick 3559 (t=59.32 s). The diff:

```
balls[0].type:       basketball  ->  soccer
balls[0].radius:     42          ->  38
balls[0].lastHitBy:  1           ->  0
match ids A: ow-tuzzu835 ow-thikke939
match ids B: ow-buvva169 ow-shammy583
```

At every round transition `pronounceableMatchName()` (`oskiewar.js:487–498`) calls
`Math.random()` five times to build a round name. `seriesBallType()`
(`oskiewar.js:1533`) then hashes that name to pick the match ball — and ball type
carries radius and mass. The comment above it reads "never the clock or
Math.random," and it is wrong: the name it hashes is a `Math.random` product.
One PRNG laundered through FNV-1a into the physics.

That is the whole nondeterminism budget of the sim. Swapping `Math.random` for
a seeded stream makes all five seeds bit-identical for 90 seconds across round
boundaries. Cost of the fix: seed the name generator from the match ID the
server already assigns.

### What the wire has to carry

The 16-bit command mask `recordReplayCommands()` already writes into replays is
sufficient. Reconstructing pads from the mask alone and re-running produces a
bit-identical gameplay state over 90 seconds on every seed. The only field that
differs is `player.lastButton`, the HUD's last-pressed label, which changes when
two buttons go down on the same tick — display only, nothing downstream.

So the uplink is **2 bytes per player per tick**, 120 B/s at 60 Hz, and the
existing replay format is already the network format.

### Paint mutates the camera

`sim()`+`paint()` diverges from `sim()` alone at tick 238, and every differing
field is under `camera.doll`. Gameplay never differs. A headless server that
never calls `paint()` is authoritative over everything that matters and has no
opinion about the camera — which is correct, because the camera is per-viewer
and belongs to the client.

### Budget

| | |
| --- | --- |
| CPU per tick, 2 players, warm | **27.66 µs** |
| Real-time 60 Hz sims per core | **603** |
| Headroom per tick | **603×** |
| Re-simulating 600 ticks (10 s of rollback) | 31 ms |
| Current JSON spectator snapshot | 1,378 B mean (1,152–1,443) |
| Per-fighter share of that snapshot | 265 B |

The sim is nowhere near the constraint. A single core can hold 600 concurrent
two-player worlds in real time, and rolling back and re-simulating ten full
seconds costs 31 ms. Reconciliation is affordable by a wide margin.

### What this does not prove

The probe ran Node v24.18.1 on one machine. It does not prove bit-identical
results between Node on the server and QuickJS on the console, or between x86
and ARM. `oskiewar.js` makes 84 transcendental calls (`Math.sin`, `Math.cos`,
`Math.atan2`, `**`), and those are the one place IEEE-754 does not guarantee
agreement across implementations. QuickJS was not available on this machine to
test against.

**It does not need to.** With one authoritative server there is one truth, and
clients are corrected toward it. Cross-engine determinism would only be
required for lockstep peer-to-peer, which is not what this is. What the
measurement buys is that *reconciliation by re-simulation* works — the server
can replay a client's inputs and get the client's answer, so corrections are
rare and small instead of constant.

---

## 2. Topology

Three planes, deployed and failed independently.

**The control plane** is stateless HTTP plus a small amount of durable state.
It answers: who are you, which shard should you join, what is your rating, and
here is a signed ticket. It records results after the fact. It never sees a
tick. It lives on Cloudflare.

**The shard** is one Node process holding one world: up to eight fighters, a
60 Hz fixed-timestep loop, and a socket per player. It is the only authority
over game state. It boots from a signed ticket, runs, and posts results back to
the control plane. It lives on a UDP-capable box in a region near its players.

**The client** — browser, AC Native, or Xbox — runs the identical sim locally,
predicts its own fighter forward from local input, and corrects when the server
disagrees. It also renders, which the server never does.

The flow:

```
  client                     control plane                shard
    |                       (Cloudflare Workers)      (Node, UDP box)
    |-- POST /session ------------>|                        |
    |   AC handle + token          |                        |
    |<-- ticket + shard addr ------|                        |
    |                              |                        |
    |-- connect(ticket) ------------------------------------>|
    |<-- world snapshot + your entity id --------------------|
    |                              |                        |
    |== 2 B input, tick n =================================>| 60 Hz sim
    |<= binary snapshot, ack'd input tick ==================| 20 Hz send
    |   [predict forward, reconcile on ack]                 |
    |                              |                        |
    |                              |<-- encounter result ---|
    |                              |    (signed, HMAC)      |
    |                              | [Glicko-2 update]      |
    |                              | [replay to Spaces]     |
```

Spectators keep the path they already have: the shard publishes the same
`ac.oskiewar.live` state it publishes today, and `oskiewar-live-manager.mjs`
relays it to phones. That subsystem does not change except to widen past two
fighters.

### Why authoritative, structurally

The player count settles this on its own. Peer-to-peer lockstep needs every
peer's inputs for frame *n* before any peer can advance to *n*, which means the
worst link sets everyone's pace, and connections grow as O(n²) — 28 links at 8
players, 120 at 16. Client-server is O(n) connections and degrades per player:
one bad link makes one player rubber-band, and nobody else notices. Quake and
Overwatch are N-player *because* they are client-server. Rollback netcode
between peers is a 1v1 technique and does not survive the pivot to a shared
world.

---

## 3. Transport

### Decision

**Ship three implementations behind one interface, in this order.**

1. **WebSocket over TLS** — the fallback, and the first thing to build.
2. **WebRTC DataChannel, unreliable and unordered** — the recommendation.
3. **WebTransport datagrams** — the successor, gated on server-side maturity.

The shard's transport layer is `send(playerId, bytes)` and
`onMessage(playerId, bytes)`. Everything above it is transport-agnostic. That
is not gold-plating: all three of these are real, none of them is obviously
permanent, and the packet budget is the same for all three.

### WebSocket first

The tick server, prediction, and reconciliation are the hard parts, and none of
them depend on the transport. WebSocket works in every browser, traverses every
firewall, needs no signaling, and the session server already runs one. Building
the loop on WebSocket makes step 1 verifiable end-to-end before any WebRTC
exists.

Its cost is real: TCP head-of-line blocking. A dropped packet stalls every
snapshot behind it until retransmission, which at a 200 ms RTT is a 200 ms
freeze rather than one skipped frame. That is why it is the fallback and not
the destination — but a fighting game that works with an occasional freeze is
strictly better than one that does not exist.

### WebRTC DataChannel is the recommendation

Set `ordered: false, maxRetransmits: 0` and the channel is unreliable and
unordered over SCTP/DTLS/UDP — exactly the semantics a 60 Hz snapshot stream
wants, where a late packet is worthless and should be dropped rather than
delivered. Both options are normative in the W3C WebRTC Recommendation of
13 March 2025 (accessed 2026-08-07): `ordered` "if set to false, data is
allowed to be delivered out of order"; `maxRetransmits` "limits the number of
times a channel will retransmit data if not successfully delivered."
`RTCDataChannel` is Baseline Widely Available, in every browser since January
2020 (MDN, accessed 2026-08-07).

This is prior art in this codebase. `session-server/` runs Geckos.io, which is
WebRTC DataChannels between browser and Node server — client-server, not P2P —
and its defaults are `ordered = false, maxRetransmits = 0`
(`packages/server/src/wrtc/connectionsManager.ts:57–73`, accessed 2026-08-07).

**Use `node-datachannel` directly, not Geckos.io.** Geckos.io's server is a
wrapper over `node-datachannel` (libdatachannel), pinned at `0.32.1`. Geckos.io
is at `@geckos.io/server@3.1.0`, published 2026-03-27, last commit 2026-03-27,
no published GitHub releases, ~934 weekly npm downloads. Its dependency
`node-datachannel` is at `0.32.3` (2026-04-26) with ~64,000 weekly downloads
(npm registry and GitHub API, accessed 2026-08-07). The wrapper is a year
behind its own upstream and adds a room abstraction this design does not want.
Take the healthy dependency.

The cost is signaling. WebRTC needs an offer/answer exchange and ICE
candidates before a channel opens. That is a Worker endpoint and about a
hundred lines — and Cloudflare offers managed TURN on 3478/udp for the clients
that cannot get a direct path (Cloudflare Realtime TURN docs, accessed
2026-08-07).

### WebTransport is close, and its server side is not

The browser side is genuinely ready. WebTransport is Baseline 2026 as of March
2026, at 89.96% global support: Chrome 97+, Edge 98+, Firefox 114+, and — the
change that matters — Safari 26.4 and Safari iOS 26.4, shipped 2026-03-24
(caniuse, MDN browser-compat-data, and the WebKit Safari 26.4 release post, all
accessed 2026-08-07). Datagram support ships wherever WebTransport ships;
`WebTransportDatagramDuplexStream` is Baseline 2026 (MDN, accessed 2026-08-07).

The server side is not there.

- Node core has `node:quic` in the source tree at "Stability: 1.0 – Early
  development," added v23.8.0, behind `--experimental-quic` — and it is a
  **build-time** option in `configure.py`, not just a runtime flag. The docs are
  not published on nodejs.org at all. Verified empirically on this machine,
  2026-08-07: Node v24.18.1, `node --experimental-quic -e
  "require('node:quic')"` returns `No such built-in module: node:quic`. You
  cannot use it without compiling your own Node.
- `@fails-components/webtransport` (1.6.7, 2026-07-25, ~43,300 weekly
  downloads) is the only real userland option. Its own README says the HTTP/3
  package "should be considered as a duct tape-style solution until a
  bulletproof native support of HTTP/3 and WebTransport is provided by node
  itself," and lists `maxDatagramSize` among the unimplemented properties —
  which is precisely the API you need, because neither the W3C spec nor Chrome
  publishes a fixed datagram cap and both defer to runtime MTU discovery
  (W3C WebTransport CR Snapshot 30 July 2026; Chrome WebTransport docs; both
  accessed 2026-08-07). Oversized datagrams are silently dropped, not errored.

Revisit when Node ships QUIC in official builds. The interface makes that a
swap, not a rewrite.

### Xbox native uses raw UDP

GDK titles get Winsock. "The Microsoft Game Development Kit (GDK) supports the
use of the Windows Sockets 2 (Winsock) API… generally the same as the way Win32
programs interact with Winsock" (Microsoft Learn, GDK Winsock introduction,
accessed 2026-08-07). No secure-device-association layer is imposed, but
security is the title's problem: GDK titles "are responsible for ensuring that
there's appropriate security and encryption for all data," and the docs direct
titles to implement DTLS. MsQuic is Microsoft's sanctioned option.

Three GDK constraints propagate upward into the design of every transport:

- **Design for a maximum UDP payload of 1,384 bytes.** Titles must set
  `IP_DONTFRAGMENT` and `IP_USER_MTU=1384`. This becomes the packet budget
  everywhere, because the wire format should be identical on all three
  transports.
- There is a **preferred local UDP multiplayer port**, the only port that
  admits inbound UDP without firewall punching.
- The kernel network stack has a **~16 MB total budget** across all Winsock and
  WinHTTP use, "after which point the system may become unstable."

XR-067 is already satisfied by reporting rather than replacement: online
multiplayer must maintain session state on the Xbox network via MPSD *or*
record player interactions through Multiplayer Activity Recent Player. A
self-hosted shard survives; it owes the network a report
(`papers/arxiv-oskiewar-store/store.tex:296`).

### Packet budget

1,384 bytes is the ceiling. At 8 players a packed binary snapshot — position
and velocity as int16, one 32-bit flag word, one state byte per fighter — is
**160 bytes**, measured against the shape of the current spectator state. That
leaves room for balls, projectiles, and events inside one datagram with no
fragmentation on any transport.

| Players | Snapshot | Per client @20 Hz | Server egress @20 Hz | P2P links |
| --- | --- | --- | --- | --- |
| 2 | 46 B | 0.9 KB/s | 0.01 Mbit/s | 1 |
| 4 | 84 B | 1.6 KB/s | 0.05 Mbit/s | 6 |
| **8** | **160 B** | **3.1 KB/s** | **0.2 Mbit/s** | 28 |
| 16 | 312 B | 6.1 KB/s | 0.8 Mbit/s | 120 |
| 32 | 616 B | 12.0 KB/s | 3.2 Mbit/s | 496 |

The current 1,378-byte JSON snapshot is nine times the binary form at the same
player count. Keep JSON for spectators, where a phone reads it and latency does
not matter. Use binary for players.

---

## 4. Hosting

### Cloudflare cannot run the tick loop

No Cloudflare product runs custom server logic over UDP. Three converging
primary sources, all accessed 2026-08-07:

- The Workers protocols page lists HTTP/HTTPS, outbound TCP sockets, inbound
  WebSockets, inbound HTTP/3, and SMTP. **UDP is not listed.**
- `connect()` is TCP-only; the runtime API page describes only TCP application
  protocols.
- `node:dgram` — Node's UDP module — is an explicit **non-functional stub** in
  Workers: "A stub can be imported or required, but does not provide a working
  implementation of the underlying Node.js API… not suitable for direct use in
  application code."

Flag: Cloudflare publishes no sentence saying "Workers cannot use UDP." The
conclusion is inferred from those three, and the `node:dgram` stub is the
strongest single piece. I am confident in it and am telling you it is an
inference.

The adjacent products do not rescue it. Spectrum proxies TCP/UDP to *your*
origin — you still need the origin, and Enterprise plus a paid add-on to get
it, and it drops fragmented UDP packets at the edge. Cloudflare Realtime is a
media SFU: "efficiently routes video and audio," no application logic.
Cloudflare TURN is a relay. All three still require a UDP server you own.

Durable Objects do give WebSockets, for "thousands of concurrent clients," with
a Hibernation API that keeps clients connected while the object is out of
memory and stops billing duration during hibernation. That is genuinely useful
— for the WebSocket fallback tier, for spectator fan-out, and for the
matchmaking queue. It is TCP, so it carries head-of-line blocking, and a code
deploy disconnects every live socket.

### The split

**On Cloudflare** — Workers, Durable Objects, D1:

- Identity: AC handle verification, session tokens, shard tickets.
- Matchmaking: the queue, as one Durable Object per region.
- Rankings: Glicko-2 state and the ladder, in D1.
- Result recording and replay ingestion.
- WebRTC signaling: offer/answer and ICE exchange.
- Shard directory and health.
- The spectator fan-out, eventually — a Durable Object per match does what
  `oskiewar-live-manager.mjs` does today, with hibernation, and gets the live
  relay off the session server.

**Not on Cloudflare** — the tick loop, and nothing else.

This satisfies the independence constraint directly. Nothing here shares fate
or deployment with `session-server/`.

### Where the shard runs, honestly

Recommendation: **jasellite as region 1, with the shard built as a placeable
process from day one.** It is always-on, tailnet-attached, and already a
services appliance. It can hold hundreds of concurrent worlds — the sim needs
27.66 µs per tick and one core sustains 603 real-time worlds.

And a single box in NYC is a latency problem for most of the planet. p50 RTT
from us-east-1, the closest published proxy for DigitalOcean NYC (cloudping.co,
1-day p50, accessed 2026-08-07; DigitalOcean publishes no equivalent table):

| To | RTT | Frames at 60 Hz |
| --- | --- | --- |
| us-west-2 (Oregon) | 65.0 ms | 3.9 |
| London | 77.4 ms | 4.6 |
| Frankfurt | 92.9 ms | 5.6 |
| São Paulo | 114.3 ms | 6.9 |
| Tokyo | 149.6 ms | 9.0 |
| Mumbai | 188.3 ms | 11.3 |
| Sydney | 211.9 ms | 12.7 |

Before jitter, before server processing. Section 5 measures what a fighting
game tolerates: past ~100 ms of prediction the client is regularly wrong by
more than a punch reach. A Sydney player on an NYC shard is not playing the
same game as a New York player, and no amount of prediction fixes it.

So: one region to prove the loop, and the control plane assigns shards by
measured RTT from the first day, even when there is only one shard to assign.
When there are players in Europe, add a second box; the shard is a single
process with a signed ticket and no local state worth keeping, so adding a
region is provisioning, not engineering. The design that fails is the one where
"jasellite" is hardcoded anywhere above the transport layer.

---

## 5. Prediction, reconciliation, and how much of it

The client predicts **its own fighter** from local input, immediately, at the
same fixed 60 Hz tick. It buffers its inputs by tick number. When the server
acknowledges tick *n*, the client discards inputs at or before *n*, snaps its
authoritative state to the server's, and re-simulates its unacknowledged
inputs forward. Section 1 measured that re-simulation is exact and that 600
ticks of it costs 31 ms — so the reconciliation budget is never the problem.

Other players are a different question, and the probe answers it. Freeze one
fighter's input at its last known value for N ticks, hand the truth back, and
measure how far the two worlds are apart. Swept across 41 start points in a
30-second fight (`node tmp/oskiewar-world/drift.mjs`):

| Mispredicted | Position error p95 | Max | Samples past punch reach (58 u) |
| --- | --- | --- | --- |
| 1 tick / 17 ms | 0 u | 17.7 u | 0% |
| 3 ticks / 50 ms | 0 u | 20.4 u | 0% |
| **6 ticks / 100 ms** | **53 u** | 81.6 u | **4.9%** |
| 12 ticks / 200 ms | 159 u | 204 u | 24.4% |
| 18 ticks / 300 ms | 265 u | 333.6 u | 36.6% |
| 30 ticks / 500 ms | 630 u | 645.9 u | 55.0% |

Calibration: `walkSpeed` is 1,060 world units/second and `PUNCH.reach` is 58
units. One tick of wrong horizontal input is 17.7 units of error; four ticks is
a whiffed punch.

That gives a hard design rule. **Predict other players forward for at most 6
ticks — 100 ms — and interpolate them from buffered snapshots otherwise.** Past
100 ms the client's picture of the opponent is regularly wrong by more than the
shortest attack reach, which is exactly the error a player experiences as "I
hit them and nothing happened."

Concretely:

- Your own fighter: predicted, reconciled, never interpolated. It must respond
  on the frame you press the button.
- Other fighters: rendered ~100 ms in the past, interpolated between the two
  most recent snapshots. Never extrapolated beyond 100 ms — freeze the pose and
  let them stand still rather than run them into a wall they never entered.
- Hit resolution: server-side only, on the server's own timeline. No lag
  compensation rewind in v1. Rewind favours the shooter, and in a game about
  reading a body it makes blocks feel broken. Revisit only with data.
- Corrections: snap when the error exceeds ~60 units (one punch reach), smooth
  over 100 ms below that. Errors under 20 units are one tick of drift and
  should be absorbed silently.

---

## 6. Identity

One account, keyed by AC handle. An AC handle is required to play. There is no
guest tier and no anonymous-to-claimed merge.

You do not pick a character. You *are* one. Your handle is your name, your
handle colors are your colors, and there is no pal-select screen. Account
identity, display name, and character are the same object.

```
  account
    handle        "@jeffrey"          -- the primary key. immutable while held.
    sub           Auth0 subject       -- how you prove it
    colors        derived from handle -- your fighter's palette, everywhere
    rating        Glicko-2 (r, RD, σ) -- section 8
    platforms[]                       -- presentation, not identity
```

`platforms[]` is where the console constraint lives, and only there:

```
  { platform: "xbox", xuid, gamertag: "SomeGamertag" }
  { platform: "web" }
  { platform: "native" }
```

### The Xbox split

XR-046 is one sentence: "On Xbox consoles, titles must use the gamertag as
their primary display name." Store Policy 10.13.5 says the same independently.
Read alone that deletes the handle.

XR-007 provides the shape: content from non-Xbox-network users must be either
anonymous and publisher-curated, or "displayed using the publisher's central
account name … clearly distinguishable from the user's Xbox network account
name." An AC handle is a publisher-managed central account name. So the
compliant arrangement is not deletion, it is **demotion**: gamertag primary,
`@handle` alongside as a visually distinct secondary badge. (Sources:
`papers/arxiv-oskiewar-store/store.tex:282–284`, citing Xbox Requirements
v16.3 and Microsoft Store Policies v7.19, read 2026-08-07.)

That yields exactly one rule, enforced in one place:

| Surface | Primary | Secondary |
| --- | --- | --- |
| Web, AC Native | `@jeffrey` in handle colors | — |
| Xbox console | `SomeGamertag` | `@jeffrey`, distinct treatment |

The server never sends a display string. It sends a handle and a platform-link
set; the client renders whichever the platform requires. Identity stays one
thing and presentation is a client-side function of it. Nothing in the sim, the
wire format, the replay format, or the ranking system knows what a gamertag is.

The gamertag has one non-cosmetic job: on Xbox it is the account's proof of
Xbox-network membership, used to satisfy XR-067 reporting. It never keys
anything.

### Linking

A player signs in to AC once, with the handle they already have. On Xbox they
sign in to Xbox first, as the platform requires, then link the handle in-world
— behind the curtain described in section 7, so the world stays visible while
they do it. The link is `xuid → handle`, one to one in both directions.

Someone with no handle cannot play, and that is the point: it is one fewer
identity system, and — see section 8 — it is most of the smurf defense.

---

## 7. The world

There is no menu.

On login you are in the world, standing in front of the dummy. The old title
screen becomes a place: a map with terrain, edges, and regions you can walk to.
Authentication is a curtain drawn over that world, following the pattern in
`system/public/aesthetic.computer/disks/prompt.mjs` — `showLoginCurtain`
(`prompt.mjs:5133`) does not replace the scene, it dims text to 50% opacity and
keeps everything behind it visible and running. Do the same here: the world
simulates and renders while you sign in, at reduced contrast, and the curtain
lifts rather than a screen transitioning.

### Navigation is geography

Where you stand decides who you fight. Not a menu, a map.

- **Center** — the dummy. Always there, always available, never leaves. This is
  the tutorial, the warmup, and the default. You are put here.
- **Walk to an edge or region** — escalate. One region is the bot. Another is
  the matched-opponent region: entering it enqueues you.
- **A matched opponent walks in.** No loading screen, no versus card. The
  control plane pairs you, the shard spawns them at the region's entrance, and
  they walk to you. Waiting is diegetic — you are standing in a place, and
  someone arrives.
- **Leaving the region is leaving the queue.** Walking away is the cancel
  button.

This is what makes an authoritative server load-bearing rather than a
preference. In a shared persistent world, everyone must agree on where everyone
is standing, continuously, before any encounter exists. There is no round
boundary to resynchronize at.

### Encounters

The world is persistent; combat happens inside it. An **encounter** is a
resolved pairwise engagement between two players, bounded in time, with a
winner. The shard opens one when two players meet under whatever the rules turn
out to be, runs the existing round logic inside it, and closes it with a result
posted to the control plane. Everything else in the world keeps simulating.

This is the assumption the whole ranking design rests on, and it is stated
plainly in section 8 so it can be argued with.

---

## 8. Player cap: eight

Derived, not preferred.

**The sim is not the constraint.** 27.66 µs per tick at 2 players, against a
16,667 µs budget — 603× headroom. Pairwise hit checks grow as O(n²): 8 players
is 28 pairs against 1, so even if *all* the cost were pairwise the tick lands
near 430 µs. Still 38× headroom. At 32 players it is 496 pairs, ~13 ms, and
only then does the tick get tight.

**Bandwidth is not the constraint.** A packed 8-player snapshot is 160 bytes;
at 20 Hz that is 3.1 KB/s per client and 0.2 Mbit/s of server egress. 32
players is 12 KB/s per client, which is still nothing. Every one of these fits
inside the 1,384-byte GDK datagram ceiling with room to spare.

**The camera is the constraint**, and so is legibility. Eight fighters on a
12,000-unit stage is the point past which you cannot tell whose limb is whose.
Smash Bros. caps at 8 and uses off-screen indicators for exactly this reason.

Recommend **8 simultaneous fighters per shard**. It costs nothing on either
budget, it is the largest number that stays readable, and it is a number with a
long precedent. If a shard fills, the control plane opens another and places
new arrivals there.

At 16 the transport does not break but the camera does, and the response would
have to be a bigger world with regions that never see each other — which is a
different game.

---

## 9. The camera, which contradicts today's work

Today's camera rect-packs both fighters' hitboxes to fill the frame, and it
took the action from 51% to 83% of action-safe height. It is the best change of
the day. It is also unconditionally two-player: `fighterFrameRect()`
(`oskiewar.js:1873`) packs a rect around the fighters and `cameraDoll.track()`
frames it.

With eight players spread across 12,000 units, framing everyone means everyone
is 3% of the frame. You cannot have both. State it plainly:

**The camera follows you. Others are allowed off-screen, with edge
indicators.**

And the rect-pack survives, scoped. When you enter an encounter, the camera
becomes the encounter camera — the rect-pack, exactly as shipped, around the
two of you. When the encounter closes, it releases back to a follow camera.

That is not a compromise, it is the better version. The camera change *is* the
transition. You feel an encounter begin because the frame tightens onto two
bodies, and you feel it end because the world opens back up. The shipped
work becomes the payoff of the geography design instead of a casualty of it.

The camera is per-viewer and lives entirely on the client — the probe confirmed
the server's sim never touches it and `paint()` is the only thing that does.

---

## 10. Ranking: Glicko-2 on pairwise encounters

**Assumption, stated so it can be rejected:** encounters are pairwise. Two
players meet, one wins. The world holds eight people but combat resolves
between two. If that turns out false and encounters become free-for-alls, this
section is wrong and the answer is a placement-based system — TrueSkill or
Weng-Lin — rated on finishing order. Everything below assumes pairwise.

**Use Glicko-2.** Not Elo.

Elo has one number per player and a fixed K-factor, which forces a bad trade in
a small population: a large K makes established ratings jump around after one
match, a small K means a new player needs dozens of matches to find their level.
With an early population measured in dozens, everyone is a new player and
everyone stays mis-rated.

Glicko-2 carries three numbers — rating, rating deviation, and volatility — so
uncertainty is explicit. A new player starts at 1500 with a wide RD (350) and
moves fast; an established player with a tight RD moves slowly. Rating periods
handle sparse play correctly: RD *grows* while you are away, so someone
returning after two months is re-measured instead of trusted. That is exactly
the shape of a game whose population is small and bursty.

Run rating periods **daily** at first. Glicko-2 wants 10–15 matches per period;
with a small population, waiting for a week's worth would make the ladder feel
dead. Daily is wrong by the book and right for the population — revisit when
volume supports it.

Display the ladder by **conservative rating**, `r − 2·RD`. It cannot be gamed
by playing twice and stopping, and it makes uncertainty visible: your number
goes up as you play, which is the correct incentive.

### Disconnection

The server owns the truth, which makes this tractable rather than a
he-said-she-said.

- **Under 10 s of silence:** the shard keeps simulating the player's fighter
  with a held-neutral input and holds their slot. Most disconnects are transient
  and never become a result.
- **10–45 s:** the fighter is marked absent and takes no damage. The encounter
  pauses. Reconnecting resumes it — the client re-authenticates with the same
  ticket and receives a full snapshot.
- **Past 45 s:** forfeit. The encounter closes, the present player wins, and it
  is a rated result.

An encounter that closes before a threshold of engagement — say 10 seconds of
contact — is **unrated** regardless of how it ended. That removes the incentive
to disconnect-farm a loss out of existence, without punishing someone whose
router rebooted.

**Abandonment is not a rating problem.** Track it as a separate per-account
counter with its own decay, and gate matchmaking on it — repeated abandoners
get matched with each other. Encoding it as rating loss corrupts the rating.

### Smurfing

Mostly solved by the identity model, which is the second reason to require a
handle. Playing requires an AC handle; handles are scarce, socially visible,
and not free to mint. A smurf costs a real second identity in a community where
identity is the product. That is a far stronger deterrent than any detection
heuristic.

What remains: wide RD on new accounts already makes a strong new player climb
fast, which is the intended behavior, not an exploit. Add one thing — if a new
account's first five encounters are all wins against opponents rated 300+
above it, collapse RD toward the observed level immediately rather than waiting
for the schedule. And on Xbox, the `xuid → handle` link is one-to-one, so a
console smurf costs an Xbox account too.

---

## 11. Where the sim runs

The probe already runs `oskiewar.js` headless in Node, driven at a fixed tick,
producing bit-identical results across processes. That is the interim answer
and it works today: the shard loads `oskiewar.js` into a `new Function` sandbox
with stub hosts — `wipe`, `box`, `line`, `triangle`, `write` all no-ops — and
never calls `paint()`.

That is where to start, and not where to stay. The sandbox is a 5,900-line file
carrying a renderer, an audio graph, a QR encoder, and a title screen into a
server process that needs none of it.

**The destination is an extracted `oskiewar-sim.mjs`** — one module, imported by
both the client and the shard, containing the physics, the hit resolution, the
round rules, and nothing that draws or makes sound. `oskiewar.js` keeps the render
pass, the audio, the input mapping, and the shell. The client imports the sim
and calls it; the server imports the sim and calls it. One implementation, two
callers, no drift possible by construction.

### What has to change in `oskiewar.js`

In dependency order, each independently verifiable:

1. **Seed the RNG.** `pronounceableMatchName()` (`oskiewar.js:487–498`) is the
   sim's only nondeterminism, via `seriesBallType()` hashing its output into
   ball radius and mass. Seed it from the server-assigned match ID. Measured:
   fixes 2 of 5 diverging seeds; all 5 become bit-identical.
   *Verify:* `node tmp/oskiewar-world/wire.mjs 5400` reports
   `repeatIdentical: true` on every seed without the `seeded` override.

2. **Fixed timestep.** `gameSim()` (`oskiewar.js:3388–3392`) derives dt from
   wall clock: `Math.min(0.04, Math.max(0.001, (now - lastSimAt) / 1000000))`.
   Replace with a fixed 1/60 and an accumulator that runs 0, 1, or 2 ticks per
   frame depending on elapsed real time. The client's render loop stays
   variable-rate and interpolates between ticks.
   *Verify:* the existing probe's "wall-clock dt vs fixed dt" test stops
   diverging at tick 0.

3. **Take the camera out of the sim path.** `paint()` mutates `cameraDoll`,
   measured to diverge at t=3.97 s with camera-only fields. Harmless today,
   fatal once the camera is per-viewer. Move all camera state client-side.

4. **De-index the players.** 89 occurrences of `players[0]` / `players[1]`
   across 54 lines, and 20 of `padSnapshots[0|1]` / `inputPads[0|1]`. Convert
   to iteration over a player list keyed by entity ID, with "the other player"
   becoming "the encounter opponent" — which is a real concept in the new
   design, not a workaround.
   *Verify:* `grep -c 'players\[[01]\]' xbox/live/oskiewar.js` reaches 0.

5. **Widen the wire contracts.** `oskiewar-live-manager.mjs:69` and
   `oskiewar-replays.mjs:58` both reject any state where
   `fighters.length !== 2`. The spectator relay and the replay format are
   two-player by contract, not by accident. Version them:
   `ac.oskiewar.live` v2 and `ac.oskiedemo` v2 accept 1–8 fighters, and the
   v1 validators stay for the archive.

6. **Extract the sim module.** Only after 1–5. Doing it first means doing it
   twice.

---

## 12. Risks

**The wire contracts are two-player by contract, and there are archives behind
them.** Stored replays validate as `ac.oskiedemo` v1 with exactly two fighters.
Widening carelessly either breaks the archive or silently admits malformed
states. Version both formats and keep the v1 validators running for stored
data. This is the risk most likely to cost a weekend on a Tuesday.

**Cross-engine float determinism is untested.** The probe ran one Node build on
one machine. QuickJS on console versus Node on server, across 84 transcendental
call sites, is unmeasured — no QuickJS binary was available here. The
architecture is designed not to need it: one authority, clients corrected. But
if divergence is large, the *client's own prediction* mispredicts constantly and
players feel permanent rubber-banding even on a good connection. Measure it
before shipping: run the same input stream through QuickJS and Node and diff.
It is the same probe with a different binary.

**One region is one region.** Sydney is 212 ms from NYC — 12.7 frames of RTT
before jitter — and section 5 shows prediction breaks past 6. Anyone outside
North America and Western Europe is playing a materially worse game on a
jasellite-only deployment, and no netcode fixes it. Either accept that the
early population is regional, or budget for a second box before opening it up.

Below those, in order: `@fails-components/webtransport` is duct tape by its
author's own description and `node:quic` is not in official Node builds, so the
WebTransport path is not schedulable — which is why WebSocket ships first.
Geckos.io is one release a year and pins an old `node-datachannel`, which is
why the dependency gets used directly. Daily Glicko-2 rating periods are below
the book's recommended match volume and the early ladder will be noisy. And the
`Math.random` fix must land before *anything* re-simulates, or every
reconciliation past the first round boundary corrects a real player for the
server's private coin flip.

---

## 13. Build sequence

Each step is verifiable on its own and leaves the game playable.

1. **Seed the RNG.** One call site. Verify with
   `node tmp/oskiewar-world/wire.mjs 5400` — all five seeds report
   `repeatIdentical: true` with no override.

2. **Fixed timestep in `gameSim()`.** Accumulator, 1/60 dt. Verify the existing
   test suite still passes and that the probe's wall-clock test converges.

3. **Headless shard on WebSocket, 2 players, no prediction.** Node process,
   `oskiewar.js` in the sandbox, 60 Hz loop, 2-byte input up, JSON snapshot down
   at 20 Hz, client renders the server's state with no local sim. It will feel
   bad. Verify: two browsers fight, the server decides, nothing desyncs.

4. **Client prediction and reconciliation.** Client re-simulates its own
   fighter from unacknowledged inputs. Verify by measuring correction magnitude
   against the drift table in section 5 — on LAN, corrections should be zero.

5. **Interpolate other players 100 ms in the past.** Verify: a synthetic 150 ms
   RTT with 2% loss produces no visible extrapolation artifacts.

6. **Control plane on Cloudflare.** Workers for identity and tickets, a Durable
   Object for the matchmaking queue, D1 for Glicko-2. Verify: a player gets a
   signed ticket from Cloudflare and connects to a shard that validates it and
   rejects a forged one.

7. **Binary wire format, 1,384-byte ceiling.** Verify: 8-player snapshot fits
   in one datagram with headroom, measured.

8. **WebRTC DataChannel transport** behind the interface, `node-datachannel`
   direct, signaling on Workers, TURN fallback. Verify: with 2% simulated
   packet loss, DataChannel shows no head-of-line stall where WebSocket does.

9. **De-index the players.** Verify: `grep -c 'players\[[01]\]'` reaches 0 and
   the existing test suite passes unchanged.

10. **Widen the wire contracts to v2**, 1–8 fighters, v1 validators retained.
    Verify: stored v1 replays still load; a 3-fighter v2 state validates and a
    9-fighter one is rejected.

11. **The world.** Persistent hub, login curtain over a live scene, dummy at
    center, geography as navigation, opponents that walk in. Verify: a player
    logs in, is standing in the world, walks to a region, and someone arrives.

12. **Encounter camera.** Follow camera in the hub, rect-pack on encounter
    open, release on close. Verify the encounter framing still measures at 83%
    of action-safe height.

13. **Glicko-2 live**, daily periods, conservative-rating ladder, disconnect
    policy. Verify against a replayed history of recorded results.

14. **Second region**, when there are players in it. Verify: the control plane
    assigns by measured RTT and a European player lands on the European shard.

Steps 1 and 2 are worth doing this week regardless of whether any of the rest
gets built. They cost almost nothing and they are the difference between a sim
that can be re-simulated and one that cannot.
