# Rollback Netcode

**Status:** phases 0 and 1 shipped — `fight` plays hotseat, the sim is proven
rollback-safe, and a real GGPO-style rollback session runs against itself over a
hostile fake wire. No actual network yet.

The product-level crossplay, lobby, tournament, and infrastructure requirements
now live in `menu-fighter/{NETCODE,LOBBY,TOURNAMENT,INFRASTRUCTURE}.md`. This file
remains the engineering history and determinism rationale for the underlying
`fight` implementation.

The goal is a versus fighting game on AC with GGPO-style rollback: both peers
run the same deterministic simulation, send only inputs, predict what they
haven't received, and silently rewind-and-resimulate when a prediction was
wrong.

The thing worth internalizing up front: **this is not a networking project, it
is a determinism project.** The transport is the easy part. Making two browsers
on two machines produce bit-identical state from identical inputs, for
thousands of frames, is the hard part, and everything below is organized around
it.

## What AC already gives us

`lib/loop.mjs:104` drains a classic fixed-timestep accumulator, calling a
piece's `sim()` at a constant **120Hz** (`updateFps`, `lib/loop.mjs:7`). Render
is gated separately. Most engines make you build this; AC hands it over. A
fighting game ticks its own logic every *other* `sim()` call, which yields a
steady 60Hz game clock on both 60Hz and 120Hz displays.

Rollback needs no change to `loop.mjs` or `disk.mjs`. A piece owns its own
state, so AC's `sim()` call is just a clock tick: when a late input arrives, the
piece restores a snapshot and resimulates N ticks inside that single call.

### Two things AC costs us

**Input is one frame stale.** The frame handler runs the sim loop
(`lib/disk.mjs:13067`) *before* dispatching input to `act()`
(`lib/disk.mjs:13088`). A key pressed during frame N is not visible to `sim()`
until frame N+1. Rollback fighters budget 3–4 frames of input delay total
(Killer Instinct uses 3), so spending one on dispatch order is expensive.
Swapping that order is a real candidate change.

**Keyboard has no pollable held-state.** Only `keyboard:down:*` /
`keyboard:up:*` events. A rollback sim must *sample* a held-button bitmask once
per tick, so the piece maintains its own held set. Pen state (`$api.pen`) is
pollable; keyboard is not.

## What AC does not give us: peers

`session-server` uses [geckos.io](https://github.com/geckosio/geckos.io), which
reads peer-to-peer because it is WebRTC underneath — but **the server is the
only peer**. Every "peer" message is relayed: `channel.broadcast.emit(...)` in
`session-server/session.mjs:3777`. Production is a single DigitalOcean droplet
in NYC, so two players in Berlin currently round-trip through New York, twice.

Production also has **STUN only, no TURN** (`session.mjs:1101-1104`, with a literal
`// TODO: Add production TURN server`).

Direct browser↔browser data channels are net-new work, but small:

- `RTCPeerConnection` is main-thread-only, so it lives in `bios.mjs` — exactly
  where `lib/udp.mjs` already sits.
- Configure the channel `{ ordered: false, maxRetransmits: 0 }` for true
  fire-and-forget. That is a legitimate UDP substitute: measured at ~1–3ms over
  raw UDP ([arXiv 2112.02163](https://arxiv.org/pdf/2112.02163)), and with
  sub-MTU packets (an input is ~2 bytes) there is no head-of-line blocking.
- Signaling is ~30 lines of offer/answer/ICE relay in `session.mjs`, which
  already has the room and broadcast plumbing.
- Budget **~15–25% of sessions needing TURN relay** (symmetric NAT, cellular
  CGNAT). The often-repeated "8% relay" figure has no traceable source; the best
  real measurement is [callstats.io's 22%](https://webrtchacks.com/usage-stats/).
  Cloudflare Realtime TURN is $0.05/GB with the first 1TB/month free — free at
  input-packet volumes.

Do **not** extend `session-server/duel-manager.mjs`. It is a server-authoritative
Quake-3 model (60Hz server sim, 20Hz snapshots). That is a genuinely different
architecture, not a stepping stone.

## Determinism rules

JavaScript is friendlier here than C++: the spec pins every intermediate to
double precision, so `+ - * /` and `Math.sqrt` cannot leak 80-bit x87
intermediates or contract into an FMA. They are bit-identical across V8,
JavaScriptCore, and SpiderMonkey.

What is poison is the **implementation-approximated** set — `sin, cos, tan, pow,
atan2, hypot, exp, log` and the `**` operator. The spec explicitly allows "some
latitude in the choice of approximation algorithms" and only *recommends*
fdlibm. Chrome uses fdlibm; Firefox historically used the platform libm, and
measured results differ in the last bit between x86-64 and ARM. One `Math.sin`
in the sim desyncs Chrome against Safari.

So `lib/fight/sim.mjs` obeys:

1. **Integers only.** Positions in subpixels (1/256px). A 2D fighter is frame
   data and hitbox rectangles — it needs no trig and no `sqrt`. This dissolves
   the problem rather than managing it, and avoids fixed-point Q16.16 entirely.
   With four keys there is no jump, so there is no gravity and no vertical axis
   at all: a hit is an overlap on one dimension.
2. **All state in one `Int32Array`.** A snapshot is `.slice()`, a checksum is a
   walk. `Int32Array` also *forces* int32 on write, so state can never silently
   become a float.
3. **Seeded PRNG in the state array.** mulberry32, integer-only (`Math.imul`,
   `>>> 0`). Its cursor rolls back with everything else.
4. **Fixed iteration order.** Both fighters are probed before either hit is
   applied, so a trade never depends on index order.
5. **No wall clock.** No `Date.now`, no `performance.now`, no `Math.random`.

A spec greps the sim source and fails the build if any banned identifier appears.

### Sound, and why it lives in state

The classic rollback bug (Street Fighter x Tekken) is audio replaying on every
resimulated frame — sounds popping and cutting out. The fix is that **the sim
never plays a sound.** It records intent into `G.SFX`, a bitmask cleared at the
top of every `step()`, and only the *leading* frame reads it:

```js
function sim({ sound }) {
  if ((half ^= 1)) return;      // ac ticks at 120hz; the game at 60
  game.step(s, held[0], held[1]);
  hear(sound);                  // never called during a rollback resim
}
```

Because the flag is derived state, a rewound frame that no longer lands a hit
un-schedules its own sound. There is a spec for exactly the GGPO case: predict
the defender standing there and the frame sounds like a kill; the real input
arrives showing they had tapped block, and after the rollback that same frame
sounds like a parry.

The same rule governs particles and screen shake. Anything gameplay-relevant
(hitboxes, animation frames that gate hurtboxes) must be *in* the snapshot.
Anything cosmetic must be derivable from it, never accumulated outside it.

### The stage never learns the screen size

`STAGE_W` is a fixed 256 in the sim. If it tracked `screen.width`, two players
on differently sized windows would simulate different fights. `paint()` scales;
the sim does not. This is easy to get wrong — it was a real bug caught by
running the piece, since AC handed it a 174×128 canvas.

## The safety net: SyncTest

`lib/fight/rollback.mjs` implements GGPO's `ggpo_start_synctest`: every frame,
rewind `distance` frames, replay them from the snapshot with the recorded
inputs, and compare checksums. Identical inputs must land on an identical
checksum. If they don't, some state lives outside the snapshot — a module-level
variable, an unrestored RNG cursor, a cached derived value.

This is the same idea as Factorio's ["heavy mode"](https://wiki.factorio.com/Desynchronization),
and it catches on one machine, in one process, what would otherwise surface as
a miserable 60Hz desync across two continents.

```bash
npx jasmine --config=spec/support/jasmine.json --filter=fight
```

Green at distances 1, 2, 4 and 8 (GGPO's `MAX_PREDICTION_FRAMES` is 8), across
random play and each of the three drills.

**A SyncTest is only worth what its inputs touch, and this has bitten three
times.** The first script tapped a button nearly every frame, so both fighters
sat in attack recovery for all 900 frames: they never walked, the gap stayed at
exactly 96px, no hit landed, and the RNG never advanced — the harness faithfully
verified that *nothing happening* rolls back correctly. The second held block
for a single frame, so it always released before a fist arrived and never once
entered the block or parry branch. The third tried to drill every mechanic in
one script, and a round ending mid-script shifted every later phase out of
alignment, silently losing the clash and the guard break.

So each mechanic now gets its own drill in `rollback.mjs` — `drills(sim)`
returns `parry`, `block`, and `clash` — and the specs assert that each drill
*reaches* its mechanic (the parry drill parries and never kills; the block drill
breaks guard) before rolling each one back. Random play essentially never
parries, because the block press must land inside a five-frame window, and never
clashes, because that needs two fists live on the same tick.

If you write a new input script, count the events it produces before you trust a
green run.

## The rollback session

`session.mjs` is GGPO's shape in about 200 lines, and it knows nothing about
transport: `send` hands out a packet, `receive` takes one back.

Each peer simulates every frame for *both* players. It only ever has its own
input on time, so it predicts the opponent's by **repeating their last known
input** — deliberately dumb, and right most of the time, because hands hold
buttons. When the real input arrives and disagrees, the session rewinds to that
frame, replays everything since with the correction, and lands back on the
present. Those in-between frames are never drawn and never sounded.

A packet carries a **window** of recent inputs, not one frame, so a dropped
packet is covered by the next one. Never build retransmission on top of an
unreliable channel — widen the window instead.

### The identity, measured

Input delay does *not* reduce how often you mispredict: repeat-last-input is
wrong exactly when the opponent's input changes, however far ahead you are. What
it buys is **shallower rewinds**. The spec asserts this exactly, and it holds
frame for frame:

```
latency 6 frames, varying input delay
  delay 0: rollbacks 24   resim 144   avg depth 6.0
  delay 2: rollbacks 24   resim  96   avg depth 4.0
  delay 5: rollbacks 24   resim  24   avg depth 1.0
  delay 6: rollbacks  0   resim   0   —
```

That is `rollback depth = one-way latency − input delay`, and it goes to zero
the moment the delay covers the latency. Past `maxPrediction` (8, as in GGPO)
the session **stalls** rather than guessing further — a dropped frame beats a
prediction that stale.

### How we know it works

Both peers are compared against a **ground-truth offline sim** fed the same
inputs, not merely against each other. Under a wire dropping 487 of 1400 packets
with jitter, both peers still land bit-exact on ground truth: 24 rollbacks, 135
resimulated frames, 44 stalls.

That assertion has teeth. Delete the `resync()` call from `advance()` and both
the ground-truth match *and* peer agreement fail immediately — verified by
mutation, because a test that passes against a broken implementation is worth
nothing.

## Files

| path | role |
| --- | --- |
| `system/public/aesthetic.computer/lib/fight/sim.mjs` | the deterministic integer simulation |
| `system/public/aesthetic.computer/lib/fight/rollback.mjs` | SyncTest, per-mechanic drills, snapshot ring, desync diff |
| `system/public/aesthetic.computer/lib/fight/session.mjs` | the rollback session: prediction, rewind, stall — plus a hostile fake link |
| `system/public/aesthetic.computer/disks/fight.mjs` | the piece: input, render, audio |
| `spec/fight-sim-spec.mjs` | determinism, sound, and rules specs |
| `spec/fight-session-spec.mjs` | rollback session vs. an offline ground-truth sim |

Run it: `fight` for a match, `fight:boxes` for the hit/hurt overlay plus a live
tick and checksum readout, `fight:synctest` to run the harness in the browser,
and `fight:lag` (or `fight:lag:10:25` — ten frames of latency, 25% loss) to
play across the real rollback session and watch the opponent snap.

### The game

One punch kills. That leaves a triangle:

- a **punch** beats anyone standing,
- a **block** survives it but spends one of three guard pips — out of guard and
  the next punch goes straight through,
- a **parry** beats the punch outright and leaves the attacker stunned long
  enough that the kill is free.

There is no parry button. Tapping block arms a five-frame parry window; holding
it is only a block. Arming a parry starts a 45-frame cooldown, so mashing block
gets you blocks, never parries. Two fists live on the same tick **clash** and
bounce both fighters apart — nobody dies. First to three rounds.

Four keys each, one hand each, so nothing overlaps:

|  | move | block / parry | punch |
| --- | --- | --- | --- |
| P1 | `A` `D` | `W` | `S` |
| P2 | `←` `→` | `↑` | `↓` |

The keys are drawn on screen as a d-pad that lights while held, and the block
key takes a gold rim when a parry is armed — the cooldown is the entire reason
mashing doesn't work, so it should be visible.

Most keyboards ghost past ~6 simultaneous keys. Four keys per player, on
opposite sides of the board, keeps a two-player scramble inside that budget.

## Roadmap

- **Phase 0 — determinism.** *Done.* Integer sim, SyncTest, hotseat play.
- **Phase 1 — the rollback session.** *Done.* `session.mjs`: input delay,
  repeat-last-input prediction, the rewind loop, the prediction-window stall,
  and a redundant input window per packet. Driven by two local sessions over a
  fake link with latency, jitter and loss. See below.
- **Phase 2 — rollback over the existing relay.** geckos.io works today and
  needs zero infrastructure. Rollback over a relay is still rollback; it just
  has worse RTT. This validates the machinery against real jitter and loss.
- **Phase 3 — P2P.** `RTCPeerConnection` in `bios.mjs`, signaling over the
  existing WebSocket, Cloudflare TURN as fallback. A latency optimization
  layered on once the hard part is proven.

### Numbers to design against

| parameter | value |
| --- | --- |
| GGPO max prediction window | 8 frames |
| input delay used by real fighters | 3 frames (Killer Instinct); 3–4 acceptable |
| prediction usability ceiling | ~100–150ms before it feels bad |
| per-frame resim budget (8-frame window) | ~1.5–1.8ms |
| input encoding | one nibble (left/right/block/punch), XOR-diffed against the previous frame |
| latency identity | total ≈ input_delay + avg rollback frames |

Inputs are re-sent redundantly: GGPO packs every un-ACKed frame into each
packet, so a dropped packet is covered by the next one. Do not build
retransmission on top of the data channel — resend the input window instead.

### Known hazards

- **A backgrounded tab stops receiving `requestAnimationFrame` entirely**,
  which stalls the peer and hangs the match. Handle hidden-tab as an explicit
  pause-or-forfeit state rather than discovering it in playtesting.
- **Do not enable cross-origin isolation** (COOP/COEP) chasing 5µs timers. It
  would break AC's cross-origin CDN assets and embeds, and rollback counts ticks
  rather than measuring time, so `performance.now()` clamping is irrelevant.
- Hitstop must be part of deterministic state, not a render pause, or the two
  peers disagree about frame numbering.
