# Multiplayer vertical slice

**Status:** implemented locally, not deployed or tournament-certified.

The current slice is deliberately split into replaceable layers:

- `fight-manager.mjs` owns reliable identity, queues, private rooms, match
  proposals, version locks, route policy, and targeted signaling.
- `fight/lobby.mjs` is the client state machine shown by the Menu Fighter
  popover.
- `fight/rtc-core.mjs` owns an unordered, zero-retransmit WebRTC input channel.
  `rtc-main.mjs` runs it in the BIOS Window; `rtc-bridge.mjs` keeps pieces safe
  when they execute in the disk worker.
- `fight/online.mjs` feeds that transport into the same rollback session used
  by the hostile fake-wire tests.
- `fight/regions.mjs` defines logical regions and provider-neutral direct/relay
  selection. It provisions nothing.

## Current public flow

1. A handled player opens Menu Fighter. The piece sends its real AC bearer
   token over the reliable session socket.
2. The session server validates the token with Auth0 and resolves the handle
   from the validated subject. Client-supplied handles are never accepted for
   `FIND`.
3. `FIND` queues the player with the exact protocol, build, simulation, rules,
   and content manifest plus a coarse region.
4. The coordinator prefers the same region for ten seconds, adjacent regions
   until thirty seconds, then all configured regions. Compatibility and network
   ceilings never widen.
5. Both players receive a private match proposal. SDP and ICE messages are
   forwarded only to the other member of that match.
6. Once both accept and the data channel opens, each client advances its local
   rollback session with its own input. Only redundant input windows cross the
   peer channel.

Configured logical regions are `us-west`, `us-east`, `eu-west`, and
`eu-central`. Route reports carry a match nonce and must contain plausible
sample counts, RTT, jitter, and loss. The policy chooses a healthy direct path
first, otherwise the lowest-scoring common relay region. Missing or malformed
samples can never win. Relay nodes and probe endpoints remain infrastructure
work; the currently runnable path is direct WebRTC with public STUN.

## UI and telemetry

The centered popover exposes `TRAIN` and, after verified handled authentication,
`FIND`. FIND cycles through waiting, matched, connecting, playing, and error.
During play the HUD reports route type, rolling RTT, jitter, estimated packet
loss, rollback count/average depth, and stalls.

## Private and cross-piece entry points

The coordinator supports guest `fight:room:create` and `fight:room:join` calls
using an unlisted `VS XXXX-XXXX` code. Guest rooms cannot enter the public
queue. Text-entry/share UI for these calls is still pending.

In `chat` or `laer-klokken`/`laklok`, a handled user may type:

```text
fight @handle
```

The command is intercepted before it becomes a public chat message and opens
Menu Fighter with the target in its colon parameters. After the new piece has
authenticated, it sends the targeted request and keeps the match socket alive.
The recipient sees the private notice in either chat surface and can type
`fight accept` or `fight decline`. Accept moves them into Menu Fighter, answers
the persisted invitation after authentication, and creates the two private
match proposals. No invitation is broadcast.

## Production gates still open

- deploy coordinator-compatible code and add shared/Valkey state before running
  more than one session-server process;
- issue short-lived TURN credentials and stand up real relay probes/nodes;
- add queue heartbeats, reconnect grace completion, ICE restart, and rematch;
- replace JSON input envelopes with the specified binary codec;
- exchange confirmed-frame checksums, save replay inputs, and add time sync;
- measure and certify web, macOS, and Xbox runtime/controller latency;
- run two-process, two-engine, cross-region soak and fault-injection suites.

Nothing in this slice provisions, deploys, purchases, or changes infrastructure.
