# Rollback and peer-to-peer plan

## What is already proven

`lib/fight/sim.mjs` is a 60 Hz integer simulation whose complete state lives in
an `Int32Array`. `lib/fight/rollback.mjs` can rewind and replay it while checking
determinism. `lib/fight/session.mjs` predicts remote input, stores snapshots,
rolls back on a bad prediction, sends redundant recent-input windows, and stalls
beyond eight speculative frames. Specs compare both peers against an offline
ground-truth simulation under latency, jitter, and packet loss.

That is the hard center of rollback. It is not yet production netcode.

## Production gaps, in order

1. **Binary protocol and handshake.** Negotiate protocol/content/rules hashes,
   seats, seed, and delay; measure round-trip time before frame zero.
2. **Direct transport.** Add a WebRTC adapter using an unordered data channel
   with zero retransmits. Use the existing session server only for room presence
   and offer/answer/ICE signaling.
3. **NAT and tournament routes.** Configure STUN plus authenticated TURN and a
   neutral tournament route. Report whether a match is direct or relayed, but
   feed every route through the same packet interface.
4. **Time synchronization.** Track local versus remote frame advantage and
   gently adjust tick pacing. Avoid visible hard skips during active inputs.
5. **Live desync detection.** Exchange periodic checksums for confirmed frames,
   retain a bounded diagnostic trace, and abort with a useful report on mismatch.
6. **Connection state.** Startup synchronization, timeout warnings, disconnect
   grace, pause/forfeit policy, and rematch without rebuilding the whole room.
7. **Replay and spectating.** Record the seed, metadata, and confirmed input
   stream. Spectators consume delayed confirmed inputs and catch up without ever
   affecting the competitors.
8. **Adversarial testing.** Run two processes and two engines, inject reordering,
   duplication, burst loss, clock drift, suspend/resume, and cross-architecture
   Mac tests. A one-process fake wire remains the fast unit test.
9. **Neutral verification.** Mirror authenticated input envelopes and confirmed
   checksums to a coordinator that can replay and sign the result without
   becoming authoritative over moment-to-moment gameplay.

## Direct Mac-to-Mac path

Yes: two Macs can play peer to peer now without a native networking rewrite.
Each joins a short-lived room over the reliable AC session connection. The
server relays SDP offers, answers, and ICE candidates; actual match inputs then
flow directly over `RTCDataChannel`. If NAT traversal fails, ICE selects TURN.

Recommended channel configuration:

```js
peer.createDataChannel("inputs", {
  ordered: false,
  maxRetransmits: 0,
});
```

The game already repeats a recent input window in every packet, so late packets
and transport retransmission only add head-of-line delay. Signaling and match
control remain reliable; per-frame inputs do not.

For a local-only mode, Bonjour can discover another native Mac, but it should
still produce the same room/transport interface. Start with room codes because
they also work across the internet and in the web build.

Direct P2P is appropriate for low-latency competitive play. Tournament policy
may intentionally require a nearby relay to hide player network addresses and
create neutral telemetry. That adds a route hop, so relay selection and preflight
qualification are part of tournament readiness, not operational afterthoughts.

## What to borrow

- [GGPO](https://github.com/pond3r/ggpo) is the behavioral reference: save/load
  callbacks, deterministic frame advancement, prediction bounds, frame delay,
  sync testing, and frame-advantage time synchronization. Its implementation is
  C++/Windows-focused; use the design, not the platform layer.
- [GGRS](https://github.com/gschup/ggrs) is the clearest modern reference for
  P2P, SyncTest, checksum exchange, disconnect notification, input delay,
  spectator catch-up, and smooth wait recommendations. It is Rust and is useful
  as a protocol/test oracle rather than a required dependency.
- [IKEMEN GO](https://github.com/ikemen-engine/Ikemen-GO) is the relevant open
  MUGEN-compatible study. Its current engine integrates a GGPO fork with full
  game-state save/load and replay during resimulation. MUGEN itself is not the
  reusable open-source example.
- [Matchbox](https://github.com/johanhelsing/matchbox) demonstrates a clean
  WebRTC P2P split across web/wasm and native Rust. It validates the transport
  architecture even if Menu Fighter stays in JavaScript.
- [WebRTC](https://www.w3.org/TR/webrtc/) is the web transport standard; MDN's
  [`maxRetransmits`](https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannel/maxRetransmits)
  reference documents the bounded-retransmission channel option.

GGPO is MIT; GGRS and Matchbox are MIT/Apache-2.0; IKEMEN GO is MIT. Preserve
licenses if code is copied. Prefer clean-room implementation from the documented
behavior because the existing small session already matches the game's needs.

## First acceptance test

On an Apple Silicon Mac and an Intel Mac (or two Apple Silicon Macs to start):

- join by code and report direct versus relay;
- exchange 10,000 deterministic frames across two real processes;
- finish with matching confirmed-frame checksums and replay hashes;
- stay responsive under 80 ms RTT, 20 ms jitter, 5% random loss, and a short
  burst-loss profile;
- disconnect one peer and produce a bounded, comprehensible outcome;
- replay the recorded match offline to the same final checksum.
