# Tournament readiness

**Status: plan.** No tournament service or infrastructure is active.

Tournament readiness is a product contract, not a checkbox on the transport.
Menu Fighter must make the same deterministic decisions on every surface,
measure the conditions of every match, preserve enough evidence to rule on a
failure, and refuse conditions outside the tournament profile.

## The promise

Every certified client uses:

- the same 60 Hz simulation and input bit layout;
- the same immutable gameplay build, content, and rules hashes;
- the same random seed and stage/rules payload;
- the same negotiated input delay for both players;
- the same maximum prediction and rollback window;
- the same timeout, pause, disconnect, and forfeit rules;
- confirmed-frame checksums and the same replay format.

Rendering, controller APIs, authentication, invites, and packet transport may
be platform-specific. They may not change simulation behavior.

## Match classes

### Casual crossplay

Prefer a direct encrypted peer connection. Fall back to a nearby relay when ICE
cannot establish a direct route. This minimizes latency and is the default for
ordinary Mac, web, and Xbox crossplay.

### Remote tournament

Use a tournament coordinator plus an approved neutral network route. A direct
route is allowed only when the organizer's policy permits exposing peer network
addresses and both clients can provide equivalent telemetry. Otherwise select
the lowest-latency regional relay that both players pass in the preflight.

The coordinator observes signed input envelopes, sequence/ack progress,
confirmed-frame checksums, connection events, and network statistics. It does
not send authoritative world snapshots and does not sit in the simulation loop.
It can replay the confirmed input stream independently to verify the result.

### On-site tournament

Use wired Ethernet and a venue-local coordinator/relay, with internet required
only for identity or bracket integration. Finals should use organizer-certified
stations, displays, controller adapters, OS builds, and browser/runtime versions.
Local versus remains available when both players share one station.

## Tournament profile v1

The concrete values below are starting gates to validate in real playtests, not
claims that every internet path will feel good.

| Property | Requirement |
| --- | --- |
| Simulation | fixed 60 Hz |
| Render | 60 Hz minimum; never drives simulation |
| Input delay | symmetric, locked before round start |
| Maximum prediction | 8 frames |
| State checksum | every 60 confirmed frames and at round end |
| Packet policy | unordered/unreliable inputs with redundant recent window |
| Control policy | reliable handshake, ready, pause, result, rematch |
| Replay | seed + metadata + every confirmed input |
| Build parity | protocol, sim, rules, and content hashes must match |
| Clock | monotonic clock; frame-advantage time sync during neutral moments |
| Connection | preflight RTT/jitter/loss test; continuous telemetry |
| Privacy | organizer may require relay-only candidates |

The preflight should classify rather than conceal a bad route. The organizer
sets published limits for RTT, jitter, sustained loss, burst loss, and platform
latency. A client outside those limits cannot enter that match as tournament
ready, even if rollback could technically continue.

## Cross-platform certification

“Same code” is necessary but not sufficient. Each shipping surface must pass:

1. **Determinism:** a shared corpus of seeds and input streams produces the same
   checksum at every checkpoint across JavaScriptCore, Chromium/V8, and the Xbox
   runtime on both ARM64 and x64 where applicable.
2. **Input:** controller state is sampled once per simulation frame into the
   shared bitmask; reconnects, remaps, dead zones, and chords behave identically.
3. **Latency:** button-to-simulation and button-to-photon measurements stay
   inside a published certification band. Faster clients are not secretly
   handicapped; outlier configurations are excluded from tournament mode.
4. **Pacing:** no background throttling, timer drift, variable simulation step,
   or display refresh dependency. A focus/suspend event stops the match under
   the shared rules.
5. **Network:** the transport passes reordering, duplication, loss, congestion,
   ICE migration/restart, suspend/resume, and disconnect recovery tests.
6. **Evidence:** a captured match replays offline to the signed final checksum
   and result on every certified surface.

Web tournament mode therefore means a supported browser/version in fullscreen
or installed-app mode, with power-saving and background throttling ruled out.
It does not mean every browser/device combination is automatically certified.

## Fairness and security

- Use authenticated short-lived match credentials and encrypted transports.
- Never expose TURN or coordinator long-lived secrets to the client.
- Bind seat, build hashes, rules, seed, and monotonically increasing input
  sequence numbers into the match record.
- Send inputs to the opponent and referee/coordinator concurrently; the referee
  reconstructs the match instead of trusting a client's claimed world state.
- Reject impossible protocol transitions, post-deadline input rewrites, replayed
  packets, and checksum claims for unconfirmed frames.
- Record network-health evidence for lag-switch and disconnect rulings. Do not
  auto-punish ordinary jitter from a single sample.
- Treat macros, controller firmware, accessibility devices, and remapping as an
  explicit tournament rules issue rather than pretending netcode can identify
  player intent.

## Crossplay transport shape

The rollback protocol stays common while adapters vary:

```text
web -------- WebRTC DataChannel ----+
macOS ------ WebRTC DataChannel ----+--- tournament edge/coordinator
Xbox ------- native WebRTC or ------+        + verifier/replay
             secure datagram gateway
```

Direct compatible peers bypass the edge for match inputs. If the Xbox shipping
host cannot run a compatible WebRTC data channel, its native secure-datagram
adapter talks to a gateway that forwards the exact same versioned binary input
packets to web/macOS peers. That route must pass the same latency and failure
tests before certification.

Microsoft's current GDK exposes Win32-like networking choices including Winsock,
DTLS, MsQuic, and PlayFab Party, so Xbox does not require gameplay or rollback
changes. It does require an early decision about the compatible wire transport.

## Ship gates

Menu Fighter is tournament-ready only after all of these are automated:

- cross-process and cross-engine soak tests of at least 100,000 frames;
- Mac↔Mac, Mac↔web, web↔web, Xbox↔Mac, and Xbox↔web certification runs;
- deterministic replay verification by the coordinator;
- direct, TURN-relayed, tournament-relayed, and venue-LAN test matrices;
- forced browser GC, CPU contention, controller reconnect, focus loss, console
  suspend/resume, route migration, and burst-loss tests;
- visible connection-quality and route indicators before players ready up;
- organizer controls for pause, restart, disqualification, result signing, and
  evidence export;
- a written ruleset for disconnects, degraded routes, controller legality, and
  platform/version eligibility.
