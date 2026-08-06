# Cross-platform architecture

Menu Fighter should begin as one web-native game with thin platform hosts. The
rollback core already has the property that matters most: deterministic,
snapshot-complete state. Keep that core independent of browsers, rendering,
controllers, accounts, and network APIs.

## Layer boundaries

```text
platform host       web / WKWebView / Xbox host
        |           lifecycle, controller, audio, store services
presentation        paint, menus, animation, accessibility
        |
match coordinator   lobby, handshake, rounds, replay, disconnects, referee
        |
rollback session    input delay, prediction, rewind, checksums, time sync
        |
transport adapter   loopback / WebRTC / native datagram / replay file
        |
deterministic sim   Int32 state + input bits + seed -> next state
```

Only the bottom simulation layer decides hits, movement, timers, meter, wins,
or random outcomes. Rendering and audio consume leading-frame events and never
run during rollback replay.

## Platform plan

| Target | First shipping shape | Input | Online transport |
| --- | --- | --- | --- |
| Web | AC piece at `/menu-fighter` | Gamepad API + keyboard | WebRTC data channel |
| macOS | Swift `WKWebView` host derived from `macos/fight-runner` | GameController bridge | same in-page WebRTC adapter |
| Xbox | validate on hardware, then choose ID@Xbox-compatible native host | Windows.Gaming.Input | WebRTC if supported; native datagram adapter otherwise |

The macOS runner is the shortest route to a distributable app because it
already hosts the live piece and polls native controllers. It should gain app
identity, signing, fullscreen/window policy, deep links, and release packaging
without absorbing gameplay code.

Xbox is deliberately a gate, not an assumption. The repository contains both a
UWP WebView shell and a native DirectX/XAudio2/QuickJS BIOS. Microsoft currently
directs game distribution through ID@Xbox, while its Xbox WebView guidance is
not consistent across documentation. Prove the chosen renderer, network API,
controller latency, suspend/resume, and Store path on real hardware before the
product depends on it. The transport boundary lets Xbox use a native datagram
channel without changing rollback or the sim.

For Xbox↔web crossplay, “native datagram adapter” also implies a compatible
edge gateway unless the Xbox host embeds a WebRTC data-channel implementation.
The gateway forwards common Menu Fighter packets; it never translates gameplay
state or becomes a second simulation implementation.

## Shared contracts

The first extraction should be interfaces, not copied implementation:

```js
transport.send(Uint8Array)
transport.onPacket = (Uint8Array) => {}
transport.close()

host.controllers.sample() // stable per-frame bitmasks
host.audio.emit(eventBits) // leading frames only
host.storage.get/set()
```

Network packets should be binary and versioned. A match handshake must include
protocol version, build/content hash, simulation rules hash, fighter choices,
stage/rules choice, random seed, player seat, and requested input delay. Refuse
the match on incompatible hashes rather than allowing a late desync.

## Build and content strategy

For the first vertical slice, macOS and web load the same deployed or local AC
content. Pin competitive builds to an immutable content version once matchmaking
exists. Xbox may bundle the same JS modules if the runtime passes determinism
and performance probes; otherwise compile or port only the platform boundary,
not game rules ad hoc.

Do not move the current fight files yet. Imports and tests already anchor them,
and an early move would obscure whether failures came from product scaffolding
or gameplay changes. Extract after the direct-peer milestone, with the existing
spec suite running unchanged before and after.
