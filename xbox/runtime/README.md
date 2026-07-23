# Aesthetic Computer Xbox native runtime

This directory is the portability seam for the native Xbox AC BIOS. The target
does **not** host a WebView: Direct3D renders AC drawing commands, XAudio2 plays
sound, Windows.Gaming.Input supplies input, and an interpreter compiled into
the package runs downloaded AC JavaScript pieces. The WebView dynamic shell is
an experimental comparison target, not the production architecture.

## Compatibility target

| Piece API | Xbox native shape | Notes |
|---|---|---|
| `boot(api)` / `sim(api)` / `paint(api)` / `act(api)` / `leave(api)` | `Piece` virtual methods | Same lifecycle and fixed-step intent |
| `api.screen.{width,height}` | `Api::screen` | Physical backbuffer dimensions |
| `api.clock.time()` / `seconds` | `Api::clock`, `Api::seconds` | QPC monotonic time plus midpoint-adjusted `/api/clock` Unix time and sync RTT |
| `wipe`, `box`, `line`, `write` | `Graphics` command interface | D3D renderer batches these commands |
| `triangles3d(Float32Array, count?)` | Flat 12-float triangle stream | One JS→native call and one hardware draw for up to 4,096 triangles |
| system type and Xbox button symbols | `systemWrite`, `systemGlyph` | DirectWrite with Segoe UI / Segoe MDL2 Assets |
| latest user painting | `painting` | Host downloads and decodes an allowlisted AC image |
| `sound.synth({...})` | `Sound::synth(SynthVoice)` | Submit immediately to XAudio2, outside Present |
| MIDI note input | `Windows.Devices.Midi` → XAudio2 | Auto-opens the first input, timestamps Note On with QPC, and reports event-to-submit latency |
| continuous sine oscillator | `Sound::oscillator` | One looped XAudio2 buffer, pitch and level changed in real time |
| mood / clock chat / painting metadata | `Api::ac` | Immutable host-polled snapshot; no general piece HTTP API |
| keyboard direction events | translated D-pad events | Existing `nom.mjs` names are preserved |
| Space/Enter munch | A/Menu translated events | A maps to Space, Menu maps to Enter |
| `api.dark`, handle, online state | `Api::system` | Populated by host/control session |

`numbnom` primarily needs the lifecycle, clock, four drawing primitives,
direction/munch events, synth, deterministic random numbers, and text. HD canvas,
speech, pointer tap-to-walk, and the shared JS `Synth` percussion helpers are the
next compatibility tier. Until those exist, the native port should retain its
game rules but use native text and basic oscillator cues.

## Host loop

1. Poll `Windows.Gaming.Input` as tightly as the foreground policy permits.
2. Timestamp transitions with QPC and call `act` immediately.
3. Run `sim` at a fixed 60 Hz; catch up with a bounded number of steps.
4. Build one graphics command list in `paint`, then present once.
5. Enqueue `Sound::synth` directly to a preallocated XAudio2 source voice.
6. Poll the outbound `ControlChannel` and emit timestamped telemetry.

The host reports XAudio2's `CurrentLatencyInSamples`, submit time, event-to-submit
time, and engine glitch count through `runtime()`. These are pipeline
measurements, not an acoustic round-trip measurement; measuring speaker-to-room
latency still requires a microphone or loopback interface.

## Live piece loading

`JsEngine` is the narrow adapter for an embedded interpreter. The intended
first engine is QuickJS: it is MIT licensed, small, has no required external
dependencies, and is an interpreter (so it does not require runtime code
generation/JIT privileges). Pin and vendor an audited release in the build;
never download the engine itself at runtime.

The AC endpoint returns a bounded `PieceBundle` containing `slug`, immutable
`version`, UTF-8 `source`, and SHA-256. The host downloads it with
`Windows.Web.Http.HttpClient`, requires HTTPS, verifies the digest, then asks
`PieceSupervisor` to stage it. Staging creates a fresh JS runtime with only AC
bindings, compiles and calls `boot`; activation happens at a frame boundary.
The prior context remains the last-known-good fallback until the new generation
has survived its probation window. Syntax/boot/runtime/watchdog failures emit
telemetry and roll back without restarting the binary.

The JS global surface must not expose WinRT, filesystem/process APIs, `eval` of
untrusted secondary sources, raw sockets, or a general-purpose browser API.
Heap, source size, stack, callback time, and pending-job counts are bounded.
Network access is host-mediated and restricted to declared AC endpoints.

This dynamic model must remain consistent with the product's declared purpose.
Microsoft Store policy 10.2.2 disallows using downloaded code to fundamentally
change or extend an app's described functionality. AC pieces therefore need to
be presented and certified as the title's normal content/runtime model, not as
a way to install unrelated programs. Dev Mode testing is feasible now; retail
certification needs an explicit policy review before relying on remote source.

The control channel accepts configuration, piece source, and probe identifiers.
It must never accept downloaded native code. Native BIOS changes still require
a rebuilt and signed package.

## Nom porting plan

Keep board generation and gameplay in a platform-neutral `NomGame` class. Put
rendering and sound behind `Graphics`/`Sound`, and adapt controller transitions
through `input_map.hpp`. This lets `numbnom`, `engnom`, and the other editions
share one engine just as they share `lib/nom.mjs` today.
