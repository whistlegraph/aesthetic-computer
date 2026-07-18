# Aesthetic Computer Xbox native runtime

This directory is the portability seam between AC game logic and the Xbox UWP
host. It does not replace the latency probe or the WebView app. It defines the
small native API that future C++ ports—starting with the `nom` family—can share.

## Compatibility target

| Piece API | Xbox native shape | Notes |
|---|---|---|
| `boot(api)` / `sim(api)` / `paint(api)` / `act(api)` / `leave(api)` | `Piece` virtual methods | Same lifecycle and fixed-step intent |
| `api.screen.{width,height}` | `Api::screen` | Physical backbuffer dimensions |
| `api.clock.time()` / `seconds` | `Api::clock`, `Api::seconds` | QPC monotonic time plus network-adjustable Unix time |
| `wipe`, `box`, `line`, `write` | `Graphics` command interface | D3D renderer batches these commands |
| `sound.synth({...})` | `Sound::synth(SynthVoice)` | Submit immediately to XAudio2, outside Present |
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

The control channel accepts only precompiled piece/probe identifiers and JSON
configuration. It must not evaluate downloaded native code. A hosted WebView can
remain available when arbitrary JS iteration is useful.

## Nom porting plan

Keep board generation and gameplay in a platform-neutral `NomGame` class. Put
rendering and sound behind `Graphics`/`Sound`, and adapt controller transitions
through `input_map.hpp`. This lets `numbnom`, `engnom`, and the other editions
share one engine just as they share `lib/nom.mjs` today.
