# Aesthetic Computer Swift native runtime

This package is the macOS analogue of `xbox/runtime`: an AC BIOS contract for a
native host with an embedded JavaScript interpreter. It does not require a
`WKWebView`, DOM, Safari, or Web Audio. A production host binds these protocols
to Metal, Core Audio (or a carefully preallocated `AVAudioEngine` graph),
GameController/AppKit, URLSession, and sandboxed storage.

| AC piece surface | Swift | Xbox/C++ |
|---|---|---|
| `boot/sim/paint/act/leave` | `ACPiece` | `ac::xbox::Piece` |
| runtime context | `ACApi` | `ac::xbox::Api` |
| 2D draw commands | `ACGraphics` | `Graphics` |
| models/textures/shaders | `ACRenderer` (Metal handles) | future D3D renderer |
| immediate synth/sample | `ACSound` (Core Audio) | `Sound` (XAudio2) |
| normalized input | `ACInputMap` | `input_map.hpp` |
| downloaded source metadata | `ACPieceBundle` | `PieceBundle` |
| embedded JS seam | `ACJSEngine` | `JsEngine` |
| atomic live reload | `ACPieceSupervisor` actor | `PieceSupervisor` |

The remote endpoint supplies UTF-8 source with an immutable version and SHA-256.
The supervisor enforces a source bound, verifies the digest, compiles and boots
in a fresh interpreter, then activates only at the host's frame boundary. The
old interpreter is retained during a bounded callback probation window. Throws
or watchdog overruns trigger rollback without restarting the native process.

The JavaScript adapter should expose only AC bindings. Do not expose `Process`,
the filesystem, Objective-C reflection, raw sockets, browser globals, or dynamic
native loading. Remote content is piece data; native BIOS changes remain signed
application updates.

Run the contract tests with:

```sh
cd macos/runtime
swift test
```

`macos/fight-runner` remains the WebKit comparison harness. This package is the
foundation for the direct native runner: add concrete Metal/Core Audio adapters
and a pinned embedded interpreter, then run the same `fight` bundle used by the
Xbox BIOS.
