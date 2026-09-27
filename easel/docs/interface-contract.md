# Aesel entry points

| Command | Interface |
| --- | --- |
| `aesel` | Open or activate the native SwiftUI Mac app. |
| `a [directory] [options]` | Open the terminal interface in pro mode. |
| `ac [directory] [options]` / `easel [directory] [options]` | Open the piece-oriented terminal interface. |

The native app manages its own saved threads. Workspace and engine flags are
TUI options. The GUI launcher checks the bundle identity
`computer.aesthetic.aesel.native` and rejects the retired Electron app, including
explicit overrides. It searches `~/Applications/Aesel Native.app` and
`/Applications/Aesel Native.app`; `AESEL_DESKTOP_APP` selects another native bundle.

The native app is built from `apple/aesel`. Shared notebook assets live in
`easel/shared`; the terminal runtime remains in `easel/src`. See the
[native README](../../apple/aesel/README.md) for build and installation instructions.
