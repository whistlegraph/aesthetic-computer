# AC Fight Runner for macOS

A deliberately small native macOS host for running the live `fight.mjs` piece
through `WKWebView`. It is a comparison BIOS for the Xbox dynamic shell: both
load the deployed web runtime, while their native controller and WebView paths
remain independently measurable.

## Run

```sh
cd macos/fight-runner
swift run ac-fight-runner
```

Use a local AC server or another endpoint without rebuilding:

```sh
swift run ac-fight-runner --url http://localhost:8888/fight
AC_FIGHT_URL=https://staging.example/fight swift run ac-fight-runner
```

`Cmd-R` reloads from origin. For automatic reload, point the runner at a tiny
endpoint whose response body or `ETag` changes when a deployment is ready:

```sh
swift run ac-fight-runner \
  --url http://localhost:8888/fight \
  --reload-endpoint http://localhost:8888/api/build-id
```

The host polls native `GameController` state at 60 Hz, logs controller identity
and connection changes, displays identities at bottom-right, and publishes
snapshots to the piece as both `window.acNativeGamepads` and the
`ac-native-gamepads` DOM event. This is additive: WebKit's standard Gamepad API
continues to work when available.

## Where this fits

- `ac-electron/` is the mature macOS/Windows desktop BIOS. Its flip view already
  bridges Chromium host gamepads into a guest webview and has CDP automation.
- `slab/menuband/Sources/MenuBand/AestheticWebWindow.swift` is the closest
  existing native macOS BIOS: it already embeds AC in WKWebView and injects a
  native bridge. Its `GamepadManager.swift` establishes GameController identity,
  background monitoring, main-queue handlers, and positional face-button
  semantics. This runner follows those contracts without importing the whole
  piano/menubar executable.
- `slab/menubar-swift/` supplies the broader native AppKit host conventions, but
  is host tooling rather than an AC piece container.
- `macos/fight-runner/` is intentionally focused: one native window, one live
  piece, native controller telemetry, and no packaging or account machinery.
- `xbox/` is the distribution target. Compare controller-to-piece timestamps and
  audio/visual response between its WebView2 shell and this WKWebView shell.

Executable updates still require rebuilding this host. Piece/runtime updates do
not: change the URL content or reload endpoint and the same binary loads them.
