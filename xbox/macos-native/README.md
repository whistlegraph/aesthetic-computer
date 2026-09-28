# oskiewar for macOS

The installed macOS game is an AppKit application with no WebView. JavaScriptCore
runs the same `xbox/live/oskiewar.js` lifecycle as Xbox; AppKit/CoreGraphics,
AVFoundation, and GameController provide native graphics, audio, and input.

```bash
npm run oskiewar:mac:build
npm run oskiewar:mac:install
```

`install` places `oskiewar.app` in `/Applications`, pins it to the Dock once,
and opens it. Re-running it safely replaces that app with the current shared
game source.

Pool marks are baked into a fixed 2048×2048 megatexture using the shared Xbox
`DecalSurface`. Stamps update dirty texture regions; the retained pool mesh
is drawn once per frame, regardless of how many marks have accumulated. Three
GPU snapshots prevent writes into textures or buffers still in use by a frame.

Regression checks:

```sh
node --test xbox/live/tests/pool-megatexture.test.mjs
clang++ -std=c++17 -O2 xbox/macos-native/PoolDecals.cpp xbox/macos-native/tests/pool-decals.cpp -o /tmp/oskiewar-pool-decals-test
/tmp/oskiewar-pool-decals-test
```

The pool park includes a half-pipe, pistol and axe pickups, eight civilians,
and breakable windows leading to an outside parking lot. Flick left-right-left
to spin. Y fires a collected pistol; B swings the axe. Jump into a window at speed
to break through. The monowheel motor is silent at rest and on dismount.

The park scene uses a cached spatial hierarchy with at most 64 quads per leaf,
frustum culling before native submission, wall/terrain visibility checks for
civilians, and projected-size character LOD with hysteresis.

```sh
node --test xbox/live/tests/pool-playground.test.mjs
OSKIEWAR_PERF=1 /Applications/oskiewar.app/Contents/MacOS/oskiewar
```

The opt-in local performance log reports FPS, frame-interval p95, and simulation
and paint time. Test after startup and include moving, wide, and marked-up views;
a steady camera alone is insufficient evidence for the 16.67 ms frame budget.
