# oskiewar's Steam shell

An Electron window around the browser runtime — the same `xbox/live/oskiewar.js`
that oskiewar.com serves and the reel factory renders three times a day. The
rationale for choosing Electron over extending the native shells is in
`../STEAM.md` under *Build strategy*; the short version is that this is the one
runtime with production mileage, headless test coverage, and a content-hash
burner watching it.

```
npm start                                   # needs electron on PATH
../../../node_modules/electron/dist/Electron.app/Contents/MacOS/Electron .   # or the repo's copy
```

## How it fits together

`oskiewar.js` has no I/O of its own — no fetch, no DOM, no audio, no storage.
It is pure logic reading nineteen host functions that `mac-test.html` passes in
positionally through `new Function`. So the shell supplies a host, not a port.

- **`main.js`** registers an `app://` scheme and serves `xbox/live` off disk,
  with the same six out-of-tree assets `xbox/tools/serve-live.mjs` maps (QR
  encoder, analytics, two cursors, Comic Relief). **Keep the two maps in step.**
- **`preload.js`** reads the game source and exposes it as
  `__fightPieceSource`, the branch `mac-test.html` checks before it fetches
  `/oskiewar.js`. Taking it also skips the two-second update poller behind it,
  which is a live-reload affordance a shipped build has no use for.
- The window is pinned to 16:9 via `setAspectRatio`. The game fixes vertical to
  1080 logical units and derives width from the box aspect, so a 16:9 window
  *is* 1920×1080 to the game. There is no set-resolution host call — this is
  the whole mechanism, and it is also how the store screenshots get their
  required dimensions.
- `blur`/`focus` drive the frame driver's `setVisible`, because a backgrounded
  desktop window never fires `visibilitychange` and would otherwise free-run.

## Verified 2026-09-15

`npm run build:mac` packages a signed (Developer ID, not notarized) arm64
app that boots to the real title screen in **~0.8 s** with `[boot]` on
stdout, `steamworks.js` loaded and `SteamAPI_Init` reached (it fails only
because no Steam client is running, which is the expected answer on a dev
box). `build:linux` and `build:win` produce `dir` outputs from macOS too.
`OSKIEWAR_SHELL_SHOT=/path.png` makes a packaged build write what its window
shows after eight seconds and quit — the smoke test, with no screen recorder.

The staged page is trimmed at stage time (see `trim()` in `stage.mjs`; every
cut is an exact-match edit that throws when the live page moves): no Open
Graph meta, no Auth0 preload, the account corner hidden, the FPS governor
pinned at full resolution, `qrcode` undefined so the four QR sites collapse,
the `?midi` lane off, and **not versus-capable** — the web front door opens a
relay room and prints `fight a friend oskiewar.com/<room>`, which a Steam
player cannot use, so the Steam build keeps the local front the native shell
gets. The page's console errors and its `[boot]` line are mirrored to stdout.

Two staging lessons that cost a blank window: `account.mjs` imports
`auth0-otp.mjs` statically (unstaged, the whole module graph fails), and the
page prefers the woff2 face since v118. Both are in the maps now, which the
header comment counts as eight.

## Still owed

- **Overlay check on all three OSes** — `electronEnableSteamOverlay()` runs,
  but nobody has seen the overlay draw; macOS is historically the flaky one.
- **A Windows and Linux boot** on real machines (they are cross-built here
  and never launched).
- **A pad-only pass** boot → rematch before ticking Full Controller Support.
- **The appid.** `steam_appid.txt` holds **480** (Spacewar) until Steamworks
  assigns one; then `node ../depots/depots.mjs --appid=<n>` writes the depot
  VDFs and `../depots/upload.sh` pushes all three builds with steamcmd.
