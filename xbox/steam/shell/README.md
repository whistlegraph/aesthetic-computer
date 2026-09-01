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

## Verified 2026-09-01

Boots to the title screen at **59.94 fps, 738 render frames against 738
simulation ticks, zero dropped**, ~2 ms render cost. Three assets 404 and all
three are expected: `auth0-spa-js.production.js` (deliberately not shipped) and
`/api/oskiewar-pops` + `/api/oskiewar-country` (already inside catches). Every
remote the page reaches for is wrapped, so the shell is offline-clean today.

## Still owed

- **`steamworks.js` is an optional dependency and is not installed**, so the
  shell runs unwired and `initSteam` logs and continues. Install it, then
  verify `electronEnableSteamOverlay()` on all three OSes — per `../STEAM.md`
  the overlay is the piece with the most platform variance under Electron, and
  macOS wants an eyeball before the build submission.
- **A trimmed page.** The shell currently loads `mac-test.html` unmodified,
  which is the live oskiewar.com page — do not edit it in place. Copy it here
  and drop: the `og:*` meta, the Auth0 block and its `log in` button (visible
  and inert in the shell today), the product-analytics fetch, the MIDI block,
  `readDummyPops`, `readLocalCountry`, `RoundRoom`, and the `saveReplay` /
  `publishLive` bodies. Pin `manualResolution = 1` to retire the FPS governor.
  The QR codes encode oskiewar.com links that are dead ends for a Steam player;
  omitting `globalThis.qrcode` collapses all four sites through their existing
  `typeof` guards.
- **Depot scripts** (`app_build.vdf`) and a Windows/Linux build pass. Steam
  Deck takes the Linux build natively.
- **The appid.** `steam_appid.txt` holds **480** (Valve's public Spacewar test
  app) so the SDK can initialize before oskiewar has one of its own.
