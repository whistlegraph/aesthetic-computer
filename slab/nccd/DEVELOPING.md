# Working on NCCd

For whoever changes the app. `README.md` says what the app is and how to get
it. `SCHEMA.md` is the map of the code: its parts, its state, and how a change
moves through it. This file has the rest: the release, how the app updates
itself, how to test with no screen, and the settings.

## The fast loop

```
./dev.sh            # save a file in Sources/, and the running window is the new code
./build.sh --run    # the app as it ships, for this Mac
```

`dev.sh` says at its top how it works. The development app starts no Signal
engine and sends nothing.

## Release

```
# 1. raise CFBundleShortVersionString in Resources/Info.plist
./dist.sh           # universal, hardened runtime, notarized: dist/*.dmg, *.zip, latest.json
./release.sh        # upload them (--dry-run to look first)
```

`dist.sh` needs the Developer ID identity in the keychain and the notary
credentials in the vault (`apple/app-specific-password.env`). `release.sh`
uploads to `assets.aesthetic.computer/nccd/`, the files first and the
manifest last.

## Updates

A running copy updates itself (`Sources/Updater.swift`, the MacPal pattern). It
reads `assets.aesthetic.computer/nccd/latest.json` half a minute after
launch and then every half hour, or from the menu (Check for Updates…). A newer
version must pass three checks before it replaces anything: its sha256 is the
manifest's, its code signature is whole, and it is signed by the same Developer
ID team as the running copy. The app then waits for a pause in the typing,
swaps itself, and starts again on the same circle with the draft still in the
composer. Only a copy in `/Applications` or `~/Applications` updates itself.

Test it with no upload: `NCC_UPDATE_MANIFEST=file:///…/latest.json` names
another manifest (its `url` may be a `file://` zip) and brings the first check
forward to six seconds.

## Test aids

A run with `NCC_SNAPSHOT` set is a test: no Dock icon, no focus, and its window
is far off every screen, so a check can run while someone uses the Mac. The
slab tiler tiles any window with this app's bundle id, so for a picture at a
chosen size run a copy with another id (copy the app, change
`CFBundleIdentifier`, `codesign --force --sign -`).

| variable | effect |
| --- | --- |
| `NCC_SNAPSHOT=<file.png>` | draw the window into that file and quit |
| `NCC_SNAPSHOT_AFTER=<seconds>` | when (default 10) |
| `NCC_SNAPSHOT_SIZE=<w>x<h>` | size the window first |
| `NCC_SNAPSHOT_CIRCLE=<n>` | open the nth circle first (this marks it read) |
| `NCC_SNAPSHOT_PICK=1` | pick the first circle on the donut, as a click would (runs the transition) |
| `NCC_SNAPSHOT_PENDING=<words>` | show those words as a message on its way; nothing is sent |
| `NCC_SNAPSHOT_ZOOM=<factor>` | zoom what is in the pane for the picture |
| `NCC_SNAPSHOT_TOP=1` or `0` | the unibar, or the tabs down the side, for the picture |
| `NCC_SNAPSHOT_DROP=1` | open the unibar's dropdown for the picture |
| `NCC_SNAPSHOT_SEARCH=<words>` | open the Search tab and search for them |
| `NCC_SNAPSHOT_TAB=<index>` | open that place first (-2 wiki, -3 search, -4 login, -5 code) |
| `NCC_SNAPSHOT_QR=<words>` | show a code to scan in Signal's pane (no link is started) |
| `NCC_SNAPSHOT_BACK=1` | press "← back" a few seconds after a login was started |
| `NCC_SNAPSHOT_NCC=1` | start the NCC login in its pane |
| `NCC_SNAPSHOT_WINDOW=<n>` | give the nth circle its own window, and picture that window |
| `NCC_SNAPSHOT_DOC=<title, id or slug>` | open that wiki page first; its markdown is written to `<file.png>.md` |
| `NCC_NO_SIGNAL_CLI=1` | as if signal-cli were not installed, to see the engine install |
| `NCC_LOGGED_OUT=1` | no Signal account and no wiki session, to see the logged-out screen (`signal` or `ncc` for one of them) |
| `-appearance dark` (argument) | the theme |

## Config

`~/.config/ncc-circles/config.json`, written on first run. Nothing about the
club ships in this repo.

| key | meaning |
| --- | --- |
| `match` | name patterns that pick the club's groups from the account |
| `circles` | the groups found: title, `circle` or `commons`, role key |
| `peopleLedger` | optional `people.json`: names, colours, roles, badges |
| `assetsDir` | optional club artwork for the rail icons |
| `wikiURL` | the wiki, default `https://wiki.nela.computer` |
| `desktopBridge` | optional path to `signal.mjs`, the Signal Desktop bridge that fills a first run with history; default is the copy in the app bundle |

## Parts

- `Sources/Login.swift` — the NCC sign-in: the club's login page in the wiki pane
- `Sources/SignalEngine.swift` — `signal-cli jsonRpc` child, device link
- `Sources/Store.swift` — per-circle message log (JSONL), people ledger
- `Sources/Wiki.swift` — wiki collections, pages, pictures, member colours
- `Sources/Markdown.swift` — a wiki page as typed text
- `Sources/Media.swift` — thumbnails, scaling, video frames
- `Sources/Donut.swift` — the donut: the 3D home screen
- `Sources/Updater.swift` — self-update
- the Code tab (`showCode` in `App.swift`) shows the club's forge (`forgeURL`, default `https://git.nela.computer`) in a web view that shares the NCC login's session
- `Sources/MenuBar.swift` — the wordmark in the macOS menu bar, its hover card (unread by chat, the last day by the hour), and the notifications for new club messages
- `Sources/About.swift` — the About NCCd window
- `Sources/Radio.swift` — KPBJ 95.9 FM, in the Netlabel tab
- `Sources/UI.swift` — wordmark, rail, sheet, wiki pane, link panel, themes
- `Sources/App.swift` — the controller, and the entry points
- `Launch/main.swift` — the release executable's one line
- `Dev/host.swift` — the development host, which loads the app's code as a library and loads it again
