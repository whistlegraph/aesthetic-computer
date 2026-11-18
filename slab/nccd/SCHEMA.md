# NCCd schema

The map of the app: what each part is, what it holds, and how a change moves
through it. Read this first; then the file you need. `README.md` is for a
person who builds and runs the app, this is for a person (or an agent) who
changes it.

## Work loop

```
./dev.sh            # fast loop: save a file in Sources/, the running window is the new code
./build.sh --run    # the app as it ships, this Mac's architecture
./dist.sh           # the release: universal, notarized DMG + update zip + manifest
./release.sh        # publish it; running copies update themselves within half an hour
```

`dev.sh` compiles the sources one file at a time (`swiftc -incremental`, about
two seconds after a change) into a dynamic library, and a host that never
changes (`Dev/host.swift`) loads the library again in the same process. The
development app starts no Signal engine and sends nothing: it shows the logs
and config that are already on this Mac. Look at a change with no screen and
no clicks: `NCC_SNAPSHOT=/tmp/x.png ./build/NCCd.app/Contents/MacOS/NCCd`
(every aid is in the README's table).

A new source file needs nothing: both builds take `Sources/*.swift`.

## Layout of the tree

```
Sources/     the app (one module, no top-level statements)
Launch/      main.swift: the release executable's one line, nccdRun()
Dev/         host.swift: the development host (dlopen, nccd_boot, nccd_teardown)
Resources/   Info.plist (the version lives here), AppIcon.icns
build.sh  dev.sh  dist.sh  release.sh  make-icon.swift
```

## Parts

| file | what it is | what it holds |
| --- | --- | --- |
| `App.swift` | the controller, and the entry points | all state (below); every decision about what shows |
| `UI.swift` | the views | `Theme`, `Pixel`, `Grain`, `Tab`, `RailRow`, `LogoTab`, `Unibar`, `LinkPanel`, `MentionList`, `MainView` |
| `Donut.swift` | the home screen's 3D donut | `Donut` (colours, flat mark), `CircleDiagram` (SceneKit scene, tokens, names, the message bubbles that float off them) |
| `Markdown.swift` | a wiki page as typed text | `Markdown.render` |
| `Wiki.swift` | the wiki's API (Outline) | token, collections, page tree, pages, pictures, search |
| `Login.swift` | the NCC sign-in | a web view on the wiki's `/auth/oidc`; hands back the wiki session |
| `SignalEngine.swift` | Signal | one `signal-cli jsonRpc` child; `call`, `onEnvelope`, `link` |
| `Store.swift` | what is kept | `Msg`, `CircleLog` (JSONL per circle), `People` (names, colours, roles) |
| `Config.swift` | settings and paths | `AppConf`, `Paths`, `Config.shared` |
| `Media.swift` | pictures and video frames | thumbnails, sizes |
| `Radio.swift` | KPBJ | an `AVPlayer` on the stream, what is on |
| `Updater.swift` | self-update | manifest, three checks, swap, relaunch |

## State (all of it is in `App`)

| what | where | notes |
| --- | --- | --- |
| which place is open | `onStart` (donut), `onSearch`, `selected` (a circle), `main.wikiOpen` | `currentTab` reads them; `go(_:)` is the one way to change place |
| the circles | `circles: [Circle]` | config + log + members; index is the tab's index |
| the two logins | `account` (Signal), `nccName` + `wiki.token` (NCC) | separate; neither needs the other |
| the wiki | `wikiTree`, `openDoc`, `startDoc` | `drawWiki()` draws the tab |
| messages on their way | `pending[groupId]` | shown glimmering; removed when Signal answers |
| circle windows | `satellites` | each a `MainView(solo: true)` |
| kept between launches | `UserDefaults`: `read.<groupId>`, `appearance`, `zoom`, `tabsOnTop`, `resume.*` | |

Places have an index: a circle is its index in `circles`; the donut is `-1`,
the wiki `-2`, the search `-3`, the login `-4`.

## How a change moves

- **A message arrives.** `SignalEngine.onEnvelope` → `App.handle` → `ingest`
  → `CircleLog.add` (to disk) → `refresh()`.
- **Something is picked.** A tab, a token, a line under the donut or a search
  result calls `main.onSelect(index)` → `App.go` → `refresh()`.
- **`refresh()` is the one place the sheet is decided.** It reads the state
  and tells `MainView` what to show: `show(...)` (colours, roles, composer),
  then one of `render` (a ledger), `renderBlank` (the donut has the sheet),
  `renderSearch`. Change what a place shows here; change how it looks in
  `MainView`.
- **A message is sent.** `MainView.send` → `onSend(words, mentions)` →
  `App.send`: into `pending`, to the engine, then into the log or back to the
  composer.

## The window (`MainView`)

Three planes that share the window: the tabs, the sheet, the wiki. A plane
that is not showing is hidden and steps out of the chain of constraints; it is
never given no width (its insides cannot be laid out in nothing, and AppKit
stops the app). `fold(width:)` decides which planes show; `fitRail(height:)`
and `fitTop(width:)` decide how much the tabs show. All three run when the
size or the contents change, never inside a layout pass.

Two arrangements of the tabs (View → Tabs on Top, `tabsOnTop`):

- **down the side**: `LogoTab` (the donut's tab), then a `RailRow` for each
  place; the two logins at the foot.
- **on top**: the unibar. One bar (`LogoTab` + `Unibar`) that says where you
  are; a tap drops down the list of every place (`drop`, `dropStack`, the same
  `RailRow`s). The logins are a task bar along the bottom.

The sheet's column, top to bottom: search field, radio bar, roles, donut,
link panel, ledger, status line, composer. Each is shown
or hidden for the place that is open.

## Conventions

- **One text size** (`Theme.size`). `Theme.mono(n)` gives it for any `n` from
  9 to 13; weight and colour tell a heading from its text. The two exceptions
  are named where they are (the donut's names and message bubbles).
- **Colour.** Every colour is a light/dark pair (`Theme.pair`). A place's
  colour is its tab; `Theme.wash` of it is its pane; `Theme.on` picks ink or
  white by contrast. The app's own colour is cyan (`Donut.icing`).
- **Icons.** Tabs: SF Symbols (`Theme.tabIcon`). The wordmark and the login
  badges: pixels (`Pixel.icon`, sizes that are multiples of 4).
- **Nothing about the club is in this repo.** Group ids, names and keys are in
  `~/.config/ncc-circles/config.json`, the Keychain and the logs.
- **Comments say what a thing is for**, in plain words, where it is.
- **Test without the screen.** A snapshot run is off every screen and takes no
  focus. Never send a message to test; `NCC_SNAPSHOT_PENDING` shows one on its
  way without sending it.

## To add a thing

- **A place (a tab).** Give it an index below `-4`; add it to `buildRail`,
  `go`, `currentTab` and a branch of `refresh()`; an icon in `Theme.tabIcon`.
- **Something in the sheet.** A view in `MainView`'s column and a `show…`
  method; call it from the right branches of `refresh()`.
- **A setting.** A key in `AppConf` (it is written to the config on save), or
  `UserDefaults` for a choice made in the window.
- **A release.** Raise `CFBundleShortVersionString` in `Resources/Info.plist`,
  `./dist.sh`, `./release.sh`, commit, `ac-deploy`.
