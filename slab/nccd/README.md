# NCCd

A macOS Signal client for the NELA Computer Club's circles, in Swift (AppKit).
The d is for donut (always written small). The club's circles are peers on one
ring, so the home screen is a donut: a turning 3D one (SceneKit) with a flat
token for each circle, its name on it. A token's size is the circle's members,
and its colour and beat are how recently the circle spoke. The commons stand
beside it.

The window is flat planes of colour in one monospace face, with a fine grain.
The tabs are folder tabs that run flush to the pane, down the left side or
(View → Tabs on Top) in a row along the top like a browser's. The first is the
wordmark with a small house: it is the donut's tab. Then the commons, each
circle, and the wiki, with no headings. The open tab is the colour of the pane
it opens; a tab that is not open is faded and shaded like a button; a circle
with a window of its own shows a window mark. Along the top the bar holds the
tabs alone, and the radio and the logins are a task bar along the bottom; as
the window narrows the wordmark's tab becomes its house alone, then only the
open tab keeps its name, then none does. Under the donut, the
newest message of every chat floats, a line each: a click goes into that
conversation. Middle, the open circle's messages with its roles on
top as colour-coded blocks (the sheet is a wash of the circle's colour);
pictures, videos, files and link previews show there. The wiki is a tab too: when it is
open it has the whole sheet. Picking a
circle on the donut zooms in on its token: the messages are already inside it,
very small, and grow until they fill the sheet. A
circle opens in a window of its own from the right-click menu on its tab, or
with ⌘N. The window's title says where you are ("NCCd: donut", "NCCd: Infra").
The tabs' icons are smooth vector drawings (SF Symbols) in one tone; the
wordmark and the login badges are pixels. The rail fits any height: it drops
its headings, then its wordmark, before anything can overlap. One text size
across every view; ⌘+ and ⌘− zoom what is in the pane (the messages and the wiki, as a
browser zooms a page, not its tabs) and ⌘0 sets it back.
The tab
shortcuts are a browser's: ⌃Tab and ⌃⇧Tab (or ⌘⇧] and ⌘⇧[) for the next and
the previous tab, ⌘1 to ⌘8 for a tab by its place, ⌘9 for the last.
KPBJ 95.9 FM plays from the Netlabel tab. The Search tab is one search over
the messages of every circle and chat and the pages of the wiki (whichever of
the two logins there is). Light and dark themes (View menu).

The download page and the member's guide are `system/public/nccd/`
(aesthetic.computer/nccd).

The app was first "NCC Circles". Its bundle id (`computer.aesthetic.ncc-circles`),
its data folder (`~/Library/Application Support/NCC Circles`) and its config path
keep that name, so a Mac that ran the old build carries on with its Signal link,
its logs and its keys.

## Build

```
./build.sh          # build/NCCd.app, this Mac's architecture
./build.sh --run    # build and open
```

The app needs `signal-cli` (`brew install signal-cli`).

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

## Writing

The composer is a bar under the ledger with a round send button in the
circle's colour. "@" opens a list of the circle's members (arrows, Return or
Tab, Esc); a name taken from it goes as a real Signal mention (`mention`,
`start:length:member` in UTF-16 units). A message goes into the ledger at once
and glimmers, a character at a time, until Signal has it. If Signal refuses
it, or says nothing for 40 seconds, it leaves the ledger and goes back into
the composer as a draft. In the ledger a link is underlined on every line it
wraps to, in a colour that is not the text's.

## The two logins

They are separate, and neither waits for the other. With neither, the app
still opens: the donut, a button to link Signal, and the club's login page in
the wiki pane. The foot of the rail shows both, each with a tick when it is done.

1. **Signal opens the circles.** The app is its own linked device. Link Signal
   shows a QR code; scan it from Signal on the phone (Settings → Linked
   devices). Its Signal data stays in
   `~/Library/Application Support/NCC Circles/signal`, apart from any other
   `signal-cli` on the Mac.
2. **NCC opens the wiki.** The club's login page (Authentik) shows in the wiki
   pane, in a web view (`Sources/Login.swift`). The app starts the wiki's own
   sign-in (`/auth/oidc`); when the club has confirmed the member, the wiki
   sets its session token, and the app asks the wiki's API with that token
   (kept in the login Keychain). The app needs no client of its own at the
   identity provider. Menu → Sign Out of NCC forgets the session. An API key
   set by hand (menu → Set Wiki Key…) works in place of the login.

## The wiki

The right pane is the wiki: the page tree as its editors arranged it (the
collection that carries the open circle's name, or the whole wiki), or a page.
A page opens in the pane (Esc or "← pages" goes back to the tree, which marks
the page last read), and links between wiki pages open there too. A page is
drawn from its markdown (`Sources/Markdown.swift`): headings, lists and
checkboxes, quotes and notices, code, tables as aligned columns, pictures, and
the inline marks. The link at the top of a page opens it in the member's
default browser, and wears that browser's icon. When the wiki has a page titled
`STARTING PAGE` (config `wikiStart`), it is the first page in the pane.

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
| `NCC_SNAPSHOT_TOP=1` or `0` | tabs along the top, or down the side, for the picture |
| `NCC_SNAPSHOT_SEARCH=<words>` | open the Search tab and search for them |
| `NCC_SNAPSHOT_WINDOW=<n>` | give the nth circle its own window, and picture that window |
| `NCC_SNAPSHOT_DOC=<title, id or slug>` | open that wiki page first; its markdown is written to `<file.png>.md` |
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
- `Sources/Radio.swift` — KPBJ 95.9 FM, in the Netlabel tab
- `Sources/UI.swift` — wordmark, rail, sheet, wiki pane, link panel, themes
- `Sources/main.swift` — the controller
