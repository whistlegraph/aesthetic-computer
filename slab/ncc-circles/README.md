# NCC Circles

A macOS Signal client for the NELA Computer Club's circles, in Swift (AppKit).
Three columns in one monospace face: the rail of circles, the open circle's
messages with its roles on top, and that circle's wiki pages. Pictures, videos,
files and link previews show on the sheet. Light and dark themes (View menu).

## Build

```
./build.sh          # build/NCC Circles.app
./build.sh --run    # build and open
```

The app needs `signal-cli` (`brew install signal-cli`).

## The two logins

1. **NCC login.** OpenID Connect against the club's identity provider
   (authorization code + PKCE, loopback redirect). The app stays shut until the
   ID token verifies. It needs a public OIDC client with the redirect
   `http://127.0.0.1:47823/callback`; put the client id in the config.
2. **Signal.** The app is its own linked device. It shows a QR code; scan it
   from Signal on the phone (Settings → Linked devices). Its Signal data stays
   in `~/Library/Application Support/NCC Circles/signal`, apart from any other
   `signal-cli` on the Mac.

## The wiki

The right column is the wiki's page tree, as its editors arranged it: the
collection that carries the open circle's name, or the whole wiki. A page opens
in place of the messages (Esc goes back), and links between wiki pages open in
the sheet. When the wiki has a page titled `STARTING PAGE` (config `wikiStart`),
it becomes the START entry at the top of the rail and the first thing shown.
The wiki needs your API key: menu → Set Wiki Key… (kept in the login Keychain).

Test aid: `NCC_SNAPSHOT=<file.png>` makes the app draw its window into that file
after ten seconds and quit, with no screen capture and no clicks.

## Config

`~/.config/ncc-circles/config.json`, written on first run. Nothing about the
club ships in this repo.

| key | meaning |
| --- | --- |
| `auth.issuer`, `auth.clientId` | the OIDC provider and this app's client |
| `auth.required` | `false` turns the NCC gate off (development) |
| `match` | name patterns that pick the club's groups from the account |
| `circles` | the groups found: title, `circle` or `commons`, role key |
| `peopleLedger` | optional `people.json`: names, colours, roles, badges |
| `assetsDir` | optional club artwork for the rail icons |
| `wikiURL` | the wiki, default `https://wiki.nela.computer` |
| `desktopBridge` | optional path to `signal.mjs`, the Signal Desktop bridge that fills a first run with history; default is the copy in the app bundle |

## Parts

- `Sources/Auth.swift` — the NCC login gate
- `Sources/SignalEngine.swift` — `signal-cli jsonRpc` child, device link
- `Sources/Store.swift` — per-circle message log (JSONL), people ledger
- `Sources/Wiki.swift` — wiki collections, pages, member colours
- `Sources/Media.swift` — thumbnails, scaling, video frames
- `Sources/UI.swift` — wordmark, gate, rail, sheet, wiki column, themes
- `Sources/main.swift` — the controller
