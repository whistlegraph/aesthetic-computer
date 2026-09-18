# aesel for iPhone

Native SwiftUI chrome uses the desktop palette, typeface, mascot scene and slash
commands. A hidden WKWebView runs the same JavaScript AC agent as the desktop;
a separate WKWebView opens the signed-in user's published piece. Unpublished
edits can preview through AC's JavaScript `dropped:piece` interface. Fresh
sessions show the native canvas placeholder until a piece is available. The runtime and model still need an internet connection.

The notebook bundles the desktop's sanitized rich-reply renderer: ruled pages,
Markdown, highlighted code, math, diagrams and color swatches. Tap the piece
title to open its published URL, or the eye to hide/show the preview. The
account's daily and purchased braincells refresh after turns and on foreground;
tap the balance to retry. Desktop source-value editing, version scrubbing and
hover interactions are not yet available on iPhone.

```sh
./run.sh                  # simulator
./run.sh device           # USB-connected iPhone
```

The script copies `easel/{src,context,phone}` and the desktop notebook renderer
into the app, generates the Xcode
project and builds with two compiler jobs. It starts no local HTTP server and
shares no laptop credentials. `./bundle-session.sh` refreshes these resources
before a manual Xcode build. Keep the iPhone unlocked for installation/launch;
`DEVICE=<UDID>` selects a device. `AESEL_HOST` is an optional explicit development
host override; normal launches use the bundled custom scheme with no listener.

## AC sign-in

`/login` opens aesthetic.computer's existing login flow inside the app. A
main-frame bridge accepts tokens only from HTTPS aesthetic.computer, while the
login sheet is open, and only from that sheet's WKWebView. The access token goes
into device-only Keychain storage; draft source goes into Documents. Old
prototype tokens borrowed from the laptop are discarded on upgrade. `/logout`
clears aesel's token and web login data.

This signs into the same AC account. It does **not** share another iOS app's
cookies or Keychain. A retained web login can renew the token when `/login` is
opened again; continuous native refresh and browser SSO are future integration
work. A new native OAuth callback must be registered before adopting
ASWebAuthenticationSession. No unregistered callback is assumed here.

The account needs an AC handle to publish; missing handles produce a linkable
instruction. Generated writes retain the existing automatic publishing behavior;
`/publish` retries explicitly, and `/open` is available after publication.
Draft preview is separate from the public sharing URL.

## Release gates

The bundle identity remains `computer.aesthetic.easel`, matching macOS's existing
App Store record; the visible name is `aesel`. The icon is opaque at 1024 pixels.
An iOS platform can be added to that record; phone functionality, sign-in,
preview, model errors and background/foreground behavior need device acceptance
before uploading. App Review approval is not guaranteed by a successful build.

The JS bridge signals `ready` after module evaluation, not on `didFinish`.
Publishing strips Node's forbidden `User-Agent` header for WebKit. Bundle guide
text is injected at startup so it does not depend on custom-scheme `fetch`.

## Threads and media

The native home screen resumes local threads or starts a Piece. Each saved
thread retains its source, transcript, publication owner and AC model conversation;
account tokens are excluded. The previous single draft migrates on first launch.
Thread switching waits for an interrupted turn and any upload before changing
source. Returning home leaves the current preview alive.

Picture, Sound, Paper and Game Boy are desktop-only until their actual render
and tool backends are ported; their chooser rows do not create pretend sessions.
The editor keeps only the piece title, preview, notebook and message input visible.
The version button opens account, status, balance and thread controls. Braincell
models are automatic, including resumed threads with old manual choices. Dollar
values come from the allowance endpoint and distinguish free from purchased credit.
Account character colors come from AC's saved palette,
including the `@` character, with the same fallback palette as desktop.
