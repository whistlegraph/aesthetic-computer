# aesel for Mac and iPhone

`Aesel` (iOS 17+) and `AeselMac` (macOS 14+) compile the same SwiftUI sources.
`ApplePlatform.swift` adapts WebKit hosting and native image drawing to UIKit or
AppKit. Both run the bundled JavaScript session, notebook renderer, and AC piece
preview; the Mac target does not use Electron or start a Node process.
The live WebKit preview stays in the upper-right corner, including for new
pieces and thread changes, and can expand to fill the workspace.
The animated title and version share a fixed top strip with the Mac window
controls. A blank notebook starts editing on its first ruled line; the remaining
paper accepts clicks. Letter animation never changes the title's hit area.

Appearance follows the system automatically. Native controls and the notebook
share the system palette; enabled Mac buttons use a pointing hand.
The [experience model](EXPERIENCE.md) defines draft, sign-in, generation, and
publishing behavior, including the proposed guest AI allowance.
The [integration roadmap](ROADMAP.md) inventories the remaining ports and their
acceptance gates, including providers, paired hosts, media, files, and releases.
The native provider dropdown lists Aesthetic.Computer, Claude, and Codex.
Codex uses the OpenAI knot mark; provider artwork comes from the
[attributed desktop assets](../../easel/desktop/assets/provider-marks.txt). Claude and Codex use the optional local helper, installed from the repo
root with `node easel/native/install.mjs`. A connected provider can generate
without AC sign-in; AC generation and publication retain their account gates.
AC's model is Automatic. iPhone pairing to the helper is not implemented.

Upload interruptions retry twice, with a 20-second deadline per attempt, outside
notebook history. After retries, a temporary notice offers Retry; source remains
saved locally. AC inference has a 45-second connection deadline and a 60-second
idle-stream deadline, without replaying paid requests. A speaker beside the title
opens volume when the preview produces audio; mute remains reachable through silence.
The sound popover's 30–240 BPM control scales the preview's shared `clock.time()`
from the default 120 BPM without restarting the piece. Clock-driven sequences and
visuals follow together. Sync restarts the preview on the network clock's epoch grid,
so a backwards clock reset cannot strand a piece's beat counter. JavaScript
wall-clock timers and audio pitch remain unchanged.

Native CLI sessions share Electron's piece-first, responsive-layout, network-clock,
sound-design and reply instructions, plus the bundled AC guides. Each turn
includes a fresh preview image when available. `ac_frame` and `ac_preview` inspect
only the matching native thread; unavailable captures are reported explicitly.
Native observations distinguish snapshot size from drawable canvas size, but do
not yet certify the exact rendered source revision or expose the full worker
console. No preview claim should exceed that evidence.

```sh
./run.sh mac               # build and open Aesel Native.app
```

The native Mac target currently supports the shared Piece workflow: AC sign-in,
hosted inference, drafts, publishing, notebook, saved threads and braincells.
It uses the separate `computer.aesthetic.aesel.native` identity while desktop
terminal/CLI providers, local media renderers and other Electron integrations
are ported. It does not replace the installed Electron app or import its
credentials. Mac sessions live in the app's Application Support directory;
iPhone sessions retain their existing Documents location. Tokens use Keychain.

To build both targets without installing, run `./bundle-session.sh` and
`xcodegen generate`, then build schemes `AeselMac` (macOS) and `Aesel`
(`generic/platform=iOS`, with signing disabled for a compile check).

The editor focus regression and preview transitions have standalone native checks:

```sh
xcrun swiftc -parse-as-library Sources/AeselComposer.swift Tests/ComposerFocusChecks.swift -o /tmp/aesel-composer-check
/tmp/aesel-composer-check
xcrun swiftc Sources/Session.swift Tests/SessionPreviewChecks.swift -o /tmp/aesel-preview-check
/tmp/aesel-preview-check
```

Native SwiftUI chrome uses the desktop palette, typeface, mascot scene and slash
commands. A hidden WKWebView runs the same JavaScript AC agent as the desktop;
a separate WKWebView opens the signed-in user's published piece. Unpublished
edits can preview through AC's JavaScript `dropped:piece` interface. The runtime and model still need an internet connection.

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

## MCP and visual acceptance

The native app exposes a private same-user automation mailbox. The monorepo adapter and Aesthetic Eye workflow live at `slab/bin/aesel-mcp.mjs`, `slab/bin/aesel-eye.mjs` and `slab/AESEL-EYE.md`. Tests inspect and act through stable UI control IDs without activating the window. Preview URLs use the same `nogap`, `nolabel` and `autoreload` contract as Electron; the top strip shows the persisted piece revision starting at v0.

## Native beta 3 — 22 September 2026

The Blueberry-installed native app identifies `aesel-provider-readiness` in its
debug symbols; the matching source baseline is `eb8a7aa275`. Blueberry main
(`547e19b517`) predates the native title-bar work and is not the build source.
Beta 3 retains that title-bar implementation, matches version/title type sizes,
and adds AppKit cursor tracking.

Build `AeselMac` in Release with Developer ID signing. Before packaging, sign the
app explicitly with `--options runtime --timestamp --entitlements Mac.entitlements`
to omit Xcode's development `get-task-allow` entitlement. Sign the DMG with a secure
timestamp, submit it to `notarytool`, require `Accepted`, and staple and validate
the DMG before uploading. No credentials belong in the release or repository.
