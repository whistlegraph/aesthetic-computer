# A native Aesel

Written 2026-09-13. Nothing here is implemented. Line counts, flags and file
paths were read out of the tree that morning; anything I could not run is
labelled as such, and there is a list of those at the end.

The question is whether Aesel should become an application — a Mac app, a
Windows app, possibly an iOS app — instead of a program you run inside somebody
else's terminal. The prompt for this memo framed it as packaging the TUI
together with the Slab overlays that today only work beside it. That framing is
right, and the answer turns out to hinge on one line in `backends.mjs` and one
rule in Apple's sandbox.

@jeffrey's constraint holds throughout: the runtime stays a webview on every
platform. Nothing below proposes rendering an Aesthetic Computer piece natively.
The native part is the shell — a terminal, the overlays, and the window they
sit in.

## What is actually here, measured

Worth getting the numbers right before arguing from them, because the brief for
this memo overstated one of them and the correction changes the argument.

`easel/src` is **5,274 lines** of Node ESM with zero npm dependencies, and
`test/` holds **89 test cases** across seventeen files. But the *interface* —
the thing that would have to be replaced if we dropped ANSI — is not 4,400
lines. It is `tui.mjs` at 1,318 and `render.mjs` at 505: **1,823 lines.** The
other 3,451 are session logic that has nothing to do with terminals: the three
bridges, the live channel, publishing, sign-in, the Slab marker, the mascot, the
updater.

That split is not accidental and it is the most useful fact in this file.
`render.mjs` exports

```js
renderFrame(state, columns, rows, useColor)
```

and it is pure. `state` is a plain object of about twenty fields declared in one
place at `tui.mjs:55` — `input`, `cursor`, `entries`, `approval`, `piece`,
`model`, `audience`, `qr`, `busy`. Nothing in it knows what a terminal is. A
model/view seam already exists, and the view side is five hundred lines.

The product ships as an 85 KB tarball behind `curl -fsSL
https://prompt.ac/easel.sh | sh`, which checks for Node 18 and unpacks to
`~/.local/share/easel`. `bin/easel` is bash, resolves its own symlink chain, and
refuses to start unless `[[ -t 0 && -t 1 ]]` — an interactive terminal is a hard
precondition, not a preference.

## The subprocess line

Two of the three bridges spawn a vendor CLI. The third does not. This is the
constraint everything else follows from, so here is what is actually in the
files rather than a summary of it.

`claude-server.mjs:268` spawns `claude` with `--print --input-format
stream-json --output-format stream-json`, `stdio: ["pipe","pipe","pipe"]`, and
reads newline-delimited JSON off stdout with `readline`. `app-server.mjs:37`
does the same shape with `codex app-server --stdio`. Both close with
`child.kill("SIGTERM")`.

`ac-server.mjs` spawns nothing. Its whole import list is:

```js
import { EventEmitter } from "node:events";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { randomUUID } from "node:crypto";
```

No `child_process`, no `http`, no `net`. It POSTs to
`/api/easel-inference`, parses the `data:` stream by hand, and runs its own
agent loop — bounded at twelve rounds, because a looping model spends someone's
daily budget on a loop. It touches the filesystem exactly twice: it reads four
bundled guides out of `easel/context/`, and it writes the piece. The tool set is
one tool, `write_piece`, which takes the entire file and replaces it.

So the claim holds, and more strongly than I expected. **The `ac` bridge needs
no subprocess and no filesystem beyond the piece file and four read-only
documents that would ship inside any app bundle.** There is no diffing, no
multi-file workspace, no `Edit` tool with line offsets. One file, whole, every
time.

What it does need is the rest of the session, and that is where the platform
assumptions live rather than in the bridge.

## Every POSIX assumption, listed

I went looking for these rather than assuming them. The result is less alarming
than the `/bin/ps` call suggests.

**The stdio protocol assumes nothing POSIX.** Pipes and newline-delimited JSON.
No pty, no ioctl, no signal semantics beyond kill-on-close. This is genuinely
portable.

The assumptions are all in the shell around it:

1. `slab-session.mjs:21` — `execFileSync("/bin/ps", ["-o","tty=","-p",pid])`, an
   absolute path with no Windows equivalent. It is already wrapped to return `""`
   on failure, so it degrades rather than crashes. The whole Slab marker is
   macOS-shaped anyway.
2. `slab-session.mjs:41` and `render.mjs:59` default to
   `~/.local/share/slab`. On Windows the directory simply never exists and the
   marker disables itself. Harmless.
3. `ac-session.mjs:38` picks `"start"` as the Windows browser opener and hands it
   to `spawn`. `start` is a `cmd.exe` builtin, not an executable, so `/login`'s
   browser hand-off throws `ENOENT` on Windows. `tui.mjs:994` gets this right with
   `"explorer"`. One of the two is a real bug, and it is three characters.
4. `updates.mjs:154` — `execFile("tar", ["-xzf", …])`. Windows 10 and later ship
   bsdtar as `tar.exe`, so this probably works. Unverified.
5. `bin/easel`, `install.sh` and `system/public/easel.sh` are shell scripts, and
   `install.sh` uses BSD `/usr/bin/sed -i ''`, which is macOS-only even among
   POSIX systems. An app bundle makes these moot for the GUI; they still have no
   Windows story for the `ac` command line.
6. `child.kill("SIGTERM")` on Windows terminates the process without giving it a
   signal to flush on. The vendor CLI dies rather than closing. Minor.

None of that is structural. The deepest assumption fails soft and the rest are
edits, not ports.

The two things I could not check are the ones that matter most for Windows, and
I want to be plain about them. First, whether `spawn("claude", …)` works there
at all: on this machine both binaries are standalone Mach-O executables
(`~/.local/share/claude/versions/2.1.270`, `~/.codex/packages/standalone/…`),
so the npm-shim problem does not arise. If a Windows user has a `.cmd` shim
install instead, Node has refused to spawn `.cmd` without `shell: true` since
18.20.2. Second, and worse: `claude`'s own Bash tool needs a shell, and on
Windows that has historically meant Git Bash. A Windows Aesel could plausibly
start the bridge and then watch every command the model runs fail. I have no
Windows machine and there is no Windows evidence anywhere in this repo.

## 1. Is the terminal emulator the right frame?

Two shapes are available. Host a real terminal and run the TUI unchanged, or
drop ANSI and drive the same state through native views.

The seam described above makes the second option look cheaper than it is. Yes,
`state` is a plain object and `renderFrame` is pure — but everything that *moves*
the state lives in `tui.mjs`: the key handler at line 1150, `commandLogin`,
`commandPublish`, `commandBackend`, `commandModel`, and the wiring that binds
`LivePiece`, `ACSession`, `SlabSession` and the bridge together. That code is not
terminal-specific in spirit, but it is not extracted either, and extracting it is
the actual work. You would end up with a shared core, one ANSI view, one native
view, and `render.test.mjs` covering half of them.

The stronger argument against the rewrite is not cost. It is that
`docs/local-contract.md` says, as a boundary rather than a description: *"The
terminal interface is always Aesel. Engines are internal bridges, not alternate
client interfaces."* A native view layer would be a second client interface. It
might be a good one, but it has to be argued for as a product, not slipped in as
a port. Nothing in the brief for this memo argues for it.

So: **host a real terminal.** Concretely, xterm.js driven by node-pty, with the
existing `bin/easel` running inside it unchanged. The TUI already asks only for
things a competent emulator provides — alternate screen (`\x1b[?1049h`), hidden
cursor, bracketed paste (`\x1b[?2004h`), `setRawMode(true)`,
`stdout.columns/rows`, and a `resize` event. xterm.js does all of it.

Two costs, stated honestly.

**node-pty is a native module**, and it is the thing that will break releases.
It must be rebuilt against each Electron ABI, and a universal macOS build needs
both arch slices. ac-electron already carries this — `@electron/rebuild` is a
dependency and `native:rebuild` is a script — but its `prebuild` step ends in
`|| echo 'Native addon build skipped'`, which is a build that is allowed to
silently produce a broken app.

**A terminal in a window is still a terminal.** You do not get native menus over
the transcript, or drag an image onto the prompt, without inventing a protocol
for it. That is fine, and it points at the right division of effort: put the
native work into the surfaces a terminal genuinely cannot draw — the piece
preview, the scan code, window management — and leave the conversation in ANSI.
Which is, not coincidentally, exactly what Slab already does.

### What ac-electron actually gives us here

I assumed going in that ac-electron was either a foundation or a distraction. It
is neither cleanly, and the specifics matter.

The dependency tree is already right: **Electron 39.2.7, `@xterm/xterm` 5.5.0
with the fit and webgl addons, and `node-pty` 1.1.0**. The main process already
has PTY plumbing — `connect-pty`, `pty-input`, `pty-resize`, and a second
`connect-flip-pty` channel — and `preload.js` exposes
`connectPty/sendToPty/resizePty/onPtyData` to renderers.

And none of it is wired to anything that ships. `renderer/shell.html`,
`development.html` and `terminal-offscreen.html` all use xterm, and `main.js`
loads none of them. `flip-view.html`'s back face, where a terminal is supposed to
be, is a literal empty `<div>`. `RIO-TERMINAL.md` proposes replacing xterm with
Rio's WASM build and references a `createShellWindow()` that does not exist in
`main.js`. Its own status block reads: research completed, integration not
started.

So the honest description is: **the parts are on the shelf and nothing is
assembled.** That is still worth a great deal — the dependency choices are made,
the native rebuild path exists, the IPC shape exists — but nobody should plan on
the basis that ac-electron has a working terminal, because it does not.

What ac-electron *does* ship is a macOS menu-bar daemon that wraps
aesthetic.computer in a nested `<webview>` inside a local `flip-view.html`, with
an animated tray icon, a silo-backed updater, deep links, and a lot of Docker and
devcontainer orchestration. It spawns subprocesses constantly — docker,
devcontainer, emacs, `ac-login.mjs`, a native notepat binary. It has no
cross-reference to `easel/` anywhere.

My read: **build inside ac-electron, not beside it.** A second Electron app with
a second signing config, a second updater and a second release pipeline is the
distraction. The daemon it already is — menu bar, tray, updater, deep links,
`~/.ac-token` — is most of the chrome an Aesel window needs around it.

## 2. What the overlays become

The rocks are worth reading before deciding what happens to them.
`PromptSigilOverlay.swift` is 3,235 lines and says what it is for in its first
paragraph: a rock's *shape* is identity, grown from the session id plus the
current prompt, so the stone re-forms when the session moves on; its *motion* is
the status channel; it carries a pet name; pointing wakes a percussion voice;
clicking reveals the inferred summary. `PromptScanCode.swift` adds the QR,
deliberately unstyled — *"a QR stops being a QR the moment it is lit, tinted,
softened, or tumbled"* — because seventeen rows of half-blocks in the transcript
was too expensive a way to get a piece onto a phone.

The overlay machinery underneath that exists for exactly one reason, stated at
line 17: *"AppKit cannot make our window a child of another process's Terminal
window."* Everything follows from that — the dedicated companion window level,
`reposition`, occlusion gating by sampling the CGWindowList, the private
`_AXUIElementGetWindow` bridge, an osascript pass to bind each tty to a
CGWindowID, spring-follow so the badge trails its terminal with inertia. Several
hundred lines simulating a parent-child relationship the OS refuses to grant
across processes.

Inside an app that owns its own window, **all of that evaporates.** A rock
becomes a view in a layout. No AX permission, no CGWindowList polling, no
osascript, no occlusion test, no tty binding. This is strictly simpler, and it is
the single clearest win of the native shape.

What is lost is more interesting than what is saved.

**The rock's identity function only survives if the app is multi-session.** The
sigil exists so you can tell nine sessions apart at a glance across a wall of
tiled panes. One window with one session has nothing to disambiguate, and the
rock degrades into decoration. So this is a design constraint, not a detail: a
native Aesel should have panes or tabs, and the rock should be what marks them.
If it is a single-session window, drop the rock and keep the scan code.

**Cross-application reach is gone.** Today the rocks decorate Terminal.app and
iTerm2 — someone else's windows. A native app cannot decorate a terminal it does
not own. If @jeffrey keeps working in Terminal.app under Slab, the native app
does not replace that; it becomes a second place Aesel lives. Two shells to keep
in agreement is a real recurring cost and I do not think it goes away.

**The scan code gets better, not worse.** `PromptScanCode` is 99 lines of
CIQRCodeGenerator plus integer-module nearest-neighbour scaling with a baked-in
quiet zone. The whole discipline — every module a whole number of device pixels,
no resampling, no crossfade on swap — transfers to a canvas or an `<img>`
unchanged. In a native window there is a real pixel surface beside the pane
instead of a 56-point stone, so the minimum module size stops being a constraint.

**`PromptPreview.swift` is the piece worth keeping wholesale.** It is already a
WKWebView showing the session's `scan_url` next to its pane, capped at low fps
until you point at it, with a badge whose only job is to say whether what you are
looking at is live, one save behind, or a throttled frame. That file is the proof
that the AC-runtime-as-webview constraint works as an inner pane and not just as
a whole window. It ports directly.

**Keep writing the Slab marker either way.** `SlabSession` writes one JSON file
per session into `~/.local/share/slab/state`. If a native Aesel keeps doing that,
it appears in the existing menu bar and prox ledger for free, and the fleet does
not need to learn a new thing. The marker costs nothing and buys continuity.

## 3. One codebase or three

Four options, and the fourth is the null one.

**(a) Electron only — Mac, Windows, Linux.** Shares 100% of `easel/src`, running
unchanged in a PTY. Reuses ac-electron's Developer ID, its two-stage notarize,
and its silo updater. No iOS, ever. Windows is 95% there in code and 0% there in
evidence.

**(b) SwiftUI + WKWebView — Mac and iOS.** No Windows. And the fatal detail:
**there is no Node.js on iOS.** `easel/src` cannot run there at all. So this
option does not share session logic between its two platforms; it reimplements
it, and then the Mac app and the iOS app share Swift chrome and nothing else. On
macOS it could embed SwiftTerm and spawn `claude` happily — but it would be a
third implementation of the same session, maintained against 89 tests it does not
run.

**(c) Electron for desktop, a separate small SwiftUI app for iOS.** What is
genuinely shared is not code — it is the protocol and the endpoints:
`/api/easel-inference` with the Anthropic Messages shape, `POST /run` with
`{piece, source, codeChannel}`, the presigned publish grant, `~/.ac-token`'s
Auth0 client id and PKCE flow, the `prompt~channel%20<channel>~!autorun` URL
grammar with its load-bearing encoded space, and the fact that a piece is one
file. What is duplicated is the agent loop (about 200 lines of `ac-server.mjs`),
the SSE parser, the publish call, sign-in, and the entire interface.

**(d) Ship no native app.** Worth naming, because Aesel's actual barriers today
are `curl | sh`, "you need Node 18", and "you need a Claude or Codex
subscription", and a window fixes only the first of those. The third is what
`ac-server.mjs` was built to fix, and it was built without any native work at
all.

I recommend **(c)**, with the emphasis on the comma: it is really *(a) now, and
iOS as a separate product decision later*. The desktop app is a packaging job
over code that already works. The iOS app is a new product that happens to speak
the same protocol. Pretending they are two targets of one codebase would produce
a shared layer that is wrong for both.

## 4. What iOS actually gets

No subprocess means the `ac` bridge only, and that is a sharper limit than
"hosted." `easel-inference.mjs` allowlists exactly three models —
`z-ai/glm-4.6`, `qwen/qwen3-coder`, `deepseek/deepseek-chat-v3.1` — with a
comment saying why: *"Adding a frontier model here multiplies the cost of the
free tier by about thirty."* It requires a token, resolves it to an `@handle`,
and meters against the same daily budget as `/api/ask`. So an iOS Aesel is
categorically a weaker agent than the desktop one **by server policy, not by
porting effort**. No Opus, no Fable, no Codex, and a day's allowance shared with
everything else AC buys for that handle.

The piece lives in the app's Documents container, and this is genuinely fine.
`write_piece` sends the whole file every time; there is no patch, no second file,
no directory to walk. The `ac` bridge already treats the workspace as exactly one
file, so the thing iOS cannot give it is a thing it never asked for.

Sign-in needs rewriting. `ac-session.mjs` runs a loopback HTTP server on port
44233 and shells out to a browser — the desktop PKCE shape. iOS wants
`ASWebAuthenticationSession` against a custom scheme. Auth0 supports it; it is
about ninety lines and a callback URL registration, and it is standard work.

So, plainly: **is an iOS Aesel a real editor or a viewer with a prompt?**

It is a real single-file editor with a deliberately weak model, and the
interesting thing about it is not the editing. It is that the piece is live and
already published while you are making it. You type a sentence, the file changes,
`prompt.ac/@handle/slug` is already serving it, and the preview is in the same
window instead of on a second device across the desk. That is a coherent product
and it is arguably a *better* demonstration of what Aesthetic Computer is than
the desktop app, because the whole live-piece apparatus stops needing a second
screen to explain itself.

It is not Aesel. It is the hosted half of Aesel with a touch interface, and it
should be named and scoped as that rather than shipped as "Aesel for iPhone" and
then apologised for.

One more consequence: it cannot be a terminal. Nobody drives slash commands and
raw-mode keys on a phone. So iOS forces the native-view rewrite regardless of
what desktop does — which is, independently, the strongest argument that it is a
separate app and not a target.

## 5. Distribution and signing

### Reuse, which is more than I expected

**Developer ID is solved.** `scripts/setup-host-mac-signing.sh:21` names
`Developer ID Application: Jeffrey Scudder (FB5948YR3S)`. `package.json` sets
`hardenedRuntime: true` and `notarize: true`, and
`scripts/notarize-dmg.js` adds a second pass because — in its own words —
electron-builder notarizes the `.app` but not the DMG wrapper, so *"the DMG ships
unsigned, so `spctl -t install` reports 'no usable signature'."* That is a solved
problem someone already paid for.

**Release plumbing is solved.** Not GitHub Releases, despite what `BUILD.md`
says: `scripts/publish-release.mjs` uploads to DigitalOcean Spaces and registers
with silo, and `electron-updater` points at
`https://releases.aesthetic.computer/desktop` as a generic provider.

**The iOS credentials exist.** App Store Connect app id 6450940883, live since
2023-07-04. Team FB5948YR3S. ASC API key `S4TQKG6U99` with cloud signing via
`-allowProvisioningUpdates` — no match, no sigh, no certificate wrangling. Six
fastlane lanes at `apple/fastlane/`.

**The WKWebView bridge exists.** `apple/aesthetic.computer/ContentView.swift` is
466 lines and already has the pattern an iOS Aesel needs: two message handlers
(`iOSApp`, `iOSAppLog`), a `console.log` monkey-patch at document start, and
native→JS calls into named globals (`window.iOSReceivePushToken`,
`iOSAppSwitchPiece`) with a retry loop for the case where the page has not
defined them yet.

Two cautions on the Apple side. `apple/aesthetic.computer.xcodeproj` is **iOS
only** — `SDKROOT = iphoneos`, no `SUPPORTED_PLATFORMS` override, no Catalyst.
There is no macOS target in it and never has been. And `fastlane ios build` is
currently *broken*: `PROGRESS.md` records it failing on 2026-09-11 because Xcode
26.6 kept the iPhoneOS SDK but has no simulator runtimes, and version 1.1 has
been sitting in `PREPARE_FOR_SUBMISSION` since. An iOS Aesel would inherit a
pipeline that does not presently produce an IPA.

An iOS Aesel would be a **new target or a new project**, not a target of
`aesthetic.computer.xcodeproj`. The precedent in this repo is clear: oskiewar,
trackdrum and tvos-tapes are each their own project, two of them generated by
XcodeGen from a `project.yml`. Follow that.

### Genuinely new work

**Windows, entirely.** `BUILD.md`'s matrix claims "Windows exe ✅ Authenticode"
and describes a `.github/workflows/electron-release.yml` that does not exist —
`.github/workflows/` holds two xbox workflows and nothing else. `dist/` contains
only macOS universal artifacts. `build-all-platforms.fish` hardcodes a
devcontainer path and builds Windows in an `electronuserland/builder:wine`
container. `install.fish` exits 1 on anything but Darwin. There is no evidence a
Windows build has ever succeeded, and Authenticode now effectively requires an
OV or EV certificate on hardware or a cloud signing service — a purchase and a
new pipeline, not a reuse.

**A CI that exists.** Every build today happens by SSHing into a Mac
(`scripts/build-publish-host-mac.sh`, key `jeffrey-macbook`).

**node-pty per ABI and per arch**, as above.

### The Mac App Store, which is the significant finding

**ac-electron cannot go to the Mac App Store as configured.** There is no `mas`
target, no provisioning profile, and no sandbox entitlement anywhere in it. Worse
than absent: `build/entitlements.mac.plist` grants
`com.apple.security.cs.allow-unsigned-executable-memory` and
`com.apple.security.cs.disable-library-validation`, both of which MAS review
rejects outright. This is a Developer ID direct-distribution app by construction.

If we wanted a MAS build regardless, the repo contains its own case study.
`slab/menuband/MenuBand-AppStore.entitlements` is the one thing here that has
actually shipped to the Mac App Store, and its comments spell out what the
sandbox cost:

> Mac App Store distribution REQUIRES the App Sandbox. This file is used ONLY
> for the MAC_APP_STORE build; the direct-download Developer ID build keeps
> MenuBand.entitlements (no sandbox).

and then, in the list of what was removed to pass review: no global keystroke
capture (*"the sandbox forbids it; gated out behind `#if MAC_APP_STORE`"*), no
Apple Events (*"App Review routinely rejects it for a menubar instrument"*), no
MultipeerConnectivity, and *"no broad Downloads, Desktop, Documents, or
home-directory access."*

Applied to Aesel, the equivalent gate is the `claude` and `codex` bridges. The
workspace itself is solvable — MenuBand holds
`com.apple.security.files.user-selected.read-write`, and a folder the user picks
through an open panel plus a security-scoped bookmark is a normal sandboxed
pattern. What is not obviously solvable is two other things: executing a binary
at `~/.local/bin/claude` from inside a container that has no read-execute access
to that path, and the fact that a spawned child **inherits the sandbox** — which
is what ac-electron's own `entitlements.mac.inherit.plist` is for. An inherited
sandbox means the vendor CLI's own Write and Bash tools would be confined to the
same container and the same user-picked grants. An agent that can only write
where you last pointed a file picker is a different tool from the one this repo
describes.

I did not test any of that, and I want to be exact about the difference between
what the repo proves and what I am inferring. The repo proves: MAS requires the
sandbox; MenuBand had to gate out three features to pass; ac-electron's current
entitlements are the opposite of sandbox-compatible; and nothing in this tree has
ever shipped a sandboxed app that spawns anything. The inheritance rule and the
execute-outside-the-container rule are documented Apple behaviour, not something
I ran.

The practical conclusion is cleaner than the policy question, and it is the
useful part: **a sandboxed Mac App Store Aesel would be the `ac` bridge only —
which is the same product as the iOS app.** So MAS and iOS are one decision, not
two, and Developer ID direct distribution is the only home the CLI bridges can
have. That is a tidy line, and it means nobody has to litigate App Review to
decide what the desktop app is.

## What I would build, in order

1. A pane in ac-electron: xterm.js on node-pty, running `bin/easel` unchanged.
   Nothing else. This is the whole of the interface question and it either works
   in a day or it reveals something this memo missed.
2. `PromptPreview` and `PromptScanCode` redrawn as ordinary views beside that
   pane — a throttled webview on `scan_url` and a hard-edged QR. Both already
   exist in Swift and both are small.
3. Panes, and the rock as what marks them. If step 3 does not happen, skip the
   rock entirely; a single-session rock is decoration.
4. Keep writing the Slab marker, so the existing menu bar and ledger see the
   native sessions without being changed.
5. Windows only after somebody has watched `claude` run one command there.
6. iOS as its own project, its own name, and the agent loop rewritten in Swift
   against `/api/easel-inference`.

## What is not verified

- Whether `claude` and `codex` run on Windows at all, whether Node can spawn the
  Windows install shape, and whether the model's own shell commands work without
  Git Bash. No Windows machine, no Windows evidence in the repo. This is the
  load-bearing unknown for option (a).
- Whether an Electron or Linux build has ever succeeded here. The tooling is
  written; there are no artifacts and no logs.
- Whether the App Sandbox would actually refuse a bundled or user-selected vendor
  CLI. Argued from Apple's documented behaviour and MenuBand's shipped
  entitlements, not from an attempted submission.
- Whether `updates.mjs`'s `tar` call works against Windows' bundled bsdtar.
- Whether the iOS pipeline can currently produce an IPA. `PROGRESS.md` says it
  could not on 2026-09-11 and nothing since says otherwise.
