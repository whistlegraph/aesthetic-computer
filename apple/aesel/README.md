# Aesel for iPhone

A SwiftUI app. The renderer is native; the thinking is not.

## The division, and why

Oskiewar's Metal view says the house rule in its own header: *"the engine speaks
the same triangle stream to every host it has ever had — canvas on the web, D3D
on Xbox, Metal here."* One engine, a narrow contract, a renderer per platform.

Aesel already had that contract before it needed a second renderer. `AcServer`
emits six notification kinds — `turn/started`, `turn/progress`,
`item/agentMessage/delta`, `item/started`, `item/completed`, `turn/completed` —
and the desktop TUI is only one thing that draws them. `Session.swift` is
another. That is the whole port.

So the agent loop is not rewritten here. `easel/src/ac-server.mjs` and
`easel/src/publish.mjs` run unmodified, inside a `WKWebView` that is never
shown, and SwiftUI draws every pixel the user sees.

**Why a hidden webview and not JavaScriptCore.** Oskiewar's Mac app hands JSC a
bundled script and drives Metal, and pays for it: JSC is bare ECMAScript, and
`main.swift:569` notes it gets no JIT in a third-party app. That trade is right
for a game, because a webview cannot draw one. Aesel's renderer is text, and its
piece preview has to be a `WKWebView` on every platform regardless — so the
thing Oskiewar was avoiding is something Aesel needs anyway. Hosting the session
in a second, invisible one costs an object nobody sees and buys `fetch`,
streaming `response.body.getReader()`, `TextDecoder` and ES modules, none of
which JSC has and all of which the shared bridge is written against.

## Running it

```sh
./run.sh                  # simulator
./run.sh device           # the iPhone plugged into this Mac
./run.sh device "make a bouncing ball"
```

The session is served from this Mac by `easel/phone/serve.mjs`, not bundled, so
a JavaScript edit reaches the phone on relaunch with no rebuild. `run.sh` starts
that server if it is not already up and stamps the current LAN address into
`Info.plist` for device builds — the address is discovered rather than committed
because it changes (this Mac moved from a phone hotspot to Wi-Fi in the middle
of the first build).

`AESEL_ASK` runs one prompt on launch. It waits for the session to report
`restored` rather than sleeping, so it cannot fire before there is a piece.

## Two things that bite

**Readiness is not `didFinish`.** The navigation delegate fires when the
document has loaded, which is before the module script has evaluated, so
`globalThis.aesel` may not exist yet — calling into it there raced and threw
`Can't find variable: aesel`. The page posts a `ready` event after installing
that global, and `SessionHost` treats *that* as the signal.

**Forbidden headers.** `publish.mjs` sets `User-Agent`, correctly, for Node.
Chrome silently drops it; WebKit leaks it into the CORS preflight, and
`/presigned-upload-url` allows only `Content-Type, Authorization,
X-Requested-With`. The preflight is refused and the whole publish returns the
uninformative "Load failed". `session.mjs` strips forbidden headers for the
publish path rather than changing the shared file, which still wants to identify
itself on the desktop.

## What is not done

* **Sign-in is a development shortcut.** The host page asks the laptop for its
  own `~/.ac-token`. The real flow is `ASWebAuthenticationSession` against the
  same Auth0 client the desktop uses (`hi.aesthetic.computer`, client
  `LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt`), which needs a callback URL registered in
  Auth0 first — that is an account change, not code.
* **The session is not bundled.** Shipping means copying `easel/src`,
  `easel/context` and `easel/phone` into the app and loading them over a custom
  scheme, the way Oskiewar's `BundleSchemeHandler` serves its offline fallback.
* **It targets this branch's bridge, not `main`'s.** Main has six tools instead
  of one, plus runtime feedback and frame capture, and needs `node:fs/promises`,
  `node:util`, a PNG encoder and a *synchronous* `createHash('sha256')` that
  `SubtleCrypto` cannot provide. Tracking it matters for more than tidiness —
  see below.
* **No app icon, no launch asset, no ASC record.**

## The model is the weak part, and there is a reason

Measured on the first three turns: every failure was the model writing a piece
that throws, not the client. It wrote `wipe(color, x, y, w, h)` once and
`ink(...)` without destructuring `ink` another time — `paint` threw after the
first call, so the preview showed a solid colour and looked plausible.

Worse, it then rewrote the same broken piece twelve times and hit the bridge's
round bound, because **nothing told it the piece was erroring.** `main` has
`runtime-feedback.mjs` and an `ac_preview` tool that close exactly that loop.
That is the strongest argument for tracking main, and it is a correctness
argument rather than a housekeeping one.
