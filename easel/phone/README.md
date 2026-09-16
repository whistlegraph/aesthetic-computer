# Aesel on a phone

The hosted half of Aesel with a touch interface. It is not a port: the agent
loop is `easel/src/ac-server.mjs`, loaded unmodified, and publishing is
`easel/src/publish.mjs`, also unmodified. This directory is only the shell
around them.

## Why this shape

`easel/docs/native-shell.md` concluded that iOS should be its own project with
"the agent loop rewritten in Swift." Reading `ac-server.mjs` closely argues
against that. Its entire Node surface is `EventEmitter`, three `fs` calls,
`path.join`, `randomUUID` and `process.env` — and `piece`, `token`, `fetch` and
`site` are already constructor-injected. Every one of those is a few lines of
shim, so a Swift rewrite would buy nothing and guarantee the phone and the
desktop drift apart. `shim/` answers the imports instead, through an import map
in `index.html`.

The one thing that genuinely could not be shimmed is `validatePieceSource`,
which shells out to `node --check`. `shim/revisions.mjs` explains what replaced
it and what that costs.

## Running it

```sh
node easel/phone/serve.mjs --port 8770 --token
```

Serves the repository so the bridge's own paths resolve — `/easel/src/*.mjs` and
`/easel/context/*.md` are fetched at the very paths `bundledContext()` builds.
Only those three trees are reachable; the root is the repository, so the fence
matters.

`--token` exposes `/dev-token`, which hands the page the access token out of
`~/.ac-token` so a phone on the same network can sign in before any Apple
plumbing exists. Private addresses only, and off by default. It is a bearer
token: do not run it on a network you do not own.

Open the printed LAN address on the phone. Without `--token`, the page asks for
a pasted token instead.

## What it does

Type a sentence; the bridge writes the whole piece; the write handler publishes
it under your `@handle`; the preview reloads. The piece is live on
`aesthetic.computer` while you are still making it, which is the thing this
client is actually for.

Caveats worth knowing before judging output: the server allowlists three cheap
models, so this is a deliberately weaker agent than desktop Aesel, and the
allowance is the same daily one `/api/ask` spends.

## Where it is going

This page is the content of the iOS app, not a detour. The Swift shell replaces
three things and nothing else:

| Browser now | iOS later |
|---|---|
| `/dev-token` or a pasted token | `ASWebAuthenticationSession`, then `window.aeselAdoptToken(token)` |
| `localStorage` | the app's Documents container, via `vfs.setWriteHandler` |
| an `<iframe>` | the same `<iframe>`, or a second `WKWebView` |

`app.mjs` is written against those seams already.
