# Easel

A coding interface for the terminal. Lives at `easel/` in the
Aesthetic Computer repository.

```sh
cd easel && ./install.sh
ac
```

`ac` and `aesthetic` open the same full-screen interface in the current
directory. Pass a directory to open another workspace:

```sh
ac ~/project
```

The interface owns the conversation, streaming, tool activity, interruption,
and approvals. It does not launch a managed provider client. Behind it sits an
engine bridge: a subprocess on stdio that the interface drives. A native
local-inference engine can replace the bridge without changing the interface.

Each live TUI publishes its own Slab marker, so the menubar and prox ledger can
name, focus, wake, close, and track it as `easel`, including which
@handle it acts as. The marker also carries the piece and its address, which the
menubar draws as a scannable code on the session's prompt rock.

A signed-in session owns its code channel by name — `<handle>/<slug>` — and
`/run` accepts a push to it only from that handle's token. Ownership is the
token, not the secrecy of the name, so the channel's name can be the piece's
public address: `prompt.ac/@handle/slug` is what the rock shows, from the moment
the session opens until after it closes. Signed out there is nothing published
and no channel to own, so the code falls back to the opaque `~<channel>` route.

The interface uses the Aesthetic Computer prompt's palette (purple ground, pink
prompt, orange highlight, magenta handle) and shows the signed-in `@handle` and
the piece currently being worked on in the header.

Inside the TUI: `/login`, `/logout`, `/whoami`, `/publish [file] [slug]`,
`/autopublish [on|off]`, `/piece [name]`, `/runtime [mjs|lisp|processing]`,
`/backend [claude|codex]`,
`/model [name]`, `/energy`, `/qr`, `/live`, `/new`, `/clear`, `/help`, `/quit`. Press
`ctrl-c` to interrupt a running turn or exit while idle.

## Engine bridges

Three bridges can drive a session:

```sh
ac                                  # claude, on claude-opus-5
ac --backend codex                  # codex app-server
ac --backend ac                     # AC hosted, using your handle's budget
ac --model claude-opus-5            # a different model on the same bridge
ac --piece path/to/fogozo.mjs        # reopen an existing piece and its versions
```

`/backend` and `/model` switch mid-session, preserving the visible conversation,
piece, channel and QR. A new provider thread receives recent user/assistant
context (up to 24,000 characters) and the current piece; provider thread IDs and
tool history are not portable. A failed connection returns to the prior engine.
`/new` explicitly starts a fresh conversation. `/backend` lists account options;
`/model` lists hosted choices or accepts a model name for your own vendor CLI.

AC hosted keeps GLM as its default. `/model sonnet` and `/model gpt` select
premium models and consume the same handle allowance. Model IDs were checked
against the [OpenRouter catalog](https://openrouter.ai/compare/openai/gpt-5.4/anthropic/claude-sonnet-4.6).
The allowance measures weighted tokens, not dollars, and is not an atomic spend
reservation. Unavailable budget checks refuse inference. New hosted choices
require the matching Lith endpoint deployment.

`/about`, or clicking **EASEL**, opens the feature map. Click **@handle** to open
your profile in a browser. Header targets highlight on hover in terminals that
support mouse reporting. `/mouse off` restores terminal selection; `/mouse on`
enables interaction again. `EASEL_MOUSE=0` disables it at launch.

Wheel and Page Up/Page Down scroll the transcript internally, keeping the input
and footer fixed. Incoming output preserves your reading position. End with an
empty input, or `/latest`, returns to the live end. The about map scrolls too;
Esc returns to the conversation.

`/performance [frames]` measures the current JavaScript piece's headless logic
with seeded randomness and drawing-call counts. The default is 600 measured
frames at 800×600 after warmup. It runs in a restricted child with a timeout;
Ctrl-C cancels it. Browser rendering, rasterization and display latency are
excluded. Unsupported APIs/imports report an error. It requires Node permission
support (Node 24 or newer recommended).

`/energy` estimates what the session cost in electricity. Every bridge reports
the tokens it spent — per round on AC hosted, per turn from the Claude CLI's own
`modelUsage` — and `src/energy.mjs` turns those counts into watt-hours: a fixed
cost per generated token plus a part that scales with the model's *active*
parameters, with prompt tokens at a tenth of a generated one and cached tokens
at a hundredth. The running total shares the footer's gauge row with the viewer
count, wearing a `~`.

It is an estimate and cannot be anything else — no provider publishes per-token
energy. The slope is anchored so a frontier-class answer lands near the only
published figures (Google's 0.24 Wh median text prompt; Epoch AI's ~0.3 Wh for a
GPT-4o query), and the open-weight hosted models carry their announced active
parameter counts, so the *relative* half of the readout — the same conversation
priced across every model, cheapest first — rests on published numbers rather
than on guessed hardware. Rows for closed models say that their size is a guess.
Serving only: no training, no water, and not your own machine.

The Claude bridge runs `claude --print --input-format stream-json
--output-format stream-json`, the same headless protocol the Claude Agent SDK
speaks, driven directly over a pipe. That is why Easel still has no
dependencies: a subprocess on stdio is the same shape as `codex app-server
--stdio`, and it carries streaming, tool calls and approvals without a package
tree behind it. Each bridge signs in with the vendor CLI's own credentials
already on the machine.

Approvals come back to this terminal on both bridges: `y` once, `a` for the
session, `n` to deny. Neither bridge inherits the user's own agent
configuration — Codex is pinned to `on-request` approvals and a
`workspace-write` sandbox, and Claude is launched with `--setting-sources ""`
and `--strict-mcp-config` — so nothing but the person watching can approve a
command in a session, and an `a` is never written to a settings file.

On the Claude bridge the session also carries Easel's own tools, served by
`src/tools.mjs` as the one MCP server the strict config admits: `ac_api` (the
piece API — runtime signatures, docs and real call sites, read off
`lib/disk.mjs` and `lib/graph.mjs` by `bin/build-api-map.mjs` into
`context/api.json`), `ac_examples` (pieces that call a symbol), `ac_outline`
(a piece's top-level symbols with line spans) and `ac_symbol` (one symbol's
source). They exist because the first ten sessions each spent six to twelve
shell calls — `grep function circle( graph.mjs`, `sed -n 6590,6650p disk.mjs`,
`grep -rn "synth({" disks/` — rebuilding the same picture before the first
edit. The guides are inlined into the first turn for the same reason. All four
tools are read-only and pre-allowed; `npm run context` rebuilds the map and
`npm test` fails when it is stale.

The two are not equivalent on containment. Codex runs commands inside an
operating-system sandbox with the network off; Claude Code has no such sandbox,
so on that bridge the approval prompt is the whole boundary. The difference is
written down in [`docs/local-contract.md`](docs/local-contract.md).

## The session's piece, live on a phone

Opening Easel opens a new blank piece. It gets a random pronounceable
name, it is a real file in the workspace, and a QR code for it sits in the
bottom right of the interface. Scan the code and the piece runs on your phone;
every edit the agent makes reaches it a moment later.

The link works through the code channel Aesthetic Computer already uses for
live editing. The QR opens `prompt~channel%20<channel>~!autorun`, which runs the
prompt's own `channel` command, and the interface POSTs each save to `/run`,
which relays it over Redis and the session server to everything watching that
channel. It is the same path the VS Code extension has always used, so nothing
new runs on the server.

The encoded space is load-bearing. Aesthetic Computer routes anything beginning
`prompt~` by handing the whole remainder to the prompt as a single parameter,
and the prompt splits its own arguments on spaces: write the channel after a
tilde and it arrives glued to the command name, so `channel` runs with nothing
to join and the phone sits on an empty prompt.

A push reaches only whoever is already on the channel — nothing keeps a copy of
one — so the interface re-sends the current source every few seconds. That is
what lets the code be scanned at any moment rather than only just after a save;
a phone that arrives mid-session picks the piece up within a beat.

A blank that is never edited is deleted when the session ends, so opening and
closing the interface leaves the workspace exactly as it was. In the Aesthetic
Computer repository the piece is written to `system/public/aesthetic.computer/
disks/`; anywhere else it lands in the workspace root.

`/runtime` switches the language of the piece — `mjs` (JavaScript), `lisp`
(KidLisp), or `lua` (Processing, via L5) — and rewrites the blank. `--runtime`
picks it at launch, and `processing`, `kidlisp`, `js` and `l5` are accepted as
names for the same three. Live pushes work for all three;
`aesthetic.computer/@handle/<name>` resolves `.mjs` and `.lisp` today, so a
Processing piece runs live but has no published route yet.

Processing pieces are written in Processing's vocabulary — `setup` and `draw`,
`background`, `fill`, `circle` — not Aesthetic Computer's `paint`. That is also
what keeps them running: a live push carries no file extension, so the client
recognises Lua by reading the source, and only source that opens with a `--`
comment and declares `setup` or `draw` is taken as Lua at all.

## Account and publishing

Easel reads the shared Aesthetic Computer sign-in at `~/.ac-token`,
the same file `ac-login` and the AC desktop apps use. `/login` runs the
Authorization-Code + PKCE flow in your browser with a loopback callback and
writes that file; a sign-in or sign-out anywhere in the suite updates the
header live. Only the handle is shown.

`/publish <file> [slug]` puts a `.mjs` or `.lisp` piece live under your handle
at `https://aesthetic.computer/@handle/slug`, exactly like the web prompt's
`publish` command: it requests a presigned upload grant in your user bucket,
uploads the source, and reads the live file back before reporting the URL.
Writing a file under `disks/` does not publish it, and the engine is told so.

Auto-publish does that on every save, so the session's URL is live the whole
time the piece is being worked on and the last edit is still there after the
terminal closes. It is **on by default**: the address on the rock is the piece's
published address, so a session that never publishes has nothing to point a
camera at. It coalesces — a publish runs once the saves stop,
never more than one at a time, and never twice for the same bytes — and it
flushes the pending save on exit. Off by default, since it writes to a public
route under your own handle: turn it on per session with the flag or
`/autopublish`, or for every session with `EASEL_AUTOPUBLISH=1`, which
`--no-autopublish` overrides. With it on the engine is told the piece is
already live and told not to ask you to publish.

```sh
aesthetic login
aesthetic whoami
aesthetic publish system/public/aesthetic.computer/disks/smiley.mjs
ac --runtime lisp
ac --backend codex
ac --autopublish
```

```sh
aesthetic doctor
npm test
```

## Designing the furniture

Easel does not draw all of itself. The QR, the live card of the piece and the
status stone are Slab menubar overlays parked on the terminal, and `frame`
filters Slab's own windows out of every capture — its usual job is reading the
machine underneath them. So a screenshot taken to judge the card's padding
shows the terminal where the card is.

`frame <machine> --overlays` opts out of that exclusion and widens a window
shot to a padded crop, so the menu bar an overlay is said to be flush against
is in the same picture.

`easel/bin/design-loop.mjs` is the whole cycle in one command: close the Easel
session, open a fresh one, wait for its overlays to land, photograph them.
Fresh because overlays are placed once, when a window appears — editing the
placement and reinstalling the menubar does not move what is already on screen,
so the only honest check is a session that has never seen the old numbers.

```sh
node easel/bin/design-loop.mjs            # restart Easel, then shoot
node easel/bin/design-loop.mjs --shot     # shoot what is already open
```

Edit an overlay, run `slab/menubar-swift/install.sh`, then run the loop.

Easel is proprietary. See `LICENSE`.

On Fish installations with existing `ac` or `aesthetic` functions, the
installer preserves them as `ac-repo` and `aesthetic-platform`.

The product boundary is recorded in
[`docs/local-contract.md`](docs/local-contract.md).

Each complete piece update gets a local version (`v1`, `v2`, …). `/versions`
lists snapshots; `/rollback vN` restores one as a new version and sends it through
the usual live/publish path. Finish or interrupt the current turn and let uploads
finish first. History persists in `~/.local/share/easel/history/`, keyed by the
piece's absolute file path; it is not yet shared between machines or accounts.

The AC backend streams text and completed `write_piece` checkpoints as they
arrive. It shows connecting, waiting, generating, composing, and writing states;
received kilobytes count stream bytes, not billed tokens. JavaScript checkpoints
are syntax-checked without executing them, so unfinished fragments keep the last
working preview. Other runtimes retain their own loader validation. This uses
ordered HTTPS streaming (SSE); a socket or UDP transport is not required for each
token to arrive immediately. Disconnects cancel an active response upstream.
