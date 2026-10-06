# Aesel

`aesel` opens the native SwiftUI Mac GUI in [`apple/aesel`](../apple/aesel/README.md).
`a`, `ac` and `easel` open the TUI, which is pro mode: the terminal harness.
The GUI is the ordinary, piece-first Aesel; both run on the same engine bridges.
`ac piece` (or `--piece FILE` / `--genre`) still opens the older piece TUI.
Install these commands with `cd easel && ./install.sh`. Install the native Mac
beta from https://aesel.app, or build it with Xcode using `apple/aesel/run.sh mac`.

Both interfaces require a verified Aesthetic Computer login and an @handle,
including pro/private mode and Claude/Codex providers.

The Electron GUI is retired. Its shell, installers, updater and release scripts
have been removed; shared native notebook resources now live in `aesel/shared/`.
The native app uses its own saved threads and does not accept TUI workspace flags.

```sh
a ~/project
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

Native Mac and iPhone open the Piece workflow. The terminal also exposes media
through `/medium` and `--medium`; its nopaint brush surface is selected with
`--genre nopaint`. Picture, Sound, Paper, and Game Boy toolkits remain
terminal-only. See the [current inventory](../apple/aesel/ROADMAP.md).

Inside the TUI: `/login`, `/logout`, `/whoami`, `/publish [file] [slug]`,
`/autopublish [on|off]`, `/piece [name]`, `/runtime [mjs|lisp|processing]`,
`/backend [claude|codex]`,
`/model [name]`, `/energy`, `/qr`, `/live`, `/new`, `/clear`, `/reify`, `/help`, `/quit`. Press
`ctrl-c` to interrupt a running turn or exit while idle.

Network interruptions keep the conversation and queued input. Aesel lets the provider reconnect first, then resumes an interrupted turn up to three times. A stuck provider retry is reopened after 90 seconds. Ctrl-C stops recovery; `/retry` resumes it. Authentication and billing failures require attention instead of automatic retries.

`/reify` (also `/restart`) loads local code edits in the same terminal without a
release or version bump. It waits for the current reply and uploads, checks the
source for syntax errors, and saves a private checkpoint for this session.
The conversation, draft and cursor, queued messages, provider settings, selected
artifact, preview and Slab identity survive. Queued messages continue after the
reload; the opening prompt is not replayed. Syntax or checkpoint failures leave
the current session running. Agents can request it through
`aesel_settings({action: "reify"})`; the response is `queued` until the turn ends.
`/reify watch` reifies on every save under `aesel/src/` (again to stop); the
watch survives the reifies it causes, and a save that fails the check leaves the
window running until the next one. `/preview <file>` pins a picture, sound,
paper or video to the Slab card and reloads the card each time the file is
written again, including by rename; a bare `/preview` lets go. Together with
`slab/menubar-swift/dev.sh --watch` for the card itself, both halves of the
preview reload on save.

Bun can host the terminal with `AESEL_JS_RUNTIME=bun ac`. Node remains the default. In a checkout, `bun aesel/bin/build-bun.mjs` builds reusable bytecode; source edits or a different Bun version automatically fall back to current JavaScript until rebuilt. `/reify` keeps the selected runtime and launch options. This setting is separate from the piece’s `/runtime` language. The headless `/perf` command uses a permission-restricted Node helper under Bun; Node 24 or newer must be installed.

The optional [native terminal host](experiments/c-tui/README.md) accepts an opening draft in C while the complete Bun TUI starts. It retains the existing providers, tools, recovery and terminal features.

Compare both hosts with `python3 aesel/bin/bench-runtimes.py --entry launch --launcher --runs 7 --out /tmp/aesel-runtimes.json`. Both use the same local reply stream and isolated test accounts. Bun recovery checks: `AESEL_TEST_RUNTIME=bun AESEL_TEST_ENTRY=launch.mjs python3 aesel/test/network-tui.py` and `AESEL_TEST_RUNTIME=bun AESEL_TEST_ENTRY=launch.mjs python3 aesel/test/restart-tui.py`.

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
`/model` accepts a model name for your own vendor CLI.

AC braincell inference uses an automatic model. Saved manual model choices are
normalized when a hosted thread resumes. Desktop version settings show braincells
and their USD equivalent, separating the daily free allowance from purchased
credit. The equivalent uses the server's pack rate ($5 per million braincells),
not the underlying provider's token price. Claude and Codex keep their model controls.

Desktop notebook text wraps beside the preview and returns to full width below it;
scrolling and preview resizing update the available space. The preview has one
manually resized footprint, with no hover zoom. Clicking it gives the piece its
keyboard; clicking the notebook returns to message input. The donkey's temporary
parenthetical thought bubble follows public activity and clears when the run ends.
New desktop windows show a themed notebook on first paint, before engine startup;
terminal initialization stays hidden. A thin waveform behind the title follows
actual speaker output and fades away in silence. The donkey and thought bubble
stay together and share a small bounce (still with reduced motion enabled).
Preview wrapping settles before paint so window-height changes do not rebound
through the conversation; new replies follow the bottom only when you are there.

`/about`, or clicking **AESEL**, opens the feature map. Click **@handle** to open
your profile in a browser. Header targets highlight on hover in terminals that
support mouse reporting. `/mouse off` restores terminal selection; `/mouse on`
enables interaction again. `AESEL_MOUSE=0` disables it at launch.

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
speaks, driven directly over a pipe. That is why Aesel still has no
dependencies: a subprocess on stdio is the same shape as `codex app-server
--stdio`, and it carries streaming, tool calls and approvals without a package
tree behind it. Each bridge signs in with the vendor CLI's own credentials
already on the machine.

Approvals come back to this terminal on both bridges: `y` once, `a` for the
session, `n` to deny. In piece mode, neither bridge inherits the user's own agent
configuration — Codex is pinned to `on-request` approvals and a
`workspace-write` sandbox, and Claude is launched with `--setting-sources ""`
and `--strict-mcp-config` — so nothing but the person watching can approve a
command in a session, and an `a` is never written to a settings file.

On the Claude bridge the session also carries Aesel's own tools, served by
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

A terminal Piece session opens a blank AC piece by default; `--genre nopaint`
selects a brush. Each gets a random pronounceable name, it is a real file in the workspace, and a QR code for it sits in the
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

Aesel reads the shared Aesthetic Computer sign-in at `~/.ac-token`,
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
flushes the pending save on exit. Disable it with `--no-autopublish`, `/autopublish off`, or
`AESEL_AUTOPUBLISH=0`. Native notebooks save their own per-thread publication
setting. Turning it off does not remove earlier public versions. With it on the engine is told the piece is
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

`npm run test:ui` checks typing, streaming, scrolling, and animation in a
300-message conversation against a 16.7 ms frame budget. The donkey, breathing
handle, and message shimmer stay enabled. It also checks reflow from 100 to 32
columns against 100 ms, and the first code block in a fresh process against
16.7 ms. The JavaScript parser loads only when code needs highlighting or notebook
controls. `npm run test:input` checks real PTY bursts, cursor edits, Unicode
paste, and trimmed submission. Characters in one input chunk share a paint;
individual keystrokes still paint immediately. `npm run test:latency` adds that
input check, terminal observer tests, and
five real Aesel PTY sessions.
It fails if p95 exceeds 250 ms to an editable prompt, 16.7 ms to echo input
(idle or streaming), 100 ms from Enter to request, 250 ms to first reply, or
25 ms from a supplied token to terminal output. Timing tests run separately
from the parallel correctness suite to avoid measuring test-runner contention.

`npm run bench:tui` compares Aesel's direct loop and its Claude/Codex bridges
with the installed native TUIs. It interleaves five launches of each and saves
raw samples, versions, source hashes, p50/p95, and failed comparisons to
`tmp/tui-latency/latest.json` at the repository root. It exits nonzero unless
Aesel meets its budgets and matches or beats each peer at both percentiles.
Missing binaries or incomplete replies fail; they are never fast samples.

`npm run test:startup` checks an installed-style `ac` symlink against Codex over
20 interleaved trials, including shell and symlink resolution. Add `--launcher a`
or `--launcher aes` to check those aliases; `--launcher direct` measures the
launcher file without an installed symlink. It requires a strict win at both p50 and p95, complete
replies in every trial, and unchanged source hashes throughout the run.
`npm run test:startup -- --runtime bun` checks the optional Bun host. Build with
`npm run build:tui` (Node) or `npm run build:bun` before measuring; traces record
whether verified built code or source was loaded. Node's compile cache starts
empty, is reused across trials, and retains the first cold sample. Add
`--cold-cache` for an empty Node cache on every trial. Filesystem caches remain
warm; Bun bytecode is built ahead of time. Reports go to
`tmp/tui-latency/startup.json`. Bun resolves installed launcher symlinks inside
its own process. `AESEL_TEST_RUNTIME=bun AESEL_TEST_LAUNCHER=ac python3 test/restart-tui.py`
checks reify through that public launcher, including loading
edited source from a relocated installation.
Reports separate prompt visibility and first input echo, CLI setup, build
verification, terminal setup, first render, engine import, and worker timings.
They record terminal observer overhead without subtracting it.
They also record executable paths, host load, and PATH directory probe times. Slow or
unavailable automounts in PATH can delay shell launchers before JavaScript starts;
the benchmark preserves the inherited PATH so that cost stays visible.

The benchmark uses isolated settings, a fake AC account, and identical local
SSE replies: 100 ms to the first token, then one chunk every 40 ms. No paid
model calls or personal credentials are used. Account/network latency, personal
hooks/MCP configuration, GUI launchers, and physical pixel presentation are
outside the measurement. “Open” means process launch to confirmed editable
input; the first reply includes any remaining bridge startup. Filesystem caches
are not cleared. Claude uses `--bare`; Codex uses `--no-daemon`. The direct
Aesel target uses its real agent loop with only the inference endpoint replaced.
For a narrow run: `python3 bin/bench-tui.py --columns 40 --runs 5 --compare`.

To compare actual hosted inference, run from `aesel/`:

```sh
npm run bench:providers -- --live --task ping --runs 3 --out ../tmp/provider-speed/ping.json
npm run bench:providers -- --live --task sequence --runs 3 --out ../tmp/provider-speed/sequence.json
npm run bench:providers -- --live --task coding --runs 3 --out ../tmp/provider-speed/coding.json
```

`--live` spends the signed-in AC and Codex accounts' allowance. Each sample uses
a fresh temporary workspace and thread with a synthetic prompt; the report omits
reply text and credentials. Runs alternate provider order and record connection,
first text, completion, model, reasoning, usage, and validation. The coding task
checks an interval merger against seven cases, including input preservation.
Timeouts and incorrect answers fail instead of counting as fast responses.
`--targets ac` or `--targets codex` limits the comparison; `--ac-model` and
`--codex-model` override defaults. Different models, system prompts, tool
configuration, and cache histories make this a product comparison, not an
equal-quality model evaluation. With three samples, p95 is simply the maximum.
Engine events do not measure GUI paint latency. Native launch/control timing has
a separate [debug-app benchmark](../apple/aesel/README.md).

## Aesel pro modules

Three dependency-free modules under `src/` carry the pro (terminal harness)
mode; the TUI wires them in.

- `profile.mjs` — `resolveProfile({ cwd, flags, configPath, env })` decides
  `piece` or `pro`, and `private`, from `--pro`/`--private`, `AESEL_PRIVATE=1`
  and the globs in `~/.config/aesel/profiles.json` (`exampleConfig()` prints
  the shape). Pro publishes nothing and passes the engine through; private
  advertises state only, so the Slab marker carries no subject.
- `inbox.mjs` — `Inbox` listens on `$SLAB_HOME/inbox/<session>/inbox.sock`,
  drains `messages.jsonl`, acks each line and emits stamped messages.
- `transcript.mjs` — `Transcript` writes one `events.jsonl` + `meta.json`
  per session under `~/.local/share/aesel/transcripts`, whatever the engine.

## Designing the furniture

Aesel does not draw all of itself. The QR, the live card of the piece and the
status stone are Slab menubar overlays parked on the terminal, and `frame`
filters Slab's own windows out of every capture — its usual job is reading the
machine underneath them. So a screenshot taken to judge the card's padding
shows the terminal where the card is.

`frame <machine> --overlays` opts out of that exclusion and widens a window
shot to a padded crop, so the menu bar an overlay is said to be flush against
is in the same picture.

`aesel/bin/design-loop.mjs` is the whole cycle in one command: close the Aesel
session, open a fresh one, wait for its overlays to land, photograph them.
Fresh because overlays are placed once, when a window appears — editing the
placement and reinstalling the menubar does not move what is already on screen,
so the only honest check is a session that has never seen the old numbers.

```sh
node easel/bin/design-loop.mjs            # restart Aesel, then shoot
node easel/bin/design-loop.mjs --shot     # shoot what is already open
```

Edit an overlay, run `slab/menubar-swift/install.sh`, then run the loop.

Aesel is proprietary. See `LICENSE`.

On Fish installations with existing `ac` or `aesthetic` functions, the
installer preserves them as `ac-repo` and `aesthetic-platform`.

The current account, publishing, inference, transcript, and billing boundary is recorded in
[`docs/local-contract.md`](docs/local-contract.md).

Each complete piece update gets a local version (`v1`, `v2`, …). `/versions`
lists snapshots; `/rollback vN` restores one as a new version and sends it through
the usual live/publish path. Finish or interrupt the current turn and let uploads
finish first. History persists in `~/.local/share/aesel/history/`, keyed by the
piece's absolute file path; it is not yet shared between machines or accounts.

Aesel was called Easel. Settings in `~/.config/easel`, history in
`~/.local/share/easel` and the model cache in `~/.cache/easel` move to the aesel
names on first run, with a link left at each old path; a workspace's `.easel/`
and `.easel-media/` stay where they are and keep being used. Every `EASEL_*`
variable still works as its `AESEL_*` twin. `src/paths.mjs` and `src/env.mjs`
hold the rules.

The AC backend streams text and completed `write_piece` checkpoints as they
arrive. It shows connecting, waiting, generating, composing, and writing states;
received kilobytes count stream bytes, not billed tokens. JavaScript checkpoints
are syntax-checked without executing them, so unfinished fragments keep the last
working preview. Other runtimes retain their own loader validation. This uses
ordered HTTPS streaming (SSE); a socket or UDP transport is not required for each
token to arrive immediately. Disconnects cancel an active response upstream.

## Aesel pro

`ac --pro [dir]` is the same terminal, pointed at ordinary work: no piece, no
QR, nothing published, and the Claude bridge runs with your own settings,
skills, hooks and MCP servers (`--setting-sources`, `--strict-mcp-config` and
the withheld tools are dropped; approvals still come back here, and `/ask` is
on until you say `/ask off`). `--private` keeps the session's subject out of
the Slab marker and the transcript index. Both can be set per directory in
`~/.config/aesel/profiles.json` — `/mode` prints the shape and says why the
current session resolved the way it did.

Every session, pro or not, listens on an inbox: `$SLAB_HOME/inbox/<session>/
inbox.sock`, advertised as `inbox_socket` in its Slab marker, with
`messages.jsonl` beside it as the fallback a remote sender can append to. A
line from another session shows as `↓ host:name · text`, starts a turn if the
machine is free, queues if it is busy, and with `"urgency": "urgent"` interrupts
the running turn and goes first. `/inbox` shows the pending count and the last
few delivered. Nothing on this path types into the terminal.

One transcript per session, whichever engine wrote it, lands under
`~/.local/share/aesel/transcripts/<session>/` as `events.jsonl` + `meta.json`.
