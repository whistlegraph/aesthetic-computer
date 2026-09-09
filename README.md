# Aesthetic Code

A coding interface for the terminal.

```sh
./install.sh
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
name, focus, wake, close, and track it as `aesthetic-code`, including which
@handle it acts as.

The interface uses the Aesthetic Computer prompt's palette (purple ground, pink
prompt, orange highlight, magenta handle) and shows the signed-in `@handle` and
the piece currently being worked on in the header.

Inside the TUI: `/login`, `/logout`, `/whoami`, `/publish [file] [slug]`,
`/piece [name]`, `/runtime [mjs|lisp|processing]`, `/backend [claude|codex]`,
`/model [name]`, `/qr`, `/live`, `/new`, `/clear`, `/help`, `/quit`. Press
`ctrl-c` to interrupt a running turn or exit while idle.

## Engine bridges

Two bridges ship, and either can drive a session:

```sh
ac                                  # claude, on claude-fable-5-1
ac --backend codex                  # codex app-server
ac --model claude-opus-5            # a different model on the same bridge
```

`/backend` and `/model` do the same thing mid-session — both restart the
conversation on the new engine and leave the piece, the channel and the QR code
exactly where they were. `/backend` with no argument says which engine and
model are running.

The Claude bridge runs `claude --print --input-format stream-json
--output-format stream-json`, the same headless protocol the Claude Agent SDK
speaks, driven directly over a pipe. That is why this repository still has no
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

The two are not equivalent on containment. Codex runs commands inside an
operating-system sandbox with the network off; Claude Code has no such sandbox,
so on that bridge the approval prompt is the whole boundary. The difference is
written down in [`docs/local-contract.md`](docs/local-contract.md).

## The session's piece, live on a phone

Opening Aesthetic Code opens a new blank piece. It gets a random pronounceable
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

Aesthetic Code reads the shared Aesthetic Computer sign-in at `~/.ac-token`,
the same file `ac-login` and the AC desktop apps use. `/login` runs the
Authorization-Code + PKCE flow in your browser with a loopback callback and
writes that file; a sign-in or sign-out anywhere in the suite updates the
header live. Only the handle is shown.

`/publish <file> [slug]` puts a `.mjs` or `.lisp` piece live under your handle
at `https://aesthetic.computer/@handle/slug`, exactly like the web prompt's
`publish` command: it requests a presigned upload grant in your user bucket,
uploads the source, and reads the live file back before reporting the URL.
Writing a file under `disks/` does not publish it, and the engine is told so.

```sh
aesthetic login
aesthetic whoami
aesthetic publish system/public/aesthetic.computer/disks/smiley.mjs
ac --runtime lisp
ac --backend codex
```

```sh
aesthetic doctor
npm test
```

This repository is proprietary. See `LICENSE`.

On Fish installations with existing `ac` or `aesthetic` functions, the
installer preserves them as `ac-repo` and `aesthetic-platform`.

The product boundary is recorded in
[`docs/local-contract.md`](docs/local-contract.md).
