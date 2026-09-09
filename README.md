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
and approvals. It does not launch a managed provider client. The current engine
bridge uses `codex app-server`; its inference is remote and the TUI labels that
boundary before a prompt is sent. A native local-inference engine can replace
the bridge without changing the interface.

Each live TUI publishes its own Slab marker, so the menubar and prox ledger can
name, focus, wake, close, and track it as `aesthetic-code`, including which
@handle it acts as.

The interface uses the Aesthetic Computer prompt's palette (purple ground, pink
prompt, orange highlight, magenta handle) and shows the signed-in `@handle` and
the piece currently being worked on in the header.

Inside the TUI: `/login`, `/logout`, `/whoami`, `/publish [file] [slug]`,
`/piece [name]`, `/runtime [mjs|lisp|lua]`, `/qr`, `/live`, `/new`, `/clear`,
`/help`, `/quit`. Press `ctrl-c` to interrupt a running turn or exit while idle.

## The session's piece, live on a phone

Opening Aesthetic Code opens a new blank piece. It gets a random pronounceable
name, it is a real file in the workspace, and a QR code for it sits in the
bottom right of the interface. Scan the code and the piece runs on your phone;
every edit the agent makes reaches it a moment later.

The link works through the code channel Aesthetic Computer already uses for
live editing. The QR opens `prompt~channel~<channel>~!autorun`, which runs the
prompt's own `channel` command, and the interface POSTs each save to `/run`,
which relays it over Redis and the session server to everything watching that
channel. It is the same path the VS Code extension has always used, so nothing
new runs on the server.

A blank that is never edited is deleted when the session ends, so opening and
closing the interface leaves the workspace exactly as it was. In the Aesthetic
Computer repository the piece is written to `system/public/aesthetic.computer/
disks/`; anywhere else it lands in the workspace root.

`/runtime` switches the language of the piece — `mjs` (JavaScript), `lisp`
(KidLisp), or `lua` — and rewrites the blank. `--runtime` picks it at launch.
Live pushes work for all three; `aesthetic.computer/@handle/<name>` resolves
`.mjs` and `.lisp` today, so a Lua piece runs live but has no published route
yet.

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
