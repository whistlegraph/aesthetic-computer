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

Inside the TUI: `/login`, `/logout`, `/whoami`, `/publish <file> [slug]`,
`/piece [name]`, `/new`, `/clear`, `/help`, `/quit`. Press `ctrl-c` to interrupt
a running turn or exit while idle.

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
