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
name, focus, wake, close, and track it as `aesthetic-code`.

Inside the TUI: `/new`, `/clear`, `/help`, `/quit`. Press `ctrl-c` to interrupt
a running turn or exit while idle.

```sh
aesthetic doctor
npm test
```

This repository is proprietary. See `LICENSE`.

On Fish installations with existing `ac` or `aesthetic` functions, the
installer preserves them as `ac-repo` and `aesthetic-platform`.

The product boundary is recorded in
[`docs/local-contract.md`](docs/local-contract.md).
