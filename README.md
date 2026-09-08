# Aesthetic Code

A local control plane for coding agents.

```sh
./install.sh
ac
```

`ac` and `aesthetic` start the preferred agent in the current directory.

```sh
ac ~/project
ac local
ac claude
ac codex ~/project -- resume --last
aesthetic model qwen2.5-coder:7b
aesthetic use local
aesthetic doctor
```

Aesthetic Code has no account, telemetry, hosted control plane, or cloud sync.
Configuration, session state, memory, and fleet coordination stay on the
developer's machines.

`ac local` uses a locally installed Ollama model through the Claude Code agent
loopback API, disables Claude telemetry and feedback traffic, disables its web
tools, and rejects Ollama cloud model identifiers. `ac claude` and `ac codex`
are explicit provider modes; their inference leaves the machine under the
chosen provider's terms. Aesthetic Code does not resell model usage.

When `codex-slab` is installed, provider-mode Codex sessions use it
automatically so they remain visible to prompt rocks and the fleet ledger.

This repository is proprietary. See `LICENSE`.

On Fish installations with existing `ac` or `aesthetic` functions, the
installer preserves them as `ac-repo` and `aesthetic-platform` before assigning
the short names to Aesthetic Code.

The enforceable product boundary is recorded in
[`docs/local-contract.md`](docs/local-contract.md).
