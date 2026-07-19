# Execution Environments

This file is the global environment score for Aesthetic Computer. It applies to
the root score and every nested `SCORE.md` unless a nested score explicitly
overrides it.

An environment is the outer session in which an agent is running, not the
repository, machine, or task. Examples are Claude Terminal on macOS, an Emacs
session with Emacs MCP, VS Code, a container, and ac-native.

## Resolution

Resolve the active environment in this order:

1. An explicit environment named by the user or launcher.
2. `AC_AGENT_ENV`, when set.
3. Capabilities actually exposed to the session (available MCP tools, editor
   bridge, terminal, operating system, and container/device markers).
4. `terminal` when an ordinary local shell is the only known interface.

Never select an environment merely because it is Jeffrey's usual setup. Never
claim that a preferred bridge is missing or that a normal local shell is a
fallback when the active environment is a terminal.

Suggested `AC_AGENT_ENV` values are `terminal-macos`, `emacs`, `vscode`,
`container`, and `ac-native`. Launchers may use a more specific value.

## Conditional score prose

Environment-specific instructions are marked with a heading or lead-in of this
form:

```text
[environment: emacs]
[environment: terminal-macos, vscode]
[environment: *]
```

Apply `*` and the active environment; ignore other environment blocks. Untagged
prose is universal. An environment tag scopes its section until the next heading
of the same or higher level, unless the text says otherwise.

## Profiles

### `[environment: terminal-macos]`

Use the shell and tools exposed directly by the agent host. On Jeffrey's Mac,
fish is preferred for interactive commands and project helpers, but a
non-interactive command runner may use its configured shell. Do not route
through Emacs unless Emacs MCP is actually present and selected.

### `[environment: emacs]`

Use Emacs MCP and the named `🐟-fishy` terminal buffer for command execution.
The other named buffers in the root score expose service logs and monitors.

### `[environment: vscode]`

Use VS Code's terminal and editor integrations that are actually exposed. The
Emacs buffer workflow does not apply.

### `[environment: container]`

Use the container's local tools. For Jeffrey's macOS host, use
`ssh jas@host.docker.internal` only when host access is needed.

### `[environment: ac-native]`

Use the on-device shell and hardware probing surfaces described in the relevant
native score. Do not assume desktop editor bridges exist.
