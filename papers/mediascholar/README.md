# Mediascholar

Mediascholar synthesizes its own new-media research topics and produces
review-only Botted Paper candidates through the complete `/papers` mill.

The execution split is deliberate:

```
mediascholar.timer
  → mediascholar.service (admission + provider harness + isolated checkout)
    → mediascholar-proxy.service (loopback credential broker)
      → Claude Code or Codex
    → Paper MCP (build + Figure-Table-QA-Check)
  → prox-worker.service (tailnet ledger + allowlisted job control)
```

`jasellite:mediascholar` appears in Prox while a run is active and remains
`awaiting` when a candidate is ready. `prox_job` can start, inspect, or cancel
the fixed `mediascholar.service`; the Linux endpoint accepts no shell text.

## Resource boundary

The runner refuses admission above `0.55` load per CPU, below 4 GiB available
memory, or below 32 GiB free disk. The service independently enforces:

- one systemd job at a time;
- `CPUQuota=50%` with the lowest CPU and I/O weights;
- 1.5 GiB soft and 2 GiB hard memory bounds;
- 128 tasks and a six-hour wall-clock limit;
- 160 brokered provider requests per UTC day, plus Claude's per-stage dollar
  budgets;
- no access to the vault, SSH keys, cloud credentials, GPG state, or provider
  credential files;
- no publication, deployment, push, or commit capability.

A daily timer retries after resource skips. A successful paper starts a
seven-day cadence hold. Four retained candidate or forensic checkouts stop new
runs until a human reviews and removes one; Mediascholar never deletes them.

## Jasellite

The installer is report-only without an action:

```sh
bash papers/mediascholar/install-jasellite.sh --check
bash papers/mediascholar/install-jasellite.sh --stage
bash papers/mediascholar/install-jasellite.sh --install
bash papers/mediascholar/install-jasellite.sh --queue
bash papers/mediascholar/install-jasellite.sh --dependencies
bash papers/mediascholar/install-jasellite.sh --enable
```

Installation stages a small runtime at `~/.local/lib/mediascholar`. The live
`~/aesthetic-computer` checkout stays clean and read-only to the agent.
`--queue` leaves the paper timer disabled and retries the dependency bootstrap
every six hours. The bootstrap must first pass admission, then runs at 25% of
one CPU, idle I/O priority, and a 1 GiB memory ceiling. It enables the paper
timer and disables itself only after the tools install successfully.

The enabled paper process also requires Bubblewrap. Its home namespace exposes
only the public repository and installed runtime as read-only mounts, plus the
Mediascholar state and Prox-advertise directories as writable mounts. An
enabled run refuses to fall back to the host filesystem.

Claude uses the existing `~/.config/claude/oauth-token` through the loopback
broker. Put an Anthropic API key in
`~/.config/mediascholar/credentials/anthropic` to prefer it. Put an OpenAI key
in `~/.config/mediascholar/credentials/openai`, install Codex with
`--install-codex`, and choose `MEDIASCHOLAR_PROVIDER=openai` to use Codex.
Credentials must be mode `0600`; their values never enter the agent's shell
environment or logs.

Candidates live under `~/.local/share/mediascholar/runs/`. Each run keeps
`run.json`, `topic.json`, provider logs, the PDF, and `candidate.patch`; its
isolated, remote-less checkout remains available for review. Publishing stays
manual.
