# AC performance guard

`performance-guard.sh` samples the prompt host every 30 seconds. It records a
full snapshot when load, memory pressure, display rendering, swapping, session
count, or duplicate AC Caddy servers exceed their budgets. Three consecutive
pressure samples produce a rate-limited macOS notification.

While a threshold is active, the guard exposes
`~/.local/share/slab/performance/pressure-active` as a stable eco-governor
signal for Slab renderers and other local tools.

The fleet worker uses the same flag as an admission gate. An active render is
allowed to finish, but that host accepts no additional missions until pressure
clears. This turns the guard into a decentralized capacity signal without
giving it authority to migrate or kill arbitrary processes.

The guard never terminates arbitrary hot processes. Its one repair is narrowly
scoped: when multiple processes have both the exact `caddy run --config
Caddyfile` command and `system/` working directory, it keeps the newest and
removes the duplicates.

```bash
toolchain/macos/performance-guard.sh --install
npm run perf:status
npm run perf:audit
```

State and threshold-crossing history live in
`~/.local/share/slab/performance/`. The log rotates at 5 MB.
