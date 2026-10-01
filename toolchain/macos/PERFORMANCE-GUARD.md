# AC performance guard

The guard samples each Mac every 30 seconds and publishes
`~/.local/share/slab/performance/pressure-active`. It watches CPU load, memory
availability, swap activity, display CPU, session/build counts, and **20 GiB of
free disk headroom**. Fleet workers defer new missions while pressure is active.

The installed PATH shims check live disk, memory, and load before `git worktree
add` or `swift build`, including builds outside the AC checkout. The check covers
both the home volume and the destination volume. Recent sampler signals such as
swap activity also defer launches. Refusal exits **75** before starting the job;
callers must check that result rather than continuing a recovery sequence.

Swift builds retain per-package serialization and the existing low-priority,
two-job default on hosts with at most 16 GiB. Admission is checked again after
waiting for the package lock. Other Git and Swift commands pass through.

```bash
toolchain/macos/performance-guard.sh --install
ac-performance-guard --status
ac-performance-guard --admit 'my expensive job' /path/to/output
python3 toolchain/macos/deploy-performance-guard.py --local neo panda chicken frisbee poorslice
```

The installer owns `~/.local/lib/ac-performance-guard/` and links `git`, `swift`,
and `ac-performance-guard` into `~/.local/bin`. That directory must precede the
real commands on PATH; fleet deployment verifies the user's login-shell routing.
It refuses to overwrite unrelated command wrappers. The deployment tool preserves
the fleet’s existing portable-Git launcher as `git.before-ac-guard` and invokes
it underneath the new gate; uninstall restores it. Other wrappers require
explicit inspection and `--install --preserve-git-wrapper`. The guard requires Python
3.9 or later and macOS command-line tools; it does not build or install packages.

Deployment packages the exact current **committed revision**, verifies installed
file hashes, launchd registration, a new sample, and command routing. It preserves
remote repository edits. Failed hosts are retried every five minutes for up to
24 hours using the same pinned bundle. The private receipt and pending list are
in `~/.local/share/slab/performance-rollout/receipt.json`. Re-run the deployment
command to begin a new rollout after that window.

`latest.json` and the compatible `latest.txt` contain current readings. Pressure
history is stored in `performance-guard.log` and `incidents.jsonl`, each bounded
to about 5 MiB. Incident records include top processes, parent chains, and up to
eight Git processes with start time, sanitized command, and working directory.
They stay local with owner-only permissions. Shell/agent command arguments and
process environments are not collected; Git config values, messages, and URLs
are redacted. Sampling locks release automatically when a process dies. Atomic
writes preserve the last complete sample if the filesystem fills.

Three consecutive pressure samples trigger a rate-limited notification. Critical
memory or less than 5 GiB disk headroom can notify immediately. The guard never
kills arbitrary workloads. Its optional `--repair` action sends SIGTERM only to
validated duplicate `caddy run --config Caddyfile` processes in this repo's
`system/` directory.

This is admission control, not a memory or disk quota. It cannot stop a job that
was already running, predict the size of an entire checkout, intercept absolute
Git/Swift paths, or govern unrelated build tools such as Xcode and npm. Use sparse
worktrees from the start and keep large builds on compute hosts. An explicit
`AC_PERFORMANCE_ALLOW_PRESSURE=1` bypass is available for deliberate recovery;
normal automation must not set it. Failed live measurements defer work. Stale
sampler files do not keep blocking after live headroom recovers.

Validation (no compiler or real workload runs):

```bash
python3 -B toolchain/macos/test_performance_guard.py
```
