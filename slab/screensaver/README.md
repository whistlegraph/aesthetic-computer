# Slab Status screensaver

A macOS `.saver` that shows, fullscreen, the abstract status of our prompts
plus this machine's system resources. Same visual language as the slab
menubar icon, scaled up.

## What it shows

- **Rotating polygon** — one edge per live Claude session, colored by that
  session's state (green working / amber-pulse awaiting / slate complete /
  gray blank / dim-blink stale). 1 session = a bar, 2 = parallel bars,
  N≥3 = the matching N-gon. Idle = one slow-breathing ring. Rotation
  speeds up the more sessions are awaiting you.
- **Status line + breakdown** — `4 awaiting · 5 active`, with non-zero
  per-state chips and `ambient` / `muted` mode tags.
- **System resources** — CPU % with per-core ticks, memory used/total,
  load average, uptime, clock.

It reads the same on-disk state the menubar reads
(`~/.local/share/slab/state/{active,awaiting}-prompts`, mode flags) and is
strictly **read-only** and **subprocess-free** — both required to behave
inside Apple's sandboxed `legacyScreenSaver` host.

## Build & install

```bash
./build.sh            # compile + install to ~/Library/Screen Savers
./build.sh --build    # compile only (bundle lands in .build/)
```

No Xcode project — `swiftc` builds the loadable bundle directly
(`-emit-library -Xlinker -bundle`), so Command Line Tools is enough.
Ad-hoc signed, which is sufficient for a locally installed saver.

After install, pick it in **System Settings → Screen Saver → Other →
"Slab Status"** (third-party savers live under the non-Apple group).

## Notes / gotchas

- Inside the screensaver sandbox, `NSHomeDirectory()` / `$HOME` point at
  the saver's container, not `/Users/<you>`. `RealHome` resolves the true
  home via `getpwuid(getuid())` so the slab state path is correct.
- No `Process`/`NSTask`: lid/sleep/tailscale (which the menubar gets by
  shelling out) are intentionally omitted. CPU/mem/load come from Mach /
  `sysctl` / `getloadavg`, which are syscalls and work in the sandbox.
- `legacyScreenSaver` caches loaded bundles; `build.sh` kills it so the
  next engage / settings-pane open picks up a fresh build.
