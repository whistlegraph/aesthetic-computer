# AC Native OS — Browser + Wayland Compositor Debug Report

**Date:** 2026-03-15
**Author:** Claude Opus 4.6 + @jeffrey
**Status:** In progress — DRM direct mode works, cage Wayland hangs
**Goal:** Package Firefox with AC Native OS so Claude Code can pop a browser for OAuth login

---

## Problem Statement

Claude Code on AC Native OS needs to authenticate via OAuth. Claude's OAuth flow requires a browser — it calls `xdg-open` to open `https://claude.ai/oauth/authorize?...`. The device has no browser, so auth fails.

The device-code proxy approach was rejected by Claude's OAuth (our redirect URI isn't registered). API keys require separate billing. The solution is to bundle a real browser.

## Architecture Approaches Tried

### Approach 1: DRM Handoff (SIGUSR1/2)
**Builds:** elastic-corona through mossy-puffin (~builds #82-134)

ac-native holds DRM master. When Claude calls xdg-open:
1. xdg-open script sends SIGUSR1 to PID 1 (ac-native)
2. ac-native releases DRM master
3. xdg-open launches `cage -- firefox <url>`
4. When cage exits, xdg-open sends SIGUSR2
5. ac-native reclaims DRM master

**Results:**
- DRM release/reclaim signals worked (`ac-native.c` SIGUSR handler ~line 1415)
- cage started but Firefox hung with black screen
- Firefox segfaulted due to missing runtime deps (fontconfig, xkb, GLib schemas)
- After fixing deps, Firefox still hung — pixman software rendering too slow for React SPA
- xdg-open logging impossible — Bun.spawn closes child file descriptors
- Eventually moved cage+firefox launch to C code (`ac-native.c` ~line 1470) for better control

**Key files:**
- `src/ac-native.c:1415` — SIGUSR1/2 handler for DRM release/reclaim
- `src/ac-native.c:1470` — C-level browser launch (system() call with cage+firefox)
- `src/js-bindings.c:2830` — `system.openBrowser()` JS binding
- `initramfs/init` — xdg-open shim script
- `ac-os` — Firefox/cage bundling in build_initramfs()

**Issues found:**
1. `sleep 0.3` — busybox doesn't have `sleep` (fixed)
2. `timeout` command missing (fixed by removing timeout)
3. Wayland socket: `Invalid argument` — XDG_RUNTIME_DIR permissions wrong (fixed with per-PID dir)
4. seatd group lookup: `Could not find group by name 'root'` — /etc/group missing (fixed)
5. seatd socket: `/run/seatd.sock` not found — /run not mounted as tmpfs (fixed)
6. Firefox: `Couldn't find the application directory` — /proc/self/exe resolves to symlink (fixed with cd + ./firefox)
7. Firefox: `ExceptionHandler::GenerateDump` segfault — missing fontconfig, xkb, dbus
8. Firefox: silent hang — likely dbus_bus_get() blocking (added MOZ_DBUS_GLIB=0, DBUS_SESSION_BUS_ADDRESS=/dev/null)
9. cage: EGL_EXT_platform_base not supported — no GPU libs, switched to WLR_RENDERER=pixman
10. cage: `Unable to open Wayland socket` — XDG_RUNTIME_DIR wrong permissions

### Approach 2: Wayland Compositor (cage as PID 1 display)
**Builds:** lattice-tundra through quartz-cricket (#136-147)

Run cage from init as the primary Wayland compositor. ac-native becomes a wl_shm Wayland client. Firefox can be spawned as a sibling Wayland client — no DRM handoff needed.

**Architecture:**
```
init (shell script) → mounts + seatd + cage
  cage (Wayland compositor) — owns DRM, GPU compositing
    ├── ac-native — Wayland client (wl_shm surface)
    └── firefox — Wayland client (fork+exec on demand)
```

**Implementation:**
- `src/wayland-display.c` — wl_shm double-buffered display backend (236 lines)
- `src/wayland-display.h` — ACWaylandDisplay struct definition
- `src/input.c` — Wayland input path (wl_keyboard/pointer/touch + NuPhy hidraw)
- `src/ac-native.c:1441` — WAYLAND_DISPLAY env detection, wayland_display_init()
- `Makefile:48-53` — USE_WAYLAND=1 flag, xdg-shell protocol generation
- `Makefile:68-79` — Conditional wayland-display.c compilation + protocol objects
- `Makefile:116-129` — wayland-scanner code generation rules

**Results:**
- Compilation works with USE_WAYLAND=1
- On hardware: screen stays black
- ac-native.log shows normal DRM-mode boot (no WAYLAND_DISPLAY env var)
- init.log never persists to USB — init runs before USB is enumerated

**Debugging attempts:**
1. Added ac_log() to wayland-display.c init path — no wayland lines in log
2. Added WAYLAND_DISPLAY + XDG_RUNTIME_DIR logging to ac-native.c:1441 — never reached
3. Tried writing init.log to /tmp then copying to /mnt — /tmp exists but copy fails
4. Tried writing to /dev/kmsg — messages written but dmesg not in initramfs
5. Read /dev/kmsg from C in ac-native.c:290 — file empty (format mismatch?)
6. Tried backgrounding cage with timeout fallback — cage blocks, no fallback
7. Disabled cage entirely (if false) — STILL BLACK

**Current blocker (build #147):** Even with cage completely disabled and a minimal init script that just does `exec /ac-native /piece.mjs`, the screen is black. This suggests the USE_WAYLAND=1 compile flag changed something in ac-native.c that breaks DRM mode, or the new wayland-display.c/input.c code introduced a regression.

## Bundled Components

| Component | Size (uncompressed) | Purpose |
|-----------|-------------------|---------|
| Firefox 148.0.2 (stripped) | 274MB | OAuth browser |
| cage 0.2.0 | 64KB | Wayland kiosk compositor |
| seatd 0.9.3 | 48KB | Seat daemon for wlroots |
| GTK3/Wayland/Pango/Cairo libs | 81MB (94 files) | Firefox runtime deps |
| fontconfig + xkb + font | 4MB | Text rendering |
| GLib schemas + GTK modules | ~300KB | GTK runtime data |
| Claude Code 2.1.76 (native) | 225MB | Claude CLI |
| **Total kernel image** | **198MB** (zstd compressed) | |

## Key Findings

1. **Firefox works in chroot test** — `cd /opt/firefox && ./firefox --version` returns `148.0.2` with only a sandbox warning
2. **cage + seatd work** — seatd creates socket, cage starts and takes DRM (screen goes black = cage has display)
3. **cage + Firefox hangs** — cage starts, Firefox launches inside it, but Firefox produces no visual output and hangs indefinitely. No crash log, no stderr output.
4. **DRM handoff works** — SIGUSR1/2 mechanism successfully releases and reclaims DRM master
5. **Bun.spawn swallows xdg-open output** — Claude Code's `Bun.spawn(["xdg-open", url]).exited.catch(() => {})` closes child fds
6. **init.log is impossible to persist** — USB not enumerated when init runs, /dev/kmsg writes succeed but can't be read back without dmesg binary

## Next Steps

1. **Fix DRM regression** — Build without USE_WAYLAND=1 to confirm DRM still works, then bisect what broke
2. **Add serial console to kernel** — Enable CONFIG_SERIAL_8250 for QEMU debugging with `-serial stdio`
3. **Debug cage in QEMU** — Use VNC + serial to see cage's actual output in a controlled environment
4. **Consider GPU compositing** — Mesa EGL + iris DRI driver (~50MB) would let cage use hardware rendering instead of pixman
5. **Alternative: Electron kiosk** — If cage+Firefox proves unworkable, a minimal Electron app (already in ac-electron/) could handle OAuth with less complexity

## File Reference

- `fedac/native/src/ac-native.c` — Main runtime, DRM init (~line 1462), Wayland detection (~line 1441), SIGUSR handler (~line 1415), kmsg dump (~line 290)
- `fedac/native/src/wayland-display.c` — wl_shm display backend (161: init, 238: present)
- `fedac/native/src/wayland-display.h` — ACWaylandDisplay struct
- `fedac/native/src/input.c` — Input with Wayland path
- `fedac/native/src/input.h` — Input header with USE_WAYLAND conditionals
- `fedac/native/src/js-bindings.c:2830` — system.openBrowser() binding
- `fedac/native/initramfs/init` — Boot script (currently minimal DRM-only)
- `fedac/native/Makefile:48-129` — USE_WAYLAND build rules
- `fedac/native/ac-os:86-207` — Firefox/cage/seatd bundling in build_initramfs()
- `fedac/native/pieces/claude.mjs` — Code piece (auth curtain → terminal:claude)
- `fedac/native/pieces/terminal.mjs` — PTY terminal (spawns /bin/claude)
