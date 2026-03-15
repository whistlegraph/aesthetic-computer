# Firefox on AC Native OS — Debug Status

## Goal
Pop a Firefox browser window on the AC Native OS device for Claude Code OAuth login.

## Architecture
```
ac-native (PID 1) — owns DRM/KMS display
  └── PTY child: /bin/claude (Claude Code native binary)
        └── Bun.spawn(["xdg-open", url]) — our shim script
              └── cage (Wayland kiosk compositor) + firefox
```

## What Works
- **DRM handoff**: SIGUSR1/SIGUSR2 between xdg-open and ac-native. Confirmed in logs — release and reclaim both fire.
- **xdg-open is called**: Claude Code's `Bun.spawn(["xdg-open", url])` executes our `/bin/xdg-open` script (proven by DRM signals firing).
- **cage starts**: Screen goes black (cage takes DRM). Confirmed in earlier builds.
- **Trap EXIT**: SIGUSR2 always fires on script exit, DRM always reclaimed. No more permanent black screens.
- **120s timeout**: cage killed after 2 min if it hangs.

## What Doesn't Work
- **Firefox crashes immediately** inside cage. cage+firefox exits so fast the screen barely flashes black.
- **No log files** are created by the xdg-open script — not in /tmp, not in /mnt. Even `exec 1>/tmp/xdg-open.log` and `/bin/sh -c "echo ... > /tmp/xdg-open.log"` produce nothing.

## Known Firefox Crash History
1. **EGL init failed** (build ~#105) — `EGL_EXT_platform_base not supported`. Fixed by setting `WLR_RENDERER=pixman`.
2. **libseat failed** (build ~#103) — `No backend was able to open a seat`. Fixed by `WLR_SESSION=direct LIBSEAT_BACKEND=noop`.
3. **Wayland socket failed** (build ~#107) — `Unable to open Wayland socket: Invalid argument`. Fixed by per-PID `XDG_RUNTIME_DIR`.
4. **Firefox segfault** (build ~#108) — `ExceptionHandler::GenerateDump minidump generation`. Missing fontconfig, xkb, font. Partially fixed by bundling those.
5. **Firefox segfault** (build ~#110+) — Still crashing. Missing GLib schemas, GTK modules bundled. Still crashes.
6. **"Couldn't find application directory"** — Firefox's wrapper reads `/proc/self/exe` which fails in chroot tests. On device it should work since /proc is mounted.

## Bundled Components (current build ~198MB kernel)
- Firefox 148 (stripped): 274MB uncompressed, cached at `/tmp/ac-firefox-cache`
- cage (Wayland kiosk): 64KB
- Claude Code native binary: 225MB
- 94 shared libs (GTK3, Pango, Cairo, Harfbuzz, etc): 81MB
- fontconfig (/etc/fonts): 188KB
- xkb keyboard data: 3.9MB
- DroidSans.ttf font: 188KB
- GLib schemas: 56KB
- GTK-3.0 modules: 192KB
- zstd compressed initramfs: 186MB

## Environment Variables Set in xdg-open
```sh
HOME=/tmp
XDG_RUNTIME_DIR=/tmp/xdg-$$  # per-invocation, 0700 perms
WLR_BACKENDS=drm
WLR_SESSION=direct
WLR_RENDERER=pixman
LIBSEAT_BACKEND=noop
LD_LIBRARY_PATH=/lib64:/opt/firefox
LIBGL_ALWAYS_SOFTWARE=1
MOZ_ENABLE_WAYLAND=1
GDK_BACKEND=wayland
MOZ_APP_LAUNCHER=/opt/firefox/firefox
GRE_HOME=/opt/firefox
```

## PTY Environment (set in pty.c)
```c
TERM=xterm-256color
HOME=/tmp
LANG=en_US.UTF-8
PATH=/bin:/sbin:/usr/bin:/usr/sbin
BROWSER=/bin/xdg-open
DISPLAY=:0              // fake — tricks Claude into calling xdg-open
WAYLAND_DISPLAY=wayland-0  // fake — same
```

## Key Mystery
The xdg-open script runs (SIGUSR1 proves it) but produces ZERO log output. Multiple approaches tried:
- `echo "..." > /tmp/xdg-open.log`
- `exec 1>/tmp/xdg-open.log 2>&1`
- `/bin/sh -c "echo ... > /tmp/xdg-open.log"`
- All produce empty or nonexistent files.

**Hypothesis**: Bun.spawn may close/redirect all file descriptors, or the process exits so fast (cage fails, trap fires) that the kernel never flushes the write.

## Possible Next Steps
1. **Have ac-native run cage+firefox directly** via `system()` from C (not through xdg-open script) — bypasses Bun.spawn fd issues
2. **Run a Wayland session from the start** — cage as the compositor, ac-native as a Wayland client. Browser is just another client window.
3. **Use a simpler browser** — lynx/links2 for text-mode OAuth (but Claude's React SPA won't work)
4. **Debug Firefox crash** by running it standalone from the PTY shell: type `cd /opt/firefox && ./firefox --version` at the sh prompt to see the exact error
5. **Strace** — add strace to initramfs and trace the xdg-open/cage/firefox process

## File Locations
- xdg-open script: bundled via `fedac/native/ac-os` (HEREDOC at ~line 129)
- DRM handoff: `fedac/native/src/ac-native.c` (SIGUSR1/SIGUSR2 handlers ~line 124, main loop ~line 1644)
- PTY env setup: `fedac/native/src/pty.c` (~line 533)
- Browser open C function: `fedac/native/src/js-bindings.c` (`js_open_browser` ~line 2995)
- Cage/Firefox bundling: `fedac/native/ac-os` (`build_initramfs` function)
- Kernel config: `fedac/native/kernel/config-minimal` (ZRAM=y, ZSTD initramfs)
