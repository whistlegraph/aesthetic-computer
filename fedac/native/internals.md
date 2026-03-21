# AC Native OS Internals

A technical narrative of how AC Native OS boots, renders, and runs interactive
pieces on bare metal -- from UEFI power-on to the 60 fps main loop.

---

## Boot Sequence

### 1. UEFI Firmware

The machine's UEFI firmware loads `EFI/BOOT/BOOTX64.EFI` from a FAT32 EFI
System Partition. This file is actually a Linux kernel (6.14.2) compiled with
`CONFIG_EFI_STUB=y` ([config-minimal:400](kernel/config-minimal#L400)), which
lets the kernel act as its own EFI bootloader -- no GRUB, no systemd-boot, no
bootloader at all. The kernel image has a CPIO initramfs archive compressed
with LZ4 embedded directly inside it, containing the entire userspace.

The kernel is minimal and purpose-built. Key features enabled in the config:

- **Display**: `CONFIG_DRM=y`, `CONFIG_DRM_I915=y` for Intel GPU
  ([config-minimal:2563](kernel/config-minimal#L2563),
  [config-minimal:2617](kernel/config-minimal#L2617))
- **WiFi**: `CONFIG_IWLWIFI=y`, `CONFIG_IWLMVM=y`, `CONFIG_CFG80211=y`
  ([config-minimal:1723](kernel/config-minimal#L1723),
  [config-minimal:1726](kernel/config-minimal#L1726))
- **Audio**: `CONFIG_SOUND=y`, `CONFIG_SND=y`, `CONFIG_SND_HDA=y`
  ([config-minimal:2796](kernel/config-minimal#L2796),
  [config-minimal:2897](kernel/config-minimal#L2897))
- **Input**: `CONFIG_INPUT_EVDEV=y`
  ([config-minimal:1823](kernel/config-minimal#L1823))
- **Swap**: `CONFIG_ZRAM=y` for compressed RAM swap
  ([config-minimal:1266](kernel/config-minimal#L1266))
- **Namespaces**: `CONFIG_NAMESPACES=y`, `CONFIG_USER_NS=y`, `CONFIG_NET_NS=y`
  ([config-minimal:180](kernel/config-minimal#L180))
- **devtmpfs**: `CONFIG_DEVTMPFS_MOUNT=y` for automatic `/dev` population
  ([config-minimal:1153](kernel/config-minimal#L1153))

### 2. Init Script

After the kernel unpacks the initramfs, it runs `/init` -- a 39-line shell
script that is the first userspace code to execute
([init:1](initramfs/init#L1)).

The init script does the minimum necessary before handing off to the native
binary:

1. **Mount virtual filesystems**: `/proc`, `/sys`, `/dev` (devtmpfs),
   `/dev/pts`, `/dev/shm`, `/tmp`, `/run`
   ([init:4-11](initramfs/init#L4))

2. **Set up zram swap**: Loads the `zram` module, creates a 1 GB compressed RAM
   swap device. This effectively doubles available memory, which is critical
   since Firefox and GTK need significant RAM beyond what the tmpfs-backed
   initramfs provides ([init:18-19](initramfs/init#L18))

3. **Bring up loopback**: `ip link set lo up` -- needed later for Claude Code's
   OAuth callback server ([init:22](initramfs/init#L22))

4. **Set environment**: `PATH`, SSL certificate paths for curl/OpenSSL
   ([init:24-27](initramfs/init#L24))

5. **Create identity files**: Writes minimal `/etc/group` and `/etc/passwd` for
   seatd, which needs to look up the `root` group later during the cage
   transition ([init:30-31](initramfs/init#L30))

6. **Performance governor**: Sets all CPU cores to `performance` mode
   ([init:34-36](initramfs/init#L34))

7. **Exec ac-native**: Replaces itself with the native binary via
   `exec /ac-native /piece.mjs` ([init:39](initramfs/init#L39)). The `exec`
   means ac-native inherits PID 1.

### 3. ac-native as PID 1

The native binary starts by checking if it is PID 1 (direct boot) or running
under a compositor (cage child)
([ac-native.c:1535](src/ac-native.c#L1535)). This fork in logic drives the
entire architecture: the same binary serves two roles.

**When PID 1** (first boot, DRM mode):

1. **`mount_minimal_fs()`**: Re-mounts core filesystems. The init script
   already mounted them, but ac-native re-mounts devtmpfs to pick up any
   devices that appeared after the init script ran (notably `/dev/dri/card0`
   from i915). It also re-enables zram swap and brings up loopback
   ([ac-native.c:215-248](src/ac-native.c#L215)). Waits up to 1 second for
   `/dev/dri/card0` or `/dev/fb0` to appear
   ([ac-native.c:244-248](src/ac-native.c#L244)).

2. **Display init (DRM)**: Calls `drm_init()` which opens `/dev/dri/card0`,
   enumerates connectors, picks the best mode, and sets up dumb buffer
   page-flipping ([ac-native.c:1617](src/ac-native.c#L1617)).

3. **Framebuffer creation**: Creates a software framebuffer at 1/3 display
   resolution (e.g., 960x640 for a 2880x1920 panel). The `pixel_scale=3`
   default gives chunky pixels that are nearest-neighbor scaled to the display
   ([ac-native.c:1585-1626](src/ac-native.c#L1585)).

4. **Graphics and font init**: Initializes the immediate-mode 2D graphics
   context and bitmap font renderer
   ([ac-native.c:1640-1642](src/ac-native.c#L1640)).

5. **USB log mount**: Tries to mount the EFI boot partition at `/mnt` for
   persistent logging and config
   ([ac-native.c:1651](src/ac-native.c#L1651)).

6. **Audio init**: Opens ALSA at 192 kHz stereo, 32 voices, with reverb and
   glitch effects. Waits up to 4 seconds for the sound card to appear (HDA
   probe can lag behind i915 GPU init)
   ([audio.c:616-664](src/audio.c#L616)). Plays a boot beep immediately
   ([ac-native.c:1683](src/ac-native.c#L1683)).

7. **TTS init**: Initializes Flite text-to-speech engine, fed through the audio
   system's ring buffer
   ([ac-native.c:1684](src/ac-native.c#L1684)).

8. **Boot animation**: `draw_startup_fade()` -- a startup fade from black to
   white that hides kernel text, displays the user's handle (read from
   `/mnt/config.json`), and speaks a greeting via TTS
   ([ac-native.c:1692](src/ac-native.c#L1692)). During this animation, holding
   `W` triggers the install-to-internal-drive flow.

9. **Input init (DRM path)**: `input_init()` scans `/dev/input/` for evdev
   devices, opening anything with key, absolute axis, relative axis, or switch
   capabilities ([input.c:315-374](src/input.c#L315)). NuPhy keyboards are
   detected by vendor ID and flagged for analog hidraw handling
   ([input.c:358-366](src/input.c#L358)).

10. **WiFi init**: `wifi_init()` spawns a background thread that manages
    wpa_supplicant, scanning, connection, and DHCP
    ([wifi.c:516-535](src/wifi.c#L516)). The thread runs a 10-second
    connectivity watchdog that auto-reconnects on link loss
    ([wifi.c:484-503](src/wifi.c#L484)).

11. **JS runtime init**: Initializes QuickJS-ng and registers all AC API
    bindings (graphics, input, audio, wifi, networking, PTY, camera, 3D)
    ([ac-native.c:1751](src/ac-native.c#L1751)).

12. **Piece loading**: Reads `/mnt/config.json` for the configured boot piece
    (defaults to `/piece.mjs`), resolves aliases like `"claude"` to
    `"terminal"`, and loads the piece's JavaScript module
    ([ac-native.c:1762-1818](src/ac-native.c#L1762)).

13. **Ready melody**: After the piece loads, waits for TTS to finish, plays a
    ready melody, and prewarms the audio engine for zero-latency first keypress
    ([ac-native.c:1839-1848](src/ac-native.c#L1839)).

14. **Call `boot()`**: Invokes the piece's `boot()` lifecycle function
    ([ac-native.c:1855](src/ac-native.c#L1855)).

**When cage child** (running under Wayland compositor):

The binary detects `WAYLAND_DISPLAY` in the environment and takes a shorter
path ([ac-native.c:1543-1558](src/ac-native.c#L1543)): skips filesystem
mounting (parent already did it), connects to the Wayland compositor via
`wayland_display_init()`, initializes input via `input_init_wayland()`, and
jumps directly to the JS runtime. No boot animation, no install flow, no
audio re-init (cage child opens its own ALSA handle).

### 4. Cage Transition

After the piece's `boot()` completes in DRM mode, ac-native attempts a graceful
transition from DRM direct rendering to a Wayland compositor session. This is
the key architectural trick: fast DRM boot (sub-second to first pixel) followed
by a compositor that enables browser popups for OAuth and web content
([ac-native.c:1860-1867](src/ac-native.c#L1860)).

The transition proceeds as follows:

1. **Close audio**: The DRM parent releases ALSA so the cage child can open it
   ([ac-native.c:1870-1871](src/ac-native.c#L1870))

2. **Release DRM master**: Gives up exclusive GPU access so cage can take it
   ([ac-native.c:1874](src/ac-native.c#L1874))

3. **Fork**: The child process will become the cage session
   ([ac-native.c:1877-1878](src/ac-native.c#L1877))

4. **Child process setup**:
   - Sets `WLR_RENDERER=pixman` (software rendering -- no GPU acceleration
     needed since we're doing software framebuffer anyway)
     ([ac-native.c:1882](src/ac-native.c#L1882))
   - Sets `WLR_BACKENDS=drm` and `WLR_LIBINPUT_NO_DEVICES=1` (cage uses DRM
     for output but doesn't need libinput since ac-native reads evdev directly)
     ([ac-native.c:1883-1885](src/ac-native.c#L1883))
   - Starts **seatd** (minimal seat manager) and waits up to 3 seconds for
     `/run/seatd.sock` ([ac-native.c:1932-1955](src/ac-native.c#L1932))
   - Execs `cage -s -- /ac-native /piece.mjs` -- cage launches ac-native as
     its Wayland client ([ac-native.c:1965](src/ac-native.c#L1965))

5. **Parent process**: Waits for the cage child to exit, copies cage stderr and
   child logs to USB, cleans up seatd
   ([ac-native.c:1970-1996](src/ac-native.c#L1970)). If the cage child
   requested reboot or poweroff, the parent (still PID 1) executes it
   ([ac-native.c:2002-2009](src/ac-native.c#L2002)). If cage failed, the
   parent reclaims DRM master, re-inits audio, and continues in DRM mode as a
   fallback ([ac-native.c:2015-2019](src/ac-native.c#L2015)).

### 5. Main Loop

The main loop runs at 60 fps with `frame_sync_60fps()`
([ac-native.c:2030-2033](src/ac-native.c#L2030)):

1. **Input poll**: `input_poll()` -- in Wayland mode, dispatches the Wayland
   event queue (keyboard/pointer/touch listeners fire); in DRM mode, reads
   evdev devices directly and handles DRM handoff signals
   ([ac-native.c:2034-2145](src/ac-native.c#L2034))

2. **Hardware keys**: Processes Ctrl+=/- for pixel scale changes (dynamic
   resolution), volume keys, power button for shutdown
   ([ac-native.c:2189-2358](src/ac-native.c#L2189))

3. **JS lifecycle**: Calls `js_call_act()` (events), `js_call_sim()` (logic),
   `js_call_paint()` (rendering) every frame

4. **Display present**: Copies the software framebuffer to the display surface
   with nearest-neighbor scaling

5. **Performance logging**: Every 30 seconds, writes frame timing CSV to USB
   for crash-resilient diagnostics
   ([ac-native.c:44-49](src/ac-native.c#L44))

---

## Display Architecture

### DRM Direct Mode

The primary display path uses Linux DRM (Direct Rendering Manager) with **dumb
buffers** -- no GPU acceleration, pure software rendering
([drm-display.c:1](src/drm-display.c#L1)). The display module opens
`/dev/dri/card0`, enumerates connectors, picks the preferred mode, and creates
two dumb buffers for page-flipping. An SDL2/KMSDRM backend exists as an
optional compile flag (`USE_SDL`) but is not used in production
([drm-display.c:13-22](src/drm-display.c#L13)).

Rendering happens at 1/3 display resolution by default (pixel_scale=3) into an
`ACFramebuffer`, then nearest-neighbor scaled to the display resolution during
`ac_display_present()`. This gives the characteristic chunky pixel aesthetic
while keeping the rendering workload small.

### Wayland SHM Mode

When running under cage, the display switches to `wayland-display.c`, which
creates a `wl_surface` with XDG shell decorations and renders to shared-memory
buffers in `WL_SHM_FORMAT_ARGB8888`
([wayland-display.c:1-3](src/wayland-display.c#L1)). Double-buffered SHM pools
are allocated via `memfd_create` and `mmap`
([wayland-display.c:152-177](src/wayland-display.c#L152)). The compositor
(cage, using wlroots with `WLR_RENDERER=pixman`) composites the ac-native
surface with any browser windows.

The Wayland init sequence:
1. Connect to compositor via `wl_display_connect(NULL)`
   ([wayland-display.c:189](src/wayland-display.c#L189))
2. Bind registry globals: `wl_compositor`, `wl_shm`, `xdg_wm_base`, `wl_seat`
   ([wayland-display.c:198-200](src/wayland-display.c#L198))
3. Create surface and XDG toplevel
   ([wayland-display.c:211-218](src/wayland-display.c#L211))
4. Allocate double-buffered SHM pool
   ([wayland-display.c:162-169](src/wayland-display.c#L162))

### DRM-to-Cage Handoff

The transition from DRM to Wayland is designed to be invisible to the user.
The DRM parent renders the boot animation and first piece frame, then
`drm_release_master()` unlocks the GPU. Cage's wlroots backend immediately
picks up the DRM device and ac-native (now a Wayland client) resumes rendering
to SHM buffers. If cage fails, the parent calls `drm_acquire_master()` and
falls back to DRM mode seamlessly
([ac-native.c:2015-2018](src/ac-native.c#L2015)).

---

## Input Architecture

### Evdev Direct (DRM mode and fallback)

`input_init()` scans `/dev/input/` for event devices and opens them with
`O_RDONLY | O_NONBLOCK` ([input.c:315-374](src/input.c#L315)). It checks
capability bits (`EV_KEY`, `EV_ABS`, `EV_REL`, `EV_SW`) to filter relevant
devices. Special handling:

- **Tablet mode switch** (`SW_TABLET_MODE`): Reads initial state from the
  ThinkPad ACPI hotkey interface, with evdev switch events as live updates
  ([input.c:345-356](src/input.c#L345))
- **NuPhy analog keyboards**: Detected by USB vendor ID, evdev key events are
  suppressed in favor of hidraw analog pressure data
  ([input.c:358-366](src/input.c#L358))

`input_poll()` reads all pending events from all device file descriptors each
frame ([input.c:396-460](src/input.c#L396)). In Wayland mode with evdev
fallback, it polls both sources.

### Wayland Seat (under cage)

`input_init_wayland()` binds to the compositor's `wl_seat` to receive keyboard,
pointer, and touch events through Wayland protocol listeners
([input.c:1032-1061](src/input.c#L1032)). Since cage runs without
udev/libinput (`WLR_LIBINPUT_NO_DEVICES=1`), the Wayland seat often has no
capabilities. In that case, it falls back to direct evdev polling while still
using the Wayland event dispatch loop
([input.c:1064-1091](src/input.c#L1064)).

Software key repeat is implemented in `input_poll()` since Wayland doesn't
send `value=2` repeat events ([input.c:434-449](src/input.c#L434)).

---

## Audio

The audio engine uses ALSA directly at 192 kHz stereo with a 192-sample period
(~1 ms latency) ([audio.h:7-9](src/audio.h#L7)). It supports:

- **32-voice polyphonic synthesizer**: Sine, triangle, sawtooth, square, and
  filtered noise waveforms with per-voice frequency, volume, pan, attack, and
  decay envelopes ([audio.h:29-49](src/audio.h#L29))
- **Sample playback**: 12 simultaneous sample voices with pitch shifting and
  looping, plus microphone recording with hot-mic mode
  ([audio.h:51-61](src/audio.h#L51))
- **Effects**: Room reverb (simple delay-line) and bit-crush glitch, with
  smoothed wet/dry mix ([audio.h:88-102](src/audio.h#L88))
- **TTS integration**: Flite speech synthesis fed through a ring buffer into
  the audio thread ([audio.h:109-115](src/audio.h#L109))
- **HDMI output**: Optional secondary audio output with downsampling and
  low-pass filtering ([audio.h:141-148](src/audio.h#L141))

Audio init waits up to 4 seconds for the sound card to appear, since i915 GPU
initialization can delay HDA codec probe
([audio.c:658-664](src/audio.c#L658)). The engine writes diagnostics to
`/mnt/ac-audio.log` on the USB partition
([audio.c:667-668](src/audio.c#L667)).

---

## Networking

### WiFi

The WiFi subsystem wraps `iw`, `wpa_supplicant`, and `udhcpc`/`dhcpcd` behind
a threaded state machine ([wifi.c:1](src/wifi.c#L1)). `wifi_init()` checks for
the `iw` binary, logs iwlwifi firmware availability, and spawns a background
thread ([wifi.c:516-535](src/wifi.c#L516)).

The thread runs a command loop with a 2-second timeout for watchdog polling
([wifi.c:448-510](src/wifi.c#L448)):

- **Scan**: Calls `iw dev wlanX scan` to enumerate available networks
- **Connect**: Configures and launches `wpa_supplicant`, then runs DHCP
- **Disconnect**: Kills `wpa_supplicant` and releases the IP
- **Watchdog**: Every ~10 seconds, checks `ip -4 addr show` for a live IP
  address. On loss, auto-reconnects with exponential backoff
  ([wifi.c:484-503](src/wifi.c#L484))

### WebSocket and UDP

The runtime includes a WebSocket client (`ws-client.c`) for session server
connections and a UDP client (`udp-client.c`) for low-latency multiplayer data
([js-bindings.h:13-14](src/js-bindings.h#L13)). These are exposed to pieces
through the JS API as `net.socket()` and `net.udp()`.

---

## Process Model

The system has a distinctive process tree that changes shape during boot:

### Phase 1: DRM Boot

```
PID 1: ac-native (DRM direct rendering)
```

Single process. Owns the DRM master, evdev devices, ALSA, and WiFi thread.

### Phase 2: Cage Transition

```
PID 1: ac-native (waiting, DRM released)
  └── cage (Wayland compositor, seatd session)
        └── ac-native (Wayland client, piece runner)
```

PID 1 forks a child that execs cage. Cage then launches ac-native as its
Wayland client. The parent (PID 1) blocks on `waitpid()` until the cage session
ends ([ac-native.c:1974](src/ac-native.c#L1974)).

### Browser Popup (DRM fallback path)

When running in DRM mode without cage, browser popups for OAuth use a different
pattern ([ac-native.c:2060-2133](src/ac-native.c#L2060)):

```
PID 1: ac-native (DRM released, waiting)
  └── child: seatd + cage + firefox (browser session)
```

The child forks, starts seatd, launches `cage -s -- firefox --kiosk <url>`,
and exits when the browser closes. The parent reclaims DRM master and resumes
rendering.

### Claude Code (PTY)

Claude Code runs as a child process spawned via `forkpty()`
([pty.c:523](src/pty.c#L523)). The child sets up a terminal environment
(`TERM=xterm-256color`), loads OAuth credentials from
`/mnt/claude-credentials.json`, fakes `DISPLAY=:0` so Claude will call
`xdg-open` for browser-based authentication, and execs the `claude` binary
([pty.c:531-566](src/pty.c#L531)). The parent reads PTY output via a
non-blocking master fd and renders it through a VT100 terminal emulator
implemented in `pty.c`.

---

## Build System

### ac-os Script

The `ac-os` script is the single entry point for all build operations
([ac-os:1-8](ac-os#L1)):

- `ac-os build` -- compile binary, pack initramfs, build kernel
- `ac-os flash` -- build + write to USB
- `ac-os upload` -- **always rebuilds first**, then uploads OTA release.
  The kernel embeds `AC_GIT_HASH` and `AC_BUILD_NAME` at compile time, so
  uploading without rebuilding would serve a stale version string.
- `ac-os flash+upload` -- all of the above

### Makefile

The Makefile compiles 17 C source files plus QuickJS-ng (the JavaScript engine)
from source ([Makefile:60-76](Makefile#L60),
[Makefile:93-94](Makefile#L93)):

```
ac-native.c, drm-display.c, framebuffer.c, graph.c, graph3d.c,
font.c, color.c, input.c, audio.c, wifi.c, tts.c, ws-client.c,
udp-client.c, camera.c, pty.c, machines.c, js-bindings.c
```

Plus conditionally: `wayland-display.c` (when `USE_WAYLAND=1`).

Key build details:

- **Compiler flags**: `-O2 -Wall -Wextra -std=gnu11` with version metadata
  baked in via `-DAC_GIT_HASH` and `-DAC_BUILD_NAME`
  ([Makefile:15](Makefile#L15))
- **Linking**: DRM, ALSA, OpenSSL, Flite TTS, optionally Wayland client and
  SDL2 ([Makefile:30-54](Makefile#L30))
- **Static linking**: Supported with musl-gcc; otherwise dynamic linking with
  `.so` files bundled in the initramfs
  ([Makefile:18-27](Makefile#L18))
- **Wayland protocol**: `xdg-shell-client-protocol.c/h` generated from the
  system's `xdg-shell.xml` via `wayland-scanner`
  ([Makefile:48-54](Makefile#L48))

### Kernel

Linux 6.14.2 compiled with GCC 15.2.1
([config-minimal:5](kernel/config-minimal#L5)). The kernel config is
purpose-built for Intel laptop hardware with EFI stub boot. The initramfs
(containing ac-native, all shared libraries, Firefox, Claude Code, firmware
blobs, and bundled pieces) is LZ4-compressed and embedded directly into the
kernel image, producing a single `vmlinuz` file that is copied to the EFI
partition as `BOOTX64.EFI`.

### JS Runtime

Pieces run on QuickJS-ng, compiled from source as part of the build
([Makefile:93-94](Makefile#L93)). The `ACRuntime` struct holds cached
references to piece lifecycle functions (`boot_fn`, `paint_fn`, `act_fn`,
`sim_fn`, `leave_fn`, `beat_fn`) and all subsystem pointers
([js-bindings.h:20-98](src/js-bindings.h#L20)). `js-bindings.c` registers
the full AC API surface: graphics primitives, audio synthesis, WiFi control,
WebSocket/UDP networking, camera, PTY terminal, OTA updates, and 3D rendering.

---

## Lifecycle Summary

```
UEFI firmware
  │
  ▼
BOOTX64.EFI (Linux 6.14.2 + EFI stub)
  │
  ▼
Kernel unpacks LZ4 initramfs
  │
  ▼
/init (shell script, 39 lines)
  ├── mount proc/sys/dev/pts/shm/tmp/run
  ├── zram swap (1 GB compressed)
  ├── loopback up
  ├── CPU performance governor
  └── exec /ac-native /piece.mjs
        │
        ▼
  ac-native (PID 1, DRM mode)
  ├── mount_minimal_fs()
  ├── drm_init() ──────────────── first pixels on screen
  ├── audio_init() ────────────── boot beep
  ├── tts_init() + startup fade
  ├── input_init()
  ├── wifi_init() ─────────────── background thread
  ├── js_init() + js_load_piece()
  ├── ready melody
  ├── js_call_boot()
  │
  ├── cage transition (fork)
  │     ├── child: seatd → cage → ac-native (Wayland client)
  │     │     ├── wayland_display_init()
  │     │     ├── input_init_wayland()
  │     │     └── main loop (60 fps)
  │     │           ├── input_poll()
  │     │           ├── js_call_act()
  │     │           ├── js_call_sim()
  │     │           ├── js_call_paint()
  │     │           └── display present (SHM buffer)
  │     │
  │     └── parent: waitpid(cage) → reboot/poweroff/DRM fallback
  │
  └── DRM fallback main loop (if cage unavailable)
        ├── input_poll() (evdev)
        ├── DRM handoff for browser popups
        ├── js_call_act/sim/paint()
        └── display present (dumb buffer flip)
```
