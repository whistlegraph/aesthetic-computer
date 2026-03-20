# AC Native OS — Initialization Analysis

Internal technical analysis of the full boot path from EFI firmware to running JS piece.

Source files analyzed:
- `fedac/native/initramfs/init` (81 lines)
- `fedac/native/src/ac-native.c` (~2300+ lines, focus on `main()` and boot functions)
- `fedac/native/src/audio.c` (1497 lines)
- `fedac/native/scripts/build-and-flash.sh` (956 lines)
- `fedac/native/ac-os` (build orchestrator)
- `fedac/native/Makefile`

---

## 1. EFI Boot

The system boots via UEFI from a GPT-partitioned drive with a single EFI System Partition (ESP), FAT32-formatted, labeled `AC-NATIVE`.

**Partition layout** (build-and-flash.sh:876-879):
```
label: gpt
type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=512M
```

**Boot chain**: The kernel (`vmlinuz`) is a standard Linux bzImage with the initramfs CPIO archive (LZ4-compressed) embedded directly via `CONFIG_INITRAMFS_SOURCE`. This means the kernel and initramfs are a single file.

Two boot layouts are supported:
1. **Direct EFI stub** — `EFI/BOOT/BOOTX64.EFI` is the kernel itself (default for USB flash).
2. **Splash chainloader** — If `fedac/native/bootloader/splash.efi` exists, it is placed at `BOOTX64.EFI` and the kernel goes to `EFI/BOOT/KERNEL.EFI`. The splash EFI displays a logo while the kernel loads (build-and-flash.sh:900-908).
3. **systemd-boot layout** — Detected during install-to-disk: kernel at `EFI/Linux/vmlinuz-ac-native` with separate loader configs.

The kernel version defaults to 6.14.2 (build-and-flash.sh:36). It is compiled with Intel WiFi firmware (`iwlwifi-*`) built in via `CONFIG_EXTRA_FIRMWARE`, i915 GPU drivers, HDA audio, and all necessary subsystems for the target ThinkPad-class hardware.

---

## 2. Init Script (`/init` — PID 1)

The init script is a minimal POSIX shell script (`#!/bin/sh`) that runs as PID 1 inside the initramfs. It never uses systemd, udev, or any service manager.

### Filesystem Mounts (lines 4-13)

Mounts are performed in order with error suppression:

| Mount Point | Type | Notes |
|---|---|---|
| `/proc` | proc | |
| `/sys` | sysfs | |
| `/dev` | devtmpfs | Kernel-populated device nodes |
| `/dev/pts` | devpts | PTY support (ptmxmode=0666) |
| `/dev/shm` | tmpfs | Shared memory |
| `/tmp` | tmpfs | Temporary storage |
| `/run` | tmpfs | Runtime data |
| `/sys/kernel/tracing` | tracefs | Performance tracing |
| `/sys/kernel/debug` | debugfs | Debug interfaces |

### Credential Restoration (lines 15-34)

The build system bakes Claude Code credentials into the initramfs at `/claude-creds.json` and `/claude-state.json`. Because `/tmp` is mounted as tmpfs (hiding the initramfs's own `/tmp`), the init script copies these from the initramfs root:

- `/claude-creds.json` -> `/tmp/.claude/.credentials.json`
- `/claude-state.json` -> `/tmp/.claude.json`
- Seeds `/tmp/.claude/settings.json` with default permissions (all tools allowed, `installMethod: "native"`, auto-updates off)
- Copies `/device-claude.md` -> `/tmp/ac/CLAUDE.md` (working dir for Claude Code sessions)

### Logging (line 37)

An `ilog()` function writes to `/tmp/ac-init.log` (tmpfs). This log is later copied to USB by `ac-native`'s `try_mount_log()` for post-mortem debugging.

### zram Swap (line 42)

Creates 1GB compressed RAM swap via zram:
```
modprobe zram && echo 1G > /sys/block/zram0/disksize && mkswap /dev/zram0 && swapon /dev/zram0
```

This effectively doubles usable memory for Firefox/GTK browser sessions.

### Networking (line 45)

Brings up loopback interface (`ip link set lo up`) for localhost connections (needed for Claude OAuth callback server).

### Environment (lines 47-51)

Sets `PATH`, `SSL_CERT_FILE`, `CURL_CA_BUNDLE`, `SSL_CERT_DIR` for curl/OpenSSL trust in the initramfs environment.

### System Setup (lines 53-59)

- Creates minimal `/etc/group` and `/etc/passwd` (root only) for seatd compatibility.
- Sets all CPU cores to `performance` governor.

### Kernel Log Dump (lines 62-77)

Before launching ac-native, the init script attempts to dump kernel boot logs to USB for debugging:
- Scans `/dev/sda1`, `/dev/sdb1`, `/dev/sdc1` for block devices
- Mounts the first available one at `/mnt`
- Reads `/dev/kmsg` (non-blocking, 1-second read window) and writes to `/mnt/dmesg.log`
- Copies `/tmp/ac-init.log` to `/mnt/init.log`

### Exec (line 80)

```sh
exec /ac-native /piece.mjs
```

The init script replaces itself with the ac-native binary (PID 1 preserved). The default piece is `/piece.mjs` (baked into initramfs at build time, defaults to `prompt.mjs`).

---

## 3. ac-native Main Boot Sequence

The `main()` function begins at line 1773 of `ac-native.c`. The binary records a monotonic clock timestamp at entry (`boot_start`) and reports total boot time at the end.

### 3.1 PID 1 Detection and Filesystem Setup (lines 1777-1802)

Two code paths based on PID:

**PID 1 (direct DRM boot — no cage):**
- Calls `mount_minimal_fs()` (line 217) which mounts proc, sysfs, devtmpfs, devpts, shm
- Creates 1GB zram swap (again — safe if already done by init, skips silently)
- Brings up loopback
- **Waits up to 1 second** for `/dev/dri/card0` or `/dev/fb0` to appear (line 246, 100 iterations x 10ms)
- Sets all CPUs to performance governor

**Non-PID-1 (under cage compositor):**
- Filesystems already mounted by init/parent
- Opens `cage-child.log` on USB or falls back to tmpfs

Both paths set `PATH` and SSL environment variables.

### 3.2 Signal Handling (lines 1804-1814)

- `SIGINT`, `SIGTERM` -> graceful shutdown (`running = 0`)
- `SIGUSR1` -> DRM master release (for browser handoff)
- `SIGUSR2` -> reboot request
- `SIGSEGV`, `SIGBUS`, `SIGABRT`, `SIGFPE` -> crash report to `/mnt/crash.json` with machine ID and timestamp, then re-raise

### 3.3 Display Initialization (lines 1834-1880)

Two display backends, selected at runtime:

**Wayland path** (when `WAYLAND_DISPLAY` env var is set — running under cage):
- Calls `wayland_display_init()` which connects to the cage compositor
- Creates a minimal `ACDisplay` struct with width/height from Wayland surface
- On failure: exits with code 1 (cage will restart or fall through to DRM)

**DRM path** (direct framebuffer — default for PID 1 boot):
- Calls `drm_init()` which opens `/dev/dri/card0`, finds a connected CRTC, and sets up double-buffered dumb buffers
- Fatal if no display found — powers off after 5 seconds if PID 1

**Framebuffer creation** (line 1869):
- Software framebuffer at 1/3 display resolution: `fb_create(display->width / 3, display->height / 3)`
- Default `pixel_scale = 3` (3x nearest-neighbor upscale on present)

### 3.4 Graphics and Font Init (lines 1883-1888)

- `graph_init()` — software rendering context bound to framebuffer
- `font_init()` — loads built-in bitmap font
- Creates separate cursor overlay framebuffer (prevents KidLisp effects from smearing cursor)

### 3.5 USB/Disk Mount and Log Setup (line 1894)

`try_mount_log()` (defined at line 266):

1. **Waits up to 2 seconds** for USB block devices (`/dev/sda1`, `/dev/sdb1`) to appear (100 iterations x 20ms)
2. **Two-pass mount strategy:**
   - Pass 0: Try removable devices first (USB install source)
   - Pass 1: Fall back to internal ESP (for disk-installed boots)
3. Scans: `sda1, sdb1, sdc1, sdd1, nvme0n1p1, nvme1n1p1`
4. Mounts as VFAT at `/mnt`
5. Opens `/mnt/ac-native.log` in append mode
6. Dumps init log (`/tmp/ac-init.log`) and cage stderr to `/mnt/init.log`
7. Logs all available block devices with removable status

### 3.6 Config Loading (line 1897)

`load_boot_visual_config()` (defined at line 542):

Reads `/mnt/config.json` (USB/HD) or falls back to `/default-config.json` (baked in initramfs).

Extracts:
- **`handle`** — sets `AC_HANDLE` env var, generates time-of-day greeting as boot title (e.g., "good morning @jeffrey")
- **`colors`** — per-character RGB colors for the boot title text, from the `handle-colors` API
- **`wifi`** — boolean, defaults to true; when false, sets `wifi_disabled = 1`
- **`claudeCreds`** — JSON object written to `/tmp/.claude/.credentials.json`
- **`claudeState`** — JSON object written to `/tmp/.claude.json`
- **`voice`** — "off" disables keystroke TTS

The config supports an `AC_IDENTITY_BLOCK_V1` header line (used by `ac-usb` flash tool).

### 3.7 Audio Init (line 1900)

`audio_init()` (defined at audio.c:616):

This is the most timing-sensitive subsystem. The function:

1. **Allocates state** — `ACAudio` struct with defaults: BPM 120, sample rate 192kHz (default target), reverb buffers, 10-second sample buffer at 48kHz, TTS ring buffer (5 seconds at output rate).

2. **Seeds default sample** — Generates a 550ms synthesized one-shot (240Hz with wobble and exponential decay) so sample-mode is playable before first mic recording (audio.c:587-614).

3. **Waits for sound card** — Polls for `/dev/snd/pcmC{0,1,2}D0p` for up to **8 seconds** (400 iterations x 20ms) at audio.c:665-670. This is the critical wait for HDA codec probe completion.

4. **Diagnostics** — On timeout, distinguishes between:
   - HDA controller present but codec not probed (`/dev/snd/controlC0` exists but no PCM device)
   - No hardware at all
   Dumps `/proc/asound/cards` and `/dev/snd/` contents to `/mnt/ac-audio.log`.

5. **ALSA device open** — Tries 11 device names with **5 retry attempts** (2-second sleep between retries, audio.c:722-743):
   ```
   hw:0,0  hw:1,0  hw:0,1  hw:1,1  hw:0,2  hw:0,3  hw:1,2  hw:1,3
   plughw:0,0  plughw:1,0  default
   ```
   Total worst case: 5 attempts x 2s sleep = 10 additional seconds on top of the 8s card wait.

6. **ALSA configuration** (audio.c:755-801):
   - Format: S16_LE interleaved, 2 channels
   - Rate: Requests 192kHz (AUDIO_SAMPLE_RATE), accepts whatever the hardware provides via `set_rate_near`
   - Period size: `AUDIO_PERIOD_SIZE` (negotiated)
   - Buffer: 6 periods for underrun tolerance
   - Logs actual vs. requested rate (common mismatch on some codecs)

7. **Mixer setup** (audio.c:809-875):
   - Opens ALSA mixer for the detected card
   - Unmutes **every** playback switch found
   - Sets **all** playback volumes to maximum
   - Enables all capture switches and sets capture volume to 90% of max
   - Attempts HDA codec verb injection for ALC257 (Speaker node 0x14, HP Out node 0x21)

8. **HDMI audio** (audio.c:882-920) — Best-effort secondary output:
   - Tries: `hdmi:0,0` through `hdmi:1,1`, `plughw:0,3`, `plughw:0,7`, `plughw:0,8`, `plughw:1,3`, `plughw:1,7`
   - Opens non-blocking, 48kHz, 512-frame period
   - Calculates downsample ratio from primary rate

9. **Audio thread** (audio.c:924) — Starts the real-time audio thread at SCHED_FIFO priority 50.

**Audio is treated as optional** — if no ALSA device is found after all retries, `audio->pcm` is NULL and the struct is returned with a status message. The rest of the system continues without sound.

### 3.8 TTS Init and Precache (lines 1901, 1928-1929)

In DRM boot path only:

- `tts_init(audio)` (tts.c:125) — Initializes Flite with two voices:
  - `cmu_us_slt` (female, primary)
  - `cmu_us_kal` (male, secondary)
  - Both set to 0.9x duration stretch (slightly faster speech)
  - Starts a background TTS thread

- `tts_precache(tts)` (tts.c:204) — Pre-renders all lowercase letters (a-z) and digits (0-9) as cached TTS waveforms. This eliminates latency on keystroke speech during typing.

### 3.9 Boot Beep (line 1927)

`audio_boot_beep(audio)` (audio.c:1407-1413):
- Two-tone "doo-dah": E5 (660Hz, 120ms) then B5 (990Hz, 150ms) with 80ms gap
- Stereo-panned slightly left then right

### 3.10 Boot Animation — Startup Fade (line 1937)

`draw_startup_fade()` (defined at line 1288):

**300 frames at 60fps = 5-second animation.**

1. **Immediate solid frame** — White (daytime 7am-6pm LA time) or black (nighttime) to hide kernel console text.
2. **Version check** — Compares `AC_GIT_HASH-AC_BUILD_TS` against `/mnt/booted-version`. Marks as "FRESH" if different. Appends to `/mnt/boot-history.log`.
3. **Machine ID** — Reads `/mnt/.machine-id` or generates `notepat-XXXXXXXX` (random hex from `/dev/urandom`).
4. **TTS greeting** (frame 10) — Speaks time-of-day greeting with handle name and build name: "good morning jeffrey. enjoy Los Angeles! fuzzy-meadow."
5. **Visual elements:**
   - Fade from solid color to time-of-day themed background (cream/warm white for day, deep blue/sunset purple for night)
   - Centered title with per-character handle colors (animated pulse)
   - Version panel (top-right): build name, git hash, build timestamp, "FRESH" badge
   - "enjoy Los Angeles!" subtitle (appears after frame 130)
   - Auth badges (bottom-left): pixel-art crab (Claude token present) and octocat (GitHub PAT present)
   - Shrinking time bar at bottom
   - "W: install to disk" hint (only when booting from removable USB)
6. **Input handling** — Opens all `/dev/input/event*` devices non-blocking:
   - First 60 frames (1 second): all key presses are drained/ignored
   - After frame 60: `W` key triggers install-to-disk flow, any other key skips animation

### 3.11 Input Init (line 1944)

`input_init()` (input.c:315):

- Scans all `/dev/input/event*` devices
- Opens each with `O_RDONLY | O_NONBLOCK | O_CLOEXEC`
- Checks event bits: accepts devices with EV_KEY, EV_ABS, EV_REL, or EV_SW
- Detects tablet mode (EV_SW + SW_TABLET_MODE) with sysfs fallback for ThinkPad ACPI
- Identifies NuPhy keyboards by vendor ID — marks them for hidraw analog handling instead of evdev
- Calls `hidraw_scan()` for analog keyboard support via HID raw protocol

### 3.12 Install-to-Disk Flow (lines 1967-1981)

Triggered only when user pressed `W` during boot animation and confirmed with `Y`:

`auto_install_to_hd()` (line 713):
1. Identifies source (removable USB with kernel at `EFI/BOOT/BOOTX64.EFI`)
2. Finds target non-removable block device (NVMe or SATA)
3. Creates GPT with EFI System Partition via `sfdisk`
4. Formats FAT32, copies kernel and config.json
5. Supports both direct EFI stub and systemd-boot layouts
6. Displays animated progress during install
7. On success: prompts reboot (with TTS "rebooting", shutdown sound, 600ms delay)

### 3.13 WiFi Init (line 1987)

`wifi_init()` (wifi.c:653):

1. Checks for `iw` binary existence
2. Logs diagnostic info: network interfaces, firmware files, kernel wireless messages from `/dev/kmsg`, PCI device enumeration
3. **Waits up to 3 seconds** for wireless interface (30 attempts x 100ms)
4. If found, stores interface name (e.g., `wlan0`)
5. `wifi_autoconnect()` called immediately after — connects to saved/preset network

WiFi is skipped entirely if `wifi_disabled` is set from config.json.

### 3.14 Secondary HDMI Display (lines 1997-2000)

`drm_init_secondary()` — attempts to find a second connected DRM output for HDMI mirroring/extension.

### 3.15 JS Runtime Init (line 2004)

`js_init()` (js-bindings.c:1637):

1. Allocates `ACRuntime` struct with pointers to all subsystems (graph, input, audio, wifi, tts)
2. Creates WebSocket and UDP client handles
3. Initializes 3D camera
4. Creates QuickJS-ng runtime and context
5. Registers `Form` and `Painting` JS classes
6. Sets up module loader (`JS_SetModuleLoaderFunc`)
7. Registers the full native API surface:
   - **Graphics chain API**: box, line, circle, plot, write, scroll, blur, zoom, contrast, spin, qr, form, ink
   - **Top-level graphics**: wipe, ink, line, box, circle, qr, plot, write, scroll, blur, zoom, contrast, spin
   - **console**: log, warn, error
   - **performance**: now()
8. Evaluates init JS code (Button, Box classes)
9. Evaluates browser API stubs for KidLisp compatibility

### 3.16 Config Application to Runtime (lines 2016-2077)

After JS init, re-reads `/mnt/config.json` to populate runtime fields:
- `rt->handle` and `rt->piece` (boot piece override)
- Voice config ("off" disables keystroke TTS)
- Claude token -> `/claude-token`
- GitHub PAT -> `/github-pat`
- Piece aliases: "claude" and "cc" resolve to "terminal" with param "claude"
- Resolves piece path: `/pieces/{name}.mjs`

### 3.17 Piece Load and Boot (lines 2080-2144)

1. `js_load_piece(rt, piece_path)` — Evaluates the `.mjs` file, extracts `boot`, `paint`, `act`, `sim`, `leave`, `beat` function references
2. Logs boot time: `"Booted in %.1fms"` (line 2105)
3. Logs subsystem status: audio, backlight, input devices, NuPhy analog status, JS function availability
4. Waits for TTS to finish greeting + 300ms drain + plays ready melody (C5-E5-G5 ascending triad) + 400ms ring-out
5. `audio_prewarm()` — plays near-silent 440Hz note (50ms, 0.001 volume) to fill ALSA buffers
6. Drains queued input events from boot animation
7. `js_call_boot(rt)` — Calls the piece's `boot()` function
8. `perf_init()` — Initializes performance logger (30-second CSV chunks to `/mnt/perf/`)

### 3.18 DRM-to-Cage Transition (lines 2149-2280)

After boot completes in DRM mode (compiled with `USE_WAYLAND`), the system attempts to transition to a Wayland session for browser popup support:

1. Destroys audio (cage child will re-open ALSA)
2. Releases DRM master
3. **Forks:**
   - **Child**: Sets env vars (`WLR_RENDERER=pixman`, `WLR_BACKENDS=drm`), creates XDG runtime dir, starts `seatd -g root`, waits up to 3s for `/run/seatd.sock`, then `execlp("cage", "cage", "-s", "--", "/ac-native", "/piece.mjs")`
   - **Parent**: Waits for cage to exit, copies cage stderr to USB log

This means ac-native boots twice: once in DRM for fast startup, then re-execs under cage for full Wayland capabilities.

---

## 4. Timing

Based on log data and code analysis, approximate timing from kernel handoff:

| Phase | Duration | Cumulative | Notes |
|---|---|---|---|
| Kernel + initramfs decompress | ~1-2s | ~1-2s | LZ4 decompression, driver probe |
| Init script (mounts, zram, dmesg dump) | ~1-2s | ~2-4s | dmesg dump takes ~1s (sleep 1) |
| Display wait | 0-1s | ~2-5s | Up to 1s for /dev/dri/card0 |
| USB mount + log setup | 0-2s | ~2-7s | Up to 2s waiting for USB block devs |
| Config load | <1ms | — | Simple file read + JSON parse |
| Audio init | 0-18s | ~2-25s | **Highly variable** — see Known Issues |
| TTS init + precache | ~0.5-1s | — | 36 Flite renders (a-z, 0-9) |
| Boot animation | 0-5s | — | Skippable with any keypress |
| Input init | <100ms | — | evdev + hidraw scan |
| WiFi init | 0-3s | — | Interface detection wait |
| JS init + piece load | ~100-200ms | — | QuickJS context + module eval |
| **Total (typical, audio found quickly)** | | **~9s** | From kernel to piece running |

The `ac_log` at line 2105 reports `"Booted in %.1fms"` measuring from `main()` entry to piece load completion. With well-behaved audio hardware (card appears quickly, first ALSA device opens on attempt 0), total time from kernel to playable piece is approximately 9 seconds.

---

## 5. Known Issues

### 5.1 ALSA Race Condition (HDA Codec Probe Timing)

**The primary boot-time variability comes from audio initialization.**

On fast NVMe boots, the HDA codec may not be fully probed when ac-native first tries to open ALSA. The code handles this with a two-layer retry:

1. **Card wait loop** (audio.c:665-670): Polls for `/dev/snd/pcmC{0,1,2}D0p` for 8 seconds. If timeout occurs and `/dev/snd/controlC0` exists, it means the HDA controller initialized but the codec hasn't finished probing — this is logged as `"HDA ctrl ok, codec not probed"`.

2. **ALSA device open retries** (audio.c:722-743): Even after a PCM device node appears, `snd_pcm_open()` can fail if the codec isn't fully ready. The code retries 5 times with 2-second intervals across 11 different device names.

**Worst case**: 8s card wait + 5 retries x 2s = 18 seconds for audio init alone. On machines where audio hardware is ready quickly, this completes in under 1 second.

The diagnostic output is written to `/mnt/ac-audio.log` (separate from the main log) with full `/proc/asound/cards` and `/dev/snd/` directory listings.

### 5.2 Missing Shared Libraries

The build system uses dynamic linking by default (gcc, not musl-gcc). This means all shared libraries must be correctly bundled into the initramfs. The build script (build-and-flash.sh) handles this with:

1. Per-binary `ldd` scanning for direct dependencies
2. A two-pass transitive dependency resolver (lines 740-769) that scans all ELF files in the initramfs and copies missing `.so` files

Historical issues:
- Libraries missed by single-pass `ldd` (transitive deps of deps)
- Unversioned `.so` symlinks needed for `dlopen()` but not created
- Mesa GPU stack requiring `libexpat`, `libffi`, `libxkbcommon` that aren't direct deps of ac-native

The build now generates an initramfs manifest (`build/initramfs-manifest.txt`, copied into the image as `/manifest.txt`) for build parity verification.

### 5.3 Firmware Requirements

Required firmware blobs (bundled in initramfs at `/lib/firmware/`):

| Firmware | Purpose |
|---|---|
| `iwlwifi-9260-th-b0-jf-b0-*.ucode` | Intel WiFi 9260 |
| `iwlwifi-cc-a0-*.ucode` | Intel WiFi AX200 |
| `iwlwifi-QuZ-a0-hr-b0-*.ucode` | Intel WiFi AX201 |
| `iwlwifi-QuZ-a0-jf-b0-*.ucode` | Intel WiFi AX201 (variant) |
| `regulatory.db` + `.p7s` | Wireless regulatory database |
| `i915/*` | Intel GPU display firmware (all generations) |
| `intel/sof/*`, `intel/sof-ipc4/*` | Intel SOF audio firmware |
| `intel/sof-tplg/*`, `intel/sof-ipc4-tplg/*` | SOF audio topology |

The kernel config also embeds a subset of WiFi firmware via `CONFIG_EXTRA_FIRMWARE` for early availability before the initramfs mounts firmware. The build script decompresses `.xz` and `.zst` compressed firmware from the host system.

### 5.4 Wayland/Cage Transition Complexity

The DRM-to-cage transition (Section 3.18) is inherently fragile:
- Audio must be destroyed and re-opened by the cage child
- DRM master must be released before cage can take it
- seatd must start successfully within 3 seconds
- The ac-native binary effectively boots twice (DRM then Wayland)
- If cage fails, the parent logs the error but the system stays on the DRM frame (no automatic fallback to DRM-only mode with continued operation)

### 5.5 Clock Accuracy

Time-of-day greetings and themed backgrounds depend on `get_la_hour()` which computes LA time. Since the system has no RTC battery on some hardware and NTP hasn't run yet at boot, the kernel time may be epoch (1970). The code accepts this gracefully — it just shows the wrong greeting.

---

## 6. Build Pipeline

### Local builds (`ac-os build`)

Orchestrated by `fedac/native/ac-os`:

1. **Binary build** (`build_binary()`):
   - Runs `make CC=gcc` (or musl-gcc if available with linux/input.h)
   - Passes `BUILD_TS` env var for timestamp embedding
   - **Verifies** the binary contains the current git hash via `strings` check
   - Builds BPF trace tools if Makefile exists

2. **Initramfs build** (`build_initramfs()`):
   - Runs `build-and-flash.sh --skip-kernel --skip-binary` under sudo
   - **KidLisp bundling** done as user (npx unavailable under sudo): `npx esbuild` bundles `kidlisp.mjs` as IIFE
   - Copies fresh binary into initramfs
   - **Claude Code native binary**: copies from `~/.local/share/claude/versions/` (latest), bakes OAuth token from `~/.claude/.credentials.json`, bakes GitHub PAT from `gh auth token`
   - Bakes credentials into initramfs root files

3. **Kernel build**: Runs `build-and-flash.sh` for kernel compilation with embedded initramfs

### Makefile version stamps (Makefile:10-15)

```makefile
GIT_HASH   := $(shell git rev-parse --short HEAD)
BUILD_TS   := $(shell date -u '+%Y-%m-%dT%H:%M')
BUILD_NAME := $(shell scripts/build-name.sh --bump)
```

These are compiled into the binary via `-DAC_GIT_HASH`, `-DAC_BUILD_TS`, `-DAC_BUILD_NAME` and used for:
- Boot screen version panel
- TTS greeting (build name spoken aloud)
- `/mnt/booted-version` comparison for "FRESH" detection
- `/mnt/boot-history.log` entries

**Critical note** (Makefile:123 comment): Make's dependency tracking doesn't detect changes to `AC_BUILD_NAME` or `AC_GIT_HASH` because they're not source file changes. The Makefile forces a rebuild of the main object file to prevent stale version strings.

### Oven builds vs. local builds

The primary differences:

| Aspect | Local (`ac-os`) | Oven (CI/Docker) |
|---|---|---|
| Compiler | gcc (dynamic) or musl-gcc (static) | gcc (dynamic) |
| Claude binary | From `~/.local/share/claude/versions/` | Downloaded from GCS (`claude-code-releases`) |
| Claude credentials | From `~/.claude/.credentials.json` | Baked by `ac-usb flash` injection |
| GitHub PAT | From `gh auth token` | Baked by flash injection |
| KidLisp bundle | `npx esbuild` (user space) | Not available under sudo |
| Firmware | Host `/lib/firmware/` (all generations) | Host `/lib/firmware/` (Docker image) |
| i915 blobs | All from `/lib/firmware/i915/` | All from Docker image |
| SOF audio | All from host | All from Docker image |
| Manifest | Generated and embedded | Generated and embedded |
| Build name | `scripts/build-name.sh --bump` | Same (increments per build) |

Both produce the same artifact structure: a single `vmlinuz` with embedded LZ4 initramfs CPIO. The build parity is verified via the initramfs manifest file.

### Flash pipeline

`ac-os flash` extends the build with USB write:
1. Full rebuild (binary + initramfs + kernel)
2. GPT partition table with 512MB ESP
3. FAT32 format labeled `AC-NATIVE`
4. Kernel copied to `EFI/BOOT/BOOTX64.EFI` (or splash chainloader + `KERNEL.EFI`)
5. `config.json` with handle + colors from `handle-colors` API
6. Credentials injected (Claude token, GitHub PAT, Claude settings)

`ac-os upload` always rebuilds first (kernel embeds git hash at compile time), then uploads to OTA release endpoint.
