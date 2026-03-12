# FedAC Native OS — Progress Report

_Last updated: 2026-03-12_

## What It Is

Bare-metal Linux OS that boots from USB via UEFI as PID 1. Runs a QuickJS engine
executing `.mjs` pieces (interactive programs) with direct hardware access — DRM
display, ALSA audio, evdev input, WiFi, TTS, camera. No userspace, no systemd, no shell.

## Current State: Fully Functional

### Boot & Runtime
- Custom kernel (6.14.2) with embedded LZ4 initramfs
- Boot time: ~7.3s from power to piece running
- Per-user config: `config.json` on EFI partition injects `@handle` into boot splash
  (rainbow animated text), TTS greeting ("hi jeffrey"), and shutdown ("bye jeffrey")
- **Build naming**: Each build gets a unique adjective-animal name (e.g., "keen-egret")
  displayed in golden text on boot screen (top-right panel) and spoken via TTS
  - Animal rotates daily (365 pool), adjective bumps every build (200 pool)
  - Counter persisted in `.build-counter`, auto-bumped by Makefile
- Auth-gated image generation: `GET /api/os-native?piece=notepat` requires handle
- Piece navigation: `system.jump("prompt")` hot-swaps pieces without reboot

### Pieces
- **notepat.mjs** (~7800 lines) — Full chromatic keyboard instrument
  - 12-note grid, 6 octaves, 5 wave types (sine/tri/saw/square/noise)
  - Echo (room reverb) slider, pitch shift slider, metronome
  - Touch + keyboard + NuPhy analog pressure support
  - Status bar: clock (LA time w/ DST), wave, octave, kHz, vol, brt, battery, WiFi
  - HDMI secondary display output (blended note colors)
- **prompt.mjs** — Command-line piece with pink block cursor, history, escape→notepat
- **os.mjs** — System management & hardware info
  - OTA update: check version, download, flash, verify, reboot (with animations)
  - Flash target selector (tab to cycle USB/NVMe/etc.)
  - Responsive layout: two-column on wide screens (>260px)
  - System stats: model/vendor, CPU, RAM usage (text + graphical bar), battery
  - Cores, process count, load average (1/5/15 min), CPU governor
  - Connected devices list: USB, input/HID, cameras, audio, disks (with size/removable), display connectors (with connected status)
- **claude.mjs** — Claude AI chat client
  - OAuth refresh token auth (via Claude Code subscription)
  - Token provisioning: QR code scan (camera) or manual paste
  - Camera preview: live V4L2 feed during QR scan with crosshair overlay
  - Chat mode: word-wrapped conversation, scrollable history
  - Commands: `/clear`, `/token`, `/scan`

### Camera (V4L2 + quirc)
- UVC driver built into kernel (CONFIG_USB_VIDEO_CLASS)
- V4L2 capture: tries /dev/video0-3, YUYV format, 640x480
- Background scan thread: grabs frames at ~30fps, scans for QR codes
- Display buffer: mutex-protected grayscale copy for main thread rendering
- `system.cameraBlit(x, y, w, h)`: renders camera feed to framebuffer with nearest-neighbor scaling
- quirc library (vendored v1.2) for QR code decoding

### Audio
- ALSA direct `hw:` access, 192kHz (negotiated), 128-sample periods
- 32 max voices with oldest-voice stealing
- Room reverb: 3-tap delay, 0.55 feedback, controllable wet mix
- Glitch mode: sample-hold + 6-bit bitcrush
- TTS via flite (male + female voices)

### WiFi
- Auto-connect: scan → pick strongest known SSID → connect (fully silent, no TTS)
- 5s timeout on failed connects, retry with next scan cycle
- Only connects to networks actually detected in scan (no blind attempts)
- WiFi panel: select-then-confirm interface (first tap selects, second connects)
- Panel doesn't disrupt active connections (no scan while connected)
- Deduplicated SSID list (strongest signal per network)
- Saved/preset networks shown below scan results with visual distinction
- Password entry mode for new encrypted networks

### Chat (WebSocket)
- Connects to `wss://chat-system.aesthetic.computer/` after WiFi
- Displays latest message in status bar with TTS
- Dedup: same message not re-TTS'd on WS reconnect cycles
- Silent reconnection (server sends history then closes, client auto-reconnects)
- Mute toggle (M key or tap)

### OS Update System
- Background auto-update: checks version on WiFi connect, silent download + flash + reboot
- Manual OS panel: responsive layout, connection-aware, version check with 10s timeout
- Release uploads to Digital Ocean Spaces (`upload-release.sh`)
- Progress bar for downloads, "do not power off" during flash
- Shutdown animation with scrolling telemetry

### Hardware Info (`system.hw`)
Exposed to JS pieces every frame:
- `model`, `vendor` — DMI product/vendor strings
- `cpu` — model name from /proc/cpuinfo
- `cores` — logical core count
- `ramTotalMB`, `ramAvailMB` — from /proc/meminfo
- `processes` — count of running PIDs
- `load1`, `load5`, `load15` — from /proc/loadavg
- `governor` — CPU frequency scaling governor
- `buildName` — compile-time adjective-animal name
- `devices[]` — array of detected peripherals:
  - USB devices (product name, vendor, bus ID)
  - Input/HID devices (evdev name)
  - Cameras (V4L2 device name)
  - Audio cards (ALSA card name)
  - Block devices (model, size in GB, removable flag)
  - Display connectors (DRM name, connected status)

### Controls
- Volume & brightness: hardware keys (F1-F6) + mouse drag on status bar indicators
- JS bindings: `system.volumeAdjust(delta)`, `system.brightnessAdjust(delta)`
- Echo/pitch sliders: touch drag or trackpad
- Trackpad FX mode (\ toggle): trackpad X→echo, Y→pitch
- Pixel scaling: Ctrl+= / Ctrl+- (wraps through 1-8x)

### Fonts
- `font_0`: built-in 8x8 bitmap
- `font_1`: 6x10 BDF (from system assets)
- `font_matrix`: MatrixChunky8 BDF

## Key Files
| File | Description |
|------|-------------|
| `src/ac-native.c` | Main loop, DRM init, boot splash, shutdown |
| `src/js-bindings.c` | QuickJS API surface, lifecycle calls, system bindings |
| `src/js-bindings.h` | ACRuntime struct (state for everything) |
| `src/audio.c` | ALSA engine, synthesis, reverb, glitch |
| `src/graph.c` | Software framebuffer (wipe/ink/box/line/circle/write) |
| `src/font.c` | BDF font renderers |
| `src/input.c` | evdev keyboard/mouse/touchpad, NuPhy hidraw analog |
| `src/wifi.c` | WiFi management (wpa_supplicant, dhclient) |
| `src/ws-client.c` | WebSocket client (TLS, background thread) |
| `src/udp-client.c` | UDP fairy point co-presence |
| `src/camera.c` | V4L2 capture, grayscale conversion, QR scanning |
| `src/camera.h` | ACCamera struct (V4L2 state, display buffer, scan results) |
| `src/tts.c` | Text-to-speech via flite |
| `src/drm-display.c` | KMS/DRM modesetting |
| `pieces/notepat.mjs` | Main instrument piece |
| `pieces/prompt.mjs` | Command-line navigation piece |
| `pieces/os.mjs` | OS update + system info piece |
| `pieces/claude.mjs` | Claude AI chat client piece |
| `lib/quirc/` | Vendored quirc v1.2 QR decoder |
| `scripts/build-name.sh` | Unique build name generator (adjective-animal) |
| `scripts/build-and-flash.sh` | Full build pipeline (binary + initramfs + kernel) |
| `scripts/upload-release.sh` | Publish release to DO Spaces |
| `Makefile` | Binary build (auto-bumps build name) |
| `ac-os` | Single-command build/flash/upload tool |

## Build & Flash

```bash
# Full build + flash (ALWAYS use this — never just `make` alone):
cd fedac/native
./ac-os flash

# Build only (no flash):
./ac-os build

# Build + flash + upload OTA release:
./ac-os flash+upload

# Upload current build as OTA:
./ac-os upload
```

**Important**: Always use `./ac-os flash` for the full pipeline. Running `make` alone
only compiles the binary but doesn't rebuild the initramfs or kernel. The kernel embeds
the initramfs, so a standalone `make` + separate flash results in stale code on the USB.

## Known Issues / Next Steps
- Audio tearing at high echo + many simultaneous voices (buffer underrun)
- No regulatory.db for WiFi (region defaults)
- i915 DMC firmware missing (cosmetic kernel warning, no functional impact)
- `console.log` from JS writes to USB log (`[js]` prefix) — useful for debugging
- NuPhy analog keyboard support compiled in but untested on current hardware
- Camera preview is grayscale only (YUYV Y-channel extraction)
