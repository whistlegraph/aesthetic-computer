# FedAC Native OS — Progress Report

_Last updated: 2026-03-09_

## What It Is

Bare-metal Linux OS that boots from USB via UEFI as PID 1. Runs a QuickJS engine
executing `.mjs` pieces (interactive programs) with direct hardware access — DRM
display, ALSA audio, evdev input, WiFi, TTS. No userspace, no systemd, no shell.

## Current State: Fully Functional

### Boot & Runtime
- Custom kernel (6.14.2) with embedded LZ4 initramfs
- Boot time: ~7.3s from power to piece running
- Per-user config: `config.json` on EFI partition injects `@handle` into boot splash
  (rainbow animated text), TTS greeting ("hi jeffrey"), and shutdown ("bye jeffrey")
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

### Controls
- Volume & brightness: hardware keys (F1-F6) + mouse drag on status bar indicators
- JS bindings: `system.volumeAdjust(delta)`, `system.brightnessAdjust(delta)`
- Echo/pitch sliders: touch drag or trackpad
- Trackpad FX mode (\ toggle): trackpad X→echo, Y→pitch

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
| `src/tts.c` | Text-to-speech via flite |
| `src/drm-display.c` | KMS/DRM modesetting |
| `pieces/notepat.mjs` | Main instrument piece |
| `pieces/prompt.mjs` | Command-line navigation piece |
| `scripts/build-and-flash.sh` | Full build pipeline (binary + initramfs + kernel) |
| `scripts/upload-release.sh` | Publish release to DO Spaces |
| `Makefile` | Binary build |

## Build & Flash

```bash
# In devcontainer:
cd fedac/native
make                                          # build binary
bash scripts/build-and-flash.sh --skip-binary # build kernel+initramfs

# Flash via docker host SSH:
ssh me@172.17.0.1 "bash -c 'sudo mcopy -o -i /dev/sda1 \
  /home/me/aesthetic-computer/fedac/native/build/vmlinuz \
  ::EFI/BOOT/BOOTX64.EFI && sync'"

# Upload remote release:
bash scripts/upload-release.sh
```

## Known Issues / Next Steps
- Audio tearing at high echo + many simultaneous voices (buffer underrun)
- No regulatory.db for WiFi (region defaults)
- i915 DMC firmware missing (cosmetic kernel warning, no functional impact)
- `console.log` from JS now writes to USB log (`[js]` prefix) — useful for debugging
- NuPhy analog keyboard support compiled in but untested on current hardware
