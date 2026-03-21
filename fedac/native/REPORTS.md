# AC Native — Stack Report

*Last updated: 2026-03-11*

## What Is This?

AC Native is a bare-metal Linux system that boots from USB (or internal disk) directly into a creative computing runtime. No distro, no systemd, no desktop — just a custom kernel running a C binary as PID 1 that executes JavaScript "pieces" via QuickJS.

## The Stack

### Layer 0: Hardware
- **Target**: ThinkPad X1 Nano Gen 2 (primary), any x86_64 UEFI machine
- **Display**: DRM (Direct Rendering Manager) — writes pixels directly to framebuffer
- **Audio**: ALSA (Advanced Linux Sound Architecture) — PCM output + Flite TTS
- **Input**: evdev — keyboard, trackpad, touchscreen, joystick, NuPhy analog keys
- **WiFi**: Intel iwlwifi (9260, AX200, AX201) via wpa_supplicant + dhclient
- **Storage**: FAT32 EFI System Partition on USB or internal NVMe

### Layer 1: Kernel
- **Linux 6.14.2** — custom minimal config (~200 options vs ~5000 in a typical distro)
- **No distro** — not Fedora, Ubuntu, or anything else. Raw kernel + initramfs
- **EFI stub boot** — kernel IS the bootloader (BOOTX64.EFI on FAT32 ESP)
- **Embedded initramfs** — root filesystem is LZ4-compressed CPIO archive baked into the kernel
- **Size**: ~40MB total (kernel + initramfs)

### Layer 2: Initramfs (Root Filesystem)
Everything the system needs lives in a ~33MB compressed archive:

| Component | Size | Purpose |
|-----------|------|---------|
| ac-native binary | 1.1 MB | PID 1 — the entire runtime |
| Shared libraries | ~40 MB (uncompressed) | libc, libdrm, libasound, libssl, etc. |
| WiFi firmware | 4.9 MB | Intel iwlwifi microcode |
| Flite TTS | 5.2 MB | Text-to-speech (2 voices) |
| ALSA config | 352 KB | Audio device configuration |
| WiFi tools | ~6 MB | wpa_supplicant, dhclient, iw, ip |
| Dropbear SSH | 228 KB | SSH daemon for remote access |
| CA certificates | ~200 KB | HTTPS trust chain |
| Pieces (.mjs) | ~120 KB | JavaScript programs |
| Shell + utils | ~3 MB | bash, grep, curl, etc. |

### Layer 3: ac-native (The Runtime)
A single C binary (~1.1MB) that does everything:

- **PID 1 init**: Mounts /proc, /sys, /dev, /tmp. Sets CPU governor. Finds boot device
- **Display**: DRM initialization, double-buffered framebuffer, 60fps vsync
- **Graphics**: Software rasterizer — wipe, ink, line, box, circle, plot, write (bitmap fonts)
- **Audio**: ALSA PCM output with synthesizer (sine, square, saw, triangle, noise)
- **Input**: Multi-device polling (keyboard, mouse, trackpad, touch, analog keys)
- **WiFi**: Scan, connect, DHCP — full network stack management
- **JavaScript**: QuickJS engine with custom AC API bindings
- **Networking**: curl (HTTP/S), WebSocket client (TLS), UDP client
- **OTA updates**: Download vmlinuz, flash to EFI partition, verify, reboot
- **Config**: JSON config on FAT32 partition (handle, colors, tokens)
- **SSH**: Dropbear daemon for remote access

### Layer 4: Pieces (JavaScript)
Interactive programs executed by QuickJS with lifecycle functions:

```
boot() → [act() → sim() → paint()] × 60fps → leave()
```

**Current pieces:**
- `notepat.mjs` — Musical notation pad (default boot piece, ~96KB)
- `prompt.mjs` — Command line with KidLisp evaluation
- `lisp.mjs` — KidLisp visual evaluator
- `claude.mjs` — Claude API chat client (OAuth refresh tokens)
- `roz.mjs` — Character piece

## Boot Sequence

```
UEFI firmware
  → BOOTX64.EFI (Linux kernel with embedded initramfs)
    → /init (symlink to /ac-native)
      → mount /proc /sys /dev /tmp
      → mount EFI partition to /mnt
      → read /mnt/config.json (handle, colors, tokens)
      → init display (DRM)
      → render boot title ("hi @jeffrey" with custom colors)
      → init audio → boot beep (E5→B5 two-tone)
      → init input (keyboard, trackpad, etc.)
      → init WiFi (auto-connect to saved network)
      → load piece.mjs → call boot()
      → main loop: input_poll → act → sim → paint → present
```

**Boot time target**: Sub-second to interactive piece.

## Network Architecture

All networking is **outbound only** (except SSH when enabled):

- **HTTP/S**: curl via system() — used for OTA checks, clock sync, API calls
- **WebSocket**: Custom TLS client in background pthread — AC chat/multiplayer
- **UDP**: Raw datagrams for lightweight co-presence data
- **SSH**: Dropbear on port 22 (auto-starts on WiFi connect)

## OTA Update Flow

1. Check version: `GET https://aesthetic.computer/api/native/version`
2. Download kernel: `GET https://aesthetic.computer/api/native/vmlinuz` (binary fetch with progress)
3. Flash to EFI: Copy to /mnt/EFI/BOOT/BOOTX64.EFI with fsync + syncfs
4. Verify: Drop page caches, byte-for-byte comparison of source vs destination
5. Reboot: Show verified bytes count, then `reboot(RB_AUTOBOOT)`

## Claude Integration (3 Approaches)

### 1. Direct API Client (claude.mjs)
- Pure JavaScript piece running in QuickJS
- Uses `system.fetchPost()` for HTTP POST requests
- OAuth: Stores refresh token in config.json, exchanges for access token
- Calls `/v1/messages` API with Claude Sonnet
- No external dependencies

### 2. SSH Remote Access (dropbear)
- Lightweight SSH daemon (~228KB) bundled in initramfs
- Auto-starts when WiFi connects
- SSH in from any machine on the local network
- Run Claude Code CLI remotely over the SSH session
- `ssh root@<device-ip>`

### 3. Node.js + Claude Code CLI (planned)
- Bundle Node.js binary in initramfs
- `system.execNode(script)` runs JS via Node subprocess
- Could run actual `@anthropic-ai/claude-code` package
- Renders output through AC native display

## Key Files

```
fedac/native/
  src/
    ac-native.c         — PID 1, main loop, boot sequence
    js-bindings.c/h     — QuickJS ↔ C API (graphics, audio, network, system)
    drm-display.c/h     — DRM framebuffer display
    audio.c/h           — ALSA audio + synthesizer
    input.c/h           — evdev input handling
    wifi.c/h            — WiFi management
    ws-client.c/h       — WebSocket client
    graph.c/h           — Software rasterizer
    font.c/h            — Bitmap font renderer
  pieces/
    notepat.mjs         — Musical notation (default piece)
    prompt.mjs          — Command prompt
    claude.mjs          — Claude API client
    lisp.mjs            — KidLisp evaluator
  scripts/
    build-and-flash.sh  — Full build pipeline
    upload-release.sh   — OTA release to DO Spaces
  kernel/
    config-minimal      — Linux kernel config
    build-kernel.sh     — Kernel compilation script
  Makefile              — ac-native binary build
```

## Build Pipeline

```
make CC=gcc                           → build/ac-native (1.1MB)
scripts/build-and-flash.sh            → build/vmlinuz (~40MB)
  ├── compile ac-native
  ├── create initramfs (copy binary + libs + firmware + tools + pieces)
  ├── compress with LZ4
  ├── build Linux kernel with embedded initramfs
  └── flash to USB via mtools (no root mount needed)
```

## What Makes This Different

- **No OS layer**: No systemd, no package manager, no filesystem hierarchy. Just kernel + one binary
- **Sub-second boot**: UEFI → interactive in under 1 second
- **Single binary runtime**: Everything in one ~1.1MB C program
- **JavaScript pieces**: Creative programs with immediate-mode graphics, audio synthesis, network
- **OTA with verification**: Flash + fsync + drop caches + byte-compare before reboot
- **Fits on any USB**: ~40MB total, boots on any UEFI x86_64 machine
