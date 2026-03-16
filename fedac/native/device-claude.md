# CLAUDE.md — AC Native OS Device Context

You are running on an AC Native OS device — a minimal Linux system booted from USB or internal disk. This is NOT a standard Linux desktop. Here's what you need to know.

## System Overview

- **OS**: Custom Linux 6.14.2 kernel with embedded initramfs (no package manager)
- **Display**: DRM direct rendering (software framebuffer, no X11/Wayland desktop)
- **Audio**: ALSA direct (hw:0,0)
- **WiFi**: iwlwifi + wpa_supplicant
- **Shell**: BusyBox ash (not bash — no arrays, limited features)
- **User**: root (PID 1 is ac-native)
- **Home**: /tmp (tmpfs — non-persistent)
- **Persistent storage**: /mnt (USB or internal EFI partition, VFAT)

## What You Can Do

### File Operations
- Read/write files in /tmp (lost on reboot)
- Read/write files in /mnt (persistent, VFAT — no symlinks, no permissions)
- The main AC repo is NOT on this device — use git clone if needed

### Network
- curl is available for HTTP requests
- git is available (GitHub PAT is pre-configured via GH_TOKEN)
- DNS works (8.8.8.8 / 1.1.1.1 fallback)

### Development
- No gcc/make — compile on the devcontainer, flash via OTA
- No Node.js (unless USE_NODE=1 build)
- QuickJS is embedded in ac-native for JS evaluation

## What You Cannot Do

- Install packages (no apt/dnf/apk)
- Run Docker
- Open a browser
- Use systemd/systemctl
- Write to / (rootfs is the initramfs, read-only after boot)

## Key Paths

| Path | Purpose |
|------|---------|
| /mnt/config.json | User identity + auth tokens |
| /mnt/ac-native.log | System log (persistent) |
| /claude-token | Claude OAuth token (initramfs) |
| /github-pat | GitHub PAT (initramfs) |
| /tmp/.claude/ | Claude Code config directory |
| /ac-native | Main system binary |
| /piece.mjs | Default JS piece |
| /bin/claude | Claude Code binary |

## Architecture

ac-native is the only userspace process (PID 1). It provides:
- DRM display rendering
- JS runtime (QuickJS) for interactive "pieces"
- PTY terminal emulator (for Claude Code)
- WiFi management
- Audio synthesis
- WebSocket/UDP networking

Pieces are JS modules with lifecycle functions (boot/paint/act/sim).
The user types commands in a prompt piece, which can jump to other pieces.

## Common Tasks

### Check system info
```sh
cat /mnt/config.json
cat /proc/version
free -h
```

### Network check
```sh
ip addr
ping -c 1 8.8.8.8
curl -fsSL https://aesthetic.computer
```

### Git operations
```sh
cd /tmp
git clone https://github.com/whistlegraph/aesthetic-computer.git
cd aesthetic-computer
# GH_TOKEN is pre-set — push works
```

### OTA update
The device checks for updates automatically via the `os` piece.
