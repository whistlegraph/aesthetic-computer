# FedAC — Fedora Boot-to-Aesthetic-Computer

A Fedora-based OS image that boots directly into Aesthetic Computer via the Electron app. Designed for ThinkPads. Sellable on a laptop.

## What It Does

```
Power on → Pals boot splash → Auto-login → AC Electron (fullscreen)
```

- **Electron kiosk** with flip view (Ctrl+B for terminal), auto-updates, offline pieces
- **Plymouth boot splash** with the pals image on dark purple
- **TUI escape hatch** on tty2 (Ctrl+Alt+F2) for WiFi config and piece selection
- **USB flasher** with checksum verification

## Quick Start

### Flash a USB

```bash
# Flash a pre-built FedAC ISO to USB
bash fedac/scripts/flash-usb.sh fedac-thinkpad.iso /dev/sdX
```

### Install on a ThinkPad

1. Boot the ThinkPad from USB
2. Anaconda runs the kickstart — fully automated
3. Reboot → pals splash → AC running

### Manual Install (on existing Fedora)

```bash
# Run post-install script on a fresh Fedora Workstation
sudo bash fedac/scripts/install.sh
```

## Structure

```
fedac/
├── kickstart/              # Anaconda kickstart files
├── plymouth/               # Boot splash theme (pals)
├── overlays/
│   ├── ac-setup/           # TUI for WiFi + piece config
│   ├── ac-electron-kiosk/  # Electron autostart + service
│   ├── ac-config-server/   # HTTP config server (phone setup)
│   └── offline-pieces/     # Bundle script for offline mode
├── scripts/
│   ├── install.sh          # Post-install setup
│   ├── build-iso.sh        # Build custom ISO (future)
│   └── flash-usb.sh        # USB flasher with verification
└── systemd/                # Service unit templates
```

## Key Bindings (in AC Electron kiosk)

- **Ctrl+B** — flip between AC and terminal
- **Ctrl+Alt+F2** — escape to TUI for WiFi/piece config
- **Ctrl+Alt+F1** — back to AC kiosk

## Requirements

- Fedora 41+ (Workstation, Wayland)
- x86_64 (ThinkPad recommended)
- 4GB+ RAM, 20GB+ disk
