# Study: FedAC — Fedora-Based Boot-to-Aesthetic-Computer OS

**Date:** 2026-02-21
**Context:** The FFOS (Arch Linux) pipeline in `utilities/ffos-build/` works but is heavy — Arch ISO builds via Docker + mkarchiso, upstream feral-file dependencies, hardware assumptions tied to FF1. Meanwhile, Fedora + Wayland runs beautifully on ThinkPads out of the box. This study explores building a Fedora-native "boot to AC" OS image that inherits the best ideas from FFOS while being simpler and ThinkPad-native.

---

## Why Move Away from FFOS for Laptops

### What FFOS gives us (and what we'd keep)
- Matrix rain TUI (`ac-setup.py`) — WiFi config, piece selection, offline mode
- Chromium kiosk service (`aesthetic-kiosk.service`) with GPU detection
- Offline piece bundles (notepat, starfield, roz) via `/api/bundle-html`
- Config server (`ac-config-server.py`) for headless setup via phone
- Systemd user service architecture with auto-restart

### What makes FFOS painful for ThinkPads
- **Arch Linux base** — rolling release, no LTS stability, unfamiliar package management
- **Docker + mkarchiso** — requires Arch container to build, no native Fedora tooling
- **FF1 hardware assumptions** — feral-controld, feral-setupd, feral-watchdog are device-specific daemons
- **No Wayland-native path** — kiosk script detects Wayland but falls back to `cage` compositor; not using Fedora's native GNOME/Wayland stack
- **Build pipeline removed** — the GitHub Actions workflow was added then deleted; no CI currently

### What Fedora gives us for free
- **Wayland/GNOME native** — works perfectly on ThinkPads, HiDPI, suspend/resume, all just work
- **Image Builder (osbuild/composer-cli)** — native Fedora ISO/qcow2 tooling, already prototyped in `fedora-image/`
- **DNF + rpm** — stable package management with Fedora's release cadence
- **Kickstart files** — declarative, reproducible installs (Fedora's native "preseed")
- **Silverblue/Kinoite option** — immutable OS variants with automatic updates
- **Fish shell** — already the project's shell of choice, trivially included
- **NetworkManager** — same `nmcli` backend the TUI already uses

---

## Existing Fedora Work in This Repo

### `fedora-image/blueprint.toml` (early prototype)
A `composer-cli` blueprint that builds a qcow2 with GNOME, GDM, git, htop, and an `me` user. Successfully built (compose `6f25eac4`). This is the seed — FedAC would evolve this into a full bootable image.

### `reports/thinkpad-ac-kiosk-report.md`
Comprehensive analysis from 2026-01-23 that already recommends Fedora + kiosk as the "fast path" over FFOS for ThinkPads. Covers three options (Fedora+Electron, minimal+kiosk session, browser kiosk) and a 5-phase FFOS patch plan. FedAC is essentially executing on Option A/B from this report.

### ThinkPad setup scripts
- `utilities/thinkpad.sh` / `utilities/thinkpad.fish` — existing Fedora dependency install
- `writing/thinkpad.txt` — installation checklist
- `.devcontainer/config.fish` — the `ac-ff1` function and broader dev environment

---

## FedAC Architecture

### Core Idea
A Fedora Workstation (or Kinoite) image that auto-logs in and boots directly into Aesthetic Computer, with a TUI escape hatch for configuration. No FF1 daemons. No Arch. No Docker build.

### Layer Cake

```
┌─────────────────────────────────────────┐
│  Aesthetic Computer (browser or Electron)│  ← kiosk session
├─────────────────────────────────────────┤
│  ac-setup TUI (Python curses)           │  ← Ctrl+Alt+F2 escape
├─────────────────────────────────────────┤
│  systemd user services                  │  ← auto-start, watchdog, restart
├─────────────────────────────────────────┤
│  Wayland (GNOME or cage)                │  ← native compositor
├─────────────────────────────────────────┤
│  Fedora 43 (Workstation or Kinoite)     │  ← base OS
├─────────────────────────────────────────┤
│  Linux kernel + ThinkPad drivers        │  ← just works on Fedora
└─────────────────────────────────────────┘
```

### Build Methods (pick one or both)

#### A. Kickstart + Anaconda (recommended to start)
A `.ks` kickstart file that automates Fedora installation:
- Partitioning, user creation, package selection
- Post-install script installs AC kiosk components
- Can be served over HTTP for PXE or baked into a custom ISO via `lorax`
- Reproducible, version-controlled, easy to iterate

#### B. Image Builder (osbuild / composer-cli)
Evolve the existing `fedora-image/blueprint.toml`:
- Produces qcow2, ISO, or raw disk images
- More declarative than kickstart
- Better for immutable/appliance-style images
- Already prototyped in this repo

#### C. Fedora Kinoite + rpm-ostree (future)
Immutable OS with atomic updates:
- Base image + layered packages
- Auto-updates that can't break the system
- Best for deployed kiosks that need zero-touch maintenance
- Higher learning curve, but strongest production story

### What Goes in `/fedac`

```
fedac/
├── README.md                           # quickstart + philosophy
├── kickstart/
│   ├── fedac-thinkpad.ks               # kickstart for ThinkPad installs
│   └── fedac-generic.ks                # kickstart for any x86_64
├── blueprint/
│   └── fedac.toml                      # osbuild blueprint (evolved from fedora-image/)
├── plymouth/
│   ├── fedac.plymouth                  # Plymouth theme config
│   ├── fedac.script                    # boot animation script
│   ├── pals.png                        # boot splash image (from assets/direct/pals.png)
│   └── watermark.png                   # "aesthetic computer" text for bottom of splash
├── overlays/
│   ├── ac-setup/
│   │   └── ac-setup.py                 # TUI (ported from ffos-build, adapted for Fedora)
│   ├── ac-electron-kiosk/
│   │   ├── ac-electron-kiosk.service   # systemd user service (launches Electron --kiosk)
│   │   ├── ac-electron-kiosk.desktop   # GNOME autostart entry
│   │   └── install-electron.sh         # fetches latest AppImage or installs rpm
│   ├── ac-config-server/
│   │   ├── ac-config-server.py         # WiFi/piece config HTTP server (from ffos-build)
│   │   └── ac-config-server.service    # systemd service
│   └── offline-pieces/
│       └── bundle.sh                   # downloads offline piece HTML bundles
├── scripts/
│   ├── install.sh                      # post-install setup (run after Fedora install)
│   ├── build-iso.sh                    # build custom ISO via lorax/pungi
│   └── flash-usb.sh                    # write ISO to USB with verification + safety
├── systemd/
│   ├── ac-electron-kiosk.service       # user service: Electron fullscreen
│   ├── ac-setup-tty.service            # auto-run TUI on tty2
│   └── ac-autologin.conf              # GDM autologin drop-in
└── .github/
    └── workflows/
        └── fedac-build.yml             # GitHub Actions: build ISO + Electron rpm, upload
```

---

## What to Port from FFOS

### Port directly (with Fedora adaptations)
| FFOS component | FedAC equivalent | Changes needed |
|---|---|---|
| `ac-setup.py` (TUI) | `overlays/ac-setup/ac-setup.py` | Remove Arch paths, use Fedora package names, keep matrix rain |
| `aesthetic-kiosk.service` | `systemd/ac-kiosk.service` | Simplify GPU detection (Wayland handles it), remove `cage` fallback |
| `start-aesthetic-kiosk.sh` | `overlays/ac-kiosk/start-ac-kiosk.sh` | Use native Wayland, simplify to `chromium --kiosk --ozone-platform=wayland` |
| `ac-config-server.py` | `overlays/ac-config-server/` | Port as-is, it's Python + nmcli (already Fedora-compatible) |
| Offline piece bundling | `overlays/offline-pieces/bundle.sh` | Same `curl` from `/api/bundle-html`, different install path |
| `verify-iso.sh` | `scripts/flash-usb.sh` | Same SHA256 + SquashFS checks, adapted for Fedora ISO structure |

### Drop entirely
| FFOS component | Why |
|---|---|
| `feral-controld` | FF1-specific daemon, no laptop equivalent |
| `feral-setupd` | FF1 hardware provisioning |
| `feral-sys-monitord` | FF1 system monitor, replaced by standard Fedora tools |
| `feral-watchdog` | Replaced by systemd `Restart=always` |
| `launcher-ui` | FF1 QR-code launcher, not needed for laptops |
| `player-wrapper-ui` | FF1 player chrome, not needed |
| Docker + mkarchiso | Replaced by Fedora-native kickstart/osbuild |

### New for FedAC
| Component | Purpose |
|---|---|
| Kickstart file | Declarative, reproducible Fedora install |
| GDM autologin config | Boot straight to kiosk user |
| GNOME autostart `.desktop` | Launch AC on login without custom session |
| Fish shell setup | Install fish + project config automatically |
| Electron support (optional) | AC Electron app as alternative to browser kiosk |

---

## Kiosk Mode: Electron (Day 1)

**Decision:** Go straight to Electron kiosk. The AC Electron app (`ac-electron/`) already builds Linux targets (AppImage, deb, rpm) via electron-builder and has the flip view (webview front + terminal back), auto-updater, and FF1 bridge built in. No reason to start with a bare browser kiosk.

### Boot Sequence
```
Plymouth (pals boot splash)
  → GDM autologin (no password, no lock screen)
    → GNOME Wayland session
      → autostart .desktop
        → AC Electron (fullscreen/kiosk)
```

### What Electron Gives Us Over Browser Kiosk
- **Flip view** — Cmd/Ctrl+B flips between AC webview and terminal (fish + emacs)
- **Auto-updater** — checks GitHub releases, downloads in background, installs on restart
- **node-pty** — real terminal with xterm.js, not a fake shell
- **FF1 bridge** — local server on port 19999 for kidlisp.com → FF1 device casting
- **Offline capable** — can bundle pieces and serve locally
- **Already builds for Linux** — `npm run build:linux` produces AppImage + deb + rpm

### Electron Build for FedAC
```bash
cd ac-electron
npm run build:linux    # → dist/Aesthetic Computer-*.AppImage, .deb, .rpm
```

The rpm can be installed directly on Fedora:
```bash
sudo dnf install ./dist/aesthetic-computer-*.rpm
```

Or the AppImage can run without installation (just needs `--no-sandbox` or proper FUSE):
```bash
chmod +x ./Aesthetic\ Computer-*.AppImage
./Aesthetic\ Computer-*.AppImage
```

### Kiosk Fullscreen
The Electron app needs a `--kiosk` or `--fullscreen` flag for FedAC mode. The `main.js` already supports `--piece=` and `--dev` flags. Adding `--kiosk` would:
- Start in fullscreen, no title bar (already `frame: false`)
- Hide the system tray (not needed in kiosk)
- Disable `alwaysOnTop` (fullscreen doesn't need it)
- Auto-connect to production URL
- Skip the devcontainer prompt on terminal side (start local fish directly)

---

## Boot Splash: Pals

The "pals" image (two pink outlined figures) is the boot branding for FedAC. It shows during Plymouth (the Linux boot splash before GDM).

### Asset Status
- **pals.png** exists at `system/public/assets/direct/pals.png` and `system/public/assets/aesthetic-inc/pals.png`
- **pals.svg** does not exist yet — needs to be created (trace from PNG or redraw)
- Plymouth themes use PNG, so the existing PNG works. SVG is nice for scaling to arbitrary resolutions.

### Plymouth Theme Structure
```
fedac/plymouth/
├── fedac.plymouth          # theme config
├── fedac.script            # animation script (optional)
├── pals.png                # main boot image (centered on black/purple bg)
├── pals-throbber-*.png     # optional spinner frames
└── watermark.png           # small "aesthetic computer" text (bottom)
```

### Plymouth Config (`fedac.plymouth`)
```ini
[Plymouth Theme]
Name=FedAC
Description=Aesthetic Computer Boot
ModuleName=script

[script]
ImageDir=/usr/share/plymouth/themes/fedac
ScriptFile=/usr/share/plymouth/themes/fedac/fedac.script
```

The boot animation could be:
1. **Simple:** Pals centered on dark purple background, subtle fade-in
2. **Matrix rain:** Port the TUI's matrix rain to Plymouth script language
3. **Pulse:** Pals image gently pulses in brightness

### Installation
```bash
sudo cp -r fedac/plymouth /usr/share/plymouth/themes/fedac
sudo plymouth-set-default-theme fedac
sudo dracut -f   # rebuild initramfs with new theme
```

---

## USB Flasher

A script to create bootable FedAC USB drives from either a pre-built ISO or by writing a live Fedora + kickstart combo.

### Approach A: Flash a Pre-Built ISO (recommended)

For pre-built FedAC ISOs (from GitHub Actions or local `build-iso.sh`):

```bash
# Usage: fedac/scripts/flash-usb.sh <iso-path> <device>
# Example: fedac/scripts/flash-usb.sh fedac-thinkpad.iso /dev/sdb
```

The flasher should:
1. **Verify ISO integrity** — check SHA256 (reuse logic from `utilities/ffos-build/verify-iso.sh`)
2. **Confirm target device** — show device info (model, size), require explicit `--yes` or interactive confirmation
3. **Write with progress** — `dd` with `status=progress` or `pv`
4. **Verify the write** — read back and compare checksum of first N bytes
5. **Safe unmount** — sync + eject when done

### Approach B: Kickstart USB (lighter)

Instead of a full custom ISO, write a standard Fedora live USB and include the kickstart file:
1. Download Fedora Workstation ISO (or use cached copy)
2. Write to USB with `dd`
3. Mount the USB's EFI partition
4. Add kickstart file + modify GRUB to auto-apply it
5. Boot → Anaconda reads kickstart → fully automated install

This is lighter (no custom ISO build) but requires internet during install for packages.

### Flasher Script Skeleton
```bash
#!/bin/bash
# fedac/scripts/flash-usb.sh — Write FedAC ISO to USB
set -euo pipefail

ISO="$1"
DEVICE="${2:-}"

# Safety checks
[[ -f "$ISO" ]] || { echo "ISO not found: $ISO"; exit 1; }
[[ -b "$DEVICE" ]] || { echo "Not a block device: $DEVICE"; exit 1; }
[[ "$DEVICE" != /dev/sda ]] || { echo "Refusing to write to /dev/sda (likely system disk)"; exit 1; }

# Show device info
echo "Target: $DEVICE"
lsblk "$DEVICE"
echo ""
read -p "Write $ISO to $DEVICE? ALL DATA WILL BE LOST. [y/N] " confirm
[[ "$confirm" == "y" ]] || exit 0

# Verify ISO
if [[ -f "${ISO}.sha256" ]]; then
  echo "Verifying checksum..."
  sha256sum -c "${ISO}.sha256" || { echo "CHECKSUM FAILED"; exit 1; }
fi

# Unmount any mounted partitions
umount "${DEVICE}"* 2>/dev/null || true

# Write
echo "Writing..."
sudo dd if="$ISO" of="$DEVICE" bs=4M status=progress oflag=sync

# Verify
echo "Verifying write..."
ISO_SIZE=$(stat -c%s "$ISO")
BLOCKS=$((ISO_SIZE / 4194304))
ISO_HASH=$(sha256sum "$ISO" | cut -d' ' -f1)
DEVICE_HASH=$(sudo dd if="$DEVICE" bs=4M count="$BLOCKS" 2>/dev/null | sha256sum | cut -d' ' -f1)
[[ "$ISO_HASH" == "$DEVICE_HASH" ]] && echo "VERIFIED" || echo "WARNING: Verification mismatch!"

# Eject
sync
sudo eject "$DEVICE" 2>/dev/null || true
echo "Done. Remove USB and boot the target machine."
```

---

## TUI Adaptations for Fedora

The existing `ac-setup.py` is already mostly Fedora-compatible since it uses `nmcli` for WiFi. Changes needed:

1. **Paths**: `/opt/ac/` → `/usr/local/share/ac/` (or keep `/opt/ac/`, both work on Fedora)
2. **Browser**: Chromium → `google-chrome-stable` or `chromium-browser` (Fedora naming)
3. **Service names**: `aesthetic-kiosk` → `ac-kiosk` (cleaner naming)
4. **Offline pieces dir**: Same concept, Fedora path conventions
5. **TTY binding**: Add `ac-setup-tty.service` to auto-start TUI on tty2 (press Ctrl+Alt+F2 from kiosk to configure)

---

## GitHub Actions Build

```yaml
# .github/workflows/fedac-build.yml (sketch)
name: FedAC ISO Build
on:
  push:
    paths: ['fedac/**']
  workflow_dispatch:

jobs:
  build:
    runs-on: ubuntu-latest  # or fedora container
    container: fedora:43
    steps:
      - uses: actions/checkout@v4
      - name: Install build tools
        run: dnf install -y lorax pungi anaconda
      - name: Build custom ISO
        run: bash fedac/scripts/build-iso.sh
      - name: Upload ISO
        uses: actions/upload-artifact@v4
        with:
          name: fedac-iso
          path: fedac/out/*.iso
```

Unlike the FFOS pipeline (which needed Docker + Arch + mkarchiso), this runs natively on a Fedora container with standard Fedora tools.

---

## Migration Path

### Phase 1: Kickstart + Manual Install
- Write `fedac-thinkpad.ks` with all packages, user config, and post-install script
- Test on a ThinkPad: boot Fedora live USB → `inst.ks=https://...` → automated install
- Port `ac-setup.py` and `ac-kiosk.service` to Fedora paths
- Validate: boots to AC kiosk, Ctrl+Alt+F2 opens TUI, WiFi works

### Phase 2: Automated Image Build
- Set up GitHub Actions to build custom ISO from kickstart
- Add offline piece bundling to the build
- Produce downloadable ISOs per release
- Flash to USB → boot ThinkPad → AC running in < 5 minutes

### Phase 3: Kinoite / Immutable (production)
- Move to Fedora Kinoite base for deployed kiosks
- Automatic OTA updates via rpm-ostree
- Rollback on failure
- Zero-touch maintenance for gallery/education deployments

### Phase 4: Multi-Device Support
- Generic x86_64 kickstart (not just ThinkPad)
- Raspberry Pi / ARM support via Fedora ARM images
- Device-specific overlays (ThinkPad power, Pi display config, etc.)

---

## Comparison: FFOS vs FedAC

| Aspect | FFOS (current) | FedAC (proposed) |
|---|---|---|
| Base OS | Arch Linux | Fedora 43 |
| Build tool | Docker + mkarchiso | Kickstart + lorax (or osbuild) |
| Target hardware | FF1 art computer | ThinkPads, any x86_64 |
| Wayland support | Via `cage` fallback | Native GNOME Wayland |
| ThinkPad compatibility | Poor (Arch driver gaps) | Excellent (Fedora first-class) |
| Update model | Manual rebuild | dnf/rpm-ostree (automatic possible) |
| TUI | `ac-setup.py` (port) | `ac-setup.py` (adapted) |
| Kiosk service | `aesthetic-kiosk.service` | `ac-kiosk.service` |
| CI/CD | Removed (was GitHub Actions) | GitHub Actions (Fedora-native) |
| Offline support | 3 bundled pieces | Same, extensible |
| Maintenance burden | High (Arch + Docker + FF1 deps) | Low (standard Fedora) |
| HiDPI | Manual flags | Automatic (GNOME Wayland) |
| Suspend/resume | Untested | Works (Fedora ThinkPad kernel) |
| Build time | ~30min (Docker + Arch) | ~10min (kickstart) or ~20min (ISO) |

---

## Decisions Made

1. **Electron kiosk from day 1** — not browser kiosk. Flip view + terminal + auto-updater are worth it.
2. **Pals as boot branding** — Plymouth splash with pals image on dark purple background.
3. **USB flasher included** — `fedac/scripts/flash-usb.sh` with verification and safety checks.

## Open Questions

1. **GNOME vs minimal compositor?** GNOME is heavier but handles everything (HiDPI, power, lid, WiFi UI). `cage` or `sway` is lighter but requires manual setup for each. Start GNOME, optimize later.

2. **Standard Fedora or Kinoite?** Standard is simpler to start, Kinoite is better for deployed kiosks. Could start standard and graduate.

3. **Keep `fedora-image/` or merge into `fedac/`?** The blueprint is a seed — probably merge it in and remove the old directory.

4. **pals.svg** — Needs to be created (trace or redraw from the PNG). Not blocking — Plymouth uses PNG. SVG is for future use (print, scaling, etc.)

5. **Electron `--kiosk` flag** — Needs to be added to `ac-electron/main.js`. Should it hide the dock/tray entirely, or keep tray for updates?

---

## Files Referenced

| File | What it contains |
|---|---|
| `utilities/ffos-build/` | Full FFOS ISO build pipeline (Arch-based) |
| `utilities/ffos-build/overlays/ac-setup/ac-setup.py` | Matrix rain TUI for WiFi + piece setup |
| `utilities/ffos-build/overlays/ffos-user/.../aesthetic-kiosk.service` | Systemd kiosk service |
| `utilities/ffos-build/overlays/ffos-user/.../start-aesthetic-kiosk.sh` | Kiosk launcher with GPU detection |
| `utilities/ffos-build/overlays/ac-config-server/` | HTTP config server for phone-based setup |
| `fedora-image/blueprint.toml` | Early Fedora osbuild blueprint |
| `fedora-image/notes.txt` | Build session notes |
| `reports/thinkpad-ac-kiosk-report.md` | ThinkPad kiosk analysis + recommendations |
| `utilities/thinkpad.sh` | Fedora ThinkPad dependency install script |
| `ac-electron/` | Electron app (alternative to browser kiosk) |
