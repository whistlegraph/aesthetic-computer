#!/bin/bash
# make-kiosk-usb.sh — Create a FedAC live kiosk USB
#
# Boots Fedora 43 straight into Firefox kiosk mode at kidlisp.com.
# No install, no GRUB menu, no desktop access. Plug in, boot, see kidlisp.com fullscreen.
#
# How it works:
#   1. Flash Fedora Workstation Live ISO to USB
#   2. Extract squashfs rootfs from live partition
#   3. Inject kiosk autostart + dconf settings into rootfs
#   4. Repack squashfs, replace on USB
#   5. Patch EFI partition: instant-boot GRUB + theme
#
# Usage:
#   sudo bash fedac/scripts/make-kiosk-usb.sh /dev/sdX
#
# Options:
#   --iso <path>    Use a local ISO instead of downloading
#   --no-eject      Don't eject when done
#   --yes           Skip confirmation prompts
#
# Requirements:
#   squashfs-tools (unsquashfs, mksquashfs)
#   ~5GB free temp space

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
PURPLE='\033[0;35m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FEDAC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
OVERLAY_DIR="$FEDAC_DIR/overlays/kiosk"

# Fedora 43 Workstation
FEDORA_VERSION="43"
FEDORA_RELEASE="43-1.6"
FEDORA_ISO_NAME="Fedora-Workstation-Live-${FEDORA_RELEASE}.x86_64.iso"
FEDORA_URL="https://dl.fedoraproject.org/pub/fedora/linux/releases/${FEDORA_VERSION}/Workstation/x86_64/iso/${FEDORA_ISO_NAME}"
FEDORA_SHA256="2a4a16c009244eb5ab2198700eb04103793b62407e8596f30a3e0cc8ac294d77"
CDLABEL="Fedora-WS-Live-${FEDORA_VERSION}"
CACHE_DIR="${HOME}/Downloads"

usage() {
  echo -e "${PURPLE}FedAC Kiosk USB Creator${NC}"
  echo ""
  echo "Creates a live-only USB that boots straight into Firefox kiosk at kidlisp.com."
  echo ""
  echo "Usage: sudo $0 <device> [options]"
  echo ""
  echo "  <device>        USB block device (e.g., /dev/sdb)"
  echo ""
  echo "Options:"
  echo "  --iso <path>    Use existing ISO instead of downloading"
  echo "  --no-eject      Don't eject the USB when done"
  echo "  --yes           Skip confirmation prompts"
  echo "  --help          Show this help"
  exit 1
}

cleanup() {
  echo -e "\n${YELLOW}Cleaning up...${NC}"
  # Unmount anything we mounted
  [ -n "${LIVE_MOUNT:-}" ] && mountpoint -q "$LIVE_MOUNT" 2>/dev/null && umount "$LIVE_MOUNT" 2>/dev/null || true
  [ -n "${EFI_MOUNT:-}" ] && mountpoint -q "$EFI_MOUNT" 2>/dev/null && umount "$EFI_MOUNT" 2>/dev/null || true
  # Remove temp dirs (but not the squashfs work dir — it's huge, leave for manual cleanup on error)
  [ -n "${LIVE_MOUNT:-}" ] && [ -d "$LIVE_MOUNT" ] && rmdir "$LIVE_MOUNT" 2>/dev/null || true
  [ -n "${EFI_MOUNT:-}" ] && [ -d "$EFI_MOUNT" ] && rmdir "$EFI_MOUNT" 2>/dev/null || true
}
trap cleanup EXIT

# ── Parse args ──
DEVICE=""
ISO_PATH=""
DO_EJECT=true
SKIP_CONFIRM=false

while [ $# -gt 0 ]; do
  case "$1" in
    --iso) ISO_PATH="$2"; shift 2 ;;
    --no-eject) DO_EJECT=false; shift ;;
    --yes) SKIP_CONFIRM=true; shift ;;
    --help|-h) usage ;;
    /dev/*) DEVICE="$1"; shift ;;
    *) echo -e "${RED}Unknown arg: $1${NC}"; usage ;;
  esac
done

[ -n "$DEVICE" ] || { echo -e "${RED}Error: No device specified${NC}"; usage; }
[ -b "$DEVICE" ] || { echo -e "${RED}Error: $DEVICE is not a block device${NC}"; exit 1; }

# Must be root
if [ "$(id -u)" -ne 0 ]; then
  echo -e "${RED}Error: Must run as root (sudo)${NC}"
  exit 1
fi

# Safety: refuse system disks
case "$DEVICE" in
  /dev/nvme0n1|/dev/vda|/dev/xvda)
    echo -e "${RED}REFUSED: $DEVICE is likely your system disk.${NC}"
    exit 1
    ;;
esac

# Check for squashfs-tools
if ! command -v unsquashfs &>/dev/null || ! command -v mksquashfs &>/dev/null; then
  echo -e "${RED}Error: squashfs-tools not installed${NC}"
  echo "Install with: sudo dnf install squashfs-tools"
  exit 1
fi

echo -e "${PURPLE}╔══════════════════════════════════════════╗${NC}"
echo -e "${PURPLE}║       FedAC Kiosk USB Creator            ║${NC}"
echo -e "${PURPLE}║  Live boot → Firefox → kidlisp.com       ║${NC}"
echo -e "${PURPLE}╚══════════════════════════════════════════╝${NC}"
echo ""

# ══════════════════════════════════════════
# Step 1: Get the ISO
# ══════════════════════════════════════════
echo -e "${CYAN}[1/7] Getting Fedora ${FEDORA_VERSION} ISO...${NC}"

if [ -n "$ISO_PATH" ]; then
  if [ ! -f "$ISO_PATH" ]; then
    echo -e "${RED}ISO not found: $ISO_PATH${NC}"
    exit 1
  fi
  echo -e "  Using: ${GREEN}$ISO_PATH${NC}"
else
  ISO_PATH="${CACHE_DIR}/${FEDORA_ISO_NAME}"
  if [ -f "$ISO_PATH" ]; then
    echo -e "  Found cached: ${GREEN}$ISO_PATH${NC}"
  else
    echo -e "  Downloading ${FEDORA_ISO_NAME} (~2.6 GB)..."
    mkdir -p "$CACHE_DIR"
    curl -L --progress-bar -o "$ISO_PATH" "$FEDORA_URL"
    echo -e "  ${GREEN}Downloaded${NC}"
  fi
fi

# ══════════════════════════════════════════
# Step 2: Verify checksum
# ══════════════════════════════════════════
echo -e "${CYAN}[2/7] Verifying checksum...${NC}"
ACTUAL_SHA=$(sha256sum "$ISO_PATH" | cut -d' ' -f1)
if [ "$ACTUAL_SHA" = "$FEDORA_SHA256" ]; then
  echo -e "  ${GREEN}SHA256 OK${NC}"
else
  echo -e "  ${YELLOW}WARNING: Checksum mismatch${NC}"
  echo "  Expected: $FEDORA_SHA256"
  echo "  Got:      $ACTUAL_SHA"
  echo "  (Continuing — may be a different Fedora build)"
fi

# ── Confirm ──
echo ""
echo -e "Target: ${YELLOW}$DEVICE${NC}"
lsblk "$DEVICE" 2>/dev/null || true
echo ""

if [ "$SKIP_CONFIRM" = false ]; then
  echo -e "${RED}ALL DATA ON $DEVICE WILL BE DESTROYED${NC}"
  read -p "Continue? [y/N] " confirm
  if [ "$confirm" != "y" ] && [ "$confirm" != "Y" ]; then
    echo "Aborted."
    exit 0
  fi
fi

# ══════════════════════════════════════════
# Step 3: Flash ISO to USB
# ══════════════════════════════════════════
echo -e "${CYAN}[3/7] Flashing ISO to $DEVICE...${NC}"

# Unmount any mounted partitions
for part in "${DEVICE}"*; do
  umount "$part" 2>/dev/null || true
done

dd if="$ISO_PATH" of="$DEVICE" bs=4M status=progress oflag=sync 2>&1
sync
echo -e "  ${GREEN}Flash complete${NC}"

# Wait for kernel to re-read partition table
echo -e "  Waiting for partitions..."
sleep 2
partprobe "$DEVICE" 2>/dev/null || true
sleep 2

# ══════════════════════════════════════════
# Step 4: Inject kiosk config into squashfs
# ══════════════════════════════════════════
echo -e "${CYAN}[4/7] Injecting kiosk config into squashfs rootfs...${NC}"

# Find and mount the live partition (partition 3 on Fedora live USB — the large ISO9660/ext4)
LIVE_PART=""
for part in "${DEVICE}3" "${DEVICE}p3" "${DEVICE}1" "${DEVICE}p1"; do
  if [ -b "$part" ]; then
    # Check if this partition has LiveOS/squashfs.img
    LIVE_MOUNT=$(mktemp -d /tmp/fedac-live-XXXX)
    if mount -o ro "$part" "$LIVE_MOUNT" 2>/dev/null; then
      if [ -f "$LIVE_MOUNT/LiveOS/squashfs.img" ]; then
        LIVE_PART="$part"
        echo -e "  Found squashfs on ${GREEN}$part${NC}"
        break
      fi
      umount "$LIVE_MOUNT"
    fi
    rmdir "$LIVE_MOUNT" 2>/dev/null || true
    LIVE_MOUNT=""
  fi
done

if [ -z "$LIVE_PART" ]; then
  echo -e "${RED}Could not find LiveOS/squashfs.img on any partition${NC}"
  echo "Partitions on $DEVICE:"
  lsblk "$DEVICE"
  exit 1
fi

# Set up work directory for squashfs manipulation
WORK_DIR=$(mktemp -d /tmp/fedac-squash-XXXX)
echo -e "  Work dir: $WORK_DIR"

# Copy squashfs.img to work dir (need to modify it)
echo -e "  Copying squashfs.img to work dir..."
cp "$LIVE_MOUNT/LiveOS/squashfs.img" "$WORK_DIR/squashfs.img"

# Unmount live partition (we'll remount rw later)
umount "$LIVE_MOUNT"

# Extract squashfs
echo -e "  Extracting squashfs (this takes a minute)..."
cd "$WORK_DIR"
unsquashfs -d squashfs-root squashfs.img

# The squashfs contains LiveOS/rootfs.img (an ext4 image)
ROOTFS_IMG="$WORK_DIR/squashfs-root/LiveOS/rootfs.img"
if [ ! -f "$ROOTFS_IMG" ]; then
  echo -e "${RED}No LiveOS/rootfs.img inside squashfs${NC}"
  ls -la "$WORK_DIR/squashfs-root/" "$WORK_DIR/squashfs-root/LiveOS/" 2>/dev/null || true
  exit 1
fi

# Mount rootfs.img (ext4) read-write
ROOTFS_MOUNT=$(mktemp -d /tmp/fedac-rootfs-XXXX)
mount -o loop "$ROOTFS_IMG" "$ROOTFS_MOUNT"
echo -e "  ${GREEN}Rootfs mounted${NC}"

# ── Inject kiosk files ──

# 1. Firefox kiosk autostart for liveuser
AUTOSTART_DIR="$ROOTFS_MOUNT/home/liveuser/.config/autostart"
mkdir -p "$AUTOSTART_DIR"
cp "$OVERLAY_DIR/firefox-kiosk.desktop" "$AUTOSTART_DIR/"
# Set ownership (liveuser is UID 1000 on Fedora live)
chown -R 1000:1000 "$ROOTFS_MOUNT/home/liveuser/.config"
echo -e "  ${GREEN}Injected firefox-kiosk.desktop${NC}"

# 2. dconf settings (disable idle, screensaver, lock, notifications)
mkdir -p "$ROOTFS_MOUNT/etc/dconf/db/local.d"
mkdir -p "$ROOTFS_MOUNT/etc/dconf/profile"
cp "$OVERLAY_DIR/00-kiosk" "$ROOTFS_MOUNT/etc/dconf/db/local.d/00-kiosk"
cp "$OVERLAY_DIR/dconf-profile-user" "$ROOTFS_MOUNT/etc/dconf/profile/user"
echo -e "  ${GREEN}Injected dconf kiosk settings${NC}"

# 3. Compile dconf database (run dconf update inside the rootfs via chroot)
# We need to compile the local dconf db so GNOME picks it up
if [ -x "$ROOTFS_MOUNT/usr/bin/dconf" ]; then
  # Use a minimal chroot to run dconf compile
  chroot "$ROOTFS_MOUNT" /usr/bin/dconf compile /etc/dconf/db/local /etc/dconf/db/local.d 2>/dev/null || true
  echo -e "  ${GREEN}Compiled dconf database${NC}"
else
  # Fallback: just place the keyfiles, GNOME will compile at runtime
  echo -e "  ${YELLOW}dconf binary not in rootfs, will compile at boot${NC}"
fi

# 4. Disable GNOME initial setup (it would block kiosk boot)
mkdir -p "$ROOTFS_MOUNT/home/liveuser/.config"
echo "yes" > "$ROOTFS_MOUNT/home/liveuser/.config/gnome-initial-setup-done"
chown 1000:1000 "$ROOTFS_MOUNT/home/liveuser/.config/gnome-initial-setup-done"
echo -e "  ${GREEN}Disabled GNOME initial setup${NC}"

# 5. Create a systemd service to disable screen blanking at runtime
cat > "$ROOTFS_MOUNT/etc/systemd/system/disable-blanking.service" << 'SVCEOF'
[Unit]
Description=Disable screen blanking for kiosk
After=graphical.target

[Service]
Type=oneshot
ExecStart=/usr/bin/bash -c 'DISPLAY=:0 xset s off; DISPLAY=:0 xset -dpms; DISPLAY=:0 xset s noblank'
ExecStart=/usr/bin/bash -c 'dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 org.freedesktop.login1.Manager.SetWallMessage string:"" boolean:false 2>/dev/null || true'

[Install]
WantedBy=graphical.target
SVCEOF

# Enable it via symlink
mkdir -p "$ROOTFS_MOUNT/etc/systemd/system/graphical.target.wants"
ln -sf /etc/systemd/system/disable-blanking.service "$ROOTFS_MOUNT/etc/systemd/system/graphical.target.wants/disable-blanking.service"
echo -e "  ${GREEN}Created disable-blanking service${NC}"

# ── Unmount rootfs and repack squashfs ──
echo -e "  Unmounting rootfs..."
umount "$ROOTFS_MOUNT"
rmdir "$ROOTFS_MOUNT"

echo -e "  Repacking squashfs (this takes a few minutes)..."
rm -f "$WORK_DIR/squashfs-new.img"
mksquashfs "$WORK_DIR/squashfs-root" "$WORK_DIR/squashfs-new.img" -comp xz -b 1M -Xdict-size 100%
echo -e "  ${GREEN}Squashfs repacked${NC}"

# ══════════════════════════════════════════
# Step 5: Replace squashfs on USB
# ══════════════════════════════════════════
echo -e "${CYAN}[5/7] Replacing squashfs on USB...${NC}"

# Remount live partition read-write
LIVE_MOUNT=$(mktemp -d /tmp/fedac-live-XXXX)
mount "$LIVE_PART" "$LIVE_MOUNT"

# Check space
NEW_SIZE=$(stat -c%s "$WORK_DIR/squashfs-new.img")
OLD_SIZE=$(stat -c%s "$LIVE_MOUNT/LiveOS/squashfs.img")
echo -e "  Old squashfs: $(numfmt --to=iec $OLD_SIZE)"
echo -e "  New squashfs: $(numfmt --to=iec $NEW_SIZE)"

# Replace
cp "$WORK_DIR/squashfs-new.img" "$LIVE_MOUNT/LiveOS/squashfs.img"
sync
echo -e "  ${GREEN}Squashfs replaced on USB${NC}"

umount "$LIVE_MOUNT"
rmdir "$LIVE_MOUNT"
LIVE_MOUNT=""

# Clean up work dir
echo -e "  Cleaning up work dir..."
rm -rf "$WORK_DIR"

# ══════════════════════════════════════════
# Step 6: Patch EFI partition
# ══════════════════════════════════════════
echo -e "${CYAN}[6/7] Patching EFI partition...${NC}"

EFI_PART=""
for part in "${DEVICE}2" "${DEVICE}p2"; do
  if [ -b "$part" ]; then
    EFI_PART="$part"
    break
  fi
done

if [ -z "$EFI_PART" ]; then
  echo -e "${YELLOW}Could not find EFI partition — skipping GRUB patch${NC}"
else
  EFI_MOUNT=$(mktemp -d /tmp/fedac-efi-XXXX)
  mount "$EFI_PART" "$EFI_MOUNT"

  # Install GRUB theme
  THEME_SRC="$FEDAC_DIR/grub-theme"
  THEME_DST="$EFI_MOUNT/EFI/BOOT/fedac-theme"
  mkdir -p "$THEME_DST"

  for asset in theme.txt background.png DejaVuSansBold36.pf2 DejaVuSans18.pf2 DejaVuSans10.pf2 select_c.png menu_c.png; do
    if [ -f "$THEME_SRC/$asset" ]; then
      cp "$THEME_SRC/$asset" "$THEME_DST/"
    fi
  done

  # Generate theme assets if missing
  NEED_GENERATE=false
  for asset in background.png DejaVuSansBold36.pf2 DejaVuSans18.pf2 DejaVuSans10.pf2; do
    [ ! -f "$THEME_DST/$asset" ] && NEED_GENERATE=true
  done

  if [ "$NEED_GENERATE" = true ] && [ -x "$THEME_SRC/prepare-theme.sh" ]; then
    echo -e "  Generating theme assets..."
    bash "$THEME_SRC/prepare-theme.sh" 2>/dev/null || true
    for asset in background.png DejaVuSansBold36.pf2 DejaVuSans18.pf2 DejaVuSans10.pf2 select_c.png menu_c.png; do
      [ -f "$THEME_SRC/$asset" ] && cp "$THEME_SRC/$asset" "$THEME_DST/"
    done
  fi
  echo -e "  ${GREEN}Theme installed${NC}"

  # Write kiosk GRUB config — instant boot, no menu choices
  GRUB_CFG=""
  for candidate in \
    "$EFI_MOUNT/EFI/BOOT/grub.cfg" \
    "$EFI_MOUNT/EFI/fedora/grub.cfg" \
    "$EFI_MOUNT/boot/grub2/grub.cfg"; do
    if [ -f "$candidate" ]; then
      GRUB_CFG="$candidate"
      break
    fi
  done

  if [ -n "$GRUB_CFG" ]; then
    cp "$GRUB_CFG" "${GRUB_CFG}.orig"

    cat > "$GRUB_CFG" << 'GRUBEOF'
# FedAC Kiosk — Live boot to Firefox fullscreen
# Auto-generated by fedac/scripts/make-kiosk-usb.sh

set default=0
set timeout=0

# ── Graphics modules ──
if [ "$grub_platform" == "efi" ]; then
  insmod efi_gop
  insmod efi_uga
fi
insmod all_video
insmod gzio
insmod part_gpt
insmod ext2
insmod png
insmod font

# ── Save EFI partition root before search changes it ──
set efi_root=$root

# ── Load fonts from EFI partition ──
loadfont ($efi_root)/EFI/BOOT/fedac-theme/DejaVuSansBold36.pf2
loadfont ($efi_root)/EFI/BOOT/fedac-theme/DejaVuSans18.pf2
loadfont ($efi_root)/EFI/BOOT/fedac-theme/DejaVuSans10.pf2

# ── Switch to graphical terminal ──
set gfxmode=auto
set gfxpayload=keep
terminal_input console
terminal_output gfxterm

# ── Load theme from EFI partition ──
if [ -f ($efi_root)/EFI/BOOT/fedac-theme/theme.txt ]; then
  set theme=($efi_root)/EFI/BOOT/fedac-theme/theme.txt
else
  set color_normal=magenta/black
  set color_highlight=white/magenta
fi

# ── Find Fedora live partition (changes $root) ──
search --file --set=root /boot/0x4da30161

menuentry "FedAC Kiosk" --class fedora {
  linux ($root)/boot/x86_64/loader/linux quiet rhgb root=live:CDLABEL=FEDAC_CDLABEL rd.live.image
  initrd ($root)/boot/x86_64/loader/initrd
}
GRUBEOF

    sed -i "s/FEDAC_CDLABEL/${CDLABEL}/g" "$GRUB_CFG"
    echo -e "  ${GREEN}GRUB patched — instant kiosk boot (0s timeout)${NC}"
  else
    echo -e "  ${YELLOW}No grub.cfg found on EFI — skipping${NC}"
  fi

  sync
  umount "$EFI_MOUNT"
  rmdir "$EFI_MOUNT"
  EFI_MOUNT=""
fi

# ══════════════════════════════════════════
# Step 7: Finalize
# ══════════════════════════════════════════
echo -e "${CYAN}[7/7] Finalizing...${NC}"
sync

if [ "$DO_EJECT" = true ]; then
  eject "$DEVICE" 2>/dev/null || true
  echo -e "  ${GREEN}Ejected${NC}"
fi

echo ""
echo -e "${GREEN}╔══════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║       FedAC Kiosk USB Ready              ║${NC}"
echo -e "${GREEN}╚══════════════════════════════════════════╝${NC}"
echo ""
echo -e "Boot flow:"
echo -e "  Power on → GRUB (instant) → Fedora live → GNOME auto-login"
echo -e "  → ${PURPLE}Firefox fullscreen → kidlisp.com${NC}"
echo ""
echo "Plug into target machine → boot from USB → done."
