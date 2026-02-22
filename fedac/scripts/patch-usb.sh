#!/bin/bash
# patch-usb.sh — Patch a Fedora live USB into a FedAC auto-installer
#
# After flashing a standard Fedora Workstation ISO to USB, run this to:
#   1. Mount the EFI partition
#   2. Copy the FedAC kickstart onto it
#   3. Replace GRUB config with FedAC-branded auto-boot
#
# Usage:
#   sudo bash fedac/scripts/patch-usb.sh /dev/sdX
#
# The USB must already have a Fedora live ISO written to it (via dd or flash-usb.sh).

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
PURPLE='\033[0;35m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FEDAC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

usage() {
  echo "Usage: sudo $0 <device>"
  echo ""
  echo "Example: sudo $0 /dev/sdb"
  echo ""
  echo "The device must be a Fedora live USB (already flashed with dd)."
  exit 1
}

DEVICE="${1:-}"
[ -n "$DEVICE" ] || usage
[ -b "$DEVICE" ] || { echo -e "${RED}Not a block device: $DEVICE${NC}"; exit 1; }

# Must be root
if [ "$(id -u)" -ne 0 ]; then
  echo -e "${RED}Error: Must run as root (sudo)${NC}"
  exit 1
fi

# Safety
case "$DEVICE" in
  /dev/sda|/dev/nvme0n1|/dev/vda)
    echo -e "${RED}REFUSED: $DEVICE looks like a system disk${NC}"
    exit 1
    ;;
esac

# Find the EFI partition (usually partition 2 on Fedora live USB, ~30M FAT)
EFI_PART=""
for part in "${DEVICE}2" "${DEVICE}p2"; do
  if [ -b "$part" ]; then
    EFI_PART="$part"
    break
  fi
done

if [ -z "$EFI_PART" ]; then
  echo -e "${RED}Could not find EFI partition (tried ${DEVICE}2, ${DEVICE}p2)${NC}"
  echo "Partitions on $DEVICE:"
  lsblk "$DEVICE"
  exit 1
fi

echo -e "${CYAN}=== FedAC USB Patcher ===${NC}"
echo -e "Device:    ${YELLOW}$DEVICE${NC}"
echo -e "EFI part:  ${YELLOW}$EFI_PART${NC}"
echo ""

# Mount EFI partition
MOUNT_DIR=$(mktemp -d /tmp/fedac-efi-XXXX)
echo -e "${CYAN}Mounting $EFI_PART...${NC}"
mount "$EFI_PART" "$MOUNT_DIR"

# Verify this looks like a Fedora EFI partition
if [ ! -d "$MOUNT_DIR/EFI/BOOT" ]; then
  echo -e "${RED}No EFI/BOOT directory found — doesn't look like a Fedora live USB${NC}"
  umount "$MOUNT_DIR"
  rmdir "$MOUNT_DIR"
  exit 1
fi

echo -e "${GREEN}EFI partition mounted at $MOUNT_DIR${NC}"
echo ""

# ── 1. Copy kickstart ──
echo -e "${CYAN}[1/3] Copying kickstart...${NC}"
KS_SRC="$FEDAC_DIR/kickstart/fedac-thinkpad.ks"
if [ ! -f "$KS_SRC" ]; then
  echo -e "${RED}Kickstart not found: $KS_SRC${NC}"
  umount "$MOUNT_DIR"
  rmdir "$MOUNT_DIR"
  exit 1
fi
cp "$KS_SRC" "$MOUNT_DIR/fedac.ks"
echo -e "  Copied to ${GREEN}$MOUNT_DIR/fedac.ks${NC}"

# ── 2. Find and read existing GRUB config ──
echo -e "${CYAN}[2/3] Patching GRUB config...${NC}"

# Find the grub.cfg (could be in several places)
GRUB_CFG=""
for candidate in \
  "$MOUNT_DIR/EFI/BOOT/grub.cfg" \
  "$MOUNT_DIR/EFI/fedora/grub.cfg" \
  "$MOUNT_DIR/boot/grub2/grub.cfg"; do
  if [ -f "$candidate" ]; then
    GRUB_CFG="$candidate"
    break
  fi
done

if [ -z "$GRUB_CFG" ]; then
  echo -e "${RED}Could not find grub.cfg on EFI partition${NC}"
  ls -laR "$MOUNT_DIR/"
  umount "$MOUNT_DIR"
  rmdir "$MOUNT_DIR"
  exit 1
fi

echo -e "  Found: ${GREEN}$GRUB_CFG${NC}"

# Back up original
cp "$GRUB_CFG" "${GRUB_CFG}.orig"
echo -e "  Backed up to ${GRUB_CFG}.orig"

# Extract the original linux/initrd lines from the first menuentry
# We need the kernel path, initrd path, and boot params
LINUX_LINE=$(grep -m1 '^\s*linux\|^\s*linuxefi' "$GRUB_CFG" || true)
INITRD_LINE=$(grep -m1 '^\s*initrd\|^\s*initrdefi' "$GRUB_CFG" || true)

if [ -z "$LINUX_LINE" ]; then
  echo -e "${YELLOW}Warning: Could not find linux/linuxefi line in grub.cfg${NC}"
  echo -e "${YELLOW}Writing generic FedAC grub.cfg...${NC}"
  # Generic fallback
  LINUX_CMD="linuxefi"
  LINUX_ARGS="/images/pxeboot/vmlinuz root=live:CDLABEL=Fedora-WS-Live-43-1-6 rd.live.image quiet"
  INITRD_CMD="initrdefi"
  INITRD_ARGS="/images/pxeboot/initrd.img"
else
  # Parse the existing line
  LINUX_CMD=$(echo "$LINUX_LINE" | awk '{print $1}')
  # Get everything after the command (kernel path + args)
  LINUX_ARGS=$(echo "$LINUX_LINE" | sed "s/^[[:space:]]*${LINUX_CMD}[[:space:]]*//")
  INITRD_CMD=$(echo "$INITRD_LINE" | awk '{print $1}')
  INITRD_ARGS=$(echo "$INITRD_LINE" | sed "s/^[[:space:]]*${INITRD_CMD}[[:space:]]*//")
fi

echo -e "  Kernel: $LINUX_CMD $LINUX_ARGS"

# ── 3. Write new GRUB config ──
echo -e "${CYAN}[3/3] Writing FedAC GRUB config...${NC}"

cat > "$GRUB_CFG" << GRUBEOF
# FedAC — Fedora Boot-to-Aesthetic-Computer
# Auto-patched by fedac/scripts/patch-usb.sh

set default=0
set timeout=3

# Purple theme
set color_normal=magenta/black
set color_highlight=white/magenta

menuentry "FedAC — Install Aesthetic Computer" {
  $LINUX_CMD $LINUX_ARGS inst.ks=hd:LABEL=EFI\\\\SYSTPART:/fedac.ks
  $INITRD_CMD $INITRD_ARGS
}

menuentry "FedAC — Install (manual, no kickstart)" {
  $LINUX_CMD $LINUX_ARGS
  $INITRD_CMD $INITRD_ARGS
}

menuentry "Boot from local drive" {
  exit
}
GRUBEOF

echo -e "  ${GREEN}GRUB config written${NC}"
echo ""

# ── Done ──
sync
umount "$MOUNT_DIR"
rmdir "$MOUNT_DIR"

echo -e "${GREEN}=== USB patched ===${NC}"
echo ""
echo -e "Boot menu will show:"
echo -e "  ${PURPLE}1. FedAC — Install Aesthetic Computer${NC}  (auto-selected after 3s)"
echo -e "  2. FedAC — Install (manual, no kickstart)"
echo -e "  3. Boot from local drive"
echo ""
echo -e "The kickstart at ${GREEN}/fedac.ks${NC} on the EFI partition will automate the install."
echo -e "Pull the USB and boot the target machine."
