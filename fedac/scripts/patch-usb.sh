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

# ── 2. Install GRUB theme ──
echo -e "${CYAN}[2/4] Installing GRUB theme...${NC}"

THEME_SRC="$FEDAC_DIR/grub-theme"
THEME_DST="$MOUNT_DIR/EFI/BOOT/fedac-theme"

mkdir -p "$THEME_DST"

# Copy theme.txt (always from repo)
if [ -f "$THEME_SRC/theme.txt" ]; then
  cp "$THEME_SRC/theme.txt" "$THEME_DST/"
  echo -e "  theme.txt"
fi

# Copy generated assets if they exist, otherwise generate on the fly
NEED_GENERATE=false
for asset in background.png DejaVuSansBold36.pf2 DejaVuSans18.pf2 DejaVuSans10.pf2 select_c.png menu_c.png; do
  if [ -f "$THEME_SRC/$asset" ]; then
    cp "$THEME_SRC/$asset" "$THEME_DST/"
    echo -e "  $asset"
  else
    NEED_GENERATE=true
  fi
done

if [ "$NEED_GENERATE" = true ]; then
  echo -e "  ${YELLOW}Some theme assets missing — generating...${NC}"
  if [ -x "$THEME_SRC/prepare-theme.sh" ]; then
    bash "$THEME_SRC/prepare-theme.sh"
    # Re-copy generated files
    for asset in background.png DejaVuSansBold36.pf2 DejaVuSans18.pf2 DejaVuSans10.pf2 select_c.png menu_c.png; do
      if [ -f "$THEME_SRC/$asset" ]; then
        cp "$THEME_SRC/$asset" "$THEME_DST/"
      fi
    done
  else
    echo -e "  ${YELLOW}Warning: prepare-theme.sh not found, theme may be incomplete${NC}"
  fi
fi

echo -e "  ${GREEN}Theme installed to EFI${NC}"

# ── 3. Patch GRUB config ──
echo -e "${CYAN}[3/4] Patching GRUB config...${NC}"

GRUB_CFG="$MOUNT_DIR/EFI/BOOT/grub.cfg"
if [ ! -f "$GRUB_CFG" ]; then
  echo -e "${RED}No EFI/BOOT/grub.cfg found${NC}"
  umount "$MOUNT_DIR"
  rmdir "$MOUNT_DIR"
  exit 1
fi

# Back up original
cp "$GRUB_CFG" "${GRUB_CFG}.orig"
echo -e "  Backed up original grub.cfg"

CDLABEL="Fedora-WS-Live-43"

echo -e "${CYAN}[4/4] Writing FedAC GRUB config...${NC}"

cat > "$GRUB_CFG" << 'GRUBEOF'
# FedAC — Fedora Boot-to-Aesthetic-Computer
# Auto-patched by fedac/scripts/patch-usb.sh

set default=0
set timeout=5

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

# ── Load fonts from EFI partition (before $root changes) ──
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

menuentry "  FedAC — Install Aesthetic Computer" --class fedora {
  linux ($root)/boot/x86_64/loader/linux quiet rhgb root=live:CDLABEL=FEDAC_CDLABEL rd.live.image inst.ks=https://raw.githubusercontent.com/whistlegraph/aesthetic-computer/main/fedac/kickstart/fedac-thinkpad.ks
  initrd ($root)/boot/x86_64/loader/initrd
}

menuentry "  FedAC — Live Desktop (no install)" --class fedora {
  linux ($root)/boot/x86_64/loader/linux quiet rhgb root=live:CDLABEL=FEDAC_CDLABEL rd.live.image
  initrd ($root)/boot/x86_64/loader/initrd
}

menuentry "  Boot from local drive" {
  exit
}
GRUBEOF

# Substitute the CDLABEL placeholder
sed -i "s/FEDAC_CDLABEL/${CDLABEL}/g" "$GRUB_CFG"

echo -e "  ${GREEN}GRUB config written (graphical theme)${NC}"
echo ""

# ── Done ──
sync
umount "$MOUNT_DIR"
rmdir "$MOUNT_DIR"

echo -e "${GREEN}=== USB patched ===${NC}"
echo ""
echo -e "Boot menu will show (with pals logo + large text):"
echo -e "  ${PURPLE}1. FedAC — Install Aesthetic Computer${NC}  (auto-selected after 5s)"
echo -e "  2. FedAC — Live Desktop (no install)"
echo -e "  3. Boot from local drive"
echo ""
echo -e "The kickstart at ${GREEN}/fedac.ks${NC} on the EFI partition will automate the install."
echo -e "Pull the USB and boot the target machine."
