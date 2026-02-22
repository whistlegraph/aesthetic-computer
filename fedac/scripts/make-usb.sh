#!/bin/bash
# make-usb.sh — One-step FedAC USB creator
#
# Downloads Fedora, flashes to USB, patches GRUB with FedAC branding + kickstart.
# One command, done.
#
# Usage:
#   sudo bash fedac/scripts/make-usb.sh /dev/sdX
#
# Options:
#   --iso <path>    Use a local ISO instead of downloading
#   --no-eject      Don't eject when done
#   --yes           Skip confirmation prompts

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
PURPLE='\033[0;35m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FEDAC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Fedora 43 Workstation
FEDORA_VERSION="43"
FEDORA_RELEASE="43-1.6"
FEDORA_ISO_NAME="Fedora-Workstation-Live-${FEDORA_RELEASE}.x86_64.iso"
FEDORA_URL="https://dl.fedoraproject.org/pub/fedora/linux/releases/${FEDORA_VERSION}/Workstation/x86_64/iso/${FEDORA_ISO_NAME}"
FEDORA_SHA256="2a4a16c009244eb5ab2198700eb04103793b62407e8596f30a3e0cc8ac294d77"
CACHE_DIR="${HOME}/Downloads"

usage() {
  echo -e "${PURPLE}FedAC USB Creator${NC}"
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
  /dev/sda|/dev/nvme0n1|/dev/vda|/dev/xvda)
    echo -e "${RED}REFUSED: $DEVICE is likely your system disk.${NC}"
    exit 1
    ;;
esac

echo -e "${PURPLE}╔══════════════════════════════════════╗${NC}"
echo -e "${PURPLE}║       FedAC USB Creator              ║${NC}"
echo -e "${PURPLE}╚══════════════════════════════════════╝${NC}"
echo ""

# ── Step 1: Get the ISO ──
echo -e "${CYAN}[1/5] Getting Fedora ${FEDORA_VERSION} ISO...${NC}"

if [ -n "$ISO_PATH" ]; then
  # User provided an ISO
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

# ── Step 2: Verify checksum ──
echo -e "${CYAN}[2/5] Verifying checksum...${NC}"
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

# ── Step 3: Flash ISO ──
echo -e "${CYAN}[3/5] Flashing ISO to $DEVICE...${NC}"

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

# ── Step 4: Patch EFI partition ──
echo -e "${CYAN}[4/5] Patching EFI partition with FedAC boot...${NC}"

# Find EFI partition
EFI_PART=""
for part in "${DEVICE}2" "${DEVICE}p2"; do
  if [ -b "$part" ]; then
    EFI_PART="$part"
    break
  fi
done

if [ -z "$EFI_PART" ]; then
  echo -e "${YELLOW}Could not find EFI partition — skipping GRUB patch${NC}"
  echo "You'll see the default Fedora boot menu."
  echo "At GRUB, press 'e' and add: inst.ks=https://..."
else
  MOUNT_DIR=$(mktemp -d /tmp/fedac-efi-XXXX)
  mount "$EFI_PART" "$MOUNT_DIR"

  # Copy kickstart
  KS_SRC="$FEDAC_DIR/kickstart/fedac-thinkpad.ks"
  if [ -f "$KS_SRC" ]; then
    cp "$KS_SRC" "$MOUNT_DIR/fedac.ks"
    echo -e "  Kickstart copied to EFI partition"
  else
    echo -e "  ${YELLOW}Warning: $KS_SRC not found — kickstart not included${NC}"
  fi

  # ── Install GRUB theme ──
  THEME_SRC="$FEDAC_DIR/grub-theme"
  THEME_DST="$MOUNT_DIR/EFI/BOOT/fedac-theme"
  mkdir -p "$THEME_DST"

  # Copy theme assets
  NEED_GENERATE=false
  for asset in theme.txt background.png DejaVuSansBold36.pf2 DejaVuSans18.pf2 DejaVuSans10.pf2 select_c.png menu_c.png; do
    if [ -f "$THEME_SRC/$asset" ]; then
      cp "$THEME_SRC/$asset" "$THEME_DST/"
    elif [ "$asset" != "theme.txt" ]; then
      NEED_GENERATE=true
    fi
  done

  if [ "$NEED_GENERATE" = true ] && [ -x "$THEME_SRC/prepare-theme.sh" ]; then
    echo -e "  Generating theme assets..."
    bash "$THEME_SRC/prepare-theme.sh" 2>/dev/null || echo -e "  ${YELLOW}Theme generation had warnings${NC}"
    for asset in background.png DejaVuSansBold36.pf2 DejaVuSans18.pf2 DejaVuSans10.pf2 select_c.png menu_c.png; do
      [ -f "$THEME_SRC/$asset" ] && cp "$THEME_SRC/$asset" "$THEME_DST/"
    done
  fi
  echo -e "  ${GREEN}Theme installed${NC}"

  # ── Patch GRUB config ──
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

  if [ -n "$GRUB_CFG" ]; then
    cp "$GRUB_CFG" "${GRUB_CFG}.orig"
    CDLABEL="Fedora-WS-Live-${FEDORA_VERSION}"

    cat > "$GRUB_CFG" << 'GRUBEOF'
# FedAC — Fedora Boot-to-Aesthetic-Computer
# Auto-generated by fedac/scripts/make-usb.sh

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

    sed -i "s/FEDAC_CDLABEL/${CDLABEL}/g" "$GRUB_CFG"
    echo -e "  ${GREEN}GRUB patched — FedAC graphical boot menu installed${NC}"
  else
    echo -e "  ${YELLOW}No grub.cfg found on EFI — skipping GRUB patch${NC}"
  fi

  sync
  umount "$MOUNT_DIR"
  rmdir "$MOUNT_DIR"
fi

# ── Step 5: Done ──
echo -e "${CYAN}[5/5] Finalizing...${NC}"
sync

if [ "$DO_EJECT" = true ]; then
  eject "$DEVICE" 2>/dev/null || true
  echo -e "  ${GREEN}Ejected${NC}"
fi

echo ""
echo -e "${GREEN}╔══════════════════════════════════════╗${NC}"
echo -e "${GREEN}║       FedAC USB Ready                ║${NC}"
echo -e "${GREEN}╚══════════════════════════════════════╝${NC}"
echo ""
echo -e "Boot menu (pals logo + large text, auto-starts in 5s):"
echo -e "  ${PURPLE}1. FedAC — Install Aesthetic Computer${NC}"
echo -e "  2. FedAC — Live Desktop (no install)"
echo -e "  3. Boot from local drive"
echo ""
echo "Plug into target machine → boot from USB → done."
