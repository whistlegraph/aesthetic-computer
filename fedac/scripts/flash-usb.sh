#!/bin/bash
# flash-usb.sh — Write a FedAC ISO (or any ISO) to USB with verification
#
# Usage:
#   bash fedac/scripts/flash-usb.sh <iso-path> <device>
#   bash fedac/scripts/flash-usb.sh fedac-thinkpad.iso /dev/sdb
#
# Safety:
#   - Refuses to write to /dev/sda or /dev/nvme0n1 (likely system disks)
#   - Shows device info before writing
#   - Requires explicit confirmation
#   - Verifies write integrity after completion

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

usage() {
  echo "Usage: $0 <iso-path> <device>"
  echo ""
  echo "Examples:"
  echo "  $0 fedac-thinkpad.iso /dev/sdb"
  echo "  $0 Fedora-Workstation-Live-x86_64-41.iso /dev/sdc"
  echo ""
  echo "Options:"
  echo "  --yes    Skip confirmation prompt"
  echo "  --help   Show this help"
  exit 1
}

# Parse args
ISO=""
DEVICE=""
SKIP_CONFIRM=false

for arg in "$@"; do
  case "$arg" in
    --yes) SKIP_CONFIRM=true ;;
    --help|-h) usage ;;
    *)
      if [ -z "$ISO" ]; then
        ISO="$arg"
      elif [ -z "$DEVICE" ]; then
        DEVICE="$arg"
      fi
      ;;
  esac
done

[ -n "$ISO" ] || { echo -e "${RED}Error: No ISO specified${NC}"; usage; }
[ -n "$DEVICE" ] || { echo -e "${RED}Error: No device specified${NC}"; usage; }

# Validate ISO exists
[ -f "$ISO" ] || { echo -e "${RED}Error: ISO not found: $ISO${NC}"; exit 1; }

# Validate device is a block device
[ -b "$DEVICE" ] || { echo -e "${RED}Error: Not a block device: $DEVICE${NC}"; exit 1; }

# Safety: refuse to write to common system disks
DANGEROUS_DEVICES="/dev/sda /dev/nvme0n1 /dev/vda /dev/xvda"
for dangerous in $DANGEROUS_DEVICES; do
  if [ "$DEVICE" = "$dangerous" ]; then
    echo -e "${RED}REFUSED: $DEVICE is likely your system disk.${NC}"
    echo "If you really want to write to this device, use dd manually."
    exit 1
  fi
done

# Show what we're about to do
echo -e "${CYAN}=== FedAC USB Flasher ===${NC}"
echo ""
echo -e "ISO:    ${GREEN}$ISO${NC}"
echo -e "Device: ${YELLOW}$DEVICE${NC}"
echo ""

# Show device details
echo -e "${CYAN}Device info:${NC}"
lsblk "$DEVICE" 2>/dev/null || true
echo ""

# Show ISO size
ISO_SIZE=$(stat -c%s "$ISO" 2>/dev/null || stat -f%z "$ISO" 2>/dev/null)
ISO_SIZE_MB=$((ISO_SIZE / 1048576))
echo -e "ISO size: ${GREEN}${ISO_SIZE_MB} MB${NC}"
echo ""

# Verify ISO checksum if .sha256 file exists
if [ -f "${ISO}.sha256" ]; then
  echo -e "${CYAN}Verifying ISO checksum...${NC}"
  if sha256sum -c "${ISO}.sha256" 2>/dev/null; then
    echo -e "${GREEN}Checksum OK${NC}"
  else
    echo -e "${RED}CHECKSUM FAILED — ISO may be corrupted${NC}"
    echo "Aborting. Re-download the ISO and try again."
    exit 1
  fi
  echo ""
fi

# Confirm
if [ "$SKIP_CONFIRM" = false ]; then
  echo -e "${RED}WARNING: ALL DATA ON $DEVICE WILL BE DESTROYED${NC}"
  read -p "Continue? [y/N] " confirm
  if [ "$confirm" != "y" ] && [ "$confirm" != "Y" ]; then
    echo "Aborted."
    exit 0
  fi
fi

# Unmount any mounted partitions on the device
echo -e "${CYAN}Unmounting partitions...${NC}"
for part in "${DEVICE}"*; do
  umount "$part" 2>/dev/null || true
done

# Write ISO to device
echo -e "${CYAN}Writing ISO to $DEVICE...${NC}"
echo "(This may take several minutes)"
echo ""

if command -v pv &>/dev/null; then
  # Use pv for progress if available
  sudo sh -c "pv '$ISO' | dd of='$DEVICE' bs=4M oflag=sync"
else
  # Fall back to dd with status=progress
  sudo dd if="$ISO" of="$DEVICE" bs=4M status=progress oflag=sync
fi

echo ""
echo -e "${CYAN}Syncing...${NC}"
sync

# Verify the write
echo -e "${CYAN}Verifying write...${NC}"
BLOCKS=$((ISO_SIZE / 4194304))
[ "$BLOCKS" -lt 1 ] && BLOCKS=1

ISO_HASH=$(sha256sum "$ISO" | cut -d' ' -f1)
DEVICE_HASH=$(sudo dd if="$DEVICE" bs=4M count="$BLOCKS" 2>/dev/null | sha256sum | cut -d' ' -f1)

if [ "$ISO_HASH" = "$DEVICE_HASH" ]; then
  echo -e "${GREEN}VERIFIED — write matches ISO${NC}"
else
  echo -e "${YELLOW}WARNING: Verification mismatch!${NC}"
  echo "ISO hash:    $ISO_HASH"
  echo "Device hash: $DEVICE_HASH"
  echo "The USB may still work, but re-flash if you have issues."
fi

# Eject
echo ""
sync
sudo eject "$DEVICE" 2>/dev/null || true
echo -e "${GREEN}Done. Remove USB and boot the target machine.${NC}"
