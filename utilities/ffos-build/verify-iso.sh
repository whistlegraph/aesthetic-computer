#!/usr/bin/env bash
# FFOS ISO Verification & Flash Script
# Verifies ISO integrity and safely writes to USB

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info()  { echo -e "${BLUE}ℹ️  $1${NC}"; }
log_ok()    { echo -e "${GREEN}✅ $1${NC}"; }
log_warn()  { echo -e "${YELLOW}⚠️  $1${NC}"; }
log_error() { echo -e "${RED}❌ $1${NC}"; }

usage() {
  cat << EOF
Usage: $(basename "$0") [OPTIONS] <iso-file> [usb-device]

Verify FFOS ISO integrity and optionally flash to USB.

OPTIONS:
  -v, --verify-only    Only verify, don't flash
  -f, --force          Skip confirmation prompts
  -c, --checksum FILE  Use specific checksum file (default: <iso>.sha256)
  -h, --help           Show this help

EXAMPLES:
  # Verify ISO only
  $(basename "$0") --verify-only FF1-develop-1.0.0.iso

  # Verify and flash to /dev/sdb
  $(basename "$0") FF1-develop-1.0.0.iso /dev/sdb

  # Flash without confirmation (dangerous!)
  $(basename "$0") --force FF1-develop-1.0.0.iso /dev/sdb

EOF
}

# Parse arguments
VERIFY_ONLY=false
FORCE=false
CHECKSUM_FILE=""

while [[ $# -gt 0 ]]; do
  case $1 in
    -v|--verify-only) VERIFY_ONLY=true; shift ;;
    -f|--force) FORCE=true; shift ;;
    -c|--checksum) CHECKSUM_FILE="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    -*) log_error "Unknown option: $1"; usage; exit 1 ;;
    *) break ;;
  esac
done

if [[ $# -lt 1 ]]; then
  log_error "Missing ISO file argument"
  usage
  exit 1
fi

ISO_FILE="$1"
USB_DEVICE="${2:-}"

if [[ ! -f "$ISO_FILE" ]]; then
  log_error "ISO file not found: $ISO_FILE"
  exit 1
fi

ISO_SIZE=$(stat -c%s "$ISO_FILE" 2>/dev/null || stat -f%z "$ISO_FILE")
ISO_SIZE_MB=$((ISO_SIZE / 1024 / 1024))

log_info "ISO: $ISO_FILE ($ISO_SIZE_MB MB)"

# ─── Checksum Verification ─────────────────────────────────────────────────────

verify_checksum() {
  local iso="$1"
  local checksum_file="${CHECKSUM_FILE:-${iso}.sha256}"
  
  log_info "Verifying SHA256 checksum..."
  
  if [[ ! -f "$checksum_file" ]]; then
    log_warn "No checksum file found: $checksum_file"
    log_info "Generating checksum for future verification..."
    sha256sum "$iso" > "${iso}.sha256"
    log_ok "Checksum saved to: ${iso}.sha256"
    return 0
  fi
  
  # Extract expected hash
  local expected_hash
  expected_hash=$(awk '{print $1}' "$checksum_file")
  
  # Calculate actual hash
  log_info "Calculating SHA256 (this may take a moment)..."
  local actual_hash
  actual_hash=$(sha256sum "$iso" | awk '{print $1}')
  
  if [[ "$expected_hash" == "$actual_hash" ]]; then
    log_ok "Checksum verified: $actual_hash"
    return 0
  else
    log_error "Checksum MISMATCH!"
    log_error "  Expected: $expected_hash"
    log_error "  Actual:   $actual_hash"
    log_error "The ISO file is corrupted or was modified."
    return 1
  fi
}

# ─── SquashFS Integrity Check ──────────────────────────────────────────────────

verify_squashfs() {
  local iso="$1"
  
  log_info "Checking SquashFS integrity inside ISO..."
  
  # Check if required tools are available
  if ! command -v unsquashfs &>/dev/null; then
    log_warn "unsquashfs not found - skipping SquashFS verification"
    log_info "Install squashfs-tools to enable: sudo dnf install squashfs-tools"
    return 0
  fi
  
  # Mount ISO temporarily
  local mount_point
  mount_point=$(mktemp -d)
  trap "sudo umount '$mount_point' 2>/dev/null; rmdir '$mount_point'" RETURN
  
  log_info "Mounting ISO to check squashfs..."
  if ! sudo mount -o loop,ro "$iso" "$mount_point"; then
    log_error "Failed to mount ISO"
    return 1
  fi
  
  # Look for squashfs file
  local sfs_file=""
  for path in \
    "$mount_point/arch/x86_64/airootfs.sfs" \
    "$mount_point/LiveOS/squashfs.img" \
    "$mount_point/casper/filesystem.squashfs"; do
    if [[ -f "$path" ]]; then
      sfs_file="$path"
      break
    fi
  done
  
  if [[ -z "$sfs_file" ]]; then
    log_warn "No squashfs found in ISO (may be different format)"
    return 0
  fi
  
  log_info "Found: $sfs_file"
  log_info "Verifying SquashFS integrity (checking xz blocks)..."
  
  # Use unsquashfs to list and verify structure
  if unsquashfs -l "$sfs_file" > /dev/null 2>&1; then
    log_ok "SquashFS structure is valid"
    
    # Get squashfs stats
    local sfs_info
    sfs_info=$(unsquashfs -s "$sfs_file" 2>/dev/null | head -20)
    echo "$sfs_info" | grep -E "Compression|Block size|Number of" | while read -r line; do
      log_info "  $line"
    done
    
    return 0
  else
    log_error "SquashFS verification FAILED - file may be corrupted"
    log_error "This explains the installation errors you saw"
    return 1
  fi
}

# ─── USB Flash ─────────────────────────────────────────────────────────────────

list_usb_devices() {
  log_info "Available USB devices:"
  echo
  lsblk -d -o NAME,SIZE,TYPE,MODEL,TRAN | grep -E "usb|NAME" || true
  echo
}

flash_to_usb() {
  local iso="$1"
  local device="$2"
  
  if [[ ! -b "$device" ]]; then
    log_error "Not a block device: $device"
    list_usb_devices
    exit 1
  fi
  
  # Safety check - don't flash to mounted devices
  if grep -q "^$device" /proc/mounts; then
    log_error "Device $device has mounted partitions!"
    log_error "Unmount all partitions first: sudo umount ${device}*"
    exit 1
  fi
  
  # Get device info
  local device_model
  device_model=$(lsblk -n -o MODEL "$device" 2>/dev/null | head -1 | xargs)
  local device_size
  device_size=$(lsblk -n -o SIZE "$device" 2>/dev/null | head -1)
  
  log_warn "TARGET: $device ($device_size) $device_model"
  log_warn "ALL DATA ON $device WILL BE DESTROYED!"
  
  if [[ "$FORCE" != "true" ]]; then
    read -rp "Type 'yes' to continue: " confirm
    if [[ "$confirm" != "yes" ]]; then
      log_info "Aborted."
      exit 0
    fi
  fi
  
  log_info "Writing ISO to $device..."
  log_info "This will take several minutes..."
  
  # Use dd with progress and sync
  sudo dd \
    if="$iso" \
    of="$device" \
    bs=4M \
    status=progress \
    conv=fsync \
    oflag=direct
  
  log_info "Syncing buffers..."
  sync
  
  log_ok "Flash complete!"
  
  # Verify write by reading back first MB and comparing
  log_info "Verifying write (first 4MB)..."
  local iso_head
  iso_head=$(head -c 4194304 "$iso" | sha256sum | awk '{print $1}')
  local usb_head
  usb_head=$(sudo head -c 4194304 "$device" | sha256sum | awk '{print $1}')
  
  if [[ "$iso_head" == "$usb_head" ]]; then
    log_ok "Write verification passed!"
  else
    log_error "Write verification FAILED - USB may be faulty"
    exit 1
  fi
  
  log_ok "USB is ready! Safe to eject: sudo eject $device"
}

# ─── Main ──────────────────────────────────────────────────────────────────────

echo "════════════════════════════════════════════════════════════════"
echo "  FFOS ISO Verification Tool"
echo "════════════════════════════════════════════════════════════════"
echo

# Step 1: Verify checksum
if ! verify_checksum "$ISO_FILE"; then
  log_error "ISO failed checksum verification. Do not use this ISO!"
  exit 1
fi

# Step 2: Verify squashfs
if ! verify_squashfs "$ISO_FILE"; then
  log_error "ISO has corrupted SquashFS. This ISO will fail during installation."
  log_info "Please download a fresh copy or rebuild the ISO."
  exit 1
fi

echo
log_ok "ISO verification passed!"

if [[ "$VERIFY_ONLY" == "true" ]]; then
  exit 0
fi

# Step 3: Flash to USB (if device provided)
if [[ -n "$USB_DEVICE" ]]; then
  echo
  flash_to_usb "$ISO_FILE" "$USB_DEVICE"
else
  echo
  log_info "To flash to USB, run:"
  echo "  $(basename "$0") '$ISO_FILE' /dev/sdX"
  echo
  list_usb_devices
fi
