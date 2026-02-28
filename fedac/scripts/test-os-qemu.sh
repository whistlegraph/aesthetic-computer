#!/bin/bash
# test-os-qemu.sh — Download an Alpine OS image from oven and boot it in QEMU
#
# Designed for macOS (Apple Silicon or Intel) with Homebrew QEMU.
# Downloads a piece-specific or base Alpine image and launches it in a
# QEMU x86_64 VM with UEFI boot and serial console output.
#
# Usage:
#   ./fedac/scripts/test-os-qemu.sh [piece] [options]
#
# Examples:
#   ./fedac/scripts/test-os-qemu.sh notepat        # Download + boot notepat image
#   ./fedac/scripts/test-os-qemu.sh --base          # Boot the base image (no piece)
#   ./fedac/scripts/test-os-qemu.sh --local my.img  # Boot a local .img file
#   ./fedac/scripts/test-os-qemu.sh --reboot        # Re-launch last downloaded image
#
# Options:
#   --base              Download the base image (no piece injection)
#   --local <path>      Boot a local .img file instead of downloading
#   --reboot            Re-launch the last downloaded image
#   --flavor <f>        Image flavor: alpine (default) or fedora
#   --ram <mb>          VM RAM in MB (default: 2048)
#   --serial            Serial console only (no graphical window)
#   --verbose           Show full kernel boot messages (remove "quiet")
#   --help              Show this help

set -euo pipefail

# ── Config ──
OVEN_URL="https://oven.aesthetic.computer"
CACHE_DIR="${HOME}/.cache/fedos-test"
FLAVOR="alpine"
RAM_MB="2048"
SERIAL_ONLY=false
VERBOSE=false
PIECE=""
LOCAL_IMG=""
REBOOT=false
BASE_MODE=false

# ── Parse args ──
while [ $# -gt 0 ]; do
  case "$1" in
    --base)       BASE_MODE=true; shift ;;
    --local)      LOCAL_IMG="$2"; shift 2 ;;
    --reboot)     REBOOT=true; shift ;;
    --flavor)     FLAVOR="$2"; shift 2 ;;
    --ram)        RAM_MB="$2"; shift 2 ;;
    --serial)     SERIAL_ONLY=true; shift ;;
    --verbose)    VERBOSE=true; shift ;;
    --help|-h)
      sed -n '2,/^$/s/^# //p' "$0"
      exit 0
      ;;
    -*)           echo "Unknown option: $1"; exit 1 ;;
    *)            PIECE="$1"; shift ;;
  esac
done

# ── Resolve QEMU paths ──
QEMU="qemu-system-x86_64"
command -v "$QEMU" >/dev/null 2>&1 || {
  echo "Error: $QEMU not found. Install with: brew install qemu"
  exit 1
}

# Find UEFI firmware (code + vars)
BREW_PREFIX="$(brew --prefix 2>/dev/null || echo /opt/homebrew)"
OVMF_CODE="$BREW_PREFIX/share/qemu/edk2-x86_64-code.fd"
OVMF_VARS_TEMPLATE="$BREW_PREFIX/share/qemu/edk2-i386-vars.fd"
[ -f "$OVMF_CODE" ] || {
  echo "Error: UEFI firmware not found at $OVMF_CODE"
  exit 1
}
[ -f "$OVMF_VARS_TEMPLATE" ] || {
  echo "Error: UEFI vars template not found at $OVMF_VARS_TEMPLATE"
  exit 1
}

mkdir -p "$CACHE_DIR"
LAST_IMG_LINK="$CACHE_DIR/last.img"

# ── Get the image ──
if [ -n "$LOCAL_IMG" ]; then
  # Use a local image file
  [ -f "$LOCAL_IMG" ] || { echo "Error: $LOCAL_IMG not found"; exit 1; }
  IMG="$LOCAL_IMG"
  echo "Using local image: $IMG"

elif [ "$REBOOT" = true ]; then
  # Re-use the last downloaded image
  [ -f "$LAST_IMG_LINK" ] || { echo "Error: No previous image found. Download one first."; exit 1; }
  IMG="$LAST_IMG_LINK"
  echo "Re-launching last image: $IMG"

elif [ "$BASE_MODE" = true ]; then
  # Download the base image from CDN
  CDN_URL="https://assets-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/os/${FLAVOR}-base-latest.img"
  IMG="$CACHE_DIR/${FLAVOR}-base.img"
  echo "Downloading ${FLAVOR} base image..."
  echo "  URL: $CDN_URL"
  curl -# -L -o "$IMG" "$CDN_URL"
  ln -sf "$IMG" "$LAST_IMG_LINK"
  echo "  Downloaded: $(du -h "$IMG" | awk '{print $1}')"

else
  # Download a piece-specific image from oven /os endpoint
  [ -n "$PIECE" ] || { echo "Error: Specify a piece name, --base, --local, or --reboot"; exit 1; }
  OS_URL="${OVEN_URL}/os?piece=${PIECE}&flavor=${FLAVOR}"
  IMG="$CACHE_DIR/${FLAVOR}-${PIECE}.img"
  echo "Downloading ${FLAVOR} image for piece: $PIECE"
  echo "  URL: $OS_URL"
  echo "  (This may take a minute — oven injects the piece into the base image)"
  curl -# -L -o "$IMG" "$OS_URL"
  ln -sf "$IMG" "$LAST_IMG_LINK"
  echo "  Downloaded: $(du -h "$IMG" | awk '{print $1}')"
fi

IMG_SIZE=$(stat -f%z "$IMG" 2>/dev/null || stat -c%s "$IMG" 2>/dev/null || echo "?")
echo ""
echo "Image: $IMG ($(echo "$IMG_SIZE" | awk '{printf "%.0fMB", $1/1048576}'))"
echo "VM: ${RAM_MB}MB RAM, x86_64 UEFI"
echo ""

# ── Prepare writable NVRAM vars (per-image copy) ──
OVMF_VARS="$CACHE_DIR/efivars.fd"
if [ ! -f "$OVMF_VARS" ]; then
  cp "$OVMF_VARS_TEMPLATE" "$OVMF_VARS"
fi

# ── Build QEMU command ──
QEMU_ARGS=(
  -machine q35
  -cpu qemu64
  -m "$RAM_MB"
  -drive "if=pflash,format=raw,readonly=on,file=$OVMF_CODE"
  -drive "if=pflash,format=raw,file=$OVMF_VARS"
  -drive "file=$IMG,format=raw,if=virtio"
  -device virtio-vga
  -device virtio-net-pci,netdev=net0
  -netdev "user,id=net0,hostfwd=tcp::2222-:22"
  -device virtio-keyboard
  -device virtio-tablet
  -usb
)

# Serial console
if [ "$SERIAL_ONLY" = true ]; then
  QEMU_ARGS+=(-nographic -serial mon:stdio)
else
  QEMU_ARGS+=(-serial stdio)
fi

# Show boot messages if verbose
if [ "$VERBOSE" = true ]; then
  echo "(Verbose mode — kernel messages will be visible)"
fi

echo "Starting QEMU... (Ctrl+C to quit, or close the window)"
echo "Serial console output below:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
exec "$QEMU" "${QEMU_ARGS[@]}"
