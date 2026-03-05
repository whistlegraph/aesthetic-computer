#!/bin/bash
# make-native-kiosk.sh — Build a sub-second boot AC native kiosk image
#
# Usage:
#   ./make-native-kiosk.sh <piece> [device|--image path] [options]
#
# Examples:
#   ./make-native-kiosk.sh hello                    # Build image only
#   ./make-native-kiosk.sh hello --image out.img    # Output to .img file
#   ./make-native-kiosk.sh hello /dev/sdb           # Flash to USB
#
# The resulting image is a single FAT32 EFI partition containing:
#   EFI/BOOT/BOOTX64.EFI  — The kernel (with built-in initramfs)
#
# Total size: ~5-15MB depending on kernel GPU drivers included.

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

log()  { echo -e "${GREEN}[ac-native]${NC} $*"; }
warn() { echo -e "${YELLOW}[ac-native]${NC} $*"; }
err()  { echo -e "${RED}[ac-native]${NC} $*" >&2; }

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NATIVE_DIR="$(dirname "${SCRIPT_DIR}")"
BUILD_DIR="${NATIVE_DIR}/build"
PIECE=""
TARGET=""
IMAGE_PATH=""
IMAGE_SIZE_MB=32  # Small — kernel + ESP overhead

usage() {
    echo "Usage: $0 <piece-name> [device|--image path] [options]"
    echo ""
    echo "  piece-name    Name of piece in test-pieces/ or path to .mjs file"
    echo "  device        Block device to flash (e.g., /dev/sdb)"
    echo "  --image path  Output as .img file"
    echo ""
    echo "Options:"
    echo "  --skip-kernel   Skip kernel build (use existing build/vmlinuz)"
    echo "  --skip-binary   Skip ac-native build (use existing build/ac-native)"
    exit 1
}

SKIP_KERNEL=0
SKIP_BINARY=0

# Parse args
while [ $# -gt 0 ]; do
    case "$1" in
        --image)     IMAGE_PATH="$2"; shift 2 ;;
        --skip-kernel) SKIP_KERNEL=1; shift ;;
        --skip-binary) SKIP_BINARY=1; shift ;;
        --help|-h)   usage ;;
        /dev/*)      TARGET="$1"; shift ;;
        *)
            if [ -z "${PIECE}" ]; then
                PIECE="$1"
            fi
            shift ;;
    esac
done

if [ -z "${PIECE}" ]; then
    err "No piece specified"
    usage
fi

# Resolve piece path
if [ -f "${PIECE}" ]; then
    PIECE_PATH="${PIECE}"
elif [ -f "${NATIVE_DIR}/test-pieces/${PIECE}.mjs" ]; then
    PIECE_PATH="${NATIVE_DIR}/test-pieces/${PIECE}.mjs"
else
    err "Piece not found: ${PIECE}"
    err "Looked in: ${NATIVE_DIR}/test-pieces/${PIECE}.mjs"
    exit 1
fi

log "Piece: ${PIECE_PATH}"
log "Build dir: ${BUILD_DIR}"
mkdir -p "${BUILD_DIR}"

# ============================================================
# Step 1: Build ac-native binary
# ============================================================
if [ "${SKIP_BINARY}" -eq 0 ]; then
    log "Building ac-native..."
    cd "${NATIVE_DIR}"
    make STATIC=1 CC="${CC:-gcc}"
    log "Binary: ${BUILD_DIR}/ac-native ($(wc -c < "${BUILD_DIR}/ac-native") bytes)"
else
    log "Skipping binary build (--skip-binary)"
fi

if [ ! -f "${BUILD_DIR}/ac-native" ]; then
    err "ac-native binary not found at ${BUILD_DIR}/ac-native"
    exit 1
fi

# ============================================================
# Step 2: Create initramfs
# ============================================================
log "Creating initramfs..."
INITRAMFS_DIR="${BUILD_DIR}/initramfs-root"
rm -rf "${INITRAMFS_DIR}"
mkdir -p "${INITRAMFS_DIR}"/{dev,proc,sys,tmp}

# Copy binary
cp "${BUILD_DIR}/ac-native" "${INITRAMFS_DIR}/ac-native"
chmod +x "${INITRAMFS_DIR}/ac-native"

# Copy piece
cp "${PIECE_PATH}" "${INITRAMFS_DIR}/piece.mjs"

# Create init script
cat > "${INITRAMFS_DIR}/init" << 'INIT_EOF'
#!/bin/sh
exec /ac-native /piece.mjs
INIT_EOF
chmod +x "${INITRAMFS_DIR}/init"

# Create cpio archive
INITRAMFS_CPIO="${BUILD_DIR}/initramfs.cpio"
cd "${INITRAMFS_DIR}"
find . -print0 | cpio --null -ov --format=newc > "${INITRAMFS_CPIO}" 2>/dev/null

# Compress with LZ4 (fastest decompression)
if command -v lz4 &>/dev/null; then
    lz4 -l -9 "${INITRAMFS_CPIO}" "${INITRAMFS_CPIO}.lz4"
    INITRAMFS_CPIO="${INITRAMFS_CPIO}.lz4"
    log "Initramfs: ${INITRAMFS_CPIO} ($(wc -c < "${INITRAMFS_CPIO}") bytes, LZ4)"
else
    warn "lz4 not found, using uncompressed initramfs"
    log "Initramfs: ${INITRAMFS_CPIO} ($(wc -c < "${INITRAMFS_CPIO}") bytes)"
fi

# ============================================================
# Step 3: Build kernel (with initramfs embedded)
# ============================================================
if [ "${SKIP_KERNEL}" -eq 0 ]; then
    log "Building kernel with embedded initramfs..."
    "${NATIVE_DIR}/kernel/build-kernel.sh" "${INITRAMFS_CPIO}" "${BUILD_DIR}"
else
    log "Skipping kernel build (--skip-kernel)"
fi

KERNEL="${BUILD_DIR}/vmlinuz"
if [ ! -f "${KERNEL}" ]; then
    err "Kernel not found at ${KERNEL}"
    err "Run without --skip-kernel to build it"
    exit 1
fi

KERNEL_SIZE=$(wc -c < "${KERNEL}")
log "Kernel: ${KERNEL} ($(( KERNEL_SIZE / 1024 / 1024 ))MB)"

# ============================================================
# Step 4: Create bootable image
# ============================================================
if [ -n "${IMAGE_PATH}" ] || [ -n "${TARGET}" ]; then
    DEST="${IMAGE_PATH:-${BUILD_DIR}/ac-native.img}"

    log "Creating bootable image: ${DEST}"

    # Create image file
    dd if=/dev/zero of="${DEST}" bs=1M count="${IMAGE_SIZE_MB}" status=progress 2>/dev/null

    # Create GPT partition table with single EFI partition
    parted -s "${DEST}" mklabel gpt
    parted -s "${DEST}" mkpart ESP fat32 1MiB 100%
    parted -s "${DEST}" set 1 esp on

    # Setup loop device
    LOOP=$(losetup --find --show --partscan "${DEST}")
    PART="${LOOP}p1"

    # Wait for partition device
    for i in $(seq 1 10); do
        [ -b "${PART}" ] && break
        sleep 0.2
    done

    # Format as FAT32
    mkfs.vfat -F 32 -n "AC-NATIVE" "${PART}"

    # Mount and copy kernel
    MOUNT_DIR="${BUILD_DIR}/mnt"
    mkdir -p "${MOUNT_DIR}"
    mount "${PART}" "${MOUNT_DIR}"

    mkdir -p "${MOUNT_DIR}/EFI/BOOT"
    cp "${KERNEL}" "${MOUNT_DIR}/EFI/BOOT/BOOTX64.EFI"

    # Unmount and detach
    umount "${MOUNT_DIR}"
    losetup -d "${LOOP}"

    IMAGE_SIZE=$(wc -c < "${DEST}")
    log "Image: ${DEST} ($(( IMAGE_SIZE / 1024 / 1024 ))MB)"

    # Flash to device if specified
    if [ -n "${TARGET}" ]; then
        warn "About to write to ${TARGET} — ALL DATA WILL BE LOST"
        read -p "Continue? [y/N] " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            log "Flashing to ${TARGET}..."
            dd if="${DEST}" of="${TARGET}" bs=4M status=progress conv=fsync
            sync
            log "Done! Remove USB and boot from it."
        else
            log "Aborted."
        fi
    fi
else
    log ""
    log "=== Build complete ==="
    log "Kernel (with initramfs): ${KERNEL}"
    log "To create a bootable image:"
    log "  $0 ${PIECE} --image ${BUILD_DIR}/ac-native.img"
    log ""
    log "To test in QEMU:"
    log "  ${NATIVE_DIR}/scripts/test-native-qemu.sh"
fi
