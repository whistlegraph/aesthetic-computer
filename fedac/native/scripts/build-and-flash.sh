#!/bin/bash
# build-and-flash.sh — Build ac-native from scratch and optionally flash to USB
#
# Works in Docker/Codespaces (no losetup/mount needed — uses mtools).
#
# Usage:
#   ./build-and-flash.sh                      # Build kernel only
#   ./build-and-flash.sh --flash /dev/sdb     # Build + flash to USB
#   ./build-and-flash.sh --skip-kernel --flash /dev/sdb  # Rebuild binary/initramfs, reuse kernel source
#   ./build-and-flash.sh --skip-binary --flash /dev/sdb  # Reuse binary, rebuild initramfs+kernel
#
# Requirements: cpio lz4 musl-gcc (or gcc) libdrm-devel alsa-lib-devel
#               mtools (for --flash)

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log()  { echo -e "${GREEN}[build]${NC} $*"; }
warn() { echo -e "${YELLOW}[build]${NC} $*"; }
err()  { echo -e "${RED}[build]${NC} $*" >&2; }

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NATIVE_DIR="$(dirname "${SCRIPT_DIR}")"
BUILD_DIR="${NATIVE_DIR}/build"
KERNEL_DIR="${NATIVE_DIR}/kernel"

FLASH_DEV=""
SKIP_KERNEL=0
SKIP_BINARY=0
PIECE_PATH="${NATIVE_DIR}/build/initramfs-root/piece.mjs"
KERNEL_VERSION="${KERNEL_VERSION:-6.14.2}"

while [ $# -gt 0 ]; do
    case "$1" in
        --flash)       FLASH_DEV="$2"; shift 2 ;;
        --skip-kernel) SKIP_KERNEL=1; shift ;;
        --skip-binary) SKIP_BINARY=1; shift ;;
        --piece)       PIECE_PATH="$2"; shift 2 ;;
        --help|-h)
            echo "Usage: $0 [--flash /dev/sdX] [--skip-kernel] [--skip-binary] [--piece path.mjs]"
            exit 0 ;;
        *) err "Unknown option: $1"; exit 1 ;;
    esac
done

mkdir -p "${BUILD_DIR}"

# ============================================================
# Step 1: Check dependencies
# ============================================================
log "Checking dependencies..."
MISSING=""
for cmd in cpio lz4 make curl; do
    command -v "$cmd" &>/dev/null || MISSING="${MISSING} ${cmd}"
done
if [ -n "${MISSING}" ]; then
    err "Missing required tools:${MISSING}"
    err "Install with: dnf install${MISSING}"
    exit 1
fi
if [ -n "${FLASH_DEV}" ]; then
    for cmd in mmd mcopy mkfs.vfat sfdisk; do
        command -v "$cmd" &>/dev/null || MISSING="${MISSING} ${cmd}"
    done
    if [ -n "${MISSING}" ]; then
        err "Missing flash tools:${MISSING}"
        err "Install with: dnf install mtools dosfstools util-linux"
        exit 1
    fi
fi

# ============================================================
# Step 2: Build ac-native binary (static, musl)
# ============================================================
if [ "${SKIP_BINARY}" -eq 0 ]; then
    log "Building ac-native binary..."
    cd "${NATIVE_DIR}"
    CC_USE="${CC:-}"
    if [ -z "${CC_USE}" ]; then
        if command -v musl-gcc &>/dev/null; then CC_USE=musl-gcc
        else CC_USE=gcc; fi
    fi
    make STATIC=1 CC="${CC_USE}"
    log "Binary: $(wc -c < "${BUILD_DIR}/ac-native") bytes"
else
    log "Skipping binary build"
fi

if [ ! -f "${BUILD_DIR}/ac-native" ]; then
    err "ac-native binary not found. Run without --skip-binary."
    exit 1
fi

# ============================================================
# Step 3: Create initramfs
# ============================================================
log "Creating initramfs..."
INITRAMFS_DIR="${BUILD_DIR}/initramfs-root"

# Preserve piece.mjs if it exists and no --piece override
if [ -f "${INITRAMFS_DIR}/piece.mjs" ] && [ "${PIECE_PATH}" = "${INITRAMFS_DIR}/piece.mjs" ]; then
    PIECE_CONTENT="$(cat "${INITRAMFS_DIR}/piece.mjs")"
fi

rm -rf "${INITRAMFS_DIR}"
mkdir -p "${INITRAMFS_DIR}"/{dev,proc,sys,tmp,mnt}

# Copy binary
cp "${BUILD_DIR}/ac-native" "${INITRAMFS_DIR}/ac-native"
chmod +x "${INITRAMFS_DIR}/ac-native"

# Copy piece
if [ -n "${PIECE_CONTENT:-}" ]; then
    echo "${PIECE_CONTENT}" > "${INITRAMFS_DIR}/piece.mjs"
elif [ -f "${PIECE_PATH}" ]; then
    cp "${PIECE_PATH}" "${INITRAMFS_DIR}/piece.mjs"
else
    err "No piece.mjs found at ${PIECE_PATH}"
    exit 1
fi

# Copy shared libs (if dynamic build)
if file "${BUILD_DIR}/ac-native" | grep -q "dynamically linked"; then
    log "Copying shared libraries for dynamic binary..."
    mkdir -p "${INITRAMFS_DIR}/lib64"
    for lib in $(ldd "${BUILD_DIR}/ac-native" | grep -oP '/\S+'); do
        [ -f "$lib" ] && cp "$lib" "${INITRAMFS_DIR}/lib64/"
    done
    # Symlink /lib -> /lib64 (some lookups use /lib)
    ln -sf lib64 "${INITRAMFS_DIR}/lib"
fi

# Copy ALSA config files (required for snd_pcm_open to resolve device names)
if [ -d "/usr/share/alsa" ]; then
    log "Copying ALSA config files..."
    mkdir -p "${INITRAMFS_DIR}/usr/share/alsa"
    cp -r /usr/share/alsa/* "${INITRAMFS_DIR}/usr/share/alsa/"
    log "ALSA config: $(du -sh "${INITRAMFS_DIR}/usr/share/alsa" | cut -f1)"
else
    warn "No /usr/share/alsa found — audio may not work"
fi

# Copy WiFi components (wpa_supplicant, dhclient, iw, ip, firmware)
WIFI_BINS=( /usr/sbin/wpa_supplicant /usr/sbin/wpa_cli /usr/sbin/dhclient /usr/sbin/iw /sbin/ip )
WIFI_COPIED=0
for bin in "${WIFI_BINS[@]}"; do
    if [ -f "$bin" ]; then
        mkdir -p "${INITRAMFS_DIR}/$(dirname "$bin")"
        cp "$bin" "${INITRAMFS_DIR}${bin}"
        chmod +x "${INITRAMFS_DIR}${bin}"
        WIFI_COPIED=$((WIFI_COPIED + 1))
        # Copy their shared libraries too
        for lib in $(ldd "$bin" 2>/dev/null | grep -oP '/\S+'); do
            [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
        done
    fi
done
if [ "${WIFI_COPIED}" -gt 0 ]; then
    log "WiFi binaries: ${WIFI_COPIED} copied"

    # Copy Intel WiFi firmware (AX200 = cc-a0, also grab QuZ for AX201)
    mkdir -p "${INITRAMFS_DIR}/lib/firmware"
    for fw in /lib/firmware/iwlwifi-cc-a0-*.ucode*; do
        [ -f "$fw" ] && { xz -dk "$fw" -c > "${INITRAMFS_DIR}/lib/firmware/$(basename "${fw%.xz}")" 2>/dev/null || cp "$fw" "${INITRAMFS_DIR}/lib/firmware/"; }
    done
    for fw in /lib/firmware/iwlwifi-QuZ-a0-*.ucode*; do
        [ -f "$fw" ] && { xz -dk "$fw" -c > "${INITRAMFS_DIR}/lib/firmware/$(basename "${fw%.xz}")" 2>/dev/null || cp "$fw" "${INITRAMFS_DIR}/lib/firmware/"; }
    done
    FW_SIZE=$(du -sh "${INITRAMFS_DIR}/lib/firmware" 2>/dev/null | cut -f1)
    log "WiFi firmware: ${FW_SIZE:-0}"

    # Create /var/run for wpa_supplicant
    mkdir -p "${INITRAMFS_DIR}/var/run/wpa_supplicant"

    # Need /bin/sh for system() calls — use busybox or link to bash if available
    mkdir -p "${INITRAMFS_DIR}/bin"
    if command -v busybox &>/dev/null; then
        cp "$(command -v busybox)" "${INITRAMFS_DIR}/bin/busybox"
        ln -sf busybox "${INITRAMFS_DIR}/bin/sh"
        for lib in $(ldd "$(command -v busybox)" 2>/dev/null | grep -oP '/\S+'); do
            [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
        done
    elif [ -f /bin/bash ]; then
        cp /bin/bash "${INITRAMFS_DIR}/bin/sh"
        for lib in $(ldd /bin/bash 2>/dev/null | grep -oP '/\S+'); do
            [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
        done
    fi

    # Need basic utilities for shell commands (grep, awk, pgrep, killall, etc.)
    for util in grep awk sed pgrep killall cat; do
        UTIL_PATH="$(command -v "$util" 2>/dev/null || true)"
        if [ -n "$UTIL_PATH" ] && [ -f "$UTIL_PATH" ]; then
            cp "$UTIL_PATH" "${INITRAMFS_DIR}/bin/"
            for lib in $(ldd "$UTIL_PATH" 2>/dev/null | grep -oP '/\S+'); do
                [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
            done
        fi
    done
else
    warn "No WiFi binaries found — WiFi will not work"
fi

# Init: symlink to ac-native (no /bin/sh in initramfs)
ln -sf /ac-native "${INITRAMFS_DIR}/init"

# Create cpio + lz4
INITRAMFS_CPIO="${BUILD_DIR}/initramfs.cpio"
cd "${INITRAMFS_DIR}"
find . -print0 | cpio --null -ov --format=newc > "${INITRAMFS_CPIO}" 2>/dev/null
lz4 -l -9 -f "${INITRAMFS_CPIO}" "${INITRAMFS_CPIO}.lz4"
CPIO_SIZE=$(wc -c < "${INITRAMFS_CPIO}.lz4")
log "Initramfs: ${CPIO_SIZE} bytes (LZ4)"

# ============================================================
# Step 4: Build kernel with embedded initramfs
# ============================================================
KERNEL_SRC="${BUILD_DIR}/linux-${KERNEL_VERSION}"
VMLINUZ="${BUILD_DIR}/vmlinuz"

if [ "${SKIP_KERNEL}" -eq 0 ] || [ ! -f "${VMLINUZ}" ]; then
    log "Building kernel ${KERNEL_VERSION} with embedded initramfs..."

    # Download kernel source if needed
    KERNEL_TAR="${BUILD_DIR}/linux-${KERNEL_VERSION}.tar.xz"
    if [ ! -f "${KERNEL_TAR}" ]; then
        log "Downloading kernel..."
        curl -L "https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-${KERNEL_VERSION}.tar.xz" -o "${KERNEL_TAR}"
    fi
    if [ ! -d "${KERNEL_SRC}" ]; then
        log "Extracting kernel..."
        tar xf "${KERNEL_TAR}" -C "${BUILD_DIR}"
    fi

    # Apply config
    cp "${KERNEL_DIR}/config-minimal" "${KERNEL_SRC}/.config"

    # Set initramfs source path
    INITRAMFS_ABS="$(cd "${BUILD_DIR}" && pwd)/initramfs.cpio.lz4"
    echo "CONFIG_INITRAMFS_SOURCE=\"${INITRAMFS_ABS}\"" >> "${KERNEL_SRC}/.config"

    # Finalize config
    cd "${KERNEL_SRC}"
    make olddefconfig

    # Build
    JOBS="${JOBS:-$(nproc)}"
    log "Compiling kernel (${JOBS} jobs)..."
    make -j"${JOBS}" bzImage

    # Copy output
    BZIMAGE="${KERNEL_SRC}/arch/x86/boot/bzImage"
    if [ -f "${BZIMAGE}" ]; then
        cp "${BZIMAGE}" "${VMLINUZ}"
    else
        err "bzImage not found!"
        exit 1
    fi
else
    log "Skipping kernel build (using existing vmlinuz)"
fi

KERNEL_SIZE=$(wc -c < "${VMLINUZ}")
log "Kernel: $(( KERNEL_SIZE / 1024 / 1024 ))MB (${KERNEL_SIZE} bytes)"

# ============================================================
# Step 5: Flash to USB (if --flash specified)
# ============================================================
if [ -n "${FLASH_DEV}" ]; then
    if [ ! -b "${FLASH_DEV}" ]; then
        err "${FLASH_DEV} is not a block device"
        exit 1
    fi

    log "Flashing to ${FLASH_DEV}..."
    warn "This will ERASE ALL DATA on ${FLASH_DEV}"

    # Create GPT + EFI System Partition
    sfdisk "${FLASH_DEV}" << 'PART_EOF'
label: gpt
type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=512M
PART_EOF
    sync

    # Wait for partition
    PART="${FLASH_DEV}1"
    for i in $(seq 1 20); do
        [ -b "${PART}" ] && break
        sleep 0.3
    done

    if [ ! -b "${PART}" ]; then
        err "Partition ${PART} not found"
        exit 1
    fi

    # Format FAT32
    mkfs.vfat -F 32 -n "AC-NATIVE" "${PART}"

    # Copy kernel using mtools (no mount needed)
    mmd -i "${PART}" ::EFI
    mmd -i "${PART}" ::EFI/BOOT
    mcopy -i "${PART}" "${VMLINUZ}" ::EFI/BOOT/BOOTX64.EFI

    sync
    log "Flashed! Kernel at EFI/BOOT/BOOTX64.EFI on ${FLASH_DEV}"
    log "Remove USB and boot from it (UEFI, Secure Boot OFF)."
else
    log ""
    log "=== Build complete ==="
    log "Kernel: ${VMLINUZ}"
    log ""
    log "To flash to USB:"
    log "  $0 --flash /dev/sdX"
    log ""
    log "Or manually with mtools:"
    log "  sfdisk /dev/sdX < <(echo 'label: gpt'; echo 'type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=512M')"
    log "  mkfs.vfat -F 32 -n AC-NATIVE /dev/sdX1"
    log "  mmd -i /dev/sdX1 ::EFI ::EFI/BOOT"
    log "  mcopy -i /dev/sdX1 ${VMLINUZ} ::EFI/BOOT/BOOTX64.EFI"
fi
