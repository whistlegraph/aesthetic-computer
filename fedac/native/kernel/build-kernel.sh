#!/bin/bash
# build-kernel.sh — Download and compile a minimal Linux kernel for ac-native
# Usage: ./build-kernel.sh [initramfs.cpio] [output-dir]
set -euo pipefail

KERNEL_VERSION="${KERNEL_VERSION:-6.13.2}"
KERNEL_URL="https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-${KERNEL_VERSION}.tar.xz"

INITRAMFS="${1:-}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
OUTPUT_DIR="$(cd "${2:-${SCRIPT_DIR}/../build}" 2>/dev/null && pwd || echo "${SCRIPT_DIR}/../build")"
mkdir -p "${OUTPUT_DIR}"
OUTPUT_DIR="$(cd "${OUTPUT_DIR}" && pwd)"
BUILD_DIR="${OUTPUT_DIR}/kernel-build"
JOBS="${JOBS:-$(nproc)}"

echo "=== ac-native kernel build ==="
echo "Kernel: ${KERNEL_VERSION}"
echo "Initramfs: ${INITRAMFS:-none (set later)}"
echo "Output: ${OUTPUT_DIR}"
echo "Jobs: ${JOBS}"
echo ""

# Download kernel source
KERNEL_TAR="${OUTPUT_DIR}/linux-${KERNEL_VERSION}.tar.xz"
KERNEL_SRC="${OUTPUT_DIR}/linux-${KERNEL_VERSION}"

mkdir -p "${OUTPUT_DIR}"

if [ ! -f "${KERNEL_TAR}" ]; then
    echo "Downloading kernel ${KERNEL_VERSION}..."
    curl -L "${KERNEL_URL}" -o "${KERNEL_TAR}"
fi

if [ ! -d "${KERNEL_SRC}" ]; then
    echo "Extracting kernel..."
    tar xf "${KERNEL_TAR}" -C "${OUTPUT_DIR}"
fi

# Apply minimal config
echo "Configuring kernel..."
cp "${SCRIPT_DIR}/config-minimal" "${KERNEL_SRC}/.config"

# Set initramfs source if provided
if [ -n "${INITRAMFS}" ]; then
    INITRAMFS_ABS="$(cd "$(dirname "${INITRAMFS}")" && pwd)/$(basename "${INITRAMFS}")"
    echo "CONFIG_INITRAMFS_SOURCE=\"${INITRAMFS_ABS}\"" >> "${KERNEL_SRC}/.config"
    echo "Initramfs: ${INITRAMFS_ABS}"
fi

# Finalize config (resolve dependencies)
cd "${KERNEL_SRC}"
make olddefconfig

# Build
echo ""
echo "Building kernel (this takes a few minutes)..."
make -j"${JOBS}" bzImage

# Copy output
BZIMAGE="${KERNEL_SRC}/arch/x86/boot/bzImage"
if [ -f "${BZIMAGE}" ]; then
    cp "${BZIMAGE}" "${OUTPUT_DIR}/vmlinuz"
    SIZE=$(wc -c < "${OUTPUT_DIR}/vmlinuz")
    echo ""
    echo "=== Kernel built ==="
    echo "Output: ${OUTPUT_DIR}/vmlinuz"
    echo "Size: $(( SIZE / 1024 / 1024 ))MB (${SIZE} bytes)"
    echo ""
    echo "This kernel is an EFI stub binary."
    echo "Copy to EFI/BOOT/BOOTX64.EFI on a FAT32 USB to boot directly."
else
    echo "ERROR: bzImage not found at ${BZIMAGE}"
    exit 1
fi
