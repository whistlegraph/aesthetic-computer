#!/bin/bash
# test-native-qemu.sh — Test ac-native in QEMU with UEFI firmware
#
# Usage:
#   ./test-native-qemu.sh                    # Use built image
#   ./test-native-qemu.sh path/to/image.img  # Use specific image
#   ./test-native-qemu.sh --kernel           # Boot kernel directly (fastest)
#
# Prerequisites:
#   - qemu-system-x86_64
#   - OVMF firmware (apt install ovmf / dnf install edk2-ovmf)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NATIVE_DIR="$(dirname "${SCRIPT_DIR}")"
BUILD_DIR="${NATIVE_DIR}/build"

# Find OVMF firmware
OVMF=""
for path in \
    /usr/share/OVMF/OVMF_CODE.fd \
    /usr/share/edk2/ovmf/OVMF_CODE.fd \
    /usr/share/qemu/OVMF_CODE.fd \
    /usr/share/edk2-ovmf/x64/OVMF_CODE.fd \
    /usr/share/OVMF/OVMF_CODE_4M.fd; do
    if [ -f "$path" ]; then
        OVMF="$path"
        break
    fi
done

if [ -z "${OVMF}" ]; then
    echo "ERROR: OVMF firmware not found"
    echo "Install: sudo apt install ovmf  OR  sudo dnf install edk2-ovmf"
    exit 1
fi

echo "OVMF: ${OVMF}"

# Create writable OVMF vars copy
OVMF_VARS="${BUILD_DIR}/ovmf-vars.fd"
if [ ! -f "${OVMF_VARS}" ]; then
    # Find OVMF_VARS template
    VARS_SRC=""
    for path in \
        /usr/share/OVMF/OVMF_VARS.fd \
        /usr/share/edk2/ovmf/OVMF_VARS.fd \
        /usr/share/qemu/OVMF_VARS.fd \
        /usr/share/edk2-ovmf/x64/OVMF_VARS.fd \
        /usr/share/OVMF/OVMF_VARS_4M.fd; do
        if [ -f "$path" ]; then
            VARS_SRC="$path"
            break
        fi
    done
    if [ -n "${VARS_SRC}" ]; then
        cp "${VARS_SRC}" "${OVMF_VARS}"
    else
        # Create empty vars
        dd if=/dev/zero of="${OVMF_VARS}" bs=1M count=4 2>/dev/null
    fi
fi

MODE="image"
IMAGE=""

if [ "${1:-}" = "--kernel" ]; then
    MODE="kernel"
elif [ -n "${1:-}" ] && [ -f "${1}" ]; then
    IMAGE="$1"
fi

case "${MODE}" in
    kernel)
        KERNEL="${BUILD_DIR}/vmlinuz"
        if [ ! -f "${KERNEL}" ]; then
            echo "ERROR: Kernel not found at ${KERNEL}"
            echo "Run: make-native-kiosk.sh hello"
            exit 1
        fi
        echo "Booting kernel directly: ${KERNEL}"
        qemu-system-x86_64 \
            -machine q35,accel=kvm:tcg \
            -cpu max \
            -m 512 \
            -kernel "${KERNEL}" \
            -append "console=ttyS0 console=tty0 loglevel=4 init=/init" \
            -drive if=pflash,format=raw,readonly=on,file="${OVMF}" \
            -drive if=pflash,format=raw,file="${OVMF_VARS}" \
            -device virtio-vga \
            -device virtio-keyboard-pci \
            -device virtio-tablet-pci \
            -device intel-hda -device hda-duplex \
            -serial stdio \
            -no-reboot
        ;;
    image)
        if [ -z "${IMAGE}" ]; then
            IMAGE="${BUILD_DIR}/ac-native.img"
        fi
        if [ ! -f "${IMAGE}" ]; then
            echo "ERROR: Image not found at ${IMAGE}"
            echo "Run: make-native-kiosk.sh hello --image ${IMAGE}"
            exit 1
        fi
        echo "Booting image: ${IMAGE}"
        qemu-system-x86_64 \
            -machine q35,accel=kvm:tcg \
            -cpu max \
            -m 512 \
            -drive if=pflash,format=raw,readonly=on,file="${OVMF}" \
            -drive if=pflash,format=raw,file="${OVMF_VARS}" \
            -drive file="${IMAGE}",format=raw,if=virtio \
            -device virtio-vga \
            -device virtio-keyboard-pci \
            -device virtio-tablet-pci \
            -device intel-hda -device hda-duplex \
            -serial stdio \
            -no-reboot
        ;;
esac
