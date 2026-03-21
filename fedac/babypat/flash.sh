#!/usr/bin/env bash
# Flash babypat to USB — uses same Docker+mtools pattern as ac-os
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EFI="${SCRIPT_DIR}/babypat-tiny.efi"

if [ ! -f "${EFI}" ]; then
    echo "Build first: make tiny"
    exit 1
fi

echo "babypat-tiny.efi: $(wc -c < "${EFI}" | tr -d ' ') bytes"

# Find USB device
find_usb() {
    for dev in sda sdb sdc; do
        if [ -e "/sys/block/${dev}/removable" ]; then
            local rem=$(cat "/sys/block/${dev}/removable" 2>/dev/null)
            if [ "${rem}" = "1" ]; then echo "/dev/${dev}"; return 0; fi
        fi
    done
    return 1
}

USB_DEV="${1:-}"
if [ -z "${USB_DEV}" ]; then
    USB_DEV="$(find_usb)" || true
fi

if [ -z "${USB_DEV}" ]; then
    echo "No USB device found."
    echo "Usage: ./flash.sh [/dev/sdX]"
    echo ""
    echo "If running in a devcontainer, try flashing via the host:"
    echo "  ssh me@172.17.0.1"
    echo "  cd aesthetic-computer/fedac/babypat"
    echo "  sudo ./flash.sh"
    exit 1
fi

echo "Flashing babypat to ${USB_DEV}..."
echo "Press Ctrl+C within 3s to cancel."
sleep 3

# Use Docker container for privileged disk access (works in devcontainers)
CONTAINER=$(sudo docker create --rm --privileged -v /dev:/dev alpine:latest sh -c "
    set -e
    apk add --quiet dosfstools util-linux mtools

    echo 'Partitioning ${USB_DEV}...'
    dd if=/dev/zero of=${USB_DEV} bs=1M count=4 2>/dev/null
    sfdisk --force ${USB_DEV} <<PARTEOF
label: gpt
type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=32M
PARTEOF
    sleep 2
    partprobe ${USB_DEV} 2>/dev/null || true
    sleep 1

    mkfs.vfat -F 12 -n BABYPAT ${USB_DEV}1
    mount ${USB_DEV}1 /mnt
    mkdir -p /mnt/EFI/BOOT
    cp /tmp/babypat.efi /mnt/EFI/BOOT/BOOTX64.EFI
    sync
    ls -la /mnt/EFI/BOOT/
    umount /mnt
    echo 'Done! Babypat flashed to ${USB_DEV}'
")

sudo docker cp "${EFI}" "${CONTAINER}:/tmp/babypat.efi"
sudo docker start -a "${CONTAINER}" 2>&1
