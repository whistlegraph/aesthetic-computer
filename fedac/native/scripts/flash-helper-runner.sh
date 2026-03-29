#!/bin/bash
set -euo pipefail

source /usr/local/lib/ac-media-layout.sh

USB_DEV="${1:?usage: flash-helper-runner.sh <usb-dev> <staged-root>}"
STAGED_ROOT="${2:?usage: flash-helper-runner.sh <usb-dev> <staged-root>}"

log()  { echo "[flash-helper] $*"; }
err()  { echo "[flash-helper] $*" >&2; }

part_path() {
    local dev="$1"
    local idx="$2"
    if [[ "${dev}" =~ [0-9]$ ]]; then
        printf '%sp%s\n' "${dev}" "${idx}"
    else
        printf '%s%s\n' "${dev}" "${idx}"
    fi
}

mount_vfat_partition() {
    local dev="$1"
    local mountpoint="$2"
    mkdir -p "${mountpoint}"
    mount -t vfat "${dev}" "${mountpoint}"
}

mount_hfs_partition() {
    local dev="$1"
    local mountpoint="$2"
    mkdir -p "${mountpoint}"
    if mount -t hfsplus "${dev}" "${mountpoint}" 2>/dev/null; then
        return 0
    fi
    err "Failed to mount ${dev} as hfsplus"
    err "Available filesystems:"
    cat /proc/filesystems >&2 || true
    return 1
}

wait_for_partition() {
    local part="$1"
    for _ in $(seq 1 40); do
        if [ -b "${part}" ]; then
            return 0
        fi
        sleep 0.25
    done
    err "Partition did not appear: ${part}"
    return 1
}

cleanup() {
    umount /mnt/ac-main 2>/dev/null || true
    umount /mnt/ac-efi 2>/dev/null || true
    umount /mnt/ac-mac 2>/dev/null || true
}
trap cleanup EXIT

copy_boot_tree_to_vfat() {
    local dev="$1"
    local mountpoint="$2"
    local include_config="${3:-no}"
    local boot_mode="${4:-chainloader}"

    mount_vfat_partition "${dev}" "${mountpoint}"
    mkdir -p "${mountpoint}/EFI/BOOT"
    case "${boot_mode}" in
        chainloader)
            cp "${STAGED_ROOT}/EFI/BOOT/BOOTX64.EFI" "${mountpoint}/EFI/BOOT/BOOTX64.EFI"
            cp "${STAGED_ROOT}/EFI/BOOT/KERNEL.EFI" "${mountpoint}/EFI/BOOT/KERNEL.EFI"
            ;;
        kernel-only)
            cp "${STAGED_ROOT}/EFI/BOOT/KERNEL.EFI" "${mountpoint}/EFI/BOOT/KERNEL.EFI"
            rm -f "${mountpoint}/EFI/BOOT/BOOTX64.EFI"
            ;;
        *)
            err "Unknown VFAT boot mode: ${boot_mode}"
            return 1
            ;;
    esac
    if [ "${include_config}" = "yes" ]; then
        cp "${STAGED_ROOT}/config.json" "${mountpoint}/config.json"
    fi
    sync
    umount "${mountpoint}"
}

populate_mac_partition() {
    local dev="$1"
    local mountpoint="$2"

    mount_hfs_partition "${dev}" "${mountpoint}"

    mkdir -p "${mountpoint}/System/Library/CoreServices"

    cp "${STAGED_ROOT}/EFI/BOOT/KERNEL.EFI" "${mountpoint}/System/Library/CoreServices/boot.efi"

    if [ -f /boot/efi/System/Library/CoreServices/SystemVersion.plist ]; then
        cp /boot/efi/System/Library/CoreServices/SystemVersion.plist \
            "${mountpoint}/System/Library/CoreServices/SystemVersion.plist"
    fi
    if [ -f /boot/efi/.VolumeIcon.icns ]; then
        cp /boot/efi/.VolumeIcon.icns "${mountpoint}/.VolumeIcon.icns"
    fi
    if [ -f /boot/efi/mach_kernel ]; then
        cp /boot/efi/mach_kernel "${mountpoint}/mach_kernel"
    fi

    hfs-bless "${mountpoint}/System/Library/CoreServices/boot.efi"
    sync
    umount "${mountpoint}"
}

verify_partition_layout() {
    log "Partition layout:"
    fdisk -l "${USB_DEV}" 2>/dev/null || true
    blkid "${USB_DEV}"* 2>/dev/null || true
    sgdisk --print-mbr "${USB_DEV}" 2>/dev/null || true
}

verify_written_media() {
    local main_part="$1"
    local efi_part="$2"
    local mac_part="$3"

    mount_vfat_partition "${main_part}" /mnt/ac-main
    log "Main config: $(ac_media_summarize_config_file /mnt/ac-main/config.json || echo config=unreadable)"
    test ! -f /mnt/ac-main/EFI/BOOT/BOOTX64.EFI
    sha256sum /mnt/ac-main/EFI/BOOT/KERNEL.EFI "${STAGED_ROOT}/EFI/BOOT/KERNEL.EFI"
    umount /mnt/ac-main

    mount_vfat_partition "${efi_part}" /mnt/ac-efi
    test ! -f /mnt/ac-efi/EFI/BOOT/BOOTX64.EFI
    sha256sum /mnt/ac-efi/EFI/BOOT/KERNEL.EFI "${STAGED_ROOT}/EFI/BOOT/KERNEL.EFI"
    umount /mnt/ac-efi

    mount_hfs_partition "${mac_part}" /mnt/ac-mac
    test ! -f /mnt/ac-mac/EFI/BOOT/BOOTX64.EFI
    test ! -f /mnt/ac-mac/EFI/BOOT/KERNEL.EFI
    sha256sum /mnt/ac-mac/System/Library/CoreServices/boot.efi "${STAGED_ROOT}/EFI/BOOT/KERNEL.EFI"
    umount /mnt/ac-mac
}

if [ ! -b "${USB_DEV}" ]; then
    err "${USB_DEV} is not a block device"
    exit 1
fi

if [ ! -d "${STAGED_ROOT}/EFI/BOOT" ] || [ ! -f "${STAGED_ROOT}/config.json" ]; then
    err "Staged boot tree missing expected files at ${STAGED_ROOT}"
    exit 1
fi

MAIN_PART="$(part_path "${USB_DEV}" 1)"
EFI_PART="$(part_path "${USB_DEV}" 2)"
MAC_PART="$(part_path "${USB_DEV}" 3)"

STAGE_MB=$(ac_media_stage_tree_size_mib "${STAGED_ROOT}")
EFI_MB=$(( STAGE_MB + 96 ))
MAC_MB=$(( STAGE_MB + 128 ))
DISK_MB=$(( $(blockdev --getsize64 "${USB_DEV}") / 1048576 ))
MAIN_MB=$(( DISK_MB - EFI_MB - MAC_MB - 32 ))

if [ "${MAIN_MB}" -lt $(( STAGE_MB + 128 )) ]; then
    err "USB device is too small for hybrid layout (${DISK_MB}MB total)"
    exit 1
fi

log "Preparing hybrid Intel Mac media on ${USB_DEV}"
log "Sizes: main=${MAIN_MB}MB efi=${EFI_MB}MB mac=${MAC_MB}MB"

wipefs -a "${USB_DEV}" >/dev/null 2>&1 || true
sgdisk --zap-all "${USB_DEV}" >/dev/null 2>&1 || true
dd if=/dev/zero of="${USB_DEV}" bs=1M count=16 status=none

cat <<PART_EOF | sfdisk --force "${USB_DEV}"
label: gpt
size=${MAIN_MB}M, type=EBD0A0A2-B9E5-4433-87C0-68B6B72699C7, name="ACBOOT"
size=${EFI_MB}M, type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, name="ACEFI"
size=${MAC_MB}M, type=48465300-0000-11AA-AA11-00306543ECAC, name="AC-MAC"
PART_EOF

partprobe "${USB_DEV}" 2>/dev/null || true
sleep 2

wait_for_partition "${MAIN_PART}"
wait_for_partition "${EFI_PART}"
wait_for_partition "${MAC_PART}"

mkfs.vfat -F 32 -n ACBOOT "${MAIN_PART}" >/dev/null
mkfs.vfat -F 32 -n ACEFI "${EFI_PART}" >/dev/null
mkfs.hfsplus -v AC-MAC "${MAC_PART}" >/dev/null

sgdisk --attributes=1:set:62 "${USB_DEV}" >/dev/null 2>&1 || true
sgdisk --attributes=2:set:62 "${USB_DEV}" >/dev/null 2>&1 || true
sgdisk --hybrid 3 "${USB_DEV}" >/dev/null 2>&1 || true
partprobe "${USB_DEV}" 2>/dev/null || true

copy_boot_tree_to_vfat "${MAIN_PART}" /mnt/ac-main yes kernel-only
copy_boot_tree_to_vfat "${EFI_PART}" /mnt/ac-efi no kernel-only
populate_mac_partition "${MAC_PART}" /mnt/ac-mac

sync
sleep 2
sync

verify_partition_layout
verify_written_media "${MAIN_PART}" "${EFI_PART}" "${MAC_PART}"

echo "Flashed!"
