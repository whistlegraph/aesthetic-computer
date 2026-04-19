#!/bin/bash
# flash-mac.sh — macOS-native AC Native OS USB flasher.
#
# Mirrors the "kernel-direct" boot mode from scripts/flash-helper-runner.sh
# (Linux/Docker) using only macOS CLI tools — no container required.
# Lays out a single ESP partition with the kernel installed as the standard
# UEFI fallback path (/EFI/BOOT/BOOTX64.EFI) plus the external initramfs at
# the partition root, matching the kernel's CONFIG_CMDLINE which loads
# `initrd=\initramfs.cpio.gz` from the ESP.
#
# Usage:
#   ./flash-mac.sh /dev/diskN [SRC_DIR]
#
# SRC_DIR defaults to /tmp/ac-os-pull (where ac-os pull stages downloads).
# It must contain `vmlinuz` and `initramfs.cpio.gz`.
#
# Requires sudo for diskutil destructive ops.

set -euo pipefail

USB_DEV="${1:?usage: $0 /dev/diskN [SRC_DIR]}"
SRC_DIR="${2:-/tmp/ac-os-pull}"
PART_NAME="${PART_NAME:-ACBOOT}"

KERNEL="${SRC_DIR}/vmlinuz"
INITRAMFS="${SRC_DIR}/initramfs.cpio.gz"

log()  { echo "[flash-mac] $*"; }
err()  { echo "[flash-mac] $*" >&2; }
die()  { err "$*"; exit 1; }

# --- preflight ---
[ "$(uname)" = "Darwin" ] || die "macOS only — use ac-os flash on Linux."
[ -b "${USB_DEV}" ] || [ -e "${USB_DEV}" ] || die "Not a device: ${USB_DEV}"
[ -f "${KERNEL}" ]   || die "Missing kernel: ${KERNEL}"
[ -f "${INITRAMFS}" ] || die "Missing initramfs: ${INITRAMFS}"

# Refuse to flash internal disks (would brick the Mac).
INFO=$(diskutil info "${USB_DEV}" 2>/dev/null) || die "diskutil info failed for ${USB_DEV}"
echo "${INFO}" | grep -q "Removable Media:.*Removable\|Device Location:.*External" \
    || die "${USB_DEV} does not look removable/external. Aborting."
DEV_NAME=$(echo "${INFO}" | awk -F': +' '/Device \/ Media Name/{print $2}' | head -1)
DEV_SIZE=$(echo "${INFO}" | awk -F': +' '/Disk Size/{print $2}' | head -1)

KERNEL_SHA=$(shasum -a 256 "${KERNEL}"   | awk '{print $1}')
INITRD_SHA=$(shasum -a 256 "${INITRAMFS}" | awk '{print $1}')
KERNEL_SZ=$(stat -f%z "${KERNEL}")
INITRD_SZ=$(stat -f%z "${INITRAMFS}")

echo
log "Target: ${USB_DEV} — ${DEV_NAME} — ${DEV_SIZE}"
log "Kernel:    ${KERNEL_SZ} bytes  ${KERNEL_SHA:0:16}…"
log "Initramfs: ${INITRD_SZ} bytes  ${INITRD_SHA:0:16}…"
echo
read -r -p "Type 'YES' to ERASE ${USB_DEV} and write AC Native OS: " CONFIRM
[ "${CONFIRM}" = "YES" ] || die "Aborted."

# --- partition + format ---
log "Unmounting…"
diskutil unmountDisk force "${USB_DEV}" >/dev/null

log "Erasing + creating GPT/FAT32 …"
# diskutil eraseDisk MS-DOS+GPT creates an EFI helper at s1 (200MB, ESP)
# and FAT32 at s2 (rest). For kernel-direct boot we need a SINGLE ESP that
# fits both kernel + initramfs (~340 MB). Use partitionDisk to lay down a
# single FAT32 spanning the disk, then convert its type GUID to ESP so
# UEFI firmware will scan it for /EFI/BOOT/BOOTX64.EFI.
sudo diskutil partitionDisk "${USB_DEV}" 1 GPT MS-DOS "${PART_NAME}" 100% >/dev/null

# After partitionDisk with 1 partition, layout is:
#   ${USB_DEV}s1 — EFI helper (200 MB FAT32, real ESP — too small for initramfs)
#   ${USB_DEV}s2 — ${PART_NAME} (30+ GB FAT32, type "Microsoft Basic Data")
#
# Most UEFI firmware will only fall back to /EFI/BOOT/BOOTX64.EFI on a
# partition typed as ESP (GUID c12a7328-f81f-11d2-ba4b-00a0c93ec93b).
# `diskutil` on macOS has no first-class way to change a partition's type
# GUID without reformatting, so we shell out to `sgdisk` (gptfdisk).
DATA_PART="${USB_DEV}s2"
log "Setting ${DATA_PART} type to EFI System Partition…"
diskutil unmount "${DATA_PART}" >/dev/null 2>&1 || true
if command -v sgdisk >/dev/null 2>&1; then
    # sgdisk type code "ef00" = EFI System Partition.
    sudo sgdisk -t 2:ef00 "${USB_DEV}" >/dev/null
    # Re-read the partition table so macOS sees the new type.
    sudo /usr/sbin/diskutil unmountDisk force "${USB_DEV}" >/dev/null 2>&1 || true
    log "  ✓ s2 retyped to ESP via sgdisk"
else
    err "  sgdisk not found — install with: brew install gptfdisk"
    err "  Without ESP type, target firmware will probably skip the boot partition."
    err "  Continuing — verify the boot afterwards."
fi

# Re-mount data partition
diskutil mount "${DATA_PART}" >/dev/null
MOUNT="/Volumes/${PART_NAME}"
[ -d "${MOUNT}" ] || die "Expected mount at ${MOUNT}"

# --- lay out kernel-direct boot tree ---
log "Writing boot tree to ${MOUNT}…"
mkdir -p "${MOUNT}/EFI/BOOT"
cp "${KERNEL}"   "${MOUNT}/EFI/BOOT/BOOTX64.EFI"
cp "${INITRAMFS}" "${MOUNT}/initramfs.cpio.gz"

# Default identity / config — overwrite with personal config later if needed.
cat > "${MOUNT}/config.json" <<'EOF'
{"handle":"","piece":"notepat","sub":"","email":""}
EOF

# Optional wifi presets — copy from local file if user has one staged
if [ -f "${SRC_DIR}/wifi_creds.json" ]; then
    cp "${SRC_DIR}/wifi_creds.json" "${MOUNT}/wifi_creds.json"
    log "Included wifi_creds.json from ${SRC_DIR}"
fi

# --- verify integrity (sha256 round-trip) ---
log "Verifying…"
WK_SHA=$(shasum -a 256 "${MOUNT}/EFI/BOOT/BOOTX64.EFI" | awk '{print $1}')
WI_SHA=$(shasum -a 256 "${MOUNT}/initramfs.cpio.gz"     | awk '{print $1}')
[ "${WK_SHA}" = "${KERNEL_SHA}" ] || die "kernel sha mismatch (got ${WK_SHA})"
[ "${WI_SHA}" = "${INITRD_SHA}" ] || die "initramfs sha mismatch (got ${WI_SHA})"
log "  ✓ kernel    ${WK_SHA:0:16}…"
log "  ✓ initramfs ${WI_SHA:0:16}…"

# --- eject ---
sync
log "Ejecting ${USB_DEV}…"
diskutil eject "${USB_DEV}" >/dev/null

log "Done. USB ready. Boot the AC native device from this drive."
