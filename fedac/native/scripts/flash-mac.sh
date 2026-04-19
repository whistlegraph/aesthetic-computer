#!/bin/bash
# flash-mac.sh — macOS-native AC Native OS USB flasher (no Docker required).
#
# Mirrors the production scripts/flash-helper-runner.sh layout using only
# macOS CLI tools (diskutil, sgdisk, newfs_msdos, mount_msdos, cp, shasum).
#
# Two partitions, matching the Linux flow:
#   1. ACBOOT (FAT32, type "Microsoft Basic Data")
#      - kernel-direct boot: /EFI/BOOT/BOOTX64.EFI = full kernel
#      - /initramfs.cpio.gz at root
#      - /config.json
#      - Works on permissive PC firmware that scans non-ESP partitions for
#        the standard UEFI fallback path.
#
#   2. ACEFI (FAT32, type "EFI System Partition")
#      - splash.efi as /EFI/BOOT/BOOTX64.EFI (chains to LOADER.EFI)
#      - systemd-bootx64.efi as /EFI/BOOT/LOADER.EFI
#      - kernel as /EFI/BOOT/KERNEL.EFI
#      - /initramfs.cpio.gz at root
#      - /loader/entries/ac-native.conf with explicit cmdline
#      - Universal: works on any UEFI firmware that respects ESP type GUID.
#
# Skipped vs Linux helper:
#   - AC-MAC HFS+ partition for Intel Mac compatibility (TODO)
#
# Usage:
#   ./flash-mac.sh /dev/diskN [SRC_DIR]
#
# SRC_DIR defaults to /tmp/ac-os-pull (where ac-os pull stages downloads).
# Must contain `vmlinuz` and `initramfs.cpio.gz`.

set -euo pipefail

USB_DEV="${1:?usage: $0 /dev/diskN [SRC_DIR]}"
SRC_DIR="${2:-/tmp/ac-os-pull}"

# This script needs root for diskutil/sgdisk/dd/newfs_msdos/mount_msdos.
# Re-exec under sudo if invoked as a regular user (sudoers.d/ac-flash-mac
# whitelists this exact path NOPASSWD).
if [ "$(id -u)" != "0" ]; then
    exec sudo --preserve-env=PATH "$0" "$@"
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
NATIVE_DIR="${REPO_ROOT}/native"
[ -d "${NATIVE_DIR}/boot" ] || NATIVE_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

KERNEL="${SRC_DIR}/vmlinuz"
INITRAMFS="${SRC_DIR}/initramfs.cpio.gz"
SPLASH_EFI="${NATIVE_DIR}/bootloader/splash.efi"
SDBOOT_EFI="${NATIVE_DIR}/boot/systemd-bootx64.efi"

log() { echo "[flash-mac] $*"; }
err() { echo "[flash-mac] $*" >&2; }
die() { err "$*"; exit 1; }

# --- preflight ---
[ "$(uname)" = "Darwin" ]    || die "macOS only — use ac-os flash on Linux."
command -v sgdisk >/dev/null || die "sgdisk required: brew install gptfdisk"
[ -f "${KERNEL}" ]           || die "Missing kernel: ${KERNEL}"
[ -f "${INITRAMFS}" ]        || die "Missing initramfs: ${INITRAMFS}"
[ -f "${SPLASH_EFI}" ]       || die "Missing splash bootloader: ${SPLASH_EFI}"
[ -f "${SDBOOT_EFI}" ]       || die "Missing systemd-boot: ${SDBOOT_EFI}"

# --- read ac-login token (~/.ac-token) for handle/sub/email injection ---
# When invoked as root via sudo, $HOME points at /var/root. Use SUDO_USER's
# home so we read the actual operator's token, not the empty root one.
TOKEN_HOME="${HOME}"
[ -n "${SUDO_USER:-}" ] && TOKEN_HOME="$(eval echo ~${SUDO_USER})"
TOKEN_FILE="${TOKEN_HOME}/.ac-token"

USER_HANDLE=""; USER_SUB=""; USER_EMAIL=""
if [ -f "${TOKEN_FILE}" ] && command -v node >/dev/null 2>&1; then
    eval "$(node -e '
        const t = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
        let h = t.user?.handle || t.user?.name || "";
        if (h.startsWith("@")) h = h.slice(1);
        const out = (k, v) => process.stdout.write(`${k}=${JSON.stringify(v || "")}\n`);
        out("USER_HANDLE", h);
        out("USER_SUB",    t.user?.sub);
        out("USER_EMAIL",  t.user?.email);
    ' "${TOKEN_FILE}" 2>/dev/null)"
    [ -n "${USER_HANDLE}" ] && log "Authenticated as @${USER_HANDLE}"
fi
[ -z "${USER_HANDLE}${USER_SUB}${USER_EMAIL}" ] && \
    log "No ~/.ac-token (run \`ac-login\` first to bake credentials in)"

INFO=$(diskutil info "${USB_DEV}" 2>/dev/null) || die "diskutil info failed for ${USB_DEV}"
echo "${INFO}" | grep -q "Removable Media:.*Removable\|Device Location:.*External" \
    || die "${USB_DEV} is not removable/external. Aborting."
DEV_NAME=$(echo "${INFO}" | awk -F': +' '/Device \/ Media Name/{print $2}' | head -1)
DEV_SIZE=$(echo "${INFO}" | awk -F': +' '/Disk Size/{print $2}' | head -1)
DEV_BYTES=$(echo "${INFO}" | awk -F': +' '/Disk Size:/ {print $2}' | grep -oE '\([0-9]+ Bytes\)' | head -1 | tr -dc 0-9)

# --- size budgeting (matches Linux helper math) ---
KERNEL_BYTES=$(stat -f%z "${KERNEL}")
INITRD_BYTES=$(stat -f%z "${INITRAMFS}")
KERNEL_SHA=$(shasum -a 256 "${KERNEL}"    | awk '{print $1}')
INITRD_SHA=$(shasum -a 256 "${INITRAMFS}" | awk '{print $1}')

mb_round_up() { echo $(( ($1 + 1048575) / 1048576 )); }
DISK_MB=$(( DEV_BYTES / 1048576 ))
STAGE_MB=$(( $(mb_round_up "${KERNEL_BYTES}") + $(mb_round_up "${INITRD_BYTES}") + 8 ))
EFI_MB=$(( STAGE_MB + 96 ))
MAIN_MB=$(( DISK_MB - EFI_MB - 64 ))   # 64 MB GPT + alignment headroom

[ "${MAIN_MB}" -ge $(( STAGE_MB + 64 )) ] \
    || die "USB too small (${DISK_MB} MB) for hybrid layout."

echo
log "Target: ${USB_DEV} — ${DEV_NAME} — ${DEV_SIZE}"
log "Kernel:    ${KERNEL_BYTES} bytes  ${KERNEL_SHA:0:16}…"
log "Initramfs: ${INITRD_BYTES} bytes  ${INITRD_SHA:0:16}…"
FREE_MB=$(( DISK_MB - MAIN_MB - EFI_MB - 64 ))
log "Layout:    ACBOOT=${MAIN_MB}MB  ACEFI=${EFI_MB}MB  free=${FREE_MB}MB"
echo
read -r -p "Type 'YES' to ERASE ${USB_DEV} and write AC Native OS: " CONFIRM
[ "${CONFIRM}" = "YES" ] || die "Aborted."

# --- wipe + repartition ---
log "Unmounting…"
diskutil unmountDisk force "${USB_DEV}" >/dev/null

log "Zapping GPT + clearing first 16 MiB…"
sgdisk --zap-all "${USB_DEV}" >/dev/null
dd if=/dev/zero of="${USB_DEV}" bs=1m count=16 status=none

log "Creating GPT layout (ACBOOT + ACEFI)…"
sgdisk \
    --new=1:0:+${MAIN_MB}M --typecode=1:0700 --change-name=1:ACBOOT \
    --new=2:0:0           --typecode=2:ef00 --change-name=2:ACEFI \
    "${USB_DEV}" >/dev/null

# Force macOS to re-read the partition table after sgdisk wrote it. The
# kernel caches the old layout until we explicitly notify it; without this,
# the s1/s2 nodes either don't exist yet or still point at the pre-zap
# partitions and newfs_msdos errors with "No such file or directory".
diskutil unmountDisk force "${USB_DEV}" >/dev/null 2>&1 || true
diskutil list "${USB_DEV}" >/dev/null 2>&1 || true

P1="${USB_DEV}s1"
P2="${USB_DEV}s2"
RAW1="/dev/r$(basename "${P1}")"
RAW2="/dev/r$(basename "${P2}")"

# Wait up to 10s for both partition nodes to materialize.
for i in $(seq 1 20); do
    [ -e "${P1}" ] && [ -e "${P2}" ] && break
    sleep 0.5
    diskutil list "${USB_DEV}" >/dev/null 2>&1 || true
done
[ -e "${P1}" ] || die "Partition ${P1} did not appear after sgdisk + reread."
[ -e "${P2}" ] || die "Partition ${P2} did not appear after sgdisk + reread."

log "Formatting FAT32 partitions…"
newfs_msdos -F 32 -v ACBOOT "${RAW1}" >/dev/null
newfs_msdos -F 32 -v ACEFI  "${RAW2}" >/dev/null

# --- mount ---
M1=$(mktemp -d /tmp/ac-main.XXXXXX)
M2=$(mktemp -d /tmp/ac-efi.XXXXXX)
trap "umount '${M1}' 2>/dev/null; umount '${M2}' 2>/dev/null; rmdir '${M1}' '${M2}' 2>/dev/null; true" EXIT

log "Mounting partitions…"
mount_msdos "${P1}" "${M1}"
mount_msdos "${P2}" "${M2}"

# --- layout ACBOOT (kernel-direct + config) ---
log "Writing ACBOOT (kernel-direct boot tree)…"
mkdir -p "${M1}/EFI/BOOT"
cp "${KERNEL}"   "${M1}/EFI/BOOT/BOOTX64.EFI"
cp "${INITRAMFS}" "${M1}/initramfs.cpio.gz"
printf '{"handle":"%s","piece":"notepat","sub":"%s","email":"%s"}\n' \
    "${USER_HANDLE}" "${USER_SUB}" "${USER_EMAIL}" | tee "${M1}/config.json" >/dev/null
[ -f "${SRC_DIR}/wifi_creds.json" ] && cp "${SRC_DIR}/wifi_creds.json" "${M1}/wifi_creds.json"

# --- layout ACEFI (systemd-boot universal) ---
log "Writing ACEFI (splash → systemd-boot → kernel)…"
mkdir -p "${M2}/EFI/BOOT" "${M2}/loader/entries"
cp "${SPLASH_EFI}" "${M2}/EFI/BOOT/BOOTX64.EFI"
cp "${SDBOOT_EFI}" "${M2}/EFI/BOOT/LOADER.EFI"
cp "${KERNEL}"     "${M2}/EFI/BOOT/KERNEL.EFI"
cp "${INITRAMFS}"   "${M2}/initramfs.cpio.gz"
tee "${M2}/loader/loader.conf" >/dev/null <<'EOF'
default ac-native.conf
timeout 0
EOF
tee "${M2}/loader/entries/ac-native.conf" >/dev/null <<'EOF'
title AC Native OS
linux /EFI/BOOT/KERNEL.EFI
initrd /initramfs.cpio.gz
options console=tty0 quiet loglevel=3 vt.global_cursor_default=0 init=/init nomodeset efi=noruntime
EOF
printf '{"handle":"%s","piece":"notepat","sub":"%s","email":"%s"}\n' \
    "${USER_HANDLE}" "${USER_SUB}" "${USER_EMAIL}" | tee "${M2}/config.json" >/dev/null
[ -f "${SRC_DIR}/wifi_creds.json" ] && cp "${SRC_DIR}/wifi_creds.json" "${M2}/wifi_creds.json"

# --- verify (sha256 round-trip on every kernel + initramfs copy) ---
log "Verifying integrity…"
verify() {
    local label="$1" path="$2" expected="$3"
    local got
    got=$(shasum -a 256 "${path}" | awk '{print $1}')
    [ "${got}" = "${expected}" ] || die "${label} sha mismatch (${got} != ${expected})"
    log "  ✓ ${label}  ${got:0:16}…"
}
verify "ACBOOT/EFI/BOOT/BOOTX64.EFI" "${M1}/EFI/BOOT/BOOTX64.EFI"  "${KERNEL_SHA}"
verify "ACBOOT/initramfs.cpio.gz"   "${M1}/initramfs.cpio.gz"      "${INITRD_SHA}"
verify "ACEFI/EFI/BOOT/KERNEL.EFI"  "${M2}/EFI/BOOT/KERNEL.EFI"    "${KERNEL_SHA}"
verify "ACEFI/initramfs.cpio.gz"    "${M2}/initramfs.cpio.gz"      "${INITRD_SHA}"

# --- finalize ---
sync
log "Unmounting + ejecting…"
umount "${M1}" "${M2}"
diskutil eject "${USB_DEV}" >/dev/null
trap - EXIT
rmdir "${M1}" "${M2}" 2>/dev/null || true

log "Done. USB has both kernel-direct (ACBOOT) + systemd-boot (ACEFI) layouts."
log "Plug into target hardware and boot — UEFI firmware should pick ACEFI (real ESP)."
