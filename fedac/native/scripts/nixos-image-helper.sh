#!/bin/bash
set -euo pipefail

source /usr/local/lib/ac-media-layout.sh

IMAGE_PATH="${1:?usage: nixos-image-helper.sh <image-path> <config-json>}"
CONFIG_JSON_PATH="${2:?usage: nixos-image-helper.sh <image-path> <config-json>}"

log() { echo "[nixos-image-helper] $*"; }
err() { echo "[nixos-image-helper] $*" >&2; }

partition_sector_field() {
    local image_path="$1"
    local part_number="$2"
    local field_name="$3"
    python3 - "$image_path" "$part_number" "$field_name" <<'PYEOF'
import re
import subprocess
import sys

image_path, part_number, field_name = sys.argv[1:]
part_number = int(part_number)
dump = subprocess.check_output(["sfdisk", "-d", image_path], text=True, stderr=subprocess.DEVNULL)
pattern = re.compile(rf"{re.escape(image_path)}(\d+)\s*:(.*)")

for line in dump.splitlines():
    match = pattern.match(line.strip())
    if not match or int(match.group(1)) != part_number:
        continue
    fields = {}
    for field in match.group(2).split(","):
        field = field.strip()
        if "=" not in field:
            continue
        key, value = field.split("=", 1)
        fields[key.strip()] = value.strip().strip('"')
    value = fields.get(field_name)
    if value and re.fullmatch(r"\d+", value):
        print(value)
        raise SystemExit(0)
    raise SystemExit(1)

raise SystemExit(1)
PYEOF
}

partition_number_for_type() {
    local image_path="$1"
    local type_guid="$2"
    python3 - "$image_path" "$type_guid" <<'PYEOF'
import re
import subprocess
import sys

image_path, type_guid = sys.argv[1:]
dump = subprocess.check_output(["sfdisk", "-d", image_path], text=True, stderr=subprocess.DEVNULL)
pattern = re.compile(rf"{re.escape(image_path)}(\d+)\s*:.*type=([0-9A-Fa-f\-]+)")
for line in dump.splitlines():
    match = pattern.match(line.strip())
    if match and match.group(2).lower() == type_guid.lower():
        print(match.group(1))
        raise SystemExit(0)
raise SystemExit(1)
PYEOF
}

max_partition_number() {
    local image_path="$1"
    python3 - "$image_path" <<'PYEOF'
import re
import subprocess
import sys

image_path = sys.argv[1]
dump = subprocess.check_output(["sfdisk", "-d", image_path], text=True, stderr=subprocess.DEVNULL)
pattern = re.compile(rf"{re.escape(image_path)}(\d+)\s*:")
max_idx = 0
for line in dump.splitlines():
    match = pattern.match(line.strip())
    if match:
        max_idx = max(max_idx, int(match.group(1)))
print(max_idx)
PYEOF
}

append_partition() {
    local image_path="$1"
    local part_number="$2"
    local size_mib="$3"
    local type_guid="$4"
    local part_name="$5"
    local sector_size=512
    local gpt_tail_sectors=34
    local image_bytes
    local image_sectors
    local start_sector
    local size_sectors
    local total_sectors

    image_bytes=$(stat -c%s "${image_path}")
    image_sectors=$(( (image_bytes + sector_size - 1) / sector_size ))
    start_sector=$(( ((image_sectors + 2047) / 2048) * 2048 ))
    size_sectors=$(( size_mib * 1024 * 1024 / sector_size ))
    total_sectors=$(( start_sector + size_sectors + gpt_tail_sectors ))

    truncate -s $(( total_sectors * sector_size )) "${image_path}"
    sgdisk -e "${image_path}" >/dev/null
    printf 'start=%s, size=%s, type=%s, name="%s"\n' \
        "${start_sector}" "${size_sectors}" "${type_guid}" "${part_name}" |
        sfdisk --no-reread -N "${part_number}" "${image_path}" >/dev/null
}

wait_for_block_device() {
    local part="$1"
    for _ in $(seq 1 40); do
        [ -b "${part}" ] && return 0
        sleep 0.25
    done
    err "Partition did not appear: ${part}"
    return 1
}

bind_partition_loop() {
    local image_path="$1"
    local part_number="$2"
    local sector_size=512
    local start_sector
    local size_sectors
    local offset_bytes
    local size_bytes
    local loop_dev

    start_sector="$(partition_sector_field "${image_path}" "${part_number}" "start")"
    size_sectors="$(partition_sector_field "${image_path}" "${part_number}" "size")"
    offset_bytes=$(( start_sector * sector_size ))
    size_bytes=$(( size_sectors * sector_size ))

    loop_dev="$(losetup --find --show --offset "${offset_bytes}" --sizelimit "${size_bytes}" "${image_path}")"
    LOOP_DEVS+=("${loop_dev}")
    printf '%s\n' "${loop_dev}"
}

declare -a LOOP_DEVS=()
TMP_DIR=""
EFI_MOUNT=""
MAC_MOUNT=""
DATA_MOUNT=""

cleanup() {
    umount "${EFI_MOUNT:-}" 2>/dev/null || true
    umount "${MAC_MOUNT:-}" 2>/dev/null || true
    umount "${DATA_MOUNT:-}" 2>/dev/null || true
    if [ "${#LOOP_DEVS[@]}" -gt 0 ]; then
        local idx
        for (( idx=${#LOOP_DEVS[@]}-1; idx>=0; idx-- )); do
            losetup -d "${LOOP_DEVS[$idx]}" 2>/dev/null || true
        done
    fi
    rm -rf "${TMP_DIR:-}"
}
trap cleanup EXIT

if [ ! -f "${IMAGE_PATH}" ]; then
    err "Missing image: ${IMAGE_PATH}"
    exit 1
fi
if [ ! -f "${CONFIG_JSON_PATH}" ]; then
    err "Missing config JSON: ${CONFIG_JSON_PATH}"
    exit 1
fi

TMP_DIR="$(mktemp -d /tmp/ac-nixos-image.XXXXXX)"
LEGACY_CONFIG="${TMP_DIR}/config.json"
IDENTITY_FILE="${TMP_DIR}/$(ac_media_identity_filename)"

CONFIG_JSON="$(cat "${CONFIG_JSON_PATH}")"
ac_media_write_legacy_config "${LEGACY_CONFIG}" "${CONFIG_JSON}"
ac_media_write_identity_config "${IDENTITY_FILE}" "${CONFIG_JSON}"

EFI_PART_NUM="$(partition_number_for_type "${IMAGE_PATH}" "c12a7328-f81f-11d2-ba4b-00a0c93ec93b" || true)"
if [ -z "${EFI_PART_NUM}" ]; then
    err "No EFI partition found in ${IMAGE_PATH}"
    exit 1
fi

MAC_PART_NUM="$(partition_number_for_type "${IMAGE_PATH}" "48465300-0000-11aa-aa11-00306543ecac" || true)"
DATA_PART_NUM="$(partition_number_for_type "${IMAGE_PATH}" "ebd0a0a2-b9e5-4433-87c0-68b6b72699c7" || true)"
NEXT_PART_NUM=$(( $(max_partition_number "${IMAGE_PATH}") + 1 ))

if [ -z "${MAC_PART_NUM}" ]; then
    MAC_PART_NUM="${NEXT_PART_NUM}"
    append_partition "${IMAGE_PATH}" "${MAC_PART_NUM}" "${AC_NIXOS_MAC_PARTITION_MIB:-256}" \
        "48465300-0000-11AA-AA11-00306543ECAC" "AC-MAC"
    NEXT_PART_NUM=$(( NEXT_PART_NUM + 1 ))
fi

if [ -z "${DATA_PART_NUM}" ]; then
    DATA_PART_NUM="${NEXT_PART_NUM}"
    append_partition "${IMAGE_PATH}" "${DATA_PART_NUM}" "$(ac_media_nixos_data_size_mib)" \
        "EBD0A0A2-B9E5-4433-87C0-68B6B72699C7" "$(ac_media_nixos_data_label)"
fi

EFI_PART="$(bind_partition_loop "${IMAGE_PATH}" "${EFI_PART_NUM}")"
MAC_PART="$(bind_partition_loop "${IMAGE_PATH}" "${MAC_PART_NUM}")"
DATA_PART="$(bind_partition_loop "${IMAGE_PATH}" "${DATA_PART_NUM}")"

wait_for_block_device "${EFI_PART}"
wait_for_block_device "${MAC_PART}"
wait_for_block_device "${DATA_PART}"

if ! blkid -o value -s LABEL "${MAC_PART}" >/dev/null 2>&1; then
    mkfs.hfsplus -v AC-MAC "${MAC_PART}" >/dev/null
fi
if [ "$(blkid -o value -s LABEL "${DATA_PART}" 2>/dev/null || true)" != "$(ac_media_nixos_data_label)" ]; then
    mkfs.vfat -F 32 -n "$(ac_media_nixos_data_label)" "${DATA_PART}" >/dev/null
fi

EFI_MOUNT="${TMP_DIR}/efi"
MAC_MOUNT="${TMP_DIR}/mac"
DATA_MOUNT="${TMP_DIR}/data"
mkdir -p "${EFI_MOUNT}" "${MAC_MOUNT}" "${DATA_MOUNT}"

mount -t vfat "${EFI_PART}" "${EFI_MOUNT}"
mount -t vfat "${DATA_PART}" "${DATA_MOUNT}"
mkdir -p "${DATA_MOUNT}/logs"
cp "${LEGACY_CONFIG}" "${DATA_MOUNT}/config.json"
cp "${IDENTITY_FILE}" "${DATA_MOUNT}/$(ac_media_identity_filename)"
sync
umount "${DATA_MOUNT}"

if mount -t hfsplus "${MAC_PART}" "${MAC_MOUNT}" 2>/dev/null; then
    mkdir -p "${MAC_MOUNT}/System/Library/CoreServices" "${MAC_MOUNT}/EFI/BOOT"
    cp "${EFI_MOUNT}/EFI/BOOT/BOOTX64.EFI" "${MAC_MOUNT}/System/Library/CoreServices/boot.efi"
    cp "${EFI_MOUNT}/EFI/BOOT/BOOTX64.EFI" "${MAC_MOUNT}/EFI/BOOT/BOOTX64.EFI"
    cat > "${MAC_MOUNT}/System/Library/CoreServices/SystemVersion.plist" <<'PLIST_EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>ProductBuildVersion</key>
    <string></string>
    <key>ProductName</key>
    <string>Linux</string>
    <key>ProductVersion</key>
    <string>AC Native OS</string>
</dict>
</plist>
PLIST_EOF
    echo "Mach Kernel" > "${MAC_MOUNT}/mach_kernel"
    hfs-bless "${MAC_MOUNT}/System/Library/CoreServices/boot.efi" || true
    sync
    umount "${MAC_MOUNT}"
else
    log "Skipping AC-MAC population; hfsplus mount unavailable"
fi

umount "${EFI_MOUNT}"
sync
fsck.hfsplus -yrdfp "${MAC_PART}" 2>/dev/null || true
log "Prepared ${IMAGE_PATH} with AC-MAC and $(ac_media_nixos_data_label)"
