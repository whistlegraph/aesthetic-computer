#!/bin/bash

# Shared helpers for AC Native boot media layouts.

MEDIA_LAYOUT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MEDIA_LAYOUT_ROOT="$(cd "${MEDIA_LAYOUT_DIR}/.." && pwd)"

ac_media_identity_marker() {
    printf '%s\n' "AC_IDENTITY_BLOCK_V1"
}

ac_media_identity_size() {
    printf '%s\n' "32768"
}

ac_media_nixos_data_label() {
    printf '%s\n' "${AC_NIXOS_DATA_LABEL:-ACDATA}"
}

ac_media_nixos_data_size_mib() {
    printf '%s\n' "${AC_NIXOS_DATA_PARTITION_MIB:-512}"
}

ac_media_legacy_config_size() {
    printf '%s\n' "${AC_LEGACY_CONFIG_BYTES:-4096}"
}

ac_media_default_identity_json() {
    printf '%s' '{"handle":"","piece":"notepat","sub":"","email":""}'
}

ac_media_identity_filename() {
    printf '%s\n' "${AC_IDENTITY_FILENAME:-identity.bin}"
}

ac_media_bootloader_path() {
    local splash="${AC_SPLASH_EFI:-${MEDIA_LAYOUT_ROOT}/bootloader/splash.efi}"
    local splash_dir
    splash_dir="$(dirname "${splash}")"
    local splash_src="${splash_dir}/splash.c"
    local splash_font="${splash_dir}/font8x8.h"
    local splash_make="${splash_dir}/Makefile"

    if [ -f "${splash_make}" ] && [ -f "${splash_src}" ]; then
        if [ ! -f "${splash}" ] ||
           [ "${splash_src}" -nt "${splash}" ] ||
           { [ -f "${splash_font}" ] && [ "${splash_font}" -nt "${splash}" ]; } ||
           [ "${splash_make}" -nt "${splash}" ]; then
            (cd "${splash_dir}" && make all >/dev/null) || {
                echo "Failed to rebuild splash bootloader in ${splash_dir}" >&2
                return 1
            }
        fi
    fi
    if [ ! -f "${splash}" ]; then
        echo "Missing splash bootloader: ${splash}" >&2
        return 1
    fi
    printf '%s\n' "${splash}"
}

ac_media_stage_tree_size_mib() {
    local stage_root="$1"
    local size_mb
    size_mb=$(du -sm "${stage_root}" 2>/dev/null | awk '{print $1}')
    printf '%s\n' "${size_mb:-1}"
}

ac_stage_tree_size_mb() {
    ac_media_stage_tree_size_mib "$@"
}

ac_media_file_mib() {
    local file_path="$1"
    local bytes
    bytes=$(stat -c%s "${file_path}")
    printf '%s\n' $(( (bytes + 1048575) / 1048576 ))
}

ac_write_identity_block_config() {
    local out_path="$1"
    local marker="$2"
    local total_bytes="$3"
    local json_payload="$4"

    mkdir -p "$(dirname "${out_path}")"
    printf '%s\n' "${marker}" > "${out_path}"
    printf '%s' "${json_payload}" >> "${out_path}"

    local current_size
    current_size=$(stat -c%s "${out_path}")
    if [ "${current_size}" -gt "${total_bytes}" ]; then
        echo "Identity block payload exceeds ${total_bytes} bytes" >&2
        return 1
    fi

    local pad_size=$(( total_bytes - current_size ))
    dd if=/dev/zero bs=1 count="${pad_size}" status=none >> "${out_path}"
}

ac_media_write_identity_config() {
    local out_path="$1"
    local json_payload="${2:-$(ac_media_default_identity_json)}"

    ac_write_identity_block_config \
        "${out_path}" \
        "$(ac_media_identity_marker)" \
        "$(ac_media_identity_size)" \
        "${json_payload}"
}

ac_media_write_legacy_config() {
    local out_path="$1"
    local json_payload="${2:-$(ac_media_default_identity_json)}"
    local total_bytes
    local current_size
    local pad_size

    total_bytes="$(ac_media_legacy_config_size)"
    mkdir -p "$(dirname "${out_path}")"
    printf '%s' "${json_payload}" > "${out_path}"

    current_size=$(stat -c%s "${out_path}")
    if [ "${current_size}" -gt "${total_bytes}" ]; then
        echo "Legacy config payload exceeds ${total_bytes} bytes" >&2
        return 1
    fi

    pad_size=$(( total_bytes - current_size ))
    if [ "${pad_size}" -gt 0 ]; then
        head -c "${pad_size}" < /dev/zero | tr '\000' ' ' >> "${out_path}"
    fi
}

ac_media_write_global_wifi_creds() {
    local out_path="$1"

    mkdir -p "$(dirname "${out_path}")"
    # Keep this preset list in sync with the hardcoded fallbacks in src/wifi.c.
    cat > "${out_path}" <<'EOF'
[
  {"ssid":"aesthetic.computer","pass":"aesthetic.computer"},
  {"ssid":"ATT2AWTpcr","pass":"t84q%7%g2h8u"},
  {"ssid":"GettyLink","pass":""},
  {"ssid":"Tondo_Guest","pass":"California"}
]
EOF
}

ac_media_merge_wifi_creds() {
    local out_path="$1"
    local base_path="$2"
    local extra_path="${3:-}"

    if [ ! -f "${base_path}" ]; then
        echo "Missing base wifi creds: ${base_path}" >&2
        return 1
    fi

    if [ -z "${extra_path}" ] || [ ! -f "${extra_path}" ]; then
        cp "${base_path}" "${out_path}"
        return 0
    fi

    python3 - "${base_path}" "${extra_path}" "${out_path}" <<'PYEOF'
import json
import pathlib
import sys

base_path, extra_path, out_path = sys.argv[1:]

def read_creds(path):
    try:
        data = json.loads(pathlib.Path(path).read_text())
    except Exception:
        return []
    if not isinstance(data, list):
        return []

    creds = []
    for entry in data:
        if not isinstance(entry, dict):
            continue
        ssid = entry.get("ssid")
        password = entry.get("pass", "")
        if not isinstance(ssid, str) or not ssid:
            continue
        if not isinstance(password, str):
            password = ""
        creds.append({"ssid": ssid, "pass": password})
    return creds

merged = {}
order = []
for path in (base_path, extra_path):
    for entry in read_creds(path):
        ssid = entry["ssid"]
        if ssid not in merged:
            order.append(ssid)
        merged[ssid] = entry

pathlib.Path(out_path).write_text(
    json.dumps([merged[ssid] for ssid in order], indent=2) + "\n"
)
PYEOF
}

ac_media_summarize_wifi_creds_file() {
    local wifi_path="$1"

    if [ ! -f "${wifi_path}" ]; then
        echo "wifi=missing"
        return 1
    fi

    python3 - "${wifi_path}" <<'PYEOF'
import json
import pathlib
import sys

path = pathlib.Path(sys.argv[1])
try:
    data = json.loads(path.read_text())
except Exception:
    print("wifi=invalid")
    raise SystemExit(1)

if not isinstance(data, list):
    print("wifi=invalid")
    raise SystemExit(1)

count = sum(
    1
    for entry in data
    if isinstance(entry, dict) and isinstance(entry.get("ssid"), str) and entry.get("ssid")
)
print(f"wifi={count}")
PYEOF
}

ac_media_stage_boot_tree() {
    local stage_root="$1"
    local kernel_path="$2"
    local config_path="$3"
    local bootloader_path

    if [ ! -f "${kernel_path}" ]; then
        echo "Missing kernel: ${kernel_path}" >&2
        return 1
    fi
    if [ ! -f "${config_path}" ]; then
        echo "Missing config: ${config_path}" >&2
        return 1
    fi

    bootloader_path="$(ac_media_bootloader_path)" || return 1

    rm -rf "${stage_root}"
    mkdir -p "${stage_root}/EFI/BOOT"

    # Kernel as BOOTX64.EFI — standard UEFI fallback path, works on all firmware.
    # No splash chainloader needed; the kernel's EFI stub boots directly.
    cp "${kernel_path}" "${stage_root}/EFI/BOOT/BOOTX64.EFI"
    # 32-bit UEFI fallback: kernel's EFI stub with EFI_MIXED=y can boot
    # directly from 32-bit firmware (handles 32→64 mode switch internally).
    cp "${kernel_path}" "${stage_root}/EFI/BOOT/BOOTIA32.EFI"
    cp "${config_path}" "${stage_root}/config.json"

    # Stage slim kernel + initramfs for universal boot (systemd-boot mode)
    local kernel_dir
    kernel_dir="$(dirname "${kernel_path}")"
    local slim="${kernel_dir}/vmlinuz-slim"
    if [ -f "${slim}" ]; then
        cp "${slim}" "${stage_root}/EFI/BOOT/KERNEL-SLIM.EFI"
    fi
    # Prefer gzip initramfs (universal), fall back to lz4
    local initramfs_gz="${kernel_dir}/initramfs.cpio.gz"
    local initramfs_lz4="${kernel_dir}/initramfs.cpio.lz4"
    if [ -f "${initramfs_gz}" ]; then
        cp "${initramfs_gz}" "${stage_root}/initramfs.cpio.gz"
    elif [ -f "${initramfs_lz4}" ]; then
        cp "${initramfs_lz4}" "${stage_root}/initramfs.cpio.lz4"
    fi
}

ac_stage_boot_media_tree() {
    ac_media_stage_boot_tree "$@"
}

ac_media_create_fat_image() {
    local stage_root="$1"
    local image_path="$2"
    local label="$3"
    local size_mb="${4:-}"

    if [ -z "${size_mb}" ]; then
        size_mb=$(( $(ac_media_stage_tree_size_mib "${stage_root}") + 64 ))
    fi

    dd if=/dev/zero of="${image_path}" bs=1M count="${size_mb}" status=none
    mkfs.vfat -F 32 -n "${label}" "${image_path}" >/dev/null 2>&1

    export MTOOLS_SKIP_CHECK=1
    mmd -i "${image_path}" ::EFI ::EFI/BOOT 2>/dev/null || true
    if [ -f "${stage_root}/config.json" ]; then
        mcopy -o -i "${image_path}" "${stage_root}/config.json" ::config.json
    fi
    if [ -f "${stage_root}/wifi_creds.json" ]; then
        mcopy -o -i "${image_path}" "${stage_root}/wifi_creds.json" ::wifi_creds.json
    fi
    mcopy -o -i "${image_path}" "${stage_root}/EFI/BOOT/BOOTX64.EFI" ::EFI/BOOT/BOOTX64.EFI
    # 32-bit UEFI fallback (kernel with EFI_MIXED=y)
    if [ -f "${stage_root}/EFI/BOOT/BOOTIA32.EFI" ]; then
        mcopy -o -i "${image_path}" "${stage_root}/EFI/BOOT/BOOTIA32.EFI" ::EFI/BOOT/BOOTIA32.EFI
    fi
}

ac_media_create_efi_disk_image() {
    local stage_root="$1"
    local image_path="$2"
    local label="${3:-AC_ESP}"
    local size_mb="${4:-}"
    local esp_start=2048
    local esp_offset

    if [ -z "${size_mb}" ]; then
        size_mb=$(( $(ac_media_stage_tree_size_mib "${stage_root}") + 96 ))
    fi

    dd if=/dev/zero of="${image_path}" bs=1M count="${size_mb}" status=none
    printf 'label: gpt\nstart=%s, type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, name="%s"\n' \
        "${esp_start}" "${label}" |
        sfdisk --force --no-reread "${image_path}" >/dev/null
    mkfs.vfat -F 32 --offset="${esp_start}" -n "${label}" "${image_path}" >/dev/null

    esp_offset=$(( esp_start * 512 ))
    export MTOOLS_SKIP_CHECK=1
    mmd -i "${image_path}@@${esp_offset}" ::EFI ::EFI/BOOT 2>/dev/null || true
    if [ -f "${stage_root}/config.json" ]; then
        mcopy -o -i "${image_path}@@${esp_offset}" "${stage_root}/config.json" ::config.json
    fi
    if [ -f "${stage_root}/wifi_creds.json" ]; then
        mcopy -o -i "${image_path}@@${esp_offset}" "${stage_root}/wifi_creds.json" ::wifi_creds.json
    fi
    if [ -f "${stage_root}/$(ac_media_identity_filename)" ]; then
        mcopy -o -i "${image_path}@@${esp_offset}" "${stage_root}/$(ac_media_identity_filename)" "::$(ac_media_identity_filename)"
    fi
    mcopy -o -i "${image_path}@@${esp_offset}" "${stage_root}/EFI/BOOT/BOOTX64.EFI" ::EFI/BOOT/BOOTX64.EFI
    if [ -f "${stage_root}/EFI/BOOT/BOOTIA32.EFI" ]; then
        mcopy -o -i "${image_path}@@${esp_offset}" "${stage_root}/EFI/BOOT/BOOTIA32.EFI" ::EFI/BOOT/BOOTIA32.EFI
    fi
}

ac_create_fat_boot_image() {
    ac_media_create_fat_image "$@"
}

ac_media_partition_start_sector() {
    local image_path="$1"
    local partition_number="$2"
    local partition_name="${image_path}${partition_number}"

    sfdisk -d "${image_path}" 2>/dev/null |
        awk -v partition_name="${partition_name}" '
            BEGIN { found = 0 }
            $1 == partition_name {
                for (i = 1; i <= NF; i++) {
                    if ($i ~ /^start=/) {
                        value = $i
                        sub(/^start=/, "", value)
                        gsub(/,/, "", value)
                        if (value == "" && i < NF) {
                            value = $(i + 1)
                            gsub(/,/, "", value)
                        }
                        if (value ~ /^[0-9]+$/) {
                            print value
                            found = 1
                            exit
                        }
                    }
                }
            }
            END { exit found ? 0 : 1 }
        '
}

ac_media_partition_number_for_type_guid() {
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

ac_media_nixos_data_partition_start_sector() {
    local image_path="$1"
    local data_part

    data_part="$(ac_media_partition_number_for_type_guid "${image_path}" "EBD0A0A2-B9E5-4433-87C0-68B6B72699C7")" || return 1
    ac_media_partition_start_sector "${image_path}" "${data_part}"
}

ac_media_generate_manifest() {
    local image_path="$1"
    local build_name="$2"
    local out_path="$3"
    local identity_size
    local config_size
    local identity_marker

    identity_size="$(ac_media_identity_size)"
    config_size="$(ac_media_legacy_config_size)"
    identity_marker="$(ac_media_identity_marker)"

    python3 - "$image_path" "$build_name" "$out_path" "$identity_size" "$config_size" "$identity_marker" <<'PYEOF'
import json
import os
import sys
from datetime import datetime, timezone

image_path, build_name, out_path, identity_size, config_size, identity_marker = sys.argv[1:]
identity_size = int(identity_size)
config_size = int(config_size)
needle = b'{"handle":"","piece":"notepat","sub":"","email":""}'
identity_header = (identity_marker + "\n").encode()

with open(image_path, "rb") as fh:
    data = fh.read()

identity_offset = data.find(identity_header)
config_offsets = []
start = 0
while True:
    idx = data.find(needle, start)
    if idx < 0:
        break
    if idx < len(identity_header) or data[idx - len(identity_header):idx] != identity_header:
        config_offsets.append(idx)
    start = idx + len(needle)

manifest = {
    "name": build_name,
    "timestamp": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
    "artifactType": "img",
    "identityBlockOffset": identity_offset,
    "identityBlockSize": identity_size,
    "identityMarker": identity_marker,
    "configOffsets": config_offsets,
    "configPatchSize": config_size,
    "imageSize": os.path.getsize(image_path),
}

with open(out_path, "w", encoding="utf-8") as fh:
    json.dump(manifest, fh, indent=2)
PYEOF
}

ac_media_customize_nixos_efi_boot() {
    local image_path="$1"
    local efi_start
    local efi_offset
    local tmpdir
    local orig_cfg
    local menu_cfg
    local direct_cfg
    local linux_line
    local initrd_line

    if [ ! -f "${image_path}" ]; then
        echo "Missing NixOS image: ${image_path}" >&2
        return 1
    fi

    local efi_part
    if ! efi_part="$(ac_media_partition_number_for_type_guid "${image_path}" "C12A7328-F81F-11D2-BA4B-00A0C93EC93B")"; then
        echo "No EFI boot partition found in ${image_path}" >&2
        return 1
    fi

    if ! efi_start="$(ac_media_partition_start_sector "${image_path}" "${efi_part}")"; then
        echo "No EFI boot partition found in ${image_path}" >&2
        return 1
    fi

    efi_offset=$(( efi_start * 512 ))
    tmpdir="$(mktemp -d /tmp/ac-nixos-efi.XXXXXX)"
    orig_cfg="${tmpdir}/grub.cfg.orig"
    menu_cfg="${tmpdir}/grub-menu.cfg"
    direct_cfg="${tmpdir}/grub.cfg"

    export MTOOLS_SKIP_CHECK=1
    if ! mcopy -o -i "${image_path}@@${efi_offset}" ::EFI/BOOT/grub.cfg "${orig_cfg}" >/dev/null 2>&1; then
        rm -rf "${tmpdir}"
        echo "Failed to read EFI grub.cfg from ${image_path}" >&2
        return 1
    fi

    linux_line="$(grep -m1 '^[[:space:]]*linux ' "${orig_cfg}" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+\$\{isoboot\}([[:space:]]|$)/ /g; s/[[:space:]]+$//')"
    initrd_line="$(grep -m1 '^[[:space:]]*initrd ' "${orig_cfg}" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//')"

    if [ -z "${linux_line}" ] || [ -z "${initrd_line}" ]; then
        rm -rf "${tmpdir}"
        echo "Failed to extract kernel/initrd lines from EFI grub.cfg" >&2
        return 1
    fi

    cp "${orig_cfg}" "${menu_cfg}"
    cat > "${direct_cfg}" <<EOF
set timeout=0
set default=0
set efi_root=\$root
terminal_output console
terminal_input console
insmod part_msdos
insmod fat
insmod iso9660
insmod search_fs_file

search --no-floppy --set=root --file /EFI/nixos-installer-image

echo "Booting AC Native..."
${linux_line}
${initrd_line}
boot

echo "AC Native boot failed."
if [ -f (\$efi_root)/EFI/BOOT/grub-menu.cfg ]; then
  configfile (\$efi_root)/EFI/BOOT/grub-menu.cfg
fi
EOF

    if ! mcopy -o -i "${image_path}@@${efi_offset}" "${menu_cfg}" ::EFI/BOOT/grub-menu.cfg >/dev/null 2>&1 ||
       ! mcopy -o -i "${image_path}@@${efi_offset}" "${direct_cfg}" ::EFI/BOOT/grub.cfg >/dev/null 2>&1; then
        rm -rf "${tmpdir}"
        echo "Failed to update EFI grub.cfg in ${image_path}" >&2
        return 1
    fi

    rm -rf "${tmpdir}"
}

ac_media_ensure_nixos_data_partition() {
    local image_path="$1"
    local config_path="$2"
    local data_size_mib="${3:-$(ac_media_nixos_data_size_mib)}"
    local wifi_creds_path="${4:-}"
    local sector_size=512
    local data_start
    local data_offset
    local label

    if [ ! -f "${image_path}" ]; then
        echo "Missing NixOS image: ${image_path}" >&2
        return 1
    fi
    if [ ! -f "${config_path}" ]; then
        echo "Missing NixOS data config: ${config_path}" >&2
        return 1
    fi

    label="$(ac_media_nixos_data_label)"

    if ! data_start="$(ac_media_nixos_data_partition_start_sector "${image_path}")"; then
        local image_bytes
        local image_sectors
        local data_sectors

        image_bytes=$(stat -c%s "${image_path}")
        image_sectors=$(( (image_bytes + sector_size - 1) / sector_size ))
        data_start=$(( ((image_sectors + 2047) / 2048) * 2048 ))
        data_sectors=$(( data_size_mib * 1024 * 1024 / sector_size ))

        truncate -s $(((data_start + data_sectors) * sector_size)) "${image_path}"
        printf 'start=%s, size=%s, type=c\n' "${data_start}" "${data_sectors}" |
            sfdisk --no-reread -N 3 "${image_path}" >/dev/null
        mkfs.vfat -F 32 -n "${label}" --offset="${data_start}" "${image_path}" >/dev/null
    fi

    data_offset=$(( data_start * sector_size ))

    export MTOOLS_SKIP_CHECK=1
    mmd -i "${image_path}@@${data_offset}" ::logs 2>/dev/null || true
    mcopy -o -i "${image_path}@@${data_offset}" "${config_path}" ::config.json
    if [ -n "${wifi_creds_path}" ] && [ -f "${wifi_creds_path}" ]; then
        mcopy -o -i "${image_path}@@${data_offset}" "${wifi_creds_path}" ::wifi_creds.json
    fi
}

ac_media_build_hybrid_iso() {
    local stage_root="$1"
    local efi_img="$2"
    local iso_out="$3"
    local volume_id="${4:-AC_NATIVE}"

    if ! command -v xorriso >/dev/null 2>&1; then
        echo "xorriso not found" >&2
        return 1
    fi

    xorriso -as mkisofs \
        -o "${iso_out}" \
        -V "${volume_id}" \
        -r -J -joliet-long \
        -hfsplus \
        -hfs-bless-by intel_bootfile /EFI/BOOT/BOOTX64.EFI \
        -append_partition 2 0xef "${efi_img}" \
        -appended_part_as_apm \
        -eltorito-alt-boot \
        -e --interval:appended_partition_2:all:: \
        -no-emul-boot \
        "${stage_root}" 2>&1 | grep -v "^xorriso\|^Drive\|^Media" || true

    [ -f "${iso_out}" ]
}

ac_media_summarize_config_file() {
    local config_path="$1"

    if [ ! -f "${config_path}" ]; then
        echo "config=missing"
        return 1
    fi

    if command -v jq >/dev/null 2>&1; then
        jq -r '
            def yesno($v): if $v then "yes" else "no" end;
            "handle=@\(.handle // "unknown") ac_token=\(yesno((((.token // "") | tostring) | length) > 0)) claude_token=\(yesno((((.claudeToken // "") | tostring) | length) > 0)) github_pat=\(yesno((((.githubPat // "") | tostring) | length) > 0)) claude_creds=\(yesno(.claudeCreds? != null)) claude_state=\(yesno(.claudeState? != null))"
        ' "${config_path}" 2>/dev/null && return 0
    fi

    if command -v node >/dev/null 2>&1; then
        node -e "
            const fs = require('fs');
            const cfg = JSON.parse(fs.readFileSync(process.argv[1], 'utf8'));
            const parts = [
              'handle=@' + (cfg.handle || 'unknown'),
              'ac_token=' + ((cfg.token || '').length > 0 ? 'yes' : 'no'),
              'claude_token=' + ((cfg.claudeToken || '').length > 0 ? 'yes' : 'no'),
              'github_pat=' + ((cfg.githubPat || '').length > 0 ? 'yes' : 'no'),
              'claude_creds=' + (cfg.claudeCreds ? 'yes' : 'no'),
              'claude_state=' + (cfg.claudeState ? 'yes' : 'no'),
            ];
            console.log(parts.join(' '));
        " "${config_path}" 2>/dev/null && return 0
    fi

    echo "config=unreadable"
    return 1
}

ac_summarize_config_file() {
    ac_media_summarize_config_file "$@"
}
