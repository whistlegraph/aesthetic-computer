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

ac_media_default_identity_json() {
    printf '%s' '{"handle":"","piece":"notepat","sub":"","email":""}'
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
    mcopy -o -i "${image_path}" "${stage_root}/EFI/BOOT/BOOTX64.EFI" ::EFI/BOOT/BOOTX64.EFI
    # 32-bit UEFI fallback (kernel with EFI_MIXED=y)
    if [ -f "${stage_root}/EFI/BOOT/BOOTIA32.EFI" ]; then
        mcopy -o -i "${image_path}" "${stage_root}/EFI/BOOT/BOOTIA32.EFI" ::EFI/BOOT/BOOTIA32.EFI
    fi
}

ac_create_fat_boot_image() {
    ac_media_create_fat_image "$@"
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
