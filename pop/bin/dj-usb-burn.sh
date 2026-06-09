#!/bin/bash
# dj-usb-burn.sh — burn a folder of audio files to a USB stick for DJ use.
#
# Works in the AC Native `dj` piece (mountMusic mounts the first vfat/exfat
# partition of any non-boot USB read-only at /media and scans 4 levels deep —
# see fedac/native/src/js-bindings.c probe_mount_music_once) and in
# traditional DJ gear (Pioneer CDJ etc.), which both want MBR + FAT32.
#
# Usage: bash pop/bin/dj-usb-burn.sh diskN [src]   (src default: ~/Desktop/dj)
#        bash pop/bin/dj-usb-burn.sh               (lists external disks)
set -e

DISK="${1:-}"
SRC="${2:-$HOME/Desktop/dj}"
LABEL="ACDJ"   # FAT32 label: ≤11 chars, uppercase

if [ -z "${DISK}" ]; then
    echo "External physical disks:"
    diskutil list external physical
    echo ""
    echo "Usage: $0 diskN [src-folder]"
    exit 1
fi

[ -d "${SRC}" ] || { echo "Source folder not found: ${SRC}"; exit 1; }
TRACKS=$(find "${SRC}" -maxdepth 1 -type f \
    \( -iname '*.mp3' -o -iname '*.wav' -o -iname '*.flac' -o -iname '*.ogg' \
       -o -iname '*.aac' -o -iname '*.m4a' -o -iname '*.opus' \) | sort)
[ -n "${TRACKS}" ] && N=$(echo "${TRACKS}" | wc -l | tr -d ' ') || { echo "No audio files in ${SRC}"; exit 1; }

# Refuse to touch an internal disk
diskutil info "${DISK}" | grep -q "Removable Media: *Removable" \
    || { echo "${DISK} is not removable media — refusing."; exit 1; }

echo "Target:"
diskutil info "${DISK}" | grep -E "Device Node|Media Name|Disk Size" | sed 's/^ */  /'
echo "  ${N} tracks from ${SRC}"
echo ""
read -p "ERASE ${DISK} as FAT32 '${LABEL}' and burn? (yes/no) " OK
[ "${OK}" = "yes" ] || { echo "Aborted."; exit 1; }

# MBR + FAT32 — what ac-usb flash uses, and the broadest DJ-gear support.
# First attempt can race the auto-mounter ("-69877 Couldn't open device");
# unmount and retry once.
diskutil unmountDisk "${DISK}" >/dev/null 2>&1 || true
diskutil eraseDisk MS-DOS "${LABEL}" MBR "${DISK}" || {
    echo "Erase raced the auto-mounter — retrying..."
    sleep 2
    diskutil eraseDisk MS-DOS "${LABEL}" MBR "${DISK}"
}

VOL="/Volumes/${LABEL}"
[ -d "${VOL}" ] || { echo "Mount not found at ${VOL}"; exit 1; }

# Keep macOS from littering the stick (dj.mjs skips dotfiles, CDJs don't)
mdutil -i off "${VOL}" >/dev/null 2>&1 || true
touch "${VOL}/.metadata_never_index"
mkdir -p "${VOL}/.fseventsd" && touch "${VOL}/.fseventsd/no_log"

echo "${TRACKS}" | while IFS= read -r f; do
    echo "  + $(basename "$f")"
    cp "$f" "${VOL}/"
done

# Strip AppleDouble junk and finish
dot_clean -m "${VOL}" 2>/dev/null || true
find "${VOL}" -name '._*' -delete 2>/dev/null || true
sync
diskutil eject "${DISK}"
echo ""
echo "Done — '${LABEL}' burned with ${N} tracks and ejected. Safe to pull."
