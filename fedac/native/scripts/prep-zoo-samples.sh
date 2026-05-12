#!/bin/bash
# prep-zoo-samples.sh — convert the 12 curated animal recordings at
# ~/Desktop/notepat-samples/zoo/ into header-less float32 mono 48 kHz
# .raw files at fedac/native/samples/zoo/<name>.raw for the C audio
# engine to load at boot (audio.c: load_oneshot_bank).
#
# Source files are a mix of .ogg/.oga/.wav/.mp3 and are NOT checked
# into the repo. Output .raw files ARE checked into the repo so flash
# builds work without network access (mirrors prep-piano-samples.sh).
#
# Trims: snake → 3 s (15 s rattle source), whale → 7 s (53 s source),
# lion → 3 s (multiple roars), owl → 4 s. Others < 4 s already, so
# kept full length.
#
# Loudness leveling: single-pass loudnorm so all 12 sit at similar
# perceived volume. No analysis pass needed — single pass is fine for
# 12 short files.
#
# Idempotent: re-running just re-converts.
#
# Requires: ffmpeg (brew install ffmpeg).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NATIVE_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
SRC_DIR="${HOME}/Desktop/notepat-samples/zoo"
DST_DIR="${NATIVE_DIR}/samples/zoo"

if [ ! -d "${SRC_DIR}" ]; then
    echo "[prep-zoo] source directory not found: ${SRC_DIR}" >&2
    exit 1
fi

mkdir -p "${DST_DIR}"

# Name → (source-basename without ext, trim-seconds-or-empty)
# Match the ZOO_NAMES table in pieces/notepat.mjs exactly.
NAMES=(dog cat cow sheep bird pig lion owl frog horse snake whale)
# Per-animal duration cap in seconds; "" = keep full length.
trim_for() {
    case "$1" in
        lion)  echo 3  ;;
        owl)   echo 4  ;;
        snake) echo 3  ;;
        whale) echo 7  ;;
        *)     echo "" ;;
    esac
}

# Resolve source file for a name (accepts any of .ogg/.oga/.wav/.mp3/.flac).
resolve_src() {
    local n="$1"
    for ext in ogg oga wav mp3 flac m4a; do
        if [ -f "${SRC_DIR}/${n}.${ext}" ]; then
            echo "${SRC_DIR}/${n}.${ext}"
            return 0
        fi
    done
    return 1
}

total_bytes=0
total_samples=0

echo "[prep-zoo] converting ${#NAMES[@]} animals → ${DST_DIR}/<name>.raw"
for name in "${NAMES[@]}"; do
    src="$(resolve_src "${name}" || true)"
    if [ -z "${src}" ]; then
        echo "[prep-zoo] WARNING: no source for ${name}, skipping" >&2
        continue
    fi
    out="${DST_DIR}/${name}.raw"
    trim="$(trim_for "${name}")"

    # ffmpeg pipeline:
    #   -ss 0 -t N  (optional duration trim — pre-decode)
    #   -ac 1 -ar 48000      (mono, 48 kHz)
    #   -af loudnorm=I=-16:LRA=11:TP=-1.5   (loudness leveling)
    #   -f f32le             (raw float32 little-endian)
    if [ -n "${trim}" ]; then
        ffmpeg -y -loglevel error -ss 0 -t "${trim}" -i "${src}" \
               -ac 1 -ar 48000 \
               -af "loudnorm=I=-16:LRA=11:TP=-1.5" \
               -f f32le "${out}"
    else
        ffmpeg -y -loglevel error -i "${src}" \
               -ac 1 -ar 48000 \
               -af "loudnorm=I=-16:LRA=11:TP=-1.5" \
               -f f32le "${out}"
    fi

    # Verify output: must be divisible by 4 (float32) and non-empty.
    sz=$(wc -c < "${out}" | tr -d ' ')
    if [ "${sz}" -le 0 ] || [ $((sz % 4)) -ne 0 ]; then
        echo "[prep-zoo] ERROR: bad output ${out} (size=${sz})" >&2
        exit 1
    fi
    samples=$((sz / 4))
    secs=$(awk -v s="${samples}" 'BEGIN { printf "%.2f", s / 48000.0 }')
    total_bytes=$((total_bytes + sz))
    total_samples=$((total_samples + samples))
    printf "  %-6s %s → %s (%s samples, %ss)\n" \
        "${name}" "$(basename "${src}")" "$(basename "${out}")" "${samples}" "${secs}"
done

count=$(ls "${DST_DIR}"/*.raw 2>/dev/null | wc -l | tr -d ' ')
total_secs=$(awk -v s="${total_samples}" 'BEGIN { printf "%.2f", s / 48000.0 }')
size_h=$(du -sh "${DST_DIR}" | cut -f1)
echo "[prep-zoo] done: ${count} files, ${total_bytes} bytes (${size_h}), ${total_secs}s total"
