#!/bin/bash
# prep-piano-samples.sh — fetch Salamander Grand Piano V3, decimate to
# the anchor pitches the AC OS audio engine wants, and write raw f32
# mono 48kHz files into fedac/native/samples/piano/.
#
# Source: archive.org mirror of Salamander Grand Piano V3 (CC0 by
# Alexander Holm). The sfzinstruments fork on GitHub is the canonical
# maintained tree; the archive.org tar is the same content, faster to
# pull from a script than cloning git LFS.
#
# Anchor density matches Salamander's own (every 3 semitones — C, D#,
# F#, A across 8 octaves = 26 anchors). One velocity layer for now
# (v8, mezzo). Future OTAs can extend to v15 (forte) for crossfaded
# velocity layering — see the load_piano_bank() in audio.c, just
# extend the filename parser to accept "<midi>_v<vel>.raw".
#
# Idempotent: re-running just re-converts. Output files are checked
# into git so flash builds work without network access.
#
# Requires: curl, tar, ffmpeg (brew install ffmpeg).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NATIVE_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
SAMPLES_DST="${NATIVE_DIR}/samples/piano"
TMP_DIR="${TMPDIR:-/tmp}/piano-prep"
ARCHIVE="${TMP_DIR}/salamander.tar.bz2"
EXTRACT_DIR="${TMP_DIR}/extracted"
URL="https://archive.org/download/SalamanderGrandPianoV3/SalamanderGrandPianoV3_44.1khz16bit.tar.bz2"

mkdir -p "${TMP_DIR}" "${EXTRACT_DIR}" "${SAMPLES_DST}"

if [ ! -f "${ARCHIVE}" ]; then
    echo "[prep-piano] downloading Salamander Grand Piano V3 (~163 MB)..."
    curl -L --fail -o "${ARCHIVE}" "${URL}"
fi

NOTES=(A0 C1 "D#1" "F#1" A1 C2 "D#2" "F#2" A2 C3 "D#3" "F#3" A3 \
       C4 "D#4" "F#4" A4 C5 "D#5" "F#5" A5 C6 "D#6" "F#6" A6 C7)
declare -A MIDI=( [A0]=21 [C1]=24 ["D#1"]=27 ["F#1"]=30 [A1]=33 \
                  [C2]=36 ["D#2"]=39 ["F#2"]=42 [A2]=45 \
                  [C3]=48 ["D#3"]=51 ["F#3"]=54 [A3]=57 \
                  [C4]=60 ["D#4"]=63 ["F#4"]=66 [A4]=69 \
                  [C5]=72 ["D#5"]=75 ["F#5"]=78 [A5]=81 \
                  [C6]=84 ["D#6"]=87 ["F#6"]=90 [A6]=93 [C7]=96 )

# Extract any anchor WAVs we haven't already pulled out of the archive.
SRC_SUBDIR="SalamanderGrandPianoV3_44.1khz16bit/44.1khz16bit"
for note in "${NOTES[@]}"; do
    target="${EXTRACT_DIR}/${SRC_SUBDIR}/${note}v8.wav"
    if [ ! -f "${target}" ]; then
        tar -xjf "${ARCHIVE}" -C "${EXTRACT_DIR}" "${SRC_SUBDIR}/${note}v8.wav"
    fi
done

# Convert each anchor to raw f32 mono 48kHz, truncated to 3 seconds.
# 3 s captures the audible decay of the loudest notes; longer notes
# would just bloat the initramfs without listenable benefit (the C4
# fundamental at v8 is below -40 dB after ~2.5 s of decay).
echo "[prep-piano] converting ${#NOTES[@]} anchors → ${SAMPLES_DST}/<midi>.raw"
for note in "${NOTES[@]}"; do
    midi=${MIDI[$note]}
    in_file="${EXTRACT_DIR}/${SRC_SUBDIR}/${note}v8.wav"
    out_file="${SAMPLES_DST}/${midi}.raw"
    ffmpeg -y -loglevel error -i "${in_file}" -ac 1 -ar 48000 -t 3 -f f32le "${out_file}"
done

count=$(ls "${SAMPLES_DST}"/*.raw 2>/dev/null | wc -l | tr -d ' ')
size=$(du -sh "${SAMPLES_DST}" | cut -f1)
echo "[prep-piano] done: ${count} anchors, ${size} total"
