#!/usr/bin/env bash
# bake-solafiya.sh — final mastering bake for the DistroKid drop.
#
# Source: pop/jungle/out/solafiya.mp3 — already the full
# instrumental+vocal mix from render.mjs (fía's voice is forced-aligned
# into vocal/vocalAd/vocalDuet/vocalH/throat lanes via the WORLD pipeline,
# baked into the master mix). render.mjs intentionally leaves ~3 dBTP of
# headroom and high-cuts at ~8.6 k so we restore the air + push to
# streaming targets here.
#
# Chain (matches the pixsies house style learned on trancenwaltz /
# helpabeach — see pop/RELEASES.md "Mastering note"):
#   highpass 28 → 250 Hz mud trim → 4.2 k presence +2 →
#   high-shelf 8.5 k +5 (air restore) → high-shelf 12.5 k +2.5 (sparkle) →
#   loudnorm I=-14 TP=-1.5 LRA=8 (jungle wants tight) → alimiter 0.95
#
# Targets: -14 LUFS / -1.5 dBTP / LRA ~6-8 LU. Source -19.4 LUFS so ~5 dB
# of program gain.
#
# Outputs (~/Documents/Working Desktop/solafiya-DISTROKID/ — durable home,
# Desktop auto-cleans — see [[feedback_desktop_autocleaned]]):
#   solafiya-MASTER.wav             — 16-bit / 44.1 kHz stereo
#   solafiya-MASTER-preBright.wav   — A/B against the dark cut
#   solafiya.mp3                    — 320 k + 3000² cover + ID3
#   solafiya-cover-3000.jpg         — DistroKid cover (illy.png upscaled)
#
# Cached: if final WAV exists and BAKE_FORCE is unset, exits 0. Re-bake:
#   BAKE_FORCE=1 bash pop/jungle/bin/bake-solafiya.sh
set -euo pipefail

REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
SRC_MP3="$REPO/pop/jungle/out/solafiya.mp3"
SRC_ILLY="$REPO/pop/jungle/out/solafiya.illy.png"

OUT="${HOME}/Documents/Working Desktop/solafiya-DISTROKID"
mkdir -p "$OUT"

PRE="$OUT/solafiya-MASTER-preBright.wav"
FINAL="$OUT/solafiya-MASTER.wav"
COVER="$OUT/solafiya-cover-3000.jpg"
MP3="$OUT/solafiya.mp3"

if [ -z "${BAKE_FORCE:-}" ] && [ -f "$FINAL" ] && [ -f "$COVER" ] && [ -f "$MP3" ]; then
  echo "[bake-solafiya] cached → $FINAL (BAKE_FORCE=1 to re-bake)"
  exit 0
fi

if [ ! -f "$SRC_MP3" ]; then
  echo "✗ source missing: $SRC_MP3"
  echo "  run: node pop/jungle/bin/render.mjs --slug solafiya"
  exit 1
fi
if [ ! -f "$SRC_ILLY" ]; then
  echo "✗ illustration missing: $SRC_ILLY"
  exit 1
fi

run() { echo; echo "[bake-solafiya] $1"; shift; "$@"; }

# 1 — pre-bright reference (decode + downmix-safe stereo 44.1k 16-bit; no EQ)
run "pre-bright reference (dark render, for A/B)" \
  ffmpeg -y -hide_banner -loglevel error -i "$SRC_MP3" \
    -ac 2 -ar 44100 -c:a pcm_s16le "$PRE"

# 2 — full master chain → -14 LUFS / -1.5 dBTP
run "master chain (air restore + loudnorm + alimiter)" \
  ffmpeg -y -hide_banner -loglevel error -i "$SRC_MP3" \
    -af "highpass=f=28, \
equalizer=f=250:t=q:w=1.2:g=-1, \
equalizer=f=4200:t=q:w=1.4:g=2, \
highshelf=f=8500:g=5, \
highshelf=f=12500:g=2.5, \
loudnorm=I=-14:TP=-1.5:LRA=8, \
alimiter=limit=0.95" \
    -ac 2 -ar 44100 -c:a pcm_s16le "$FINAL"

# 3 — cover: upscale 1024² illy → 3000² JPG (lanczos, sRGB, q=92)
run "cover (lanczos 1024 → 3000²)" \
  magick "$SRC_ILLY" -filter Lanczos -resize 3000x3000 \
    -colorspace sRGB -quality 92 "$COVER"

# 4 — 320 k mp3 with embedded cover + ID3 (matches other pixsies singles)
run "mp3 (320 k + 3000² cover + ID3)" \
  ffmpeg -y -hide_banner -loglevel error -i "$FINAL" -i "$COVER" \
    -map 0:a -map 1:v -c:a libmp3lame -b:a 320k -c:v copy \
    -id3v2_version 3 \
    -metadata title="solafiya" \
    -metadata artist="fía" \
    -metadata album="pixsies" \
    -metadata date="2026" \
    -metadata genre="Jungle" \
    -metadata publisher="Aesthetic.Computer" \
    -metadata copyright="Aesthetic.Computer" \
    -metadata:s:v title="Album cover" \
    -metadata:s:v comment="Cover (front)" \
    "$MP3"

# 5 — measure final master (single-pass ebur128 summary)
echo
echo "[bake-solafiya] ── loudness ──────────────────────────────────────"
ffmpeg -hide_banner -nostats -i "$FINAL" -af "ebur128=peak=true" -f null - 2>&1 \
  | awk '/Integrated loudness:|I:|LRA:|Peak:|^  /' | tail -20

DUR=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$FINAL")
echo
echo "[bake-solafiya] done"
echo "  master : $FINAL  (${DUR}s)"
echo "  pre    : $PRE"
echo "  cover  : $COVER"
echo "  mp3    : $MP3"
