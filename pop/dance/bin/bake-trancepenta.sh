#!/usr/bin/env bash
# bake-trancepenta.sh — the 4-stage trancepenta master bake. build.mjs's
# trancepenta CONFIG invokes this (instead of bake.mjs, which is hardcoded
# to trancenwaltzi's 3-stage path). Stages:
#   1 — engine (recap/bin/trance.mjs --mode chill --meter 5 --bpm 126
#       --scale dorian --hell 13 --chill-hats off --gallop --beat-in 30)
#       → trancepenta-MASTER-preBright.wav (+ struct.json)
#   2 — scratch-mix (post-FX, beat-locked to engine struct)
#   3 — place-penta-vocal (3 sung entries layered on the scratch master)
#   4 — finalize-penta-vocal (stage-3 master → -14 LUFS / -1.5 dBTP /
#       18 s fade) → trancepenta-MASTER.wav + .mp3
#
# Cached: if the final master + paired struct.json already exist and
# BAKE_FORCE is unset, exits 0 silently (build.mjs reads them via
# audioSrc/structSrc anyway). Re-bake explicitly:
#   BAKE_FORCE=1 bash bake-trancepenta.sh
set -euo pipefail
REPO="$(cd "$(dirname "$0")/../../.." && pwd)"
OUT="${HOME}/Documents/Shelf/twi-out"
mkdir -p "$OUT"
PRE="$OUT/trancepenta-MASTER-preBright.wav"
SCR="$OUT/.tp-scr.wav"            # stage-2 scratch master (instrumental + post-FX)
VOX="$OUT/.tp-vox-bus.wav"        # stage-3 vocal bus (3 placed entries, silence between)
FINAL="$OUT/trancepenta-MASTER.wav"
MP3="$OUT/trancepenta.mp3"
STRUCT="$PRE.assets/struct.json"
VOCAL="$REPO/pop/dance/out/trancepenta-hum-layered.mp3"  # wordless hum, dances with the Odyssey theremin
STAMP="$REPO/pop/dance/out/.ac-dot-stamp-vocal.mp3"

# cached short-circuit
if [ -z "${BAKE_FORCE:-}" ] && [ -f "$FINAL" ] && [ -f "$STRUCT" ]; then
  echo "[bake-trancepenta] cached → $FINAL (BAKE_FORCE=1 to re-bake)"
  exit 0
fi

run() { echo; echo "[bake-trancepenta] $1"; shift; "$@"; }

# 1 — engine (beat-in pulled 30 → 12 so the bass beat enters within the
#     first 15 s and the first drop lands much sooner)
run "engine (single all-sine multi-bus render, --hell 13 --gallop --beat-in 12)" \
  node "$REPO/recap/bin/trance.mjs" --mode chill --meter 5 --master \
    --bpm 126 --scale dorian --hell 13 --chill-hats on --gallop \
    --beat-in 12 --out "$PRE"

# 2 — scratch-mix (post-FX, beat-locked) → SCR is the instrumental master
run "scratch-mix (post-FX beat-locked to struct)" \
  node "$REPO/pop/dance/bin/scratch-mix.mjs" \
    "$PRE" "$SCR" "$STAMP" "$STRUCT"

# 3 — jeffrey HARMONY BUS (wandering chordal hum that dances with the
#     Odyssey theremin — sometimes lower, sometimes higher, never fully
#     aligned). Set BAKE_NO_VOCAL=1 to revert to a silent vox bus.
SCRDUR=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$SCR")
if [ "${BAKE_NO_VOCAL:-}" = "1" ] || [ ! -f "$VOCAL" ]; then
  echo "[bake-trancepenta] silent vox bus (BAKE_NO_VOCAL=1 or hum missing · dur=${SCRDUR}s)"
  ffmpeg -y -f lavfi -i "anullsrc=channel_layout=stereo:sample_rate=44100" \
    -t "$SCRDUR" -c:a pcm_s16le "$VOX" 2>/dev/null
else
  run "place-penta-harmony (wandering jeffrey hum stack · dur=${SCRDUR}s)" \
    node "$REPO/pop/dance/bin/place-penta-harmony.mjs" \
      --phrase "$VOCAL" --dur "$SCRDUR" --out "$VOX"
fi

# 4 — finalize: SCR (instrumental) + VOX (vocal bus) → -14 LUFS / -1.5 dBTP / 18 s fade
run "finalize-penta-vocal (--vox-db -3 · scratch master + vocal bus)" \
  node "$REPO/pop/dance/bin/finalize-penta-vocal.mjs" \
    --scr "$SCR" --vox "$VOX" --vox-db -3 \
    --out "$FINAL"

# mp3 for build.mjs + DistroKid asset pack — 320 k with the cover art
# embedded as ID3v2 attached_pic + full ID3 tags (title/artist/album/
# year/genre). Most modern players + Spotify/Apple/DistroKid all read
# ID3v2.3.
COVER="$REPO/system/public/assets/pop/trancepenta.jpg"
if [ -f "$COVER" ]; then
  ffmpeg -y -loglevel error -i "$FINAL" -i "$COVER" \
    -map 0:a -map 1:v -c:a libmp3lame -b:a 320k -c:v copy \
    -id3v2_version 3 \
    -metadata title="trancepenta" \
    -metadata artist="Aesthetic Dot Computer" \
    -metadata album="pixsies" \
    -metadata date="2026" \
    -metadata genre="Dance" \
    -metadata:s:v title="Album cover" \
    -metadata:s:v comment="Cover (front)" \
    "$MP3" 2>/dev/null
else
  echo "[bake-trancepenta] ⚠ cover missing at $COVER — mp3 will lack embedded art"
  ffmpeg -y -i "$FINAL" -codec:a libmp3lame -b:a 320k "$MP3" 2>/dev/null
fi

rm -f "$SCR" "$VOX"
echo "[bake-trancepenta] done → $FINAL"
