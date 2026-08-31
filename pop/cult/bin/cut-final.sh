#!/usr/bin/env bash
# cut-final.sh — the release master (pop / DistroKid), proper loudness.
#
# Chain: fresh render → 2:21 discovery edit → the Peep-pass space (bass shelf +
# cathedral convolution, same as cut-v10) → MEASURE → one static dB to
# −11.5 LUFS integrated → true-peak limiter with a −1.0 dBTP ceiling.
# Never a second loudnorm. Deliverables:
#   out/cult-remix-final-master.wav   24-bit/48k reference master
#   out/cult-remix-final.flac         16-bit/44.1k — the DistroKid upload
#   out/cult-remix-final.mp3          320k preview
# Then the ~/Documents/Shelf/wannadash-DISTROKID/ folder is staged for
# pop/bin/distrokid-submit.mjs.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"
TARGET="${TARGET:--11.5}"

FULL="$OUT/cult-remix-v10-full.wav"
TRIM="$OUT/.final-trim.wav"
SPACE="$OUT/.final-space.wav"

echo "→ 2:21 discovery edit (cut bars 10–28, 60–63, 68–71, 84–91)"
ffmpeg -y -v error -i "$FULL" -filter_complex \
  "[0:a]atrim=start=15.95:end=20,asetpts=PTS-STARTPTS,afade=t=out:st=4.04:d=0.01[a];\
[0:a]atrim=start=58:end=120,asetpts=PTS-STARTPTS,afade=t=in:d=0.01,afade=t=out:st=61.99:d=0.01[b];\
[0:a]atrim=start=128:end=136,asetpts=PTS-STARTPTS,afade=t=in:d=0.01,afade=t=out:st=7.99:d=0.01[c];\
[0:a]atrim=start=144:end=168,asetpts=PTS-STARTPTS,afade=t=in:d=0.01,afade=t=out:st=23.99:d=0.01[d];\
[0:a]atrim=start=184,asetpts=PTS-STARTPTS,afade=t=in:d=0.01[e];\
[a][b][c][d][e]concat=n=5:v=0:a=1,afade=t=in:st=0:d=0.02[out]" \
  -map "[out]" -c:a pcm_s24le "$TRIM"

echo "→ cathedral + low shelf"
ffmpeg -y -v error -i "$TRIM" -i "$LANE/samples/cathedral-ir.wav" -filter_complex \
  "[0:a]bass=g=2.5:f=95:w=0.6,asplit[dry][s];[s][1:a]afir=dry=0:wet=3[wet];\
[dry][wet]amix=inputs=2:weights='1 0.35':normalize=0[out]" \
  -map "[out]" -ar 48000 -c:a pcm_s24le "$SPACE"

echo "→ measure"
STATS=$(ffmpeg -hide_banner -nostats -i "$SPACE" \
  -af loudnorm=I="$TARGET":TP=-1.0:LRA=9:print_format=json -f null - 2>&1 | awk '/^\{/,/^\}/')
MI=$(echo "$STATS" | grep '"input_i"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
GAIN=$(awk -v i="$MI" -v t="$TARGET" 'BEGIN{printf "%.2f", t-i}')
echo "  measured I=$MI  →  static ${GAIN} dB, then true-peak limit at -1.0 dBTP"

# alimiter at 0.84 ≈ −1.5 dBFS sample ceiling, leaving true-peak room —
# the ebur128 verify below must read ≤ −1.0 dBTP.
ffmpeg -y -v error -i "$SPACE" -af "\
volume=${GAIN}dB,\
alimiter=limit=0.84:attack=4:release=110:level=disabled" \
  -ar 48000 -c:a pcm_s24le "$OUT/cult-remix-final-master.wav"

ffmpeg -y -v error -i "$OUT/cult-remix-final-master.wav" \
  -ar 44100 -sample_fmt s16 -compression_level 8 \
  -metadata title="wannadash" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/cult-remix-final.flac"
ffmpeg -y -v error -i "$OUT/cult-remix-final-master.wav" -c:a libmp3lame -b:a 320k \
  -metadata title="wannadash" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/cult-remix-final.mp3"
rm -f "$TRIM" "$SPACE"

echo "→ verify"
ffmpeg -hide_banner -nostats -i "$OUT/cult-remix-final-master.wav" \
  -af ebur128=peak=true -f null - 2>&1 | grep -E "^\s+(I|LRA|Peak):"

DK="$HOME/Documents/Shelf/wannadash-DISTROKID"
mkdir -p "$DK"
cp "$OUT/cult-remix-final.flac" "$DK/wannadash-MASTER.flac"
cp "$LANE/cover/cover-b-signal.jpg" "$DK/wannadash-cover.jpg"
cp "$LANE/release.json" "$DK/release.json"
echo "✓ $OUT/cult-remix-final.flac  (DistroKid folder staged at $DK)"
