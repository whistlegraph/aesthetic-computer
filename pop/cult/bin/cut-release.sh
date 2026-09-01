#!/usr/bin/env bash
# cut-release.sh — DistroKid delivery master for "wannadash".
#
# Edit geometry is bot's current competitive edit, unchanged (2:21), including
# its seam fix: [d] starts at 143.76 so the 0.24 s fade-in COMPLETES at 144.00
# and the bar-72 downbeat lands at full gain. Only two things differ:
#
#   1. Crossfade curves tri -> qsin. `tri` is linear; two linear ramps sum to
#      -3 dB on uncorrelated material. `qsin` is the constant-power pair.
#      Worth ~1 dB at each of the three seams.
#   2. The density stage. The competitive chain was +7.6 dB into a hard
#      limiter at a 0.700 ceiling: measured as a 2.45:1 macro-compressor at a
#      3-second time constant, collapsing the raw edit's LRA from 8.1 to 4.2
#      while leaving 3 dB of true-peak headroom unused. Here the kick
#      transients are tamed musically with a tanh soft-clip, the compression
#      is 1.33:1, and the loudness is taken at the ceiling instead:
#
#        competitive   -11.2 LUFS   LRA 4.2   -2.9 dBTP
#        release       -10.6 LUFS   LRA 6.3   -1.9 dBTP
#
#      Louder, 50% more loudness range, still codec-safe.
#
# Also: high-pass moved 28 -> 32 Hz. Eleven bass events sit at 24.5-30.9 Hz
# (including a one-off octave-wrap to G0 / MIDI 19 at bar 34) in an octave
# that is 5.8% of the energy and inaudible on anything but a club rig.
#
# Outputs (all new filenames — nothing here overwrites bot's masters):
#   out/wannadash-release-master.wav   24-bit / 48 kHz
#   out/wannadash-release.flac         24-bit / 48 kHz — the DistroKid upload
#   out/wannadash-release.mp3          320 kbps listening copy
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"
FULL="$OUT/cult-remix-v10-full.wav"
TRIM="$OUT/.release-trim.wav"
SPACE="$OUT/.release-space.wav"
MASTER="$OUT/wannadash-release-master.wav"

cleanup() { rm -f "$TRIM" "$SPACE"; }
trap cleanup EXIT

echo "→ edit (bot's competitive geometry, constant-power seams)"
ffmpeg -y -v error -i "$FULL" -filter_complex \
  "[0:a]atrim=start=15.95:end=20,asetpts=PTS-STARTPTS,afade=t=out:st=4.04:d=0.01[a];\
[0:a]atrim=start=58:end=120,asetpts=PTS-STARTPTS,afade=t=in:d=0.01[b];\
[0:a]atrim=start=127.76:end=136,asetpts=PTS-STARTPTS[c];\
[0:a]atrim=start=143.76:end=167.95,asetpts=PTS-STARTPTS[d];\
[0:a]atrim=start=183.71,asetpts=PTS-STARTPTS[e];\
[b][c]acrossfade=d=0.24:c1=qsin:c2=qsin[bc];\
[bc][d]acrossfade=d=0.24:c1=qsin:c2=qsin[bcd];\
[bcd][e]acrossfade=d=0.24:c1=qsin:c2=qsin[bcde];\
[a][bcde]concat=n=2:v=0:a=1,afade=t=in:st=0:d=0.02[out]" \
  -map "[out]" -c:a pcm_s24le "$TRIM"

echo "→ cathedral + bright translation premaster"
ffmpeg -y -v error -i "$TRIM" -i "$LANE/samples/cathedral-ir.wav" -filter_complex \
  "[0:a]highpass=f=32,\
bass=g=-1.2:f=95:w=0.6,\
equalizer=f=220:t=q:w=0.9:g=0.8,\
equalizer=f=800:t=q:w=0.85:g=2.2,\
equalizer=f=2800:t=q:w=0.9:g=1.2,\
treble=g=1.6:f=6500:w=0.6,asplit[dry][s];[s][1:a]afir=dry=0:wet=3[wet];\
[dry][wet]amix=inputs=2:weights='1 0.35':normalize=0[out]" \
  -map "[out]" -ar 48000 -c:a pcm_s24le "$SPACE"

echo "→ density (1.33:1) + loudness taken at the ceiling"
ffmpeg -y -v error -i "$SPACE" -af \
  "acompressor=threshold=0.20:ratio=1.5:attack=30:release=180:knee=4:link=maximum:detection=rms:mix=0.50,\
volume=4.0dB,\
asoftclip=type=tanh:threshold=0.62:output=0.92,\
volume=4.2dB,\
aresample=192000,\
alimiter=limit=0.800:attack=4:release=120:asc=true:asc_level=0.35:level=false,\
aresample=48000" \
  -ar 48000 -c:a pcm_s24le "$MASTER"

echo "→ deliverables"
ffmpeg -y -v error -i "$MASTER" -c:a flac -compression_level 8 -sample_fmt s32 \
  -metadata title="wannadash" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/wannadash-release.flac"
ffmpeg -y -v error -i "$MASTER" -c:a libmp3lame -b:a 320k \
  -metadata title="wannadash" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/wannadash-release.mp3"

echo "→ verify"
ffmpeg -hide_banner -nostats -i "$MASTER" \
  -af ebur128=peak=true:framelog=quiet -f null - 2>&1 | grep -E "^\s+(I|LRA|Peak):"
echo "✓ $OUT/wannadash-release.flac"
