#!/usr/bin/env bash
# cut-flow.sh — "flow" candidate for wannadash. Non-destructive.
#
# The approved 2:21 competitive edit makes four splices, three of them in the
# last 35 seconds, and two of them land on the record's densest passages:
#
#   src 120-128 (bars 60-64) — act V's crest, 231 events, the densest 4 bars
#                              in the whole reply. The old cut deletes it, so
#                              act V is amputated and act VI's downbeat is
#                              pasted on. This is the 1:06 "disconnected".
#   src 136-144 (bars 68-72) — a genuine hole in the render: ZERO events for
#                              four bars, then every voice re-entering on the
#                              same sample at 144.0. Cutting it is correct;
#                              butt-cutting it is what "hits hard" at 1:15.
#   src 168-184 (bars 84-92) — act VII's middle, 431 events, the densest 16
#                              seconds in the record. The old cut deletes it,
#                              halving the one section where the harmony
#                              stops wandering (Bm-D-G-Em).
#
# This cut keeps both crests and removes only the hole, turning it into a
# one-bar caesura by letting the dash at src 135.0 ring out into silence
# before act VI's wide point lands. Two seams instead of four; one of them
# is the approved cold open. Everything from act V onward is unbroken.
#
# Outputs (all new filenames — nothing here overwrites the release master):
#   out/wannadash-flow-master.wav   24-bit / 48 kHz review master
#   out/wannadash-flow.flac         24-bit / 48 kHz candidate delivery
#   out/wannadash-flow.mp3          320 kbps listening copy
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
OUT="$LANE/out"
FULL="$OUT/cult-remix-v10-full.wav"
TRIM="$OUT/.flow-trim.wav"
SPACE="$OUT/.flow-space.wav"
MASTER="$OUT/wannadash-flow-master.wav"

cleanup() { rm -f "$TRIM" "$SPACE"; }
trap cleanup EXIT

echo "→ flow edit: 2 seams, act V and act VII intact"
# [a] 15.95-20.00  cold open, two-bar buildup (unchanged, approved)
# [b] 58.00-138.00 act III tail → IV THE SECRET → V COMPLETE (crest restored)
#                  → VI bars 64-68, then the tail rings out into a one-bar
#                  caesura (the 8-bar hole is shortened by 6s, so the grid
#                  survives the seam: bar 72 still lands on a bar line)
# [c] 144.00-216.00 VI bars 72-76 → VII COMPLETE (all 20 bars) → VIII → IX
ffmpeg -y -v error -i "$FULL" -filter_complex \
  "[0:a]atrim=start=15.95:end=20,asetpts=PTS-STARTPTS,afade=t=out:st=4.04:d=0.01[a];\
[0:a]atrim=start=58:end=138,asetpts=PTS-STARTPTS,afade=t=in:d=0.01,afade=t=out:st=79.4:d=0.6[b];\
[0:a]atrim=start=144,asetpts=PTS-STARTPTS,afade=t=out:st=82.9:d=0.3[c];\
[a][b][c]concat=n=3:v=0:a=1,afade=t=in:st=0:d=0.02[out]" \
  -map "[out]" -c:a pcm_s24le "$TRIM"

echo "→ cathedral + bright translation premaster"
ffmpeg -y -v error -i "$TRIM" -i "$LANE/samples/cathedral-ir.wav" -filter_complex \
  "[0:a]highpass=f=28,\
bass=g=-1.2:f=95:w=0.6,\
equalizer=f=220:t=q:w=0.9:g=0.8,\
equalizer=f=800:t=q:w=0.85:g=2.2,\
equalizer=f=2800:t=q:w=0.9:g=1.2,\
treble=g=1.6:f=6500:w=0.6,asplit[dry][s];[s][1:a]afir=dry=0:wet=3[wet];\
[dry][wet]amix=inputs=2:weights='1 0.35':normalize=0[out]" \
  -map "[out]" -ar 48000 -c:a pcm_s24le "$SPACE"

echo "→ density with the peaks left alive"
# The competitive chain squashed LRA 6.3 -> 4.0 LU: +7.6 dB into a hard
# limiter meant bars 76 and 92 had nowhere to go. Here the kick transients
# are tamed musically with a tanh soft-clip before the true-peak stage, so
# the limiter only catches strays and the arc survives: -11.5 LUFS, LRA 5.2.
ffmpeg -y -v error -i "$SPACE" -af \
  "acompressor=threshold=0.20:ratio=1.5:attack=30:release=180:knee=4:link=maximum:detection=rms:mix=0.50,\
volume=4.0dB,\
asoftclip=type=tanh:threshold=0.62:output=0.92,\
volume=3.0dB,\
aresample=192000,\
alimiter=limit=0.700:attack=4:release=120:asc=true:asc_level=0.35:level=false,\
aresample=48000" \
  -ar 48000 -c:a pcm_s24le "$MASTER"

ffmpeg -y -v error -i "$MASTER" -c:a flac -compression_level 8 \
  -metadata title="wannadash" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/wannadash-flow.flac"
ffmpeg -y -v error -i "$MASTER" -c:a libmp3lame -b:a 320k \
  -metadata title="wannadash" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/wannadash-flow.mp3"

echo "→ verify"
ffmpeg -hide_banner -nostats -i "$MASTER" -af ebur128=peak=true:framelog=quiet -f null - 2>&1 | \
  grep -E "^\s+(I|LRA|Peak):"
echo "✓ $MASTER"
