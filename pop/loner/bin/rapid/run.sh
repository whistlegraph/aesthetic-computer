#!/usr/bin/env bash
# run.sh — lonerclubRAPID: a whole new arrangement of lonerclub (v4pid),
# sprinting. The record about waiting very patiently loses its patience.
#
# Built from the v4pid work dir's ingredients (the sacred audio's separated
# stems + the composed layers), the clubber360 arrangement idiom, and the
# wannadash mastering arsenal:
#
#   SQUEEZE  rubberband R3, pitch held, formants held on her: every bed and
#            her whole vocal chain compressed by exactly 122/156 so one
#            v4pid bar = one rapid bar on the new grid
#   FLOOR    gen-floor-rapid — the v4pid instrument voices laid fresh at 156:
#            closed 16ths, click-rush doors, turns, one kickless break
#   BASS     gen-bass-rapid — sub pedal at mouth/tail, wub in four passes
#   STAGE    assemble-rapid — 88 bars / 2:15. Sprint I (her verse squeezed),
#            sprint II, an 8-bar kickless break where she sings "sitting
#            curled up in myself" at REAL speed while only the shaker ticks,
#            then the finale material drops at speed, the hook reprises,
#            climbs, and the natural ending peels to the stamp. The last
#            click cuts the record (wannadash ending law).
#   MASTER   the wannadash release chain at the house profile
#            (pop/MASTERING.md: −10 ±1 LUFS, ≤−2 dBTP): bright translation
#            EQ + a parallel bass-harmonics branch (tanh'd sub, 250–2000 Hz
#            product — the fix for v4pid's −13.6 dB phone loss), then 1.5:1
#            rms density at half mix, tanh soft-clip, one measured static
#            gain into an oversampled true-peak limiter.
#
# GAIN2/LIMIT/HARM are measured, not guessed — re-measure after any change
# upstream (run once, read I/phone off the audit, adjust, bake).
#
# Needs the v4pid work dir (~/.cache/ac/v4pid) already built by v4pid/run.sh.
# Usage:  bash pop/loner/bin/rapid/run.sh
#   SQUEEZE=0  reuse squeezed stems     RAPID_BPM  grid tempo (156)
#   GAIN2      gain into the limiter    LIMIT      ceiling (0.760)
#   HARM       bass-harmonics dB
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../../../.." && pwd)"
cd "$REPO"

export V4PID_WORK="${V4PID_WORK:-$HOME/.cache/ac/v4pid}"
export RAPID_WORK="${RAPID_WORK:-$HOME/.cache/ac/rapid}"
export RAPID_BPM="${RAPID_BPM:-156}"
V="$V4PID_WORK"; W="$RAPID_WORK"
mkdir -p "$W"
OUT="pop/loner/out"
MASTER="$OUT/lonerclub-rapid-master.wav"
PY="${PY:-$REPO/pop/.venv/bin/python}"

[ -f "$V/vocalsFX.wav" ] || { echo "v4pid work dir is empty — run v4pid/run.sh first"; exit 1; }
RT=$(awk -v b="$RAPID_BPM" 'BEGIN{printf "%.6f", 122.0/b}')
# pads/pluck/bells raws are 44.1k data read as 48k (the 8.8%-fast quirk IS
# the shipped v4pid sound). Their squeeze compensates the read so a musical
# bar still lands on one rapid bar — the bright pitch character survives.
RTQ=$(awk -v b="$RAPID_BPM" 'BEGIN{printf "%.6f", 122.0/b*48000.0/44100.0}')

squeeze() { # squeeze <ratio> <in.wav> <out.wav> [extra rubberband flags]
  local rt="$1" in="$2" out="$3"; shift 3
  [ "${SQUEEZE:-1}" = "0" ] && [ -f "$out" ] && return 0
  [ -f "$out" ] && [ "${SQUEEZE:-1}" != "force" ] && return 0
  echo "  x$rt $(basename "$out")"
  rubberband -3 --time "$rt" "$@" "$in" "$out" >/dev/null 2>&1
}

echo "→ squeeze (rubberband R3, one v4pid bar = one rapid bar)"
for b in pads pluck bells; do
  [ -f "$W/st-$b.wav" ] || ffmpeg -y -v error -f f32le -ar 48000 -ac 2 \
    -i "$V/st-$b.raw" -c:a pcm_s24le "$W/st-$b.wav"
  squeeze "$RTQ" "$W/st-$b.wav" "$W/sq-$b.wav"
done
[ -f "$W/st-piano.wav" ] || ffmpeg -y -v error -f f32le -ar 48000 -ac 2 \
  -i "$V/st-piano.raw" -c:a pcm_s24le "$W/st-piano.wav"
squeeze "$RT" "$W/st-piano.wav" "$W/sq-piano.wav"
squeeze "$RT" "$V/sep4/htdemucs/v4pid-trim/bass.wav" "$W/sq-bass.wav"
squeeze "$RT" "$V/vocalsFX.wav" "$W/sq-vocalsFX.wav" --formant

echo "→ floor"
$PY "$HERE/gen-floor-rapid.py"
echo "→ bass"
$PY "$HERE/gen-bass-rapid.py"
echo "→ assemble"
$PY "$HERE/assemble-rapid.py"
ffmpeg -y -v error -f f32le -ar 48000 -ac 2 -i "$W/premaster-rapid.raw" \
  -c:a pcm_s24le "$W/premaster-rapid.wav"

echo "→ bright translation premaster + bass harmonics"
HARM="${HARM:-11.5}"
ffmpeg -y -v error -i "$W/premaster-rapid.wav" -filter_complex \
  "[0:a]asplit[m][lo];\
[lo]lowpass=f=150,volume=14dB,asoftclip=type=tanh:threshold=0.30,\
highpass=f=250,lowpass=f=2000,volume=${HARM}dB[h];\
[m][h]amix=inputs=2:weights='1 1':normalize=0,\
highpass=f=30,\
bass=g=-1.0:f=90:w=0.6,\
equalizer=f=220:t=q:w=0.9:g=1.2,\
equalizer=f=800:t=q:w=0.85:g=2.0,\
equalizer=f=2800:t=q:w=0.9:g=1.4,\
treble=g=1.6:f=6500:w=0.6[out]" \
  -map "[out]" -ar 48000 -c:a pcm_s24le "$W/space.wav"

echo "→ density (1.5:1 half-mix) + loudness taken at the ceiling"
# Measured 2026-09-02 (the 88-bar arrangement) with GAIN2=2.0:
#   -9.3 LUFS · LRA 2.0 · -3.0 dBTP · PLR 6.3 · mono 0.0 · phone -3.6
# LRA/PLR under their bands = compositional: a continuous club sprint.
GAIN2="${GAIN2:-2.0}"
LIMIT="${LIMIT:-0.760}"
ffmpeg -y -v error -i "$W/space.wav" -af \
  "acompressor=threshold=0.20:ratio=1.5:attack=30:release=180:knee=4:link=maximum:detection=rms:mix=0.50,\
volume=4.0dB,\
asoftclip=type=tanh:threshold=0.62:output=0.92,\
volume=${GAIN2}dB,\
aresample=192000,\
alimiter=limit=${LIMIT}:attack=4:release=120:asc=true:asc_level=0.35:level=false,\
aresample=48000" \
  -ar 48000 -c:a pcm_s24le "$MASTER"

echo "→ deliverables"
ffmpeg -y -v error -i "$MASTER" -c:a flac -compression_level 8 -sample_fmt s32 \
  -metadata title="lonerclubRAPID" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/lonerclub-rapid.flac"
ffmpeg -y -v error -i "$MASTER" -c:a libmp3lame -b:a 320k \
  -metadata title="lonerclubRAPID" -metadata artist="Whistlegraph Dot Org" \
  -metadata album="pixsies" "$OUT/lonerclub-rapid.mp3"

echo "→ verify"
ffmpeg -hide_banner -nostats -i "$MASTER" \
  -af ebur128=peak=true:framelog=quiet -f null - 2>&1 | grep -E "^\s+(I|LRA|Peak):"
node pop/bin/master-audit.mjs "$MASTER"
echo "✓ $OUT/lonerclub-rapid.flac"
