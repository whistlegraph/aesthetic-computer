#!/usr/bin/env bash
# run.sh — build lonerclub (v4pid) from the sacred v4 audio to a finished master.
#
# v4pid does not render from an engine: its source of truth is the 163 s
# `out/loner-remix-v4.mp3`, whose own source was lost. Everything below splices
# that audio, separates it, and rebuilds the arrangement around her voice.
#
#   SPLICE   v4 31.11–62.951 + 94.426→end, 30 ms crossfade, trimmed to 94.2 s
#   SEPARATE htdemucs, two-stem (vocals) and four-stem (drums/bass/other)
#   STEMS    drums→club kick, other→pads/pluck/bells, plus the composed layers:
#            wub, piano, eager percussion, fills, glass bowl, voice stamp
#   VOCAL    f- take at pass 2, held as one direct centered foreground
#   STAGE    assemble.py places everything by ITD/ILD and gates the space
#   MASTER   cut-wax.sh — the wax/FM material chain, then the lane's law
#
# Usage:  bash pop/loner/bin/v4pid/run.sh [dest.mp3]
#   SPLICE=0  reuse the existing splice        STEMS=0  reuse the stems
#   VOX=0     reuse the vocal                  TARGET   master LUFS (-13.5)
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../../../.." && pwd)"
cd "$REPO"

export V4PID_WORK="${V4PID_WORK:-$HOME/.cache/ac/v4pid}"
S="$V4PID_WORK"
mkdir -p "$S"
DEST="${1:-pop/loner/out/lonerclub-v4pid-release.mp3}"
PY="${PY:-python3}"

if [ "${SPLICE:-1}" = "1" ]; then
  echo "→ splice"
  ffmpeg -y -v error -i pop/loner/out/loner-remix-v4.mp3 -filter_complex \
    "[0:a]atrim=31.11:62.951,asetpts=PTS-STARTPTS,afade=t=in:st=0:d=0.005[a];\
[0:a]atrim=94.426,asetpts=PTS-STARTPTS[b];[a][b]acrossfade=d=0.03:c1=tri:c2=tri[out]" \
    -map "[out]" -ar 48000 -c:a pcm_s24le "$S/v4pid.wav"
  ffmpeg -y -v error -i "$S/v4pid.wav" -af "atrim=0:94.2,afade=t=out:st=92.7:d=1.4" \
    -ar 48000 -c:a pcm_s24le "$S/v4pid-trim.wav"

  echo "→ separate (slow)"
  demucs -n htdemucs --two-stems=vocals -o "$S/sep2" "$S/v4pid-trim.wav" 2>&1 | tail -1
  demucs -n htdemucs                    -o "$S/sep4" "$S/v4pid-trim.wav" 2>&1 | tail -1
fi

if [ "${STEMS:-1}" = "1" ]; then
  echo "→ drums"
  $PY "$HERE/gen-drums.py"
  # the club kick splits at 180 Hz; the 'other' bus becomes pads / pluck / bells
  ffmpeg -y -v error -f f32le -ar 48000 -ac 2 -i "$S/drums-cool.raw" -af "lowpass=f=180"  -f f32le "$S/st-kick.raw"
  ffmpeg -y -v error -f f32le -ar 48000 -ac 2 -i "$S/drums-cool.raw" -af "highpass=f=180" -f f32le "$S/st-hats.raw"
  O="$S/sep4/htdemucs/v4pid-trim/other.wav"
  ffmpeg -y -v error -i "$O" -af "lowpass=f=700"                 -f f32le "$S/st-pads.raw"
  ffmpeg -y -v error -i "$O" -af "highpass=f=700,lowpass=f=2800" -f f32le "$S/st-pluck.raw"
  ffmpeg -y -v error -i "$O" -af "highpass=f=2800"               -f f32le "$S/st-bells.raw"

  echo "→ instruments"
  $PY "$HERE/gen-piano.py"     # also lays a hat pattern…
  $PY "$HERE/gen-swing.py"     # …which this supersedes with the eager hand

  # the glass meditation bowls come out of the physical-model bell engine
  [ -x /tmp/bell ] || { bash pop/bell/c/build.sh >/dev/null && cp pop/bell/c/bell /tmp/bell; }
  /tmp/bell --note "D#6" --material glass --geometry bowl --dur 9 --vel 0.26 --sr 48000 --out "$S/bowlD.wav" >/dev/null
  $PY "$HERE/gen-fills.py"

  echo "→ stamp"
  # spoken by ElevenLabs in jeffrey's voice, then WORLD-snapped onto F2/C#2/A#1
  printf "Whistlegraph Dot Org\n" > "$S/stamp.txt"
  [ -f "$S/stamp-jeffrey.mp3" ] || node spinging/bin/spinging.mjs say "$S/stamp.txt" --out "$S/stamp-jeffrey.mp3" 2>&1 | tail -1
  ffmpeg -y -v error -i "$S/stamp-jeffrey.mp3" -ar 48000 -ac 1 "$S/stamp-jeffrey.wav"
  pop/.venv/bin/python pop/bin/pitchsnap_world.py "$S/stamp-jeffrey.wav" \
    "$S/stamp-jsnapped.wav" --notes "F2,C#2,A#1" --detect-boundaries 2>&1 | tail -1
  $PY "$HERE/gen-stamp.py"
fi

if [ "${VOX:-1}" = "1" ]; then
  echo "→ vocal"
  $PY "$HERE/build-vocal.py"
  ffmpeg -y -v error -f f32le -ar 48000 -ac 2 -i "$S/vox-arped.raw" -c:a pcm_s24le "$S/vox-arped.wav"
  $PY "$HERE/steppan.py" "$S/vox-arped.wav" "$S/vocalsFX.wav" >/dev/null
fi

# The club wub keys its envelope from the finished direct vocal, so regenerate
# it after either the instrument or vocal side of the arrangement changes.
if [ "${STEMS:-1}" = "1" ] || [ "${VOX:-1}" = "1" ]; then
  echo "→ vocal-keyed club wub"
  $PY "$HERE/gen-wub.py"
fi

echo "→ assemble"
$PY "$HERE/assemble.py"
ffmpeg -y -v error -f f32le -ar 48000 -ac 2 -i "$S/premaster.raw" -c:a pcm_s24le "$S/premaster.wav"

echo "→ master"
TARGET="${TARGET:--13.5}" bash pop/loner/c/cut-wax.sh "$S/premaster.wav" "$DEST" 2>&1 | tail -2
ffmpeg -y -v error -i "$DEST" -c copy \
  -metadata title="lonerclub" -metadata artist="Whistlegraph Dot Org" -metadata album="pixsies" "$S/t.mp3"
mv "$S/t.mp3" "$DEST"
echo "✓ $DEST"
