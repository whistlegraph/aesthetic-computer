#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
AC="${AESTHETIC_ROOT:-/Users/jas/aesthetic-computer}"
PY="$AC/pop/.venv/bin/python"
AUTOTUNE="$AC/pop/bin/autotune.py"
GRID="$AC/pop/boombaboom/bin/grid-warp.py"
DUBS="$ROOT/voice-takes/dubs"
WORK="$ROOT/out/aesthetivox"
STEMS="$ROOT/out/wattajetta-stone-club-stems"
OUT="$ROOT/out/aesthetivox-mix"

mkdir -p "$WORK" "$OUT"

prep_take() {
  local n="$1" caf="$2"
  local raw="$WORK/take-$n-raw.wav"
  local tuned="$WORK/take-$n-tuned.wav" aligned="$WORK/take-$n-aligned.wav"
  if [ ! -f "$raw" ] || [ "$caf" -nt "$raw" ]; then
    ffmpeg -y -hide_banner -loglevel error -i "$caf" -ar 48000 -ac 1 -c:a pcm_f32le "$raw"
  fi
  if [ ! -f "$tuned" ] || [ "$raw" -nt "$tuned" ]; then
    "$PY" "$AUTOTUNE" "$raw" "$tuned" \
      --key E --scale minorpent --mode note --strength 0.94 --preserve 0.34
  fi
  if [ ! -f "$aligned" ] || [ "$tuned" -nt "$aligned" ]; then
    "$PY" "$GRID" "$tuned" "$aligned" \
      --bpm 138 --grid 2 --anchor 0.42 --max-ratio 1.35
  fi
}

prep_take 1 "$DUBS/2026-08-08T08-04-36Z.caf"
prep_take 2 "$DUBS/2026-08-08T08-08-27Z.caf"

T1="$WORK/take-1-aligned.wav"
T2="$WORK/take-2-aligned.wav"
BRIGHT="$WORK/take-2-bright.wav"
LOW="$WORK/take-2-low.wav"

if [ ! -f "$BRIGHT" ] || [ "$T2" -nt "$BRIGHT" ]; then
  "$PY" "$AUTOTUNE" "$T2" "$BRIGHT" \
    --key E --scale majorpent --mode frame --strength 0.98 --preserve 0.12
fi
if [ ! -f "$LOW" ] || [ "$T2" -nt "$LOW" ]; then
  "$PY" "$AUTOTUNE" "$T2" "$LOW" \
    --key E --scale minorpent --mode frame --strength 1 --preserve 0.10 --shift -12
fi

VOX="$OUT/wattajetta-aesthetivox-events.wav"
MIX="$OUT/wattajetta-aesthetivox-mix.wav"
MP3="$OUT/wattajetta-aesthetivox-mix.mp3"

# The long performances are source libraries, never continuous lead tracks.
# Windows follow the 138 BPM arrangement; silence between events is authored.
ffmpeg -y -hide_banner -loglevel warning \
  -i "$T1" -i "$T2" -i "$BRIGHT" -i "$LOW" \
  -filter_complex \
  "[0:a]asplit=2[t1][ts]; \
   [t1]highpass=f=78,lowpass=f=14500,equalizer=f=280:t=q:w=1.1:g=-3,equalizer=f=3400:t=q:w=1:g=3,deesser=i=0.20:m=0.42:f=0.55,acompressor=threshold=0.07:ratio=3:attack=6:release=95:makeup=1.6,volume='0.90*(between(t,13.913,20.870)+between(t,31.304,34.782)+between(t,48.696,52.174)+between(t,78.261,81.739)+between(t,93.913,97.391)+between(t,116.522,120.000))':eval=frame[lead]; \
   [1:a]highpass=f=95,lowpass=f=12000,equalizer=f=330:t=q:w=1:g=-4,acompressor=threshold=0.08:ratio=4:attack=4:release=85:makeup=1.5,adelay=18,volume='0.56*(between(t,34.782,38.261)+between(t,62.609,66.087)+between(t,97.391,100.870)+between(t,107.826,111.304))':eval=frame[answer]; \
   [2:a]highpass=f=145,lowpass=f=9200,aecho=0.75:0.45:110|220:0.18|0.09,volume='0.24*(between(t,66.087,67.826)+between(t,100.870,102.609)+between(t,111.304,113.043))':eval=frame[bright]; \
   [3:a]highpass=f=70,lowpass=f=6500,volume='0.20*(between(t,76.522,78.261)+between(t,104.348,106.087))':eval=frame[low]; \
   [ts]atrim=start=85.217:end=85.652,asetpts=PTS-STARTPTS,afade=t=in:d=0.006,afade=t=out:st=0.38:d=0.05,asplit=8[s0][s1][s2][s3][s4][s5][s6][s7]; \
   [s0]adelay=85217[x0];[s1]adelay=85652[x1];[s2]adelay=86087[x2];[s3]adelay=86522[x3];[s4]adelay=86957[x4];[s5]adelay=87391[x5];[s6]adelay=87826[x6];[s7]adelay=88261[x7]; \
   [lead][answer][bright][low][x0][x1][x2][x3][x4][x5][x6][x7]amix=inputs=12:duration=longest:normalize=0,adeclick=w=55:o=75:a=2:t=2:b=2:m=s,aecho=0.8:0.25:95|190:0.06|0.03,loudnorm=I=-20:TP=-5:LRA=11,alimiter=limit=0.92:attack=4:release=70[vox]" \
  -map "[vox]" -ar 48000 -ac 2 -c:a pcm_f32le "$VOX"

# Voice keys only competing musical stems; kick, samples and Trash FX stay put.
ffmpeg -y -hide_banner -loglevel warning \
  -i "$STEMS/01-kick.wav" -i "$STEMS/02-water-engine.wav" \
  -i "$STEMS/03-stone-bells-uke.wav" -i "$STEMS/04-disco-bass-squares.wav" \
  -i "$STEMS/05-guitar.wav" -i "$STEMS/06-samples-fx.wav" \
  -i "$STEMS/07-empty-trash-fx.wav" -i "$VOX" \
  -filter_complex \
  "[7:a]apad,asplit=5[kw][kb][kd][kg][vox]; \
   [1:a][kw]sidechaincompress=threshold=0.020:ratio=2.4:attack=7:release=145[water]; \
   [2:a][kb]sidechaincompress=threshold=0.018:ratio=3:attack=5:release=180[bells]; \
   [3:a][kd]sidechaincompress=threshold=0.022:ratio=2.2:attack=8:release=125[disco]; \
   [4:a][kg]sidechaincompress=threshold=0.018:ratio=3.2:attack=6:release=155[guitar]; \
   [0:a][water][bells][disco][guitar][5:a][6:a]amix=inputs=7:duration=longest:normalize=0,acompressor=threshold=0.125:ratio=1.8:attack=16:release=140:makeup=1.08,equalizer=f=54:t=q:w=0.8:g=1.6,volume=0.944[bed]; \
   [bed][vox]amix=inputs=2:duration=first:normalize=0,volume=1.35,alimiter=limit=0.89:attack=5:release=80[mix]" \
  -map "[mix]" -ar 48000 -c:a pcm_s24le "$MIX"

ffmpeg -y -hide_banner -loglevel warning -i "$MIX" -c:a libmp3lame -b:a 320k "$MP3"
printf '%s\n' "$VOX" "$MIX" "$MP3"
