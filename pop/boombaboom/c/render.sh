#!/usr/bin/env bash
# render.sh — the full boombaboom render, all native (C engine + ffmpeg master,
# no JS). Autotunes the take to D minor (once, cached), builds + runs the C
# engine, then a light club master → mp3 + wav in ~/Documents/Shelf/boombaboom.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
ROOT="$(cd -- "$HERE/.." && pwd)"          # pop/boombaboom
POP="$(cd -- "$ROOT/.." && pwd)"           # pop
OUT="${1:-$HOME/Documents/Shelf/boombaboom}"
mkdir -p "$OUT"

RAW="$ROOT/sources/boombaboom-raw.wav"
TUNED="$ROOT/sources/boombaboom-tuned.wav"
ALIGNED="$ROOT/sources/boombaboom-aligned.wav"
HARM5="$ROOT/sources/boombaboom-harm5.wav"
HARMOCT="$ROOT/sources/boombaboom-harmoct.wav"
HARM3="$ROOT/sources/boombaboom-harm3.wav"
HARMDN="$ROOT/sources/boombaboom-harmdn.wav"
OSC="$ROOT/sources/boombaboom-osc.wav"
H5TMP="$ROOT/sources/.boombaboom-harm5-pre.wav"
H3TMP="$ROOT/sources/.boombaboom-harm3-pre.wav"
# --sines (or env SINES=1) → pure-sine variant with a -sines suffix.
SINES_FLAG=""; SUF=""
if [ "${SINES:-0}" = "1" ] || [ "${2:-}" = "--sines" ]; then SINES_FLAG="--sines"; SUF="-sines"; fi
BEDRAW="$HERE/out/boombaboom${SUF}-raw.wav"
MASTER="$OUT/boombaboom${SUF}-MASTER.wav"
MP3="$OUT/boombaboom${SUF}.mp3"
PY="$POP/.venv/bin/python"
mkdir -p "$HERE/out"
BPM=140

# 1 · autotune the take to D minor — note-mode, pushed harder (low preserve →
#     more obviously tuned but still musical). Cached on the raw mtime.
if [ ! -f "$TUNED" ] || [ "$RAW" -nt "$TUNED" ]; then
  echo "# autotune → D minor (strong)"
  "$PY" "$POP/bin/autotune.py" "$RAW" "$TUNED" \
    --key D --scale minor --mode note --strength 0.97 --preserve 0.22
fi

# 2 · beat-align: fit the 'boom'/'ma' plosive onsets to 8th-notes, stretch the
#     rest around them (rubberband timemap). Keeps his melody + words.
if [ ! -f "$ALIGNED" ] || [ "$TUNED" -nt "$ALIGNED" ]; then
  echo "# beat-align → $BPM, booms snapped to the KICK (beats)"
  "$PY" "$ROOT/bin/grid-warp.py" "$TUNED" "$ALIGNED" --bpm "$BPM" --grid 1
  # then STRETCH it longer for an epic, drawn-out "boooooma" (formant-preserving R3).
  echo "# stretch vocal 1.5× → long sustained boooooma"
  rubberband -3 --time 1.5 --formant "$ALIGNED" "$ROOT/sources/.aligned-stretch.wav" >/dev/null 2>&1 \
    && mv "$ROOT/sources/.aligned-stretch.wav" "$ALIGNED"
fi

# 3 · harmonies (stay in D minor): octave up = always diatonic; fifth up needs
#     a re-snap pass (snap→+7 can land off-scale) so it stays clean.
if [ ! -f "$HARMOCT" ] || [ "$ALIGNED" -nt "$HARMOCT" ]; then
  echo "# harmony: octave up"
  "$PY" "$POP/bin/autotune.py" "$ALIGNED" "$HARMOCT" \
    --key D --scale minor --mode frame --strength 1.0 --shift 12
fi
if [ ! -f "$HARM5" ] || [ "$ALIGNED" -nt "$HARM5" ]; then
  echo "# harmony: fifth up (two-pass, re-snapped to Dm)"
  "$PY" "$POP/bin/autotune.py" "$ALIGNED" "$H5TMP" \
    --key D --scale minor --mode frame --strength 1.0 --shift 7
  "$PY" "$POP/bin/autotune.py" "$H5TMP" "$HARM5" \
    --key D --scale minor --mode frame --strength 1.0 --shift 0
  rm -f "$H5TMP"
fi
if [ ! -f "$HARM3" ] || [ "$ALIGNED" -nt "$HARM3" ]; then
  echo "# harmony: diatonic third up (two-pass, re-snapped to Dm)"
  "$PY" "$POP/bin/autotune.py" "$ALIGNED" "$H3TMP" \
    --key D --scale minor --mode frame --strength 1.0 --shift 4
  "$PY" "$POP/bin/autotune.py" "$H3TMP" "$HARM3" \
    --key D --scale minor --mode frame --strength 1.0 --shift 0
  rm -f "$H3TMP"
fi
if [ ! -f "$HARMDN" ] || [ "$ALIGNED" -nt "$HARMDN" ]; then
  echo "# harmony: octave down (for the grounded ending)"
  "$PY" "$POP/bin/autotune.py" "$ALIGNED" "$HARMDN" \
    --key D --scale minor --mode frame --strength 1.0 --shift -12
fi

# 3b · the OSC feature — cut the end "ooowwoowwoo" wobble (~28–32.4s of the
#      aligned take) with micro-fades, for the prelude + bridge hooks. Also make
#      octave-up / octave-down copies (SAME duration) so the prelude can stack
#      one phrase across octaves IN SYNC.
OSCUP="$ROOT/sources/boombaboom-osc-up.wav"
OSCDN="$ROOT/sources/boombaboom-osc-dn.wav"
if [ ! -f "$OSC" ] || [ "$ALIGNED" -nt "$OSC" ]; then
  echo "# osc feature: end wobble segment (+ octaves)"
  ffmpeg -hide_banner -loglevel error -y -ss 42.0 -i "$ALIGNED" \
    -af "afade=t=in:st=0:d=0.04,afade=t=out:st=5.5:d=0.6" -t 6.2 \
    -ar 48000 -ac 1 "$OSC"
  rubberband -3 --pitch 12 --formant "$OSC" "$OSCUP" >/dev/null 2>&1
  rubberband -3 --pitch -12 --formant "$OSC" "$OSCDN" >/dev/null 2>&1
fi

# 3c · single-beat BOOM + MA hits to scatter rhythmically. BOOM = the very
#      first plosive of the take (sharp), MA = a sustained vowel a bit later.
BOOM="$ROOT/sources/boombaboom-boom.wav"
MA="$ROOT/sources/boombaboom-ma.wav"
if [ ! -f "$BOOM" ] || [ "$ALIGNED" -nt "$BOOM" ]; then
  echo "# single-beat boom + ma hits"
  ffmpeg -hide_banner -loglevel error -y -ss 0.0 -i "$ALIGNED" \
    -af "afade=t=in:st=0:d=0.005,afade=t=out:st=0.40:d=0.06" -t 0.46 \
    -ar 48000 -ac 1 "$BOOM"
  ffmpeg -hide_banner -loglevel error -y -ss 3.0 -i "$ALIGNED" \
    -af "afade=t=in:st=0:d=0.02,afade=t=out:st=0.40:d=0.06" -t 0.46 \
    -ar 48000 -ac 1 "$MA"
  # SHORT, percussive isolations of the transient — tight hits to scatter like
  # drums on their own track (boom = the plosive attack, ma = the nasal onset).
  ffmpeg -hide_banner -loglevel error -y -ss 0.0 -i "$ALIGNED" \
    -af "afade=t=in:st=0:d=0.003,afade=t=out:st=0.15:d=0.04" -t 0.19 \
    -ar 48000 -ac 1 "$ROOT/sources/boombaboom-boomp.wav"
  ffmpeg -hide_banner -loglevel error -y -ss 3.0 -i "$ALIGNED" \
    -af "afade=t=in:st=0:d=0.01,afade=t=out:st=0.13:d=0.04" -t 0.17 \
    -ar 48000 -ac 1 "$ROOT/sources/boombaboom-map.wav"
fi

# 4 · build + render the C engine (sine bed + 3 vocal layers + harmonies + osc).
#     Run FROM the engine dir so its relative "../sources/" defaults (osc
#     octaves, boom/ma, rain) resolve to pop/boombaboom/sources.
bash "$HERE/build.sh"
( cd "$HERE" && ./boombaboom --out "$BEDRAW" --bpm "$BPM" $SINES_FLAG \
  --vocal "$ALIGNED" --harm5 "$HARM5" --harmoct "$HARMOCT" \
  --harm3 "$HARM3" --harmdn "$HARMDN" --osc "$OSC" )

# 5 · POP MASTER — bright, punchy, vocal-forward, loud (~-9 LUFS): tighten the
# lows, scoop a little mud, lift presence + air for sheen, gentle bus glue,
# soft-clip for density, loudnorm + ceiling. Wider top via a light stereo lift.
MASTER_AF="highpass=f=30,\
equalizer=f=60:t=q:w=0.9:g=2.0,\
equalizer=f=300:t=q:w=1.1:g=-1.6,\
equalizer=f=3000:t=q:w=1.2:g=0.5,\
equalizer=f=6500:t=q:w=1.0:g=0.7,\
highshelf=f=12000:g=3.2,\
highshelf=f=16000:g=2.2,\
lowpass=f=19000,\
acompressor=threshold=-20dB:ratio=1.9:attack=30:release=260:makeup=1.3:knee=8,\
asoftclip=type=tanh:threshold=0.985,\
stereotools=slev=1.08,\
loudnorm=I=-13:TP=-1.5:LRA=12,\
alimiter=limit=0.95:attack=5:release=90"

echo "# master → $MASTER"
ffmpeg -hide_banner -loglevel error -y -i "$BEDRAW" -af "$MASTER_AF" \
  -ar 44100 -sample_fmt s16 "$MASTER"

# 6 · ACCELERANDO — gradually speed the whole mix up (pitch-preserving), so the
# track pushes faster as it goes. Applied to the master so bed + vocal ramp in
# sync. (+9% by the end.)
echo "# accelerando (+18% by the end → trance build)"
"$PY" "$ROOT/bin/accel.py" "$MASTER" "$ROOT/sources/.master-accel.wav" 0.18
# re-normalize after the accel (rubberband overshoots past the limiter) with a
# true-peak ceiling so there is NO clipping.
ffmpeg -hide_banner -loglevel error -y -i "$ROOT/sources/.master-accel.wav" \
  -af "loudnorm=I=-13:TP=-1.5:LRA=12,aformat=s16" -ar 44100 "$ROOT/sources/.master-pre.wav"
rm -f "$ROOT/sources/.master-accel.wav"

# FINAL fade-out AFTER loudnorm (so the normalizer can't pump the tail back up —
# that was the "stops then starts again" ending). Clean 9s fade.
DUR=$("$PY" -c "import soundfile as sf; print(sf.info('$ROOT/sources/.master-pre.wav').duration)")
FST=$("$PY" -c "print(max(0.0, $DUR - 9.0))")
ffmpeg -hide_banner -loglevel error -y -i "$ROOT/sources/.master-pre.wav" \
  -af "afade=t=out:st=${FST}:d=9,aformat=s16" -ar 44100 "$MASTER"
rm -f "$ROOT/sources/.master-pre.wav"

ffmpeg -hide_banner -loglevel error -y -i "$MASTER" -c:a libmp3lame -b:a 320k "$MP3"
echo "✓ $MP3"
echo "✓ $MASTER"
