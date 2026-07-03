#!/usr/bin/env bash
# render.sh — full emotribtranse render, all native (C engine → ffmpeg trance
# master), no JS. Builds + runs the engine, then a driving club master —
# firm kick fundamental, presence for the claps/hats, air for the bells and
# the melted triangle top — → mp3 + wav in ~/Documents/Shelf/emotribtranse.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
OUT="${1:-$HOME/Documents/Shelf/emotribtranse}"
mkdir -p "$OUT" "$HERE/out"
BPM="${BPM:-155}"

RAW="$HERE/out/emotribtranse-raw.wav"
MASTER="$OUT/emotribtranse-MASTER.wav"
MP3="$OUT/emotribtranse.mp3"

# 1 · build + render the C engine
bash "$HERE/build.sh"
( cd "$HERE" && ./emotribtranse --out "$RAW" --bpm "$BPM" )

# 2 · TRANCE MASTER — protect the sub, keep the four-on-the-floor tight and
# loud. Highpass the rumble, firm the kick fundamental, scoop low-mid mud,
# lift presence + air (the melt zone lives up there), glue compress, soft-clip
# for density, then TWO-PASS loudnorm (measure → linear gain) so the
# arrangement's dynamics survive — single-pass loudnorm rides gain and irons
# the whole track into a flat sausage (quiet intro boosted to drop loudness,
# melt no louder than anything else). No heavy crunch — the bells and marimba
# stay clean; the engine's wavefolder is the only distortion artist.
TONE_AF="highpass=f=24,\
equalizer=f=52:t=q:w=0.8:g=2.4,\
equalizer=f=120:t=q:w=1.0:g=-1.2,\
equalizer=f=350:t=q:w=1.1:g=-2.0,\
equalizer=f=3500:t=q:w=1.1:g=1.2,\
equalizer=f=8000:t=q:w=1.0:g=1.2,\
highshelf=f=12000:g=2.2,\
lowpass=f=19000,\
acompressor=threshold=-18dB:ratio=2.2:attack=12:release=180:makeup=1.4:knee=8,\
asoftclip=type=tanh:threshold=0.98"
LN="I=-10.5:TP=-1.2:LRA=10"

echo "# measure loudness (pass 1)"
STATS=$(ffmpeg -hide_banner -y -i "$RAW" -af "$TONE_AF,loudnorm=$LN:print_format=json" \
  -f null - 2>&1 | sed -n '/^{/,/^}/p')
mI=$(echo "$STATS"     | sed -n 's/.*"input_i" *: *"\([^"]*\)".*/\1/p')
mTP=$(echo "$STATS"    | sed -n 's/.*"input_tp" *: *"\([^"]*\)".*/\1/p')
mLRA=$(echo "$STATS"   | sed -n 's/.*"input_lra" *: *"\([^"]*\)".*/\1/p')
mTH=$(echo "$STATS"    | sed -n 's/.*"input_thresh" *: *"\([^"]*\)".*/\1/p')
mOFF=$(echo "$STATS"   | sed -n 's/.*"target_offset" *: *"\([^"]*\)".*/\1/p')

echo "# master → $MASTER (pass 2, linear: I=$mI TP=$mTP LRA=$mLRA)"
ffmpeg -hide_banner -loglevel error -y -i "$RAW" -af "$TONE_AF,\
loudnorm=$LN:measured_I=$mI:measured_TP=$mTP:measured_LRA=$mLRA:measured_thresh=$mTH:offset=$mOFF:linear=true,\
alimiter=limit=0.96:attack=4:release=80" \
  -ar 44100 -sample_fmt s16 "$MASTER"

ffmpeg -hide_banner -loglevel error -y -i "$MASTER" -c:a libmp3lame -b:a 320k "$MP3"
echo "✓ $MP3"
echo "✓ $MASTER"
