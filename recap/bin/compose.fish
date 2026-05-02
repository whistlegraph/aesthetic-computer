#!/usr/bin/env fish
# compose.fish — final ffmpeg pass: concat slides + audio (with trailing
# silence) + waveform + animated progress bar + word-synced subtitles
# (loaded as movie sources, overlaid with enable=between(t,a,b)).
# Reads out/concat.txt, out/recap.mp3, out/duration.txt, out/subs.json.

set -l ROOT (realpath (dirname (status -f))/..)
set -l OUT $ROOT/out
set -l TOTAL (cat $OUT/duration.txt)
set -l AUDIO $OUT/recap.mp3
set -l WALTZ $OUT/waltz.mp3
set -l VIDEO $OUT/recap.mp4
set -l FILTER $OUT/filter.txt

if not test -f $OUT/concat.txt
  echo "✗ missing $OUT/concat.txt — run bin/slides.mjs first"
  exit 1
end
if not test -f $OUT/subs.json
  echo "✗ missing $OUT/subs.json — run bin/subtitles.mjs first"
  exit 1
end

echo "→ ffmpeg compose · $TOTAL s · 1080x1920"

# Build the filter graph in node so we can splice in one overlay per subtitle
# chunk without fish escape gymnastics around brackets and quotes.
node $ROOT/bin/build-filter.mjs $TOTAL > $FILTER

# If a piano-waltz bed exists, append a mix into the same filter graph so
# we don't need a second -filter_complex flag (only the last one wins).
# The waltz is already gain-staged by waltz.mjs (audience.waltz.voiceGain);
# we still clamp it lightly here so it sits well under the spoken track.
if test -f $WALTZ
  echo "  + bed: $WALTZ (waltz)"
  # printf — fish parses $TOTAL[bed] as a slice index; %s sidesteps that.
  # NOTE: rely on `-stream_loop -1` at the input level for looping; do NOT
  # also use `aloop=loop=-1:size=2e9` — that allocates a 2-billion-sample
  # buffer (~24 GB worst case) which OOMs on the 8 GB machine.
  # `atrim=duration=$TOTAL` is enough to cut the looped stream at length.
  printf ';[2:a]volume=0.42,atrim=duration=%s[bed];[a1][bed]amix=inputs=2:duration=first:dropout_transition=0:weights=1.0 0.55[mix]\n' "$TOTAL" >> $FILTER
  ffmpeg -hide_banner -y \
    -f concat -safe 0 -i $OUT/concat.txt \
    -i $AUDIO \
    -stream_loop -1 -i $WALTZ \
    -filter_complex_script $FILTER \
    -map "[final]" -map "[mix]" \
    -c:v h264_videotoolbox -b:v 5M -pix_fmt yuv420p \
    -c:a aac -b:a 192k \
    -movflags +faststart \
    -t $TOTAL \
    $VIDEO
else
  ffmpeg -hide_banner -y \
    -f concat -safe 0 -i $OUT/concat.txt \
    -i $AUDIO \
    -filter_complex_script $FILTER \
    -map "[final]" -map "[a1]" \
    -c:v h264_videotoolbox -b:v 5M -pix_fmt yuv420p \
    -c:a aac -b:a 192k \
    -movflags +faststart \
    -t $TOTAL \
    $VIDEO
end

echo "✓ $VIDEO"
