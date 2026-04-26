#!/usr/bin/env fish
# compose.fish — final ffmpeg pass: concat slides + audio (with trailing
# silence) + waveform + animated progress bar + word-synced subtitles
# (loaded as movie sources, overlaid with enable=between(t,a,b)).
# Reads out/concat.txt, out/recap.mp3, out/duration.txt, out/subs.json.

set -l ROOT (realpath (dirname (status -f))/..)
set -l OUT $ROOT/out
set -l TOTAL (cat $OUT/duration.txt)
set -l AUDIO $OUT/recap.mp3
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

ffmpeg -hide_banner -y \
  -f concat -safe 0 -i $OUT/concat.txt \
  -i $AUDIO \
  -filter_complex_script $FILTER \
  -map "[final]" -map "[a1]" \
  -c:v libx264 -preset medium -crf 20 -pix_fmt yuv420p \
  -c:a aac -b:a 192k \
  -movflags +faststart \
  -t $TOTAL \
  $VIDEO

echo "✓ $VIDEO"
