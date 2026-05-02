#!/usr/bin/env fish
# compose.fish — final ffmpeg pass: concat slides + audio (with trailing
# silence) + waveform + animated progress bar + word-synced subtitles.
#
# Subtitles arrive as a single concat-demuxer track (out/subtitle-track.txt
# pointing to full-frame transparent PNGs with stored durations) — see
# subtitle-track.mjs. That replaces the 135-deep movie= overlay chain that
# bottlenecked the oven encode. Filter graph = a single overlay onto the
# slide stream.
#
# Reads out/concat.txt, out/recap.mp3, out/duration.txt, out/subs.json,
# out/subtitle-track.txt.

set -l ROOT (realpath (dirname (status -f))/..)
set -l OUT $ROOT/out
set -l TOTAL (cat $OUT/duration.txt)
set -l AUDIO $OUT/recap.mp3
set -l WALTZ $OUT/waltz.mp3
set -l SUBTRACK $OUT/subtitle-track.webm
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
if not test -f $SUBTRACK
  echo "✗ missing $SUBTRACK — run bin/subtitle-track.mjs first"
  exit 1
end

echo "→ ffmpeg compose · $TOTAL s · 1080x1920"

# Build the filter graph in node. With the single-overlay subtitle track
# the graph is short — three formatting filters on slides, an audio split
# for the showwaves, and a final overlay of the subtitle stream.
node $ROOT/bin/build-filter.mjs $TOTAL > $FILTER

# If a piano-waltz bed exists, append a mix into the same filter graph so
# we don't need a second -filter_complex flag (only the last one wins).
# Slides=0, narration=1, subs=2, waltz=3 — input order matters.
if test -f $WALTZ
  echo "  + bed: $WALTZ (waltz)"
  # printf — fish parses $TOTAL[bed] as a slice index; %s sidesteps that.
  printf ';[3:a]volume=0.42,atrim=duration=%s[bed];[a1][bed]amix=inputs=2:duration=first:dropout_transition=0:weights=1.0 0.55[mix]\n' "$TOTAL" >> $FILTER
  ffmpeg -hide_banner -y \
    -f concat -safe 0 -i $OUT/concat.txt \
    -i $AUDIO \
    -i $SUBTRACK \
    -stream_loop -1 -i $WALTZ \
    -filter_complex_script $FILTER \
    -map "[final]" -map "[mix]" \
    -c:v libx264 -preset ultrafast -crf 22 -pix_fmt yuv420p \
    -c:a aac -b:a 192k \
    -movflags +faststart \
    -t $TOTAL \
    $VIDEO
else
  ffmpeg -hide_banner -y \
    -f concat -safe 0 -i $OUT/concat.txt \
    -i $AUDIO \
    -i $SUBTRACK \
    -filter_complex_script $FILTER \
    -map "[final]" -map "[a1]" \
    -c:v libx264 -preset ultrafast -crf 22 -pix_fmt yuv420p \
    -c:a aac -b:a 192k \
    -movflags +faststart \
    -t $TOTAL \
    $VIDEO
end

echo "✓ $VIDEO"
