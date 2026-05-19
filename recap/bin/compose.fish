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
# Prefer the sung version of the narration when sing.mjs has produced
# one — falls back to plain TTS otherwise.
set -l AUDIO $OUT/recap.mp3
if test -f $OUT/recap-sung.mp3
  set AUDIO $OUT/recap-sung.mp3
  echo "  · using sung vocal: $OUT/recap-sung.mp3"
end
set -l WALTZ $OUT/waltz.mp3
set -l BEAT $OUT/beat.mp3
set -l SUBSASS $OUT/subs.ass
set -l VIDEO $OUT/recap.mp4
set -l FILTER $OUT/filter.txt

# Prefer ffmpeg-full (built with libass) when present — Homebrew's plain
# `ffmpeg` formula now ships without libass, so the `subtitles=` filter
# fails as "Filter not found." Fall back to PATH ffmpeg otherwise (e.g.
# the oven, where ffmpeg is built with libass directly).
set -l FFMPEG ffmpeg
if test -x /opt/homebrew/opt/ffmpeg-full/bin/ffmpeg
  set FFMPEG /opt/homebrew/opt/ffmpeg-full/bin/ffmpeg
end

if not test -f $OUT/concat.txt
  echo "✗ missing $OUT/concat.txt — run bin/slides.mjs first"
  exit 1
end
if not test -f $OUT/subs.json
  echo "✗ missing $OUT/subs.json — run bin/subtitles.mjs first"
  exit 1
end
if not test -f $SUBSASS
  echo "✗ missing $SUBSASS — run bin/subtitle-track.mjs first"
  exit 1
end

# Pre-render photos.txt → photos.mov AND chrome.txt → chrome.mov. The
# main compose pass below uses libass `subtitles=` filter, which
# deadlocks ffmpeg's input scheduler at frame=0 forever when the photo
# input is a concat-demuxer over PNGs and there's any audio `-i` input
# (the per-frame libass ticks starve the demuxer). Baking the slide
# streams to continuous-frame mov files sidesteps it. Chrome.mov is
# read via `movie=` filter source (NOT `-i` input) because adding a
# *second* `-i` video input — even from a real video file — alongside
# concat-demuxer images also reliably deadlocks.

# --- photos.mov: scaled to 1188×2112, sar=1, fps=25. Time-dependent
# crop/pan/shake/tmix/grain happens on top of this in the main pass.
set -l PHOTOS_MOV $OUT/photos.mov
set -l PHOTOS_HASH_FILE $OUT/photos.mov.hash
set -l PHOTOS_HASH (cat $OUT/photos.txt; for f in (grep "^file " $OUT/photos.txt | sed -E "s/^file '(.*)'\$/\1/"); shasum -a 256 $f; end | shasum -a 256 | awk '{print $1}')
if test -f $PHOTOS_MOV -a -f $PHOTOS_HASH_FILE -a "$PHOTOS_HASH" = (cat $PHOTOS_HASH_FILE 2>/dev/null)
  echo "→ photos.mov cached (hash $PHOTOS_HASH) · skipping pre-render"
else
  echo "→ pre-render photos.txt → photos.mov (h264, 1188×2112, fps=25)"
  $FFMPEG -hide_banner -y -f concat -safe 0 -i $OUT/photos.txt \
    -vf "fps=25,format=yuv420p,scale=-2:2112,setsar=1,crop=1188:2112" \
    -c:v libx264 -preset ultrafast -crf 18 \
    $PHOTOS_MOV 2>&1 | tail -3
  or exit 1
  test -s $PHOTOS_MOV; or echo "✗ photos.mov failed to render"; or exit 1
  echo $PHOTOS_HASH > $PHOTOS_HASH_FILE
end

# --- chrome.mov (qtrle, alpha preserved).
#
# Cache: hash chrome.txt + every PNG it lists. Skip the ~2 min qtrle
# encode unless any chrome PNG changed.
set -l CHROME_MOV $OUT/chrome.mov
set -l CHROME_HASH_FILE $OUT/chrome.mov.hash
set -l CHROME_HASH (cat $OUT/chrome.txt; for f in (grep "^file " $OUT/chrome.txt | sed -E "s/^file '(.*)'\$/\1/"); shasum -a 256 $f; end | shasum -a 256 | awk '{print $1}')
if test -f $CHROME_MOV -a -f $CHROME_HASH_FILE -a "$CHROME_HASH" = (cat $CHROME_HASH_FILE 2>/dev/null)
  echo "→ chrome.mov cached (hash $CHROME_HASH) · skipping pre-render"
else
  echo "→ pre-render chrome.txt → chrome.mov (qtrle alpha)"
  $FFMPEG -hide_banner -y -f concat -safe 0 -i $OUT/chrome.txt \
    -vf "fps=25,format=rgba,scale=1080:1920,setsar=1" \
    -c:v qtrle \
    $CHROME_MOV 2>&1 | tail -3
  or exit 1
  test -s $CHROME_MOV; or echo "✗ chrome.mov failed to render"; or exit 1
  echo $CHROME_HASH > $CHROME_HASH_FILE
end

echo "→ ffmpeg compose · $TOTAL s · 1080x1920"

# Build the filter graph in node. With the single-overlay subtitle track
# the graph is short — three formatting filters on slides, an audio split
# for the showwaves, and a final overlay of the subtitle stream.
node $ROOT/bin/build-filter.mjs $TOTAL > $FILTER

# Inputs (build-filter expects this exact order):
#   0 = photos.txt   (raw jeffrey-photos at slide durations — gets the
#                     handicam crop / pan / shake / grain treatment)
#   1 = narration mp3
#   2 = waltz.mp3   (optional)
#   3 = beat.mp3    (optional)
# Chrome.mov is read via `movie=` filter source inside the graph (NOT a
# `-i` input). Adding chrome as a `-i` input alongside the photos
# concat-demuxer + libass `subtitles=` filter deadlocks ffmpeg's input
# scheduler at frame=0 forever — `movie=` bypasses that scheduler.
if test -f $WALTZ
  echo "  + bed: $WALTZ (waltz, content-length, no loop)"
  if test -f $BEAT
    echo "  + beat: $BEAT (kick on every waltz bar)"
    printf ';[2:a]apad=whole_dur=%s,atrim=duration=%s,volume=0.42[bed];[3:a]apad=whole_dur=%s,atrim=duration=%s,volume=0.55[bk];[a1][bed][bk]amix=inputs=3:duration=first:dropout_transition=0:weights=1.0 0.55 0.45[mix]\n' "$TOTAL" "$TOTAL" "$TOTAL" "$TOTAL" >> $FILTER
    $FFMPEG -hide_banner -y \
      -i $PHOTOS_MOV \
      -i $AUDIO \
      -i $WALTZ \
      -i $BEAT \
      -filter_complex_script $FILTER \
      -map "[final]" -map "[mix]" \
      -c:v libx264 -preset medium -crf 24 -maxrate 6000k -bufsize 12000k -tune grain -pix_fmt yuv420p \
      -c:a aac -b:a 192k \
      -movflags +faststart \
      -t $TOTAL \
      $VIDEO
  else
    printf ';[2:a]apad=whole_dur=%s,atrim=duration=%s,volume=0.42[bed];[a1][bed]amix=inputs=2:duration=first:dropout_transition=0:weights=1.0 0.55[mix]\n' "$TOTAL" "$TOTAL" >> $FILTER
    $FFMPEG -hide_banner -y \
      -i $PHOTOS_MOV \
      -i $AUDIO \
      -i $WALTZ \
      -filter_complex_script $FILTER \
      -map "[final]" -map "[mix]" \
      -c:v libx264 -preset medium -crf 24 -maxrate 6000k -bufsize 12000k -tune grain -pix_fmt yuv420p \
      -c:a aac -b:a 192k \
      -movflags +faststart \
      -t $TOTAL \
      $VIDEO
  end
else
  $FFMPEG -hide_banner -y \
    -i $PHOTOS_MOV \
    -i $AUDIO \
    -filter_complex_script $FILTER \
    -map "[final]" -map "[a1]" \
    -c:v libx264 -preset medium -crf 24 -maxrate 6000k -bufsize 12000k -tune grain -pix_fmt yuv420p \
    -c:a aac -b:a 192k \
    -movflags +faststart \
    -t $TOTAL \
    $VIDEO
end

echo "✓ $VIDEO"
