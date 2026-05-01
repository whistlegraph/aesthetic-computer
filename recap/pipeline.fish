#!/usr/bin/env fish
# pipeline.fish — full end-to-end recap build.
# Usage: ./pipeline.fish [audience-name]   (default: fia)
#        ./pipeline.fish fia --skip-tts    (reuse existing out/recap.mp3)

set -l ROOT (realpath (dirname (status -f)))
set -l AUDIENCE $argv[1]
test -z "$AUDIENCE"; and set AUDIENCE fia
set -l SKIP_TTS 0
contains -- --skip-tts $argv; and set SKIP_TTS 1

cd $ROOT

echo "━━━ recap pipeline · audience=$AUDIENCE ━━━"

if test $SKIP_TTS -eq 0
  echo "▸ 1/6 tts"
  node bin/tts.mjs $AUDIENCE; or exit 1
else
  echo "▸ 1/6 tts (skipped — reusing out/recap.mp3)"
end

echo "▸ 2/7 transcribe + align"
node bin/transcribe.mjs; or exit 1
node bin/align.mjs $AUDIENCE; or exit 1

echo "▸ 3/7 jeffrey-photos (gpt-image-2, cached per segment)"
node bin/jeffrey-photos.mjs $AUDIENCE; or exit 1

echo "▸ 4/7 scout (resolve per-slide content queries)"
node bin/scout.mjs $AUDIENCE; or exit 1

echo "▸ 5/7 slides"
node bin/slides.mjs $AUDIENCE; or exit 1

echo "▸ 6/7 subtitles"
node bin/subtitles.mjs $AUDIENCE; or exit 1

echo "▸ 7/7 compose"
fish bin/compose.fish; or exit 1

echo "━━━ done · $ROOT/out/recap.mp4 ━━━"
ls -lh $ROOT/out/recap.mp4
