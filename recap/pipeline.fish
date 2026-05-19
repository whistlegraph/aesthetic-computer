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
  echo "▸ 1/8 tts"
  node bin/tts.mjs $AUDIENCE; or exit 1
else
  echo "▸ 1/8 tts (skipped — reusing out/recap.mp3)"
end

echo "▸ 2/8 transcribe + align"
node bin/transcribe.mjs; or exit 1
node bin/align.mjs $AUDIENCE; or exit 1

echo "▸ 2.5/8 sing (pitchsnap → recap-sung.mp3 + rewrites words.json)"
node bin/sing.mjs $AUDIENCE
or echo "  ↳ sing step skipped or failed — compose will use plain narration"
# Re-align after sing rewrote words.json so segments.json reflects
# the stretched timeline. align caches on words.json hash so this
# only re-runs when sing actually changed something.
node bin/align.mjs $AUDIENCE --force; or exit 1

echo "▸ 3/8 jeffrey-photos (gpt-image-2, cached per segment)"
node bin/jeffrey-photos.mjs $AUDIENCE; or exit 1

echo "▸ 3.3/8 cv (face + laptop + shirt-logo bboxes)"
if test -x .venv/bin/python3
  .venv/bin/python3 bin/debug-composition.py
  or echo "  ↳ cv step failed — layout solver will fall back to defaults"
else
  echo "  ↳ no .venv — skipping cv (layout will fall back to defaults)"
end

echo "▸ 3.4/8 layout (per-segment chrome positions for chapter / subs / piano)"
node bin/layout.mjs $AUDIENCE
or echo "  ↳ layout step skipped or failed — subs/piano use static positions"

echo "▸ 3.45/8 qrs (per-segment QR → tangled.sh commit)"
node bin/qrs.mjs $AUDIENCE
or echo "  ↳ qrs step skipped or failed — slides will render without commit QR"

echo "▸ 3.5/8 chat-fetch (laer-klokken + system snapshots)"
node bin/chat-fetch.mjs
or echo "  ↳ chat-fetch step skipped or failed — chat slide will render empty"

echo "▸ 3.7/8 screenshots (production-URL artifact insets, cached)"
node bin/screenshots.mjs $AUDIENCE
or echo "  ↳ screenshots step skipped or failed — slides without cached artifacts will render without insets"

echo "▸ 4/8 scout (resolve per-slide content queries)"
node bin/scout.mjs $AUDIENCE; or exit 1

echo "▸ 5/8 slides"
node bin/slides.mjs $AUDIENCE; or exit 1

echo "▸ 6/8 subtitles"
node bin/subtitles.mjs $AUDIENCE; or exit 1
node bin/subtitle-track.mjs; or exit 1

echo "▸ 7/8 waltz (piano bed; harmless if audience.waltz is absent)"
node bin/waltz.mjs $AUDIENCE
or echo "  ↳ waltz step skipped or failed — compose falls back to narration-only"

echo "▸ 7.5/8 waltz piano-roll overlay (ASS drawings)"
node bin/waltz-overlay.mjs
or echo "  ↳ waltz-overlay skipped (no events.json) — compose without piano bug"

echo "▸ 8/8 compose"
fish bin/compose.fish; or exit 1

echo "▸ 8.5/8 timeline (post-analysis PNG → desktop)"
set -l TIMELINE_OUT "$HOME/Desktop/recap-timing_"(date +%Y-%m-%d_%H%M%S)".png"
if test -x ../pop/.venv/bin/python
  ../pop/.venv/bin/python bin/timeline.py \
    --subs out/subs.json \
    --events out/waltz-events.json \
    --audio out/waltz.mp3 \
    --out "$TIMELINE_OUT"
  or echo "  ↳ timeline step skipped or failed"
else
  echo "  ↳ no pop/.venv — skipping timeline (install librosa/matplotlib to enable)"
end

echo "━━━ done · $ROOT/out/recap.mp4 ━━━"
ls -lh $ROOT/out/recap.mp4
