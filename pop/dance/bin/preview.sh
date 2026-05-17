#!/usr/bin/env bash
# preview.sh — open one or more rendered cover videos in a clean,
# chrome-free mpv window: borderless (no title bar, so the window
# hugs the exact video aspect — ZERO black bars), autoplay, loop
# forever, no on-screen controls/OSD. Each file gets its own window,
# tiled with a small offset so multiple aspects show side by side.
#
# Usage:
#   pop/dance/bin/preview.sh file1.mp4 [file2.mp4 ...]
#   pop/dance/bin/preview.sh            # defaults to latest tw + twi builds
#
# Press q (or Esc) in a window to close just that one.

set -euo pipefail

MPV="$(command -v mpv || echo /opt/homebrew/bin/mpv)"
if [[ ! -x "$MPV" ]]; then
  echo "mpv not found — install with: brew install mpv" >&2
  exit 1
fi
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOVER_LUA="$SCRIPT_DIR/hover-loop.lua"

# No args → grab the newest build of each known track.
if [[ $# -eq 0 ]]; then
  TW="$HOME/Desktop/builds/trancenwaltz"
  TWI="$HOME/Desktop/builds/trancenwaltzi"
  args=()
  if [[ -d "$TW" ]]; then
    b="$(ls -1d "$TW"/b??? 2>/dev/null | sort | tail -1)"
    [[ -n "${b:-}" ]] && for v in "$b"/trancenwaltz-cover.mp4 "$b"/trancenwaltz-cover-vertical.mp4; do
      [[ -f "$v" ]] && args+=("$v")
    done
  fi
  if [[ -d "$TWI" ]]; then
    b="$(ls -1d "$TWI"/b??? 2>/dev/null | sort | tail -1)"
    [[ -n "${b:-}" && -f "$b/trancenwaltzi-cover.mp4" ]] && args+=("$b/trancenwaltzi-cover.mp4")
  fi
  set -- "${args[@]}"
fi

if [[ $# -eq 0 ]]; then
  echo "no videos to preview (pass paths, or build first)" >&2
  exit 1
fi

ox=40; oy=40
for f in "$@"; do
  if [[ ! -f "$f" ]]; then echo "skip (missing): $f" >&2; continue; fi
  "$MPV" \
    --loop-file=inf \
    --no-border \
    --no-osc \
    --osd-level=0 \
    --no-osd-bar \
    --pause=yes \
    --script="$HOVER_LUA" \
    --autofit-larger=46%x46% \
    --geometry="+${ox}+${oy}" \
    --title="${f##*/}" \
    --quiet \
    "$f" >/dev/null 2>&1 &
  ox=$((ox + 60)); oy=$((oy + 60))
done

echo "opened $# clean looping window(s) in mpv (borderless · autoplay · loop · no controls)"
# detached — windows live independently; script returns immediately
disown -a 2>/dev/null || true
