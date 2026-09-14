#!/bin/sh
# Max-level compressed sidecars for everything Caddy serves out of xbox/live,
# so oskiewar.com's file_server can answer `precompressed br zstd` instead of
# encoding 680 KB of game on the fly at default level on every request:
# brotli -q 11 and zstd -19 are each about a fifth smaller than that.
#
# Runs on lith right after every checkout — deploys AND rollbacks — because
# Caddy serves a sidecar that exists without asking whether it is older than
# its source. A sidecar is only rebuilt when its source is newer, so an
# unchanged module keeps its mtime, its ETag, and the open title screens
# that would otherwise reload for no reason.
set -e
cd "$(dirname "$0")/../live"
for f in *.js *.mjs *.html; do
  [ -f "$f" ] || continue
  if command -v brotli >/dev/null 2>&1; then
    if [ ! -f "$f.br" ] || [ "$f" -nt "$f.br" ]; then brotli -f -q 11 -o "$f.br" "$f"; fi
  else rm -f "$f.br"; fi
  if command -v zstd >/dev/null 2>&1; then
    if [ ! -f "$f.zst" ] || [ "$f" -nt "$f.zst" ]; then zstd -q -f -19 -o "$f.zst" "$f"; fi
  else rm -f "$f.zst"; fi
done
# A sidecar must not outlive the file it was made from.
for side in *.br *.zst; do
  [ -f "$side" ] && [ ! -f "${side%.*}" ] && rm -f "$side"
done
exit 0
