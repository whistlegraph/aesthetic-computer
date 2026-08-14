#!/bin/sh
# deploy-preview.sh — push the oskiewar preview shell to a standing host.
#
# The preview is a static shell plus one tiny node server; the game runs in
# whoever's browser opens it. So deployment is an rsync of the handful of
# files shell.mjs actually serves — no git, no build, same recipe as the slab
# minis. Re-run after editing oskiewar.js to update the standing preview.
#
#   sh xbox/live/marketing/deploy-preview.sh            # → jasellite
#   sh xbox/live/marketing/deploy-preview.sh otherhost
set -eu

HOST="${1:-jasellite}"
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
DEST="oskiewar-preview"

cd "$ROOT"
# -R keeps paths repo-relative, because shell.mjs finds the public files by
# walking up from its own location — the tree's shape is the contract.
# (Plain -azR: the macOS system rsync is 2.6.9 and knows none of the newer
# delete flags; a stale extra file on the host is harmless anyway.)
rsync -azR \
  xbox/live/oskiewar.js \
  xbox/live/oskiewar-sfx.mjs \
  xbox/live/oskiewar-midi.mjs \
  xbox/live/frame-driver.mjs \
  xbox/live/round-room.mjs \
  xbox/live/mac-test.html \
  xbox/live/marketing/shell.mjs \
  xbox/live/marketing/preview-server.mjs \
  "system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs" \
  system/public/aesthetic.computer/lib/product-analytics.mjs \
  system/public/aesthetic.computer/lib/oskiewar-analytics.mjs \
  system/public/aesthetic.computer/cursors/precise.svg \
  system/public/aesthetic.computer/cursors/active.svg \
  system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf \
  "$HOST:$DEST/"

ssh "$HOST" 'systemctl --user restart oskiewar-preview 2>/dev/null \
  && echo "restarted oskiewar-preview" \
  || echo "service not installed yet — see deploy-preview.sh comments"'
echo "→ http://$HOST:7899/?social-preview&replay-oven&reel-hud&self-play"

# First-time service install, run ONCE on the host:
#   mkdir -p ~/.config/systemd/user && cat > ~/.config/systemd/user/oskiewar-preview.service <<'UNIT'
#   [Unit]
#   Description=oskiewar reel preview shell
#   [Service]
#   Environment=PORT=7899
#   ExecStart=/usr/bin/env node %h/oskiewar-preview/xbox/live/marketing/preview-server.mjs
#   Restart=on-failure
#   [Install]
#   WantedBy=default.target
#   UNIT
#   systemctl --user daemon-reload && systemctl --user enable --now oskiewar-preview
