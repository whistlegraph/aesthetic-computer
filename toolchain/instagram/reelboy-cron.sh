#!/bin/bash
# reelboy-cron.sh — the crontab's road into reelboy.mjs.
#
# This exists for the same reason oskiewar-clockwork.sh does: cron's bash
# has no fnm hook, so `node` does not exist there, and an fnm multishell
# path pasted into a crontab rots the moment that shell closes. Node is
# resolved from installations, newest first, then the system fallbacks.
# Install: cp into ~/.local/bin (re-copy when this file changes), then:
#   */15 * * * * $HOME/.local/bin/reelboy-cron
set -u
REPO="${REELBOY_REPO:-$HOME/aesthetic-computer}"
LOG_DIR="$HOME/.local/state/reelboy"
mkdir -p "$LOG_DIR"

NODE=""
for candidate in \
  $(ls -d "$HOME"/.local/share/fnm/node-versions/*/installation/bin/node 2>/dev/null | sort -V -r) \
  /opt/homebrew/bin/node /usr/local/bin/node /usr/bin/node; do
  if [ -x "$candidate" ]; then NODE="$candidate"; break; fi
done
if [ -z "$NODE" ]; then
  echo "$(date -u +%FT%TZ) no usable node found" >> "$LOG_DIR/reelboy.log"
  exit 1
fi

cd "$REPO" || exit 1
exec "$NODE" toolchain/instagram/reelboy.mjs >> "$LOG_DIR/reelboy.log" 2>&1
