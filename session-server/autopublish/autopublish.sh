#!/usr/bin/env bash
# autopublish.sh — the session server watching for its own changes.
#
# lith redeploys itself the moment a push lands, because a GitHub webhook calls
# it. This box has no such door: it sits behind DigitalOcean with nothing
# listening but the game socket, so somebody had to remember to run
# `npm run session:publish` by hand. They did not always remember — a Mongo
# credential rotation once sat undeployed long enough that chat died days later
# — and "it is deployed" was a thing you could only find out by asking.
#
# So the box watches instead of being told. Every minute: fetch, and if `main`
# has moved AND the move touched this server's own code, run the same deploy
# the hand path runs, health gate and auto-rollback included.
#
# The delay is up to a minute of polling on top of up to a minute of lith's
# knot→GitHub mirror, because this box pulls from the GitHub side. Two minutes
# from `compush` to a live relay, worst case, with no key shared anywhere and
# no inbound port opened.
set -uo pipefail

REMOTE=/home/aesthetic-computer
NODE_BIN=/root/.local/share/fnm/aliases/default/bin
BRANCH=main
BOOT_BUDGET=150

cd "$REMOTE" || { echo "$(date -Iseconds) ✗ no checkout at $REMOTE" >&2; exit 1; }

git fetch origin --quiet "$BRANCH" || {
  echo "$(date -Iseconds) ✗ fetch failed" >&2; exit 1; }

here=$(git rev-parse HEAD)
there=$(git rev-parse "origin/$BRANCH")
[ "$here" = "$there" ] && exit 0

# Only this server's own code. The repo is a monorepo and most of what lands in
# it — pieces, the site, the game — has nothing to do with the process running
# here, and restarting for those would drop every live chat and match for
# nothing. `shared/` counts because session.mjs imports it.
if ! git diff --name-only "$here" "$there" | grep -qE '^(session-server|shared)/'; then
  exit 0
fi

# A hand deploy and a timer must never both be mid-restart. `flock -n` means the
# loser gives up rather than queueing: whatever the winner lands will be at or
# past this tip anyway, and the next tick re-checks.
exec flock -n /run/session-autopublish.lock \
  env NODE_BIN="$NODE_BIN" REMOTE="$REMOTE" REF="origin/$BRANCH" \
      EXPECT="$there" BOOT_BUDGET="$BOOT_BUDGET" \
  bash "$REMOTE/session-server/deploy-remote.sh"
