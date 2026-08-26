#!/usr/bin/env bash
# The menuband reel clockwork, as cron should call it.
#
#   menuband-clockwork publish       # render + post the lane's next waltz
#   menuband-clockwork insights      # pull Meta's figures onto the ledger
#
# A sibling of oskiewar-clockwork.sh with the same hard-won shape — node
# resolved from installations (not a shell's PATH), the automation checkout
# fast-forwarded before use, the ledger pushed after, and a heartbeat on
# every exit path. Differences: the bake is a local render (the waltz
# renderer runs on this machine, nice'd, not on the oven), it needs the
# menu-bar rig frames copied in (they are captured artifacts, not in git),
# and there is no wall to post figures to.
set -uo pipefail

MODE="${1:-publish}"
CHECKOUT="${MENUBAND_CLOCKWORK_CHECKOUT:-$HOME/.local/share/ac-automation}"
IG_ENV="${MENUBAND_IG_ENV:-$HOME/aesthetic-computer-vault/menuband/instagram.env}"
STATE="${MENUBAND_CLOCKWORK_STATE:-$HOME/.local/state}"
LOG="$STATE/menuband-reels.log"
BEAT="$STATE/menuband-clockwork.json"
LEDGER="social/instagram/menuband-ledger.json"
# The generator appends new waltzes here; the commit carries them home too.
LANE="pop/menuband/waltzes/reel-lane.json"
# The renderer's rig: captured menu-bar frames that live outside git. The
# main working tree keeps the canonical copy (synced from blueberry).
RIG_SRC="$HOME/aesthetic-computer/pop/menuband/out"

mkdir -p "$STATE"
STARTED="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
say() { printf '%s %s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$*" | tee -a "$LOG"; }

# An hourly cadence with a quarter-hour render means a slow slot can still be
# running when the next one fires. The lock makes overlap a quiet skip — the
# lane self-throttles instead of stacking two renders on an 8 GB machine.
LOCK="$STATE/menuband-clockwork.lock"
if [ "$MODE" = "publish" ]; then
  if mkdir "$LOCK" 2>/dev/null; then
    echo $$ > "$LOCK/pid"
  else
    HOLDER="$(cat "$LOCK/pid" 2>/dev/null || echo unknown)"
    if [ "$HOLDER" != "unknown" ] && kill -0 "$HOLDER" 2>/dev/null; then
      say "previous publish (pid $HOLDER) still running — skipping this slot"
      exit 0
    fi
    say "stale lock (pid $HOLDER gone) — taking over"
    echo $$ > "$LOCK/pid"
  fi
fi

STAGE="starting"
finish() {
  local code=$?
  [ "$MODE" = "publish" ] && rm -rf "$LOCK"
  printf '{"format":"ac.menuband.clockwork","mode":"%s","startedAt":"%s","finishedAt":"%s","exitCode":%d,"stage":"%s","ok":%s}\n' \
    "$MODE" "$STARTED" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$code" "$STAGE" \
    "$([ $code -eq 0 ] && echo true || echo false)" > "$BEAT"
  if [ $code -ne 0 ]; then
    say "✗ FAILED at stage '$STAGE' (exit $code) · see $LOG"
    osascript -e "display notification \"menuband $MODE failed at $STAGE\" with title \"reel clockwork\"" 2>/dev/null || true
  fi
  exit $code
}
trap finish EXIT

# --- node, resolved from installations rather than from a shell's PATH ---
STAGE="resolving node"
NODE=""
for candidate in \
  $(ls -d "$HOME"/.local/share/fnm/node-versions/*/installation/bin/node 2>/dev/null | sort -V -r) \
  /opt/homebrew/bin/node /usr/local/bin/node /usr/bin/node; do
  if [ -x "$candidate" ]; then NODE="$candidate"; break; fi
done
[ -n "$NODE" ] || { say "no usable node found"; exit 1; }

# --- checkout, brought up to what was actually shipped ---
STAGE="updating checkout"
[ -d "$CHECKOUT/.git" ] || [ -f "$CHECKOUT/.git" ] || { say "no checkout at $CHECKOUT"; exit 1; }
if [ -n "$(git -C "$CHECKOUT" branch --show-current 2>/dev/null)" ]; then
  say "checkout is on a branch, not detached — refusing to reset it"; exit 1
fi
git -C "$CHECKOUT" fetch origin main --quiet || { say "fetch failed"; exit 1; }
git -C "$CHECKOUT" reset --hard origin/main --quiet || { say "reset failed"; exit 1; }
say "▸ $MODE · node $("$NODE" --version) · $(git -C "$CHECKOUT" rev-parse --short HEAD)"

[ -d "$CHECKOUT/node_modules" ] || say "warning: $CHECKOUT/node_modules is missing; deps may fail"

# --- rig frames: untracked capture artifacts the video renderer requires ---
STAGE="staging rig frames"
if [ -d "$RIG_SRC" ]; then
  mkdir -p "$CHECKOUT/pop/menuband/out"
  for rig in "$RIG_SRC"/menubar-frames-*; do
    [ -d "$rig" ] || continue
    rsync -a "$rig/" "$CHECKOUT/pop/menuband/out/$(basename "$rig")/"
  done
fi

# --- credentials ---
STAGE="loading credentials"
[ -f "$IG_ENV" ] || { say "no IG env at $IG_ENV"; exit 1; }
set -a
# shellcheck disable=SC1090
. "$IG_ENV"
set +a

cd "$CHECKOUT" || exit 1
BEFORE="$(git -C "$CHECKOUT" hash-object "$LEDGER" "$LANE" 2>/dev/null || echo none)"

case "$MODE" in
  publish)
    STAGE="rendering and publishing the next waltz"
    "$NODE" toolchain/instagram/menuband-reel.mjs next --auto 2>&1 | tee -a "$LOG"
    status=${PIPESTATUS[0]}
    [ "$status" -eq 0 ] || exit "$status"
    ;;
  insights)
    STAGE="pulling reel insights"
    "$NODE" toolchain/instagram/menuband-reel.mjs insights 2>&1 | tee -a "$LOG"
    status=${PIPESTATUS[0]}
    [ "$status" -eq 0 ] || exit "$status"
    ;;
  *)
    say "unknown mode '$MODE' (want: publish | insights)"; exit 2 ;;
esac

# --- close the loop in git: the ledger is the record a human reads ---
STAGE="publishing the ledger"
AFTER="$(git -C "$CHECKOUT" hash-object "$LEDGER" "$LANE" 2>/dev/null || echo none)"
if [ "$BEFORE" = "$AFTER" ]; then
  say "ledger unchanged"
else
  git -C "$CHECKOUT" add "$LEDGER" "$LANE"
  git -C "$CHECKOUT" -c user.name="menuband clockwork" \
    -c user.email="clockwork@aesthetic.computer" \
    commit -q -m "Record menuband reel $MODE" -- "$LEDGER" "$LANE" || {
      say "nothing to commit"; exit 0; }
  pushed=0
  for attempt in 1 2 3; do
    git -C "$CHECKOUT" fetch origin main --quiet
    git -C "$CHECKOUT" rebase origin/main --quiet >/dev/null 2>&1 || {
      git -C "$CHECKOUT" rebase --abort >/dev/null 2>&1 || true
      say "rebase onto origin/main failed"; break; }
    if git -C "$CHECKOUT" push origin HEAD:main --quiet 2>/dev/null; then
      pushed=1; say "ledger pushed ($(git -C "$CHECKOUT" rev-parse --short HEAD))"; break
    fi
    say "push race on attempt $attempt, retrying"
  done
  [ "$pushed" -eq 1 ] || { say "could not push the ledger"; exit 1; }
fi

STAGE="done"
say "✓ $MODE complete"
