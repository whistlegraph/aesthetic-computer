#!/usr/bin/env bash
# The oskiewar reel clockwork, as cron should call it.
#
#   oskiewar-clockwork publish 0     # bake slot 0 on the oven and post it
#   oskiewar-clockwork insights      # pull Meta's figures onto the ledger
#
# This exists because the crontab used to call `bash -lc '... node ...'` directly
# and every part of that was wrong in a way that failed silently:
#
#   1. node was not on the PATH. fnm is wired into fish, not into a bash login
#      shell, so `node` simply did not exist and three publishes a day died
#      before they started. Worse, `which node` in an interactive shell reports
#      an fnm *multishell* path under a PID directory, which is per-session and
#      would rot the moment it was pasted into a crontab. So node is resolved
#      here, from the installations themselves.
#   2. Nothing updated the automation checkout, so it ran whatever it happened
#      to be sitting at. It is fast-forwarded to origin/main first.
#   3. Publishing writes the ledger next to reel.mjs (publish.mjs resolves it
#      from import.meta.url), so a checkout without a committed ledger records
#      what it published into a file nobody reads. The update in step 2 brings
#      the real one in.
#   4. A publish mutates that ledger and nothing pushed it, so lith — which
#      serves the reel figures to the wall from the file on disk — never learned
#      about a new reel. It is committed and pushed here.
#   5. None of the above announced itself. Every run now leaves a timestamped
#      log line and a heartbeat with the exit code in it.
set -uo pipefail

MODE="${1:-publish}"
INDEX="${2:-0}"
CHECKOUT="${OSKIEWAR_CLOCKWORK_CHECKOUT:-$HOME/.local/share/ac-automation}"
IG_ENV="${OSKIEWAR_IG_ENV:-$HOME/aesthetic-computer-vault/oskiewar/instagram.env}"
STATE="${OSKIEWAR_CLOCKWORK_STATE:-$HOME/.local/state}"
LOG="$STATE/oskiewar-reels.log"
BEAT="$STATE/oskiewar-clockwork.json"
LEDGER="xbox/live/marketing/ledger.json"

mkdir -p "$STATE"
STARTED="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
say() { printf '%s %s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$*" | tee -a "$LOG"; }

# The heartbeat is written on every exit path, including the failures, because a
# run that dies without saying so is the whole reason this script exists.
STAGE="starting"
finish() {
  local code=$?
  printf '{"format":"ac.oskiewar.clockwork","mode":"%s","index":%s,"startedAt":"%s","finishedAt":"%s","exitCode":%d,"stage":"%s","ok":%s}\n' \
    "$MODE" "$INDEX" "$STARTED" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$code" "$STAGE" \
    "$([ $code -eq 0 ] && echo true || echo false)" > "$BEAT"
  if [ $code -ne 0 ]; then
    say "✗ FAILED at stage '$STAGE' (exit $code) · see $LOG"
    # Best effort only: cron has no GUI session on every machine, and a missing
    # notifier must not turn a reel failure into a script failure.
    osascript -e "display notification \"oskiewar $MODE $INDEX failed at $STAGE\" with title \"reel clockwork\"" 2>/dev/null || true
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
# A detached automation checkout holds nothing worth keeping; a checkout sitting
# on a branch with real edits might, so that case stops rather than resetting.
if [ -n "$(git -C "$CHECKOUT" branch --show-current 2>/dev/null)" ]; then
  say "checkout is on a branch, not detached — refusing to reset it"; exit 1
fi
git -C "$CHECKOUT" fetch origin main --quiet || { say "fetch failed"; exit 1; }
git -C "$CHECKOUT" reset --hard origin/main --quiet || { say "reset failed"; exit 1; }
say "▸ $MODE $INDEX · node $("$NODE" --version) · $(git -C "$CHECKOUT" rev-parse --short HEAD)"

[ -d "$CHECKOUT/node_modules" ] || say "warning: $CHECKOUT/node_modules is missing; deps may fail"

# --- credentials ---
STAGE="loading credentials"
[ -f "$IG_ENV" ] || { say "no IG env at $IG_ENV"; exit 1; }
set -a
# shellcheck disable=SC1090
. "$IG_ENV"
WALL_ENV="${OSKIEWAR_WALL_ENV:-$HOME/aesthetic-computer-vault/oskiewar/wall.env}"
# shellcheck disable=SC1090
[ -f "$WALL_ENV" ] && . "$WALL_ENV"
# Hosts without the vault carry the oven key as a plain file instead.
if [ -z "${OS_BUILD_ADMIN_KEY:-}" ] && [ -f "${OSKIEWAR_OVEN_KEY_FILE:-}" ]; then
  OS_BUILD_ADMIN_KEY="$(cat "$OSKIEWAR_OVEN_KEY_FILE")"
fi
set +a

cd "$CHECKOUT" || exit 1
BEFORE="$(git -C "$CHECKOUT" hash-object "$LEDGER" 2>/dev/null || echo none)"

case "$MODE" in
  publish)
    STAGE="baking and publishing slot $INDEX"
    "$NODE" toolchain/instagram/oskiewar-oven.mjs --index "$INDEX" --auto 2>&1 | tee -a "$LOG"
    status=${PIPESTATUS[0]}
    [ "$status" -eq 0 ] || exit "$status"
    ;;
  insights)
    STAGE="pulling reel insights"
    "$NODE" xbox/live/marketing/reel.mjs --insights 2>&1 | tee -a "$LOG"
    status=${PIPESTATUS[0]}
    [ "$status" -eq 0 ] || exit "$status"
    ;;
  *)
    say "unknown mode '$MODE' (want: publish | insights)"; exit 2 ;;
esac

# --- send the figures where the wall can see them now ---
# The ledger only reaches lith with a deploy, so git alone would leave the wall
# quoting the last deploy's numbers indefinitely. Posting them costs one request
# and makes the nightly refresh actually visible.
STAGE="posting insights to lith"
if [ -n "${OSKIEWAR_WALL_KEY:-}" ] && [ -f "$LEDGER" ]; then
  posted=$("$NODE" -e '
    const { readFileSync } = await import("node:fs");
    const ledger = JSON.parse(readFileSync(process.argv[1], "utf8"));
    const posts = (ledger.posts || []).filter((post) => post.insights);
    if (!posts.length) { console.log("none"); process.exit(0); }
    const host = process.env.OSKIEWAR_STATS_HOST || "https://oskiewar.com";
    const response = await fetch(
      `${host}/api/oskiewar-stats?key=${encodeURIComponent(process.env.OSKIEWAR_WALL_KEY)}`,
      { method: "POST", headers: { "content-type": "application/json" },
        body: JSON.stringify({ posts }) });
    const body = await response.json().catch(() => ({}));
    if (!response.ok) { console.error(body.error || `HTTP ${response.status}`); process.exit(1); }
    console.log(`stored ${body.stored}`);
  ' "$LEDGER" 2>&1) && say "insights → lith: $posted" \
    || say "warning: could not post insights to lith ($posted)"
else
  say "no wall key — skipping the post to lith"
fi

# --- close the loop in git too: the ledger is the record a human reads ---
# Not rebase: one conflict during a slow slot once stranded a live menuband
# reel off its record (repost trap). The ledger is an append-only registry,
# so the durable publish is: save this run's copy, reset to fresh origin,
# merge the new posts back in by id, push.
STAGE="publishing the ledger"
AFTER="$(git -C "$CHECKOUT" hash-object "$LEDGER" 2>/dev/null || echo none)"
if [ "$BEFORE" = "$AFTER" ]; then
  say "ledger unchanged"
else
  cp "$CHECKOUT/$LEDGER" "$STATE/oskiewar-pending-ledger.json"
  pushed=0
  for attempt in 1 2 3; do
    git -C "$CHECKOUT" fetch origin main --quiet
    git -C "$CHECKOUT" reset --hard origin/main --quiet
    "$NODE" -e '
      // This run is the account ledger single writer, so its copy wins —
      // insights passes REWRITE existing posts, not just append. Posts that
      // exist only on origin (another writer) are still preserved.
      const { readFileSync, writeFileSync } = await import("node:fs");
      const read = (p) => JSON.parse(readFileSync(p, "utf8"));
      const origin = read(process.argv[1]), pending = read(process.argv[2]);
      const key = (post) => `${post.mediaId ?? ""}·${post.id ?? ""}·${post.mode ?? ""}`;
      const mine = new Set((pending.posts || []).map(key));
      for (const post of origin.posts || [])
        if (!mine.has(key(post))) pending.posts.push(post);
      writeFileSync(process.argv[1], JSON.stringify(pending, null, 2) + "\n");
    ' "$CHECKOUT/$LEDGER" "$STATE/oskiewar-pending-ledger.json" \
      || { say "ledger merge failed"; break; }
    git -C "$CHECKOUT" add "$LEDGER"
    if ! git -C "$CHECKOUT" -c user.name="oskiewar clockwork" \
      -c user.email="clockwork@aesthetic.computer" \
      commit -q -m "Record oskiewar reel $MODE $INDEX" -- "$LEDGER" 2>/dev/null; then
      pushed=1; say "ledger already on origin"; break
    fi
    if git -C "$CHECKOUT" push origin HEAD:main --quiet 2>/dev/null; then
      pushed=1; say "ledger pushed ($(git -C "$CHECKOUT" rev-parse --short HEAD))"; break
    fi
    say "push race on attempt $attempt, retrying"
  done
  [ "$pushed" -eq 1 ] || { say "could not push the ledger"; exit 1; }
fi

STAGE="done"
say "✓ $MODE $INDEX complete"
