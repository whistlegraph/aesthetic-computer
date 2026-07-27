#!/usr/bin/env bash
# Provision one reachable fleet Mac with the guard + typed compute worker.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
HOST="${1:-}"
NAME="${2:-$HOST}"
ROLE="${3:-balanced}"
REMOTE_REPO="${4:-}"
TOKEN="$HOME/.config/ac-fleet-worker/token"

[[ "$HOST" =~ ^[A-Za-z0-9._-]+$ ]] || { echo "usage: deploy-worker.sh HOST [NAME] [ROLE] [REMOTE_REPO]" >&2; exit 2; }
[[ "$NAME" =~ ^[A-Za-z0-9._-]+$ ]] || { echo "invalid worker name" >&2; exit 2; }
case "$ROLE" in heavy|light|interactive|balanced) ;; *) echo "invalid role: $ROLE" >&2; exit 2 ;; esac
[[ -s "$TOKEN" ]] || { echo "shared worker token is missing: $TOKEN" >&2; exit 1; }

if ! ssh -o BatchMode=yes -o ConnectTimeout=5 "$HOST" 'printf ready' 2>/dev/null | grep -q ready; then
  echo "$HOST is offline; deployment deferred" >&2
  exit 75
fi

REMOTE_HOME="$(ssh -o BatchMode=yes "$HOST" 'printf %s "$HOME"')"
[[ "$REMOTE_HOME" == /Users/* ]] || { echo "unexpected remote home: $REMOTE_HOME" >&2; exit 1; }
[[ -n "$REMOTE_REPO" ]] || REMOTE_REPO="$REMOTE_HOME/aesthetic-computer"
[[ "$REMOTE_REPO" == "$REMOTE_HOME"/* ]] || { echo "remote repo must stay under remote home" >&2; exit 1; }
REMOTE_TMP="$(ssh -o BatchMode=yes "$HOST" 'mktemp -d /tmp/ac-fleet-worker.XXXXXX')"
[[ "$REMOTE_TMP" == /tmp/ac-fleet-worker.* ]] || { echo "unexpected remote temp path" >&2; exit 1; }

cleanup() {
  ssh -o BatchMode=yes -o ConnectTimeout=5 "$HOST" \
    "rm -f '$REMOTE_TMP/worker.mjs' '$REMOTE_TMP/install-worker.sh' '$REMOTE_TMP/performance-guard.sh' '$REMOTE_TMP/token'; rmdir '$REMOTE_TMP' 2>/dev/null || true" \
    >/dev/null 2>&1 || true
}
trap cleanup EXIT HUP INT TERM

scp -q "$HERE/worker.mjs" "$HERE/install-worker.sh" \
  "$REPO/toolchain/macos/performance-guard.sh" "$TOKEN" "$HOST:$REMOTE_TMP/"
ssh -o BatchMode=yes "$HOST" \
  "mkdir -p '$REMOTE_HOME/.local/lib/ac-fleet-worker'; \
   install -m 755 '$REMOTE_TMP/performance-guard.sh' '$REMOTE_HOME/.local/lib/ac-fleet-worker/performance-guard.sh'; \
   env AC_REPO='$REMOTE_REPO' bash '$REMOTE_HOME/.local/lib/ac-fleet-worker/performance-guard.sh' --install; \
   bash '$REMOTE_TMP/install-worker.sh' --name '$NAME' --role '$ROLE' --token-file '$REMOTE_TMP/token'"

echo "deployed fleet worker to $NAME ($HOST, $ROLE)"
