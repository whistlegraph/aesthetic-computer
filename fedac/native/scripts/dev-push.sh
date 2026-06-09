#!/bin/bash
# dev-push.sh — push a piece to a running AC Native device over LAN and
# hot-reload it, via the lanserv HTTP endpoint (port 80 on the device).
#
# Usage:
#   scripts/dev-push.sh <host> [piece] [--watch]
#
#   scripts/dev-push.sh notepat.local              # push notepat + reload
#   scripts/dev-push.sh 192.168.1.214 notepat --watch
#
# <host> is the device's mDNS name (slot.local / notepat.local) or IP.
# [piece] defaults to notepat. --watch re-pushes on save (0.5s mtime poll,
# no fswatch dependency). Lib deps (the /lib modules the oven bakes) are
# pushed when their mtimes change too — but QuickJS caches /lib imports
# per context, so lib changes only take effect after a device reboot;
# piece changes reload instantly (unique module name per load).
#
# Fallback for SSH-only builds: scp pieces/<p>.mjs root@<host>:/pieces/ &&
# ssh root@<host> 'echo <p> > /tmp/ac-jump'

set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NATIVE="$(dirname "$SCRIPT_DIR")"
SRC="$(cd "$NATIVE/../.." && pwd)"
WEB_LIB="$SRC/system/public/aesthetic.computer/lib"

HOST="${1:-}"
[ -z "$HOST" ] && { sed -n '2,17p' "$0" | sed 's/^# \{0,1\}//'; exit 1; }
shift

PIECE="notepat"
WATCH=0
for arg in "$@"; do
    case "$arg" in
        --watch) WATCH=1 ;;
        *) PIECE="${arg%.mjs}" ;;
    esac
done

PIECE_FILE="$NATIVE/pieces/$PIECE.mjs"
[ -f "$PIECE_FILE" ] || { echo "dev-push: no $PIECE_FILE" >&2; exit 1; }

# Same list as docker-build.sh §2p — modules pieces import as ../lib/X.mjs.
LIB_FILES=()
for libmjs in melody-parser.mjs notepat-convert.mjs note-colors.mjs num.mjs percussion.mjs; do
    [ -f "$WEB_LIB/$libmjs" ] && LIB_FILES+=("$WEB_LIB/$libmjs")
done

BASE="http://$HOST"

push() { # push <local-file> <remote-path> [jump]
    local file="$1" remote="$2" jump="${3:-}"
    local url="$BASE$remote"
    [ -n "$jump" ] && url="$url?jump=1"
    if curl -fsS --max-time 10 -X PUT --data-binary "@$file" "$url" > /dev/null; then
        echo "→ $remote ($(wc -c < "$file" | tr -d ' ') bytes)${jump:+ + reload}"
    else
        echo "dev-push: PUT $url failed (is the device on? lanserv build?)" >&2
        return 1
    fi
}

mtime() {
    stat -f %m "$1" 2>/dev/null || stat -c %Y "$1" 2>/dev/null
}

echo "dev-push → $BASE  piece=$PIECE"
curl -fsS --max-time 5 "$BASE/" 2>/dev/null || echo "(no status — pushing anyway)"

for lib in ${LIB_FILES[@]+"${LIB_FILES[@]}"}; do
    push "$lib" "/lib/$(basename "$lib")" || exit 1
done
push "$PIECE_FILE" "/pieces/$PIECE.mjs" jump || exit 1

[ "$WATCH" = 0 ] && exit 0

echo "watching for changes (ctrl-c to stop)..."
declare -A SEEN
SEEN["$PIECE_FILE"]=$(mtime "$PIECE_FILE")
for lib in ${LIB_FILES[@]+"${LIB_FILES[@]}"}; do
    SEEN["$lib"]=$(mtime "$lib")
done

while sleep 0.5; do
    changed=0
    for lib in ${LIB_FILES[@]+"${LIB_FILES[@]}"}; do
        m=$(mtime "$lib")
        if [ "$m" != "${SEEN[$lib]}" ]; then
            SEEN["$lib"]=$m
            push "$lib" "/lib/$(basename "$lib")" && changed=1
        fi
    done
    m=$(mtime "$PIECE_FILE")
    if [ "$m" != "${SEEN[$PIECE_FILE]}" ]; then
        SEEN["$PIECE_FILE"]=$m
        push "$PIECE_FILE" "/pieces/$PIECE.mjs" jump
    elif [ "$changed" = 1 ]; then
        echo "(lib pushed — QuickJS caches /lib imports; reboot device to apply)"
    fi
done
