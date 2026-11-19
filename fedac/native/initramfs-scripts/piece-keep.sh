#!/bin/sh
# piece-keep.sh <name.mjs> — keep a piece that was pushed to this machine.
#
# The LAN server writes a pushed piece into /pieces, which is memory: it is
# gone at the next restart. This script does the two things that make the
# piece its author's:
#   1. copies it to /mnt/pieces on the drive (init puts it back at boot), and
#   2. publishes it to the owner's account as @handle/<name>, flagged
#      platform "ac-native" so the web knows it is an AC OS piece.
# Step 2 needs the `token` in /mnt/config.json — the device token written at
# flash time, or the sign-in token `link` stores; with no token the piece is
# still kept on the drive.
#
# Called in the background by lanserv after each PUT /pieces/<name>.
# Result of the last run per piece: /tmp/piece-keep/<name>.log

NAME="$1"
SRC="/pieces/$NAME"
KEEP="/mnt/pieces"
CONFIG="/mnt/config.json"
SITE="https://aesthetic.computer"
CA="/etc/pki/tls/certs/ca-bundle.crt"

case "$NAME" in
    */*|.*) exit 0 ;;
    *.mjs) EXT="mjs" ;;
    *.lisp) EXT="lisp" ;;
    *) exit 0 ;;
esac
SLUG="${NAME%.*}"
[ -f "$SRC" ] || exit 0

mkdir -p /tmp/piece-keep
LOG="/tmp/piece-keep/$NAME.log"
say() { echo "$(date -u +%H:%M:%S) $*" >> "$LOG"; }
: > "$LOG"

# 1. Keep on the drive. Write beside the target and rename, so a power cut
#    never leaves half a piece where a whole one was.
if [ -d /mnt/EFI ] || [ -f "$CONFIG" ]; then
    mkdir -p "$KEEP"
    if cp "$SRC" "$KEEP/$NAME.tmp" && mv "$KEEP/$NAME.tmp" "$KEEP/$NAME"; then
        say "kept $KEEP/$NAME"
    else
        rm -f "$KEEP/$NAME.tmp"
        say "keep FAILED"
    fi
else
    say "no drive mounted at /mnt — not kept"
fi

# 2. Publish. An agent pushes on every edit, so wait for the pushes to stop:
#    the newest run owns the stamp, and an older run that wakes to find a
#    newer stamp leaves the work to it.
STAMP="/tmp/piece-keep/$NAME.stamp"
echo "$$" > "$STAMP"
sleep 20
[ "$(cat "$STAMP" 2>/dev/null)" = "$$" ] || exit 0

TOKEN=$(jq -r '.token // empty' "$CONFIG" 2>/dev/null)
HANDLE=$(jq -r '.handle // empty' "$CONFIG" 2>/dev/null)
if [ -z "$TOKEN" ] || [ -z "$HANDLE" ]; then
    say "not published: no account token on this machine"
    exit 0
fi

CURL="curl -sS --max-time 30"
[ -f "$CA" ] && CURL="$CURL --cacert $CA"

# Publish the bytes that are on disk now, and name them by their hash.
BODY="/tmp/piece-keep/$NAME.body"
cp "$SRC" "$BODY" || exit 0
REV=$(sha256sum "$BODY" | cut -d' ' -f1)
if [ "$(cat "/tmp/piece-keep/$NAME.published" 2>/dev/null)" = "$REV" ]; then
    say "already published $REV"
    rm -f "$BODY"
    exit 0
fi

# One call: the site checks this machine's token, stores the source at
# <owner>/piece/<slug>.<ext> and registers it flagged platform "ac-native".
REQ="/tmp/piece-keep/$NAME.json"
jq -n --arg handle "$HANDLE" --arg token "$TOKEN" --arg slug "$SLUG" \
    --arg ext "$EXT" --rawfile source "$BODY" \
    '{handle:$handle, token:$token, slug:$slug, ext:$ext, source:$source}' > "$REQ"
rm -f "$BODY"
OUT="/tmp/piece-keep/$NAME.reply"
CODE=$($CURL -o "$OUT" -w '%{http_code}' -X POST \
    -H "Content-Type: application/json" --data-binary "@$REQ" \
    "$SITE/api/native-piece" 2>>"$LOG")
rm -f "$REQ"
if [ "$CODE" != "200" ]; then
    say "not published: HTTP $CODE $(head -c 200 "$OUT" 2>/dev/null)"
    exit 1
fi
echo "$REV" > "/tmp/piece-keep/$NAME.published"
say "published @$HANDLE/$SLUG $REV $(head -c 200 "$OUT" 2>/dev/null)"
