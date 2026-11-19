#!/bin/sh
# piece-keep.sh <name.mjs> — keep a piece that was pushed to this machine.
#
# The LAN server writes a pushed piece into /pieces, which is memory: it is
# gone at the next restart. This script does the two things that make the
# piece its author's:
#   1. copies it to /mnt/pieces on the drive (init puts it back at boot), and
#   2. publishes it to the owner's account as @handle/<name>, flagged
#      origin "ac-native" so the web knows it is an AC OS piece.
# Step 2 needs the account token `link` writes into /mnt/config.json; with no
# token the piece is still kept on the drive.
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
    *.mjs) EXT="mjs"; MIME="application/javascript; charset=utf-8" ;;
    *.lisp) EXT="lisp"; MIME="text/x-lisp; charset=utf-8" ;;
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
    say "not published: no account token on this machine (run the link piece)"
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

GRANT=$($CURL -H "Authorization: Bearer $TOKEN" -H "Accept: application/json" \
    "$SITE/presigned-upload-url/$EXT/piece-$SLUG.$EXT/user" 2>>"$LOG")
URL=$(printf '%s' "$GRANT" | jq -r '.uploadURL // empty' 2>/dev/null)
if [ -z "$URL" ]; then
    say "not published: no upload grant ($(printf '%s' "$GRANT" | head -c 160))"
    rm -f "$BODY"
    exit 1
fi

CODE=$($CURL -o /dev/null -w '%{http_code}' -X PUT \
    -H "Content-Type: $MIME" -H "Content-Disposition: inline" \
    -H "x-amz-acl: public-read" --data-binary "@$BODY" "$URL" 2>>"$LOG")
rm -f "$BODY"
if [ "$CODE" != "200" ]; then
    say "not published: upload answered HTTP $CODE"
    exit 1
fi

REG=$($CURL -X POST -H "Authorization: Bearer $TOKEN" \
    -H "Content-Type: application/json" \
    -d "{\"slug\":\"$SLUG\",\"ext\":\"$EXT\",\"revision\":\"$REV\",\"origin\":\"ac-native\"}" \
    "$SITE/api/register-piece" 2>>"$LOG")
echo "$REV" > "/tmp/piece-keep/$NAME.published"
say "published @$HANDLE/$SLUG $REV $(printf '%s' "$REG" | head -c 200)"
