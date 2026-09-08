#!/bin/sh
# score-push.sh <host> <file.npscore|file.mbscore|file.nsscore> — put a
# score on a device and perform it. .mbscore translates on the way
# (tools/mbscore-to-npscore.mjs); .nsscore performs via notespatial; the
# pointer file names the score because lanserv's /jump can't carry colon
# params.
set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NATIVE="$(dirname "$SCRIPT_DIR")"

HOST="${1:-}"; FILE="${2:-}"
[ -z "$HOST" ] || [ -z "$FILE" ] && { echo "usage: score-push.sh <host> <score.npscore|.mbscore>" >&2; exit 1; }

case "$FILE" in
  *.mbscore)
    OUT="${TMPDIR:-/tmp}/$(basename "${FILE%.mbscore}").npscore"
    node "$NATIVE/tools/mbscore-to-npscore.mjs" "$FILE" "$OUT"
    FILE="$OUT" ;;
esac

case "$FILE" in
  *.nsscore) EXT="nsscore"; PIECE="notespatial" ;;
  *) EXT="npscore"; PIECE="npscore" ;;
esac
NAME="$(basename "$FILE")"; NAME="${NAME%.$EXT}"
BASE="http://$HOST"

# lanserv has been seen dropping a PUT body while answering 200 — verify
# the round-trip size and retry once before performing a truncated score.
WANT="$(wc -c < "$FILE" | tr -d ' ')"
for attempt in 1 2; do
    curl -fsS --max-time 20 -X PUT --data-binary "@$FILE" "$BASE/pieces/$NAME.$EXT" > /dev/null
    GOT="$(curl -fsS --max-time 20 "$BASE/pieces/$NAME.$EXT" | wc -c | tr -d ' ')"
    [ "$GOT" = "$WANT" ] && break
    echo "score-push: verify failed ($GOT/$WANT bytes), attempt $attempt" >&2
    [ "$attempt" = 2 ] && exit 1
done
printf '%s' "$NAME" | curl -fsS --max-time 10 -X PUT --data-binary @- "$BASE/pieces/$PIECE-current.txt" > /dev/null
curl -fsS --max-time 10 -X POST "$BASE/jump/$PIECE" > /dev/null
echo "→ $NAME.$EXT on $HOST ($WANT bytes verified), performing via $PIECE"
