#!/bin/sh
# score-push.sh <host> <file.npscore|file.mbscore> — put a score on a
# device and perform it. .mbscore translates on the way (tools/
# mbscore-to-npscore.mjs); the pointer file names the score because
# lanserv's /jump can't carry colon params.
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

NAME="$(basename "${FILE%.npscore}")"
BASE="http://$HOST"

curl -fsS --max-time 10 -X PUT --data-binary "@$FILE" "$BASE/pieces/$NAME.npscore" > /dev/null
printf '%s' "$NAME" | curl -fsS --max-time 10 -X PUT --data-binary @- "$BASE/pieces/npscore-current.txt" > /dev/null
curl -fsS --max-time 10 -X POST "$BASE/jump/npscore" > /dev/null
echo "→ $NAME.npscore on $HOST, performing"
