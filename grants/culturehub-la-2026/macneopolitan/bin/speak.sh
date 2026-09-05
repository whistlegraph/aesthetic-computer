#!/usr/bin/env bash
# speak.sh — record a band member's autobiography in its own voice, on its
# own hardware. The voiceover layer of the aesthetivox identity: each member
# has a cast macOS voice (see members/<name>/voice.json) and always renders
# its own text through its own speech apparatus — neo speaks on neo,
# blueberry speaks on blueberry (over ssh, rendered there, streamed back).
# Output: members/<name>/autobiography.m4a
set -euo pipefail
NAME="${1:?usage: speak.sh <neo|blueberry|third>}"
DIR="$(cd "$(dirname "$0")/.." && pwd)"
MD="$DIR/members/$NAME/autobiography.md"
OUT="$DIR/members/$NAME/autobiography.m4a"

case "$NAME" in
  neo)       VOICE=Fred;  HOST=local;;
  blueberry) VOICE=Kathy; HOST=blueberry;;
  third)     VOICE=Junior; HOST=local;;  # borrowed hardware until it is born
  *) echo "unknown member: $NAME" >&2; exit 1;;
esac

TEXT=$(sed -e '/^# /d' -e 's/`//g' "$MD")
TMPD=$(mktemp -d); trap 'rm -rf "$TMPD"' EXIT
AIFF="$TMPD/$NAME.aiff"

if [ "$HOST" = local ]; then
  printf '%s' "$TEXT" | say -v "$VOICE" -r 165 -o "$AIFF" -f -
else
  printf '%s' "$TEXT" | ssh "$HOST" \
    "say -v $VOICE -r 165 -o /tmp/speak-$NAME.aiff -f - && cat /tmp/speak-$NAME.aiff && rm /tmp/speak-$NAME.aiff" \
    > "$AIFF"
fi

afconvert -f m4af -d aac "$AIFF" "$OUT"
afinfo "$OUT" | grep "estimated duration"
echo "$OUT"
