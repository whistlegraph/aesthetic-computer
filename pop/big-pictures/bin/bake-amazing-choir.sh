#!/usr/bin/env bash
# Review-only choir arrangement + spatial sparkle + house mastering.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
REPO="$(cd -- "$HERE/../../.." && pwd)"
SOURCE="${SOURCE:-$REPO/pop/big-pictures/out/amazing-grace-harmonized-2026-09-23}"
export OUT="${OUT:-$REPO/pop/big-pictures/out/amazing-grace-choir-2026-09-23}"
PY="$REPO/pop/.venv/bin/python"
"$PY" "$HERE/choir-amazing.py" "$SOURCE" "$OUT"
"$PY" "$HERE/sparkle-amazing.py" "$OUT/pre-choir.wav" "$OUT" "$OUT/vocal-spatial.wav"
bash "$HERE/cut-amazing.sh"
ffmpeg -y -v error -i "$OUT/amazing-grace-release.flac" -c:a aac -b:a 256k "$OUT/aac-check.m4a"
node "$REPO/pop/bin/master-audit.mjs" "$OUT/amazing-grace-release.flac" "$OUT/aac-check.m4a" > "$OUT/master-audit.tsv"
cat "$OUT/master-audit.tsv"
"$PY" "$HERE/audit-amazing-harmony.py" "$OUT"
