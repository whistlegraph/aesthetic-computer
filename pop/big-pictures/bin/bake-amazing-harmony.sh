#!/usr/bin/env bash
# Two-pass review, isolated from the original render. Requires smooth source stems.
set -euo pipefail
HERE="$(cd -- "$(dirname -- "$0")" && pwd)"
REPO="$(cd -- "$HERE/../../.." && pwd)"
SOURCE="${SOURCE:-$REPO/pop/big-pictures/out/amazing-grace-smooth-2026-09-23}"
export OUT="${OUT:-$REPO/pop/big-pictures/out/amazing-grace-harmonized-2026-09-23}"
"$REPO/pop/.venv/bin/python" "$HERE/harmonize-amazing.py" "$SOURCE" "$OUT"
bash "$HERE/cut-amazing.sh"
ffmpeg -y -v error -i "$OUT/amazing-grace-release.flac" -c:a aac -b:a 256k "$OUT/aac-check.m4a"
node "$REPO/pop/bin/master-audit.mjs" "$OUT/amazing-grace-release.flac" "$OUT/aac-check.m4a" > "$OUT/master-audit.tsv"
cat "$OUT/master-audit.tsv"

"$REPO/pop/.venv/bin/python" "$HERE/audit-amazing-harmony.py" "$OUT"
