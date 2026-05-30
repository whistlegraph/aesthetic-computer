#!/usr/bin/env bash
# Capture the WaveWizard window so Claude (or any reviewer) can see
# the current UI state. Usage: bin/screenshot.sh [outpath]
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
OUT="${1:-/tmp/wave-wizard-screenshot.png}"
WID=$(swift "$HERE/winid.swift" WaveWizard 2>/dev/null || true)
if [[ -z "$WID" ]]; then
  echo "✗ no WaveWizard window found" >&2
  exit 1
fi
screencapture -l"$WID" -o -x "$OUT"
echo "→ $OUT"
ls -la "$OUT"
