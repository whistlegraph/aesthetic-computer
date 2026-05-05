#!/usr/bin/env bash
# vendor-pt.sh — fetch Neil Thapen's original Pink Trombone (MIT) into
# vendor/pinktrombone/.
#
# PT lives canonically at https://dood.al/pinktrombone/ as a single
# self-contained HTML file. The MIT license notice is the comment header
# at the top of that file (lines 28–46 as of v1.1, March 2017).
#
# We fetch the HTML directly with curl rather than cloning a third-party
# git mirror, because:
#   1. dood.al IS the canonical source — every github mirror is downstream
#   2. some mirrors (zakaton/Pink-Trombone, others) re-license under GPL
#      after refactoring; that's incompatible with the AC-native goal of
#      shipping a permissive C/WASM jeffrey voice
#   3. a single HTML file pinned by sha256 is the simplest possible
#      provenance contract
#
# Usage:
#   bin/vendor-pt.sh                  # fetch from dood.al
#   bin/vendor-pt.sh <url>            # override source
#   bin/vendor-pt.sh --dry-run        # print what it would do, no fetch

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"
VENDOR_DIR="$ROOT/vendor/pinktrombone"

DEFAULT_URL="https://dood.al/pinktrombone/"

URL="${1:-$DEFAULT_URL}"
DRY=0
[ "${1:-}" = "--dry-run" ] && DRY=1 && URL="$DEFAULT_URL"

echo "→ vendoring pink trombone (MIT, Neil Thapen 2017)"
echo "  url:    $URL"
echo "  target: $VENDOR_DIR"

if [ -f "$VENDOR_DIR/index.html" ] && [ -f "$VENDOR_DIR/UPSTREAM.txt" ]; then
  echo "✗ $VENDOR_DIR already populated. delete it first if you want a fresh fetch."
  exit 1
fi

if [ "$DRY" = 1 ]; then
  echo "  (dry run — would curl now)"
  exit 0
fi

mkdir -p "$VENDOR_DIR"

TMP="$(mktemp)"
trap 'rm -f "$TMP"' EXIT
curl -sSLf "$URL" -o "$TMP"

# Sanity-check: confirm the MIT notice we expect is in the file. Refuse
# to vendor an unverified blob.
if ! grep -q "Copyright 2017 Neil Thapen" "$TMP"; then
  echo "✗ fetched file missing 'Copyright 2017 Neil Thapen' header — wrong upstream?"
  exit 1
fi
if ! grep -q "Permission is hereby granted, free of charge" "$TMP"; then
  echo "✗ fetched file missing MIT preamble — license check failed."
  exit 1
fi

cp "$TMP" "$VENDOR_DIR/index.html"

# Extract the MIT block into a stand-alone LICENSE file so anyone reading
# the vendor tree sees the license without parsing HTML comments.
awk '
  /Copyright 2017 Neil Thapen/      { writing=1 }
  writing                            { print }
  /IN THE SOFTWARE\./                { if (writing) { writing=0; exit } }
' "$TMP" > "$VENDOR_DIR/LICENSE"

SHA="$(shasum -a 256 "$VENDOR_DIR/index.html" | awk '{print $1}')"
SIZE="$(wc -c < "$VENDOR_DIR/index.html" | tr -d ' ')"

cat > "$VENDOR_DIR/UPSTREAM.txt" <<EOF
upstream:     $URL
upstream_url: ${URL}index.html
fetched_at:   $(date -u +"%Y-%m-%dT%H:%M:%SZ")
sha256:       $SHA
size_bytes:   $SIZE
version:      "version 1.1, March 2017"  (per index.html line 8)
author:       Neil Thapen <venuspatrol.nfshost.com>
license:      MIT (Copyright 2017 Neil Thapen — see LICENSE in this directory)
notice:       pop/voice/NOTICE.md tracks third-party material for this lane
EOF

echo "✓ vendored pink trombone"
echo "  upstream:  $URL"
echo "  sha256:    $SHA"
echo "  size:      $SIZE bytes"
echo "  files:     index.html, LICENSE, UPSTREAM.txt"
echo
echo "next: read script/PinkTrombone.js logic from index.html lines 50+,"
echo "      then write bin/render-pt.mjs (node-side renderer)."
