#!/usr/bin/env bash
# Pop open the Whistlegraph Wizard on the loner score: draw each word's
# gesture on the trackpad (underlay = Camille's finished drawing),
# recording lands in viz/wg-perform.json. Export to the reel with
# wizard-export.py afterwards.
set -euo pipefail
cd "$(dirname "$0")"
LONER="$(cd ../.. && pwd)"
WIZ="$(cd ../../../../slab/whistlegraphwizard && pwd)"

[ -x "$WIZ/build/WhistlegraphWizard" ] || bash "$WIZ/build.sh"

exec "$WIZ/build/WhistlegraphWizard" \
  --score "$LONER/viz/wordclock.json" \
  --under "$LONER/viz/wg-final.png" \
  --out "$LONER/viz/wg-perform.json"
