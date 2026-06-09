#!/bin/bash
# dj-usb-collect.sh — stage every finished pop/ track into one flat folder
# of mp3s, ready for dj-usb-burn.sh (AC Native dj.mjs + CDJ-compatible).
#
# Usage: bash pop/bin/dj-usb-collect.sh [dest]   (default: ~/Desktop/dj)
#
# Released singles come from the AC assets CDN (canonical masters);
# everything else is each lane's final render in pop/*/out/.
set -e

DEST="${1:-$HOME/Desktop/dj}"
POP="$(cd "$(dirname "$0")/.." && pwd)"
CDN="https://assets.aesthetic.computer/pop"

mkdir -p "${DEST}"

# Released masters — local assets mirror first, CDN fallback
MIRROR="${POP}/../system/public/assets/pop"
for t in marimbaba helpabeach trancenwaltz trancepenta amaythingra hellsine; do
    if [ -s "${MIRROR}/${t}.mp3" ]; then
        echo "mirror ${t}"
        cp "${MIRROR}/${t}.mp3" "${DEST}/${t}.mp3"
    else
        echo "cdn    ${t}"
        curl -fsSL -o "${DEST}/${t}.mp3" "${CDN}/${t}.mp3"
    fi
done

# Lane finals (local) — "src|name on stick"
LOCAL="
gradus/gradus.mp3|gradus
gecs/out/gecs-song.mp3|gecs
americomputadora/out/americomputadora.mp3|americomputadora
hopehop/out/hopehop.mp3|hopehop
jungle/out/solafiya.mp3|solafiya
jungle/out/raggasol.mp3|raggasol
jungle/out/jungleton.mp3|jungleton
jungle/out/rodando.mp3|rodando
maytrax/out/maytrax.mp3|maytrax
hippyhayzard/out/hippyhayzard.mp3|hippyhayzard
moronboba/out/moronbobasleep.mp3|moronbobasleep
booch/out/visualize-my-booch-song.mp3|visualize-my-booch
"
echo "${LOCAL}" | while IFS='|' read -r src name; do
    [ -z "${src}" ] && continue
    if [ -f "${POP}/${src}" ]; then
        echo "local ${name}"
        cp "${POP}/${src}" "${DEST}/${name}.mp3"
    else
        echo "MISS  ${POP}/${src}" >&2
    fi
done

echo ""
ls -lh "${DEST}"/*.mp3 | awk '{print $5, $NF}'
echo ""
echo "$(ls "${DEST}"/*.mp3 | wc -l | tr -d ' ') tracks staged in ${DEST}"
