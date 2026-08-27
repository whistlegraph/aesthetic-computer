#!/usr/bin/env bash
# phones.sh — rebuild phone/*.wav from the vault freesound cache.
#
# The manifest (phone/manifest.json, tracked) names each sample and the
# freesound id it came from; the cached previews live in the vault at
# ~/aesthetic-computer-vault/personal/pop/freesound-cache as <id>-<slug>.mp3.
# If one is missing, re-pull it with pop/bin/freesound-fetch.mjs (the ids
# are all CC0 — search the title from the manifest). This script only
# decodes: 48k mono WAV, which is what the render's loader expects.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LANE="$(dirname "$HERE")"
CACHE="$HOME/aesthetic-computer-vault/personal/pop/freesound-cache"

python3 - "$LANE/phone/manifest.json" <<'PY' | while read -r name id; do
import json, sys
m = json.load(open(sys.argv[1]))
for name, e in m["samples"].items():
    print(name, e["id"])
PY
  src=$(ls "$CACHE/$id-"*.mp3 2>/dev/null | head -1)
  if [ -z "$src" ]; then
    echo "! $name: freesound $id not in vault cache — re-fetch it (see manifest title)" >&2
    continue
  fi
  out="$LANE/phone/$name.wav"
  if [ ! -f "$out" ] || [ "$src" -nt "$out" ]; then
    ffmpeg -y -v error -i "$src" -ac 1 -ar 48000 "$out"
    echo "✓ $name ← $(basename "$src")"
  fi
done
