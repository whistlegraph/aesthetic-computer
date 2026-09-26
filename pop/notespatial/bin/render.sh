#!/usr/bin/env bash
# pop/notespatial/bin/render.sh — the whole record from the repo.
#   bash pop/notespatial/bin/render.sh            # → pop/notespatial/out/notespatial-MASTER.flac
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
OUT="$ROOT/pop/notespatial/out"; mkdir -p "$OUT"
SCORE="$ROOT/fedac/native/scores/notespatial-native.nsscore"
PRINT="$OUT/notespatial-binaural-sub-f32.wav"
node "$ROOT/fedac/native/tools/notespatial-native-render.mjs" "$SCORE" --audio-only --sub --float --out "$PRINT"
bash "$ROOT/pop/notespatial/bin/master.sh" "$PRINT" 3 "$OUT/notespatial-MASTER.flac"
ffmpeg -y -v error -i "$OUT/notespatial-MASTER.flac" -c:a copy \
  -metadata title="notespatial" -metadata artist="Aesthetic Dot Computer" \
  -metadata album="notespatial" -metadata album_artist="Aesthetic Dot Computer" -metadata date=2026 \
  "$OUT/notespatial-MASTER.tagged.flac" && mv "$OUT/notespatial-MASTER.tagged.flac" "$OUT/notespatial-MASTER.flac"
node "$ROOT/pop/bin/master-audit.mjs" "$OUT/notespatial-MASTER.flac" | tee "$OUT/master-audit.tsv"
