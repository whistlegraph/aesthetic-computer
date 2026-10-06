#!/usr/bin/env bash
# pop/notespatial/bin/now.sh — one turn of the climbalift loop: print, master,
# and an mp3 at one path that never moves, so a Slab card (or any player)
# pinned to it hears every change. ~35 s with a warm cache.
#   bash pop/notespatial/bin/now.sh              # the whole record
#   bash pop/notespatial/bin/now.sh lift2        # just lift2, a bar either side
#   bash pop/notespatial/bin/now.sh climb3 lift3 # from climb3 through lift3
# Writes out/climbalift-now.{flac,mp3} and out/climbalift-score.json.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"; O="$(cd "$HERE/../out" && pwd)"
FROM="${1:-}"; TO="${2:-$FROM}"
node "$HERE/climbalift.mjs" --events "$O/climbalift-score.json" | grep '^intro'
ARC=0 PAD=3 PAD_FADE=1.5 bash "$HERE/master-v2.sh" "$O/climbalift-print.wav" "$O/climbalift-now.flac" -11 -2.0 | grep '^stage'
TRIM=()
if [ -n "$FROM" ]; then
  read -r T0 T1 < <(node -e '
    const { sec } = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
    const [a, b] = [sec[process.argv[2]], sec[process.argv[3]]];
    if (!a || !b) { console.error("sections: " + Object.keys(sec).join(" ")); process.exit(1); }
    console.log(Math.max(0, a.t - a.barLen).toFixed(3), (b.end + b.barLen).toFixed(3));
  ' "$O/climbalift-score.json" "$FROM" "$TO")
  TRIM=(-ss "$T0" -to "$T1" -af "afade=t=in:d=0.05,areverse,afade=t=in:d=0.3,areverse")
  echo "excerpt $FROM..$TO · ${T0}s → ${T1}s"
fi
# write beside, then rename: a watcher never reads a half-written mp3
ffmpeg -y -v error -i "$O/climbalift-now.flac" "${TRIM[@]}" -c:a libmp3lame -b:a 256k "$O/.climbalift-now.tmp.mp3"
mv "$O/.climbalift-now.tmp.mp3" "$O/climbalift-now.mp3"
ffmpeg -hide_banner -nostats -i "$O/climbalift-now.flac" -af ebur128=peak=true -f null - 2>&1 | awk '/^ +I:/{i=$2} /^ +LRA:/{l=$2} /^ +Peak:/{p=$2} END{print i" LUFS · "p" dBTP · LRA "l}'
echo "$O/climbalift-now.mp3"
