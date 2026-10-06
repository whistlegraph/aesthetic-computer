#!/usr/bin/env bash
# icp-review-playlists.sh — runs ON the render host (where the cuts live):
# one UNLISTED playlist per client, every voice cut uploaded UNLISTED into it,
# then prints the playlist and video URLs. Refuses if any receipt is not unlisted.
#
#   icp-review-playlists.sh <channel-name> <plan-file>
# plan-file lines:   playlist|<Client> — <Product> (voice A/B)
#                    video|<title>|<absolute path to .mp4>
# A blank line or '#' is ignored. Videos attach to the most recent playlist line.
set -euo pipefail
CH=${1:?channel}; PLAN=${2:?plan file}
export PATH=/opt/homebrew/bin:$PATH
export YT_CLIENT_JSON=${YT_CLIENT_JSON:-$HOME/.config/yt/client.json}
export YT_TOKEN_JSON=${YT_TOKEN_JSON:-$HOME/.config/yt/$CH-token.json}
YT="node $HOME/.local/share/yt/toolchain/youtube/yt.mjs"
OUT=${OUT:-$HOME/.local/share/yt/icp-review-urls.md}; : > "$OUT"
PL=""
while IFS= read -r line; do
  [[ -z "$line" || "$line" == \#* ]] && continue
  kind=${line%%|*}; rest=${line#*|}
  if [[ $kind == playlist ]]; then
    PL=$($YT playlist-ensure --as "$CH" --title "Fuser ICP review · $rest" --privacy unlisted --json | node -e 'let s="";process.stdin.on("data",d=>s+=d).on("end",()=>{const j=JSON.parse(s.trim().split("\n").pop());if(j.privacy!=="unlisted"){console.error("PLAYLIST NOT UNLISTED");process.exit(2)}console.log(j.id)})')
    printf "\n## %s\nplaylist: https://www.youtube.com/playlist?list=%s\n" "$rest" "$PL" >> "$OUT"
  elif [[ $kind == video ]]; then
    title=${rest%%|*}; file=${rest#*|}
    [[ -f $file ]] || { echo "skip (missing): $file" >&2; continue; }
    sidecar="${file%.*}.youtube.json"          # yt.mjs writes <name>.youtube.json beside the video
    if [[ -f $sidecar ]]; then echo "already uploaded (receipt exists): $(basename "$file")" >&2
    else
      $YT upload "$file" --as "$CH" --title "$title" --privacy unlisted --category 28 --playlist "$PL" \
        --description "Fuser ICP review cut (unlisted). Voice A/B. Not for distribution." >/dev/null
    fi
    url=$(node -e 'const r=require(process.argv[1]);if(r.privacy!=="unlisted"){console.error("NOT UNLISTED",r.videoId);process.exit(2)}console.log(r.watchUrl||("https://youtu.be/"+r.videoId))' "$sidecar")
    echo "- $title — $url" >> "$OUT"
  fi
done < "$PLAN"
cat "$OUT"
