#!/bin/sh
set -eu
app_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cache_check_dir=$(mktemp -d "${TMPDIR:-/tmp}/whistlegraph-cache.XXXXXX")
trap 'rm -rf "$cache_check_dir"' EXIT HUP INT TERM
ffmpeg -v error -f lavfi -i color=c=pink:s=320x240:r=30 -f lavfi -i sine=frequency=440:sample_rate=44100 -t 0.25 -c:v libx264 -pix_fmt yuv420p -c:a aac -movflags +faststart "$cache_check_dir/card.mp4"
swiftc -parse-as-library "$app_dir/Sources/StoryCache.swift" "$app_dir/Sources/StoryCardStyle.swift" "$app_dir/Sources/StoryExport.swift" "$app_dir/Tests/StoryCacheCheck.swift" -o "$cache_check_dir/check" 2>"$cache_check_dir/compile.log" || { cat "$cache_check_dir/compile.log"; exit 1; }
"$cache_check_dir/check" "$cache_check_dir/card.mp4"
