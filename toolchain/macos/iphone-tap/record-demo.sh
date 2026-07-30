#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
tap="$here/iphone-tap"
out_dir="${1:-$here/recordings/$(date +%Y%m%d-%H%M%S)}"
mkdir -p "$out_dir"

frame="$($tap frame)"
x="$(jq -r '.x | round' <<<"$frame")"
y="$(jq -r '.y | round' <<<"$frame")"
w="$(jq -r '.w | round' <<<"$frame")"
h="$(jq -r '.h | round' <<<"$frame")"
started="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

jq -n \
  --arg started "$started" \
  --argjson frame "$frame" \
  '{startedAt:$started, window:"iPhone Mirroring", frame:$frame}' \
  > "$out_dir/session.json"

logger_pid=""
video_pid=""
cleanup() {
  trap - INT TERM EXIT
  if [[ -n "$video_pid" ]] && kill -0 "$video_pid" 2>/dev/null; then
    kill -INT "$video_pid" 2>/dev/null || true
    wait "$video_pid" 2>/dev/null || true
  fi
  if [[ -n "$logger_pid" ]] && kill -0 "$logger_pid" 2>/dev/null; then
    kill -TERM "$logger_pid" 2>/dev/null || true
    wait "$logger_pid" 2>/dev/null || true
  fi
  printf '\nrecorded\n%s\n%s\n' "$out_dir/demo.mov" "$out_dir/interactions.jsonl"
}
trap cleanup INT TERM EXIT

"$tap" record-events "$out_dir/interactions.jsonl" > "$out_dir/events.log" 2>&1 &
logger_pid=$!
screencapture -x -v -k -R"$x,$y,$w,$h" "$out_dir/demo.mov" &
video_pid=$!

sleep 1
if ! kill -0 "$logger_pid" 2>/dev/null; then
  cat "$out_dir/events.log" >&2
  exit 1
fi
if ! kill -0 "$video_pid" 2>/dev/null; then
  echo "screen recording failed to start" >&2
  exit 1
fi

printf 'RECORDING %s\n' "$out_dir"
wait "$video_pid"
