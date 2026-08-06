#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_dir="$(cd "$script_dir/.." && pwd)"
state_dir="${PIECEFARM_STATE_DIR:-$HOME/.local/share/piecefarm}"
port="${PIECEFARM_PORT:-8788}"

mkdir -p "$state_dir" "$state_dir/qa"

children=()
stop() {
  for pid in "${children[@]:-}"; do kill "$pid" 2>/dev/null || true; done
  wait 2>/dev/null || true
}
trap stop EXIT INT TERM

cd "$project_dir"
node src/soup-server.mjs \
  --port "$port" \
  --cycle-ms "${PIECEFARM_CYCLE_MS:-250}" \
  --workers "${PIECEFARM_WORKERS:-1}" \
  --root "$state_dir/state" &
children+=("$!")

# The verified archive grows over the season; allow one bounded minute for
# replay and SQLite rehydration before treating startup as failed.
for _ in $(seq 1 600); do
  if curl --fail --silent "http://127.0.0.1:$port/api/state" >/dev/null; then break; fi
  sleep 0.1
done
curl --fail --silent "http://127.0.0.1:$port/api/state" >/dev/null

# PaperWM treats each monitor as a scrolling workspace and competes with the
# fixed two-panel kiosk. SDL owns both detected display bounds directly.
if command -v gnome-extensions >/dev/null; then
  gnome-extensions disable paperwm@paperwm.github.com >/dev/null 2>&1 || true
fi

make -s -C native
SDL_VIDEODRIVER="${PIECEFARM_SDL_VIDEODRIVER:-x11}" \
  native/piecefarm-sdl --port "$port" --snapshot-dir "$state_dir/qa" &
children+=("$!")

# Keep the terrarium on the tower's physical Line Out path. Select the ALC1220
# port and then pin this one application stream back if WirePlumber moves it.
route_piecefarm_audio() {
  command -v pactl >/dev/null || return 0
  local sink="${PIECEFARM_AUDIO_SINK:-alsa_output.pci-0000_09_00.4.analog-stereo}"
  local port_name="${PIECEFARM_AUDIO_PORT:-analog-output-lineout}"
  local stream_volume="${PIECEFARM_AUDIO_VOLUME:-42%}"
  pactl set-sink-port "$sink" "$port_name" >/dev/null 2>&1 || true
  for _ in $(seq 1 50); do
    local input
    input="$(pactl list sink-inputs 2>/dev/null | awk '
      /^Sink Input #/ { id = substr($3, 2) }
      /application.name = "piecefarm-sdl"/ { print id; exit }
    ')"
    if [[ -n "$input" ]]; then
      pactl move-sink-input "$input" "$sink" >/dev/null
      pactl set-sink-input-volume "$input" "$stream_volume" >/dev/null
      echo "Piecefarm audio: $input -> $sink / $port_name @ $stream_volume"
      return 0
    fi
    sleep 0.1
  done
  echo "Piecefarm audio: stream did not appear" >&2
}
route_piecefarm_audio &

wait -n "${children[@]}"
