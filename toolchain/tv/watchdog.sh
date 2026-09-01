#!/bin/sh
# watchdog.sh — is the station actually broadcasting *television*?
#
# ffmpeg keeps happily transmitting even when Chrome wedges or the page
# freezes, so process supervision alone can stream a corpse for days.
# Every run (a systemd timer, ~5 min) this checks three vital signs and
# restarts the wgtv unit after repeated failures:
#
#   processes — Chrome, Xvfb, and the RTMP ffmpeg all alive (miss = restart)
#   motion    — md5 of a center crop of the virtual screen; the TV never
#               shows the same center twice at 5-minute spacing
#               (2 identical checks in a row = frozen page → restart)
#   sound     — 3s peak off the null sink; the TV is never silent for long
#               (3 silent checks in a row = wedged audio → restart)
#
# State + log live in ~/wgtv/watchdog/. Strikes survive between runs as
# files; any healthy check clears its own counter.
set -u

D="${DISPLAY_NUM:-:99}"
SIZE="${SIZE:-1920x1080}"
SINK="${SINK:-tvsink}"
UNIT="${UNIT:-wgtv}"
WD="$HOME/wgtv/watchdog"
LOG="$HOME/wgtv/watchdog.log"
mkdir -p "$WD"

log() { echo "[watchdog $(date '+%Y-%m-%d %H:%M:%S')] $*" >> "$LOG"; }

restart() {
  log "RESTART ($1)"
  rm -f "$WD/freeze_strikes" "$WD/silence_strikes" "$WD/last_md5"
  systemctl --user restart "$UNIT"
  exit 0
}

strikes() { cat "$WD/$1" 2>/dev/null || echo 0; }
strike() { echo $(($(strikes "$1") + 1)) > "$WD/$1"; }
clear_() { rm -f "$WD/$1"; }

# ── vital 1: the process tree ────────────────────────────────────────
pgrep -f "tv-rig-chrome" >/dev/null || restart "chrome gone"
pgrep -x Xvfb >/dev/null || restart "Xvfb gone"
pgrep -f "rtmp://" >/dev/null || restart "ffmpeg gone"

# ── vital 2: motion in the picture ───────────────────────────────────
md5=$(ffmpeg -v error -f x11grab -video_size "$SIZE" -i "$D" -frames:v 1 \
  -vf "crop=in_w/2:in_h/2:in_w/4:in_h/4" -f rawvideo - 2>/dev/null | md5sum | cut -d" " -f1)
if [ -n "$md5" ] && [ "$md5" = "$(cat "$WD/last_md5" 2>/dev/null)" ]; then
  strike freeze_strikes
  log "frozen frame (strike $(strikes freeze_strikes))"
  [ "$(strikes freeze_strikes)" -ge 2 ] && restart "picture frozen"
else
  clear_ freeze_strikes
fi
echo "$md5" > "$WD/last_md5"

# ── vital 3: sound on the sink ───────────────────────────────────────
peak=$(ffmpeg -v info -f pulse -i "${SINK}.monitor" -t 3 -af volumedetect -f null - 2>&1 |
  sed -n 's/.*max_volume: \(-\{0,1\}[0-9.]*\) dB.*/\1/p')
if [ -z "$peak" ] || [ "$(echo "$peak" | cut -d. -f1)" -lt -70 ] 2>/dev/null; then
  strike silence_strikes
  log "silence peak=${peak:-none} (strike $(strikes silence_strikes))"
  [ "$(strikes silence_strikes)" -ge 3 ] && restart "audio dead"
else
  clear_ silence_strikes
fi

log "ok md5=${md5:-none} peak=${peak:-?}dB"
