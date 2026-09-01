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
PROFILE_DIR="${PROFILE_DIR:-$HOME/.tv-rig-chrome}"
AUDIO_CHECK="${AUDIO_CHECK:-1}"   # 0 for stations that are legitimately quiet
WD="${WD_DIR:-$HOME/$UNIT/watchdog}"
LOG="${WD_LOG:-$HOME/$UNIT/watchdog.log}"
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

# ── warm-up grace ────────────────────────────────────────────────────
# The timer can fire during (or right after) a station boot — Chrome takes
# ~20s to appear. Judging a station that's been up under two minutes shot
# actv down mid-startup once; never again.
enter_us=$(systemctl --user show "$UNIT" -p ActiveEnterTimestampMonotonic --value 2>/dev/null)
now_us=$(awk '{printf "%d", $1 * 1000000}' /proc/uptime)
if [ -z "$enter_us" ] || [ "$enter_us" = "0" ] ||
   [ $((now_us - enter_us)) -lt 120000000 ]; then
  log "warming up (unit age <120s) — skipping checks"
  exit 0
fi

# ── deploy watch: restart when the station's upstream code updates ───
# The broadcast should always show the latest deploy. RELOAD_URLS lists
# the files whose change means "the page you're airing is stale" — their
# combined Last-Modified/ETag is the fingerprint; a change restarts the
# station (fresh Chrome, fresh code, few seconds of blip).
if [ -n "${RELOAD_URLS:-}" ]; then
  marker=""
  for u in $RELOAD_URLS; do
    body=$(curl -s --max-time 10 "$u")
    # /api/version answers carry a per-request timestamp — fingerprint the
    # deployed sha alone; anything else fingerprints as a body hash.
    fp=$(printf %s "$body" | grep -o '"deployed":"[^"]*"' | head -1)
    [ -z "$fp" ] && [ -n "$body" ] && fp=$(printf %s "$body" | md5sum | cut -d" " -f1)
    marker="$marker|$fp"
  done
  if [ -n "$(echo "$marker" | tr -d '| ,')" ]; then
    prev=$(cat "$WD/deploy_marker" 2>/dev/null || true)
    echo "$marker" > "$WD/deploy_marker"
    if [ -n "$prev" ] && [ "$marker" != "$prev" ]; then
      restart "upstream code updated"
    fi
  fi
fi

# ── vital 1: the process tree (this station's, not any station's) ────
pgrep -f "user-data-dir=$PROFILE_DIR" >/dev/null || restart "chrome gone"
pgrep -f "Xvfb $D" >/dev/null || restart "Xvfb gone"
ps -eo args | grep -v grep | grep "rtmp://" | grep -q -- "-i $D " || restart "ffmpeg gone"

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
peak=""
if [ "$AUDIO_CHECK" = "1" ]; then
  peak=$(ffmpeg -v info -f pulse -i "${SINK}.monitor" -t 3 -af volumedetect -f null - 2>&1 |
    sed -n 's/.*max_volume: \(-\{0,1\}[0-9.]*\) dB.*/\1/p')
  if [ -z "$peak" ] || [ "$(echo "$peak" | cut -d. -f1)" -lt -70 ] 2>/dev/null; then
    strike silence_strikes
    log "silence peak=${peak:-none} (strike $(strikes silence_strikes))"
    [ "$(strikes silence_strikes)" -ge 3 ] && restart "audio dead"
  else
    clear_ silence_strikes
  fi
fi

log "ok md5=${md5:-none} peak=${peak:-?}dB"
