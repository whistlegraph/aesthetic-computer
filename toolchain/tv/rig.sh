#!/bin/sh
# rig.sh — broadcast a web page as an RTMP live stream, 24/7.
#
# The whole rig in one process tree: a virtual screen (Xvfb), a virtual
# speaker (PulseAudio null sink), Chrome in kiosk mode watching the page,
# and ffmpeg turning screen + speaker into RTMP. Built for jasellite;
# any Ubuntu box with google-chrome/Xvfb/pulseaudio/xdotool/ffmpeg works.
#
#   rig.sh <env-file>
#
# The env file carries the secrets and the station config:
#   RTMP_URL=rtmp://a.rtmp.youtube.com/live2      (from live.mjs ensure-stream)
#   STREAM_KEY=xxxx-xxxx-…                        (secret — chmod 600)
#   URL=https://tv.whistlegraph.org               (what the station shows)
#   SIZE=1920x1080  FPS=30  VBITRATE=4500k        (optional overrides)
#   UNMUTE_CLICK=1                                (tv.whistlegraph.org: first
#                                                  tap = sound on; exactly one)
#
# Foreground process is ffmpeg; when it dies the whole rig is torn down,
# so a systemd unit with Restart=always makes the station self-healing.

set -eu

ENV_FILE="${1:?usage: rig.sh <env-file>}"
. "$ENV_FILE"

: "${RTMP_URL:?env file must set RTMP_URL}"
: "${STREAM_KEY:?env file must set STREAM_KEY}"
URL="${URL:-https://tv.whistlegraph.org}"
SIZE="${SIZE:-1920x1080}"
FPS="${FPS:-30}"
VBITRATE="${VBITRATE:-4500k}"
DISPLAY_NUM="${DISPLAY_NUM:-:99}"
SINK="${SINK:-tvsink}"
UNMUTE_CLICK="${UNMUTE_CLICK:-1}"
PROFILE_DIR="${PROFILE_DIR:-$HOME/.tv-rig-chrome}"

W="${SIZE%x*}"
H="${SIZE#*x}"

log() { echo "[rig $(date +%H:%M:%S)] $*"; }

cleanup() {
  log "tearing down"
  [ -n "${CHROME_PID:-}" ] && kill "$CHROME_PID" 2>/dev/null || true
  [ -n "${XVFB_PID:-}" ] && kill "$XVFB_PID" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

# ── virtual speaker ──────────────────────────────────────────────────
pulseaudio --check 2>/dev/null || pulseaudio --start --exit-idle-time=-1
pactl list short sinks | awk '{print $2}' | grep -qx "$SINK" ||
  pactl load-module module-null-sink "sink_name=$SINK" >/dev/null
log "pulse sink '$SINK' ready"

# ── virtual screen ───────────────────────────────────────────────────
Xvfb "$DISPLAY_NUM" -screen 0 "${SIZE}x24" -nolisten tcp &
XVFB_PID=$!
sleep 2
log "Xvfb up on $DISPLAY_NUM (${SIZE})"

# ── the viewer ───────────────────────────────────────────────────────
# Fresh profile every boot: the TV page remembers sound-on across visits,
# and a remembered unmute would turn our one tap into a channel change.
rm -rf "$PROFILE_DIR"
# GPU-less VM: force software raster + decode. No HEVC here either way —
# tv.html detects that and serves its H.264 (-avc) twins.
PULSE_SINK="$SINK" DISPLAY="$DISPLAY_NUM" google-chrome \
  --kiosk "--window-size=${W},${H}" --window-position=0,0 \
  --no-first-run --disable-infobars --disable-session-crashed-bubble \
  --disable-features=TranslateUI --hide-crash-restore-bubble \
  --autoplay-policy=no-user-gesture-required \
  --disable-gpu --disable-accelerated-video-decode --disable-gpu-compositing \
  --user-data-dir="$PROFILE_DIR" \
  "$URL" &
CHROME_PID=$!
sleep 15
log "chrome watching $URL"

if [ "$UNMUTE_CLICK" = "1" ]; then
  DISPLAY="$DISPLAY_NUM" xdotool mousemove "$((W / 2))" "$((H / 2))" click 1
  sleep 1
  # Park the cursor in the corner so it doesn't sit on the picture.
  DISPLAY="$DISPLAY_NUM" xdotool mousemove "$((W - 1))" "$((H - 1))"
  log "unmute tap sent"
fi

# ── the transmitter ──────────────────────────────────────────────────
# Keyframe every 2s (YouTube wants ≤4s), constant-ish bitrate for ingest.
log "ffmpeg → ${RTMP_URL}/(key)"
exec ffmpeg -hide_banner -loglevel warning \
  -f x11grab -framerate "$FPS" -video_size "$SIZE" -i "$DISPLAY_NUM" \
  -f pulse -i "${SINK}.monitor" \
  -c:v libx264 -preset veryfast -tune zerolatency -pix_fmt yuv420p \
  -b:v "$VBITRATE" -maxrate "$VBITRATE" -bufsize 9000k \
  -x264-params "keyint=$((FPS * 2)):min-keyint=$((FPS * 2)):scenecut=0" \
  -c:a aac -b:a 160k -ar 44100 -ac 2 \
  -f flv "${RTMP_URL}/${STREAM_KEY}"
