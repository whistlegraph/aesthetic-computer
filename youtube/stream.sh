#!/bin/bash
set -e

XVFB_RESOLUTION="${XVFB_RESOLUTION:-320x240x24}"
DISPLAY="${DISPLAY:-:1}"
CHROME_URL="https://aesthetic.computer"
OUTPUT_URL="rtmp://localhost/live/test"

cleanup() {
  pkill -TERM chromium-browser || true
  pkill -TERM ffmpeg || true
  pkill -TERM Xvfb || true
  pulseaudio --kill || true
}
trap cleanup SIGINT SIGTERM

echo "Starting Xvfb..."
Xvfb "$DISPLAY" -screen 0 "$XVFB_RESOLUTION" -ac &
sleep 2

echo "Starting PulseAudio..."
pulseaudio --start --exit-idle-time=-1 --log-level=error
sleep 2

echo "Loading null sink..."
pactl load-module module-null-sink sink_name=dummy_sink
pactl set-default-sink dummy_sink
sleep 2

echo "Starting nginx RTMP server..."
/usr/local/nginx/sbin/nginx -c /usr/local/nginx/conf/nginx.conf

echo "Launching Chromium..."
chromium-browser \
  --no-sandbox \
  --window-size=320,240 "$CHROME_URL" \
  --disable-infobars \
  --no-first-run \
  --no-default-browser-check \
  --disable-dev-shm-usage \
  --remote-debugging-port=9222 &

sleep 5

echo "Starting ffmpeg stream..."
ffmpeg -y \
  -f x11grab -video_size 320x240 -i "$DISPLAY" \
  -f pulse -ac 2 -i default \
  -c:v libx264 -preset ultrafast -maxrate 800k -bufsize 400k \
  -g 60 -r 30 -pix_fmt yuv420p \
  -c:a aac -b:a 128k -ar 44100 \
  -f flv "$OUTPUT_URL"
