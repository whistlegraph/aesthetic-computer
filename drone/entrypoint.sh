#!/bin/bash
set -e

echo "🖥️ Starting Xvfb..."
Xvfb :99 -screen 0 320x240x24 &

echo "🔇 Starting PulseAudio..."
pulseaudio --start --exit-idle-time=-1 &

echo "🌐 Launching Chromium..."
CHROME_BIN=$(command -v chromium-browser || command -v chromium)
"$CHROME_BIN" \
  --no-sandbox \
  --window-size=320,240 \
  --start-maximized \
  --disable-notifications \
  --disable-infobars \
  --kiosk \
  --noerrdialogs \
  --disable-session-crashed-bubble \
  --autoplay-policy=no-user-gesture-required \
  https://aesthetic.computer/\$zip &

echo "🙈 Hiding mouse..."
unclutter -idle 0.01 -root &

echo "🌍 Starting nginx..."
nginx

echo "🎥 Starting ffmpeg capture..."
ffmpeg -f x11grab -draw_mouse 0 -video_size 320x240 -i :99 \
  -f pulse -i default \
  -c:v libx264 -preset ultrafast -tune zerolatency -b:v 600k \
  -c:a aac -b:a 96k \
  -f hls -hls_time 4 -hls_list_size 5 -hls_flags delete_segments \
  /var/www/html/hls/stream.m3u8
