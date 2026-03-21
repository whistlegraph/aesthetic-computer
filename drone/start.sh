
#!/bin/bash
set -e
cd "$(dirname "$0")"


docker network prune -f >/dev/null 2>&1 || true

echo "🛑 Stopping existing container if running..."
docker rm -f drone-streamer 2>/dev/null || true

echo "🔨 Rebuilding image..."
docker build -t drone-streamer .

echo "🚀 Starting container..."
docker run \
  --name drone-streamer \
  --rm \
  -p 8081:80 \
  --shm-size=2g \
  drone-streamer

echo -e "\n🌐 Stream URL: http://localhost:8081/hls/stream.m3u8\n"
