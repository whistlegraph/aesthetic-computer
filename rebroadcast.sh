#!/bin/bash
echo "🎬 Rebroadcasting updated playlist (single item: 3-kidlisp-tests)"
cd /workspaces/aesthetic-computer

echo "📡 Fetching updated playlist..."
curl -s "https://local.aesthetic.computer/api/playlist" > /tmp/current-playlist.json

echo "📊 Playlist items count:"
cat /tmp/current-playlist.json | jq '.items | length'

echo "📝 Current item:"
cat /tmp/current-playlist.json | jq -r '.items[0].source'

echo "🔧 Creating broadcast payload..."
cat /tmp/current-playlist.json | jq '{"dp1_call": ., "intent": {"action": "now_display"}}' > /tmp/broadcast-payload.json

echo "📦 Payload size: $(wc -c < /tmp/broadcast-payload.json) bytes"

echo "🚀 Broadcasting to TV cast..."
if [ -n "$FF1_CAST_API_KEY" ]; then
  curl -X POST \
    "https://tv-cast-coordination.autonomy-system.workers.dev/api/cast?topicID=2Ps0iMbsZUvrXOFavFhrFwVmRmrzJD0rZ" \
    -H "content-type: application/json" \
    -H "API-KEY: $FF1_CAST_API_KEY" \
    -d @/tmp/broadcast-payload.json
  echo ""
  echo "✅ Broadcast complete!"
else
  echo "❌ FF1_CAST_API_KEY not set"
fi
