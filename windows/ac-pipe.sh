#!/bin/bash
# Fast pipe script - send lines to session server asynchronously
while IFS= read -r line; do
    # Echo immediately (no delay)
    echo "$line"
    
    # Send to server in background (fire and forget)
    (
        escaped_line=$(echo "$line" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g')
        curl -s -X POST https://session-server.aesthetic.computer/build-stream \
            --header "Content-Type: application/json" \
            --data "{\"line\": \"$escaped_line\"}" > /dev/null 2>&1
    ) &
done
# Wait for all background jobs to finish
wait
