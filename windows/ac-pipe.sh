#!/bin/bash
# Simple pipe script to send lines to session server
while IFS= read -r line; do
    # Escape for JSON
    escaped_line=$(echo "$line" | sed 's/"/\\"/g' | sed "s/'/\\'/g")
    curl -s -X POST https://session-server.aesthetic.computer/build-stream \
        --header "Content-Type: application/json" \
        --data "{\"line\": \"$escaped_line\"}" > /dev/null 2>&1 &
    echo "$line"
done
