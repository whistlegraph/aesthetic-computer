#!/bin/bash

WEBHOOK_URL=$DISCORD_WEBHOOK_URL # Define the Discord webhook URL
COMMIT_URL="$REPOSITORY_URL/commit/$COMMIT_REF"

# Get the last commit message and escape backticks
COMMIT_MESSAGE=$(git log -1 --pretty=%B | sed 's/`/\\`/g')

# Get the short hash
SHORT_HASH="${COMMIT_REF:0:7}"

# Create the Discord message
DISCORD_MESSAGE="$COMMIT_MESSAGE️ ($SHORT_HASH)️"
FLAGS="2"  # Define the message flags (SUPPRESS_EMBEDS)

# Use Python to generate a properly escaped JSON payload
JSON_PAYLOAD=$(python -c "
import json
import sys
message = sys.argv[1]
print(json.dumps({'content': message, 'flags': $FLAGS}))
" "$DISCORD_MESSAGE")

# Send the POST request to the Discord webhook URL
curl -H "Content-Type: application/json" -X POST -d "$JSON_PAYLOAD" $WEBHOOK_URL
