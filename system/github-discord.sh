#!/bin/bash

# Define the Discord webhook URL
WEBHOOK_URL=$DISCORD_WEBHOOK_URL

COMMIT_MESSAGE=$(git log -1 --pretty=%B)
DISCORD_MESSAGE="system updated to \`$COMMIT_REF\` - $COMMIT_MESSAGE - $(netlify open)"

echo $DISCORD_MESSAGE
echo $DISCORD_WEBHOOK_URL

# Send the POST request to the Discord webhook URL
curl -H "Content-Type: application/json" -X POST -d "{\"content\":\"$DISCORD_MESSAGE\", \"disable_web_page_preview\": true}" $WEBHOOK_URL
