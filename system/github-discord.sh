#!/bin/bash

WEBHOOK_URL=$DISCORD_WEBHOOK_URL # Define the Discord webhook URL
COMMIT_MESSAGE=$(git log -1 --pretty=%B)
COMMIT_URL="$REPOSITORY_URL/commit/$COMMIT_REF"
SHORT_HASH="${COMMIT_REF:0:7}"
DISCORD_MESSAGE="$COMMIT_MESSAGE️ ([$SHORT_HASH](<$COMMIT_URL>))️"
FLAGS="\"2\"" # Define the message flags (SUPPRESS_EMBEDS) - https://discord.com/developers/docs/resources/channel#message-object-message-flags

# Send the POST request to the Discord webhook URL
curl -H "Content-Type: application/json" -X POST -d "{\"content\":\"$DISCORD_MESSAGE\",\"flags\":$FLAGS}" $WEBHOOK_URL
