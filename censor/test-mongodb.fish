#!/usr/bin/env fish

# Test chat messages from MongoDB through the censor filter
set limit $argv[1]

if test -z "$limit"
    set limit 10
end

# Load MongoDB credentials from vault
set -x MONGODB_CONNECTION_STRING $MONGODB_CONNECTION_STRING
set -x MONGODB_NAME "aesthetic"

echo "Testing $limit messages from MongoDB chat-system collection..."
echo "Make sure the Caddy server is running (./start-server.fish)"
echo ""

node /workspaces/aesthetic-computer/censor/test-mongodb-messages.mjs $limit
