#!/usr/bin/env fish

# Continuous test of chat messages from MongoDB
# Usage: ./test-continuous.fish [batch_size]

set batch_size $argv[1]

if test -z "$batch_size"
    set batch_size 50
end

# Load MongoDB credentials
set -x MONGODB_CONNECTION_STRING $MONGODB_CONNECTION_STRING
set -x MONGODB_NAME "aesthetic"

echo "🔍 Starting continuous chat message moderation test..."
echo "📦 Batch size: $batch_size messages"
echo ""

node /workspaces/aesthetic-computer/censor/test-mongodb-messages.mjs $batch_size --continuous
