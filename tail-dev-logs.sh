#!/bin/bash
# Dev log tailing script for remote debugging

LOG_FILE="dev-logs.txt"

echo "🔍 Watching for iPhone debug logs..."
echo "📱 Touch/draw on your iPhone to see logs appear here"
echo "----------------------------------------"

# Create log file if it doesn't exist
touch "$LOG_FILE"

# Tail the log file, following new additions
tail -f "$LOG_FILE"
