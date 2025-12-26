#!/bin/bash
# Post-attach script to ensure the aesthetic task is running

echo "✅ Attached to aesthetic container"

# Check if aesthetic-launch.sh is already running
if ! pgrep -f "aesthetic-launch.sh" > /dev/null; then
    echo "Starting aesthetic platform..."
    # Note: VS Code will auto-run the task, but if it doesn't, the user can start it manually
fi
