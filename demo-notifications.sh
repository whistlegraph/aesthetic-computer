#!/bin/bash

# Demo script for ac-event-daemon notifications
# Shows different notification types with timing

echo "🎬 Starting ac-event-daemon notification demo..."
echo "Make sure the daemon is running on the host!"
echo ""

notifications=(
    "success:BUILD COMPLETED"
    "info:STARTING PROCESS"
    "error:CONNECTION FAILED"
    "success:TASK FINISHED"
    "info:LOADING DATA"
    "warning:DISK SPACE LOW"
)

for notification in "${notifications[@]}"; do
    type="${notification%:*}"
    message="${notification#*:}"
    
    echo "📡 Sending: $type -> $message"
    ./ac-notify "$type" "$message"
    
    # Wait between notifications to see each overlay
    sleep 4
done

echo ""
echo "🎉 Demo complete!"
