#!/bin/bash

# Monitor Emacs loading process
echo "=== Starting Emacs Load Monitor ==="
echo "Time: $(date)"
echo "PID: $$"

# Clear previous debug log
rm -f /tmp/emacs-debug.log

# Function to log system metrics
log_metrics() {
    echo "=== METRICS $(date) ==="
    echo "Load Average: $(uptime | awk -F'load average:' '{ print $2 }')"
    echo "Memory Usage:"
    free -h
    echo "Top CPU processes:"
    ps aux --sort=-%cpu | head -10
    echo "Emacs processes:"
    ps aux | grep -E "(emacs|aesthetic)" | grep -v grep
    echo "Open files by emacs:"
    lsof -c emacs 2>/dev/null | wc -l || echo "No emacs processes"
    echo "Network connections:"
    ss -an | grep ESTABLISHED | wc -l 2>/dev/null || echo "0"
    echo "---"
}

# Monitor in background
while true; do
    log_metrics
    sleep 2
done &

MONITOR_PID=$!

echo "Monitor started with PID: $MONITOR_PID"
echo "Debug log will be at: /tmp/emacs-debug.log"
echo "Kill monitor with: kill $MONITOR_PID"

# Keep script running
wait