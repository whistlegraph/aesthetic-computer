#!/bin/bash

# Cleanup script for hanging Node.js recording processes
# Run this if you notice orphaned tape.mjs processes after dashboard exits

echo "🧹 Cleaning up hanging Node.js recording processes..."

# Kill any tape.mjs processes
TAPE_PIDS=$(ps aux | grep "node.*tape.mjs" | grep -v grep | awk '{print $2}')
if [ ! -z "$TAPE_PIDS" ]; then
    echo "Found tape.mjs processes: $TAPE_PIDS"
    echo "$TAPE_PIDS" | xargs kill -9
    echo "✅ Killed tape.mjs processes"
else
    echo "ℹ️ No tape.mjs processes found"
fi

# Kill any dashboard-main.mjs processes
DASHBOARD_PIDS=$(ps aux | grep "node.*dashboard-main.mjs" | grep -v grep | awk '{print $2}')
if [ ! -z "$DASHBOARD_PIDS" ]; then
    echo "Found dashboard processes: $DASHBOARD_PIDS"
    echo "$DASHBOARD_PIDS" | xargs kill -9
    echo "✅ Killed dashboard processes"
else
    echo "ℹ️ No dashboard processes found"
fi

# Clean up PID file if it exists
if [ -f ".recording-pid" ]; then
    OLD_PID=$(cat .recording-pid)
    echo "Found old PID file: $OLD_PID"
    kill -9 $OLD_PID 2>/dev/null && echo "✅ Killed PID $OLD_PID" || echo "ℹ️ PID $OLD_PID already dead"
    rm .recording-pid
    echo "✅ Removed PID file"
fi

# Clean up any heap profiling files that might be left behind
echo "🧹 Cleaning up profiling files..."
rm -f *.heapprofile *.heapsnapshot v8.log isolate-*.log 2>/dev/null
echo "✅ Profiling files cleaned"

echo "🎉 Cleanup complete!"