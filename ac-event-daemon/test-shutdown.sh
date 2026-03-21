#!/bin/bash

# Test script to verify daemon shutdown behavior

echo "🧪 Testing daemon shutdown behavior..."

# Start daemon in background and capture its PID
echo "🚀 Starting daemon..."
cd "$(dirname "$0")"
cargo run &
DAEMON_PID=$!

echo "📊 Daemon PID: $DAEMON_PID"

# Wait a moment for daemon to start
sleep 2

# Send a test notification
echo "📡 Sending test notification..."
../ac-notify success "test"

# Wait a moment
sleep 1

# Check if daemon is running
if ps -p $DAEMON_PID > /dev/null; then
    echo "✅ Daemon is running (PID $DAEMON_PID)"
else
    echo "❌ Daemon is not running"
    exit 1
fi

# Test signal handling
echo "📤 Sending SIGTERM to daemon..."
kill -TERM $DAEMON_PID

# Wait a moment for graceful shutdown
sleep 2

# Check if daemon stopped
if ps -p $DAEMON_PID > /dev/null; then
    echo "❌ Daemon still running after SIGTERM"
    kill -9 $DAEMON_PID  # Force kill
    exit 1
else
    echo "✅ Daemon stopped gracefully after SIGTERM"
fi

echo "🎉 All tests passed!"
