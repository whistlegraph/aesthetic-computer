#!/bin/bash

# Start ac-event-daemon
# This script builds and runs the notification daemon

echo "🚀 Starting ac-event-daemon..."

cd "$(dirname "$0")"

# Build the daemon
echo "🔨 Building daemon..."
cargo build --release

if [ $? -eq 0 ]; then
    echo "✅ Build successful"
    echo "🎵 Starting ac-event-daemon..."
    echo "💡 Press Ctrl+C to stop, or daemon will auto-stop when terminal closes"
    echo ""
    
    # Run the daemon
    ./target/release/ac-event-daemon
else
    echo "❌ Build failed"
    exit 1
fi
