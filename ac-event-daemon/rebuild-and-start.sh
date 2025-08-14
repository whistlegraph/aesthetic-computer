#!/bin/bash

# Quick script to rebuild the daemon with latest changes
# Run this on the HOST machine, not in the container

echo "🔄 Rebuilding ac-event-daemon with latest changes..."

cd "$(dirname "$0")"

# Stop any running daemon
echo "🛑 Stopping existing daemon..."
pkill -f ac-event-daemon

# Rebuild
echo "🔨 Building..."
cargo build --release

if [ $? -eq 0 ]; then
    echo "✅ Build successful!"
    echo ""
    echo "🚀 Starting daemon with improved overlays..."
    echo "💡 Press Ctrl+C to stop"
    echo ""
    
    # Start the new daemon
    ./target/release/ac-event-daemon
else
    echo "❌ Build failed!"
    exit 1
fi
