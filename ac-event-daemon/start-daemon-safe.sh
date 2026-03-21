#!/bin/bash

# Safe daemon starter that handles root permissions properly

echo "🔍 Checking user permissions..."

if [ "$EUID" -eq 0 ]; then
    echo "⚠️  Running as root detected"
    
    # Try to find the actual user who should run this
    REAL_USER=$(who | awk 'NR==1{print $1}')
    if [ -n "$REAL_USER" ] && [ "$REAL_USER" != "root" ]; then
        echo "👤 Found user: $REAL_USER"
        echo "🔄 Switching to user $REAL_USER..."
        
        # Run as the real user
        exec sudo -u "$REAL_USER" "$0" "$@"
    else
        echo "⚠️  No non-root user found, running with limited overlay features"
        echo "🔧 GUI overlays will be disabled due to root permissions"
    fi
fi

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
