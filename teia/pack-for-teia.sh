#!/bin/bash

# Pack aesthetic.computer pieces for Teia Interactive OBJKTs  
# Usage: ./pack-for-teia.sh <piece-name> [options]

# Ensure we're in the right directory (go up one level from teia/ to project root)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

if [ -z "$1" ]; then
    echo "🎨 Pack aesthetic.computer pieces for Teia Interactive OBJKTs"
    echo ""
    echo "Usage: ./pack-for-teia.sh <piece-name> [options]"
    echo ""
    echo "Examples:"
    echo "  ./pack-for-teia.sh paint"
    echo "  ./pack-for-teia.sh starfield --title \"My Starfield\""  
    echo "  ./pack-for-teia.sh brush --author \"Artist Name\""
    echo ""
    echo "Available pieces:"
    ls system/public/aesthetic.computer/disks/*.{mjs,lisp} 2>/dev/null | grep -o "[^/]*\.\(mjs\|lisp\)$" | sed "s/\.\(mjs\|lisp\)$//" | sort | uniq | head -20
    echo "  (and more...)"
    exit 1
fi

echo "📦 Packing $1 for Teia..."

# Run the packer
node teia/ac-pack.mjs "$@"

if [ $? -eq 0 ]; then
    echo ""
    echo "🎉 Package assets generated successfully!"
    echo "📁 Directory: teia/output/$1/"
    echo ""
    echo "🚀 Starting local server for testing..."
    echo "🌐 Opening at: http://localhost:8001"
    echo ""
    
    # Kill any existing servers on port 8001
    pkill -f "python3 -m http.server 8001" 2>/dev/null || true
    pkill -f "caddy file-server.*:8001" 2>/dev/null || true
    
    # Change to the output directory and start server
    cd "teia/output/$1"
    
    # Start Caddy server in background
    caddy file-server --listen :8001 --root . > /dev/null 2>&1 &
    SERVER_PID=$!
    
    echo "🔍 Testing your piece..."
    echo "   • Check if the piece loads correctly"
    echo "   • Verify animations and interactions work"
    echo "   • Test in different browser window sizes"
    echo ""
    echo "📋 Teia simulation:"
    echo "   • Viewer parameter: ?viewer=tz1abc..."
    echo "   • Creator parameter: ?creator=tz1def..."
    echo ""
    
    # Wait for user input
    echo "✅ Does everything look good? (y/N)"
    read -r response
    
    # Kill the server
    kill $SERVER_PID 2>/dev/null || true
    
    # Go back to original directory
    cd - > /dev/null
    
    # Note: Zip creation is now handled by ac-pack.mjs - no need to duplicate here
else
    echo ""
    echo "❌ Packing failed. Check the error messages above."
fi