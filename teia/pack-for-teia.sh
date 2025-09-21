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
    echo "🎉 Package completed successfully!"
    echo "   • Zip file created with timestamp"
    echo "   • Build artifacts cleaned up"
    echo "   • Ready for Teia upload"
else
    echo ""
    echo "❌ Packing failed. Check the error messages above."
fi