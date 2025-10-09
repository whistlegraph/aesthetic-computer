#!/bin/bash

# Pack aesthetic.computer pieces for Teia Interactive OBJKTs  
# Usage: ./pack-for-objkt.fish <piece-name> [options]

if [ -z "$1" ]; then
    echo "🎨 Pack aesthetic.computer pieces for Teia Interactive OBJKTs"
    echo ""
    echo "Usage: ./pack-for-objkt.fish <piece-name> [options]"
    echo ""
    echo "Examples:"
    echo "  ./pack-for-objkt.fish paint"
    echo "  ./pack-for-objkt.fish starfield --title \"My Starfield\""  
    echo "  ./pack-for-objkt.fish brush --author \"Artist Name\""
    echo ""
    echo "Available pieces:"
    ls system/public/aesthetic.computer/disks/*.{mjs,lisp} 2>/dev/null | grep -o "[^/]*\.\(mjs\|lisp\)$" | sed "s/\.\(mjs\|lisp\)$//" | sort | uniq | head -20
    echo "  (and more...)"
    exit 1
fi

echo "📦 Packing $1 for Teia..."

# Run the packer
node utilities/ac-pack.mjs "$@"

if [ $? -eq 0 ]; then
    echo ""
    echo "🎉 Success! Your package is ready:"
    echo "📁 Directory: tokens/$1/"
    echo "📦 Zip file: tokens/$1.zip"
    echo ""
    echo "🚀 Next steps:"
    echo "1. Go to https://teia.art/mint"
    echo "2. Upload tokens/$1.zip"  
    echo "3. Preview and test your interactive OBJKT"
    echo "4. Mint when ready!"
else
    echo ""
    echo "❌ Packing failed. Check the error messages above."
fi
