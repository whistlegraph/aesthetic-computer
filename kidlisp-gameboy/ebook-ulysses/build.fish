#!/usr/bin/env fish

# Build script for Ulysses eBook ROM

set script_dir (dirname (status -f))

echo "📚 Ulysses eBook for Game Boy Color"
echo "===================================="
echo ""

# Check if book_data.h exists
if not test -f "$script_dir/book_data.h"
    echo "📥 Book data not found. Fetching from Project Gutenberg..."
    
    # Check for Python
    if not command -v python3 > /dev/null
        echo "❌ Python 3 required. Please install Python."
        exit 1
    end
    
    # Check for requests module
    python3 -c "import requests" 2>/dev/null
    if test $status -ne 0
        echo "📦 Installing requests module..."
        pip3 install requests
    end
    
    # Run fetch script
    cd $script_dir
    python3 fetch_ulysses.py
    
    if test $status -ne 0
        echo "❌ Failed to fetch book data"
        exit 1
    end
end

echo ""
echo "🔨 Building ROM..."

# Use parent build script
set parent_dir (dirname $script_dir)
cd $script_dir

# Build with GBDK
set gbdk_path "$parent_dir/gbdk"
set lcc "$gbdk_path/bin/lcc"

# Compile as GBC ROM with 8 banks (128KB) and MBC5
# -Wm-yc = CGB mode
# -Wm-yn = ROM name
# -Wm-yo8 = 8 ROM banks (128KB)
# -Wm-ya4 = MBC type 4 (MBC5)
$lcc -Wm-yc -Wm-yn"ULYSSES" -Wm-yo8 -Wm-ya4 -o ulysses.gbc ulysses.c

if test $status -eq 0
    echo "✅ Build successful: ulysses.gbc"
    
    # Show ROM info
    set rom_size (stat -c%s ulysses.gbc 2>/dev/null || stat -f%z ulysses.gbc 2>/dev/null)
    echo "📦 ROM size: $rom_size bytes"
    
    # Copy to assets if exists
    set assets_dir "$parent_dir/../system/public/assets/gameboy"
    if test -d $assets_dir
        cp ulysses.gbc $assets_dir/
        echo "📁 Copied to: $assets_dir/ulysses.gbc"
    end
else
    echo "❌ Build failed"
    exit 1
end

echo ""
echo "🎮 Ready to read Ulysses on your Game Boy Color!"
echo ""
echo "Controls:"
echo "  A / →     : Next page"
echo "  B / ←     : Previous page"  
echo "  ↑         : Skip 10 pages forward"
echo "  ↓         : Skip 10 pages back"
echo "  SELECT    : Return to start"
