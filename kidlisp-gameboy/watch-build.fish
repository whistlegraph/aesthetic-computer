#!/usr/bin/env fish
# watch-build.fish - Watch and auto-rebuild GameBoy ROMs with RGBDS
# Usage: ./watch-build.fish <rom-name>
# Example: ./watch-build.fish line-demo

if test (count $argv) -lt 1
    echo "Usage: ./watch-build.fish <rom-name>"
    echo ""
    echo "This will watch test/<rom-name>.asm for changes and auto-rebuild"
    echo ""
    echo "Examples:"
    echo "  ./watch-build.fish line-demo"
    echo "  ./watch-build.fish bounce-ball"
    exit 1
end

# Get the directory where this script lives
set SCRIPT_DIR (dirname (status --current-filename))
cd $SCRIPT_DIR

set ROM_NAME $argv[1]
set ASM_FILE "test/$ROM_NAME.asm"
set OBJ_FILE "test/$ROM_NAME.o"
set GB_FILE "test/$ROM_NAME.gb"
set DEPLOY_PATH "../system/public/aesthetic.computer/gb-emulator/$ROM_NAME.gb"

# Check if file exists
if not test -f $ASM_FILE
    echo "❌ Error: $ASM_FILE not found"
    exit 1
end

echo "👀 Watching $ASM_FILE for changes..."
echo "🌐 Load with: ac gameboy~$ROM_NAME"
echo "Press Ctrl+C to stop"
echo ""

# Build function
function build_rom
    rgbasm -o $OBJ_FILE $ASM_FILE
    and rgblink -o $GB_FILE $OBJ_FILE
    and rgbfix -p 255 -v $GB_FILE
    and cp $GB_FILE $DEPLOY_PATH
    and rm -f $OBJ_FILE
end

# Initial build
echo "🔨 Initial build..."
if build_rom
    echo "✅ Ready! Edit $ASM_FILE and save to rebuild."
    echo "🔄 Auto-reloading gameboy~$ROM_NAME..."
    ac gameboy~$ROM_NAME
else
    echo "❌ Initial build failed!"
end
echo ""

# Watch for changes using inotifywait (if available) or fswatch
if command -v inotifywait > /dev/null
    # Linux
    while inotifywait -e modify $ASM_FILE
        echo ""
        echo "🔨 Rebuilding..."
        if build_rom
            echo "✅ Build complete!"
            echo "🔄 Auto-reloading gameboy~$ROM_NAME..."
            ac gameboy~$ROM_NAME
        else
            echo "❌ Build failed!"
        end
        echo ""
    end
else if command -v fswatch > /dev/null
    # macOS
    fswatch -o $ASM_FILE | while read num
        echo ""
        echo "🔨 Rebuilding..."
        if build_rom
            echo "✅ Build complete!"
            echo "🔄 Auto-reloading gameboy~$ROM_NAME..."
            ac gameboy~$ROM_NAME
        else
            echo "❌ Build failed!"
        end
        echo ""
    end
else
    echo "⚠️  No file watcher found (inotifywait or fswatch)"
    echo "Install inotify-tools: dnf install inotify-tools"
    exit 1
end
