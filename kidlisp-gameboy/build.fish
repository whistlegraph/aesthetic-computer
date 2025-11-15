#!/usr/bin/env fish
# build.fish - Build GameBoy ROM from assembly using RGBDS
# Usage: ./build.fish <rom-name>
# Example: ./build.fish line-demo

if test (count $argv) -lt 1
    echo "Usage: ./build.fish <rom-name>"
    echo ""
    echo "Examples:"
    echo "  ./build.fish line-demo"
    echo "  ./build.fish ascii-scroll"
    echo "  ./build.fish bounce-ball"
    echo ""
    echo "This will:"
    echo "  1. Assemble test/<rom-name>.asm with RGBDS"
    echo "  2. Copy to gb-emulator directory"
    echo "  3. Ready to load with: ac gameboy~<rom-name>"
    exit 1
end

set ROM_NAME $argv[1]
set WORKSPACE_ROOT /workspaces/aesthetic-computer
set ASM_FILE "$WORKSPACE_ROOT/kidlisp-gameboy/test/$ROM_NAME.asm"
set OBJ_FILE "$WORKSPACE_ROOT/kidlisp-gameboy/test/$ROM_NAME.o"
set GB_FILE "$WORKSPACE_ROOT/kidlisp-gameboy/test/$ROM_NAME.gb"
set DEPLOY_PATH "$WORKSPACE_ROOT/system/public/aesthetic.computer/gb-emulator/$ROM_NAME.gb"

# Check if the ASM file exists
if not test -f "$ASM_FILE"
    echo "❌ Error: test/$ROM_NAME.asm not found"
    echo ""
    echo "Available ROMs:"
    ls -1 $WORKSPACE_ROOT/kidlisp-gameboy/test/*.asm | sed 's/.*\///' | sed 's/\.asm$//' | head -20
    exit 1
end

echo "🔨 Building $ROM_NAME with RGBDS..."

# Assemble with rgbasm
cd $WORKSPACE_ROOT/kidlisp-gameboy
rgbasm -o $OBJ_FILE $ASM_FILE

if test $status -eq 0
    echo "✅ Assembly successful!"
    
    # Link with rgblink
    rgblink -o $GB_FILE $OBJ_FILE
    
    if test $status -eq 0
        echo "✅ Linking successful!"
        
        # Fix ROM header with rgbfix
        rgbfix -p 255 -v $GB_FILE
        
        if test $status -eq 0
            echo "✅ ROM fix successful!"
            
            # Copy to gb-emulator directory
            cp $GB_FILE $DEPLOY_PATH
            
            # Clean up object file
            rm -f $OBJ_FILE
            
            echo "✅ Deployed to gb-emulator/"
            echo ""
            echo "🌐 Load with:"
            echo "   ac gameboy~$ROM_NAME"
            echo ""
        else
            echo "❌ ROM fix failed!"
            rm -f $OBJ_FILE
            exit 1
        end
    else
        echo "❌ Linking failed!"
        rm -f $OBJ_FILE
        exit 1
    end
else
    echo "❌ Assembly failed!"
    exit 1
end
