#!/usr/bin/env fish

# Compile all GBDK examples to .gb format
set GBDK_BIN /workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/bin/lcc
set EXAMPLES_DIR /workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/examples/gb
set OUTPUT_DIR /workspaces/aesthetic-computer/system/public/assets/gameboy/gbdk

echo "🎮 Compiling GBDK examples..."

# Find all standalone .c files in examples (not in subdirs with Makefiles)
set examples galaxy/galaxy filltest/filltest colorbar/colorbar sound/sound \
             rand/rand dscan/dscan template_minimal/main

for example in $examples
    set dir (dirname $example)
    set file (basename $example)
    set name (basename $example .c)
    
    echo "  Compiling $dir/$file.c -> $name.gb"
    
    cd $EXAMPLES_DIR/$dir
    $GBDK_BIN -o $name.gb $file.c 2>&1 | grep -v "^$file.c:"
    
    if test -f $name.gb
        cp $name.gb $OUTPUT_DIR/
        echo "    ✓ $name.gb"
    else
        echo "    ✗ Failed: $name.gb"
    end
end

cd $OUTPUT_DIR
echo ""
echo "✅ Compiled examples:"
ls -1 *.gb 2>/dev/null
echo ""
echo "Total: "(ls -1 *.gb 2>/dev/null | wc -l)" ROMs"
