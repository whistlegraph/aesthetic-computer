#!/usr/bin/env fish

# Compile all GBDK examples - auto-detect if GBC features are needed
set GBDK_BIN /workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/bin/lcc
set EXAMPLES_DIR /workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/examples/gb
set OUTPUT_DIR /workspaces/aesthetic-computer/system/public/assets/gameboy/gbdk

echo "🎮 Compiling GBDK examples (auto-detecting GB/GBC)..."

# Find all standalone .c files in examples
set examples galaxy/galaxy filltest/filltest colorbar/colorbar sound/sound \
             rand/rand dscan/dscan template_minimal/main

for example in $examples
    set dir (dirname $example)
    set file (basename $example)
    set name (basename $example .c)
    set source_file $EXAMPLES_DIR/$dir/$file.c
    
    # Auto-detect if GBC features are used
    set is_gbc false
    set extension gb
    set flags ""
    
    # Check for GBC indicators: cgb.h include or CGB-specific functions
    if grep -q "gb/cgb.h\|set_bkg_palette\|set_sprite_palette\|CGBPal" $source_file
        set is_gbc true
        set extension gbc
        set flags "-Wm-yc"
        echo "  Compiling $dir/$file.c -> $name.gbc (GBC)"
    else
        echo "  Compiling $dir/$file.c -> $name.gb (GB)"
    end
    
    cd $EXAMPLES_DIR/$dir
    $GBDK_BIN $flags -o $name.$extension $file.c 2>&1 | grep -v "^$file.c:"
    
    if test -f $name.$extension
        cp $name.$extension $OUTPUT_DIR/
        echo "    ✓ $name.$extension"
    else
        echo "    ✗ Failed: $name.$extension"
    end
end

cd $OUTPUT_DIR
echo ""
echo "✅ Compiled examples:"
ls -1 *.gb *.gbc 2>/dev/null
echo ""
echo "Total: "(ls -1 *.gb *.gbc 2>/dev/null | wc -l)" ROMs"
