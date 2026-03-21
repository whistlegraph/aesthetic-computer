#!/usr/bin/env fish

# Compile all gbdk_playground examples with auto-detection for GB vs GBC

set GBDK_BIN /workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/bin/lcc
set PLAYGROUND_DIR /workspaces/aesthetic-computer/kidlisp-gameboy/gbdk_playground
set OUTPUT_DIR /workspaces/aesthetic-computer/system/public/assets/gameboy/playground

# Create output directory
mkdir -p $OUTPUT_DIR

echo "🎮 Compiling gbdk_playground examples (auto-detecting GB/GBC)..."
echo ""

# Find all directories with .c files
for dir in $PLAYGROUND_DIR/*/
    set dir_name (basename $dir)
    
    # Skip non-project directories
    if test "$dir_name" = "docs"
        continue
    end
    
    # Check for Makefile to determine which .c files to compile
    set makefile "$dir/Makefile"
    if not test -f $makefile
        continue
    end
    
    # Extract OBJS from Makefile and convert .o to .c
    set objs_line (grep "^OBJS" $makefile)
    if test -z "$objs_line"
        continue
    end
    
    # Parse the object files and convert to .c files
    set c_files
    for obj in (string split " " $objs_line | string match "*.o")
        set c_file (string replace ".o" ".c" $obj)
        set c_files $c_files "$dir/$c_file"
    end
    
    if test (count $c_files) -eq 0
        continue
    end
    
    # Check if any source file indicates GBC features
    set is_gbc 0
    for c_file in $c_files
        if grep -q "gb/cgb.h\|set_bkg_palette\|set_sprite_palette\|CGBPal" $c_file
            set is_gbc 1
            break
        end
    end
    
    # Set extension and flags
    if test $is_gbc -eq 1
        set ext "gbc"
        set flags "-Wm-yc"
    else
        set ext "gb"
        set flags ""
    end
    
    set output_name "$dir_name.$ext"
    
    echo "📦 $dir_name → $output_name"
    
    # Compile (lcc will find all .c files in the directory)
    cd $dir
    if eval $GBDK_BIN $flags -o $OUTPUT_DIR/$output_name $c_files
        echo "   ✓ Compiled"
    else
        echo "   ✗ Failed"
    end
    echo ""
end

echo "✨ Done! ROMs in: $OUTPUT_DIR"
echo "   Run with: ac gameboy~playground/example-name"
