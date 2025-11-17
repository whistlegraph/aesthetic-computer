#!/usr/bin/env fish

# Build script for Game Boy ROMs
# Automatically detects if source needs CGB (Color Game Boy) or DMG (original Game Boy)
# based on includes and BUILD_TARGET comment

if test (count $argv) -lt 1
    echo "Usage: build.fish <source.c> [output_name]"
    echo "Example: build.fish src/melody.c"
    echo "Example: build.fish src/scrub.c scrub"
    exit 1
end

set source_file $argv[1]
set gbdk_path (dirname (status -f))"/gbdk"
set lcc "$gbdk_path/bin/lcc"

# Get base name for output
if test (count $argv) -ge 2
    set output_name $argv[2]
else
    set output_name (basename $source_file .c)
end

# Detect if this needs CGB support
set is_cgb 0

# Check for BUILD_TARGET comment
if grep -q "BUILD_TARGET: CGB" $source_file
    set is_cgb 1
    echo "📟 Detected BUILD_TARGET: CGB"
else if grep -q "#include <gb/cgb.h>" $source_file
    set is_cgb 1
    echo "📟 Detected CGB include"
else if grep -q "BCPS_REG\|BCPD_REG\|set_sprite_palette\|set_bkg_palette" $source_file
    set is_cgb 1
    echo "📟 Detected CGB API usage"
end

# Build command
if test $is_cgb -eq 1
    set extension "gbc"
    set flags "-Wm-yc"
    echo "🎨 Building as Game Boy Color ROM..."
else
    set extension "gb"
    set flags ""
    echo "🎮 Building as original Game Boy ROM..."
end

set output_file "$output_name.$extension"

# Compile
echo "🔨 Compiling: $source_file -> $output_file"
if test -n "$flags"
    $lcc $flags -o $output_file $source_file
else
    $lcc -o $output_file $source_file
end

if test $status -eq 0
    echo "✅ Build successful: $output_file"
    
    # Copy to assets directory if it exists
    set assets_dir (dirname (status -f))"/../system/public/assets/gameboy"
    if test -d $assets_dir
        cp $output_file $assets_dir/
        echo "📦 Copied to: $assets_dir/$output_file"
    end
else
    echo "❌ Build failed"
    exit 1
end
