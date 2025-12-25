#!/usr/bin/env fish
# build.fish - Build a KidLisp program for Playdate
# Usage: ./build.fish examples/hello.lisp

set -l SCRIPT_DIR (realpath (dirname (status filename)))
set -l SDK_PATH "$SCRIPT_DIR/sdk"

# Check for input file
if test (count $argv) -lt 1
    echo "Usage: ./build.fish <source.lisp> [output-name]"
    echo ""
    echo "Examples:"
    echo "  ./build.fish examples/hello.lisp"
    echo "  ./build.fish examples/crank.lisp MyCrankGame"
    exit 1
end

set -l SOURCE_FILE $argv[1]
set -l OUTPUT_NAME (test (count $argv) -ge 2; and echo $argv[2]; or echo (basename $SOURCE_FILE .lisp))

# Check source exists
if not test -f $SOURCE_FILE
    echo "❌ Source file not found: $SOURCE_FILE"
    exit 1
end

echo "🎮 KidLisp Playdate Builder"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📄 Source: $SOURCE_FILE"
echo "📦 Output: $OUTPUT_NAME.pdx"
echo ""

# Create build directory (use absolute path)
set -l BUILD_DIR "$SCRIPT_DIR/build"
mkdir -p $BUILD_DIR/src

# Step 1: Compile KidLisp to C
echo "🔄 Compiling KidLisp → C..."
node $SCRIPT_DIR/compiler/kidlisp-to-pd.mjs $SOURCE_FILE $BUILD_DIR/src/main.c
if test $status -ne 0
    echo "❌ Compilation failed!"
    exit 1
end

# Step 2: Copy runtime files
echo "📋 Copying runtime..."
cp $SCRIPT_DIR/src/kidlisp.c $BUILD_DIR/src/
cp $SCRIPT_DIR/src/kidlisp.h $BUILD_DIR/src/

# Step 3: Create Makefile in build directory
echo "📝 Creating Makefile..."
printf 'HEAP_SIZE      = 8388208
STACK_SIZE     = 61800

PRODUCT = %s.pdx

SDK = %s

VPATH += src

SRC = src/main.c src/kidlisp.c

UINCDIR = src

include $(SDK)/C_API/buildsupport/common.mk
' $OUTPUT_NAME $SDK_PATH > $BUILD_DIR/Makefile

# Step 4: Build with make
echo "🔨 Building..."
cd $BUILD_DIR
set -x PLAYDATE_SDK_PATH $SDK_PATH
make 2>&1 | tee make.log

# Check if we have the binaries even if pdc failed
if test -f build/pdex.elf -a -f build/pdex.so
    echo ""
    echo "📦 Creating .pdx bundle..."

    rm -rf $OUTPUT_NAME.pdx
    mkdir -p $OUTPUT_NAME.pdx
    
    # Simulator needs the shared library
    cp build/pdex.so $OUTPUT_NAME.pdx/pdex.so
    
    # Device needs pdex.bin - try to use host pdc first, fall back to manual
    echo "🔧 Running pdc on host to create device binary..."
    
    # Copy Source folder to host and run pdc there
    mkdir -p Source
    cp build/pdex.elf Source/
    cp build/pdex.so Source/
    printf 'name=%s
author=aesthetic.computer
version=1.0.0
buildNumber=1
bundleID=computer.aesthetic.%s
' $OUTPUT_NAME $OUTPUT_NAME > Source/pdxinfo
    
    scp -rq Source jas@172.17.0.1:~/kidlisp-build-temp/
    ssh jas@172.17.0.1 "cd ~/kidlisp-build-temp && ~/PlaydateSDK/bin/pdc Source $OUTPUT_NAME.pdx" 2>/dev/null
    
    if test $status -eq 0
        # Copy back the properly built pdx
        scp -rq jas@172.17.0.1:~/kidlisp-build-temp/$OUTPUT_NAME.pdx ./
        ssh jas@172.17.0.1 "rm -rf ~/kidlisp-build-temp" 2>/dev/null
        echo "✅ Used host pdc for device-compatible build"
    else
        # Fallback: create pdex.bin manually from ELF
        echo "⚠️  Host pdc failed, creating pdex.bin manually..."
        arm-none-eabi-objcopy -O binary build/pdex.elf $OUTPUT_NAME.pdx/pdex.bin
        
        # Create pdxinfo
        printf 'name=%s
author=aesthetic.computer
version=1.0.0
buildNumber=1
bundleID=computer.aesthetic.%s
' $OUTPUT_NAME $OUTPUT_NAME > $OUTPUT_NAME.pdx/pdxinfo
    end
    
    # Cleanup
    rm -rf Source
    
    echo ""
    echo "✅ Build successful!"
    echo "📦 Output: $BUILD_DIR/$OUTPUT_NAME.pdx"
    
    # Show what's in the bundle
    echo ""
    echo "📋 Bundle contents:"
    ls -la $OUTPUT_NAME.pdx/
else
    echo "❌ Build failed - no binaries produced"
    exit 1
end
