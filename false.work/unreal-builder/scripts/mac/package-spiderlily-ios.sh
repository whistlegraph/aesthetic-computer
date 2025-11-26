#!/bin/bash
# Package SpiderLily for iOS
set -e

UE_ROOT="/Users/Shared/Epic Games/UE_5.6"
UAT="$UE_ROOT/Engine/Build/BatchFiles/RunUAT.sh"
PROJECT_ROOT="$HOME/Perforce/spiderlily_build_workspace_macmini/SL_main"
PROJECT_FILE="$PROJECT_ROOT/SpiderLily.uproject"
OUTPUT_DIR="$PROJECT_ROOT/Packaged/IOS"

# Default to device build, can override with: ./package-spiderlily-ios.sh simulator
BUILD_TYPE="${1:-device}"

echo "========================================="
echo "Packaging SpiderLily for iOS ($BUILD_TYPE)"
echo "========================================="
echo ""
echo "Project: $PROJECT_FILE"
echo "Output: $OUTPUT_DIR"
echo ""

# Check if files exist
if [ ! -f "$UAT" ]; then
    echo "❌ ERROR: RunUAT not found at $UAT"
    exit 1
fi

if [ ! -f "$PROJECT_FILE" ]; then
    echo "❌ ERROR: Project file not found at $PROJECT_FILE"
    exit 1
fi

echo "✓ Files verified, starting package..."
echo ""

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Function to fix FMOD header bug (same as Mac)
fix_fmod_headers() {
    echo "🔧 Checking for FMOD header generation bugs..."
    local callback_file_game="$PROJECT_ROOT/Plugins/FMODStudio/Intermediate/Build/IOS/UnrealGame/Inc/FMODStudio/UHT/FMODCallbackHandler.gen.cpp"
    local callback_file_editor="$PROJECT_ROOT/Plugins/FMODStudio/Intermediate/Build/IOS/UnrealEditor/Inc/FMODStudio/UHT/FMODCallbackHandler.gen.cpp"
    
    if [ -f "$callback_file_game" ] && grep -q "allbackHandler.h" "$callback_file_game"; then
        sed -i.bak 's/allbackHandler\.h/FMODCallbackHandler.h/g' "$callback_file_game"
        echo "  ✓ Fixed: UnrealGame/FMODCallbackHandler.gen.cpp"
    fi
    
    if [ -f "$callback_file_editor" ] && grep -q "allbackHandler.h" "$callback_file_editor"; then
        sed -i.bak 's/allbackHandler\.h/FMODCallbackHandler.h/g' "$callback_file_editor"
        echo "  ✓ Fixed: UnrealEditor/FMODCallbackHandler.gen.cpp"
    fi
}

# Function to patch Xcode project with team ID
patch_xcode_project() {
    echo "🔧 Patching Xcode project with team ID..."
    local xcode_project="$PROJECT_ROOT/Intermediate/ProjectFilesIOS/SpiderLily (IOS).xcodeproj/project.pbxproj"
    
    # Wait for the Xcode project to be generated
    local max_wait=30
    local waited=0
    while [ ! -f "$xcode_project" ] && [ $waited -lt $max_wait ]; do
        echo "  ⏳ Waiting for Xcode project to be generated..."
        sleep 1
        waited=$((waited + 1))
    done
    
    if [ -f "$xcode_project" ]; then
        # Add DEVELOPMENT_TEAM to all build configurations
        # Look for buildSettings sections and add team ID if not present
        if ! grep -q "DEVELOPMENT_TEAM = F7G74Z35B8" "$xcode_project"; then
            # Use perl for more robust multi-line replacement
            perl -i.bak -pe 's/(buildSettings = \{)/\1\n\t\t\t\tDEVELOPMENT_TEAM = F7G74Z35B8;/g' "$xcode_project"
            echo "  ✓ Xcode project patched with Team ID F7G74Z35B8"
        else
            echo "  ✓ Xcode project already has Team ID F7G74Z35B8"
        fi
    else
        echo "  ⚠️  Warning: Xcode project not found at $xcode_project after ${max_wait}s"
    fi
}

# Determine build flags based on build type
if [ "$BUILD_TYPE" = "simulator" ]; then
    echo "🔧 Building for iOS Simulator..."
    PLATFORM_FLAGS="-platform=IOS -cookflavor=Multi -device=IOS_Simulator"
    EXPECTED_STAGED_PATH="$PROJECT_ROOT/Saved/StagedBuilds/IOS_Simulator/SpiderLily.app"
    # Simulator uses ad-hoc signing
    SIGNING_IDENTITY="-"
    BUNDLE_NAME="work.false.SpiderLily"
    PROVISION_UUID=""
else
    echo "🔧 Building for iOS Device..."
    PLATFORM_FLAGS="-platform=IOS -device=IOS"
    EXPECTED_STAGED_PATH="$PROJECT_ROOT/Saved/StagedBuilds/IOS/SpiderLily.app"
    # Device builds use proper signing with Apple Development certificate and provisioning profile
    SIGNING_IDENTITY="Apple Development: make@false.work (7F4BYG5WCH)"
    BUNDLE_NAME="work.false.SpiderLily"
    PROVISION_UUID=""  # Empty to allow automatic provisioning
fi

# Run Unreal Automation Tool to build and package
echo "🚀 Running BuildCookRun..."
echo "   NOTE: We'll let the Xcode signing fail, then manually sign and stage"
echo ""

if [ -n "$PROVISION_UUID" ]; then
    # Device build - just cook and build, skip staging (will fail on signing)
    "$UAT" BuildCookRun \
        -project="$PROJECT_FILE" \
        $PLATFORM_FLAGS \
        -clientconfig=Development \
        -serverconfig=Development \
        -cook \
        -allmaps \
        -build \
        -noP4 \
        -utf8output 2>&1 | tee /tmp/build_ios.log || true
else
    # Simulator build (no provisioning profile needed)
    "$UAT" BuildCookRun \
        -project="$PROJECT_FILE" \
        $PLATFORM_FLAGS \
        -codesigningidentity="$SIGNING_IDENTITY" \
        -bundlename="$BUNDLE_NAME" \
        -teamID=F7G74Z35B8 \
        -clientconfig=Development \
        -serverconfig=Development \
        -cook \
        -allmaps \
        -build \
        -stage \
        -pak \
        -archive \
        -archivedirectory="$OUTPUT_DIR" \
        -noP4 \
        -utf8output 2>&1 | tee /tmp/build_ios.log
fi

echo ""
echo "✅ Cook and build complete (signing step skipped)"
echo ""

# For device builds, manually patch and sign
if [ -z "$PROVISION_UUID" ]; then
    echo "Simulator build complete, skipping manual signing"
else
    echo "🔧 Manually signing iOS binary..."
    
    # Patch the Xcode project with team ID
    XCODE_PROJ="$PROJECT_ROOT/Intermediate/ProjectFilesIOS/SpiderLily (IOS).xcodeproj/project.pbxproj"
    if [ -f "$XCODE_PROJ" ]; then
        perl -i.bak -pe 's/(buildSettings = \{)/\1\n\t\t\t\tDEVELOPMENT_TEAM = F7G74Z35B8;/g' "$XCODE_PROJ"
        echo "  ✓ Patched Xcode project"
    fi
    
    # Now run xcodebuild with team ID to sign the binary
    cd "$PROJECT_ROOT/Intermediate/ProjectFiles"
    xcodebuild build \
        -workspace "SpiderLily_IOS_SpiderLily.xcworkspace" \
        -scheme "SpiderLily" \
        -configuration "Development" \
        -destination generic/platform="iOS" \
        DEVELOPMENT_TEAM=F7G74Z35B8 \
        CODE_SIGN_ALLOW_ENTITLEMENTS_MODIFICATION=YES \
        -allowProvisioningUpdates
    
    if [ $? -eq 0 ]; then
        echo "  ✓ Xcode build and signing successful"
    else
        echo "  ❌ Xcode build failed"
        exit 1
    fi
    
    # Now stage the app
    echo ""
    echo "📦 Staging the signed app..."
    "$UAT" BuildCookRun \
        -project="$PROJECT_FILE" \
        $PLATFORM_FLAGS \
        -clientconfig=Development \
        -serverconfig=Development \
        -skipcook \
        -skipbuild \
        -stage \
        -pak \
        -archive \
        -archivedirectory="$OUTPUT_DIR" \
        -noP4 \
        -utf8output
fi

# Check if it failed with FMOD bug
if grep -q "allbackHandler.h" /tmp/build_ios.log; then
    echo ""
    echo "⚠️  Detected FMOD header bug, applying fix and retrying..."
    fix_fmod_headers
    
    echo ""
    echo "🚀 Retrying build after FMOD fix..."
    if [ -n "$PROVISION_UUID" ]; then
        # Device build
        "$UAT" BuildCookRun \
            -project="$PROJECT_FILE" \
            $PLATFORM_FLAGS \
            -clientconfig=Development \
            -serverconfig=Development \
            -cook \
            -allmaps \
            -build \
            -noP4 \
            -utf8output || true
    else
        # Simulator build
        "$UAT" BuildCookRun \
            -project="$PROJECT_FILE" \
            $PLATFORM_FLAGS \
            -codesigningidentity="$SIGNING_IDENTITY" \
            -bundlename="$BUNDLE_NAME" \
            -teamID=F7G74Z35B8 \
            -clientconfig=Development \
            -serverconfig=Development \
            -cook \
            -allmaps \
            -build \
            -stage \
            -pak \
            -archive \
            -archivedirectory="$OUTPUT_DIR" \
            -noP4 \
            -utf8output
    fi
fi

echo ""
echo "✅ Build complete!"
echo ""

# Fix archiving - copy the correct .app with all staged content
if [ -d "$EXPECTED_STAGED_PATH" ]; then
    echo "📦 Copying staged .app to archive directory..."
    APP_NAME=$(basename "$EXPECTED_STAGED_PATH")
    rm -rf "$OUTPUT_DIR/$APP_NAME"
    cp -R "$EXPECTED_STAGED_PATH" "$OUTPUT_DIR/$APP_NAME"
    echo "  ✓ Copied: $EXPECTED_STAGED_PATH -> $OUTPUT_DIR/$APP_NAME"
    
    # iOS requires .uproject file to be in the package
    echo "📄 Copying .uproject file to iOS package..."
    cp "$PROJECT_FILE" "$OUTPUT_DIR/$APP_NAME/SpiderLily.uproject"
    echo "  ✓ Copied: SpiderLily.uproject -> $OUTPUT_DIR/$APP_NAME/"
    
    # Fix FMOD folder structure - create Mobile subfolder with bank files
    echo "🔧 Fixing FMOD folder structure..."
    FMOD_DIR="$OUTPUT_DIR/$APP_NAME/cookeddata/spiderlily/content/fmod"
    if [ -d "$FMOD_DIR" ]; then
        mkdir -p "$FMOD_DIR/Mobile"
        if [ -f "$FMOD_DIR/master.bank" ]; then
            cp "$FMOD_DIR/master.bank" "$FMOD_DIR/Mobile/"
            cp "$FMOD_DIR/master.strings.bank" "$FMOD_DIR/Mobile/"
            echo "  ✓ Created Mobile/ folder with FMOD banks"
        fi
    fi
fi

echo ""
echo "✅ Package complete!"
echo ""
echo "📦 Output location:"
find "$OUTPUT_DIR" -name "*.app" -maxdepth 2 2>/dev/null || echo "   $OUTPUT_DIR"
echo ""

if [ "$BUILD_TYPE" = "simulator" ]; then
    echo "🎮 To test in simulator:"
    echo "   xcrun simctl boot 'iPhone 16 Pro' # or your preferred simulator"
    echo "   xcrun simctl install booted '$OUTPUT_DIR/SpiderLily.app'"
    echo "   xcrun simctl launch booted com.YourCompany.SpiderLily"
else
    # Upload to builds.false.work
    UPLOAD_SCRIPT="$(dirname "$0")/upload-spiderlily-ios.sh"
    if [ -f "$UPLOAD_SCRIPT" ]; then
        echo "📤 Uploading to builds.false.work..."
        bash "$UPLOAD_SCRIPT" "$OUTPUT_DIR/SpiderLily.app"
    else
        echo "📱 To deploy to device:"
        echo "   1. Connect your iOS device via USB"
        echo "   2. Open Xcode and deploy using: Window > Devices and Simulators"
        echo "   3. Or use: ios-deploy --bundle '$OUTPUT_DIR/SpiderLily.app'"
    fi
fi
