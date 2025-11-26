#!/bin/bash
# Build and package SpiderLily for iOS (device or simulator)
# Supports both remote (SSH to Mac) and local execution
# Usage: 
#   ./remote-ios-build.sh              # Remote build (device)
#   ./remote-ios-build.sh --local      # Local build on Mac (device)
#   ./remote-ios-build.sh simulator    # Remote build (simulator)
#   ./remote-ios-build.sh --local simulator  # Local build (simulator)

set -e

build_version=$(date +%Y.%m.%d-%H%M)
log_file="/tmp/build-ios-$build_version.log"
build_type="device"
local_mode=false

# Parse arguments
for arg in "$@"; do
    case $arg in
        --local)
            local_mode=true
            ;;
        simulator|device)
            build_type=$arg
            ;;
    esac
done

echo "========================================="
echo "Build & Package SpiderLily (iOS $build_type)"
echo "========================================="
echo ""
echo "Version: $build_version"
if [ "$local_mode" = true ]; then
    echo "Mode: LOCAL"
else
    echo "Mode: REMOTE"
fi
echo ""

if [ "$local_mode" = false ]; then
    # ============================================
    # REMOTE MODE: SSH to Mac
    # ============================================
    
    # Load Mac credentials
    VAULT_DIR=/workspaces/aesthetic-computer/aesthetic-computer-vault/false.work
    MAC_CREDS=$VAULT_DIR/mac-builder-credentials.env

    if [ ! -f "$MAC_CREDS" ]; then
        echo "❌ Mac credentials not found: $MAC_CREDS"
        exit 1
    fi

    MAC_HOST=$(grep '^MAC_HOST=' "$MAC_CREDS" | cut -d= -f2 | tr -d '"')
    MAC_USERNAME=$(grep '^MAC_USERNAME=' "$MAC_CREDS" | cut -d= -f2 | tr -d '"')
    MAC_PASSWORD=$(grep '^MAC_PASSWORD=' "$MAC_CREDS" | cut -d= -f2 | tr -d '"')

    # Check if sshpass is installed
    if ! command -v sshpass >/dev/null; then
        echo "⚠️  Installing sshpass..."
        sudo apt-get update -qq && sudo apt-get install -y -qq sshpass
    fi

    # Helper for SSH commands
    ssh_exec() {
        sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no "$MAC_USERNAME@$MAC_HOST" "$@"
    }

    echo "========================================="
    echo "Syncing build script to Mac..."
    echo "========================================="
    sshpass -p "$MAC_PASSWORD" scp -o StrictHostKeyChecking=no \
        /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/mac/package-spiderlily-ios.sh \
        "$MAC_USERNAME@$MAC_HOST:~/package-spiderlily-ios.sh"
    ssh_exec "chmod +x ~/package-spiderlily-ios.sh"
    echo ""

    # Sync Perforce on Mac
    echo "========================================="
    echo "Syncing Perforce on Mac..."
    echo "========================================="
    ssh_exec "cd ~/Perforce/spiderlily_build_workspace_macmini/SL_main && export P4PORT=ssl:falsework.helixcore.io:1666 && export P4USER=machine && export P4CLIENT=spiderlily_build_workspace_macmini && /opt/homebrew/bin/p4 sync" 2>&1 | tee -a "$log_file"
    echo ""

    # Run build on Mac via SSH
    echo "========================================="
    echo "Building iOS on Mac..."
    echo "========================================="
    ssh_exec "~/package-spiderlily-ios.sh $build_type 2>&1" | tee -a "$log_file"

    # Check if build succeeded
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
        echo ""
        echo "❌ Build failed!"
        echo "   Check log: $log_file"
        exit 1
    fi

    # Verify build exists
    app_path="~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app"
    build_exists=$(ssh_exec "test -d $app_path && echo yes || echo no")

    if [ "$build_exists" != "yes" ]; then
        echo ""
        echo "❌ Build verification failed!"
        echo "   Expected .app at: $app_path"
        exit 1
    fi

    echo ""
    echo "✅ Build verified!"

    echo ""
    echo "========================================="
    echo "Creating .ipa package..."
    echo "========================================="

    # Create .ipa on Mac (zip the Payload folder structure)
    ipa_name="spiderlily-ios-$build_type-$build_version.ipa"
    ssh_exec "cd ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS && rm -rf Payload && mkdir -p Payload && cp -R SpiderLily.app Payload/ && zip -r -q ~/$ipa_name Payload/"

    echo ""
    echo "========================================="
    echo "Copying build to assets..."
    echo "========================================="

    # Copy .ipa to local assets
    sshpass -p "$MAC_PASSWORD" scp -o StrictHostKeyChecking=no \
        "$MAC_USERNAME@$MAC_HOST:~/$ipa_name" \
        /workspaces/aesthetic-computer/system/public/assets/false.work/

    # Copy log file to assets
    cp "$log_file" /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-ios-$build_type-$build_version.txt

    echo ""
    echo "========================================="
    echo "Uploading to assets.aesthetic.computer..."
    echo "========================================="

    cd /workspaces/aesthetic-computer
    npm run assets:sync:up

    echo ""
    echo "========================================="
    echo "Updating builds.false.work page..."
    echo "========================================="

    # Ensure the file exists before calculating size
    if [ ! -f "/workspaces/aesthetic-computer/system/public/assets/false.work/$ipa_name" ]; then
        echo "❌ Build IPA not found at /workspaces/aesthetic-computer/system/public/assets/false.work/$ipa_name"
        exit 1
    fi

    # Get file size in MB (rounded to whole number)
    file_size=$(stat -c%s "/workspaces/aesthetic-computer/system/public/assets/false.work/$ipa_name" | awk '{print int($1/1048576)}')
    download_url="https://assets.aesthetic.computer/false.work/$ipa_name"
    iso_timestamp=$(date -Iseconds | cut -d'+' -f1)

    # Extract start level from DefaultEngine.ini (get last part after dot)
    full_map=$(ssh_exec "grep 'GameDefaultMap=' ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Config/DefaultEngine.ini | cut -d= -f2")
    start_level=$(echo "$full_map" | awk -F'.' '{print $NF}')

    # Extract UE version
    ue_version="UE_5.6"

    # Use shared function to update the builds page
    source /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/shared/update-builds-page.fish
    update_builds_page "ios" "$build_version" "$iso_timestamp" "$file_size" "$start_level" "$ue_version" "$download_url"

    echo ""
    echo "========================================="
    echo "Committing and pushing to GitHub..."
    echo "========================================="

    cd /workspaces/aesthetic-computer
    git add system/public/builds.false.work/index.html
    git commit -m "Add SpiderLily iOS ($build_type) build $build_version"

    # Try to push, if it fails due to remote changes, pull and retry
    if ! git push; then
        echo "Push failed, pulling remote changes and retrying..."
        
        # Check if there are uncommitted changes (excluding the file we just committed)
        has_changes=$(git status --porcelain | grep -v "^M  system/public/builds.false.work/index.html" | wc -l)
        
        if [ "$has_changes" -gt 0 ]; then
            echo "⚠️  Warning: Uncommitted changes detected, stashing them..."
            git status --short
            git stash push -m "Build script auto-stash before rebase"
            echo "💾 Changes stashed - use 'git stash pop' to restore them after the build"
        fi
        
        # Pull with rebase
        git pull --rebase
        # Push again
        git push
        
        echo "✅ Build pushed successfully"
    fi

    echo ""
    echo "✅ Build complete and deployed!"
    echo "📦 Build: $build_version"
    echo "📱 Type: iOS $build_type"
    echo "🔗 Download: $download_url"
    echo "🌐 Page: https://builds.false.work"

else
    # ============================================
    # LOCAL MODE: Run on Mac directly
    # ============================================
    
    echo "========================================="
    echo "Running local build..."
    echo "========================================="
    echo ""
    
    # Check if we're actually on a Mac
    if [ "$(uname)" != "Darwin" ]; then
        echo "❌ Error: Local mode must be run on macOS"
        echo "   Current OS: $(uname)"
        exit 1
    fi
    
    # Check if build script exists
    BUILD_SCRIPT=~/package-spiderlily-ios.sh
    if [ ! -f "$BUILD_SCRIPT" ]; then
        echo "⚠️  Build script not found, using script from repo..."
        BUILD_SCRIPT=/workspaces/aesthetic-computer/false.work/unreal-builder/scripts/mac/package-spiderlily-ios.sh
        
        if [ ! -f "$BUILD_SCRIPT" ]; then
            echo "❌ Build script not found at: $BUILD_SCRIPT"
            exit 1
        fi
    fi
    
    # Sync Perforce
    echo "========================================="
    echo "Syncing Perforce..."
    echo "========================================="
    cd ~/Perforce/spiderlily_build_workspace_macmini/SL_main
    export P4PORT=ssl:falsework.helixcore.io:1666
    export P4USER=machine
    export P4CLIENT=spiderlily_build_workspace_macmini
    /opt/homebrew/bin/p4 sync
    echo ""
    
    # Run build
    echo "========================================="
    echo "Building iOS ($build_type)..."
    echo "========================================="
    bash "$BUILD_SCRIPT" "$build_type" 2>&1 | tee "$log_file"
    
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
        echo ""
        echo "❌ Build failed!"
        echo "   Check log: $log_file"
        exit 1
    fi
    
    # Verify build
    app_path=~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app
    if [ ! -d "$app_path" ]; then
        echo ""
        echo "❌ Build verification failed!"
        echo "   Expected .app at: $app_path"
        exit 1
    fi
    
    echo ""
    echo "✅ Build complete!"
    echo ""
    echo "📦 Output: $app_path"
    echo "📊 Size: $(du -sh "$app_path" | cut -f1)"
    echo ""
    echo "📱 To deploy to device:"
    echo "   ./deploy-spiderlily-ios.sh"
    echo ""
    echo "📝 Build log saved to: $log_file"
fi
