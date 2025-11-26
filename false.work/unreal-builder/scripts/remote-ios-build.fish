#!/usr/bin/env fish
# Build and package SpiderLily for iOS (device or simulator)
# Supports both remote (SSH to Mac) and local execution
# Usage: 
#   ./remote-ios-build.fish              # Remote build (device)
#   ./remote-ios-build.fish --local      # Local build on Mac (device)
#   ./remote-ios-build.fish simulator    # Remote build (simulator)
#   ./remote-ios-build.fish --local simulator  # Local build (simulator)

set build_version (date +%Y.%m.%d-%H%M)
set log_file "/tmp/build-ios-$build_version.log"
set build_type "device"
set local_mode false

# Parse arguments
for arg in $argv
    switch $arg
        case --local
            set local_mode true
        case simulator device
            set build_type $arg
    end
end

echo "========================================="
echo "Build & Package SpiderLily (iOS $build_type)"
echo "========================================="
echo ""
echo "Version: $build_version"
echo "Mode: "(test $local_mode = true && echo "LOCAL" || echo "REMOTE")
echo ""

if test $local_mode = false
    # ============================================
    # REMOTE MODE: SSH to Mac
    # ============================================
    
    # Load Mac credentials
    set VAULT_DIR /workspaces/aesthetic-computer/aesthetic-computer-vault/false.work
    set MAC_CREDS $VAULT_DIR/mac-builder-credentials.env

    if not test -f $MAC_CREDS
        echo "❌ Mac credentials not found: $MAC_CREDS"
        exit 1
    end

    set MAC_HOST (grep '^MAC_HOST=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
    set MAC_USERNAME (grep '^MAC_USERNAME=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
    set MAC_PASSWORD (grep '^MAC_PASSWORD=' $MAC_CREDS | cut -d= -f2 | tr -d '"')

    # Check if sshpass is installed
    if not command -v sshpass >/dev/null
        echo "⚠️  Installing sshpass..."
        sudo apt-get update -qq && sudo apt-get install -y -qq sshpass
    end

    # Helper for SSH commands
    function ssh_exec
        sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no "$MAC_USERNAME@$MAC_HOST" $argv
    end

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
    ssh_exec "cd ~/Perforce/spiderlily_build_workspace_macmini/SL_main && export P4PORT=ssl:falsework.helixcore.io:1666 && export P4USER=machine && export P4CLIENT=spiderlily_build_workspace_macmini && /opt/homebrew/bin/p4 sync" 2>&1 | tee -a $log_file
    echo ""

    # Run build on Mac via SSH
    echo "========================================="
    echo "Building iOS on Mac..."
    echo "========================================="
    ssh_exec "~/package-spiderlily-ios.sh $build_type 2>&1" | tee -a $log_file

    # Check if build succeeded
    if test $status -ne 0
        echo ""
        echo "❌ Build failed!"
        echo "   Check log: $log_file"
        exit 1
    end

    # Verify build exists
    set app_path "~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app"
    set build_exists (ssh_exec "test -d $app_path && echo yes || echo no")

    if test "$build_exists" != "yes"
        echo ""
        echo "❌ Build verification failed!"
        echo "   Expected .app at: $app_path"
        exit 1
    end

    echo ""
    echo "✅ Build verified!"

    echo ""
    echo "========================================="
    echo "Creating .ipa package..."
    echo "========================================="

    # Create .ipa on Mac (zip the Payload folder structure)
    set ipa_name "spiderlily-ios-$build_type-$build_version.ipa"
    ssh_exec "cd ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS && rm -rf Payload && mkdir -p Payload && cp -R SpiderLily.app Payload/ && zip -r -q ~/$ipa_name Payload/"

    echo ""
    echo "========================================="
    echo "Copying build locally..."
    echo "========================================="

    # Copy .ipa to builds.false.work/ios/
    sshpass -p "$MAC_PASSWORD" scp -o StrictHostKeyChecking=no \
        "$MAC_USERNAME@$MAC_HOST:~/$ipa_name" \
        /workspaces/aesthetic-computer/system/public/builds.false.work/ios/SpiderLily-latest.ipa

    # Also save dated version
    sshpass -p "$MAC_PASSWORD" scp -o StrictHostKeyChecking=no \
        "$MAC_USERNAME@$MAC_HOST:~/$ipa_name" \
        /workspaces/aesthetic-computer/system/public/builds.false.work/ios/$ipa_name

    echo ""
    echo "========================================="
    echo "Registering build in database..."
    echo "========================================="

    # Calculate file size
    set file_size_mb (math -s0 (stat -c%s /workspaces/aesthetic-computer/system/public/builds.false.work/ios/SpiderLily-latest.ipa) / 1048576)
    
    # Get the dated IPA filename  
    set ipa_dated_name spiderlily-ios-$build_type-$build_version.ipa
    
    # Build the download URL
    set download_url "https://builds.false.work/ios/$ipa_dated_name"
    
    # Get current timestamp in ISO format
    set build_timestamp (date -Iseconds)
    
    # Register build in MongoDB via Netlify function
    source /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/shared/register-build.fish
    register_build "ios" "$build_version" "$build_timestamp" "$file_size_mb" "$download_url" "L_VerticalSlice_Demo" "UE_5.6" "" "" "$build_type"

    echo ""
    echo "✅ Build complete and deployed!"
    echo "📦 Build: $build_version"
    echo "📱 Type: iOS $build_type"
    echo "💾 Size: $file_size_mb MB"
    echo "🌐 Builds page: https://builds.false.work/"
    echo "📱 Install via Safari: https://builds.false.work/ios/"
    echo "📥 Direct download: https://builds.false.work/ios/$ipa_dated_name"

else
    # ============================================
    # LOCAL MODE: Run on Mac directly
    # ============================================
    
    echo "========================================="
    echo "Running local build..."
    echo "========================================="
    echo ""
    
    # Check if we're actually on a Mac
    if not test (uname) = "Darwin"
        echo "❌ Error: Local mode must be run on macOS"
        echo "   Current OS: "(uname)
        exit 1
    end
    
    # Check if build script exists
    set BUILD_SCRIPT ~/package-spiderlily-ios.sh
    if not test -f $BUILD_SCRIPT
        echo "⚠️  Build script not found, using script from repo..."
        set BUILD_SCRIPT /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/mac/package-spiderlily-ios.sh
        
        if not test -f $BUILD_SCRIPT
            echo "❌ Build script not found at: $BUILD_SCRIPT"
            exit 1
        end
    end
    
    # Sync Perforce
    echo "========================================="
    echo "Syncing Perforce..."
    echo "========================================="
    cd ~/Perforce/spiderlily_build_workspace_macmini/SL_main
    set -x P4PORT ssl:falsework.helixcore.io:1666
    set -x P4USER machine
    set -x P4CLIENT spiderlily_build_workspace_macmini
    /opt/homebrew/bin/p4 sync
    echo ""
    
    # Run build
    echo "========================================="
    echo "Building iOS ($build_type)..."
    echo "========================================="
    bash $BUILD_SCRIPT $build_type 2>&1 | tee $log_file
    
    if test $status -ne 0
        echo ""
        echo "❌ Build failed!"
        echo "   Check log: $log_file"
        exit 1
    end
    
    # Verify build
    set app_path ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app
    if not test -d $app_path
        echo ""
        echo "❌ Build verification failed!"
        echo "   Expected .app at: $app_path"
        exit 1
    end
    
    echo ""
    echo "✅ Build complete!"
    echo ""
    echo "📦 Output: $app_path"
    echo "📊 Size: "(du -sh $app_path | cut -f1)
    echo ""
    echo "📱 To deploy to device:"
    echo "   ./deploy-spiderlily-ios.sh"
    echo ""
    echo "📝 Build log saved to: $log_file"
end
