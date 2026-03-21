#!/usr/bin/env fish
# Schedule automated builds from dev container
# Triggers builds on remote machines (Mac Mini, GCP Windows VM)

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../../../aesthetic-computer-vault/false.work"

# Load Mac credentials
if test -f "$VAULT_DIR/mac-builder-credentials.env"
    source "$VAULT_DIR/mac-builder-credentials.env"
else
    echo "❌ Mac credentials not found in vault"
    exit 1
end

# Load Windows/GCP credentials
if test -f "$SCRIPT_DIR/../../ue5-builder.env"
    source "$SCRIPT_DIR/../../ue5-builder.env"
else
    echo "❌ GCP credentials not found"
    exit 1
end

set BUILD_TYPE $argv[1]
set PLATFORM $argv[2]

if test -z "$BUILD_TYPE"
    set BUILD_TYPE "nightly"
end

if test -z "$PLATFORM"
    set PLATFORM "all"
end

echo "========================================="
echo "SpiderLily Automated Build Scheduler"
echo "========================================="
echo "Type: $BUILD_TYPE"
echo "Platform: $PLATFORM"
echo ""

set VERSION (date +%Y.%m.%d.%H.%M)

# Mac build
if test "$PLATFORM" = "all" -o "$PLATFORM" = "mac"
    echo "🍎 Triggering Mac build..."
    echo "  Target: $MAC_BUILDER_HOST"
    echo ""
    
    sshpass -p "$MAC_BUILDER_PASSWORD" ssh -o StrictHostKeyChecking=no "$MAC_BUILDER_USER@$MAC_BUILDER_HOST" \
        "cd ~/aesthetic-computer/false.work/unreal-builder/scripts/mac && ./build-and-upload.sh $VERSION"
    
    if test $status -eq 0
        echo "✅ Mac build complete!"
    else
        echo "❌ Mac build failed!"
    end
    echo ""
end

# Windows build
if test "$PLATFORM" = "all" -o "$PLATFORM" = "windows"
    echo "🪟 Triggering Windows build..."
    echo "  Target: $VM_IP"
    echo ""
    
    # Use gcloud compute ssh or direct RDP/SSH to Windows VM
    # For GCP, we can use gcloud compute ssh with PowerShell
    gcloud compute ssh "$GCP_VM_NAME" \
        --zone="$GCP_ZONE" \
        --project="$GCP_PROJECT_ID" \
        --command="powershell -File C:\aesthetic-computer\false.work\unreal-builder\scripts\windows-build-and-upload.ps1 -Version $VERSION"
    
    if test $status -eq 0
        echo "✅ Windows build complete!"
    else
        echo "❌ Windows build failed!"
    end
    echo ""
end

# iOS build (if implemented)
if test "$PLATFORM" = "all" -o "$PLATFORM" = "ios"
    echo "📱 Triggering iOS build..."
    echo "  Target: $MAC_BUILDER_HOST (same as Mac)"
    echo ""
    
    sshpass -p "$MAC_BUILDER_PASSWORD" ssh -o StrictHostKeyChecking=no "$MAC_BUILDER_USER@$MAC_BUILDER_HOST" \
        "cd ~/aesthetic-computer/false.work/unreal-builder/scripts/mac && ./package-spiderlily-ios.sh device && ../upload-to-spaces.sh ios ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Packaged/IOS/SpiderLily.app $VERSION"
    
    if test $status -eq 0
        echo "✅ iOS build complete!"
    else
        echo "❌ iOS build failed (may need code signing setup)"
    end
    echo ""
end

echo "🎉 Build job complete!"
echo ""
echo "🌐 View builds: https://builds.false.work"
