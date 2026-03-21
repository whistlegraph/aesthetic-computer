#!/usr/bin/env fish
# Remote Mac Build Script with Multi-Host Support
# Builds SpiderLily on either Jeffrey's MacBook or Mac Mini
#
# Usage:
#   ./remote-mac-build-multihost.fish                    # Build on default host (macbook)
#   ./remote-mac-build-multihost.fish --target=macmini   # Build on Mac Mini
#   ./remote-mac-build-multihost.fish --target=macbook   # Build on Jeffrey's MacBook
#   ./remote-mac-build-multihost.fish --no-sync          # Skip Perforce sync
#
# The default target is 'macbook' when running from Jeffrey's MacBook devcontainer
# (host.docker.internal resolves to the host machine)

set build_version (date +%Y.%m.%d-%H%M)
set log_file "/tmp/build-mac-$build_version.log"
set skip_sync false
set target "macbook"

# Parse arguments
for arg in $argv
    switch $arg
        case '--target=*'
            set target (string replace '--target=' '' $arg)
        case '--macmini'
            set target "macmini"
        case '--macbook'
            set target "macbook"
        case '--no-sync' '--skip-sync'
            set skip_sync true
    end
end

echo "========================================="
echo "Update & Rebuild SpiderLily (Mac)"
echo "========================================="
echo ""
echo "Version:    $build_version"
echo "Target:     $target"
echo "Skip Sync:  $skip_sync"
echo ""

# Load Mac credentials
set VAULT_DIR /workspaces/aesthetic-computer/aesthetic-computer-vault/false.work
set MAC_CREDS $VAULT_DIR/mac-builder-credentials.env

if not test -f $MAC_CREDS
    echo "❌ Mac credentials not found: $MAC_CREDS"
    exit 1
end

# Load target-specific credentials
switch $target
    case 'macmini'
        set MAC_HOST (grep '^MACMINI_HOST=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
        set MAC_USERNAME (grep '^MACMINI_USERNAME=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
        set MAC_PASSWORD (grep '^MACMINI_PASSWORD=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
        set MAC_P4_WORKSPACE (grep '^MACMINI_P4_WORKSPACE=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
        set MAC_PROJECT_ROOT (grep '^MACMINI_PROJECT_ROOT=' $MAC_CREDS | cut -d= -f2 | tr -d '"' | sed 's/$HOME/~/')
    case 'macbook' '*'
        set MAC_HOST (grep '^MACBOOK_HOST=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
        set MAC_USERNAME (grep '^MACBOOK_USERNAME=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
        set MAC_PASSWORD (grep '^MACBOOK_PASSWORD=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
        set MAC_P4_WORKSPACE (grep '^MACBOOK_P4_WORKSPACE=' $MAC_CREDS | cut -d= -f2 | tr -d '"')
        set MAC_PROJECT_ROOT (grep '^MACBOOK_PROJECT_ROOT=' $MAC_CREDS | cut -d= -f2 | tr -d '"' | sed 's/$HOME/~/')
end

echo "Host:       $MAC_HOST"
echo "Username:   $MAC_USERNAME"
echo "Workspace:  $MAC_P4_WORKSPACE"
echo ""

# Check if sshpass is installed
if not command -v sshpass >/dev/null
    echo "❌ sshpass is not installed. Please rebuild the devcontainer or run:"
    echo "   sudo dnf install -y sshpass"
    exit 1
end

# Helper for SSH commands
function ssh_exec
    sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no "$MAC_USERNAME@$MAC_HOST" $argv
end

function scp_to_mac
    sshpass -p "$MAC_PASSWORD" scp -o StrictHostKeyChecking=no $argv[1] "$MAC_USERNAME@$MAC_HOST:$argv[2]"
end

function scp_from_mac
    sshpass -p "$MAC_PASSWORD" scp -o StrictHostKeyChecking=no "$MAC_USERNAME@$MAC_HOST:$argv[1]" $argv[2]
end

# Test connection
echo "🔌 Testing connection to $target..."
if not ssh_exec "echo 'Connected successfully'"
    echo "❌ Failed to connect to $MAC_HOST"
    echo "   If using macbook, make sure you're running from the devcontainer on that machine"
    exit 1
end
echo ""

echo "========================================="
echo "Syncing build script to Mac..."
echo "========================================="

# Create a customized build script for this host
set build_script_content "#!/bin/bash
# Generated build script for $target
# Build version: $build_version
# P4 Workspace: $MAC_P4_WORKSPACE

set -e

BUILD_VERSION=\"$build_version\"
SKIP_SYNC=$skip_sync
P4_WORKSPACE=\"$MAC_P4_WORKSPACE\"

# Mac paths - adjust based on target
PROJECT_ROOT=\"\$HOME/Perforce/$MAC_P4_WORKSPACE/SL_main\"
PROJECT_FILE=\"\$PROJECT_ROOT/SpiderLily.uproject\"
UE_ROOT=\"/Users/Shared/Epic Games/UE_5.6\"
OUTPUT_DIR=\"\$HOME/Builds/\$BUILD_VERSION\"

echo \"=========================================\"
echo \"SpiderLily - Mac Build ($target)\"
echo \"=========================================\"
echo \"Version: \$BUILD_VERSION\"
echo \"Skip Sync: \$SKIP_SYNC\"
echo \"P4 Workspace: \$P4_WORKSPACE\"
echo \"\"

# Verify paths
if [ ! -f \"\$PROJECT_FILE\" ]; then
    echo \"ERROR: Project not found: \$PROJECT_FILE\"
    exit 1
fi

if [ ! -d \"\$UE_ROOT\" ]; then
    echo \"ERROR: Unreal Engine not found: \$UE_ROOT\"
    exit 1
fi

# Sync latest from Perforce (unless --no-sync)
if [ \"\$SKIP_SYNC\" = \"false\" ]; then
    echo \"📥 Syncing latest from Perforce...\"
    cd \"\$PROJECT_ROOT\"
    export P4PORT=ssl:falsework.helixcore.io:1666
    export P4USER=machine
    export P4CLIENT=\$P4_WORKSPACE

    /opt/homebrew/bin/p4 sync || echo \"⚠️  P4 sync failed, continuing with existing files...\"
else
    echo \"⏭️  Skipping Perforce sync (--no-sync flag)\"
    cd \"\$PROJECT_ROOT\"
fi

# Fix FMOD compilation bug (uninitialized variable)
echo \"\"
echo \"🔧 Applying FMOD plugin fix...\"
sed -i.bak 's/FMOD_STUDIO_PLAYBACK_STATE pS;/FMOD_STUDIO_PLAYBACK_STATE pS = FMOD_STUDIO_PLAYBACK_STOPPED;/' \\
    \"\$PROJECT_ROOT/Plugins/FMODStudio/Source/FMODStudio/Private/FMODBlueprintStatics.cpp\" 2>/dev/null || echo \"  (already applied)\"

echo \"\"
echo \"🔨 Building and cooking with BuildCookRun...\"
echo \"   This will take several minutes...\"
echo \"\"

# Clean old builds
rm -rf \"\$OUTPUT_DIR\"
rm -rf \"\$PROJECT_ROOT/Saved/Cooked/Mac\"
rm -rf \"\$PROJECT_ROOT/Saved/Shaders\"
rm -rf \"\$PROJECT_ROOT/Saved/StagedBuilds\"

# Use BuildCookRun for complete packaging
\"\$UE_ROOT/Engine/Build/BatchFiles/RunUAT.sh\" BuildCookRun \\
    -project=\"\$PROJECT_FILE\" \\
    -platform=Mac \\
    -clientconfig=Development \\
    -cook \\
    -build \\
    -stage \\
    -pak \\
    -noP4 \\
    -utf8output

if [ \$? -ne 0 ]; then
    echo \"\"
    echo \"❌ Build failed\"
    exit 1
fi

# Manually copy staged build to output
echo \"\"
echo \"📦 Copying staged build to archive...\"
mkdir -p \"\$OUTPUT_DIR\"
cp -R \"\$PROJECT_ROOT/Saved/StagedBuilds/Mac\" \"\$OUTPUT_DIR/\"

echo \"\"
echo \"✅ Build complete!\"
echo \"📦 Output: \$OUTPUT_DIR/Mac\"

BUILD_SIZE=\$(du -sh \"\$OUTPUT_DIR/Mac\" 2>/dev/null | cut -f1)
echo \"📊 Size: \$BUILD_SIZE\"
"

# Write the script to a temp file and upload
echo "$build_script_content" > /tmp/build-spiderlily-$target.sh
scp_to_mac /tmp/build-spiderlily-$target.sh "~/build-spiderlily.sh"
ssh_exec "chmod +x ~/build-spiderlily.sh"
echo ""

# Run build on Mac via SSH (streaming output to log file)
echo "========================================="
echo "Building on Mac ($target)..."
echo "========================================="
ssh_exec "~/build-spiderlily.sh 2>&1" | tee $log_file

# Verify build exists and is substantial (>100MB)
set build_size (ssh_exec "du -sk ~/Builds/$build_version/Mac 2>/dev/null | cut -f1")

if test -z "$build_size" -o "$build_size" -lt 100000
    echo ""
    echo "❌ Build verification failed!"
    echo "   Expected Mac folder >100MB, got: $build_size KB"
    exit 1
end

echo ""
echo "✅ Build verified: "(math $build_size / 1024)" MB"

echo ""
echo "========================================="
echo "Copying build to assets..."
echo "========================================="

# Create ZIP on Mac if it doesn't exist
set zip_name "spiderlily-mac-$build_version.zip"
ssh_exec "cd ~/Builds/$build_version && zip -r -q ~/$zip_name Mac/"

# Copy ZIP to local assets
scp_from_mac "~/$zip_name" /workspaces/aesthetic-computer/system/public/assets/false.work/

# Copy log file to assets
cp $log_file /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-mac-$build_version.txt

echo ""
echo "========================================="
echo "Uploading to assets.aesthetic.computer..."
echo "========================================="

# Direct S3 upload for build files
set local_zip /workspaces/aesthetic-computer/system/public/assets/false.work/$zip_name
set local_log /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-mac-$build_version.txt
set s3_endpoint "https://sfo3.digitaloceanspaces.com"
set s3_bucket "s3://assets-aesthetic-computer/false.work"

aws s3 cp $local_zip $s3_bucket/$zip_name --endpoint-url $s3_endpoint --acl public-read
aws s3 cp $local_log $s3_bucket/spiderlily-mac-$build_version.txt --endpoint-url $s3_endpoint --acl public-read

echo ""
echo "========================================="
echo "Registering build in database..."
echo "========================================="

# Ensure the file exists before calculating size
if not test -f /workspaces/aesthetic-computer/system/public/assets/false.work/$zip_name
    echo "❌ Build ZIP not found at /workspaces/aesthetic-computer/system/public/assets/false.work/$zip_name"
    exit 1
end

# Get file size in MB (rounded to whole number)
set file_size (math -s0 (stat -c%s /workspaces/aesthetic-computer/system/public/assets/false.work/$zip_name) / 1048576)
set download_url "https://assets.aesthetic.computer/false.work/$zip_name"
set log_url "https://assets.aesthetic.computer/false.work/spiderlily-mac-$build_version.txt"
set iso_timestamp (date -Iseconds)

# Extract start level from DefaultEngine.ini
set full_map (ssh_exec "grep 'GameDefaultMap=' ~/Perforce/$MAC_P4_WORKSPACE/SL_main/Config/DefaultEngine.ini | cut -d= -f2")
set start_level (echo $full_map | awk -F'.' '{print $NF}')

# Extract UE version
set ue_version "UE_5.6"

# Register build in MongoDB via Netlify function
source /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/shared/register-build.fish
register_build "mac" "$build_version" "$iso_timestamp" "$file_size" "$download_url" "$start_level" "$ue_version" "$log_url"

echo ""
echo "✅ Build complete and deployed!"
echo "📦 Build: $build_version"
echo "🖥️  Built on: $target"
echo "🔗 Download: $download_url"
echo "🌐 Page: https://builds.false.work"
