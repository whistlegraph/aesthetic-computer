#!/usr/bin/env fish
# Update Perforce and build SpiderLily Mac from the dev container
# Follows same pattern as Windows remote-update-and-build.fish

set build_version (date +%Y.%m.%d-%H%M)
set log_file "/tmp/build-mac-$build_version.log"

echo "========================================="
echo "Update & Rebuild SpiderLily (Mac)"
echo "========================================="
echo ""
echo "Version: $build_version"
echo ""

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
    echo "❌ sshpass is not installed. Please rebuild the devcontainer or run:"
    echo "   sudo dnf install -y sshpass"
    exit 1
end

# Helper for SSH commands
function ssh_exec
    sshpass -p "$MAC_PASSWORD" ssh -o StrictHostKeyChecking=no "$MAC_USERNAME@$MAC_HOST" $argv
end

echo "========================================="
echo "Syncing build script to Mac..."
echo "========================================="
sshpass -p "$MAC_PASSWORD" scp -o StrictHostKeyChecking=no \
    /workspaces/aesthetic-computer/false.work/build-run-spiderlily.sh \
    "$MAC_USERNAME@$MAC_HOST:~/build-spiderlily.sh"
ssh_exec "chmod +x ~/build-spiderlily.sh"
echo ""

# Run build on Mac via SSH (streaming output to log file)
echo "========================================="
echo "Building on Mac..."
echo "========================================="
ssh_exec "~/build-spiderlily.sh $build_version 2>&1" | tee $log_file

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
sshpass -p "$MAC_PASSWORD" scp -o StrictHostKeyChecking=no \
    "$MAC_USERNAME@$MAC_HOST:~/$zip_name" \
    /workspaces/aesthetic-computer/system/public/assets/false.work/

# Copy log file to assets
cp $log_file /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-mac-$build_version.txt

echo ""
echo "========================================="
echo "Uploading to assets.aesthetic.computer..."
echo "========================================="

cd /workspaces/aesthetic-computer
npm run assets:sync:up

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

# Extract start level from DefaultEngine.ini (get last part after dot)
set full_map (ssh_exec "grep 'GameDefaultMap=' ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Config/DefaultEngine.ini | cut -d= -f2")
set start_level (echo $full_map | awk -F'.' '{print $NF}')

# Extract UE version
set ue_version "UE_5.6"

# Register build in MongoDB via Netlify function
source /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/shared/register-build.fish
register_build "mac" "$build_version" "$iso_timestamp" "$file_size" "$download_url" "$start_level" "$ue_version" "$log_url"

echo ""
echo "✅ Build complete and deployed!"
echo "📦 Build: $build_version"
echo "🔗 Download: $download_url"
echo "🌐 Page: https://builds.false.work"

