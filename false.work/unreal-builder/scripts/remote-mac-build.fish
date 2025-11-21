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
echo "Updating builds.false.work page..."
echo "========================================="

# Get file size in MB (rounded to whole number)
set file_size (math -s0 (stat -c%s /workspaces/aesthetic-computer/system/public/assets/false.work/$zip_name) / 1048576)
set download_url "https://assets.aesthetic.computer/false.work/$zip_name"
set iso_timestamp (date -Iseconds | cut -d'+' -f1)

# Extract start level from DefaultEngine.ini (get last part after dot)
set full_map (ssh_exec "grep 'GameDefaultMap=' ~/Perforce/spiderlily_build_workspace_macmini/SL_main/Config/DefaultEngine.ini | cut -d= -f2")
set start_level (echo $full_map | awk -F'.' '{print $NF}')

# Extract UE version from build script
set ue_version "UE_5.6"

# Use awk to replace latest build section (removes old, inserts new)
awk -v version="$build_version" -v timestamp="$iso_timestamp" -v size="$file_size" -v level="$start_level" -v ue_ver="$ue_version" -v url="$download_url" '
  /<!-- BUILD_LIST_ALL -->/ {
    print
    print "        <div class=\"build-header\">"
    print "          <span class=\"platform-tag platform-mac\">🍎 Mac</span>"
    print "          <span class=\"project-name\">SpiderLily</span>"
    print "          <a href=\"" url "\">" version ".zip</a>"
    print "          <span style=\"margin-left: 0.5rem; color: #666; font-size: 0.9rem;\">(<a href=\"https://assets.aesthetic.computer/false.work/spiderlily-mac-" version ".txt\" style=\"color: #888;\">download log</a>)</span>"
    print "        </div>"
    print "        <div class=\"meta\">" size " MB | " level " | " ue_ver " | <span class=\"build-time\" data-date=\"" timestamp "\">just now</span></div>"
    print "        <div class=\"build-log-preview animated\" id=\"build-log-preview\">"
    print "          <div class=\"log-header\">📜 Build Log</div>"
    print "          Loading build log..."
    print "        </div>"
    # Skip all content until we hit the next section header
    in_old_builds = 1
    next
  }
  in_old_builds && /<h2/ {
    in_old_builds = 0
  }
  !in_old_builds { print }
' /workspaces/aesthetic-computer/system/public/builds.false.work/index.html > /tmp/builds-temp.html
mv /tmp/builds-temp.html /workspaces/aesthetic-computer/system/public/builds.false.work/index.html

echo ""
echo "========================================="
echo "Committing and pushing to GitHub..."
echo "========================================="

cd /workspaces/aesthetic-computer
git add system/public/builds.false.work/index.html
git commit -m "Add SpiderLily Mac build $build_version"

# Try to push, if it fails due to remote changes, pull and retry
if not git push
    echo "Push failed, pulling remote changes and retrying..."
    
    # Check if there are uncommitted changes (excluding the file we just committed)
    set -l has_changes (git status --porcelain | grep -v "^M  system/public/builds.false.work/index.html" | wc -l)
    
    if test $has_changes -gt 0
        echo "⚠️  Warning: Uncommitted changes detected, stashing them..."
        git status --short
        git stash push -m "Build script auto-stash before rebase"
        echo "💾 Changes stashed - use 'git stash pop' to restore them after the build"
    end
    
    # Pull with rebase
    git pull --rebase
    # Push again
    git push
    
    echo "✅ Build pushed successfully"
end

echo ""
echo "✅ Build complete and deployed!"
echo "📦 Build: $build_version"
echo "🔗 Download: $download_url"
echo "🌐 Page: https://builds.false.work"
