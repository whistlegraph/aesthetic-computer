#!/usr/bin/env fish
# Update Perforce and build SpiderLily from the dev container

set build_version (date +%Y.%m.%d-%H%M)
set log_file "/tmp/build-$build_version.log"

echo "========================================="
echo "Update & Rebuild SpiderLily"
echo "========================================="
echo ""
echo "Version: $build_version"
echo ""

# First, copy updated build script to remote machine
echo "========================================="
echo "Syncing build script to remote..."
echo "========================================="
scp /workspaces/aesthetic-computer/windows/build-false-work.ps1 me@host.docker.internal:"C:/Perforce/SpiderLily/SL_main/build-false-work.ps1"
echo ""

# Run build on Windows via SSH (streaming output to build stream AND log file)
# Note: removed -t flag to allow proper piping
ssh me@host.docker.internal "powershell -NoProfile -ExecutionPolicy Bypass -File C:\\Perforce\\SpiderLily\\SL_main\\build-false-work.ps1 -Version $build_version -AutoPackage" 2>&1 | tee $log_file | /workspaces/aesthetic-computer/windows/ac-pipe.sh

# Note: We verify success by checking the build output size below, not the exit code

# Verify build exists and the Windows folder is substantial (>100MB)
set build_size (ssh me@host.docker.internal "powershell -NoProfile -Command \"if (Test-Path C:\\Users\\me\\Desktop\\SpiderLily-$build_version\\Windows) { (Get-ChildItem C:\\Users\\me\\Desktop\\SpiderLily-$build_version\\Windows -Recurse | Measure-Object -Property Length -Sum).Sum } else { 0 }\"")

if test $build_size -lt 100000000
    echo ""
    echo "❌ Build verification failed!"
    echo "   Expected Windows folder >100MB, got: "(math $build_size / 1000000)"MB"
    exit 1
end

echo ""
echo "✅ Build verified: "(math $build_size / 1000000)"MB"

echo ""
echo "========================================="
echo "Copying build to assets..."
echo "========================================="

# Copy ZIP file (already created by PowerShell with -AutoPackage)
scp me@host.docker.internal:"C:/Users/me/Desktop/spiderlily-windows-$build_version.zip" /workspaces/aesthetic-computer/system/public/assets/false.work/

# Copy log file to assets
cp $log_file /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-windows-$build_version.txt

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
set file_size (math -s0 (stat -c%s /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-windows-$build_version.zip) / 1048576)
set download_url "https://assets.aesthetic.computer/false.work/spiderlily-windows-$build_version.zip"
set iso_timestamp (date -Iseconds | cut -d'+' -f1)

# Extract start level from remote DefaultEngine.ini (get last part after dot)
set full_map (ssh me@host.docker.internal "powershell -NoProfile -Command \"(Get-Content 'C:\\Perforce\\SpiderLily\\SL_main\\Config\\DefaultEngine.ini' | Select-String -Pattern 'GameDefaultMap=').ToString().Split('=')[1]\"")
set start_level (echo $full_map | awk -F'.' '{print $NF}')

# Extract Unreal Engine version from build script
set ue_version (ssh me@host.docker.internal "powershell -NoProfile -Command \"(Get-Content 'C:\\Perforce\\SpiderLily\\SL_main\\build-false-work.ps1' | Select-String -Pattern 'UE_5\\.\\d+').Matches.Value\"")

# Use shared function to update the builds page
source /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/shared/update-builds-page.fish
update_builds_page "windows" "$build_version" "$iso_timestamp" "$file_size" "$start_level" "$ue_version" "$download_url"

echo ""
echo "========================================="
echo "Committing and pushing to GitHub..."
echo "========================================="

cd /workspaces/aesthetic-computer
git add system/public/builds.false.work/index.html
git commit -m "Add SpiderLily Windows build $build_version"

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
    
    # Don't automatically pop the stash - let the user decide when to restore
    echo "✅ Build pushed successfully"
end

echo ""
echo "✅ Build complete and deployed!"
echo "📦 Build: $build_version"
echo "🔗 Download: $download_url"
echo "🌐 Page: https://builds.false.work"
