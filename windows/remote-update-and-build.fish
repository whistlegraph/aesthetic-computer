#!/usr/bin/env fish
# Update Perforce and build SpiderLily from the dev container
# Usage: remote-update-and-build.fish [quality]
#   quality: auto (default), ultra, high, medium

set quality $argv[1]
if test -z "$quality"
    set quality "auto"
end

# Validate quality
if not contains $quality auto ultra high medium
    echo "❌ Invalid quality: $quality"
    echo "   Valid options: auto, ultra, high, medium"
    exit 1
end

set quality_suffix ""
if test "$quality" != "auto"
    set quality_suffix "-$quality"
end

set build_version (date +%Y.%m.%d-%H%M)
set log_file "/tmp/build-$build_version$quality_suffix.log"

echo "========================================="
echo "Update & Rebuild SpiderLily"
echo "========================================="
echo ""
echo "Version: $build_version"
echo "Quality: $quality"
echo ""

# First, copy updated build script to remote machine
echo "========================================="
echo "Syncing build script to remote..."
echo "========================================="
scp /workspaces/aesthetic-computer/windows/build-false-work.ps1 me@host.docker.internal:"C:/Perforce/SpiderLily/SL_main/build-false-work.ps1"
echo ""

# Run build on Windows via SSH (streaming output to build stream AND log file)
# Note: removed -t flag to allow proper piping
ssh me@host.docker.internal "powershell -NoProfile -ExecutionPolicy Bypass -File C:\\Perforce\\SpiderLily\\SL_main\\build-false-work.ps1 -Version $build_version -Quality $quality -AutoPackage" 2>&1 | tee $log_file | /workspaces/aesthetic-computer/windows/ac-pipe.sh

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
scp me@host.docker.internal:"C:/Users/me/Desktop/spiderlily-windows-$build_version$quality_suffix.zip" /workspaces/aesthetic-computer/system/public/assets/false.work/

# Copy log file to assets
cp $log_file /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-windows-$build_version$quality_suffix.txt

echo ""
echo "========================================="
echo "Uploading to assets.aesthetic.computer..."
echo "========================================="

# Direct S3 upload for build files (npm run assets:sync:up excludes large zips)
set local_zip /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-windows-$build_version$quality_suffix.zip
set local_log /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-windows-$build_version$quality_suffix.txt
set s3_endpoint "https://sfo3.digitaloceanspaces.com"
set s3_bucket "s3://assets-aesthetic-computer/false.work"

aws s3 cp $local_zip $s3_bucket/spiderlily-windows-$build_version$quality_suffix.zip --endpoint-url $s3_endpoint --acl public-read
aws s3 cp $local_log $s3_bucket/spiderlily-windows-$build_version$quality_suffix.txt --endpoint-url $s3_endpoint --acl public-read

echo ""
echo "========================================="
echo "Registering build in database..."
echo "========================================="

# Get file size in MB (rounded to whole number)
set file_size (math -s0 (stat -c%s /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-windows-$build_version$quality_suffix.zip) / 1048576)
set download_url "https://assets.aesthetic.computer/false.work/spiderlily-windows-$build_version$quality_suffix.zip"
set log_url "https://assets.aesthetic.computer/false.work/spiderlily-windows-$build_version$quality_suffix.txt"
set iso_timestamp (date -Iseconds)

# Extract start level from remote DefaultEngine.ini (get last part after dot)
set full_map (ssh me@host.docker.internal "powershell -NoProfile -Command \"(Get-Content 'C:\\Perforce\\SpiderLily\\SL_main\\Config\\DefaultEngine.ini' | Select-String -Pattern 'GameDefaultMap=').ToString().Split('=')[1]\"")
set start_level (echo $full_map | awk -F'.' '{print $NF}')

# UE version - hardcoded for now since extraction is complex
set ue_version "UE_5.6"

# Build tags based on quality setting
set build_tags ""
switch $quality
    case ultra
        set build_tags "ultra,cinematic"
    case high
        set build_tags "high,epic"
    case medium
        set build_tags "medium"
    case auto
        set build_tags "auto-detect"
end

# Register build in MongoDB via Netlify function
source /workspaces/aesthetic-computer/false.work/unreal-builder/scripts/shared/register-build.fish
register_build "windows" "$build_version$quality_suffix" "$iso_timestamp" "$file_size" "$download_url" "$start_level" "$ue_version" "$log_url" "" "" "$build_tags"

echo ""
echo "✅ Build complete and deployed!"
echo "📦 Build: $build_version$quality_suffix"
echo "🎨 Quality: $quality"
echo "🔗 Download: $download_url"
echo "🌐 Page: https://builds.false.work"

