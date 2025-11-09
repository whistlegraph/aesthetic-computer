#!/usr/bin/env fish
# Update Perforce and build SpiderLily from the dev container

set build_version (date +%Y.%m.%d-%H%M)

echo "========================================="
echo "Update & Rebuild SpiderLily"
echo "========================================="
echo ""
echo "Version: $build_version"
echo ""

# Run build on Windows via SSH (streaming output to build stream)
# Note: removed -t flag to allow proper piping
ssh me@host.docker.internal "powershell -NoProfile -ExecutionPolicy Bypass -Command \"cd C:\\Perforce\\SpiderLily\\SL_main; p4 sync ...; p4 changes -m 5 ...; .\\build-false-work.ps1 -Version $build_version\"" 2>&1 | /workspaces/aesthetic-computer/windows/ac-pipe.sh

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

# Compress build on Windows
ssh me@host.docker.internal "powershell -NoProfile -Command \"cd C:\\Users\\me\\Desktop; if (Test-Path 'SpiderLily-$build_version\\Windows') { Compress-Archive -Path 'SpiderLily-$build_version\\Windows\\*' -DestinationPath 'spiderlily-windows-$build_version.zip' -Force; echo 'Compressed successfully' }\""

# Copy ZIP file using SCP
scp me@host.docker.internal:"C:/Users/me/Desktop/spiderlily-windows-$build_version.zip" /workspaces/aesthetic-computer/system/public/assets/false.work/

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

# Get file size in MB
set file_size (math (stat -c%s /workspaces/aesthetic-computer/system/public/assets/false.work/spiderlily-windows-$build_version.zip) / 1000000)
set download_url "https://assets.aesthetic.computer/false.work/spiderlily-windows-$build_version.zip"
set iso_timestamp (date -Iseconds | cut -d'+' -f1)

# Extract start level from remote DefaultEngine.ini
set start_level (ssh me@host.docker.internal "powershell -NoProfile -Command \"(Get-Content 'C:\\Perforce\\SpiderLily\\SL_main\\Config\\DefaultEngine.ini' | Select-String -Pattern 'GameDefaultMap=').ToString().Split('/')[-1].Replace('.umap','')\"")

# Use awk to insert new build entry (more reliable than sed with multiline)
awk -v version="$build_version" -v url="$download_url" -v size="$file_size" -v level="$start_level" -v timestamp="$iso_timestamp" '
  /<!-- BUILD_LIST_windows -->/ {
    print
    print "        <li>"
    print "          <a href=\"" url "\">" version "</a>"
    print "          <div class=\"meta\">" size " MB | " level " | <span class=\"build-time\" data-date=\"" timestamp "\">just now</span></div>"
    print "        </li>"
    next
  }
  { print }
' /workspaces/aesthetic-computer/system/public/builds.false.work/index.html > /tmp/builds-temp.html
mv /tmp/builds-temp.html /workspaces/aesthetic-computer/system/public/builds.false.work/index.html

echo ""
echo "========================================="
echo "Committing and pushing to GitHub..."
echo "========================================="

cd /workspaces/aesthetic-computer
git add system/public/builds.false.work/index.html
git commit -m "Add SpiderLily Windows build $build_version"
git push

echo ""
echo "✅ Build complete and deployed!"
echo "📦 Build: $build_version"
echo "🔗 Download: $download_url"
echo "🌐 Page: https://builds.false.work"
