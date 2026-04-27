#!/usr/bin/env fish
# Aesthetic Computer Electron — Install latest published macOS release
# Pulls the signed/notarized DMG from releases.aesthetic.computer/desktop and
# installs Aesthetic.Computer.app into /Applications. No build required.
#
# Usage:
#   ./install.fish                # Download, install, launch
#   ./install.fish --no-launch    # Install without opening
#   ./install.fish --force        # Reinstall even if same version is present
#   ./install.fish --version=X.Y.Z   # Pin to a specific version

set -l RELEASES_URL "https://releases.aesthetic.computer/desktop"
set -l APP_NAME "Aesthetic.Computer.app"

set -l cyan (set_color cyan)
set -l green (set_color green)
set -l yellow (set_color yellow)
set -l red (set_color red)
set -l magenta (set_color magenta)
set -l normal (set_color normal)

set -l launch true
set -l force false
set -l pinned_version ""

for arg in $argv
    switch $arg
        case "--no-launch"
            set launch false
        case "--force"
            set force true
        case "--version=*"
            set pinned_version (string replace "--version=" "" $arg)
        case "--help" "-h"
            echo "Usage: install.fish [--no-launch] [--force] [--version=X.Y.Z]"
            exit 0
    end
end

if test (uname) != "Darwin"
    echo "$red✗ macOS only (this host is "(uname)")$normal"
    exit 1
end

echo "$magenta🎨 Aesthetic.Computer — installing latest desktop release$normal"
echo ""

# Resolve target version + DMG filename
# (note: fish reserves $version for the shell version, so we use $app_version)
set -l app_version
set -l dmg_file
if test -n "$pinned_version"
    set app_version $pinned_version
    set dmg_file "Aesthetic.Computer-$app_version-universal.dmg"
else
    echo "$cyan→ Fetching $RELEASES_URL/latest-mac.yml$normal"
    set -l yml (curl -fsSL "$RELEASES_URL/latest-mac.yml")
    if test -z "$yml"
        echo "$red✗ Could not reach release feed$normal"
        exit 1
    end
    set app_version (printf '%s\n' $yml | grep -E '^version:' | head -1 | string replace -r '^version:\s*' '')
    set dmg_file (printf '%s\n' $yml | grep -E 'url:\s*.+\.dmg' | head -1 | string replace -r '^\s*-?\s*url:\s*' '' | string trim)
    if test -z "$app_version" -o -z "$dmg_file"
        echo "$red✗ Could not parse latest-mac.yml$normal"
        printf '%s\n' $yml
        exit 1
    end
end

set -l dmg_url "$RELEASES_URL/$dmg_file"
echo "  version: $green$app_version$normal"
echo "  dmg:     $dmg_file"
echo ""

# Skip if same version is already installed
set -l plist "/Applications/$APP_NAME/Contents/Info.plist"
if test -f "$plist" -a "$force" != true
    set -l installed (defaults read "$plist" CFBundleShortVersionString 2>/dev/null)
    if test "$installed" = "$app_version"
        echo "$green✓ $APP_NAME $app_version is already installed$normal"
        if test "$launch" = true
            echo "$cyan▶ Launching...$normal"
            open -a "/Applications/$APP_NAME"
        end
        exit 0
    else if test -n "$installed"
        echo "$yellow… replacing installed version $installed with $app_version$normal"
    end
end

# Download
set -l tmp_dmg (mktemp -t ac-electron-install).dmg
echo "$cyan→ Downloading$normal"
if not curl -fL --progress-bar "$dmg_url" -o "$tmp_dmg"
    echo "$red✗ Download failed$normal"
    rm -f "$tmp_dmg"
    exit 1
end

# Mount
echo "$cyan→ Mounting$normal"
set -l mount_point (hdiutil attach -nobrowse -readonly "$tmp_dmg" | grep -oE '/Volumes/[^	]+' | tail -1 | string trim)
if test -z "$mount_point" -o ! -d "$mount_point"
    echo "$red✗ Could not mount DMG$normal"
    rm -f "$tmp_dmg"
    exit 1
end

# Find the .app bundle inside the volume (defend against rename)
set -l src_app "$mount_point/$APP_NAME"
if not test -d "$src_app"
    set src_app (find "$mount_point" -maxdepth 2 -name "*.app" -type d | head -1)
end
if test -z "$src_app" -o ! -d "$src_app"
    echo "$red✗ No .app bundle inside DMG$normal"
    hdiutil detach "$mount_point" -quiet
    rm -f "$tmp_dmg"
    exit 1
end

# Quit any running instance before replacing
osascript -e 'tell application "Aesthetic.Computer" to quit' >/dev/null 2>&1
pkill -f "Aesthetic.Computer" 2>/dev/null
sleep 0.5

# Install
echo "$cyan→ Installing to /Applications$normal"
if test -d "/Applications/$APP_NAME"
    rm -rf "/Applications/$APP_NAME"
end
cp -R "$src_app" "/Applications/$APP_NAME"

# Cleanup
hdiutil detach "$mount_point" -quiet
rm -f "$tmp_dmg"

# Strip quarantine flag so Gatekeeper opens it cleanly (app is already notarized)
xattr -dr com.apple.quarantine "/Applications/$APP_NAME" 2>/dev/null

echo ""
echo "$green✓ Installed Aesthetic.Computer $app_version → /Applications/$APP_NAME$normal"

if test "$launch" = true
    echo "$cyan▶ Launching$normal"
    open -a "/Applications/$APP_NAME"
end
