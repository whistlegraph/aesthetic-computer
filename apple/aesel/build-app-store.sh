#!/usr/bin/env bash
# Build the Swift app for the existing Aesel Mac App Store record.
set -euo pipefail
cd "$(dirname "$0")"
: "${AESEL_MAS_PROFILE:?Set AESEL_MAS_PROFILE to a Mac App Store provisioning profile}"
version=$(/usr/libexec/PlistBuddy -c Print:CFBundleShortVersionString MacInfo.plist)
output=${AESEL_MAS_OUTPUT:-"$HOME/Library/Developer/Aesel Releases/$version"}
app_identity=${AESEL_MAS_APP_IDENTITY:-"3rd Party Mac Developer Application: Jeffrey Scudder (FB5948YR3S)"}
installer_identity=${AESEL_MAS_INSTALLER_IDENTITY:-"3rd Party Mac Developer Installer: Jeffrey Scudder (FB5948YR3S)"}
mkdir -p "$output"
security cms -D -i "$AESEL_MAS_PROFILE" > "$output/profile.plist"
profile_uuid=$(/usr/libexec/PlistBuddy -c Print:UUID "$output/profile.plist")
profile_name=$(/usr/libexec/PlistBuddy -c Print:Name "$output/profile.plist")
profile_app=$(/usr/libexec/PlistBuddy -c Print:Entitlements:com.apple.application-identifier "$output/profile.plist")
[[ "$profile_app" == "FB5948YR3S.computer.aesthetic.easel" ]] || { echo 'Wrong App Store profile' >&2; exit 1; }
mkdir -p "$HOME/Library/Developer/Xcode/UserData/Provisioning Profiles"
cp "$AESEL_MAS_PROFILE" "$HOME/Library/Developer/Xcode/UserData/Provisioning Profiles/$profile_uuid.provisionprofile"
chmod 644 "$HOME/Library/Developer/Xcode/UserData/Provisioning Profiles/$profile_uuid.provisionprofile"
./bundle-session.sh
xcodegen generate
xcodebuild -project Aesel.xcodeproj -scheme AeselMac -configuration Release \
  -destination 'generic/platform=macOS' -archivePath "$output/Aesel.xcarchive" \
  -derivedDataPath "$output/DerivedData" -jobs 2 \
  CODE_SIGN_STYLE=Manual CODE_SIGN_IDENTITY="$app_identity" \
  PROVISIONING_PROFILE_SPECIFIER="$profile_name" \
  ONLY_ACTIVE_ARCH=NO 'ARCHS=arm64 x86_64' ENABLE_DEBUG_DYLIB=NO archive
app="$output/Aesel.xcarchive/Products/Applications/Aesel.app"
[[ ! -d "$app/Contents/Frameworks/Electron Framework.framework" ]]
[[ "$(/usr/libexec/PlistBuddy -c Print:CFBundleIdentifier "$app/Contents/Info.plist")" == computer.aesthetic.easel ]]
codesign --verify --deep --strict "$app"
lipo "$app/Contents/MacOS/Aesel" -verify_arch arm64
lipo "$app/Contents/MacOS/Aesel" -verify_arch x86_64
pkg="$output/Aesel-$version.pkg"
productbuild --component "$app" /Applications --sign "$installer_identity" "$pkg"
pkgutil --check-signature "$pkg"
printf 'Verified native App Store package: %s\n' "$pkg"
