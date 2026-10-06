#!/usr/bin/env bash
# Sign a poorslice CODE_SIGNING_ALLOWED=NO build using this Mac's identity.
set -euo pipefail
if [[ $# != 4 ]]; then
  echo 'Usage: bash sign-device.sh <Whistlegraph.app> <identity> <profile> <entitlements>' >&2
  exit 1
fi
app=${1%/}
identity=$2
profile=$3
entitlements=$4
[[ -d "$app" && -f "$app/Info.plist" && -f "$profile" && -f "$entitlements" ]]
cp "$profile" "$app/embedded.mobileprovision"
# Xcode's Debug executable loads the application code from a separate dylib.
# Signing only the .app seals that file without signing its executable code.
while IFS= read -r library; do
  codesign --force --sign "$identity" "$library"
done < <(rg --files "$app" -g '*.dylib')
codesign --force --sign "$identity" --entitlements "$entitlements" --generate-entitlement-der "$app"
codesign --verify --deep --strict --verbose=2 "$app"
