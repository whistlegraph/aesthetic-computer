#!/bin/zsh
set -euo pipefail
cd "${0:A:h}/.."
identity="${EASEL_MAS_IDENTITY:-Jeffrey Scudder (FB5948YR3S)}"
profile="${EASEL_MAS_PROFILE:-build/Easel-Mac-App-Store.provisionprofile}"
[[ -f "$profile" ]] || { print -u2 "missing provisioning profile: $profile"; exit 1; }
# Match both application and installer identities by owner, not one cert hash.
# Skip the preliminary Darwin signature; the MAS pass signs with its profile.
env -u ELECTRON_RUN_AS_NODE ./node_modules/.bin/electron-builder \
  --mac mas --arm64 --publish never \
  -c.buildVersion="${EASEL_MAS_BUILD:-0.7.3}" \
  -c.mac.identity=null \
  -c.mas.identity="$identity" \
  -c.mas.provisioningProfile="$profile"
app="dist/mas-arm64/aesel.app"
pkg="dist/mas-arm64/aesel-$(node -p "require('./package.json').version")-mas-arm64.pkg"
codesign --verify --deep --strict "$app"
pkgutil --check-signature "$pkg"
print "Verified $pkg"
