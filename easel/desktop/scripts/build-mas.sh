#!/bin/zsh
set -euo pipefail
cd "${0:A:h}/.."
identity="${EASEL_MAS_IDENTITY:-Jeffrey Scudder (FB5948YR3S)}"
profile="${EASEL_MAS_PROFILE:-build/Easel-Mac-App-Store.provisionprofile}"
version="$(node -p "require('./package.json').version")"
[[ -f "$profile" ]] || { print -u2 "missing provisioning profile: $profile"; exit 1; }
# Apple rejects the upload (ITMS-90284) when the profile names a different
# certificate than the one signing, so check that before spending a build.
cert_hash="$(security find-identity -v -p codesigning | grep -F "3rd Party Mac Developer Application: $identity" | head -1 | awk '{print $2}')"
profile_hashes="$(security cms -D -i "$profile" 2>/dev/null | python3 -c 'import plistlib,sys,hashlib;print("\n".join(hashlib.sha1(c).hexdigest().upper() for c in plistlib.loads(sys.stdin.buffer.read())["DeveloperCertificates"]))')"
[[ -n "$cert_hash" && "$profile_hashes" == *"$cert_hash"* ]] || { print -u2 "profile $profile does not contain the signing certificate $cert_hash; download the profile that does (asc.mjs get /v1/profiles?filter[profileType]=MAC_APP_STORE&include=certificates)"; exit 1; }
# Match both application and installer identities by owner, not one cert hash.
# Skip the preliminary Darwin signature; the MAS pass signs with its profile.
env -u ELECTRON_RUN_AS_NODE ./node_modules/.bin/electron-builder \
  --mac mas --arm64 --publish never \
  -c.buildVersion="${EASEL_MAS_BUILD:-$version}" \
  -c.mac.identity=null \
  -c.mas.identity="$identity" \
  -c.mas.provisioningProfile="$profile"
app="dist/mas-arm64/Aesel.app"
pkg="dist/mas-arm64/aesel-$version-mas-arm64.pkg"
codesign --verify --deep --strict "$app"
pkgutil --check-signature "$pkg"
print "Verified $pkg"
