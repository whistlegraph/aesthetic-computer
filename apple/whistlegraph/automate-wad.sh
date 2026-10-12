#!/bin/bash
# Submit one real WAD request through Whistlegraph's iPhone UI.
set -euo pipefail
device=${1:?Usage: automate-wad.sh DEVICE_UDID [PROMPT]}
prompt=${2:-"Make a lollipop world with custom candy textures and sprites, connected rooms and stairs."}
repo=$(cd "$(dirname "$0")/../.." && pwd)
derived=${AC_WAD_AUTOMATION_DERIVED_DATA:-/tmp/whistlegraph-doom-test-dd}
cd "$repo"
# Fail promptly if the phone is locked or unavailable.
xcrun devicectl device process launch --device "$device" computer.aesthetic.walkieware
xcodebuild -project apple/whistlegraph/Whistlegraph.xcodeproj -scheme WhistlegraphUITests \
  -configuration Debug -destination "id=$device" -derivedDataPath "$derived" \
  -allowProvisioningUpdates -jobs 2 build-for-testing
run=$(python3 - "$derived/Build/Products" "$prompt" <<'PY'
import pathlib, plistlib, sys
products = pathlib.Path(sys.argv[1])
candidates = sorted(products.glob('WhistlegraphUITests_iphoneos*.xctestrun'), key=lambda p: p.stat().st_mtime)
if not candidates:
    raise SystemExit('Missing built iPhone test runner')
data = plistlib.loads(candidates[-1].read_bytes())
for key, target in data.items():
    if key.startswith('__') or not isinstance(target, dict):
        continue
    target.setdefault('EnvironmentVariables', {})['WHISTLEGRAPH_LIVE_WAD_AUTOMATION'] = '1'
    target.setdefault('TestingEnvironmentVariables', {})['WHISTLEGRAPH_LIVE_WAD_AUTOMATION'] = '1'
    for key in ['EnvironmentVariables', 'TestingEnvironmentVariables']:
        target[key]['WHISTLEGRAPH_LIVE_WAD_PROMPT'] = sys.argv[2]
# Keep __TESTROOT__ relative paths beside the original runner.
output = products / 'WhistlegraphLiveWad.xctestrun'
output.write_bytes(plistlib.dumps(data))
print(output)
PY
)
xcodebuild test-without-building -xctestrun "$run" -destination "id=$device" \
  -only-testing:WhistlegraphUITests/WaresTests/testGenerateWadThroughLiveAccount \
  -parallel-testing-enabled NO -collect-test-diagnostics never -resultBundlePath "/tmp/whistlegraph-live-wad-$(date +%Y%m%d-%H%M%S).xcresult"
# XCTest closes its app session; leave the resulting WAD open.
xcrun devicectl device process launch --device "$device" computer.aesthetic.walkieware
