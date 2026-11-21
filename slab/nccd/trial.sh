#!/usr/bin/env bash
# trial.sh — a second copy of NCCd that runs on the Signal engine inside the
# app (engine/), beside the installed copy.
#
#   ./trial.sh           build it and put it in ~/Applications/NCCd Lib.app
#   ./trial.sh --run     and open it
#
# The copy is apart from NCCd in each place that matters:
#
#   - its own bundle identifier, so its Keychain items (the log key, the engine
#     key, the wiki session) are its own;
#   - its own data folder and config (NCC_CIRCLES_HOME, NCC_CIRCLES_CONFIG);
#   - its own name, "NCCd Lib", which is also the name of its linked device in
#     Signal on the phone;
#   - it does not update itself (the updater changes only a bundle named NCCd.app).
#
# To remove it: Unlink Signal in the copy, then delete the app,
# ~/Library/Application Support/NCC Circles Lib and ~/.config/ncc-circles-lib.
set -euo pipefail
cd "$(dirname "$0")"

[[ -f engine/lib/libnccd_engine.a ]] || { echo "no engine/lib/libnccd_engine.a: see engine/README.md" >&2; exit 1; }
./build.sh

DEST="$HOME/Applications/NCCd Lib.app"
PLIST="$DEST/Contents/Info.plist"
pkill -x "NCCd Lib" 2>/dev/null || true
rm -rf "$DEST"
ditto build/NCCd.app "$DEST"
mv "$DEST/Contents/MacOS/NCCd" "$DEST/Contents/MacOS/NCCd Lib"

pb() { /usr/libexec/PlistBuddy -c "$1" "$PLIST"; }
pb "Set :CFBundleIdentifier computer.aesthetic.ncc-circles.lib"
pb "Set :CFBundleDisplayName NCCd Lib"
pb "Set :CFBundleName NCCd Lib"
pb "Set :CFBundleExecutable NCCd Lib"
pb "Add :LSEnvironment dict"
pb "Add :LSEnvironment:NCC_ENGINE string lib"
pb "Add :LSEnvironment:NCC_CIRCLES_HOME string $HOME/Library/Application Support/NCC Circles Lib"
pb "Add :LSEnvironment:NCC_CIRCLES_CONFIG string $HOME/.config/ncc-circles-lib/config.json"

SIGN_ID="${DEV_ID:-$(security find-identity -v -p codesigning 2>/dev/null |
    sed -n 's/.*"\(Developer ID Application: [^"]*\)".*/\1/p' | head -1)}"
codesign --force --sign "${SIGN_ID:--}" "$DEST"
echo "✓ $DEST (${SIGN_ID:-ad-hoc})"
[[ "${1:-}" == "--run" ]] && open "$DEST"
exit 0
