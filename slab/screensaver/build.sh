#!/usr/bin/env bash
# build.sh — compile and install the Slab status screensaver.
#
# Produces a loadable `.saver` bundle with swiftc directly (no Xcode
# project needed — Command Line Tools is enough) and drops it into
# ~/Library/Screen Savers/. Idempotent; safe to re-run after edits.
#
#   ./build.sh            build + install
#   ./build.sh --build    build only (assemble bundle, don't install)

set -euo pipefail

CYAN=$'\033[1;36m'; GREEN=$'\033[1;32m'; YELLOW=$'\033[1;33m'; RESET=$'\033[0m'
say() { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()  { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
warn(){ printf "%s! %s%s\n" "$YELLOW" "$1" "$RESET"; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

command -v swiftc >/dev/null 2>&1 || {
    echo "swiftc not found — install Command Line Tools: xcode-select --install"
    exit 1
}

SDK="$(xcrun --sdk macosx --show-sdk-path)"
ARCH="$(uname -m)"
TARGET="${ARCH}-apple-macos13.0"
BUILD="$SCRIPT_DIR/.build"
SAVER="$BUILD/Slab.saver"
MACOS_DIR="$SAVER/Contents/MacOS"
RES_DIR="$SAVER/Contents/Resources"

say "compiling Slab.saver ($ARCH, swift $(swiftc --version | head -1 | awk '{print $4}'))"
rm -rf "$SAVER"
mkdir -p "$MACOS_DIR" "$RES_DIR"

# `-emit-library` puts swiftc in library-link mode (no implicit main);
# `-Xlinker -bundle` flips the Mach-O type from dylib to MH_BUNDLE, which
# is what NSBundle's principalClass loader expects from a `.saver`.
# `-swift-version 5` relaxes Swift-6 strict concurrency for the AppKit
# overrides.
swiftc \
    -emit-library \
    -o "$MACOS_DIR/Slab" \
    -module-name Slab \
    -swift-version 5 \
    -target "$TARGET" \
    -sdk "$SDK" \
    -O \
    -framework ScreenSaver \
    -framework AppKit \
    -framework Foundation \
    -Xlinker -bundle \
    Sources/*.swift

if ! file "$MACOS_DIR/Slab" | grep -q "bundle"; then
    warn "expected a Mach-O bundle; got: $(file "$MACOS_DIR/Slab")"
fi

cp "$SCRIPT_DIR/Info.plist" "$SAVER/Contents/Info.plist"
ok "bundle assembled: $SAVER"

# Ad-hoc signing — sufficient for a locally built saver in ~/Library.
codesign --force --deep --sign - \
    --identifier computer.slab.screensaver \
    "$SAVER" >/dev/null 2>&1 \
    && ok "ad-hoc signed" \
    || warn "codesign failed (saver may still load locally)"

if [[ "${1:-}" == "--build" ]]; then
    printf "\n%sbuilt only.%s  bundle: %s\n" "$GREEN" "$RESET" "$SAVER"
    exit 0
fi

DEST_DIR="$HOME/Library/Screen Savers"
DEST="$DEST_DIR/Slab.saver"
say "installing → $DEST"
mkdir -p "$DEST_DIR"

# legacyScreenSaver caches loaded bundles; killing it forces a fresh load
# next time the screensaver engages or the System Settings pane opens.
rm -rf "$DEST"
cp -R "$SAVER" "$DEST"
ok "installed"

pkill -x legacyScreenSaver 2>/dev/null || true
killall ScreenSaverEngine 2>/dev/null || true

printf "\n%sdone.%s\n" "$GREEN" "$RESET"
echo "  bundle:  $DEST"
echo
echo "  Select it:  System Settings → Screen Saver → Other → \"Slab Status\""
echo "  (it appears under the 'Other' / non-Apple group)"
echo "  Preview now: open the Screen Saver settings pane and pick it."
