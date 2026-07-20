#!/bin/sh
# Build and install a self-contained JukeWizard CLI bundle for this Mac.
set -eu

ROOT="$(cd "$(dirname "$0")" && pwd)"
INSTALL_ROOT="${JUKEWIZARD_HOME:-$HOME/.local/lib/jukewizard}"
BIN_DIR="${JUKEWIZARD_BIN_DIR:-$HOME/.local/bin}"

/usr/bin/swift build -c release --package-path "$ROOT"
BUILD_BIN="$(/usr/bin/swift build -c release --package-path "$ROOT" --show-bin-path)"
BUNDLE="$BUILD_BIN/JukeWizard_JukeWizard.bundle"

test -x "$BUILD_BIN/JukeWizard"
test -d "$BUNDLE"
/bin/mkdir -p "$INSTALL_ROOT" "$BIN_DIR"
/usr/bin/install -m 0755 "$BUILD_BIN/JukeWizard" "$INSTALL_ROOT/JukeWizard"
/usr/bin/ditto "$BUNDLE" "$INSTALL_ROOT/JukeWizard_JukeWizard.bundle"
/usr/bin/install -m 0755 "$ROOT/bin/jukewizard-installed" "$BIN_DIR/jukewizard"

echo "installed JukeWizard -> $INSTALL_ROOT/JukeWizard"
echo "launcher -> $BIN_DIR/jukewizard"
