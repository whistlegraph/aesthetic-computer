#!/bin/sh
# Build and install a self-contained JukeWizard CLI bundle for this Mac.
set -eu

ROOT="$(cd "$(dirname "$0")" && pwd)"
INSTALL_ROOT="${JUKEWIZARD_HOME:-$HOME/.local/lib/jukewizard}"
BIN_DIR="${JUKEWIZARD_BIN_DIR:-$HOME/.local/bin}"
LAUNCH_LABEL="computer.aesthetic.jukewizard"
LAUNCH_AGENT_DIR="$HOME/Library/LaunchAgents"
LAUNCH_AGENT="$LAUNCH_AGENT_DIR/$LAUNCH_LABEL.plist"
LOG_DIR="$HOME/Library/Logs"

/usr/bin/swift build -c release --package-path "$ROOT"
BUILD_BIN="$(/usr/bin/swift build -c release --package-path "$ROOT" --show-bin-path)"
BUNDLE="$BUILD_BIN/JukeWizard_JukeWizard.bundle"

test -x "$BUILD_BIN/JukeWizard"
test -d "$BUNDLE"
/bin/mkdir -p "$INSTALL_ROOT" "$BIN_DIR"
/usr/bin/install -m 0755 "$BUILD_BIN/JukeWizard" "$INSTALL_ROOT/JukeWizard"
/usr/bin/ditto "$BUNDLE" "$INSTALL_ROOT/JukeWizard_JukeWizard.bundle"
/usr/bin/install -m 0755 "$ROOT/bin/jukewizard-installed" "$BIN_DIR/jukewizard"

# Own the resident menu-bar process with the user's Aqua launchd session.
# Abnormal exits restart; a deliberate Quit exits successfully and stays quit.
/bin/mkdir -p "$LAUNCH_AGENT_DIR" "$LOG_DIR"
/bin/cat > "$LAUNCH_AGENT" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>$LAUNCH_LABEL</string>
  <key>ProgramArguments</key>
  <array>
    <string>$BIN_DIR/jukewizard</string>
    <string>--background</string>
  </array>
  <key>RunAtLoad</key>
  <true/>
  <key>KeepAlive</key>
  <dict>
    <key>SuccessfulExit</key>
    <false/>
  </dict>
  <key>ProcessType</key>
  <string>Interactive</string>
  <key>LimitLoadToSessionType</key>
  <string>Aqua</string>
  <key>ThrottleInterval</key>
  <integer>5</integer>
  <key>StandardOutPath</key>
  <string>$LOG_DIR/JukeWizard.log</string>
  <key>StandardErrorPath</key>
  <string>$LOG_DIR/JukeWizard.log</string>
</dict>
</plist>
EOF
/usr/bin/plutil -lint "$LAUNCH_AGENT" >/dev/null
uid="$(/usr/bin/id -u)"
/bin/launchctl bootout "gui/$uid/$LAUNCH_LABEL" 2>/dev/null || true
/usr/bin/pkill -x JukeWizard 2>/dev/null || true
/bin/launchctl bootstrap "gui/$uid" "$LAUNCH_AGENT"

echo "installed JukeWizard -> $INSTALL_ROOT/JukeWizard"
echo "launcher -> $BIN_DIR/jukewizard"
echo "launch agent -> $LAUNCH_AGENT"
