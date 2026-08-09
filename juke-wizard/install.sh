#!/bin/sh
# Juke is a Menu Band feature; this compatibility entry point installs the
# owning application and its control-only `jukewizard` shell command. It no
# longer builds or registers a standalone JukeWizard process — the one that
# existed before Juke moved into Menu Band is torn down below.
set -eu

ROOT="$(cd "$(dirname "$0")" && pwd)"
INSTALL_ROOT="${JUKEWIZARD_HOME:-$HOME/.local/lib/jukewizard}"
BIN_DIR="${JUKEWIZARD_BIN_DIR:-$HOME/.local/bin}"
LEGACY_LAUNCH_LABEL="computer.aesthetic.jukewizard"
LEGACY_LAUNCH_AGENT="$HOME/Library/LaunchAgents/$LEGACY_LAUNCH_LABEL.plist"

# Retire the pre-Menu-Band standalone: its launch agent, resident process,
# and installed binary. The control CLI and cloud helpers stay.
uid="$(/usr/bin/id -u)"
/bin/launchctl bootout "gui/$uid/$LEGACY_LAUNCH_LABEL" 2>/dev/null || true
/bin/rm -f "$LEGACY_LAUNCH_AGENT"
/usr/bin/pkill -x JukeWizard 2>/dev/null || true
/bin/rm -f "$INSTALL_ROOT/JukeWizard"
/bin/rm -rf "$INSTALL_ROOT/JukeWizard_JukeWizard.bundle"

/bin/mkdir -p "$INSTALL_ROOT" "$BIN_DIR"
/usr/bin/install -m 0755 "$ROOT/bin/juke-cloud.mjs" "$INSTALL_ROOT/juke-cloud.mjs"
/usr/bin/install -m 0755 "$ROOT/bin/jukewizard-control.mjs" "$INSTALL_ROOT/jukewizard-control.mjs"
/usr/bin/install -m 0755 "$ROOT/../tezos/ac-login.mjs" "$INSTALL_ROOT/ac-login.mjs"
# A machine with the checkout can symlink this at the repo so edits to the
# wrapper take effect without reinstalling. Copying over that link would put
# the drift back, so leave a link the way we found it.
if [ -L "$BIN_DIR/jukewizard" ]; then
  echo "jukewizard: $BIN_DIR/jukewizard is a symlink — leaving it pointed at $(readlink "$BIN_DIR/jukewizard")"
else
  /usr/bin/install -m 0755 "$ROOT/bin/jukewizard-installed" "$BIN_DIR/jukewizard"
fi

# The owning application. Menu Band's installer builds, signs, and registers
# the launch agents that keep the Juke's process resident.
bash "$ROOT/../slab/menuband/install.sh"

echo "jukewizard command -> $BIN_DIR/jukewizard"
echo "Juke lives inside Menu Band; control socket at ~/.config/jukewizard/control.sock"
