#!/bin/bash
# slab installer — sets up the lid-ambient / Claude Code audio system on macOS.
#
# Usage:
#   ./install.sh                # install everything interactively
#   ./install.sh --no-hooks     # skip Claude Code hook merge
#   ./install.sh --no-sudoers   # skip passwordless-sudo rule for pmset
#   ./install.sh --uninstall    # same as ./uninstall.sh
#
# Idempotent: re-running updates symlinks, plist, etc.

set -euo pipefail

SLAB_REPO=$(cd "$(dirname "$0")" && pwd)
HOME_DIR=${HOME:-$(echo ~)}
USER_NAME=${USER:-$(whoami)}

SLAB_HOME="$HOME_DIR/.local/share/slab"
SLAB_BIN="$HOME_DIR/.local/bin"
CLAUDE_DIR="$HOME_DIR/.claude"
LAUNCH_AGENTS="$HOME_DIR/Library/LaunchAgents"
PLIST_NAME=computer.slab.daemon.plist
PLIST_INSTALLED="$LAUNCH_AGENTS/$PLIST_NAME"
SUDOERS_FILE=/etc/sudoers.d/slab-pmset

DO_HOOKS=1
DO_SUDOERS=1
for arg in "$@"; do
    case "$arg" in
        --no-hooks)   DO_HOOKS=0 ;;
        --no-sudoers) DO_SUDOERS=0 ;;
        --uninstall)  exec "$SLAB_REPO/uninstall.sh" ;;
        -h|--help)    grep '^# ' "$0" | sed 's/^# //'; exit 0 ;;
    esac
done

say() { printf '\033[1;36m• %s\033[0m\n' "$*"; }
warn() { printf '\033[1;33m⚠ %s\033[0m\n' "$*"; }
err() { printf '\033[1;31m✗ %s\033[0m\n' "$*" >&2; }

# ------------ prereqs ------------
say "checking prerequisites"
for cmd in brew python3 jq ioreg pmset afplay osascript; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
        err "missing: $cmd"
        [[ "$cmd" == "brew" ]] && echo "  install Homebrew first: https://brew.sh"
        [[ "$cmd" == "jq" ]] && echo "  brew install jq"
        exit 1
    fi
done

# ------------ layout ------------
say "creating directories"
mkdir -p "$SLAB_HOME/sounds" "$SLAB_HOME/logs" "$SLAB_HOME/sessions" "$SLAB_BIN" "$LAUNCH_AGENTS"

# ------------ scripts (symlinked from repo) ------------
say "symlinking scripts into $SLAB_BIN"
for f in "$SLAB_REPO/bin/"*; do
    base=$(basename "$f")
    dest="$SLAB_BIN/$base"
    rm -f "$dest"
    ln -s "$f" "$dest"
    chmod +x "$f"
done

# ------------ sounds ------------
say "copying sounds to $SLAB_HOME/sounds"
cp -f "$SLAB_REPO/sounds/"*.wav "$SLAB_HOME/sounds/"

# regenerate ambient.wav if missing or flagged
if [[ ! -f "$SLAB_HOME/sounds/ambient.wav" ]]; then
    say "generating ambient.wav (~17s)"
    SLAB_HOME="$SLAB_HOME" python3 "$SLAB_REPO/bin/lid-ambient-generate.py" || \
        warn "ambient.wav generation failed — rerun manually later"
fi

# ------------ python venv (numpy + sounddevice for reactive listener) ------------
if [[ ! -x "$SLAB_HOME/venv/bin/python3" ]]; then
    say "creating Python venv at $SLAB_HOME/venv"
    python3 -m venv "$SLAB_HOME/venv"
fi
say "installing numpy + sounddevice + rumps into venv"
"$SLAB_HOME/venv/bin/pip" install --quiet --upgrade pip
"$SLAB_HOME/venv/bin/pip" install --quiet numpy sounddevice rumps

# ------------ launchd plists ------------
say "installing launchd plist → $PLIST_INSTALLED"
sed "s|@HOME@|$HOME_DIR|g" "$SLAB_REPO/launchd/$PLIST_NAME.template" > "$PLIST_INSTALLED"

# load (or reload) the daemon
if launchctl list | grep -q computer.slab.daemon; then
    launchctl unload "$PLIST_INSTALLED" 2>/dev/null || true
fi
launchctl load "$PLIST_INSTALLED"

MENUBAR_PLIST_NAME=computer.slab.menubar.plist
MENUBAR_PLIST_INSTALLED="$LAUNCH_AGENTS/$MENUBAR_PLIST_NAME"
say "installing menu-bar plist → $MENUBAR_PLIST_INSTALLED"
sed "s|@HOME@|$HOME_DIR|g" "$SLAB_REPO/launchd/$MENUBAR_PLIST_NAME.template" > "$MENUBAR_PLIST_INSTALLED"

if launchctl list | grep -q computer.slab.menubar; then
    launchctl unload "$MENUBAR_PLIST_INSTALLED" 2>/dev/null || true
fi
launchctl load "$MENUBAR_PLIST_INSTALLED"

# ------------ Claude Code hooks ------------
if [[ $DO_HOOKS -eq 1 ]]; then
    say "merging Claude Code hooks into $CLAUDE_DIR/settings.json"
    mkdir -p "$CLAUDE_DIR"
    target="$CLAUDE_DIR/settings.json"
    fragment=$(sed "s|@HOME@|$HOME_DIR|g" "$SLAB_REPO/settings-fragment.json")
    if [[ -f "$target" ]]; then
        # merge: slab hooks overwrite existing ones under same event keys
        tmp=$(mktemp)
        echo "$fragment" | jq -s '.[0]' > "$tmp.frag"
        jq -s '.[0] * .[1]' "$target" "$tmp.frag" > "$tmp"
        mv "$tmp" "$target"
        rm -f "$tmp.frag"
    else
        echo "$fragment" > "$target"
    fi
fi

# ------------ passwordless sudo for pmset ------------
if [[ $DO_SUDOERS -eq 1 ]]; then
    if [[ ! -f "$SUDOERS_FILE" ]]; then
        say "installing sudoers rule (requires sudo password once)"
        rendered=$(sed "s|@USER@|$USER_NAME|g" "$SLAB_REPO/sudoers.d/slab-pmset.template")
        echo "$rendered" | sudo tee "$SUDOERS_FILE" > /dev/null
        sudo chmod 440 "$SUDOERS_FILE"
    else
        say "sudoers rule already present"
    fi
fi

say "install complete"
cat <<EOF

  daemon:           $PLIST_INSTALLED
  scripts:          $SLAB_BIN
  sounds + venv:    $SLAB_HOME
  sessions + logs:  $SLAB_HOME/{sessions,logs}
  Claude hooks:     $CLAUDE_DIR/settings.json$([ $DO_HOOKS -eq 0 ] && echo ' (skipped)')
  sudoers:          $SUDOERS_FILE$([ $DO_SUDOERS -eq 0 ] && echo ' (skipped)')

  try it:
    claude-sleep status         # check sleep state
    claude-sleep awake          # stay awake with lid closed
    tail -f $SLAB_HOME/logs/lidalive.log

  uninstall: $SLAB_REPO/uninstall.sh
EOF
