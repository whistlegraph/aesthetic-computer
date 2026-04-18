#!/bin/bash
# slab uninstaller — undoes install.sh. Leaves generated data (sessions, logs)
# alone by default; pass --purge to remove them too.

set -euo pipefail

SLAB_REPO=$(cd "$(dirname "$0")" && pwd)
HOME_DIR=${HOME:-$(echo ~)}

SLAB_HOME="$HOME_DIR/.local/share/slab"
SLAB_BIN="$HOME_DIR/.local/bin"
CLAUDE_DIR="$HOME_DIR/.claude"
LAUNCH_AGENTS="$HOME_DIR/Library/LaunchAgents"
PLIST_INSTALLED="$LAUNCH_AGENTS/computer.slab.daemon.plist"
SUDOERS_FILE=/etc/sudoers.d/slab-pmset

PURGE=0
for arg in "$@"; do [[ "$arg" == "--purge" ]] && PURGE=1; done

say() { printf '\033[1;36m• %s\033[0m\n' "$*"; }

# ------------ stop + unload ------------
if [[ -f "$PLIST_INSTALLED" ]]; then
    say "unloading launchd agent"
    launchctl unload "$PLIST_INSTALLED" 2>/dev/null || true
    rm -f "$PLIST_INSTALLED"
fi

# kill stragglers
pkill -f lid-ambient.sh 2>/dev/null || true
pkill -f lid-reactive.py 2>/dev/null || true
pkill -f slab-monitor.sh 2>/dev/null || true
pkill -f claude-ping-repeat.sh 2>/dev/null || true
pkill -f claude-sleep-schedule.sh 2>/dev/null || true

# ------------ remove symlinks ------------
say "removing script symlinks from $SLAB_BIN"
for f in "$SLAB_REPO/bin/"*; do
    base=$(basename "$f")
    dest="$SLAB_BIN/$base"
    if [[ -L "$dest" ]]; then rm -f "$dest"; fi
done

# ------------ Claude Code hooks: best-effort surgical removal ------------
target="$CLAUDE_DIR/settings.json"
if [[ -f "$target" ]]; then
    say "removing slab hooks from $target"
    tmp=$(mktemp)
    jq 'def strip(events; cmd_pattern):
          reduce events[] as $ev (.;
            if .hooks[$ev] then
              .hooks[$ev] |= map(
                .hooks |= map(select(.command | test(cmd_pattern) | not)) | select(.hooks | length > 0)
              ) | (if (.hooks[$ev] | length) == 0 then del(.hooks[$ev]) else . end)
            else . end);
        strip(["Stop", "SubagentStop", "UserPromptSubmit", "SessionStart"];
              "claude-(stop|ping-repeat|sleep-schedule|prompt-log)|slab/sounds/ping\\.wav")
        | if (.hooks | length) == 0 then del(.hooks) else . end' \
        "$target" > "$tmp" && mv "$tmp" "$target"
fi

# ------------ sudoers ------------
if [[ -f "$SUDOERS_FILE" ]]; then
    say "removing sudoers rule (needs sudo)"
    sudo rm -f "$SUDOERS_FILE"
fi

# ------------ data ------------
if [[ $PURGE -eq 1 ]]; then
    say "purging $SLAB_HOME"
    rm -rf "$SLAB_HOME"
else
    say "leaving $SLAB_HOME in place (use --purge to remove sessions/logs/venv/sounds)"
fi

say "uninstall complete"
