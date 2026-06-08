#!/bin/bash
# PreToolUse / PostToolUse heartbeat. Claude Code fires NO hook when the user
# interrupts (Esc) a turn, so the menubar can't tell "still working" from
# "interrupted, idle at the prompt" — it would stay stuck green.
#
# This refreshes the active-prompts marker's mtime on every tool start/stop
# (so a quiet, Esc'd session goes stale and the menubar flips it to the
# `interrupted` state), and keeps a running-tools/<sid> flag for the
# duration of a tool call (so a long, legit tool stays green even while the
# marker mtime ages). See ClaudeSession.swift isInterrupted().
#
# Usage: claude-tool-heartbeat.sh <pre|post>   (hook JSON on stdin)
set -u
MODE="${1:-pre}"
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
ACTIVE_DIR="$SLAB_HOME/state/active-prompts"
RUNNING_DIR="$SLAB_HOME/state/running-tools"

input=$(cat)
sid=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)
[[ -z "$sid" ]] && exit 0

mkdir -p "$RUNNING_DIR" 2>/dev/null

# Heartbeat: bump the active marker's mtime (only if it already exists — never
# synthesize a phantom session). The menubar reads this mtime as last-activity.
[[ -f "$ACTIVE_DIR/$sid" ]] && touch "$ACTIVE_DIR/$sid" 2>/dev/null

if [[ "$MODE" == "post" ]]; then
    # Tool finished → clear the running flag so idleness can be detected.
    rm -f "$RUNNING_DIR/$sid" 2>/dev/null
else
    # Tool starting → pin the session green for the call's duration.
    touch "$RUNNING_DIR/$sid" 2>/dev/null
fi

exit 0
