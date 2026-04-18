#!/bin/bash
# SubagentStop hook: remove the oldest subagent marker and play a ping.
# FIFO removal keeps the count accurate even if subagents finish out of order
# (we don't need per-subagent correlation, just a correct total).
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SUBAGENT_DIR="$SLAB_HOME/state/active-subagents"
SOUNDS="$SLAB_HOME/sounds"

mkdir -p "$SUBAGENT_DIR"
oldest=$(ls -1tr "$SUBAGENT_DIR" 2>/dev/null | head -1)
[[ -n "$oldest" ]] && rm -f "$SUBAGENT_DIR/$oldest"

/usr/bin/afplay "$SOUNDS/ping.wav" 2>/dev/null &
exit 0
