#!/bin/bash
# PreToolUse hook: when the Task tool fires, mark a subagent as in-flight.
# SubagentStop removes the oldest marker to decrement the count.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SUBAGENT_DIR="$SLAB_HOME/state/active-subagents"

input=$(cat)
tool=$(echo "$input" | jq -r '.tool_name // empty' 2>/dev/null)

if [[ "$tool" == "Task" ]]; then
    mkdir -p "$SUBAGENT_DIR"
    : > "$SUBAGENT_DIR/$(date +%s)-$$-$RANDOM"
fi

exit 0
