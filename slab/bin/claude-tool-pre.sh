#!/bin/bash
# PreToolUse hook: when the Task tool fires, mark a subagent as in-flight under
# the owning session, so the menubar can draw one dot per running subagent on
# THAT session's polygon edge. SubagentStop removes the session's oldest marker.
# (Workflow-tool agents don't fire this hook — the menubar counts those live
# from the workflow transcript dir instead.)
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SUBAGENT_DIR="$SLAB_HOME/state/active-subagents"

input=$(cat)
tool=$(echo "$input" | jq -r '.tool_name // empty' 2>/dev/null)

if [[ "$tool" == "Task" ]]; then
    session=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)
    dir="$SUBAGENT_DIR/${session:-_global}"
    mkdir -p "$dir"
    : > "$dir/$(date +%s)-$$-$RANDOM"
fi

exit 0
