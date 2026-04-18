#!/bin/bash
# UserPromptSubmit hook: log prompt, mark session as active, set awake.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}
PROMPT_LOG=${PROMPT_LOG:-$HOME/.claude/prompts.log.jsonl}
ACTIVE_DIR="$SLAB_HOME/state/active-prompts"

input=$(cat)

pkill -f claude-ping-repeat.sh 2>/dev/null
pkill -f claude-sleep-schedule.sh 2>/dev/null

# keep the machine awake while this new prompt runs
"$SLAB_BIN/claude-sleep" awake >/dev/null 2>&1 &

if [[ -n "$input" ]]; then
    mkdir -p "$ACTIVE_DIR"
    session_id=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)
    [[ -n "$session_id" ]] && : > "$ACTIVE_DIR/$session_id"

    mkdir -p "$(dirname "$PROMPT_LOG")"
    echo "$input" | jq -c --arg ts "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
        '{ts: $ts, session: .session_id, cwd: .cwd, prompt: .prompt}' \
        >> "$PROMPT_LOG" 2>/dev/null
fi

exit 0
