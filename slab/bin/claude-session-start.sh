#!/bin/bash
# Register a standalone Claude terminal before its first prompt.
set -u

# Aesel owns its engine's rock. Run before reading input or touching state.
[[ -n "${AESEL_SESSION_ID:-}${EASEL_SESSION_ID:-}" ]] && exit 0

SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
ACTIVE_DIR="$SLAB_HOME/state/active-prompts"

input=$(cat)
[[ -z "$input" ]] && exit 0

session_id=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)
[[ -z "$session_id" ]] && exit 0

mkdir -p "$ACTIVE_DIR"
marker="$ACTIVE_DIR/$session_id"

# Resuming a session must not demote an existing working or completed marker.
if [[ -f "$marker" ]]; then
    existing_state=$(jq -r '.state // empty' < "$marker" 2>/dev/null)
    if [[ -n "$existing_state" && "$existing_state" != "blank" ]]; then
        exit 0
    fi
fi

# Hooks have no controlling terminal; find the owning Claude process.
claude_pid=$$
for _ in 1 2 3 4 5 6 7 8; do
    parent=$(ps -o ppid= -p "$claude_pid" 2>/dev/null | tr -d ' ')
    [[ -z "$parent" || "$parent" == "1" ]] && break
    comm=$(ps -o comm= -p "$parent" 2>/dev/null | tr -d ' ')
    claude_pid=$parent
    [[ "$comm" == *claude* ]] && break
done
tty=${SLAB_TERMINAL_TTY:-$(ps -o tty= -p "$claude_pid" 2>/dev/null | tr -d ' ')}
ts=$(date -u +%Y-%m-%dT%H:%M:%SZ)
cwd=$(echo "$input" | jq -r '.cwd // ""' 2>/dev/null)

echo "$input" | jq -c \
    --arg sid "$session_id" \
    --arg cwd "$cwd" \
    --arg tty "$tty" \
    --arg pid "$claude_pid" \
    --arg nudge "${SLAB_NUDGE_SCREEN:-}" --arg contact "${SLAB_LOOPBOY_CONTACT:-}" \
    --arg ts "$ts" \
    '{session_id: $sid, cwd: $cwd, subject: "", summary: "", tty: $tty, claude_pid: ($pid | tonumber? // 0), updated: $ts, state: "blank", nudge_screen:$nudge, loopboy_contact:$contact}' \
    > "$marker" 2>/dev/null

exit 0
