#!/bin/bash
# Claude Stop hook. Active work is tracked in two dirs under $SLAB_HOME/state/:
#   active-prompts/<session_id>      — UserPromptSubmit → Stop
#   active-subagents/<timestamp>-..  — PreToolUse(Task) → SubagentStop
# This script removes its own prompt marker and counts whatever remains.
#   others > 0 → N distinct ascending pentatonic beeps (capped at 8).
#   others = 0 → "all done" chime.
#                If lid is closed, stop ambient + sleep the machine immediately.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}
CH="$SLAB_HOME/sounds"
LOG=${CLAUDE_STOP_LOG:-$SLAB_HOME/logs/claude-stop.log}
ACTIVE_DIR="$SLAB_HOME/state/active-prompts"
SUBAGENT_DIR="$SLAB_HOME/state/active-subagents"
mkdir -p "$(dirname "$LOG")" "$ACTIVE_DIR" "$SUBAGENT_DIR"

pkill -f claude-ping-repeat.sh 2>/dev/null
pkill -f claude-sleep-schedule.sh 2>/dev/null

input=$(cat)
session_id=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)
[[ -n "$session_id" ]] && rm -f "$ACTIVE_DIR/$session_id"

shopt -s nullglob
prompts=("$ACTIVE_DIR"/*)
subagents=("$SUBAGENT_DIR"/*)
others=$((${#prompts[@]} + ${#subagents[@]}))
shopt -u nullglob

echo "$(date '+%Y-%m-%d %H:%M:%S') Stop: session=${session_id:-?} prompts=${#prompts[@]} subagents=${#subagents[@]} others=$others" >> "$LOG"

stop_ambient() {
    # Ambient is owned by lid-reactive.py now. Fade gracefully via SIGTERM;
    # the process ramps to silence over ~2s and exits itself. We also pkill
    # any stray afplay (e.g. the short start chime).
    local pid
    if [[ -f /tmp/lidreactive.pid ]]; then
        pid=$(cat /tmp/lidreactive.pid 2>/dev/null)
        [[ -n "$pid" ]] && kill -TERM "$pid" 2>/dev/null
        rm -f /tmp/lidreactive.pid
    else
        pkill -TERM -f lid-reactive.py 2>/dev/null
    fi
    rm -f /tmp/slab-ambient-active
    pkill -x afplay 2>/dev/null
}

lid=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')

if (( others == 0 )); then
    if [[ "$lid" == "Yes" ]]; then
        stop_ambient
        /usr/bin/afplay "$CH/all-done.wav" 2>/dev/null
        "$SLAB_BIN/claude-sleep" now
    else
        /usr/bin/afplay "$CH/all-done.wav" 2>/dev/null &
    fi
else
    max=8
    n=$others
    (( n > max )) && n=$max
    for ((i=1; i<=n; i++)); do
        /usr/bin/afplay "$CH/beep_${i}.wav" 2>/dev/null
        sleep 0.08
    done
fi

exit 0
