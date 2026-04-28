#!/bin/bash
# Claude Stop hook. Active work is tracked in two dirs under $SLAB_HOME/state/:
#   active-prompts/<session_id>      — UserPromptSubmit → Stop
#   active-subagents/<timestamp>-..  — PreToolUse(Task) → SubagentStop
# This script removes its own prompt marker and counts whatever remains.
#   others > 0 → N distinct ascending pentatonic beeps (capped at 8).
#   others = 0 → "all done" chime (lid open) OR TTS "i'm tired" with fade-out
#                tail → `pmset sleepnow` (lid closed: stops ambient first, so
#                the transition to sleep is a gentle dissolve instead of a cut).
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}
CH="$SLAB_HOME/sounds"
LOG=${CLAUDE_STOP_LOG:-$SLAB_HOME/logs/claude-stop.log}
ACTIVE_DIR="$SLAB_HOME/state/active-prompts"
AWAITING_DIR="$SLAB_HOME/state/awaiting-prompts"
SUBAGENT_DIR="$SLAB_HOME/state/active-subagents"
mkdir -p "$(dirname "$LOG")" "$ACTIVE_DIR" "$AWAITING_DIR" "$SUBAGENT_DIR"

pkill -f claude-ping-repeat.sh 2>/dev/null
pkill -f claude-sleep-schedule.sh 2>/dev/null

input=$(cat)
session_id=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)
if [[ -n "$session_id" ]]; then
    rm -f "$ACTIVE_DIR/$session_id" "$AWAITING_DIR/$session_id"
fi

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
    rm -f /tmp/slab-ambient-active /tmp/slab-ambient-paused
    pkill -x afplay 2>/dev/null
}

lid=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')

tired_stinger() {
    # Speak "i'm tired" with a cosine fade-out tail, so the transition to
    # sleep is a gentle dissolve rather than an abrupt cut. Falls back to
    # the all-done chime if the venv/helper is missing for any reason.
    local py="$SLAB_HOME/venv/bin/python3"
    local helper="$SLAB_BIN/claude-tired.py"
    if [[ -x "$py" && -f "$helper" ]]; then
        "$py" "$helper" 2>>"$LOG" || /usr/bin/afplay "$CH/all-done.wav" 2>/dev/null
    else
        /usr/bin/afplay "$CH/all-done.wav" 2>/dev/null
    fi
}

if (( others == 0 )); then
    if [[ "$lid" == "Yes" ]]; then
        stop_ambient
        tired_stinger
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
