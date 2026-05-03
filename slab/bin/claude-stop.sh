#!/bin/bash
# Claude Stop hook. State under $SLAB_HOME/state/:
#   active-prompts/<session_id>      — one per live claude session; lives until
#                                      the claude_pid dies (janitored here).
#   awaiting-prompts/<session_id>    — paused (turn complete OR awaiting
#                                      permission). Written here + by
#                                      claude-notify.sh; cleared by next
#                                      UserPromptSubmit.
#   active-subagents/<timestamp>-..  — PreToolUse(Task) → SubagentStop
# Claude Code's Stop fires after every assistant turn — NOT only at session
# end — so we keep the active marker around (so the menubar can still draw a
# bar for the live thread) and just flip its state to awaiting.
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
MUTE_FLAG="$SLAB_HOME/state/muted"
mkdir -p "$(dirname "$LOG")" "$ACTIVE_DIR" "$AWAITING_DIR" "$SUBAGENT_DIR"

pkill -f claude-ping-repeat.sh 2>/dev/null
pkill -f claude-sleep-schedule.sh 2>/dev/null

input=$(cat)
session_id=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)

# Mark this session as awaiting (turn complete). The active marker stays so
# the menubar can keep a bar for this thread; the awaiting marker flips its
# render to rainbow-pulse. Cleared on next UserPromptSubmit.
if [[ -n "$session_id" ]]; then
    printf 'turn complete\n' > "$AWAITING_DIR/$session_id" 2>/dev/null
fi

# Janitor: drop active-prompts whose claude_pid is dead (terminal closed),
# and any awaiting-prompts orphans (no matching active). Keeps disk state in
# sync with reality without leaking on terminal close.
shopt -s nullglob
for f in "$ACTIVE_DIR"/*; do
    [[ -f "$f" ]] || continue
    pid=$(jq -r '.claude_pid // 0' "$f" 2>/dev/null)
    [[ -z "$pid" || "$pid" == "0" ]] && continue
    if ! kill -0 "$pid" 2>/dev/null; then
        sid=$(basename "$f")
        rm -f "$f" "$AWAITING_DIR/$sid"
    fi
done
for f in "$AWAITING_DIR"/*; do
    [[ -f "$f" ]] || continue
    sid=$(basename "$f")
    [[ -e "$ACTIVE_DIR/$sid" ]] || rm -f "$f"
done

# "Working" = active sessions that aren't awaiting. That's what the chime /
# sleep logic cares about: how many threads are still doing work.
working=0
for f in "$ACTIVE_DIR"/*; do
    [[ -f "$f" ]] || continue
    sid=$(basename "$f")
    [[ -e "$AWAITING_DIR/$sid" ]] && continue
    working=$((working + 1))
done
subagents=("$SUBAGENT_DIR"/*)
others=$((working + ${#subagents[@]}))
shopt -u nullglob

echo "$(date '+%Y-%m-%d %H:%M:%S') Stop: session=${session_id:-?} working=$working subagents=${#subagents[@]} others=$others" >> "$LOG"

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

if [[ -e "$MUTE_FLAG" ]]; then
    # User asked for silence — keep the lid-aware sleep handoff but skip
    # every chime / TTS / pentatonic beep. Ambient is force-stopped so a
    # daemon-restarted pad doesn't sneak back on while muted.
    stop_ambient
    if (( others == 0 )) && [[ "$lid" == "Yes" ]]; then
        "$SLAB_BIN/claude-sleep" now
    fi
elif (( others == 0 )); then
    if [[ "$lid" == "Yes" ]]; then
        stop_ambient
        tired_stinger
        "$SLAB_BIN/claude-sleep" now
    else
        # Lid open + everything settled → sinebells waltz phrase. Falls back
        # to the legacy chime if the rendered file is missing (e.g. fresh
        # checkout that hasn't run waltz.mjs yet).
        if [[ -f "$CH/all-done-waltz.mp3" ]]; then
            /usr/bin/afplay "$CH/all-done-waltz.mp3" 2>/dev/null &
        else
            /usr/bin/afplay "$CH/all-done.wav" 2>/dev/null &
        fi
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
