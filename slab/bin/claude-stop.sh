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

"$SLAB_BIN/slab-cancel-pending" 2>/dev/null || true

input=$(cat)
session_id=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)

# Mark this session as awaiting (turn complete). The active marker stays so
# the menubar can keep a bar for this thread; the awaiting marker flips its
# render to rainbow-pulse. Cleared on next UserPromptSubmit.
if [[ -n "$session_id" ]]; then
    printf 'turn complete\n' > "$AWAITING_DIR/$session_id" 2>/dev/null
    # Turn ended → no tool running; clear the heartbeat flag (see
    # claude-tool-heartbeat.sh) so the next idle window detects cleanly.
    rm -f "$SLAB_HOME/state/running-tools/$session_id" 2>/dev/null
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
        rm -f "$f" "$AWAITING_DIR/$sid" "$SLAB_HOME/state/running-tools/$sid"
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
    # Fade both ambient processes (Python reactive + Swift drone) and
    # silence slab-spawned afplay. Clears the pause flag so a stalled
    # Notification→Stop sequence can't leave ambient stuck off.
    "$SLAB_BIN/slab-fade-ambient" --clear-pause --kill-slab-afplay
}

lid=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')

JEFFREY_PY="$SLAB_HOME/venv/bin/python3"
JEFFREY_SAY="$SLAB_BIN/jeffrey-say.py"

jeffrey_say() {
    # Speak via Jeffrey ElevenLabs voice. First arg = category (or --phrase),
    # remaining args passed through. Returns non-zero if helper missing so
    # callers can fall back to the legacy wav chime.
    [[ -x "$JEFFREY_PY" && -f "$JEFFREY_SAY" ]] || return 1
    "$JEFFREY_PY" "$JEFFREY_SAY" "$@" 2>>"$LOG"
}

tired_stinger() {
    # Lid-CLOSED completion: Jeffrey "i'm tired" before pmset sleepnow.
    jeffrey_say tired || "$SLAB_BIN/slab-afplay" "$CH/all-done.wav"
}

done_stinger() {
    # Lid-OPEN completion: Jeffrey "i'm all done".
    jeffrey_say done || "$SLAB_BIN/slab-afplay" "$CH/all-done.wav" &
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
        # Lid open + everything settled → Jeffrey "i'm all done".
        done_stinger
    fi
else
    # Speak the count once — "one" .. "eight" in Jeffrey's voice.
    # >8 active still says "eight" (matches the original beep cap).
    n=$others
    (( n > 8 )) && n=8
    jeffrey_say count "$n" || {
        max=8
        (( others > max )) && others=$max
        for ((i=1; i<=others; i++)); do
            "$SLAB_BIN/slab-afplay" "$CH/beep_${i}.wav"
            sleep 0.08
        done
    }
fi

exit 0
