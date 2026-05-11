#!/bin/bash
# Claude Notification hook. Fires when the agent pauses and needs user
# feedback (permission prompt or idle-waiting-on-input). We:
#   1. Fade the ambient bed (SIGTERM the reactive listener + synth).
#   2. Drop a pause flag so the lid-ambient daemon's polling loop does NOT
#      auto-restart ambient while we wait on the user — the active marker
#      for the prompt is still present, so without this flag the daemon
#      would re-arm on the next 0.5s tick.
#   3. Speak "help me" with a short fade tail (claude-help.py).
# The pause flag is cleared by the next UserPromptSubmit (user responded)
# or by Stop (work ended). Does NOT sleep the Mac.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}
LOG=${CLAUDE_NOTIFY_LOG:-$SLAB_HOME/logs/claude-notify.log}
PAUSE_FLAG=/tmp/slab-ambient-paused
AWAITING_DIR="$SLAB_HOME/state/awaiting-prompts"
mkdir -p "$(dirname "$LOG")" "$AWAITING_DIR"

input=$(cat)
session_id=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)
message=$(echo "$input" | jq -r '.message // empty' 2>/dev/null)
echo "$(date '+%Y-%m-%d %H:%M:%S') Notification: session=${session_id:-?} msg=${message:-?}" >> "$LOG"

# Mark paused before fading so the daemon sees the flag on its next tick.
: > "$PAUSE_FLAG"

# Per-session awaiting marker — read by the menubar to flash the icon and
# tag this session in the dropdown. Cleared by the next UserPromptSubmit or
# by Stop. The message (if any) lands inside as the awaiting reason.
if [[ -n "$session_id" ]]; then
    printf '%s\n' "${message:-awaiting input}" > "$AWAITING_DIR/$session_id" 2>/dev/null
fi

# Fade ambient. Short chimes (e.g. the start stinger) can finish naturally,
# so we do NOT --kill-slab-afplay. We also do NOT --clear-pause — we just
# set the pause flag above and want it to persist until UserPromptSubmit
# or Stop clears it.
"$SLAB_BIN/slab-fade-ambient"

py="$SLAB_HOME/venv/bin/python3"
helper="$SLAB_BIN/claude-help.py"
if [[ -x "$py" && -f "$helper" ]]; then
    "$py" "$helper" 2>>"$LOG" &
fi

exit 0
