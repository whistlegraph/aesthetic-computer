#!/bin/bash
# Sleeps the Mac after DELAY seconds if the lid is still closed at that point.
# Plays the sleep tone first for a dreamy auditory cue.
# A Claude that Easel is driving is that session's engine, not a rock of its
# own; Easel keeps the marker, so this hook has nothing to say.
[[ -n "${EASEL_SESSION_ID:-}" ]] && exit 0
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}

DELAY=${1:-120}
SLEEP_TONE="$SLAB_HOME/sounds/sleep-tone.wav"

sleep "$DELAY"

lid=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')
if [[ "$lid" == "Yes" ]]; then
    py="$SLAB_HOME/venv/bin/python3"
    helper="$SLAB_BIN/jeffrey-say.py"
    if [[ -x "$py" && -f "$helper" ]]; then
        "$py" "$helper" sleep 2>/dev/null || "$SLAB_BIN/slab-afplay" "$SLEEP_TONE"
    else
        "$SLAB_BIN/slab-afplay" "$SLEEP_TONE"
    fi
    "$SLAB_BIN/claude-sleep" now
fi
