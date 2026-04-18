#!/bin/bash
# Sleeps the Mac after DELAY seconds if the lid is still closed at that point.
# Plays the sleep tone first for a dreamy auditory cue.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}

DELAY=${1:-120}
SLEEP_TONE="$SLAB_HOME/sounds/sleep-tone.wav"

sleep "$DELAY"

lid=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')
if [[ "$lid" == "Yes" ]]; then
    /usr/bin/afplay "$SLEEP_TONE" 2>/dev/null
    "$SLAB_BIN/claude-sleep" now
fi
