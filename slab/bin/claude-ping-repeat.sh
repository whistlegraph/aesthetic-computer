#!/bin/bash
# Random high-pitched major-chord ping every INTERVAL seconds while lid is CLOSED.
# Exits automatically once the lid is reopened.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}

INTERVAL=${INTERVAL:-30}
CHORDS=(C F G D A Eb Bb Ab)
DIR="$SLAB_HOME/sounds"

lid_closed() {
    local s
    s=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')
    [[ "$s" == "Yes" ]]
}

while true; do
    sleep "$INTERVAL"
    if ! lid_closed; then
        exit 0
    fi
    chord=${CHORDS[$((RANDOM % ${#CHORDS[@]}))]}
    /usr/bin/afplay "$DIR/ping_${chord}.wav" 2>/dev/null
done
