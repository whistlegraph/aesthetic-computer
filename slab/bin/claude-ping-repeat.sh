#!/bin/bash
# Random high-pitched major-chord ping every INTERVAL seconds while lid is CLOSED.
# Exits automatically once the lid is reopened.
# A Claude that Easel is driving is that session's engine, not a rock of its
# own; Easel keeps the marker, so this hook has nothing to say.
[[ -n "${EASEL_SESSION_ID:-}" ]] && exit 0
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}

INTERVAL=${INTERVAL:-30}
DIR="$SLAB_HOME/sounds"
# Fallback pitch classes if jeffrey-say is unavailable.
CHORDS=(C F G D A Eb Bb Ab)

JEFFREY_PY="$SLAB_HOME/venv/bin/python3"
JEFFREY_SAY="$SLAB_BIN/jeffrey-say.py"

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
    if [[ -x "$JEFFREY_PY" && -f "$JEFFREY_SAY" ]]; then
        "$JEFFREY_PY" "$JEFFREY_SAY" ping 2>/dev/null
    else
        chord=${CHORDS[$((RANDOM % ${#CHORDS[@]}))]}
        "$SLAB_BIN/slab-afplay" "$DIR/ping_${chord}.wav"
    fi
done
