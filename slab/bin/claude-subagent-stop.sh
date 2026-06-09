#!/bin/bash
# SubagentStop hook: remove this session's oldest subagent marker and play a
# ping. FIFO-per-session removal keeps each session's count accurate even when
# subagents finish out of order. Falls back to the globally-oldest marker if
# the session subdir is empty, so counts can never leak upward.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}
SUBAGENT_DIR="$SLAB_HOME/state/active-subagents"
SOUNDS="$SLAB_HOME/sounds"
MUTE_FLAG="$SLAB_HOME/state/muted"

mkdir -p "$SUBAGENT_DIR"
input=$(cat 2>/dev/null || true)
session=$(echo "$input" | jq -r '.session_id // empty' 2>/dev/null)
removed=0
if [[ -n "$session" && -d "$SUBAGENT_DIR/$session" ]]; then
    oldest=$(ls -1tr "$SUBAGENT_DIR/$session" 2>/dev/null | head -1)
    if [[ -n "$oldest" ]]; then rm -f "$SUBAGENT_DIR/$session/$oldest"; removed=1; fi
fi
if [[ "$removed" == 0 ]]; then
    oldest=$(ls -1tr "$SUBAGENT_DIR"/*/* 2>/dev/null | head -1)
    [[ -n "$oldest" ]] && rm -f "$oldest"
fi

# Honor the menubar's "Mute ambient sonification" toggle — no ping / TTS
# while muted. Marker accounting above still runs so counts stay correct.
if [[ -e "$MUTE_FLAG" ]]; then
    exit 0
fi

py="$SLAB_HOME/venv/bin/python3"
helper="$SLAB_BIN/jeffrey-say.py"
if [[ -x "$py" && -f "$helper" ]]; then
    "$py" "$helper" subagent 2>/dev/null &
else
    "$SLAB_BIN/slab-afplay" "$SOUNDS/ping.wav" &
fi
exit 0
