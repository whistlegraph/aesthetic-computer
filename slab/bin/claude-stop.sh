#!/bin/bash
# Claude Stop hook.
#   other > 0 → N distinct ascending pentatonic beeps (capped at 8).
#   other = 0 → "all done" rising arp. If lid is closed, start repeat + auto-sleep.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}
SLAB_BIN=${SLAB_BIN:-$HOME/.local/bin}
CH="$SLAB_HOME/sounds"
LOG=${CLAUDE_STOP_LOG:-$SLAB_HOME/logs/claude-stop.log}
mkdir -p "$(dirname "$LOG")"

pkill -f claude-ping-repeat.sh 2>/dev/null
pkill -f claude-sleep-schedule.sh 2>/dev/null

# count running Claude Code sessions (inner `claude` CLI; anchor to /Users/)
total=$(ps -eo command | grep -cE '^/Users/.*/claude\.app/Contents/MacOS/claude ')
other=$((total - 1))
[[ $other -lt 0 ]] && other=0

echo "$(date '+%Y-%m-%d %H:%M:%S') Stop: total=$total other=$other" >> "$LOG"

if [[ $other -eq 0 ]]; then
    /usr/bin/afplay "$CH/all-done.wav" 2>/dev/null &
else
    max=8
    n=$other
    [[ $n -gt $max ]] && n=$max
    for ((i=1; i<=n; i++)); do
        /usr/bin/afplay "$CH/beep_${i}.wav" 2>/dev/null
        sleep 0.08
    done
fi

lid=$(ioreg -r -k AppleClamshellState -d 4 | awk '/AppleClamshellState/{print $NF; exit}')
if [[ "$lid" == "Yes" && $other -eq 0 ]]; then
    nohup "$SLAB_BIN/claude-ping-repeat.sh" > /dev/null 2>&1 &
    nohup "$SLAB_BIN/claude-sleep-schedule.sh" 120 > /dev/null 2>&1 &
fi

exit 0
