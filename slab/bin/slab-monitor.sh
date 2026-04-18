#!/bin/bash
# Slab resource monitor. Samples CPU/RSS of ambient-system processes every
# INTERVAL seconds and appends a JSONL line per process. Runs as a child of
# the lid-ambient daemon.
set -u
SLAB_HOME=${SLAB_HOME:-$HOME/.local/share/slab}

INTERVAL=${1:-15}
LOG_DIR="$SLAB_HOME/logs"
mkdir -p "$LOG_DIR"
LOG="$LOG_DIR/resources.jsonl"

PATTERNS=(
    "lid-ambient.sh"
    "lid-reactive.py"
    "claude-ping-repeat.sh"
    "claude-sleep-schedule.sh"
    "afplay"
)

trap 'exit 0' SIGTERM SIGINT

while true; do
    ts=$(date -u +%Y-%m-%dT%H:%M:%SZ)
    for pat in "${PATTERNS[@]}"; do
        ps -eo pid=,rss=,pcpu=,command= 2>/dev/null \
            | grep -E "$pat" \
            | grep -v grep \
            | grep -v slab-monitor.sh \
            | while read -r pid rss pcpu cmd; do
                printf '{"ts":"%s","pattern":"%s","pid":%s,"rss_kb":%s,"cpu_pct":%s}\n' \
                    "$ts" "$pat" "$pid" "$rss" "$pcpu"
            done
    done >> "$LOG"
    sleep "$INTERVAL"
done
