#!/bin/bash
# Collect local evidence after a dropout; does not reset audio or upload data.
set -euo pipefail
minutes="${1:-10}"
if ! [[ "$minutes" =~ ^[1-9][0-9]?$ ]] || (( minutes > 60 )); then
    printf 'Usage: bash audio-diagnostics.sh [minutes: 1..60]\n' >&2
    exit 2
fi
diagnostic_root="$HOME/Library/Logs/MenuBand"
mkdir -p "$diagnostic_root"
diagnostic_dir="$(mktemp -d "$diagnostic_root/report-$(date '+%Y%m%d-%H%M%S').XXXXXX")"
for name in audio-diagnostics.jsonl audio-diagnostics.previous.jsonl; do
    if [[ -f "$diagnostic_root/$name" ]]; then
        cp "$diagnostic_root/$name" "$diagnostic_dir/$name"
    fi
done
if [[ -f /tmp/menuband.err ]]; then
    tail -n 400 /tmp/menuband.err > "$diagnostic_dir/app.log"
fi
/usr/bin/log show --last "${minutes}m" --style compact --predicate '
    (process == "usbaudiod" AND
        (eventMessage CONTAINS "transaction status error" OR
         eventMessage CONTAINS "requestConfigurationChange" OR
         eventMessage CONTAINS "performStartIO" OR
         eventMessage CONTAINS "performStopIO")) OR
    (process == "coreaudiod" AND
        (eventMessage CONTAINS "HandlePlugIn_RequestConfigChange" OR
         eventMessage CONTAINS "IO Stopped Context" OR
         eventMessage CONTAINS "Overload possibly" OR
         eventMessage CONTAINS "NonFinites" OR
         eventMessage CONTAINS "ClientTimeoutStart"))
' > "$diagnostic_dir/driver.log"
printf '%s\n' "$diagnostic_dir"
