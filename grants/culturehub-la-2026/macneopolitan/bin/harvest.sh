#!/usr/bin/env bash
# harvest.sh — one band member's autobiography-of-use facts, as JSON on stdout.
#
#   ./harvest.sh              this machine
#   ./harvest.sh blueberry    that machine, over ssh (remote shell may be fish;
#                             we always land in bash via 'bash -s')
#
# Truthful, low-ceremony, dependency-free. Serial numbers are truncated to
# their last four characters on purpose — enough for a band member to be
# recognized, not enough to impersonate one.

set -euo pipefail

if [ -n "${1:-}" ] && [ "$1" != "local" ]; then
  exec ssh "$1" 'bash -s' -- local < "$0"
fi

hw=$(system_profiler SPHardwareDataType 2>/dev/null)
field() { echo "$hw" | awk -F': ' "/$1/ {print \$2; exit}"; }

model=$(field "Model Name")
model_id=$(field "Model Identifier")
chip=$(field "Chip")
memory=$(field "Memory")
serial=$(field "Serial Number")
serial_tail=${serial: -4}

born=$(stat -f "%SB" -t "%Y-%m-%dT%H:%M:%S" /private/var/db/.AppleSetupDone 2>/dev/null || echo null)

pw=$(system_profiler SPPowerDataType 2>/dev/null)
cycles=$(echo "$pw" | awk -F': ' '/Cycle Count/ {print $2; exit}')
condition=$(echo "$pw" | awk -F': ' '/Condition/ {print $2; exit}')
capacity=$(echo "$pw" | awk -F': ' '/Maximum Capacity/ {print $2; exit}')

os=$(sw_vers -productVersion 2>/dev/null || echo "?")

H="$HOME/.local/share/fish/fish_history"
shell_count=0 shell_since=null top_json="[]"
if [ -f "$H" ]; then
  shell_count=$(grep -c '^- cmd:' "$H" || echo 0)
  first_epoch=$(grep -m1 'when:' "$H" | awk '{print $2}')
  [ -n "$first_epoch" ] && shell_since="\"$(date -r "$first_epoch" '+%Y-%m-%d')\""
  top_json=$(grep '^- cmd:' "$H" | sed 's/^- cmd: //' | awk '{print $1}' \
    | sort | uniq -c | sort -rn | head -8 \
    | awk '{printf "%s{\"cmd\":\"%s\",\"count\":%s}", (NR>1?",":""), $2, $1}')
  top_json="[$top_json]"
fi

sessions=0 sessions_since=null
if [ -d "$HOME/.ac-agent-memory/sessions" ]; then
  sessions=$(ls "$HOME/.ac-agent-memory/sessions" | wc -l | tr -d ' ')
  sessions_since="\"$(stat -f "%SB" -t "%Y-%m-%d" "$HOME/.ac-agent-memory/device-id" 2>/dev/null)\""
fi

mb() { defaults read computer.aestheticcomputer.menuband "$1" 2>/dev/null || echo null; }
program=$(mb "notepat.melodicProgram")
backend=$(mb "notepat.instrumentBackend")
radio=$(mb "notepat.radioStation")

cat <<JSON
{
  "hostname": "$(hostname -s)",
  "model": "$model",
  "model_identifier": "$model_id",
  "chip": "$chip",
  "memory": "$memory",
  "serial_tail": "$serial_tail",
  "os": "$os",
  "born": "$born",
  "battery": { "cycles": ${cycles:-null}, "condition": "${condition:-?}", "capacity": "${capacity:-?}" },
  "shell": { "commands": ${shell_count:-0}, "since": ${shell_since}, "top": ${top_json} },
  "agent_sessions": { "count": ${sessions}, "since": ${sessions_since} },
  "menuband": { "melodic_program": ${program}, "backend": "${backend}", "radio": "${radio}" },
  "harvested_at": "$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
}
JSON
