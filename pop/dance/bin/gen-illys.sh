#!/usr/bin/env bash
# gen-illys.sh — regenerate the trancenwaltzi midnight set for a given
# variant tag. 7 portrait IG illys (<VAR>p, 1024x1536) + 1 square album
# hero (<VAR>, 1024x1024, outro). Concurrency 3 (8GB machine cap).
#   usage: bash gen-illys.sh [VARIANT]   (default v41)
set -u
VAR="${1:-v41}"
REPO="/Users/jas/aesthetic-computer"
SECROOT="$HOME/Documents/Working Desktop/gens/trancenwaltzi-sections"
LOG="$HOME/Documents/Working Desktop/gens/trancenwaltzi-genillys-${VAR}.log"
: > "$LOG"
GEN="$REPO/marketing/bin/gen-promo.mjs"

jobs=(
  "intro|${VAR}p|1024x1536"
  "break1|${VAR}p|1024x1536"
  "build1|${VAR}p|1024x1536"
  "drop1|${VAR}p|1024x1536"
  "break2|${VAR}p|1024x1536"
  "build2|${VAR}p|1024x1536"
  "drop2|${VAR}p|1024x1536"
  "outro|${VAR}p|1024x1536"
)
# (square cover hero is derived by cropping outro <VAR>p — the 1024x1024
# gen path kept timing out server-side; crop is reliable + consistent.)

run_one() {
  local sec="$1" var="$2" size="$3"
  echo "▸ START $sec $var $size $(date +%H:%M:%S)" >> "$LOG"
  node "$GEN" "$SECROOT/$sec" --variant "$var" --size "$size" --force --no-mirror \
    >> "$LOG" 2>&1 \
    && echo "✓ DONE  $sec $var $(date +%H:%M:%S)" >> "$LOG" \
    || echo "✗ FAIL  $sec $var (exit $?) $(date +%H:%M:%S)" >> "$LOG"
}

i=0
for spec in "${jobs[@]}"; do
  IFS='|' read -r sec var size <<< "$spec"
  run_one "$sec" "$var" "$size" &
  i=$((i+1))
  if (( i % 3 == 0 )); then wait -n 2>/dev/null || wait; fi
done
wait
echo "=== ALL GENS COMPLETE $(date +%H:%M:%S) ===" >> "$LOG"
grep -E '✓ DONE|✗ FAIL' "$LOG"
