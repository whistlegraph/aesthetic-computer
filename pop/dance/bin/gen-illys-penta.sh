#!/usr/bin/env bash
# gen-illys-penta.sh — regenerate the trancepenta after-hours-Trader-Joe's
# set for a given variant tag. 8 portrait IG illys (<VAR>p input → png at
# 1024x1536). Concurrency 3 (8GB machine cap). Mirrors gen-illys.sh
# (trancenwaltzi) but rooted at trancepenta-sections.
#   usage: bash gen-illys-penta.sh [VARIANT]   (default v2)
set -u
VAR="${1:-v2}"
REPO="/Users/jas/aesthetic-computer"
SECROOT="$HOME/Documents/Shelf/gens/trancepenta-sections"
LOG="$HOME/Documents/Shelf/gens/trancepenta-genillys-${VAR}.log"
: > "$LOG"
GEN="$REPO/marketing/bin/gen-promo.mjs"

jobs=(
  "intro|${VAR}|1024x1536"
  "break1|${VAR}|1024x1536"
  "build1|${VAR}|1024x1536"
  "drop1|${VAR}|1024x1536"
  "break2|${VAR}|1024x1536"
  "build2|${VAR}|1024x1536"
  "drop2|${VAR}|1024x1536"
  "outro|${VAR}|1024x1536"
)

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
