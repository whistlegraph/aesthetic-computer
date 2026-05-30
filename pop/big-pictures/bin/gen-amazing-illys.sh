#!/usr/bin/env bash
# gen-amazing-illys.sh — regenerate the amazing-grace rooftop hymn-circle
# set for a given variant tag. 7 portrait IG illys (1024×1536) + 1
# square COVER illy (1024×1024). Concurrency 3 (8 GB machine cap).
# Mirrors gen-illys-penta.sh.
#   usage: bash gen-amazing-illys.sh [VARIANT]   (default v1)
set -u
VAR="${1:-v1}"
REPO="/Users/jas/aesthetic-computer"
SECROOT="$HOME/Documents/Shelf/gens/amazing-grace-sections"
LOG="$HOME/Documents/Shelf/gens/amazing-grace-genillys-${VAR}.log"
: > "$LOG"
GEN="$REPO/marketing/bin/gen-promo.mjs"

jobs=(
  "verse1|${VAR}|1024x1536"
  "verse2|${VAR}|1024x1536"
  "verse3|${VAR}|1024x1536"
  "verse4|${VAR}|1024x1536"
  "verse5|${VAR}|1024x1536"
  "verse6|${VAR}|1024x1536"
  "verse7|${VAR}|1024x1536"
  "cover|${VAR}|1024x1024"
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
