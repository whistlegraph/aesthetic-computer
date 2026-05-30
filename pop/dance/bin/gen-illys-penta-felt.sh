#!/usr/bin/env bash
# gen-illys-penta-felt.sh — regenerate the trancepenta storyline panels
# in FELT-TATTERED mode (matching the felt cover at
# `trancepenta-cover-final/cover-prompt.txt`). Writes a per-section
# `cover-prompt.felt.txt` from the template + section colour, then runs
# gen-promo with `--prompt-file cover-prompt.felt.txt --variant vfelt`.
# Concurrency 3 on the 8 GB machine.
#   usage: bash gen-illys-penta-felt.sh
set -u
REPO="/Users/jas/aesthetic-computer"
SECROOT="$HOME/Documents/Shelf/gens/trancepenta-sections"
TEMPLATE="$REPO/pop/dance/bin/cover-prompt.felt-portrait.template.txt"
LOG="$HOME/Documents/Shelf/gens/trancepenta-genillys-vfelt.log"
: > "$LOG"
GEN="$REPO/marketing/bin/gen-promo.mjs"

# section | LID_GLOW | SECTION_LIGHTING (richer phrase for the wash)
specs=(
  "intro|drained slate-blue|a drained slate-blue wash, low and cold like a power-saving fluorescent that's been on too long"
  "break1|cold cyan-grey|a cold cyan-grey wash, the kind of colour your skin goes under a freezer-aisle light"
  "build1|dim sick teal|a dim sick-teal wash, fog-heavy and faintly sour, the lighting of a long stalled night"
  "drop1|cold cyan-blue|a cold cyan-blue wash, dominant on the laptop, slicing forward through the fog like a small searchlight"
  "break2|bruised violet-grey|a bruised violet-grey wash, the bruise-toned hush of a store after the last customer is long gone"
  "build2|acid grey-green|an acid grey-green wash low on the floor, a sour reflection rising from spilled fluorescents"
  "drop2|cold steel-blue|a cold steel-blue wash, the DOMINANT central beam on the laptop, harder and more clinical than drop1"
  "outro|ashen grey-blue|an ashen grey-blue wash, fully dimmed, the lights drained — almost the colour of held breath"
)

stamp_prompt() {
  local sec="$1" lid="$2" sect="$3"
  local out="$SECROOT/$sec/cover-prompt.felt.txt"
  # NB: sed with delimiters that don't clash with prose.
  sed -e "s|{{LID_GLOW}}|${lid}|g" -e "s|{{SECTION_LIGHTING}}|${sect}|g" "$TEMPLATE" > "$out"
  echo "▸ stamped $sec → $out  (LID=$lid)" >> "$LOG"
}

run_one() {
  local sec="$1"
  echo "▸ START $sec $(date +%H:%M:%S)" >> "$LOG"
  node "$GEN" "$SECROOT/$sec" \
    --prompt-file cover-prompt.felt.txt \
    --variant vfelt --size 1024x1536 --force --no-mirror \
    >> "$LOG" 2>&1 \
    && echo "✓ DONE  $sec $(date +%H:%M:%S)" >> "$LOG" \
    || echo "✗ FAIL  $sec (exit $?) $(date +%H:%M:%S)" >> "$LOG"
}

# 1) stamp prompts (cheap, sequential)
for spec in "${specs[@]}"; do
  IFS='|' read -r sec lid sect <<< "$spec"
  stamp_prompt "$sec" "$lid" "$sect"
done

# 2) regen (concurrency 3 to stay under 8 GB pressure)
i=0
for spec in "${specs[@]}"; do
  IFS='|' read -r sec _ _ <<< "$spec"
  run_one "$sec" &
  i=$((i+1))
  if (( i % 3 == 0 )); then wait -n 2>/dev/null || wait; fi
done
wait
echo "=== ALL FELT GENS COMPLETE $(date +%H:%M:%S) ===" >> "$LOG"
grep -E '✓ DONE|✗ FAIL' "$LOG"
