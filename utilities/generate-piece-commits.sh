#!/bin/bash
# Generate piece commit data for the list piece
# This script creates a JSON file with last commit info for each piece
# Usage: bash ../utilities/generate-piece-commits.sh (from system/ dir)

# Find repo root (script may be called from system/ dir on Netlify)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DISKS_DIR="$REPO_ROOT/system/public/aesthetic.computer/disks"
OUTPUT_FILE="$REPO_ROOT/system/public/.piece-commits.json"

echo "Generating piece commit data..."

# Start JSON object
echo "{" > "$OUTPUT_FILE"
echo '  "commits": {' >> "$OUTPUT_FILE"

first=true

# Loop through all .mjs files in disks/
for file in "$DISKS_DIR"/*.mjs; do
  if [ -f "$file" ]; then
    # Get piece name (filename without .mjs)
    piece=$(basename "$file" .mjs)

    # Get last commit info for this file
    commit_info=$(git -C "$REPO_ROOT" log -1 --format="%H|%ai|%an|%s" -- "$file" 2>/dev/null)

    if [ -n "$commit_info" ]; then
      # Split commit info
      IFS='|' read -r hash date author message <<< "$commit_info"

      # Truncate hash to 7 chars
      short_hash="${hash:0:7}"

      # Escape quotes in message
      message=$(echo "$message" | sed 's/"/\\"/g' | head -c 100)

      # Add comma if not first entry
      if [ "$first" = false ]; then
        echo "," >> "$OUTPUT_FILE"
      fi
      first=false

      # Write JSON entry
      echo -n "    \"$piece\": {" >> "$OUTPUT_FILE"
      echo -n "\"hash\":\"$short_hash\"," >> "$OUTPUT_FILE"
      echo -n "\"date\":\"$date\"," >> "$OUTPUT_FILE"
      echo -n "\"author\":\"$author\"," >> "$OUTPUT_FILE"
      echo -n "\"message\":\"$message\"" >> "$OUTPUT_FILE"
      echo -n "}" >> "$OUTPUT_FILE"
    fi
  fi
done

# Close JSON
echo "" >> "$OUTPUT_FILE"
echo '  },' >> "$OUTPUT_FILE"
echo "  \"generated\": \"$(date -u +"%Y-%m-%dT%H:%M:%SZ")\"" >> "$OUTPUT_FILE"
echo "}" >> "$OUTPUT_FILE"

echo "✅ Generated commit data for pieces in $OUTPUT_FILE"
piece_count=$(grep -c "\"hash\"" "$OUTPUT_FILE")
echo "   Found commits for $piece_count pieces"
