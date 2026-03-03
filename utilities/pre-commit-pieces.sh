#!/bin/bash
# Pre-commit hook: auto-update .piece-commits.json and docs.js stubs
# for any new or modified pieces in disks/.
#
# Install: cp utilities/pre-commit-pieces.sh .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit

REPO_ROOT="$(git rev-parse --show-toplevel)"
DISKS_DIR="$REPO_ROOT/system/public/aesthetic.computer/disks"
COMMITS_FILE="$REPO_ROOT/system/public/.piece-commits.json"
DOCS_FILE="$REPO_ROOT/system/netlify/functions/docs.js"

# --- 1. Regenerate .piece-commits.json ---

# Check if any piece files are being committed
PIECE_CHANGES=$(git diff --cached --name-only -- "system/public/aesthetic.computer/disks/*.mjs" 2>/dev/null)

if [ -n "$PIECE_CHANGES" ] || [ ! -f "$COMMITS_FILE" ]; then
  echo "🔄 Regenerating .piece-commits.json..."

  echo "{" > "$COMMITS_FILE"
  echo '  "commits": {' >> "$COMMITS_FILE"

  first=true

  for file in "$DISKS_DIR"/*.mjs; do
    [ -f "$file" ] || continue
    piece=$(basename "$file" .mjs)

    # For staged new files that have no git history yet
    if git diff --cached --name-only --diff-filter=A 2>/dev/null | grep -q "disks/$piece.mjs"; then
      hash="0000000"
      date="$(date -u +"%Y-%m-%d %H:%M:%S +0000")"
      author="$(git config user.name)"
      message="(new piece)"
    else
      commit_info=$(git log -1 --format="%H|%ai|%an|%s" -- "$file" 2>/dev/null)
      if [ -z "$commit_info" ]; then
        continue
      fi
      IFS='|' read -r hash date author message <<< "$commit_info"
      hash="${hash:0:7}"
      message=$(echo "$message" | sed 's/"/\\"/g' | head -c 100)
    fi

    if [ "$first" = false ]; then
      echo "," >> "$COMMITS_FILE"
    fi
    first=false

    echo -n "    \"$piece\": {" >> "$COMMITS_FILE"
    echo -n "\"hash\":\"$hash\"," >> "$COMMITS_FILE"
    echo -n "\"date\":\"$date\"," >> "$COMMITS_FILE"
    echo -n "\"author\":\"$author\"," >> "$COMMITS_FILE"
    echo -n "\"message\":\"$message\"" >> "$COMMITS_FILE"
    echo -n "}" >> "$COMMITS_FILE"
  done

  echo "" >> "$COMMITS_FILE"
  echo '  },' >> "$COMMITS_FILE"
  echo "  \"generated\": \"$(date -u +"%Y-%m-%dT%H:%M:%SZ")\"" >> "$COMMITS_FILE"
  echo "}" >> "$COMMITS_FILE"

  git add "$COMMITS_FILE"
  echo "   ✅ Updated .piece-commits.json"
fi

# --- 2. Auto-add new pieces to docs.js ---

NEW_PIECES=$(git diff --cached --name-only --diff-filter=A -- "system/public/aesthetic.computer/disks/*.mjs" 2>/dev/null)

if [ -n "$NEW_PIECES" ]; then
  DOCS_CHANGED=false

  for file in $NEW_PIECES; do
    piece=$(basename "$file" .mjs)

    # Skip if already in docs.js
    if grep -q "\"$piece\":\|[[:space:]]$piece:" "$DOCS_FILE" 2>/dev/null; then
      continue
    fi

    # Extract description from line 2 comment (// Description text)
    desc=""
    if [ -f "$REPO_ROOT/$file" ]; then
      desc=$(sed -n '2s|^// *||p' "$REPO_ROOT/$file" | sed 's/"/\\"/g' | head -c 120)
    fi

    # Find the closing }, of the pieces section (last one before the `};` that closes the docs object)
    # The pieces section ends at the last `    },` line in docs.js
    CLOSE_LINE=$(grep -n "^    }," "$DOCS_FILE" | tail -1 | cut -d: -f1)

    if [ -n "$CLOSE_LINE" ]; then
      # Insert stub before the closing brace using a temp file (portable)
      head -n $((CLOSE_LINE - 1)) "$DOCS_FILE" > "$DOCS_FILE.tmp"
      cat >> "$DOCS_FILE.tmp" << STUB
      "$piece": {
        sig: "$piece",
        desc: "$desc",
        done: false,
      },
STUB
      tail -n +"$CLOSE_LINE" "$DOCS_FILE" >> "$DOCS_FILE.tmp"
      mv "$DOCS_FILE.tmp" "$DOCS_FILE"
      DOCS_CHANGED=true
      echo "   ✅ Added '$piece' stub to docs.js"
    fi
  done

  if [ "$DOCS_CHANGED" = true ]; then
    git add "$DOCS_FILE"
  fi
fi

exit 0
