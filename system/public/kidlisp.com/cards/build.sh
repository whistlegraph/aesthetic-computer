#!/usr/bin/env bash
# Build all KidLisp reference cards from LaTeX to SVG
# Usage: ./build.sh [card-name] (build one card or all)

set -e
cd "$(dirname "$0")"

SRC_DIR="src"
SVG_DIR="svg"
TMP_DIR="/tmp/kidlisp-cards"

mkdir -p "$SVG_DIR" "$TMP_DIR"

build_card() {
  local tex_file="$1"
  local name=$(basename "$tex_file" .tex)
  
  # Skip template
  [[ "$name" == "template" ]] && return
  
  echo "📄 Building $name..."
  
  # Compile to PDF
  if pdflatex -interaction=nonstopmode -output-directory="$TMP_DIR" "$tex_file" > /dev/null 2>&1; then
    # Convert to SVG
    if pdf2svg "$TMP_DIR/${name}.pdf" "$SVG_DIR/${name}.svg"; then
      echo "✅ $name.svg"
    else
      echo "❌ $name - pdf2svg failed"
      return 1
    fi
  else
    echo "❌ $name - pdflatex failed"
    cat "$TMP_DIR/${name}.log" | tail -20
    return 1
  fi
}

# Build specific card or all
if [[ -n "$1" ]]; then
  if [[ -f "$SRC_DIR/$1.tex" ]]; then
    build_card "$SRC_DIR/$1.tex"
  else
    echo "Card not found: $1"
    exit 1
  fi
else
  count=0
  for tex in "$SRC_DIR"/*.tex; do
    [[ -f "$tex" ]] || continue
    build_card "$tex" && ((count++)) || true
  done
  echo ""
  echo "🃏 Built $count cards"
fi

# Cleanup
rm -rf "$TMP_DIR"
