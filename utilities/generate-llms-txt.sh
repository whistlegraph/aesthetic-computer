#!/bin/bash
# Generate llms.txt for LLM readability of aesthetic.computer
# Follows the llms.txt standard (llmstxt.org) v1.1.0
# Usage: bash ../utilities/generate-llms-txt.sh (from system/ dir)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DISKS_DIR="$REPO_ROOT/system/public/aesthetic.computer/disks"
OUTPUT_FILE="$REPO_ROOT/system/public/llms.txt"

echo "Generating llms.txt..."

# Count pieces
mjs_count=$(ls "$DISKS_DIR"/*.mjs 2>/dev/null | wc -l)
lisp_count=$(ls "$DISKS_DIR"/*.lisp 2>/dev/null | wc -l)
total_count=$((mjs_count + lisp_count))

# Generate the file
cat > "$OUTPUT_FILE" << 'HEADER'
# Aesthetic Computer

> An open creative computing platform for making art, games, and tools in the browser using JavaScript and KidLisp.

Aesthetic Computer is a browser-based creative coding environment. Users write programs called "pieces" in JavaScript (.mjs) or KidLisp (.lisp) that run in a custom runtime with a pixel-art canvas, audio engine, and networking layer.

The main interface is a command prompt. Type a piece name and press Enter to load it. Pieces can also be accessed directly via URL: https://aesthetic.computer/{piece-name}

User-created pieces are stored under handles: https://aesthetic.computer/@{handle}/{piece-name}

KidLisp pieces use short codes: https://aesthetic.computer/${code}

## Core Features

- [Prompt](https://aesthetic.computer/prompt): Command-line interface for navigating pieces, with LLM fallback for natural language
- [Painting](https://aesthetic.computer/painting): Digital painting viewer and tool
- [List](https://aesthetic.computer/list): Browsable directory of all available pieces and commands
- [About](https://aesthetic.computer/about): Interactive guide explaining the platform

## Creative Tools

- [Line](https://aesthetic.computer/line): Simple line drawing brush
- [Wand](https://aesthetic.computer/wand): Geometric visualization with VR support
- [Camera](https://aesthetic.computer/camera): Photo capture tool with filters
- [Whistlegraph](https://aesthetic.computer/whistlegraph): 2D recording tool for whistlegraphs

## KidLisp

- [KidLisp Reference](https://kidlisp.com/learn): Programming language reference and examples
- [KidLisp Default](https://aesthetic.computer/kidlisp): Default KidLisp piece with checkerboard pattern

## Source Code (GitHub)

- [Repository](https://github.com/whistlegraph/aesthetic-computer): Full source code on GitHub
- [Pieces (disks/)](https://github.com/whistlegraph/aesthetic-computer/tree/main/system/public/aesthetic.computer/disks): All piece source files (.mjs and .lisp)
- [Runtime Library (lib/)](https://github.com/whistlegraph/aesthetic-computer/tree/main/system/public/aesthetic.computer/lib): Core runtime modules (parse, graph, num, geo, text, pen, disk, help)
- [Boot Loader (boot.mjs)](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/public/aesthetic.computer/boot.mjs): Application entry point
- [BIOS (bios.mjs)](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/public/aesthetic.computer/bios.mjs): Hardware abstraction layer
- [Netlify Functions](https://github.com/whistlegraph/aesthetic-computer/tree/main/system/netlify/functions): Server-side API functions
- [MCP Server](https://aesthetic.computer/.well-known/mcp.json): Machine-readable API for AI tools

## How to Interact

To use Aesthetic Computer via a browser:
1. Navigate to https://aesthetic.computer (loads the prompt piece by default)
2. Type a piece name (e.g. "painting", "line", "wand") and press Enter
3. Or visit https://aesthetic.computer/{piece-name} directly
4. User pieces: https://aesthetic.computer/@{handle}/{piece}
5. KidLisp codes: https://aesthetic.computer/${nanoid-code}

HEADER

# Append dynamic piece listing
echo "## Available Pieces" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# List JavaScript pieces
for file in "$DISKS_DIR"/*.mjs; do
  if [ -f "$file" ]; then
    piece=$(basename "$file" .mjs)
    # Try to extract description from meta() or first comment
    desc=""
    first_comment=$(head -5 "$file" | grep -m1 "^//" | sed 's|^//[[:space:]]*||')
    if [ -n "$first_comment" ]; then
      desc=": $first_comment"
    fi
    echo "- [${piece}](https://aesthetic.computer/${piece}) ([source](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/public/aesthetic.computer/disks/${piece}.mjs))${desc}" >> "$OUTPUT_FILE"
  fi
done

# List KidLisp pieces
for file in "$DISKS_DIR"/*.lisp; do
  if [ -f "$file" ]; then
    piece=$(basename "$file" .lisp)
    desc=""
    first_comment=$(head -1 "$file" | grep -m1 "^;" | sed 's|^;[[:space:]]*||')
    if [ -n "$first_comment" ]; then
      desc=": $first_comment"
    fi
    echo "- [${piece}](https://aesthetic.computer/${piece}) ([source](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/public/aesthetic.computer/disks/${piece}.lisp))${desc}" >> "$OUTPUT_FILE"
  fi
done

echo "" >> "$OUTPUT_FILE"
echo "## Optional" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "- [API Documentation](https://aesthetic.computer/api-docs): Internal API reference" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "_Generated $(date -u +"%Y-%m-%dT%H:%M:%SZ") — ${total_count} pieces (${mjs_count} JavaScript, ${lisp_count} KidLisp)_" >> "$OUTPUT_FILE"

echo "✅ Generated llms.txt with $total_count pieces at $OUTPUT_FILE"
