#!/bin/bash
# Build the Aesel stack figure: PDF, 300-dpi PNG and SVG next to the source.
set -euo pipefail
cd "$(dirname "$0")"
xelatex -interaction=nonstopmode -halt-on-error stack.tex >build.log 2>&1 || { tail -40 build.log; exit 1; }
pdftocairo -png -transp -r 300 -singlefile stack.pdf stack
pdftocairo -svg stack.pdf stack.svg
rm -f stack.aux stack.log
ls -la stack.pdf stack.png stack.svg
