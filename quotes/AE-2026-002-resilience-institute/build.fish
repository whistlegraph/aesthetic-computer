#!/usr/bin/env fish
# build.fish — builds AE-2026-002 in color and grayscale.
# Each variant runs xelatex twice (remember-picture overlays need two passes).
# Grayscale defines \GRAYSCALE, which flips \ifgraymode in the source.

cd (dirname (status filename))
set tex AE-2026-002.tex

echo "→ color"
for pass in 1 2
    xelatex -interaction=nonstopmode -halt-on-error $tex >/dev/null
end

echo "→ grayscale"
for pass in 1 2
    xelatex -interaction=nonstopmode -halt-on-error \
        -jobname AE-2026-002-grayscale "\def\GRAYSCALE{}\input{$tex}" >/dev/null
end

rm -f *.aux *.log *.out
echo "done — AE-2026-002.pdf · AE-2026-002-grayscale.pdf"
