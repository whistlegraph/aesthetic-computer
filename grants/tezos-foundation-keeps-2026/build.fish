#!/usr/bin/env fish
# build.fish — Tezos Foundation grant proposal PDF (xelatex, AC paper style).
# Two passes for hyperref + tabularx layout settling.
cd (dirname (status filename))
set tex proposal.tex

for pass in 1 2
    xelatex -interaction=nonstopmode -halt-on-error $tex >/dev/null
end

rm -f *.aux *.log *.out
echo "done — proposal.pdf"
