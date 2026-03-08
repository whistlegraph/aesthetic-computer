#!/usr/bin/env fish
#
# Compile Keeps FA2 SmartPy contracts.
#
# Usage:
#   ./compile.fish            # default: v7 (KeepsFA2v7)
#   ./compile.fish v7
#   ./compile.fish v6
#   ./compile.fish v5
#   ./compile.fish v4
#   ./compile.fish v3
#   ./compile.fish v2
#
# Notes:
# - v7 is the KidLisp Keeps final production contract source.
# - v5/v5rc remain available for release-candidate compatibility.
#

set -l SCRIPT_DIR (dirname (status filename))
cd $SCRIPT_DIR

set -l target v7
if test (count $argv) -gt 0
    set target (string lower -- $argv[1])
end

set -l source_file
set -l output_dir
set -l label

switch $target
    case v7
        set source_file kidlisp_keeps_fa2_v7.py
        set output_dir KeepsFA2v7
        set label "v7 (KidLisp Keeps final production)"
    case v6
        set source_file kidlisp_keeps_fa2_v6.py
        set output_dir KeepsFA2v6
        set label "v6 (KidLisp Keeps production)"
    case v5 v5rc rc
        set source_file keeps_fa2_v5.py
        set output_dir KeepsFA2v5
        set label "v5 release candidate"
    case v4
        set source_file keeps_fa2_v4.py
        set output_dir KeepsFA2v4
        set label "v4"
    case v3
        set source_file keeps_fa2_v3.py
        set output_dir KeepsFA2v3
        set label "v3"
    case v2
        set source_file keeps_fa2_v2.py
        set output_dir KeepsFA2v2
        set label "v2"
    case '*'
        echo "❌ Unknown target: $target"
        echo "   Use one of: v7, v6, v5, v4, v3, v2"
        exit 1
end

# Ensure virtualenv exists
if not test -d .venv
    echo "Creating virtual environment..."
    python -m venv .venv
    source .venv/bin/activate.fish
    pip install smartpy-tezos
else
    source .venv/bin/activate.fish
end

echo "Compiling $label from $source_file..."
python $source_file

if test $status -eq 0
    echo ""
    echo "✅ Compilation successful!"
    echo ""
    echo "Output files:"
    ls -la $output_dir/*.tz 2>/dev/null
    echo ""
    echo "Contract code:   $output_dir/step_002_cont_0_contract.tz"
    echo "Initial storage: $output_dir/step_002_cont_0_storage.tz"
else
    echo ""
    echo "❌ Compilation failed!"
    exit 1
end
