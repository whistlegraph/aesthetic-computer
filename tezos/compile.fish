#!/usr/bin/env fish
#
# Compile the Keeps FA2 v2 SmartPy contract
#
# Usage: ./compile.fish
#
# This will create output in the KeepsFA2v2/ directory with:
# - step_002_cont_0_contract.tz  (Michelson code)
# - step_002_cont_0_storage.tz   (Initial storage)
#

set -l SCRIPT_DIR (dirname (status filename))
cd $SCRIPT_DIR

# Ensure virtualenv exists
if not test -d .venv
    echo "Creating virtual environment..."
    python -m venv .venv
    source .venv/bin/activate.fish
    pip install smartpy-tezos
else
    source .venv/bin/activate.fish
end

echo "Compiling keeps_fa2_v2.py..."
python keeps_fa2_v2.py

if test $status -eq 0
    echo ""
    echo "✅ Compilation successful!"
    echo ""
    echo "Output files:"
    ls -la KeepsFA2v2/*.tz 2>/dev/null
    echo ""
    echo "Contract code:   KeepsFA2v2/step_002_cont_0_contract.tz"
    echo "Initial storage: KeepsFA2v2/step_002_cont_0_storage.tz"
else
    echo ""
    echo "❌ Compilation failed!"
    exit 1
end
