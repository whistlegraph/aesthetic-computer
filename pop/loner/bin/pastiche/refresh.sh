#!/usr/bin/env bash
# One move after a wizard session: export the chosen takes, score them
# against the footage's ground truth, and re-chrome the reel. The loop:
#   wizard.sh -> draw / retake words -> q -> refresh.sh -> watch
set -euo pipefail
cd "$(dirname "$0")"
python3 wizard-export.py
python3 wizard-accuracy.py || true
node chrome-reel.mjs
echo "reel refreshed with the latest performance"
