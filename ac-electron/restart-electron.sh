#!/bin/bash
# Auto-restart wrapper for Electron app on macOS/Linux host
# This runs on the HOST machine, not in the devcontainer

cd "$(dirname "$0")"

echo "🎨 Aesthetic Computer - Auto-restart wrapper"
echo "→ Working directory: $(pwd)"
echo "→ Press Ctrl+C twice quickly to stop"
echo ""

RESTART_COUNT=0

while true; do
  if [ $RESTART_COUNT -gt 0 ]; then
    echo "♻️  Restarting Electron (restart #$RESTART_COUNT)..."
    sleep 1
  fi
  
  npm start
  EXIT_CODE=$?
  
  if [ $EXIT_CODE -eq 42 ]; then
    # Exit code 42 = intentional reboot request
    RESTART_COUNT=$((RESTART_COUNT + 1))
    echo ""
    echo "✓ Reboot request received"
  else
    # Any other exit code = stop
    echo ""
    echo "✓ Electron exited with code $EXIT_CODE"
    break
  fi
done

echo "👋 Goodbye!"
