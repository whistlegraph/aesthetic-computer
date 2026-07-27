#!/bin/zsh
# iris heartbeat — liveness plus bounded mission recovery. Runs every 5 min.
# It stamps presence, checks for a stopped Captutor mission, repairs a crashed
# Fuser renderer, and removes one local failure tombstone so the orchestrator
# can retry. The orchestrator still enforces current Asana ownership before it
# launches anything; the heartbeat cannot invent work or bypass assignment.
#
# Kill switch:
#   launchctl bootout gui/$(id -u)/ai.iris.heartbeat   # stop the pulse
export PATH=/opt/homebrew/bin:$PATH
LOG="$HOME/.hermes/logs/heartbeat.log"
mkdir -p "$HOME/.hermes/logs"

# read-only liveness checks
GW=$(pgrep -f "hermes.*gateway" >/dev/null 2>&1 && echo up || echo down)
BOARD=$(curl -s -o /dev/null -w "%{http_code}" http://127.0.0.1:9120/ 2>/dev/null)

# stamp the beat onto the board (lastHeartbeat + updatedAt), without touching items
/opt/homebrew/bin/node -e '
const fs=require("fs");
const p=process.env.HOME+"/.local/share/desktop-badge/mission.json";
try{
  const d=JSON.parse(fs.readFileSync(p,"utf8"));
  const now=new Date().toISOString();
  d.lastHeartbeat=now; d.updatedAt=now;
  fs.writeFileSync(p, JSON.stringify(d,null,2));
}catch(e){}' 2>/dev/null

echo "$(date -u +%FT%TZ) ♥ beat · gateway=$GW · board=$BOARD" >> "$LOG"

# Save a recoverable Captutor mission from a worker/browser stoppage. This is a
# strict one-retry supervisor by default, and it never draws Frame/Puppet UI.
RECOVERY="$HOME/Developer/captutor/ops/iris-heartbeat-recovery.mjs"
if [[ -f "$RECOVERY" ]]; then
  /opt/homebrew/bin/node "$RECOVERY" >> "$LOG" 2>&1
fi
