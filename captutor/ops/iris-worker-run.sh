#!/bin/bash
# Run one Iris assignment headlessly. Operational missions receive the fleet
# Frame MCP explicitly because print-mode workers do not inherit project MCPs.
set -u

export PATH=/opt/homebrew/bin:/Users/fusermacminipanda/.local/bin:/usr/bin:/bin
WT="$1"
LOG="$2"
PROMPTFILE="$3"
FRAME_MCP="${IRIS_FRAME_MCP:-$HOME/.hermes/mcp/frame/frame-mcp.mjs}"

set -a
source "$HOME/.hermes/.env" 2>/dev/null
set +a

export GH_CONFIG_DIR="$HOME/.config/gh-iris"
export GIT_AUTHOR_NAME=iris GIT_AUTHOR_EMAIL=iris@fuser.studio
export GIT_COMMITTER_NAME=iris GIT_COMMITTER_EMAIL=iris@fuser.studio

cd "$WT" || { echo "no worktree $WT" >> "$LOG"; exit 2; }
git config --local credential.helper '!gh auth git-credential' 2>/dev/null

MCP_ARGS=()
if [[ -f "$FRAME_MCP" ]]; then
  MCP_CONFIG="$(node -e 'process.stdout.write(JSON.stringify({mcpServers:{frame:{command:process.execPath,args:[process.argv[1]]}}}))' "$FRAME_MCP")"
  MCP_ARGS=(--mcp-config "$MCP_CONFIG")
elif [[ -n "${IRIS_MISSION_RECEIPT:-}" ]]; then
  echo "mission requires Frame MCP, but $FRAME_MCP is missing" >> "$LOG"
  echo "=== worker-run exit 3 $(date -u +%FT%TZ) ===" >> "$LOG"
  exit 3
fi

echo "=== worker-run start $(date -u +%FT%TZ) in $WT ===" >> "$LOG"
claude -p "$(cat "$PROMPTFILE")" --permission-mode bypassPermissions "${MCP_ARGS[@]}" >> "$LOG" 2>&1
STATUS=$?
if [[ -n "${CAPTUTOR_TASK_GID:-}" && "$STATUS" -eq 0 ]] \
    && ! grep -Fxq "CAPTUTOR_MISSION_ACCEPTED" "$LOG"; then
  echo "captutor worker withheld CAPTUTOR_MISSION_ACCEPTED; treating QA as failed" >> "$LOG"
  STATUS=4
fi
echo "=== worker-run exit $STATUS $(date -u +%FT%TZ) ===" >> "$LOG"
exit "$STATUS"
