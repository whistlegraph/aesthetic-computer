#!/usr/bin/env bash
# slack-mcp.sh — launch slack-mcp-server with its tokens read from the vault,
# so ~/.claude.json holds a command instead of a pair of plaintext Slack tokens.
#
# `exec` matters: the server must inherit our stdin/stdout to speak JSON-RPC,
# and replacing this shell keeps the cost at one process per session. (The old
# `npx -y slack-mcp-server` entry spawned three: npm exec → node shim → binary.)
#
# Anything we print on stdout would corrupt the protocol, so diagnostics go to
# stderr and we exit non-zero — Claude surfaces that as a failed MCP server
# rather than a server that connects and then misbehaves.

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENV_FILE="${SLACK_MCP_ENV:-$REPO/aesthetic-computer-vault/mcp/slack.env}"

if [ ! -r "$ENV_FILE" ]; then
  echo "slack-mcp: cannot read $ENV_FILE" >&2
  echo "  vault locked?  fish $REPO/aesthetic-computer-vault/vault-tool.fish unlock" >&2
  exit 1
fi

set -a
# shellcheck disable=SC1090
. "$ENV_FILE"
set +a

# The global install lives under whichever node fnm currently defaults to, so
# glob rather than hardcode the version. SLACK_MCP_BIN overrides.
BIN="${SLACK_MCP_BIN:-}"
if [ -z "$BIN" ]; then
  for candidate in \
    "$HOME"/.local/share/fnm/node-versions/*/installation/lib/node_modules/slack-mcp-server/node_modules/slack-mcp-server-darwin-arm64/bin/slack-mcp-server-darwin-arm64 \
    /opt/homebrew/lib/node_modules/slack-mcp-server/node_modules/slack-mcp-server-darwin-arm64/bin/slack-mcp-server-darwin-arm64
  do
    [ -x "$candidate" ] && { BIN="$candidate"; break; }
  done
fi

if [ -z "$BIN" ]; then
  echo "slack-mcp: server binary not found — npm i -g slack-mcp-server@1.3.0" >&2
  exit 1
fi

exec "$BIN" --transport stdio "$@"
