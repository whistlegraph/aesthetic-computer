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

# Screenshots are half of a bug report: attachment_get_data reads a message's
# file by id. Off in the server by default; set to false to turn it back off.
export SLACK_MCP_ATTACHMENT_TOOL="${SLACK_MCP_ATTACHMENT_TOOL:-true}"

# slack-proxy adds files_upload, which the server lacks (see slack-proxy.mjs).
exec node "$REPO/toolchain/mcp/slack-proxy.mjs" node "$REPO/toolchain/vault/launch.mjs" slack "$BIN" --transport stdio "$@"
