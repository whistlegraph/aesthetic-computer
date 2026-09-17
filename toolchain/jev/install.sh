#!/bin/bash
# Register the same portable launcher in each installed local MCP client.
set -euo pipefail
JEV_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
case "${1:-}" in
  ''|--codex|--claude) ;;
  *) echo 'Usage: install.sh [--codex | --claude]' >&2; exit 1 ;;
esac
installed=0
if [ "${1:-}" != '--claude' ] && command -v codex >/dev/null 2>&1; then
  codex mcp add oskiewar-coach -- /bin/bash "$JEV_DIR/run.sh"
  installed=$((installed + 1))
fi
if [ "${1:-}" != '--codex' ] && command -v claude >/dev/null 2>&1; then
  # Only replace this named user-scope entry; leave project/local entries alone.
  claude mcp remove --scope user oskiewar-coach >/dev/null 2>&1 || true
  claude mcp add --scope user --transport stdio oskiewar-coach -- /bin/bash "$JEV_DIR/run.sh"
  installed=$((installed + 1))
fi
if [ "$installed" -eq 0 ]; then
  echo 'No requested MCP client found on PATH.' >&2
  exit 1
fi
echo 'Registered oskiewar-coach. Reload your MCP connections or start a new client session.'
