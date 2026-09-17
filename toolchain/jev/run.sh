#!/bin/bash
# Resolve repo, runtime and private credentials on the machine running MCP.
set -euo pipefail
JEV_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JEV_NODE="${JEV_NODE:-$HOME/.local/share/fnm/aliases/default/bin/node}"
if [ ! -x "$JEV_NODE" ]; then
  JEV_NODE="$(command -v node || true)"
fi
if [ -z "$JEV_NODE" ]; then
  echo 'Node 22+ is required. Install Node or set JEV_NODE.' >&2
  exit 1
fi
"$JEV_NODE" -e 'if (Number(process.versions.node.split(".")[0]) < 22) { console.error("Node 22+ is required."); process.exit(1); }'
JEV_ENV_FILE="${JEV_ENV_FILE:-$HOME/.config/aesthetic-computer/jev.env}"
JEV_COMMAND=("$JEV_NODE")
if [ -f "$JEV_ENV_FILE" ]; then
  JEV_COMMAND+=("--env-file=$JEV_ENV_FILE")
fi
case "${1:-}" in
  '') exec "${JEV_COMMAND[@]}" "$JEV_DIR/coach.mjs" ;;
  --smoke) exec "${JEV_COMMAND[@]}" "$JEV_DIR/evaluate.mjs" < "$JEV_DIR/smoke.json" ;;
  --test) exec "$JEV_NODE" --test "$JEV_DIR/jev.test.mjs" "$JEV_DIR/../../xbox/live/tests/coach.test.mjs" ;;
  *) echo 'Usage: run.sh [--smoke | --test]' >&2; exit 1 ;;
esac
