#!/bin/zsh
set -euo pipefail

SCRIPT_DIR=${0:A:h}
SOURCE_DIR="$SCRIPT_DIR/posthog-on-demand"
CODEX_ROOT="${CODEX_HOME:-$HOME/.codex}"
CONFIG_PATH="$CODEX_ROOT/config.toml"
SKILL_TARGET="$CODEX_ROOT/skills/posthog-on-demand"
POSTHOG_MARKETPLACE="https://github.com/PostHog/ai-plugin.git"
POSTHOG_MCP="https://mcp.posthog.com/mcp"

command -v codex >/dev/null || { echo "codex is required" >&2; exit 1; }
command -v node >/dev/null || { echo "node is required" >&2; exit 1; }
command -v python3 >/dev/null || { echo "python3 is required" >&2; exit 1; }

mkdir -p "$CODEX_ROOT/skills" "$SKILL_TARGET"
rsync -a "$SOURCE_DIR/" "$SKILL_TARGET/"

if ! codex plugin marketplace list | awk '$1 == "posthog" { found = 1 } END { exit !found }'; then
  codex plugin marketplace add "$POSTHOG_MARKETPLACE"
fi

if ! codex plugin list | awk '$1 == "posthog@posthog" && $2 == "installed," { found = 1 } END { exit !found }'; then
  codex plugin add posthog@posthog
fi

mkdir -p "$CODEX_ROOT"
touch "$CONFIG_PATH"
python3 - "$CONFIG_PATH" <<'PY'
from pathlib import Path
import re
import shutil
import sys

path = Path(sys.argv[1])
text = path.read_text()
section = '[plugins."posthog@posthog"]'
pattern = re.compile(r'(?ms)^\[plugins\."posthog@posthog"\]\s*\n(.*?)(?=^\[|\Z)')
match = pattern.search(text)

if match:
    body = match.group(1)
    if re.search(r'(?m)^enabled\s*=', body):
        body = re.sub(r'(?m)^enabled\s*=.*$', 'enabled = false', body)
    else:
        body = 'enabled = false\n' + body
    updated = text[:match.start()] + section + '\n' + body + text[match.end():]
else:
    updated = text.rstrip() + f'\n\n{section}\nenabled = false\n'

if updated != text:
    if path.stat().st_size:
        shutil.copy2(path, path.with_suffix('.toml.posthog-on-demand.bak'))
    path.write_text(updated)
PY

if ! codex mcp get posthog >/dev/null 2>&1; then
  codex mcp add posthog --url "$POSTHOG_MCP"
fi

if codex mcp get posthog-skills >/dev/null 2>&1; then
  codex mcp remove posthog-skills
fi
codex mcp add posthog-skills -- node "$SKILL_TARGET/scripts/posthog-skills-mcp.mjs"

codex plugin list | awk '$1 == "posthog@posthog" { print }'
codex mcp list | awk '$1 == "posthog" || $1 == "posthog-skills" { print }'
