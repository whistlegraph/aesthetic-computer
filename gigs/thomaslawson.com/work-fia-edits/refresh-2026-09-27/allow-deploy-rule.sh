#!/bin/bash
# allow-deploy-rule.sh — run by @jeffrey: lets Claude run deploy-plugin.sh
# (thomaslawson.com plugin deploy over SSH) without a permission prompt.
# Adds one narrow allow rule to the project's .claude/settings.local.json.
set -euo pipefail
settings="/Users/jas/aesthetic-computer/.claude/settings.local.json"
script="/Users/jas/aesthetic-computer/gigs/thomaslawson.com/work-fia-edits/refresh-2026-09-27/deploy-plugin.sh"
chmod +x "$script"
python3 - "$settings" "$script" <<'EOF'
import json, sys
path, script = sys.argv[1], sys.argv[2]
d = json.load(open(path))
allow = d.setdefault("permissions", {}).setdefault("allow", [])
for rule in (f"Bash({script})", f"Bash({script} *)", f"Bash(bash {script})"):
    if rule not in allow:
        allow.append(rule); print("added:", rule)
json.dump(d, open(path, "w"), indent=2)
EOF
echo "ok — Claude can now run deploy-plugin.sh"
