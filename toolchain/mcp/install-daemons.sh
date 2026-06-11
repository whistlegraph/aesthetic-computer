#!/usr/bin/env bash
# install-daemons.sh — run mail-mcp + emacs-mcp as ONE shared HTTP daemon
# each (launchd), instead of one stdio process per Claude session.
#
# Why: Claude Code spawns every stdio MCP server fresh per session, so N
# parallel sessions cost N copies of each server. The daemons listen on
# localhost and every session connects over streamable HTTP instead:
#   mail-mcp   → http://127.0.0.1:7765/mcp
#   emacs-mcp  → http://127.0.0.1:7766/mcp
#
# After installing, point Claude at them with same-name local-scope
# overrides (local scope shadows the stdio entries in .mcp.json):
#   claude mcp add --transport http --scope local mail  http://127.0.0.1:7765/mcp
#   claude mcp add --transport http --scope local emacs http://127.0.0.1:7766/mcp
#
# Idempotent: re-running rewrites the plists and restarts both daemons.

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
NODE="$HOME/.local/share/fnm/aliases/default/bin/node"
[ -x "$NODE" ] || NODE="$(command -v node)"
AGENTS="$HOME/Library/LaunchAgents"
mkdir -p "$AGENTS"

install_one() {
  local label="$1" script="$2" port="$3"
  local plist="$AGENTS/$label.plist"
  cat > "$plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTD/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>$label</string>
    <key>ProgramArguments</key>
    <array>
        <string>$NODE</string>
        <string>$script</string>
        <string>--http</string>
        <string>$port</string>
    </array>
    <key>EnvironmentVariables</key>
    <dict>
        <key>HOME</key>
        <string>$HOME</string>
        <key>PATH</key>
        <string>/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin</string>
    </dict>
    <key>RunAtLoad</key>
    <true/>
    <key>KeepAlive</key>
    <true/>
    <key>ThrottleInterval</key>
    <integer>5</integer>
    <key>StandardOutPath</key>
    <string>/tmp/$label.out</string>
    <key>StandardErrorPath</key>
    <string>/tmp/$label.err</string>
</dict>
</plist>
EOF
  launchctl bootout "gui/$(id -u)/$label" 2>/dev/null || true
  launchctl bootstrap "gui/$(id -u)" "$plist"
  echo "✓ $label → 127.0.0.1:$port ($script)"
}

install_one computer.aesthetic.mail-mcp "$REPO/ants/mail-mcp/server.mjs" 7765
install_one computer.aesthetic.emacs-mcp "$REPO/artery/emacs-mcp.mjs" 7766
