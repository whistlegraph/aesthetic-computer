#!/usr/bin/env fish
# toolchain/resolve/setup.fish
# Preflight + install for the DaVinci Resolve MCP (Tier 1, local).
# Run this AFTER DaVinci Resolve is installed. It checks prerequisites,
# then hands off to the upstream installer which auto-wires Claude Code.
#
#   ./toolchain/resolve/setup.fish

set -l app_glob /Applications/DaVinci\ Resolve*
set -l modules "/Library/Application Support/Blackmagic Design/DaVinci Resolve/Developer/Scripting/Modules"

echo "▸ DaVinci Resolve MCP — preflight"

# 1. Resolve installed?
if not test -d /Applications/DaVinci\ Resolve.app -o -d /Applications/DaVinci\ Resolve\ Studio.app
    echo "  ✗ Resolve not found in /Applications. Install it first, then re-run."
    exit 1
end
echo "  ✓ Resolve app present"

# 2. Scripting modules present (ships with the app install)?
if not test -d "$modules"
    echo "  ⚠ Scripting modules dir missing: $modules"
    echo "    (Usually created by the installer — confirm Resolve installed cleanly.)"
else
    echo "  ✓ Scripting modules present"
end

# 3. Studio vs free — Python/execute_python/full tool set need Studio.
if test -d /Applications/DaVinci\ Resolve\ Studio.app
    echo "  ✓ Studio detected — full Python API available"
else
    echo "  ⚠ Free version — Lua/local only; many MCP tools need Studio"
end

# 4. Remind about the preference toggle (can't be set from CLI).
echo ""
echo "▸ Manual step (once): in Resolve →"
echo "    Preferences → System → General → External scripting using → Local"
echo "  (Set to 'Network' only for Tier 2 remote control — see README.md)"
echo ""

# 5. Hand off to upstream installer (auto-configures Claude Code).
echo "▸ Running upstream installer (npx davinci-resolve-mcp setup)…"
npx davinci-resolve-mcp setup

echo ""
echo "▸ Done. Restart Claude Code so the 'davinci-resolve' MCP tools load."
