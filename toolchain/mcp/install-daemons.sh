#!/usr/bin/env bash
# install-daemons.sh — run our MCP servers as ONE shared HTTP daemon each
# (launchd), instead of one stdio process per Claude session.
#
# Why: Claude Code spawns every stdio MCP server fresh per session, so N
# parallel sessions cost N copies of each server. On neo (8 GB) three sessions
# once carried ~570 MB of duplicate MCP processes, which contributed to a hang
# so complete that launchd could no longer fork an sshd to let anyone in.
#
#   mail-mcp   → http://127.0.0.1:7765/mcp
#   emacs-mcp  → http://127.0.0.1:7766/mcp
#   frame-mcp  → http://127.0.0.1:7767/mcp
#   puppet-mcp → http://127.0.0.1:7769/mcp   (7768 is Spotify's)
#   dm-mcp     → http://127.0.0.1:7771/mcp
#   memory-mcp → http://127.0.0.1:7772/mcp
#   prox-mcp   → http://127.0.0.1:7773/mcp
#   cal-mcp    → http://127.0.0.1:7774/mcp
#   chat-mcp   → http://127.0.0.1:7775/mcp
#   fleet-mcp  → http://127.0.0.1:7776/mcp
#   paper-mcp  → http://127.0.0.1:7777/mcp
#   grafana-mcp→ http://127.0.0.1:7778/mcp (when fuser credentials exist)
#
# This script also POINTS Claude and Codex at the daemons. Claude gets same-name
# local-scope entries that shadow the stdio ones in .mcp.json; Codex gets its
# user-level MCP entries via `codex mcp add --url`.
#
# The stdio entries in .mcp.json stay as the fallback: a box without daemons
# (or a session outside this repo) still works, just at one process per session.
#
# Idempotent: re-running rewrites the plists, restarts the daemons, and
# re-points Claude at them.

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# Prefer fnm's stable alias path over a transient fnm_multishells one, which
# dies with the shell that made it. frame-mcp also spawns `node` as a child, so
# this directory goes on the daemon's PATH below — launchd inherits no shell.
NODE="$HOME/.local/share/fnm/aliases/default/bin/node"
[ -x "$NODE" ] || NODE="$(command -v node)"
EMACSCLIENT="$(command -v emacsclient 2>/dev/null || true)"
if [ -z "$EMACSCLIENT" ]; then
  for candidate in \
    /Applications/Emacs.app/Contents/MacOS/bin/emacsclient \
    "$HOME/Applications/Emacs.app/Contents/MacOS/bin/emacsclient" \
    /opt/homebrew/bin/emacsclient \
    /usr/local/bin/emacsclient; do
    if [ -x "$candidate" ]; then
      EMACSCLIENT="$candidate"
      break
    fi
  done
fi
AGENTS="$HOME/Library/LaunchAgents"
mkdir -p "$AGENTS"

# launchd can briefly retain a booted-out label and return EIO when the same
# plist is immediately bootstrapped. Retry that narrow handoff instead of
# aborting a full daemon refresh on a transient registration race.
bootstrap_plist() {
  local plist="$1" i=0
  while [ $i -lt 20 ]; do
    launchctl bootstrap "gui/$(id -u)" "$plist" 2>/dev/null && return 0
    sleep 0.1
    i=$((i+1))
  done
  launchctl bootstrap "gui/$(id -u)" "$plist"
}

# "name port script" rows. macOS ships bash 3.2 — no associative arrays. Fed to
# the loops below by here-doc, which (unlike a pipe) keeps them in this shell so
# `fail` survives.
servers() {
  cat <<'ROWS'
mail   7765 ants/mail-mcp/server.mjs
ROWS
  if [ -n "$EMACSCLIENT" ]; then
    cat <<'ROWS'
emacs  7766 artery/emacs-mcp.mjs
ROWS
  fi
  cat <<'ROWS'
frame  7767 slab/bin/frame-mcp.mjs
puppet 7769 slab/bin/puppet-mcp.mjs
dm     7771 slab/bin/dm-mcp.mjs
memory 7772 memory/memory-mcp.mjs
prox   7773 slab/bin/prox-mcp.mjs
cal    7774 slab/bin/cal-mcp.mjs
chat   7775 slab/bin/chat-mcp.mjs
fleet  7776 toolchain/fleet/fleet-mcp.mjs
paper  7777 slab/bin/paper-mcp.mjs
ROWS
}

# Env the stdio entries in .mcp.json pass, which the daemons need too. Both
# servers fall back to paths that are wrong here (emacs → /usr/sbin/emacsclient,
# mail → a /workspaces devcontainer path), so a daemon without these is subtly
# broken rather than obviously dead.
extra_env() {
  case "$1" in
    emacs) printf '        <key>EMACSCLIENT</key>\n        <string>%s</string>\n' "$EMACSCLIENT" ;;
    mail)  printf '        <key>AC_EMAIL_STYLE_GUIDE</key>\n        <string>%s/toolchain/email/style-guide.md</string>\n' "$REPO" ;;
  esac
}

write_plist() {
  local name="$1" label="$2" script="$3" port="$4"
  cat > "$AGENTS/$label.plist" <<EOF
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
        <string>$(dirname "$NODE"):/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin</string>
$(extra_env "$name")    </dict>
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
  bootstrap_plist "$AGENTS/$label.plist"
}

# Grafana's Go MCP binary supports streamable HTTP directly. It is kept
# separate from the Node table because its wrapper sources machine-local
# credentials before execing the binary. Sharing it avoids one Go process per
# prompt while leaving the token out of both launchd and MCP configuration.
write_grafana_plist() {
  local label="computer.aesthetic.grafana-mcp"
  local wrapper="$HOME/.local/bin/mcp-grafana-fuser"
  cat > "$AGENTS/$label.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
    <key>Label</key><string>$label</string>
    <key>ProgramArguments</key><array>
        <string>$wrapper</string>
        <string>-t</string><string>streamable-http</string>
        <string>-address</string><string>127.0.0.1:7778</string>
    </array>
    <key>EnvironmentVariables</key><dict>
        <key>HOME</key><string>$HOME</string>
        <key>PATH</key><string>$HOME/.local/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin</string>
    </dict>
    <key>RunAtLoad</key><true/>
    <key>KeepAlive</key><true/>
    <key>ThrottleInterval</key><integer>5</integer>
    <key>StandardOutPath</key><string>/tmp/$label.out</string>
    <key>StandardErrorPath</key><string>/tmp/$label.err</string>
</dict></plist>
EOF
  launchctl bootout "gui/$(id -u)/$label" 2>/dev/null || true
  bootstrap_plist "$AGENTS/$label.plist"
}

configure_codex_prox_headers() {
  local config_root="${CODEX_HOME:-$HOME/.codex}"
  local config="$config_root/config.toml"
  [ -f "$config" ] || return 1
  "$NODE" --input-type=module - "$config" <<'NODE'
import { readFile, writeFile } from "node:fs/promises";

const path = process.argv[2];
const headerLine = 'env_http_headers = { "X-Slab-Loopboy-Contact" = "SLAB_LOOPBOY_CONTACT", "X-Slab-Prompt-Session-Id" = "SLAB_PROMPT_SESSION_ID" }';
let text = await readFile(path, "utf8");
const section = /(^\[mcp_servers\.prox\]\n)([\s\S]*?)(?=^\[[^\]]+\]\s*$|(?![\s\S]))/m;
const match = text.match(section);
if (!match) throw new Error("Codex prox MCP section is missing");
let body = match[2].replace(/^env_http_headers\s*=.*\n?/m, "");
const urlLine = /^url\s*=.*$/m;
body = urlLine.test(body)
  ? body.replace(urlLine, (line) => `${line}\n${headerLine}`)
  : `${headerLine}\n${body}`;
text = text.replace(section, `${match[1]}${body}`);
await writeFile(path, text);
NODE
}

# puppet-mcp is a thin HTTP facade over a warm, stateful CDP multiplexer. Keep
# that backend under launchd too; otherwise the MCP port looks healthy while
# every browser tool fails against a stale unix socket after logout or reboot.
write_puppet_core_plist() {
  local label="computer.aesthetic.puppet-core"
  cat > "$AGENTS/$label.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>$label</string>
    <key>ProgramArguments</key>
    <array>
        <string>$NODE</string>
        <string>$REPO/slab/bin/puppet.mjs</string>
        <string>daemon</string>
    </array>
    <key>EnvironmentVariables</key>
    <dict>
        <key>HOME</key>
        <string>$HOME</string>
        <key>PATH</key>
        <string>$(dirname "$NODE"):/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin</string>
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
  bootstrap_plist "$AGENTS/$label.plist"
  echo "✓ puppet core → $HOME/.local/share/puppet/puppet.sock"
}

HAVE_CLAUDE=0
command -v claude >/dev/null 2>&1 && HAVE_CLAUDE=1
HAVE_CODEX=0
command -v codex >/dev/null 2>&1 && HAVE_CODEX=1
cd "$REPO"  # `claude mcp --scope local` is per-project

if [ -f "$HOME/.config/slab/puppet.json" ]; then
  write_puppet_core_plist
else
  echo "⚠ puppet core skipped — missing ~/.config/slab/puppet.json"
fi

# An unavailable optional editor bridge should not remain advertised as a
# working MCP. Re-running this installer after Emacs is installed restores it.
if [ -z "$EMACSCLIENT" ]; then
  launchctl bootout "gui/$(id -u)/computer.aesthetic.emacs-mcp" 2>/dev/null || true
  [ "$HAVE_CLAUDE" = 1 ] && claude mcp remove emacs --scope local >/dev/null 2>&1 || true
  [ "$HAVE_CODEX" = 1 ] && codex mcp remove emacs >/dev/null 2>&1 || true
  echo "⚠ emacs-mcp disabled — emacsclient is not installed on this host"
fi

while read -r name port script; do
  [ -n "$name" ] || continue
  write_plist "$name" "computer.aesthetic.$name-mcp" "$REPO/$script" "$port"
  echo "✓ $name-mcp → 127.0.0.1:$port"
  if [ "$HAVE_CLAUDE" = 1 ]; then
    # Local scope shadows the stdio entry of the same name in .mcp.json.
    claude mcp remove "$name" --scope local >/dev/null 2>&1 || true
    claude mcp add --transport http --scope local "$name" "http://127.0.0.1:$port/mcp" >/dev/null
    echo "  ↳ claude → http://127.0.0.1:$port/mcp (local scope)"
  fi
  if [ "$HAVE_CODEX" = 1 ]; then
    codex mcp remove "$name" >/dev/null 2>&1 || true
    codex mcp add "$name" --url "http://127.0.0.1:$port/mcp" >/dev/null
    if [ "$name" = prox ]; then
      configure_codex_prox_headers
    fi
    echo "  ↳ codex  → http://127.0.0.1:$port/mcp"
  fi
done <<<"$(servers)"

HAVE_GRAFANA=0
if [ -x "$HOME/.local/bin/mcp-grafana-fuser" ] && [ -f "$HOME/.config/fuser/grafana.env" ]; then
  write_grafana_plist
  HAVE_GRAFANA=1
  echo "✓ grafana-mcp → 127.0.0.1:7778"
  if [ "$HAVE_CLAUDE" = 1 ]; then
    claude mcp remove grafana --scope local >/dev/null 2>&1 || true
    claude mcp add --transport http --scope local grafana "http://127.0.0.1:7778/mcp" >/dev/null
    echo "  ↳ claude → http://127.0.0.1:7778/mcp (local scope)"
  fi
  if [ "$HAVE_CODEX" = 1 ]; then
    codex mcp remove grafana >/dev/null 2>&1 || true
    codex mcp add grafana --url "http://127.0.0.1:7778/mcp" >/dev/null
    echo "  ↳ codex  → http://127.0.0.1:7778/mcp"
  fi
else
  launchctl bootout "gui/$(id -u)/computer.aesthetic.grafana-mcp" 2>/dev/null || true
  echo "⚠ grafana-mcp skipped — wrapper or ~/.config/fuser/grafana.env is missing"
fi

if [ "$HAVE_CLAUDE" = 0 ]; then
  echo
  echo "⚠ 'claude' not on PATH — daemons installed, but sessions will still spawn"
  echo "  stdio copies. Re-run from a shell where 'claude' resolves."
fi
if [ "$HAVE_CODEX" = 0 ]; then
  echo
  echo "⚠ 'codex' not on PATH — daemons installed, but Codex will not see them"
  echo "  until you re-run from a shell where 'codex' resolves."
fi

# Verify. A daemon that never answers is precisely the silent half-apply this
# script exists to prevent. Speak like a real MCP client: mail-mcp's SDK
# transport 406s a request that won't accept text/event-stream, and a launchd
# respawn after a port clash costs one ThrottleInterval, so allow ~10s.
echo
fail=0
while read -r name port script; do
  [ -n "$name" ] || continue
  i=0
  while [ $i -lt 50 ]; do nc -z 127.0.0.1 "$port" 2>/dev/null && break; sleep 0.2; i=$((i+1)); done
  if curl -s -m 5 -X POST "http://127.0.0.1:$port/mcp" \
       -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' \
       -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"install-daemons","version":"1"}}}' \
       2>/dev/null | grep -q '"serverInfo"'; then
    echo "✓ $name-mcp answering on $port"
  else
    echo "✗ $name-mcp NOT answering on $port — see /tmp/computer.aesthetic.$name-mcp.err"
    echo "   (is another app holding $port?  lsof -nP -iTCP:$port -sTCP:LISTEN)"
    fail=1
  fi
done <<<"$(servers)"

if [ "$HAVE_GRAFANA" = 1 ]; then
  i=0
  while [ $i -lt 50 ]; do nc -z 127.0.0.1 7778 2>/dev/null && break; sleep 0.2; i=$((i+1)); done
  if curl -s -m 5 -X POST "http://127.0.0.1:7778/mcp" \
       -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' \
       -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"install-daemons","version":"1"}}}' \
       2>/dev/null | grep -q '"serverInfo"'; then
    echo "✓ grafana-mcp answering on 7778"
  else
    echo "✗ grafana-mcp NOT answering on 7778 — see /tmp/computer.aesthetic.grafana-mcp.err"
    fail=1
  fi
fi

exit $fail
