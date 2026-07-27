#!/usr/bin/env bash
# Install the typed AC fleet worker as a per-user launchd service.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE="$HERE/worker.mjs"
ROLE="${AC_FLEET_WORKER_ROLE:-balanced}"
NAME="${AC_FLEET_WORKER_NAME:-$(hostname -s)}"
PORT="${AC_FLEET_WORKER_PORT:-5263}"
TOKEN_SOURCE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --role) ROLE="${2:?missing role}"; shift 2 ;;
    --name) NAME="${2:?missing name}"; shift 2 ;;
    --port) PORT="${2:?missing port}"; shift 2 ;;
    --token-file) TOKEN_SOURCE="${2:?missing token file}"; shift 2 ;;
    *) echo "usage: install-worker.sh [--name HOST] [--role heavy|light|interactive] [--port 5263] [--token-file PATH]" >&2; exit 2 ;;
  esac
done

case "$ROLE" in heavy|light|interactive|balanced) ;; *) echo "invalid role: $ROLE" >&2; exit 2 ;; esac
[[ -f "$SOURCE" ]] || { echo "missing $SOURCE" >&2; exit 1; }

TAILSCALE="$(command -v tailscale 2>/dev/null || true)"
for candidate in /opt/homebrew/bin/tailscale /usr/local/bin/tailscale \
  /Applications/Tailscale.app/Contents/MacOS/Tailscale; do
  [[ -x "$TAILSCALE" ]] && break
  [[ -x "$candidate" ]] && TAILSCALE="$candidate"
done
[[ -x "$TAILSCALE" ]] || { echo "Tailscale CLI is required" >&2; exit 1; }
BIND="$($TAILSCALE ip -4 2>/dev/null | head -1)"
[[ "$BIND" =~ ^100\. ]] || { echo "refusing to bind without a 100.x tailnet address" >&2; exit 1; }

NODE="$(command -v node 2>/dev/null || true)"
for candidate in /opt/homebrew/bin/node /usr/local/bin/node; do
  [[ -x "$NODE" ]] && break
  [[ -x "$candidate" ]] && NODE="$candidate"
done
[[ -x "$NODE" ]] || { echo "node is required" >&2; exit 1; }
# fnm exposes a per-shell multishell symlink. launchd needs the durable version
# path behind it, which Node reports as process.execPath.
NODE="$($NODE -p 'process.execPath')"
FFMPEG="/opt/homebrew/bin/ffmpeg"
[[ -x "$FFMPEG" ]] || FFMPEG="/usr/local/bin/ffmpeg"
[[ -x "$FFMPEG" ]] || { echo "ffmpeg is required" >&2; exit 1; }

LIB="$HOME/.local/lib/ac-fleet-worker"
CONFIG="$HOME/.config/ac-fleet-worker"
STATE="$HOME/.local/share/ac-fleet-worker"
TOKEN="$CONFIG/token"
PLIST="$HOME/Library/LaunchAgents/computer.aesthetic.fleet-worker.plist"
LABEL="computer.aesthetic.fleet-worker"
mkdir -p "$LIB" "$CONFIG" "$STATE" "$HOME/Library/LaunchAgents"
install -m 755 "$SOURCE" "$LIB/worker.mjs"
if [[ -n "$TOKEN_SOURCE" ]]; then
  [[ -f "$TOKEN_SOURCE" ]] || { echo "token source is missing: $TOKEN_SOURCE" >&2; exit 1; }
  install -m 600 "$TOKEN_SOURCE" "$TOKEN"
elif [[ ! -s "$TOKEN" ]]; then
  umask 077
  /usr/bin/openssl rand -hex 32 > "$TOKEN"
fi
chmod 600 "$TOKEN"

cat > "$PLIST" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>Label</key><string>$LABEL</string>
  <key>ProgramArguments</key><array><string>$NODE</string><string>$LIB/worker.mjs</string></array>
  <key>EnvironmentVariables</key><dict>
    <key>AC_FLEET_WORKER_BIND</key><string>$BIND</string>
    <key>AC_FLEET_WORKER_NAME</key><string>$NAME</string>
    <key>AC_FLEET_WORKER_PORT</key><string>$PORT</string>
    <key>AC_FLEET_WORKER_ROLE</key><string>$ROLE</string>
    <key>AC_FLEET_WORKER_FFMPEG</key><string>$FFMPEG</string>
    <key>PATH</key><string>/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin</string>
  </dict>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
  <key>ThrottleInterval</key><integer>10</integer>
  <key>StandardOutPath</key><string>$STATE/launchd.out</string>
  <key>StandardErrorPath</key><string>$STATE/launchd.err</string>
</dict></plist>
PLIST

/usr/bin/plutil -lint "$PLIST" >/dev/null
launchctl bootout "gui/$(id -u)/$LABEL" 2>/dev/null || true
attempt=0
while ! launchctl bootstrap "gui/$(id -u)" "$PLIST" 2>/dev/null; do
  attempt=$((attempt + 1))
  if (( attempt >= 20 )); then
    launchctl bootstrap "gui/$(id -u)" "$PLIST"
    break
  fi
  sleep 0.1
done
launchctl kickstart -k "gui/$(id -u)/$LABEL"
echo "installed $LABEL on $BIND:$PORT ($ROLE)"
