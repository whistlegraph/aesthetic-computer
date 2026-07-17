#!/bin/bash
set -euo pipefail

MACHINE=""
SCREEN_NAME=""
ADDRESS=""
ROLE="client"
SERVER_HOST=""
CONTROLLER=false
CLIENTS=""
DEFER_START=false
TRUSTED_SERVERS=""
TRUSTED_CLIENTS=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --machine) MACHINE=$2; shift 2 ;;
    --screen-name) SCREEN_NAME=$2; shift 2 ;;
    --address) ADDRESS=$2; shift 2 ;;
    --role) ROLE=$2; shift 2 ;;
    --server-host) SERVER_HOST=$2; shift 2 ;;
    --controller) CONTROLLER=true; shift ;;
    --clients) CLIENTS=$2; shift 2 ;;
    --defer-start) DEFER_START=true; shift ;;
    --trusted-servers) TRUSTED_SERVERS=$2; shift 2 ;;
    --trusted-clients) TRUSTED_CLIENTS=$2; shift 2 ;;
    *) echo "unknown argument: $1" >&2; exit 64 ;;
  esac
done

if [[ -z "$MACHINE" || -z "$SCREEN_NAME" || -z "$ADDRESS" ]]; then
  echo "usage: install.sh --machine NAME --screen-name NAME --address IP [--controller --clients a,b,c] [--role server|client --server-host IP]" >&2
  exit 64
fi
if [[ -z "$SERVER_HOST" ]]; then SERVER_HOST="$ADDRESS"; fi

HERE=$(cd "$(dirname "$0")" && pwd)
UID_=$(id -u)
mkdir -p "$HOME/.local/bin" "$HOME/.config/slab" "$HOME/Library/Deskflow" "$HOME/Library/LaunchAgents"
test -f "$HOME/.config/slab/deskflow-role-epoch" || printf '0\n' > "$HOME/.config/slab/deskflow-role-epoch"

write_fingerprints() {
  local path=$1
  local csv=$2
  local fingerprint
  : > "$path"
  IFS=',' read -r -a fingerprints <<<"$csv"
  for fingerprint in "${fingerprints[@]}"; do
    if [[ -n "$fingerprint" ]]; then
      printf 'v2:sha256:%s\n' "$(printf '%s' "$fingerprint" | tr '[:upper:]' '[:lower:]')" >> "$path"
    fi
  done
}

for file in deskflow-role-runner deskflow-set-role deskflow-claim-control deskflow-role-watchdog deskflow-start; do
  cp "$HERE/$file" "$HOME/.local/bin/$file"
  chmod 755 "$HOME/.local/bin/$file"
done
rm -f "$HOME/.local/bin/deskflow-role-idle"
cp "$HERE/deskflow-server.conf" "$HOME/Library/Deskflow/deskflow-handoff-server.conf"
mkdir -p "$HOME/Library/Deskflow/tls"
if [[ -n "$TRUSTED_SERVERS" ]]; then
  write_fingerprints "$HOME/Library/Deskflow/tls/trusted-servers" "$TRUSTED_SERVERS"
fi
if [[ -n "$TRUSTED_CLIENTS" ]]; then
  write_fingerprints "$HOME/Library/Deskflow/tls/trusted-clients" "$TRUSTED_CLIENTS"
fi

cat > "$HOME/Library/Deskflow/Deskflow-server-role.conf" <<EOF
[core]
computerName=$SCREEN_NAME
coreMode=2
lastVersion=1.26.0.0

[gui]
enableUpdateCheck=false
startCoreWithGui=false

[server]
externalConfig=true
externalConfigFile=$HOME/Library/Deskflow/deskflow-handoff-server.conf
EOF

cat > "$HOME/Library/Deskflow/Deskflow-client-role.conf" <<EOF
[client]
remoteHost=$SERVER_HOST

[core]
computerName=$SCREEN_NAME
coreMode=1
lastVersion=1.26.0.0

[gui]
enableUpdateCheck=false
startCoreWithGui=false
EOF

/usr/bin/python3 -c 'import json,sys; path,machine,screen,address,controller,clients=sys.argv[1:]; data={"enabled":True,"machine":machine,"screenName":screen,"address":address,"controller":controller=="true","clients":[x for x in clients.split(",") if x]}; f=open(path,"w"); json.dump(data,f,indent=2,sort_keys=True); f.write("\n"); f.close()' \
  "$HOME/.config/slab/deskflow-handoff.json" "$MACHINE" "$SCREEN_NAME" "$ADDRESS" "$CONTROLLER" "$CLIENTS"

/usr/bin/python3 - "$HOME/.config/slab/deskflow.json" "$ROLE" <<'PY'
import json, sys
path, role = sys.argv[1:]
try:
    with open(path) as f: data = json.load(f)
except Exception:
    data = {}
data.update({"enabled": True, "role": role, "label": data.get("label", "Deskflow"), "agent": "computer.aesthetic.deskflow"})
with open(path, "w") as f:
    json.dump(data, f, indent=2, sort_keys=True)
    f.write("\n")
PY

PLIST="$HOME/Library/LaunchAgents/computer.aesthetic.deskflow.plist"
cp "$PLIST" "$PLIST.pre-handoff-backup" 2>/dev/null || true
/usr/libexec/PlistBuddy -c "Delete :ProgramArguments" "$PLIST" 2>/dev/null || true
/usr/libexec/PlistBuddy -c "Add :ProgramArguments array" "$PLIST"
if [[ "$CONTROLLER" == "true" ]]; then
  /usr/libexec/PlistBuddy -c "Add :ProgramArguments:0 string $HOME/.local/bin/deskflow-role-runner" "$PLIST"
else
  /usr/libexec/PlistBuddy -c "Add :ProgramArguments:0 string /Applications/Deskflow.app/Contents/MacOS/deskflow-core" "$PLIST"
  /usr/libexec/PlistBuddy -c "Add :ProgramArguments:1 string $ROLE" "$PLIST"
  /usr/libexec/PlistBuddy -c "Add :ProgramArguments:2 string -s" "$PLIST"
  /usr/libexec/PlistBuddy -c "Add :ProgramArguments:3 string $HOME/Library/Deskflow/Deskflow-${ROLE}-role.conf" "$PLIST"
fi
/usr/libexec/PlistBuddy -c "Set :KeepAlive true" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :RunAtLoad true" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :StandardOutPath $HOME/Library/Logs/deskflow-core.log" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :StandardErrorPath $HOME/Library/Logs/deskflow-core.log" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :ThrottleInterval 1" "$PLIST"

STANDBY="$HOME/Library/LaunchAgents/computer.aesthetic.deskflow-standby-server.plist"
launchctl bootout "gui/${UID_}/computer.aesthetic.deskflow-standby-server" 2>/dev/null || true
rm -f "$HOME/.config/slab/deskflow-standby-enabled" "$STANDBY"

for old in computer.aesthetic.deskflow-watchdog computer.aesthetic.deskflow-server-watchdog; do
  launchctl bootout "gui/${UID_}/${old}" 2>/dev/null || true
done
launchctl disable "gui/${UID_}/computer.aesthetic.deskflow-server-watchdog" 2>/dev/null || true

WATCHDOG="$HOME/Library/LaunchAgents/computer.aesthetic.deskflow-watchdog.plist"
cat > "$WATCHDOG" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>computer.aesthetic.deskflow-watchdog</string>
<key>ProgramArguments</key><array><string>$HOME/.local/bin/deskflow-role-watchdog</string></array>
<key>RunAtLoad</key><true/>
<key>StartInterval</key><integer>45</integer>
<key>StandardOutPath</key><string>$HOME/Library/Logs/deskflow-watchdog.log</string>
<key>StandardErrorPath</key><string>$HOME/Library/Logs/deskflow-watchdog.log</string>
</dict></plist>
EOF

launchctl bootout "gui/${UID_}/computer.aesthetic.deskflow" 2>/dev/null || true
if [[ "$DEFER_START" != "true" ]]; then
  "$HOME/.local/bin/deskflow-start"
fi

echo "installed Deskflow handoff on $MACHINE ($ROLE)"
