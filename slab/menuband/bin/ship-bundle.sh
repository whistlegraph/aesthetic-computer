#!/bin/bash
# Ship neo's installed Menu Band bundle to a member (tarball over scp, not
# rsync), swap it in atomically and restart the launch agents in place.
set -eu
host=$1
T=$(mktemp /tmp/menuband-ship.XXXXXX)   # BSD mktemp: the Xs must end the name
trap 'rm -f "$T"' EXIT
tar -C "$HOME/Applications" -cf "$T" "Menu Band.app"     # always the bundle as installed right now
scp -q "$T" "$host:/tmp/menuband-ship.tar"
ssh -o ConnectTimeout=8 "$host" 'bash -s' <<'REMOTE'
set -eu
cd ~/Applications
rm -rf ".menuband-ship"; mkdir ".menuband-ship"
tar -C ".menuband-ship" -xf /tmp/menuband-ship.tar
xattr -cr ".menuband-ship/Menu Band.app" 2>/dev/null || true
rm -rf "Menu Band.app.old"
mv "Menu Band.app" "Menu Band.app.old" 2>/dev/null || true
mv ".menuband-ship/Menu Band.app" "Menu Band.app"
rmdir ".menuband-ship"
codesign --verify --deep --strict "Menu Band.app" >/dev/null 2>&1 && echo "signature ok" || echo "signature: not verified"
uid=$(id -u)
for label in computer.aestheticcomputer.menuband computer.aestheticcomputer.menubandlauncher; do
  launchctl kickstart -k "gui/$uid/$label" 2>/dev/null && echo "kickstarted $label" || echo "no agent $label"
done
sleep 3
pgrep -x MenuBand >/dev/null && echo "$(scutil --get LocalHostName): MenuBand running $(stat -f %Sm "Menu Band.app/Contents/MacOS/MenuBand")" || { open "Menu Band.app"; sleep 2; pgrep -x MenuBand >/dev/null && echo "$(scutil --get LocalHostName): MenuBand opened" || echo "$(scutil --get LocalHostName): MenuBand NOT running"; }
rm -rf "Menu Band.app.old" /tmp/menuband-ship.tar
REMOTE
