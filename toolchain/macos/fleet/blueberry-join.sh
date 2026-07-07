#!/bin/bash
# One-shot: add fleet SSH keys + configure Stats to match the neo/chicken/panda fleet.
set -e
echo "== 1/5 fleet SSH keys =="
mkdir -p ~/.ssh; touch ~/.ssh/authorized_keys; chmod 700 ~/.ssh; chmod 600 ~/.ssh/authorized_keys
cat > /tmp/fleet-keys.pub <<'KEYS'
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM5Dbs/qJ3ut0TTkK37F260rP6wjOaTNfEbweDTjgmHv jas@aesthetic -> aesthetics-macbook-pro (via tailscale)
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJtloowUraFVv4YJ2RzX09gdpM+G4rEkz38Z4jPG/Gpq jas-neo
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIccQMDDp3cOl0AvRnpOUqtt7xiuKMNFjrJHsDinnvAP chicken-to-neo
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJagCRWxbqzJIQrTvQ/TMYhLiMFD9+6eUwEeE66ptoZ panda-to-neo
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK8prNgeHqxy7mseAWLotHwTVIB6na+XLH2eRlxdrEgk blueberry-to-neo
KEYS
before=$(grep -c . ~/.ssh/authorized_keys || echo 0)
while read -r line; do
  [ -z "$line" ] && continue
  blob=$(echo "$line" | awk '{print $2}')
  awk '{print $2}' ~/.ssh/authorized_keys | grep -qxF "$blob" || echo "$line" >> ~/.ssh/authorized_keys
done < /tmp/fleet-keys.pub
rm -f /tmp/fleet-keys.pub
echo "authorized_keys: $before -> $(grep -c . ~/.ssh/authorized_keys) keys"

echo "== 2/5 Stats config =="
if [ ! -d /Applications/Stats.app ]; then
  echo "Stats not installed; installing via brew..."; brew install --cask stats || { echo "install Stats manually from https://github.com/exelban/stats"; exit 1; }
fi
pkill -x Stats 2>/dev/null || true; sleep 1
echo "PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiPz4KPCFET0NUWVBFIHBsaXN0IFBVQkxJQyAiLS8vQXBwbGUvL0RURCBQTElTVCAxLjAvL0VOIiAiaHR0cDovL3d3dy5hcHBsZS5jb20vRFREcy9Qcm9wZXJ0eUxpc3QtMS4wLmR0ZCI+CjxwbGlzdCB2ZXJzaW9uPSIxLjAiPgo8ZGljdD4KCTxrZXk+QmF0dGVyeV9zdGF0ZTwva2V5PgoJPGZhbHNlLz4KCTxrZXk+Q1BVX2JhckNoYXJ0X3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj4zPC9pbnRlZ2VyPgoJPGtleT5DUFVfbGFiZWxfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjE8L2ludGVnZXI+Cgk8a2V5PkNQVV9saW5lQ2hhcnRfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjI8L2ludGVnZXI+Cgk8a2V5PkNQVV9taW5pX3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj4wPC9pbnRlZ2VyPgoJPGtleT5DUFVfcGllQ2hhcnRfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjQ8L2ludGVnZXI+Cgk8a2V5PkNQVV9zdGF0ZTwva2V5PgoJPHRydWUvPgoJPGtleT5DUFVfdGFjaG9tZXRlcl9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+NTwvaW50ZWdlcj4KCTxrZXk+Q1BVX3dpZGdldDwva2V5PgoJPHN0cmluZz5taW5pPC9zdHJpbmc+Cgk8a2V5PkRpc2tfYmFyQ2hhcnRfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjM8L2ludGVnZXI+Cgk8a2V5PkRpc2tfbGFiZWxfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjI8L2ludGVnZXI+Cgk8a2V5PkRpc2tfbWVtb3J5X3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj41PC9pbnRlZ2VyPgoJPGtleT5EaXNrX21pbmlfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjA8L2ludGVnZXI+Cgk8a2V5PkRpc2tfbmV0d29ya0NoYXJ0X3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj42PC9pbnRlZ2VyPgoJPGtleT5EaXNrX3BpZUNoYXJ0X3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj40PC9pbnRlZ2VyPgoJPGtleT5EaXNrX3NwZWVkX3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj4xPC9pbnRlZ2VyPgoJPGtleT5EaXNrX3N0YXRlPC9rZXk+Cgk8dHJ1ZS8+Cgk8a2V5PkRpc2tfdGV4dF9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+NzwvaW50ZWdlcj4KCTxrZXk+RGlza193aWRnZXQ8L2tleT4KCTxzdHJpbmc+bWluaTwvc3RyaW5nPgoJPGtleT5HUFVfYmFyQ2hhcnRfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjM8L2ludGVnZXI+Cgk8a2V5PkdQVV9sYWJlbF9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+MTwvaW50ZWdlcj4KCTxrZXk+R1BVX2xpbmVDaGFydF9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+MjwvaW50ZWdlcj4KCTxrZXk+R1BVX21pbmlfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjA8L2ludGVnZXI+Cgk8a2V5PkdQVV9zdGF0ZTwva2V5PgoJPHRydWUvPgoJPGtleT5HUFVfdGFjaG9tZXRlcl9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+NDwvaW50ZWdlcj4KCTxrZXk+R1BVX3dpZGdldDwva2V5PgoJPHN0cmluZz5taW5pPC9zdHJpbmc+Cgk8a2V5Pk5ldHdvcmtfYmFzZTwva2V5PgoJPHN0cmluZz5ieXRlPC9zdHJpbmc+Cgk8a2V5Pk5ldHdvcmtfZG93bmxvYWRDb2xvcjwva2V5PgoJPHN0cmluZz5zeXN0ZW08L3N0cmluZz4KCTxrZXk+TmV0d29ya19sYWJlbF9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+NDwvaW50ZWdlcj4KCTxrZXk+TmV0d29ya19uZXR3b3JrQ2hhcnRfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjE8L2ludGVnZXI+Cgk8a2V5Pk5ldHdvcmtfcHJvY2Vzc2VzPC9rZXk+Cgk8aW50ZWdlcj44PC9pbnRlZ2VyPgoJPGtleT5OZXR3b3JrX3NwZWVkX3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj4wPC9pbnRlZ2VyPgoJPGtleT5OZXR3b3JrX3N0YXRlPC9rZXk+Cgk8dHJ1ZS8+Cgk8a2V5Pk5ldHdvcmtfc3RhdGVfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjI8L2ludGVnZXI+Cgk8a2V5Pk5ldHdvcmtfdGV4dF9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+MzwvaW50ZWdlcj4KCTxrZXk+TmV0d29ya191cGxvYWRDb2xvcjwva2V5PgoJPHN0cmluZz5tb25vY2hyb21lPC9zdHJpbmc+Cgk8a2V5Pk5ldHdvcmtfd2lkZ2V0PC9rZXk+Cgk8c3RyaW5nPnNwZWVkPC9zdHJpbmc+Cgk8a2V5PlJBTV9iYXJDaGFydF9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+NDwvaW50ZWdlcj4KCTxrZXk+UkFNX2xhYmVsX3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj4yPC9pbnRlZ2VyPgoJPGtleT5SQU1fbGluZUNoYXJ0X3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj4zPC9pbnRlZ2VyPgoJPGtleT5SQU1fbWVtb3J5X3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj4xPC9pbnRlZ2VyPgoJPGtleT5SQU1fbWluaV9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+MDwvaW50ZWdlcj4KCTxrZXk+UkFNX3BpZUNoYXJ0X3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj41PC9pbnRlZ2VyPgoJPGtleT5SQU1fc3RhdGU8L2tleT4KCTx0cnVlLz4KCTxrZXk+UkFNX3N0YXRlX3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj44PC9pbnRlZ2VyPgoJPGtleT5SQU1fdGFjaG9tZXRlcl9wb3NpdGlvbjwva2V5PgoJPGludGVnZXI+NjwvaW50ZWdlcj4KCTxrZXk+UkFNX3RleHRfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjc8L2ludGVnZXI+Cgk8a2V5PlJBTV93aWRnZXQ8L2tleT4KCTxzdHJpbmc+bWluaTwvc3RyaW5nPgoJPGtleT5TZW5zb3JzX2JhckNoYXJ0X3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj4yPC9pbnRlZ2VyPgoJPGtleT5TZW5zb3JzX2xhYmVsX3Bvc2l0aW9uPC9rZXk+Cgk8aW50ZWdlcj4zPC9pbnRlZ2VyPgoJPGtleT5TZW5zb3JzX21pbmlfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjA8L2ludGVnZXI+Cgk8a2V5PlNlbnNvcnNfc3RhY2tfcG9zaXRpb248L2tleT4KCTxpbnRlZ2VyPjE8L2ludGVnZXI+Cgk8a2V5PlNlbnNvcnNfc3RhdGU8L2tleT4KCTxmYWxzZS8+Cgk8a2V5PlNlbnNvcnNfd2lkZ2V0PC9rZXk+Cgk8c3RyaW5nPm1pbmk8L3N0cmluZz4KPC9kaWN0Pgo8L3BsaXN0Pgo=" | base64 -d > /tmp/stats-shared.plist
python3 - <<'PY'
import plistlib, subprocess
p = plistlib.load(open("/tmp/stats-shared.plist","rb"))
for k,v in p.items():
    if isinstance(v,bool): t,vv="-bool",("true" if v else "false")
    elif isinstance(v,int): t,vv="-int",str(v)
    else: t,vv="-string",str(v)
    subprocess.run(["defaults","write","eu.exelban.Stats",k,t,vv],check=True)
print("imported",len(p),"Stats keys")
PY

echo "== 3/5 login item =="
osascript -e 'tell application "System Events" to delete (every login item whose name is "Stats")' >/dev/null 2>&1 || true
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Stats.app", hidden:true}' >/dev/null 2>&1 || true
echo "login items: $(osascript -e 'tell application "System Events" to get the name of every login item')"

echo "== 4/5 launch =="
open -a Stats; sleep 2
pgrep -x Stats >/dev/null && echo "Stats RUNNING ✅" || echo "Stats did not launch"

echo "== 5/5 cursor color (blue) =="
# blueberry's signature: a blue pointer. Shows on next login / lock-unlock (⌃⌘Q),
# since SIP blocks hot-reloading universalaccessd from the CLI.
defaults write com.apple.universalaccess cursorFill -dict alpha 1 red 0 green 0 blue 1
defaults write com.apple.universalaccess cursorOutline -dict alpha 1 red 1 green 1 blue 1
echo "cursorFill set to blue (applies on next login / lock-unlock)"

echo "DONE — blueberry has joined the SSH mesh, Stats matches the fleet, cursor is blue."
