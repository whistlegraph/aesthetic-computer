#!/bin/bash
# One-shot: add fleet SSH keys and configure Blueberry's cursor identity.
set -e
echo "== 1/2 fleet SSH keys =="
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

echo "== 2/2 cursor color (blue) =="
# blueberry's signature: a blue pointer. Shows on next login / lock-unlock (⌃⌘Q),
# since SIP blocks hot-reloading universalaccessd from the CLI.
defaults write com.apple.universalaccess cursorFill -dict alpha 1 red 0 green 0 blue 1
defaults write com.apple.universalaccess cursorOutline -dict alpha 1 red 1 green 1 blue 1
echo "cursorFill set to blue (applies on next login / lock-unlock)"

echo "DONE — blueberry has joined the SSH mesh and its cursor is blue."
