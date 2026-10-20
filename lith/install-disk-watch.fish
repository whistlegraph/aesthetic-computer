#!/usr/bin/env fish
# Installs lith/disk-watch.mjs: makes lith a df-only SSH key, authorizes it on
# silo, oven and session-server (it can run nothing but `df -Pk /`), and
# enables lith/disk-watch.timer. Re-running is safe.
# Usage: fish lith/install-disk-watch.fish
set -l vault (dirname (status filename))/../aesthetic-computer-vault
set -l lith root@lith.aesthetic.computer

ssh $lith 'test -f /root/.ssh/disk_watch || ssh-keygen -q -t ed25519 -N "" -C disk-watch@lith -f /root/.ssh/disk_watch'
or exit 1
set -l pub (ssh $lith 'cat /root/.ssh/disk_watch.pub')
set -l line "restrict,command=\"df -Pk /\" $pub"

for target in "silo.aesthetic.computer $vault/home/.ssh/id_rsa" \
              "oven.aesthetic.computer $vault/oven/ssh/oven-deploy-key" \
              "session-server.aesthetic.computer $vault/session-server/session_server"
    set -l parts (string split ' ' $target)
    echo "→ authorizing disk-watch on $parts[1]"
    ssh -i $parts[2] -o StrictHostKeyChecking=accept-new root@$parts[1] \
        "grep -qF 'disk-watch@lith' /root/.ssh/authorized_keys || echo '$line' >> /root/.ssh/authorized_keys"
    or exit 1
end

ssh $lith 'cp /opt/ac/lith/disk-watch.service /opt/ac/lith/disk-watch.timer /etc/systemd/system/ && systemctl daemon-reload && systemctl enable --now disk-watch.timer && node /opt/ac/lith/disk-watch.mjs --dry'
