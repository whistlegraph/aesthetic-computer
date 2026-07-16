#!/usr/bin/env bash
# jastow disk self-maintenance — idempotent, safe to re-run.
# Legacy filename retained for compatibility. Uses sudo internally.
set -euo pipefail

echo "== 1. Cap systemd journal at 500M =="
sudo mkdir -p /etc/systemd/journald.conf.d
printf '[Journal]\nSystemMaxUse=500M\nSystemKeepFree=1G\n' \
  | sudo tee /etc/systemd/journald.conf.d/cap.conf >/dev/null
sudo systemctl restart systemd-journald
sudo journalctl --vacuum-size=500M

echo "== 2. Weekly docker + package-cache prune (systemd timer) =="
sudo tee /etc/systemd/system/disk-tidy.service >/dev/null <<'EOF'
[Unit]
Description=jastow weekly disk tidy (docker + pkg caches)
[Service]
Type=oneshot
ExecStart=/usr/bin/docker image prune -af
ExecStart=/usr/bin/docker builder prune -af
ExecStart=/bin/bash -c 'rm -rf /var/cache/PackageKit/* 2>/dev/null; dnf clean all'
EOF
sudo tee /etc/systemd/system/disk-tidy.timer >/dev/null <<'EOF'
[Unit]
Description=Run disk tidy weekly
[Timer]
OnCalendar=weekly
Persistent=true
[Install]
WantedBy=timers.target
EOF

echo "== 3. Disk guard: prune when '/' crosses 90% (checked hourly) =="
sudo tee /usr/local/bin/disk-guard.sh >/dev/null <<'EOF'
#!/usr/bin/env bash
use=$(df --output=pcent / | tail -1 | tr -dc '0-9')
[ "${use:-0}" -lt 90 ] && exit 0
logger -t disk-guard "root at ${use}% — pruning"
docker image prune -af; docker builder prune -af
rm -rf /var/cache/PackageKit/* 2>/dev/null; dnf clean all
EOF
sudo chmod +x /usr/local/bin/disk-guard.sh
sudo tee /etc/systemd/system/disk-guard.service >/dev/null <<'EOF'
[Unit]
Description=Prune caches if root disk >90%
[Service]
Type=oneshot
ExecStart=/usr/local/bin/disk-guard.sh
EOF
sudo tee /etc/systemd/system/disk-guard.timer >/dev/null <<'EOF'
[Unit]
Description=Hourly disk-guard check
[Timer]
OnCalendar=hourly
Persistent=true
[Install]
WantedBy=timers.target
EOF

sudo systemctl daemon-reload
sudo systemctl enable --now disk-tidy.timer disk-guard.timer
echo "== done =="
df -h / | tail -1
systemctl list-timers disk-tidy.timer disk-guard.timer --no-pager
