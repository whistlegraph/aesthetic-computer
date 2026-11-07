#!/bin/bash
# Invoked over SSH by install-backup.fish, with its private staging directory.
set -euo pipefail
stage=$1
[[ "$stage" == /var/tmp/silo-backup-install.* ]] || exit 2
trap 'rm -rf -- "$stage"' EXIT
bash -n "$stage/mongodb-backup.sh"
python3 -c 'import ast,sys; ast.parse(open(sys.argv[1]).read())' "$stage/compress-datomic-logs.py"
chmod 644 "$stage/backup-logrotate.conf"
logrotate --debug "$stage/backup-logrotate.conf" >/dev/null 2>&1
if [ -f /usr/local/bin/mongodb-backup.sh ]; then
  cp -p /usr/local/bin/mongodb-backup.sh "/usr/local/bin/mongodb-backup.sh.before-$(date -u +%Y%m%dT%H%M%SZ)"
fi
install -m 755 "$stage/mongodb-backup.sh" /usr/local/bin/mongodb-backup.sh.new
mv /usr/local/bin/mongodb-backup.sh.new /usr/local/bin/mongodb-backup.sh
install -m 755 "$stage/compress-datomic-logs.py" /etc/cron.daily/ac-datomic-log-compress
install -m 644 "$stage/backup-logrotate.conf" /etc/logrotate.d/silo-backup
mkdir -p /etc/systemd/journald.conf.d
install -m 644 "$stage/journald-silo.conf" /etc/systemd/journald.conf.d/60-silo-storage.conf
systemctl restart systemd-journald
crontab -l | grep mongodb-backup
