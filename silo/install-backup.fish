#!/usr/bin/env fish
# Installs silo/mongodb-backup.sh on silo as /usr/local/bin/mongodb-backup.sh.
# The root crontab already runs it at 04:00 UTC. Also installs bounded journal
# storage, backup log rotation and lossless daily compression of old Datomic logs.
# Usage: fish silo/install-backup.fish (no application/database restart)
set -l here (dirname (status filename))
set -l key $here/../aesthetic-computer-vault/home/.ssh/id_rsa
set -l host root@silo.aesthetic.computer

set -l stage (ssh -i $key $host 'mktemp -d /var/tmp/silo-backup-install.XXXXXX')
or exit 1
scp -i $key $here/mongodb-backup.sh $here/compress-datomic-logs.py \
    $here/journald-silo.conf $here/backup-logrotate.conf $host:$stage/
or exit 1
ssh -i $key $host "bash -s -- '$stage'" < $here/install-backup-remote.sh
