#!/usr/bin/env fish
# Installs silo/mongodb-backup.sh on silo as /usr/local/bin/mongodb-backup.sh.
# The root crontab already runs it at 04:00 UTC. Usage: fish silo/install-backup.fish
set -l here (dirname (status filename))
set -l key $here/../aesthetic-computer-vault/home/.ssh/id_rsa
set -l host root@silo.aesthetic.computer

scp -i $key $here/mongodb-backup.sh $host:/usr/local/bin/mongodb-backup.sh.new
and ssh -i $key $host 'chmod 755 /usr/local/bin/mongodb-backup.sh.new && bash -n /usr/local/bin/mongodb-backup.sh.new && mv /usr/local/bin/mongodb-backup.sh.new /usr/local/bin/mongodb-backup.sh && crontab -l | grep mongodb-backup'
