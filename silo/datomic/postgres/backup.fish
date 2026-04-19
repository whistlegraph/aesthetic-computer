#!/usr/bin/env fish
## Nightly pg_dump of the Datomic storage database on silo.
## Intended to run under cron as the postgres system user.
## Installed at /opt/datomic/backup.fish via silo/datomic/deploy.fish.

set -l BACKUP_DIR /var/backups/datomic
set -l TS (date +%Y%m%d-%H%M%S)
set -l OUT "$BACKUP_DIR/datomic-$TS.sql.gz"
set -l RETAIN_DAYS 14

mkdir -p $BACKUP_DIR

pg_dump -Fc datomic | gzip -9 > $OUT
if test $status -ne 0
    echo "backup failed: $OUT" >&2
    exit 1
end

## Prune old dumps
find $BACKUP_DIR -type f -name 'datomic-*.sql.gz' -mtime +$RETAIN_DAYS -delete

echo "backup ok: $OUT"
