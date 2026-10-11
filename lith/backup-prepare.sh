#!/usr/bin/env bash
# Consistent PostgreSQL exports for the following DigitalOcean disk backup.
set -euo pipefail
umask 077

[[ $EUID -eq 0 ]] || { echo 'Run as root.' >&2; exit 1; }
root=/var/backups/lith
install -d -m 700 "$root"
exec 9>/run/lock/lith-backup-prepare.lock
flock -n 9 || { echo 'Another backup is running.' >&2; exit 1; }
stamp=$(date -u +%Y%m%dT%H%M%SZ)
stage=$(mktemp -d "$root/.partial.XXXXXXXX")
trap 'rm -rf -- "$stage"' EXIT

runuser -u postgres -- pg_dumpall --globals-only > "$stage/globals.sql"
runuser -u postgres -- psql -XAt -v ON_ERROR_STOP=1 -d postgres \
  -c 'SELECT datname FROM pg_database WHERE NOT datistemplate AND datallowconn ORDER BY datname' \
  > "$stage/databases.txt"
while IFS= read -r db; do
  # Database names become filenames; refuse unexpected names rather than omit them.
  [[ "$db" =~ ^[A-Za-z0-9_][A-Za-z0-9_-]*$ ]] || { echo 'Unsupported database name.' >&2; exit 1; }
  runuser -u postgres -- pg_dump --format=custom --dbname="$db" > "$stage/$db.dump"
  pg_restore --list "$stage/$db.dump" > /dev/null
done < "$stage/databases.txt"

date -u +%FT%TZ > "$stage/completed-at.txt"
(
  cd "$stage"
  sha256sum -- globals.sql databases.txt completed-at.txt ./*.dump > SHA256SUMS
)
mv -T -- "$stage" "$root/$stamp"
trap - EXIT
# Only prune our completed, timestamped directories after a successful export.
find "$root" -mindepth 1 -maxdepth 1 -type d -name '20??????T??????Z' \
  -mtime +7 -exec rm -rf -- {} +
echo "PostgreSQL exports completed: $root/$stamp"
