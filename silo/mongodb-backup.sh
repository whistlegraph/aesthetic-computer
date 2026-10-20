#!/bin/bash
# Nightly MongoDB backup for silo.aesthetic.computer (cron, 04:00 UTC).
# Installed to /usr/local/bin/mongodb-backup.sh by silo/install-backup.fish.
#
# On 2026-10-20 the old version dumped uncompressed (1.8 GB) beside 15 days
# of archives, filled the 24 GB disk and crashed mongod. This one:
#   - streams one gzipped archive (mongodump --archive --gzip), no staging copy;
#   - writes <name>.partial and renames it only once the dump and gzip check
#     pass, removing the partial on any failure;
#   - checks free space first (twice the last archive plus 1 GB), pruning the
#     oldest archives down to MIN_KEEP and skipping the night rather than
#     ever filling the disk;
#   - keeps KEEP archives on silo and copies each to a private Spaces bucket,
#     keeping REMOTE_DAYS days there.
# Credentials come from /opt/silo/.env, never from this file.

set -euo pipefail

ENV_FILE=/opt/silo/.env
BACKUP_DIR=/var/backups/mongodb
LOG=/var/log/silo/backup.log
KEEP=7
MIN_KEEP=3
REMOTE_DAYS=30
BUCKET=silo-backups-aesthetic-computer
PREFIX=mongodb

log() { echo "[$(date -Iseconds)] $*" >> "$LOG"; }
fail() { log "ERROR: $*"; exit 1; }

read_env() { grep -E "^$1=" "$ENV_FILE" | head -1 | cut -d= -f2-; }
MONGO_URI=$(read_env MONGODB_CONNECTION_STRING)
SPACES_KEY=$(read_env SPACES_KEY)
SPACES_SECRET=$(read_env SPACES_SECRET)
SPACES_ENDPOINT=$(read_env SPACES_ENDPOINT)
[ -n "$MONGO_URI" ] || fail "MONGODB_CONNECTION_STRING missing from $ENV_FILE"

mkdir -p "$BACKUP_DIR"
cd "$BACKUP_DIR"
NAME="$(date -u +%Y%m%d_%H%M%S).archive.gz"
PARTIAL="$NAME.partial"
trap 'rm -f "$BACKUP_DIR/$PARTIAL"' EXIT

# Finished archives, oldest first (the .tar.gz ones are the old script's).
archives() {
  shopt -s nullglob
  local found=("$BACKUP_DIR"/*.archive.gz "$BACKUP_DIR"/*.tar.gz)
  if [ ${#found[@]} -gt 0 ]; then printf '%s\n' "${found[@]}" | sort; fi
}
free_kb() { df -Pk "$BACKUP_DIR" | awk 'NR==2 {print $4}'; }

# Room for twice the newest archive plus 1 GB, pruning the oldest to make it.
last=$(archives | tail -1)
need_kb=$(( ${last:+$(du -k "$last" | cut -f1) * 2} + 1048576 ))
while [ "$(free_kb)" -lt "$need_kb" ]; do
  [ "$(archives | wc -l)" -gt "$MIN_KEEP" ] || fail "only $(( $(free_kb) / 1024 )) MB free, need $(( need_kb / 1024 )) MB; skipping tonight's backup"
  oldest=$(archives | head -1)
  log "Low space: removing $(basename "$oldest") to make room"
  rm -f "$oldest"
done

log "Starting backup $NAME..."
mongodump --uri="$MONGO_URI" --archive="$PARTIAL" --gzip --quiet 2>> "$LOG" || fail "mongodump failed"
gzip -t "$PARTIAL" 2>> "$LOG" || fail "archive failed its gzip check"
mv "$PARTIAL" "$NAME"
log "Backup complete: $NAME ($(du -h "$NAME" | cut -f1))"

# Keep the newest KEEP archives on silo.
archives | head -n -"$KEEP" | while read -r old; do rm -f "$old"; log "Removed $(basename "$old")"; done
log "$(archives | wc -l) local backups retained, $(( $(free_kb) / 1024 )) MB free."

# Offsite copy, private (no ACL header means private on Spaces).
if [ -n "$SPACES_KEY" ] && [ -n "$SPACES_SECRET" ] && [ -n "$SPACES_ENDPOINT" ]; then
  region=$(echo "$SPACES_ENDPOINT" | sed -E 's#https://([a-z0-9]+)\..*#\1#')
  base="https://$BUCKET.$region.digitaloceanspaces.com"
  s3() { curl -sS --fail --aws-sigv4 "aws:amz:$region:s3" --user "$SPACES_KEY:$SPACES_SECRET" \
    -H "x-amz-content-sha256: UNSIGNED-PAYLOAD" "$@"; }
  if s3 -T "$NAME" "$base/$PREFIX/$NAME" -o /dev/null 2>> "$LOG"; then
    log "Uploaded $NAME to $BUCKET/$PREFIX/"
  else
    log "ERROR: upload of $NAME to $BUCKET failed (local copy kept)"
  fi
  cutoff=$(date -u -d "-$REMOTE_DAYS days" +%Y%m%d)
  s3 "$base/?list-type=2&prefix=$PREFIX/" 2>> "$LOG" | grep -oE "<Key>[^<]+</Key>" | sed -E 's#</?Key>##g' |
    while read -r key; do
      day=$(basename "$key" | cut -c1-8)
      if [[ "$day" =~ ^[0-9]{8}$ ]] && [ "$day" -lt "$cutoff" ]; then
        s3 -X DELETE "$base/$key" -o /dev/null 2>> "$LOG" && log "Removed offsite $key"
      fi
    done || true
else
  log "ERROR: Spaces credentials missing from $ENV_FILE; offsite copy skipped"
fi
