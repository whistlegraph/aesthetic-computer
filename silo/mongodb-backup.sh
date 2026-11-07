#!/bin/bash
# Nightly Mongo backup (04:00 UTC). --prune-only verifies offsite copies and
# applies local retention without taking a new dump. No database writes.
# Three local archives, 30 days offsite. Failed verification always keeps data.
# The 2026-10-20 outage was a backup filling the database's root filesystem.
set -euo pipefail
umask 077

# Overrides are for isolated tests/restore drills, never client input.
ENV_FILE=${SILO_BACKUP_ENV_FILE:-/opt/silo/.env}
BACKUP_DIR=${SILO_BACKUP_DIR:-/var/backups/mongodb}
LOG=${SILO_BACKUP_LOG:-/var/log/silo/backup.log}
KEEP=3
REMOTE_DAYS=30
RESERVE_KB=2097152 # Leave 2 GiB outside the dump's file-size budget.
BUCKET=silo-backups-aesthetic-computer
PREFIX=mongodb
MODE=${1:-backup}
case "$MODE" in backup|--prune-only) ;; *) echo 'Usage: mongodb-backup.sh [--prune-only]' >&2; exit 2 ;; esac

mkdir -p "$BACKUP_DIR" "$(dirname "$LOG")"
log() { echo "[$(date -Iseconds)] $*" >> "$LOG"; }
fail() { log "ERROR: $*"; echo "Mongo backup failed: $*" >&2; exit 1; }
exec 9>"$BACKUP_DIR/.backup.lock"
flock -n 9 || fail 'another backup/prune is running'
PARTIAL=""
trap '[ -z "$PARTIAL" ] || rm -f -- "$PARTIAL"' EXIT

read_env() { (grep -E "^$1=" "$ENV_FILE" || true) | head -1 | cut -d= -f2-; }
MONGO_URI=$(read_env MONGODB_CONNECTION_STRING)
SPACES_KEY=$(read_env SPACES_KEY)
SPACES_SECRET=$(read_env SPACES_SECRET)
SPACES_ENDPOINT=$(read_env SPACES_ENDPOINT)
REMOTE_READY=false
if [ -n "$SPACES_KEY" ] && [ -n "$SPACES_SECRET" ] &&
   [[ "$SPACES_ENDPOINT" =~ ^https://([a-z0-9]+)\.digitaloceanspaces\.com/?$ ]]; then
  region=${BASH_REMATCH[1]}
  base="https://$BUCKET.$region.digitaloceanspaces.com"
  REMOTE_READY=true
fi
s3() {
  curl -sS --fail --connect-timeout 10 --max-time 1200 --retry 2 \
    --aws-sigv4 "aws:amz:$region:s3" --user "$SPACES_KEY:$SPACES_SECRET" \
    -H 'x-amz-content-sha256: UNSIGNED-PAYLOAD' "$@"
}
archives() {
  shopt -s nullglob
  local found=("$BACKUP_DIR"/*.archive.gz "$BACKUP_DIR"/*.tar.gz)
  if [ ${#found[@]} -gt 0 ]; then printf '%s\n' "${found[@]}" | sort; fi
}
free_kb() { df -Pk "$BACKUP_DIR" | awk 'NR==2 {print $4}'; }

# Single-PUT ETags are content MD5s. Multipart or nonmatching ETags fail
# closed. Size alone is not enough evidence to delete a local copy.
verify_remote() {
  local file=$1 headers bytes digest remote_bytes remote_digest
  $REMOTE_READY || return 1
  headers=$(s3 -I "$base/$PREFIX/$(basename "$file")" 2>> "$LOG") || return 1
  remote_bytes=$(printf '%s\n' "$headers" | awk 'tolower($1)=="content-length:" {gsub(/\r/,"",$2); v=$2} END {print v}')
  remote_digest=$(printf '%s\n' "$headers" | awk 'tolower($1)=="etag:" {gsub(/["\r]/,"",$2); v=tolower($2)} END {print v}')
  bytes=$(wc -c < "$file" | tr -d ' ')
  digest=$(md5sum "$file" | cut -d' ' -f1)
  [ "$bytes" = "$remote_bytes" ] && [ "$digest" = "$remote_digest" ]
}

prune_local() {
  local files=() file i failed=0
  while IFS= read -r file; do files+=("$file"); done < <(archives)
  for ((i=0; i<${#files[@]}-KEEP; i++)); do
    file=${files[$i]}
    if verify_remote "$file"; then
      rm -- "$file" || return 1
      log "Removed $(basename "$file"): offsite size + MD5 verified"
    else
      log "KEEP $(basename "$file"): offsite verification unavailable or mismatched"
      failed=1
    fi
  done
  return "$failed"
}

if [ "$MODE" = --prune-only ]; then
  prune_local || fail 'some local archives could not be verified offsite; kept them'
  log "Retention checked: $(archives | wc -l) local archives, $(( $(free_kb) / 1024 )) MiB free"
  exit 0
fi
[ -n "$MONGO_URI" ] || fail "MONGODB_CONNECTION_STRING missing from $ENV_FILE"

# Remote outages may increase local retention, never erase unverified copies.
prune_local || true
last=$(archives | tail -1)
last_kb=0
if [ -n "$last" ]; then last_kb=$(du -k "$last" | cut -f1); fi
need_kb=$(( RESERVE_KB + last_kb * 2 ))
available_kb=$(free_kb)
[ "$available_kb" -ge "$need_kb" ] || fail "only $(( available_kb / 1024 )) MiB free; need $(( need_kb / 1024 )); skipping dump"

NAME="$(date -u +%Y%m%d_%H%M%S).archive.gz"
[ ! -e "$BACKUP_DIR/$NAME" ] && [ ! -e "$BACKUP_DIR/$NAME.partial" ] || fail 'backup filename already exists'
PARTIAL="$BACKUP_DIR/$NAME.partial"
log "Starting backup $NAME..."
# Linux bash's file-size limit uses KiB. Stop an unexpectedly large dump;
# disable core dumps so exceeding the limit cannot create one. Other writers
# still need normal disk monitoring. No uncompressed staging copy.
if ! (
  ulimit -c 0 || exit 1
  ulimit -f "$(( available_kb - RESERVE_KB ))" || exit 1
  exec mongodump --uri="$MONGO_URI" --archive="$PARTIAL" --gzip \
    --numParallelCollections=1 --quiet
) 2>> "$LOG"; then
  fail 'mongodump failed or exceeded its disk budget; incomplete archive removed'
fi
gzip -t "$PARTIAL" 2>> "$LOG" || fail 'archive failed its gzip check'
mv "$PARTIAL" "$BACKUP_DIR/$NAME"
PARTIAL=""
log "Backup complete: $NAME ($(du -h "$BACKUP_DIR/$NAME" | cut -f1))"

$REMOTE_READY || fail 'Spaces configuration missing; local backup retained'
s3 -T "$BACKUP_DIR/$NAME" "$base/$PREFIX/$NAME" -o /dev/null 2>> "$LOG" || fail 'offsite upload failed; local backup retained'
verify_remote "$BACKUP_DIR/$NAME" || fail 'offsite upload verification failed; local backup retained'
log "Uploaded and verified $NAME (size + MD5)"
prune_local || fail 'some older archives could not be verified offsite; kept them'
log "$(archives | wc -l) local backups retained, $(( $(free_kb) / 1024 )) MiB free"

# Only expire offsite history after today's new copy passed verification.
cutoff=$(date -u -d "-$REMOTE_DAYS days" +%Y%m%d)
listing=$(s3 "$base/?list-type=2&prefix=$PREFIX/" 2>> "$LOG") || fail 'offsite retention listing failed'
while IFS= read -r key; do
  if [[ "$key" =~ ^mongodb/([0-9]{8})_[0-9]{6}\.(archive|tar)\.gz$ ]] &&
     [ "${BASH_REMATCH[1]}" -lt "$cutoff" ]; then
    s3 -X DELETE "$base/$key" -o /dev/null 2>> "$LOG" || fail 'offsite retention delete failed'
    log "Removed offsite $key"
  fi
done < <(printf '%s\n' "$listing" | grep -oE '<Key>[^<]+</Key>' | sed -E 's#</?Key>##g')
