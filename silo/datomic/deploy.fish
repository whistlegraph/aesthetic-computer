#!/usr/bin/env fish
## Deploys Datomic transactor config, postgres init, and backup script to silo.
## Does NOT install JVM/Postgres/Datomic jar itself — those are one-time
## operator steps (see silo/datomic/README.md bring-up section).
##
## Usage:
##   fish deploy.fish          Full deploy (config + systemd + cron)
##   fish deploy.fish --config Config only (no systemd reload, no restart)

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set TRANSACTOR_PROPS_GPG "$VAULT_DIR/kidlisp-datomic/transactor.properties.gpg"
set POSTGRES_ENV_GPG "$VAULT_DIR/kidlisp-datomic/postgres.env.gpg"
set SILO_HOST "silo.aesthetic.computer"
set SILO_USER "root"
set REMOTE_OPT "/opt/datomic"

set CONFIG_ONLY false
if contains -- --config $argv
    set CONFIG_ONLY true
end

## Prereq checks
if not test -f $SSH_KEY
    echo -e "$RED x SSH key not found: $SSH_KEY$NC"
    exit 1
end

for f in $TRANSACTOR_PROPS_GPG $POSTGRES_ENV_GPG
    if not test -f $f
        echo -e "$RED x Vault file missing: $f$NC"
        echo -e "$YELLOW   Populate via vault workflow before deploying.$NC"
        exit 1
    end
end

echo -e "$GREEN-> Testing SSH to $SILO_HOST...$NC"
if not ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 \
        $SILO_USER@$SILO_HOST "echo ok" &>/dev/null
    echo -e "$RED x Cannot connect to $SILO_HOST$NC"
    exit 1
end

## Render transactor.properties from template + vault values in-memory.
## Secrets never touch disk on the dev machine — decrypted into /dev/shm,
## scp'd to silo, then shredded.
set TPL "$SCRIPT_DIR/transactor/transactor.properties.template"
set TMP_DIR (mktemp -d /dev/shm/datomic-render-XXXXXX)
set RENDERED "$TMP_DIR/transactor.properties"
set DECRYPTED_TRANSACTOR "$TMP_DIR/transactor.values"

## Decrypt transactor.properties values
gpg --decrypt --quiet --output $DECRYPTED_TRANSACTOR $TRANSACTOR_PROPS_GPG
if test $status -ne 0
    echo -e "$RED x Failed to decrypt $TRANSACTOR_PROPS_GPG$NC"
    shred -u $DECRYPTED_TRANSACTOR 2>/dev/null
    rmdir $TMP_DIR
    exit 1
end

## Source values into current fish env (isolated subshell vars)
set -l POSTGRES_USER (grep '^POSTGRES_USER=' $DECRYPTED_TRANSACTOR | cut -d= -f2-)
set -l POSTGRES_PASSWORD (grep '^POSTGRES_PASSWORD=' $DECRYPTED_TRANSACTOR | cut -d= -f2-)
set -l STORAGE_ACCESS_SECRET (grep '^STORAGE_ACCESS_SECRET=' $DECRYPTED_TRANSACTOR | cut -d= -f2-)

## Substitute placeholders
sed -e "s|{{POSTGRES_USER}}|$POSTGRES_USER|g" \
    -e "s|{{POSTGRES_PASSWORD}}|$POSTGRES_PASSWORD|g" \
    -e "s|{{STORAGE_ACCESS_SECRET}}|$STORAGE_ACCESS_SECRET|g" \
    $TPL > $RENDERED

echo -e "$GREEN-> Uploading rendered transactor.properties...$NC"
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $RENDERED \
    $SILO_USER@$SILO_HOST:/opt/datomic/config/transactor.properties
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST \
    "chown datomic:datomic /opt/datomic/config/transactor.properties; chmod 0600 /opt/datomic/config/transactor.properties"

## Shred plaintext (including sed's temp file if any)
shred -u $RENDERED $DECRYPTED_TRANSACTOR 2>/dev/null
rmdir $TMP_DIR

## Upload non-secret files
echo -e "$GREEN-> Uploading systemd unit, init.sql, backup.fish, cron...$NC"
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $SCRIPT_DIR/transactor/datomic-transactor.service \
    $SILO_USER@$SILO_HOST:/etc/systemd/system/datomic-transactor.service

scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $SCRIPT_DIR/postgres/init.sql \
    $SCRIPT_DIR/postgres/backup.fish \
    $SILO_USER@$SILO_HOST:$REMOTE_OPT/

scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $SCRIPT_DIR/backup.cron \
    $SILO_USER@$SILO_HOST:/etc/cron.d/datomic-backup

if test $CONFIG_ONLY = false
    ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
        chmod +x $REMOTE_OPT/backup.fish
        chmod 0644 /etc/cron.d/datomic-backup
        systemctl daemon-reload
        systemctl restart datomic-transactor
        sleep 2
        systemctl is-active datomic-transactor
    "
    set STATUS $status
    if test $STATUS -eq 0
        echo -e "$GREEN   Transactor restarted OK.$NC"
    else
        echo -e "$RED x Transactor failed to start. Check:$NC"
        echo -e "$YELLOW   ssh -i $SSH_KEY $SILO_USER@$SILO_HOST journalctl -u datomic-transactor -n 50$NC"
        exit 1
    end
end

echo -e "$GREEN Done.$NC"
