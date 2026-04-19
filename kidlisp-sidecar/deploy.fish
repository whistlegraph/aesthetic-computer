#!/usr/bin/env fish
## Builds the kidlisp-sidecar uberjar and ships it to silo.
## Assumes silo/datomic/deploy.fish has already set up the transactor.
##
## Usage:
##   fish deploy.fish          Full deploy (build + ship + restart)
##   fish deploy.fish --no-build  Ship existing uberjar only

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set SIDECAR_ENV_GPG "$VAULT_DIR/kidlisp-datomic/sidecar.env.gpg"
set SILO_HOST "silo.aesthetic.computer"
set SILO_USER "root"
set REMOTE_DIR "/opt/kidlisp-sidecar"
set JAR "$SCRIPT_DIR/target/kidlisp-sidecar.jar"

set DO_BUILD true
if contains -- --no-build $argv
    set DO_BUILD false
end

## Prereqs
if not test -f $SSH_KEY
    echo -e "$RED x SSH key not found: $SSH_KEY$NC"
    exit 1
end

if not test -f $SIDECAR_ENV_GPG
    echo -e "$RED x Vault file missing: $SIDECAR_ENV_GPG$NC"
    echo -e "$YELLOW   Populate via vault workflow before deploying.$NC"
    exit 1
end

## Build
if test $DO_BUILD = true
    echo -e "$GREEN-> Building uberjar...$NC"
    cd $SCRIPT_DIR
    clojure -X:uberjar
    if test $status -ne 0
        echo -e "$RED x Build failed.$NC"
        exit 1
    end
end

if not test -f $JAR
    echo -e "$RED x Uberjar not found: $JAR$NC"
    exit 1
end

## Decrypt env locally, upload, then wipe local copy
set TMP_ENV (mktemp)
gpg --decrypt --quiet --output $TMP_ENV $SIDECAR_ENV_GPG
if test $status -ne 0
    echo -e "$RED x gpg decrypt failed.$NC"
    rm -f $TMP_ENV
    exit 1
end

echo -e "$GREEN-> Uploading jar + env + systemd unit...$NC"
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $JAR \
    $SILO_USER@$SILO_HOST:$REMOTE_DIR/kidlisp-sidecar.jar
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $TMP_ENV \
    $SILO_USER@$SILO_HOST:$REMOTE_DIR/.env
scp -i $SSH_KEY -o StrictHostKeyChecking=no \
    $SCRIPT_DIR/kidlisp-sidecar.service \
    $SILO_USER@$SILO_HOST:/etc/systemd/system/kidlisp-sidecar.service

rm -f $TMP_ENV

## Permission + restart
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
    chown datomic:datomic $REMOTE_DIR/kidlisp-sidecar.jar $REMOTE_DIR/.env
    chmod 0600 $REMOTE_DIR/.env
    systemctl daemon-reload
    systemctl restart kidlisp-sidecar
    sleep 2
    systemctl is-active kidlisp-sidecar
"
set STATUS $status
if test $STATUS -eq 0
    echo -e "$GREEN   Sidecar restarted OK.$NC"
else
    echo -e "$RED x Sidecar failed to start. Check:$NC"
    echo -e "$YELLOW   ssh -i $SSH_KEY $SILO_USER@$SILO_HOST journalctl -u kidlisp-sidecar -n 50$NC"
    exit 1
end

echo -e "$GREEN Done.$NC"
