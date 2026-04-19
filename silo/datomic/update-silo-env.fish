#!/usr/bin/env fish
## Adds DATOMIC_SIDECAR_URL and DATOMIC_SIDECAR_ADMIN_SECRET to silo/.env,
## reading ADMIN_SECRET from kidlisp-datomic/sidecar.env.
##
## Safe to re-run: appends only if the vars aren't already present.
## Requires the vault to be unlocked (plaintext files on disk).
##
## Usage:
##   fish aesthetic-computer-vault/vault-tool.fish unlock
##   fish silo/datomic/update-silo-env.fish
##   fish aesthetic-computer-vault/vault-tool.fish lock   # optional

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../../aesthetic-computer-vault"
set SIDECAR_ENV "$VAULT_DIR/kidlisp-datomic/sidecar.env"
set SILO_ENV "$VAULT_DIR/silo/.env"

for f in $SIDECAR_ENV $SILO_ENV
    if not test -f $f
        echo -e "$RED x Not found: $f$NC"
        echo -e "$YELLOW   Run: fish aesthetic-computer-vault/vault-tool.fish unlock$NC"
        exit 1
    end
end

set ADMIN_SECRET (grep '^ADMIN_SECRET=' $SIDECAR_ENV | cut -d= -f2-)
if test -z "$ADMIN_SECRET"
    echo -e "$RED x ADMIN_SECRET missing from $SIDECAR_ENV$NC"
    exit 1
end

set CHANGED false

if not grep -q '^DATOMIC_SIDECAR_URL=' $SILO_ENV
    echo 'DATOMIC_SIDECAR_URL=http://127.0.0.1:8891' >> $SILO_ENV
    echo -e "$GREEN + DATOMIC_SIDECAR_URL added$NC"
    set CHANGED true
else
    echo "  DATOMIC_SIDECAR_URL already present — skipping"
end

if not grep -q '^DATOMIC_SIDECAR_ADMIN_SECRET=' $SILO_ENV
    echo "DATOMIC_SIDECAR_ADMIN_SECRET=$ADMIN_SECRET" >> $SILO_ENV
    echo -e "$GREEN + DATOMIC_SIDECAR_ADMIN_SECRET added$NC"
    set CHANGED true
else
    echo "  DATOMIC_SIDECAR_ADMIN_SECRET already present — skipping"
end

if test $CHANGED = true
    echo -e "$GREEN Done. Remember to re-lock the vault and redeploy silo:$NC"
    echo "  fish aesthetic-computer-vault/vault-tool.fish lock"
    echo "  fish silo/deploy.fish"
else
    echo -e "$GREEN Nothing to change.$NC"
end
