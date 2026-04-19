#!/usr/bin/env fish
## First-time bring-up of Datomic + Postgres on silo.
## Idempotent — safe to re-run.
##
## Prereqs (run locally, from repo root):
##   fish aesthetic-computer-vault/vault-tool.fish unlock
##
## What it does on silo (as root):
##   1. apt install: temurin-21-jdk, postgresql-16, unzip
##   2. Create `datomic` user + dirs (/opt/datomic, /var/log/datomic,
##      /var/lib/datomic, /var/backups/datomic)
##   3. Create PG datomic role + database using vault creds
##   4. Verify Datomic jar is present (or fail with instructions)
##
## Datomic Pro jar download:
##   The user must manually download datomic-pro-<version>.zip from
##   https://docs.datomic.com/pro/ (now Apache 2.0) and scp it to
##   /opt/datomic/ on silo BEFORE running this script. This script
##   handles unzip + symlink setup.
##
## Usage:
##   fish silo/datomic/bootstrap-silo.fish
##   fish silo/datomic/bootstrap-silo.fish --check   # dry run, capacity check only

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set BLUE '\033[0;34m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set POSTGRES_ENV "$VAULT_DIR/kidlisp-datomic/postgres.env"
set SILO_HOST "silo.aesthetic.computer"
set SILO_USER "root"

set CHECK_ONLY false
if contains -- --check $argv
    set CHECK_ONLY true
end

function die
    echo -e "$RED x $argv$NC" >&2
    exit 1
end

function step
    echo -e "$BLUE-> $argv$NC"
end

## Prereq checks (local)
if not test -f $SSH_KEY
    die "SSH key not found at $SSH_KEY. Run: fish aesthetic-computer-vault/vault-tool.fish unlock"
end

if not test -f $POSTGRES_ENV
    die "Postgres env not found at $POSTGRES_ENV. Run: fish aesthetic-computer-vault/vault-tool.fish unlock"
end

## Source the decrypted postgres env
set POSTGRES_USER (grep '^POSTGRES_USER=' $POSTGRES_ENV | cut -d= -f2-)
set POSTGRES_PASSWORD (grep '^POSTGRES_PASSWORD=' $POSTGRES_ENV | cut -d= -f2-)

if test -z "$POSTGRES_USER"; or test -z "$POSTGRES_PASSWORD"
    die "postgres.env missing POSTGRES_USER or POSTGRES_PASSWORD"
end

step "Testing SSH to $SILO_HOST..."
if not ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 \
        $SILO_USER@$SILO_HOST "echo ok" >/dev/null 2>&1
    die "Cannot connect to $SILO_HOST"
end

step "Capacity check on silo..."
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST '
    echo "  host: $(hostname)"
    echo "  ram:  $(free -h | awk "NR==2 {print \$2 \" total, \" \$3 \" used, \" \$7 \" available\"}")"
    echo "  disk: $(df -h / | awk "NR==2 {print \$2 \" total, \" \$3 \" used, \" \$4 \" avail on /\"}")"
    echo "  java: $(command -v java || echo \"(not installed)\")"
    echo "  pg:   $(command -v psql || echo \"(not installed)\")"
    echo "  datomic user: $(id -u datomic 2>/dev/null || echo \"(absent)\")"
'

if test $CHECK_ONLY = true
    echo -e "$GREEN Check-only mode — no changes made.$NC"
    exit 0
end

## Confirm
echo -e "$YELLOW About to install JDK + Postgres + configure Datomic on silo.$NC"
echo -n "Proceed? [y/N] "
read -P "" answer
if test "$answer" != "y"; and test "$answer" != "Y"
    echo "Aborted."
    exit 0
end

step "Installing JDK + Postgres via apt..."
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
    set -e
    export DEBIAN_FRONTEND=noninteractive

    # Temurin (Eclipse Adoptium) repo
    if ! command -v java >/dev/null 2>&1; then
        apt-get update -qq
        apt-get install -y -qq wget gnupg ca-certificates
        mkdir -p /etc/apt/keyrings
        wget -qO- https://packages.adoptium.net/artifactory/api/gpg/key/public | gpg --dearmor -o /etc/apt/keyrings/adoptium.gpg
        echo 'deb [signed-by=/etc/apt/keyrings/adoptium.gpg] https://packages.adoptium.net/artifactory/deb '(. /etc/os-release && echo \$VERSION_CODENAME)' main' > /etc/apt/sources.list.d/adoptium.list
        apt-get update -qq
        apt-get install -y -qq temurin-21-jdk
    fi

    if ! command -v psql >/dev/null 2>&1; then
        apt-get install -y -qq postgresql postgresql-contrib
    fi

    apt-get install -y -qq unzip
    systemctl enable --now postgresql
"
or die "apt install failed"

step "Creating datomic user + directories..."
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
    set -e
    if ! id -u datomic >/dev/null 2>&1; then
        useradd -r -s /bin/false -d /opt/datomic datomic
    fi
    mkdir -p /opt/datomic /opt/datomic/config /opt/kidlisp-sidecar \
             /var/log/datomic /var/lib/datomic /var/backups/datomic
    chown -R datomic:datomic /opt/datomic /opt/kidlisp-sidecar \
                             /var/log/datomic /var/lib/datomic /var/backups/datomic
"
or die "dir setup failed"

step "Creating Postgres datomic role + database (idempotent)..."
ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
    set -e
    sudo -u postgres psql -tAc \"SELECT 1 FROM pg_roles WHERE rolname='datomic'\" | grep -q 1 || \
        sudo -u postgres psql -c \"CREATE ROLE datomic WITH LOGIN PASSWORD '$POSTGRES_PASSWORD'\"
    sudo -u postgres psql -tAc \"SELECT 1 FROM pg_database WHERE datname='datomic'\" | grep -q 1 || \
        sudo -u postgres psql -c \"CREATE DATABASE datomic OWNER datomic\"
    sudo -u postgres psql -c \"GRANT ALL PRIVILEGES ON DATABASE datomic TO datomic\"
"
or die "postgres setup failed"

step "Checking for Datomic Pro jar on silo..."
set DATOMIC_PRESENT (ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "ls /opt/datomic/bin/transactor 2>/dev/null && echo yes || echo no" | tail -1)
if test "$DATOMIC_PRESENT" != "yes"
    echo -e "$YELLOW"
    echo "Datomic Pro transactor not installed at /opt/datomic/bin/transactor."
    echo ""
    echo "To install:"
    echo "  1. Download datomic-pro-<version>.zip from https://docs.datomic.com/pro/"
    echo "  2. scp -i $SSH_KEY datomic-pro-<version>.zip root@$SILO_HOST:/tmp/"
    echo "  3. ssh -i $SSH_KEY root@$SILO_HOST \\"
    echo "       'cd /opt/datomic && unzip -o /tmp/datomic-pro-*.zip && \\"
    echo "        ln -sfn datomic-pro-*/bin bin && \\"
    echo "        ln -sfn datomic-pro-*/lib lib && \\"
    echo "        chown -R datomic:datomic /opt/datomic'"
    echo ""
    echo "Then re-run this script to complete bring-up."
    echo -e "$NC"
    exit 2
end

step "Bootstrap complete."
echo -e "$GREEN"
echo "Next steps:"
echo "  1. fish silo/datomic/deploy.fish       (uploads transactor config, starts service)"
echo "  2. fish kidlisp-sidecar/deploy.fish    (builds + deploys sidecar)"
echo "  3. Add DATOMIC_SIDECAR_URL + DATOMIC_SIDECAR_ADMIN_SECRET to silo/.env"
echo "     (see silo/datomic/README.md for values)"
echo "  4. Redeploy silo:  fish silo/deploy.fish"
echo "  5. Run backfill:   ssh + node system/backend/backfill-kidlisp-to-datomic.mjs"
echo -e "$NC"
