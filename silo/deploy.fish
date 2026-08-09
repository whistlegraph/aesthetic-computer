#!/usr/bin/env fish
# Silo Deployment Script
# Deploys the Silo data dashboard to DigitalOcean droplet
#
# Usage:
#   fish deploy.fish          # Full deploy (server + dashboard, restarts service)
#   fish deploy.fish --dash   # Dashboard-only (uploads HTML, SIGHUP reload, zero downtime)

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set SERVICE_ENV "$VAULT_DIR/silo/.env"
set SILO_HOST "silo.aesthetic.computer"
set SILO_USER "root"
set REMOTE_DIR "/opt/silo"

set DASH_ONLY false
if contains -- --dash $argv
    set DASH_ONLY true
end

# Check for required files
if not test -f $SSH_KEY
    echo -e "$RED x SSH key not found: $SSH_KEY$NC"
    exit 1
end

if not test -f $SERVICE_ENV; and test $DASH_ONLY = false
    echo -e "$RED x Service env not found: $SERVICE_ENV$NC"
    exit 1
end

# Test SSH connection
echo -e "$GREEN-> Testing SSH connection to $SILO_HOST...$NC"
if not ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 $SILO_USER@$SILO_HOST "echo ok" &>/dev/null
    echo -e "$RED x Cannot connect to $SILO_HOST$NC"
    exit 1
end
echo -e "$GREEN   Connected.$NC"

if test $DASH_ONLY = true
    # Dashboard-only deploy: upload HTML, send SIGHUP (no restart)
    echo -e "$GREEN-> Uploading dashboard.html (zero-downtime)...$NC"
    scp -i $SSH_KEY -o StrictHostKeyChecking=no \
        $SCRIPT_DIR/dashboard.html \
        $SILO_USER@$SILO_HOST:$REMOTE_DIR/

    ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
        chown silo:silo $REMOTE_DIR/dashboard.html
        kill -HUP \$(systemctl show -p MainPID --value silo)
    "
    echo -e "$GREEN   Dashboard reloaded (SIGHUP). No restart needed.$NC"
else
    # Full deploy: upload all files + restart
    echo -e "$GREEN-> Uploading silo files...$NC"
    scp -i $SSH_KEY -o StrictHostKeyChecking=no \
        $SCRIPT_DIR/server.mjs \
        $SCRIPT_DIR/bluesky-ingest.mjs \
        $SCRIPT_DIR/dashboard.html \
        $SCRIPT_DIR/package.json \
        $SCRIPT_DIR/package-lock.json \
        $SILO_USER@$SILO_HOST:$REMOTE_DIR/

    # Keep the .env we are about to replace. The box's copy can drift from the
    # vault's — credentials get rotated on the box and the vault copy goes
    # stale — and overwriting it blind takes mongo down with no way back:
    # 2026-08-09 this replaced a working aesthetic_app password with a stale one
    # and there was no copy of the old file anywhere.
    # \$(date) so the REMOTE shell stamps it — and because fish only does
    # command substitution inside double quotes as $(...), never (...), which
    # silently shipped a literal paren to bash the first time this ran.
    ssh -i $SSH_KEY $SILO_USER@$SILO_HOST \
        "test -f $REMOTE_DIR/.env && cp -a $REMOTE_DIR/.env $REMOTE_DIR/.env.bak.\$(date +%Y%m%d-%H%M%S) || true"

    # Upload production .env from vault
    scp -i $SSH_KEY -o StrictHostKeyChecking=no \
        $SERVICE_ENV $SILO_USER@$SILO_HOST:$REMOTE_DIR/.env

    # Stamp the commit being shipped. silo is scp'd as loose files with no git
    # repo on the box, so this is the only way it can answer "what am I
    # running?" — and without that answer a stale silo looks identical to a
    # current one, which is the failure this whole contract exists to catch.
    set -l SHA (git -C $SCRIPT_DIR/.. rev-parse HEAD 2>/dev/null)
    # This deploy scp's the working tree, not a git checkout, so an uncommitted
    # silo/ file ships while the stamp still names HEAD — provenance that reads
    # clean and is not. Say so rather than let the fleet believe a sha that does
    # not contain what is running.
    set -l DIRTY 0
    if test -n (git -C $SCRIPT_DIR/.. status --porcelain -- silo/ | head -1)
        set DIRTY 1
    end
    if test -n "$SHA"
        set -l suffix ""
        test $DIRTY -eq 1; and set suffix " + UNCOMMITTED silo/ changes"
        echo -e "$GREEN-> Stamping AC_GIT_SHA="(string sub -l 9 $SHA)"$suffix$NC"
        test $DIRTY -eq 1; and echo -e "$RED   ! shipping files that are not in any commit — ac-fleet will flag this$NC"
        ssh -i $SSH_KEY $SILO_USER@$SILO_HOST \
            "sed -i '/^AC_GIT_SHA=/d;/^AC_GIT_DIRTY=/d' $REMOTE_DIR/.env
             echo 'AC_GIT_SHA=$SHA' >> $REMOTE_DIR/.env
             echo 'AC_GIT_DIRTY=$DIRTY' >> $REMOTE_DIR/.env"
    else
        echo -e "$RED   ! could not resolve HEAD — silo will report sha:null$NC"
    end

    # Fix ownership, install deps if needed, restart
    echo -e "$GREEN-> Installing dependencies & restarting...$NC"
    ssh -i $SSH_KEY $SILO_USER@$SILO_HOST "
        chown -R silo:silo $REMOTE_DIR
        cd $REMOTE_DIR && npm install --production --silent 2>&1 | tail -1
        systemctl restart silo
        sleep 2
        systemctl is-active silo
    "

    set STATUS $status
    if test $STATUS -eq 0
        echo -e "$GREEN   Silo is running.$NC"
    else
        echo -e "$RED x Silo failed to start. Check logs:$NC"
        echo -e "$YELLOW   ssh -i $SSH_KEY $SILO_USER@$SILO_HOST journalctl -u silo -n 30$NC"
        exit 1
    end
end

echo ""
echo -e "$GREEN Done. Silo deployed to https://$SILO_HOST$NC"
