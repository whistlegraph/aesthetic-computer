#!/usr/bin/env fish
# lith Deployment Script
# Deploys the AC monolith (frontend + API) to DigitalOcean droplet

set RED '\033[0;31m'
set GREEN '\033[0;32m'
set YELLOW '\033[1;33m'
set NC '\033[0m'

set SCRIPT_DIR (dirname (status --current-filename))
set REPO_ROOT (realpath "$SCRIPT_DIR/..")
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set SERVICE_ENV "$VAULT_DIR/lith/.env"
set LITH_USER "root"
set REMOTE_DIR "/opt/ac"
set DEFAULT_LITH_HOST "lith.aesthetic.computer"
set DEFAULT_LITH_DROPLET_NAME "ac-lith"
set TARGET_HOST $DEFAULT_LITH_HOST
set TARGET_DROPLET_NAME $DEFAULT_LITH_DROPLET_NAME
set LOCAL_BRANCH (git -C $REPO_ROOT branch --show-current 2>/dev/null)
set TARGET_BRANCH $LOCAL_BRANCH

if set -q LITH_HOST
    set TARGET_HOST $LITH_HOST
end

if set -q LITH_DROPLET_NAME
    set TARGET_DROPLET_NAME $LITH_DROPLET_NAME
end

if set -q DEPLOY_BRANCH
    set TARGET_BRANCH $DEPLOY_BRANCH
end

if test -z "$TARGET_BRANCH"
    set TARGET_BRANCH main
end

function ssh_ok --argument host
    ssh -i $SSH_KEY -o StrictHostKeyChecking=no -o ConnectTimeout=10 $LITH_USER@$host "echo ok" &>/dev/null
end

function get_do_token
    if set -q DIGITALOCEAN_ACCESS_TOKEN
        echo $DIGITALOCEAN_ACCESS_TOKEN
        return 0
    end

    if set -q DO_TOKEN
        echo $DO_TOKEN
        return 0
    end

    for token_file in \
        "$VAULT_DIR/help/deploy.env" \
        "$VAULT_DIR/judge/deploy.env" \
        "$VAULT_DIR/oven/deploy.env" \
        "$VAULT_DIR/at/deploy.env"
        if not test -f $token_file
            continue
        end

        set token_line (rg -m1 '^DO_TOKEN=' $token_file)
        if test -n "$token_line"
            string replace -r '^DO_TOKEN=' '' -- $token_line
            return 0
        end
    end

    return 1
end

function get_lith_host_from_do
    if not command -sq doctl
        return 1
    end

    set do_token (get_do_token)
    if test -z "$do_token"
        return 1
    end

    set row (env DIGITALOCEAN_ACCESS_TOKEN="$do_token" \
        doctl compute droplet list --format Name,PublicIPv4 --no-header 2>/dev/null | \
        rg "^$TARGET_DROPLET_NAME\\s")

    if test -z "$row"
        return 1
    end

    set compact_row (string replace -ra '\s+' ' ' -- (string trim -- $row))
    set fields (string split ' ' -- $compact_row)

    if test (count $fields) -lt 2
        return 1
    end

    echo $fields[2]
end

# Check for required files. If the plaintext key is missing but the GPG-armored
# vault copy exists, ask the slab menubar daemon for the passphrase and
# decrypt to a tempfile we use for this run only.
set DECRYPTED_KEY ""
function cleanup_decrypted_key --on-event fish_exit
    if test -n "$DECRYPTED_KEY"; and test -f $DECRYPTED_KEY
        rm -f $DECRYPTED_KEY
    end
end

if not test -f $SSH_KEY
    set GPG_KEY "$SSH_KEY.gpg"
    if not test -f $GPG_KEY
        echo -e "$RED x SSH key not found: $SSH_KEY (and no $GPG_KEY to decrypt)$NC"
        exit 1
    end

    set HELPER "$REPO_ROOT/slab/bin/ac-passphrase"
    if not test -x $HELPER
        echo -e "$RED x Vault is encrypted but $HELPER is missing/not executable.$NC"
        exit 1
    end

    echo -e "$GREEN-> Requesting vault passphrase via slab daemon...$NC"
    set passphrase ($HELPER vault 600)
    if test -z "$passphrase"
        echo -e "$RED x No passphrase provided; aborting.$NC"
        exit 1
    end

    set DECRYPTED_KEY (mktemp -t ac-lith-key)
    chmod 600 $DECRYPTED_KEY
    if not echo -n "$passphrase" | gpg --batch --pinentry-mode loopback \
            --passphrase-fd 0 --decrypt $GPG_KEY >$DECRYPTED_KEY 2>/dev/null
        rm -f $DECRYPTED_KEY
        set DECRYPTED_KEY ""
        echo -e "$RED x Failed to decrypt $GPG_KEY (wrong passphrase?).$NC"
        exit 1
    end
    set SSH_KEY $DECRYPTED_KEY
end

# Env upload is optional: if the vault has a lith/.env we upload it, otherwise
# we trust the env already present on the server at /opt/ac/system/.env.
set UPLOAD_ENV true
if not test -f $SERVICE_ENV
    echo -e "$YELLOW   Service env not found: $SERVICE_ENV$NC"
    echo -e "$YELLOW   Skipping env upload; preserving existing /opt/ac/system/.env on the server.$NC"
    set UPLOAD_ENV false
else if not rg -q '^DEPLOY_SECRET=' $SERVICE_ENV
    echo -e "$RED x DEPLOY_SECRET missing from $SERVICE_ENV$NC"
    echo -e "$YELLOW   Remove the file or add DEPLOY_SECRET. lith reads this file via /opt/ac/system/.env on the server.$NC"
    exit 1
end

# Test SSH connection
echo -e "$GREEN-> Testing SSH connection to $TARGET_HOST...$NC"
if not ssh_ok $TARGET_HOST
    set fallback_host (get_lith_host_from_do)

    if test -n "$fallback_host"; and test "$fallback_host" != "$TARGET_HOST"
        echo -e "$YELLOW   Falling back to DigitalOcean droplet $TARGET_DROPLET_NAME at $fallback_host.$NC"
        set TARGET_HOST $fallback_host
    end

    if not ssh_ok $TARGET_HOST
        echo -e "$RED x Cannot connect to $TARGET_HOST$NC"
        exit 1
    end
end

echo -e "$GREEN-> Connected to $TARGET_HOST.$NC"

# Deploy from pushed git state only. This avoids production drift from local rsync overlays.
echo -e "$GREEN-> Verifying origin/$TARGET_BRANCH...$NC"
git -C $REPO_ROOT fetch origin $TARGET_BRANCH --quiet
set ORIGIN_HEAD (git -C $REPO_ROOT rev-parse origin/$TARGET_BRANCH)

if test "$LOCAL_BRANCH" = "$TARGET_BRANCH"
    set LOCAL_HEAD (git -C $REPO_ROOT rev-parse HEAD)
    if test "$LOCAL_HEAD" != "$ORIGIN_HEAD"
        echo -e "$RED x Local $TARGET_BRANCH is ahead of origin/$TARGET_BRANCH.$NC"
        echo -e "$YELLOW   Push first. This deploy script no longer rsyncs uncommitted or unpushed code into production.$NC"
        exit 1
    end
end

echo -e "$GREEN-> Deploying branch $TARGET_BRANCH at $ORIGIN_HEAD...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "\
cd $REMOTE_DIR && \
git fetch origin $TARGET_BRANCH --quiet && \
if git show-ref --verify --quiet refs/heads/$TARGET_BRANCH; then \
  git checkout $TARGET_BRANCH --quiet; \
else \
  git checkout -B $TARGET_BRANCH origin/$TARGET_BRANCH --quiet; \
fi && \
git reset --hard origin/$TARGET_BRANCH --quiet && \
git rev-parse HEAD > system/public/.commit-ref"

# Upload env (only if the vault has one — otherwise keep the remote's existing env)
# Note: lith.service reads EnvironmentFile=/opt/ac/system/.env, so the canonical
# vault source lives at aesthetic-computer-vault/lith/.env and is uploaded into
# system/.env on the remote host.
if test $UPLOAD_ENV = true
    echo -e "$GREEN-> Uploading environment...$NC"
    scp -i $SSH_KEY $SERVICE_ENV $LITH_USER@$TARGET_HOST:$REMOTE_DIR/system/.env
else
    echo -e "$GREEN-> Using existing remote environment (no local vault env to upload).$NC"
end

# Install deps
echo -e "$GREEN-> Installing dependencies...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR/lith && npm install --omit=dev && cd $REMOTE_DIR/system && npm install --omit=dev && cd $REMOTE_DIR/oven && PUPPETEER_SKIP_DOWNLOAD=1 npm install --omit=dev"

# notepat.com amxd build stream.
# Modeled after `ac-os upload`'s OTA flow: only rebuild + re-upload
# when an amxd input actually changed since the last successful build
# (via --if-stale), then push the versioned artifact + latest.json to
# DO Spaces (--sync-spaces) so each release has a durable CDN URL
# outside lith.
#
# DO Spaces credentials live in aesthetic-computer-vault/spaces/.env
# (canonical: spaces/.env.gpg). We decrypt locally, ship to /tmp on
# lith for the build's lifetime, then remove — avoids storing S3 keys
# permanently in /opt/ac/system/.env. If the vault file is missing or
# GPG can't decrypt it, the build still runs — `--sync-spaces` just
# gracefully skips the upload with a warning.
set SPACES_ENV_SRC "$VAULT_DIR/spaces/.env"
set SPACES_ENV_GPG "$VAULT_DIR/spaces/.env.gpg"
set TMP_SPACES (mktemp)
set SPACES_READY false
if test -f $SPACES_ENV_SRC
    cp $SPACES_ENV_SRC $TMP_SPACES
    set SPACES_READY true
else if test -f $SPACES_ENV_GPG
    if gpg --batch --pinentry-mode loopback -d $SPACES_ENV_GPG >$TMP_SPACES 2>/dev/null
        set SPACES_READY true
    end
end

echo -e "$GREEN-> Refreshing notepat.com.amxd build stream...$NC"
if test $SPACES_READY = true
    scp -i $SSH_KEY -q $TMP_SPACES $LITH_USER@$TARGET_HOST:/tmp/notepat-spaces.env
    ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR && set -a && . /tmp/notepat-spaces.env && set +a && node ac-m4l/build-notepat.mjs --if-stale --sync-spaces; rc=\$?; rm -f /tmp/notepat-spaces.env; exit \$rc"
else
    echo -e "$YELLOW   spaces creds unavailable — building without S3 sync.$NC"
    ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR && node ac-m4l/build-notepat.mjs --if-stale"
end
rm -f $TMP_SPACES

# Install service file + Caddy config from the deployed checkout
echo -e "$GREEN-> Updating service + Caddy config...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "\
cp $REMOTE_DIR/lith/lith.service /etc/systemd/system/lith.service && \
cp $REMOTE_DIR/lith/Caddyfile /etc/caddy/Caddyfile && \
systemctl daemon-reload && \
systemctl reload caddy"

# Restart lith service
echo -e "$GREEN-> Restarting lith...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "systemctl restart lith"

# Purge the Cloudflare cache so new code is served immediately. Runtime .mjs
# (kidlisp, disk, graph, …) are STATIC sub-imports without the ?v= cache-bust
# boot.mjs puts on top-level modules, so the edge would otherwise serve stale
# code until the TTL. Creds come from the process env or a vault env file; if
# absent we skip (the short Caddy TTL still bounds staleness).
#
# Auth priority: the Global API Key (email + key) is preferred because the
# scoped CLOUDFLARE_API_TOKEN has gone stale in several env files; Bearer token
# is the fallback when that's all we have.
set -l CF_ENV_FILES $SERVICE_ENV \
    "$VAULT_DIR/.devcontainer/envs/devcontainer.env" \
    "$VAULT_DIR/oven/deploy.env" "$VAULT_DIR/nanos/conductor.env" \
    "$VAULT_DIR/help/deploy.env" "$VAULT_DIR/at/deploy.env"

# Read the first value found for each key across the candidate env files
# (an already-exported process env var wins).
function cf_lookup --argument-names key
    for f in $cf_env_files_g
        test -f $f; or continue
        set -l line (rg -m1 "^(export )?$key=" $f)
        if test -n "$line"
            string replace -r "^(export )?$key=" '' -- $line | string trim --chars '"\''
            return 0
        end
    end
    return 1
end
set -g cf_env_files_g $CF_ENV_FILES

set -l CF_EMAIL $CLOUDFLARE_EMAIL
set -l CF_KEY $CLOUDFLARE_API_KEY
set -l CF_TOKEN $CLOUDFLARE_API_TOKEN
test -z "$CF_EMAIL"; and set CF_EMAIL (cf_lookup CLOUDFLARE_EMAIL)
test -z "$CF_KEY"; and set CF_KEY (cf_lookup CLOUDFLARE_API_KEY)
test -z "$CF_TOKEN"; and set CF_TOKEN (cf_lookup CLOUDFLARE_API_TOKEN)
set -e cf_env_files_g

# Build curl auth args: Global API Key first, else Bearer token.
set -l CF_AUTH
if test -n "$CF_EMAIL"; and test -n "$CF_KEY"
    set CF_AUTH -H "X-Auth-Email: $CF_EMAIL" -H "X-Auth-Key: $CF_KEY"
else if test -n "$CF_TOKEN"
    set CF_AUTH -H "Authorization: Bearer $CF_TOKEN"
end

echo -e "$GREEN-> Purging Cloudflare cache...$NC"
if test -z "$CF_AUTH"
    echo -e "$YELLOW   No Cloudflare credentials found — skipping purge (Caddy short TTL still applies).$NC"
else
    set CF_ZONE (curl -s -X GET "https://api.cloudflare.com/client/v4/zones?name=aesthetic.computer" \
        $CF_AUTH -H "content-type: application/json" \
        | python3 -c "import json,sys; r=json.load(sys.stdin).get('result') or []; print(r[0]['id'] if r else '')" 2>/dev/null)
    if test -z "$CF_ZONE"
        echo -e "$YELLOW   Could not resolve zone id — skipping purge.$NC"
    else
        set CF_RESULT (curl -s -X POST "https://api.cloudflare.com/client/v4/zones/$CF_ZONE/purge_cache" \
            $CF_AUTH -H "content-type: application/json" \
            --data '{"purge_everything":true}' \
            | python3 -c "import json,sys; d=json.load(sys.stdin); print('ok' if d.get('success') else 'failed: '+str(d.get('errors')))" 2>/dev/null)
        echo -e "$GREEN   purge: $CF_RESULT$NC"
    end
end

echo -e "$GREEN-> Done. lith deployed to $TARGET_HOST$NC"

# Mirror slab/menuband/ to its standalone GitHub repo. Runs after a
# successful site deploy so the mirror's release pace matches what's
# actually live on aesthetic.computer/menuband. Failure here doesn't
# void the deploy — the mirror is a courtesy surface for external
# contributors, not a critical path.
set MIRROR_SYNC "$REPO_ROOT/slab/menuband/bin/mirror-sync.sh"
if test -x "$MIRROR_SYNC"
    echo -e "$GREEN-> Syncing menuband mirror...$NC"
    bash "$MIRROR_SYNC" 2>&1 | tail -3
    or echo -e "$YELLOW   menuband mirror sync failed (non-fatal)$NC"
end
