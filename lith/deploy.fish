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

# The upload replaces the server's env wholesale, so a key set by hand on lith
# and never written to the vault dies on the next deploy (the Whistlegraph
# turn worker's secret did, 2026-10-11). lith/.env.keys names every key that
# must survive; refuse rather than ship an env that is missing one.
set ENV_KEYS "$VAULT_DIR/lith/.env.keys"
if test "$UPLOAD_ENV" = true; and test -f $ENV_KEYS
    set missing_keys
    for key in (string match -rv '^\s*(#|$)' < $ENV_KEYS | string trim)
        rg -q "^$key=." $SERVICE_ENV; or set -a missing_keys $key
    end
    if test (count $missing_keys) -gt 0
        echo -e "$RED x $SERVICE_ENV is missing keys that lith/.env.keys requires: $missing_keys$NC"
        exit 1
    end
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

# A feature branch must carry the current production routes and assets. An old
# branch deployment previously removed mime.ac and served the AC prompt there.
if test "$TARGET_BRANCH" != "main"
    if not git -C $REPO_ROOT fetch origin main --quiet
        echo -e "$RED x Could not verify current main; leaving production unchanged.$NC"
        exit 1
    end
    if not git -C $REPO_ROOT merge-base --is-ancestor origin/main $ORIGIN_HEAD
        echo -e "$RED x $TARGET_BRANCH is missing current main; deployment would roll back live changes.$NC"
        echo -e "$YELLOW   Rebase or merge origin/main into the branch, then push and deploy again.$NC"
        exit 1
    end
end

if test "$LOCAL_BRANCH" = "$TARGET_BRANCH"
    set LOCAL_HEAD (git -C $REPO_ROOT rev-parse HEAD)
    # A behind checkout is safe: production uses origin's pushed commit below.
    # Refuse only local commits not preserved on the deployment branch.
    if not git -C $REPO_ROOT merge-base --is-ancestor $LOCAL_HEAD $ORIGIN_HEAD
        echo -e "$RED x Local $TARGET_BRANCH is ahead of or diverged from origin/$TARGET_BRANCH.$NC"
        echo -e "$YELLOW   Push first. This deploy script no longer rsyncs uncommitted or unpushed code into production.$NC"
        exit 1
    end
end

# Validate the environment before any production checkout or product changes.
# Note: lith.service reads EnvironmentFile=/opt/ac/system/.env, so the canonical
# vault source lives at aesthetic-computer-vault/lith/.env and is uploaded into
# system/.env on the remote host.
if test $UPLOAD_ENV = true
    # The env is the one thing a deploy ships without ever trying it first, and
    # on 2026-08-09 a stale MongoDB password rode one up and took every
    # database-backed endpoint down for fourteen hours. Try the credential from
    # here; a password that will not open the database on this machine will not
    # open it on the server either.
    echo -e "$GREEN-> Verifying environment credentials...$NC"
    if not node $REPO_ROOT/lith/verify-env.mjs $SERVICE_ENV
        echo -e "$RED x Refusing to upload an environment that cannot reach its database.$NC"
        echo -e "$YELLOW   $SERVICE_ENV is stale. Production is untouched — the running$NC"
        echo -e "$YELLOW   env on $TARGET_HOST is still whatever last worked.$NC"
        exit 1
    end
    # The upload replaces the server's env wholesale, and every Mac carries its
    # own copy of the vault env. On 2026-10-14 blueberry deployed a copy that
    # neo had outgrown by seven keys, and No Paint's cloud catalog and the
    # choir uploads went dark with no error anywhere. lith/.env.keys only
    # catches keys someone remembered to list, so compare against the server
    # itself: refuse to ship an env that drops a key lith is running with.
    # DEPLOY_ENV_DROP="KEY OTHER_KEY" names keys you mean to retire.
    echo -e "$GREEN-> Comparing environment keys with $TARGET_HOST...$NC"
    set remote_keys (ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cut -d= -f1 $REMOTE_DIR/system/.env 2>/dev/null | grep -E '^[A-Z0-9_]+\$' | sort -u")
    set local_keys (string match -rg '^([A-Z0-9_]+)=' < $SERVICE_ENV | sort -u)
    set dropped_keys
    for key in $remote_keys
        contains -- $key $local_keys; or contains -- $key (string split ' ' -- "$DEPLOY_ENV_DROP"); or set -a dropped_keys $key
    end
    set added_keys
    for key in $local_keys
        contains -- $key $remote_keys; or set -a added_keys $key
    end
    if test (count $dropped_keys) -gt 0
        echo -e "$RED x $SERVICE_ENV would drop keys $TARGET_HOST is running with: $dropped_keys$NC"
        echo -e "$YELLOW   This Mac's vault env is stale. Copy lith/.env from the Mac that deployed last$NC"
        echo -e "$YELLOW   (or merge the missing lines in), then deploy again. To retire a key on purpose,$NC"
        echo -e "$YELLOW   run with DEPLOY_ENV_DROP=\"$dropped_keys\". Production is untouched.$NC"
        exit 1
    end
    if test (count $added_keys) -gt 0
        echo -e "$GREEN   adding keys: $added_keys$NC"
    end
end

# Operational privacy must not disable server-side diagnostics at cutover.
if test "$UPLOAD_ENV" = true
    if not rg -q '^LITH_OPERATIONS_TOKEN=.' "$SERVICE_ENV"
        echo -e "$RED x Prepare LITH_OPERATIONS_TOKEN in the private Lith environment before release.$NC"
        exit 1
    end
else if not ssh -i "$SSH_KEY" "$LITH_USER@$TARGET_HOST" "grep -q '^LITH_OPERATIONS_TOKEN=.' '$REMOTE_DIR/system/.env'"
    echo -e "$RED x Production needs LITH_OPERATIONS_TOKEN before operational routes become private.$NC"
    exit 1
end

# Freeze the private revision and backup the installed products before reset.
source "$REPO_ROOT/toolchain/private-products-deploy.fish"
prepare_private_products "$REPO_ROOT" "$SSH_KEY" "$LITH_USER@$TARGET_HOST" "$REMOTE_DIR" node "$ORIGIN_HEAD"; or exit 1

echo -e "$GREEN-> Deploying branch $TARGET_BRANCH at $ORIGIN_HEAD...$NC"
set PREVIOUS_HEAD (ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR && git rev-parse HEAD")
if test -z "$PREVIOUS_HEAD"
    echo -e "$RED x Could not resolve the currently deployed commit.$NC"
    exit 1
end

# Keep the pre-cutover environment and routing alongside the product snapshot.
if not ssh -i "$SSH_KEY" "$LITH_USER@$TARGET_HOST" "umask 077 && cp '$REMOTE_DIR/system/.env' '$PRODUCT_REMOTE_STAGE/lith.env.before' && cp /etc/caddy/Caddyfile '$PRODUCT_REMOTE_STAGE/Caddyfile.before' && git -C '$REMOTE_DIR' rev-parse HEAD > '$PRODUCT_REMOTE_STAGE/core.before'"
    echo -e "$RED x Could not preserve the pre-release environment and routing.$NC"
    exit 1
end
echo -e "$GREEN-> Rollback snapshot: $PRODUCT_REMOTE_STAGE ($PREVIOUS_HEAD)$NC"

if not ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "\
cd $REMOTE_DIR && \
git fetch origin $TARGET_BRANCH --quiet && \
if git show-ref --verify --quiet refs/heads/$TARGET_BRANCH; then \
  git checkout $TARGET_BRANCH --quiet; \
else \
  git checkout -B $TARGET_BRANCH origin/$TARGET_BRANCH --quiet; \
fi && \
git reset --hard $ORIGIN_HEAD --quiet && \
$PRODUCT_RUN install $PRODUCT_REMOTE_STAGE/incoming && \
git rev-parse HEAD > system/public/.commit-ref && \
git rev-list --count HEAD > system/public/.commit-count && \
sh xbox/tools/precompress-live.sh"
    echo -e "$RED x Failed to install the composed release; restoring $PREVIOUS_HEAD.$NC"
    ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR && git reset --hard $PREVIOUS_HEAD --quiet && $PRODUCT_RUN install $PRODUCT_REMOTE_STAGE/previous && git rev-parse HEAD > system/public/.commit-ref && git rev-list --count HEAD > system/public/.commit-count && sh xbox/tools/precompress-live.sh"
    exit 1
end

# Advisory, not a gate: oskiewar ships on its own clock (npm run
# oskiewar:deploy reburns the preview before it deploys), so unreleased
# oskiewar commits on main must not hold every other lith change hostage.
echo -e "$GREEN-> Verifying oskiewar social preview freshness...$NC"
if not ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR && node xbox/live/render-social-preview.mjs --check"
    echo -e "$RED ! oskiewar social preview is stale; deploying anyway. Run npm run oskiewar:deploy to release oskiewar.$NC"
end

echo -e "$GREEN-> Verifying disk worker bundle freshness...$NC"
if not ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR/system && node scripts/verify-disk-worker.mjs"
    echo -e "$RED x Disk worker bundle is missing or stale; restoring $PREVIOUS_HEAD.$NC"
    ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "\
cd $REMOTE_DIR && \
git reset --hard $PREVIOUS_HEAD --quiet && \
$PRODUCT_RUN install $PRODUCT_REMOTE_STAGE/previous && \
git rev-parse HEAD > system/public/.commit-ref && \
git rev-list --count HEAD > system/public/.commit-count && \
sh xbox/tools/precompress-live.sh"
    exit 1
end

# Validate the committed public-host inventory on the deployment checkout.
# This does not read the maintainer's possibly dirty local working tree.
echo -e "$GREEN-> Verifying public machine discovery...$NC"
if not ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR && node utilities/generate-machine-manifests.mjs --check"
    echo -e "$RED x Machine discovery is stale or a public host is unclassified; restoring $PREVIOUS_HEAD.$NC"
    ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "\
cd $REMOTE_DIR && \
git reset --hard $PREVIOUS_HEAD --quiet && \
$PRODUCT_RUN install $PRODUCT_REMOTE_STAGE/previous && \
git rev-parse HEAD > system/public/.commit-ref && \
git rev-list --count HEAD > system/public/.commit-count && \
sh xbox/tools/precompress-live.sh"
    exit 1
end

# Upload only the environment that passed preflight before the source cutover.
if test "$UPLOAD_ENV" = true
    echo -e "$GREEN-> Uploading environment...$NC"
    scp -i $SSH_KEY $SERVICE_ENV $LITH_USER@$TARGET_HOST:$REMOTE_DIR/system/.env; or exit 1
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
#
# The remote sources this file with `.`, so it must contain assignments and
# nothing else. A plaintext vault .env written by a `gpg -d > .env` that
# forgot to redirect stderr carries gpg's "encrypted with … key" preamble on
# its first lines, and each of those becomes a `command not found` on lith.
# Keep only well-formed KEY=VALUE lines — the whole line is preserved, so
# values containing `=` or spaces survive intact.
set SPACES_ENV_SRC "$VAULT_DIR/spaces/.env"
set SPACES_ENV_GPG "$VAULT_DIR/spaces/.env.gpg"
set TMP_SPACES (mktemp)
set SPACES_READY false
if test -f $SPACES_ENV_SRC
    grep -E '^[A-Za-z_][A-Za-z0-9_]*=' $SPACES_ENV_SRC >$TMP_SPACES
    set SPACES_READY true
else if test -f $SPACES_ENV_GPG
    # gpg waits forever for a passphrase the agent has not cached — that held
    # deploys for ten minutes at a time. The Spaces env is optional, so hand
    # gpg the passphrase the slab daemon already gave us when there is one,
    # and bound the wait either way.
    # The bounded decrypt runs under sh: when `timeout` kills a hung gpg,
    # fish 4 panics on the signal status of a pipeline member ("Unsupported
    # status value", src/proc.rs) and takes the whole deploy with it. sh
    # absorbs that and hands fish a plain exit code.
    set -l bounded ""
    if command -q timeout
        set bounded "timeout 20"
    else if command -q gtimeout
        set bounded "gtimeout 20"
    end
    if test -n "$passphrase"
        echo -n "$passphrase" | sh -c "$bounded gpg --batch --pinentry-mode loopback --passphrase-fd 0 -d \"\$1\" 2>/dev/null | grep -E '^[A-Za-z_][A-Za-z0-9_]*='" sh $SPACES_ENV_GPG >$TMP_SPACES
    else
        sh -c "$bounded gpg --batch --pinentry-mode loopback -d \"\$1\" 2>/dev/null </dev/null | grep -E '^[A-Za-z_][A-Za-z0-9_]*='" sh $SPACES_ENV_GPG >$TMP_SPACES
    end
    if test -s $TMP_SPACES
        set SPACES_READY true
    else
        echo -e "$YELLOW   Spaces env not decrypted (no cached passphrase) — building without S3 sync.$NC"
    end
end

# Aesel is not packed here. It releases on its own clock with `ac-deploy
# aesel`, which packs aesel/ from the knot's main in a scratch directory on
# this box and swaps the tarball and manifest in, so a site deploy never
# re-releases Aesel and an Aesel release never moves this checkout. The
# served aesel.tar.gz and aesel.json are untracked and survive the reset above.

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
# systemctl reload uses --force, rebuilding TLS listeners even for identical
# config. Ordinary code deploys should leave them running; caddy reload skips
# unchanged config. Certificate-only rotations still need an explicit --force.
echo -e "$GREEN-> Updating service + Caddy config...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "\
cp $REMOTE_DIR/lith/lith.service /etc/systemd/system/lith.service && \
cp $REMOTE_DIR/lith/lith-mail.service /etc/systemd/system/lith-mail.service && \
id -u lith-mail >/dev/null 2>&1 || useradd --system --shell /usr/sbin/nologin --home-dir /nonexistent lith-mail && \
install -m 755 $REMOTE_DIR/lith/lith-mail-renew.sh /etc/letsencrypt/renewal-hooks/deploy/lith-mail.sh 2>/dev/null; \
sh $REMOTE_DIR/lith/lith-mail-renew.sh && \
systemctl enable -q lith-mail && \
cp $REMOTE_DIR/lith/Caddyfile /etc/caddy/Caddyfile && \
mkdir -p /var/lib/aesthetic-computer/gym.anthonyzollo.com && \
if [ ! -f /var/lib/aesthetic-computer/gym.anthonyzollo.com/index.html ]; then \
  cp $REMOTE_DIR/lith/gym/index.html /var/lib/aesthetic-computer/gym.anthonyzollo.com/index.html; \
fi && \
systemctl daemon-reload && \
caddy reload --config /etc/caddy/Caddyfile --adapter caddyfile"

# Restart lith service, and the Amail SMTP door beside it
echo -e "$GREEN-> Restarting lith + lith-mail...$NC"
ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "systemctl restart lith lith-mail"

# Stop automatic transcript deletion immediately, even before the next API call.
echo -e "$GREEN-> Migrating Aesel transcript expiration to a soft marker...$NC"
if not ssh -i $SSH_KEY $LITH_USER@$TARGET_HOST "cd $REMOTE_DIR/system && node --env-file=.env --input-type=module -e 'const {connect,closePool}=await import(\"./backend/database.mjs\"); const {ensureTranscriptIndexes}=await import(\"./backend/easel-transcripts.mjs\"); try { const {db}=await connect(); await ensureTranscriptIndexes(db); } finally { await closePool(); }'"
    echo -e "$RED x Transcript retention migration failed.$NC"
    exit 1
end
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
    # Every zone this deploy serves, not just the apex. prompt.ac is a separate
    # Cloudflare zone, so purging aesthetic.computer left its edge holding
    # whatever it had — including a cached 404 for a file that had since been
    # built, which is exactly how prompt.ac/easel.tar.gz stayed missing after a
    # deploy that produced it.
    for CF_HOST in aesthetic.computer prompt.ac
        set CF_ZONE (curl -s -X GET "https://api.cloudflare.com/client/v4/zones?name=$CF_HOST" \
            $CF_AUTH -H "content-type: application/json" \
            | python3 -c "import json,sys; r=json.load(sys.stdin).get('result') or []; print(r[0]['id'] if r else '')" 2>/dev/null)
        if test -z "$CF_ZONE"
            echo -e "$YELLOW   $CF_HOST: could not resolve zone id — skipping.$NC"
        else
            set CF_RESULT (curl -s -X POST "https://api.cloudflare.com/client/v4/zones/$CF_ZONE/purge_cache" \
                $CF_AUTH -H "content-type: application/json" \
                --data '{"purge_everything":true}' \
                | python3 -c "import json,sys; d=json.load(sys.stdin); print('ok' if d.get('success') else 'failed: '+str(d.get('errors')))" 2>/dev/null)
            echo -e "$GREEN   purge $CF_HOST: $CF_RESULT$NC"
        end
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

# Last, and deliberately last: ask a database-backed endpoint for a real answer.
#
# Everything above can pass while every query behind it fails — that is exactly
# what happened on 2026-08-09, when a stale credential rode up with a perfectly
# good build and nothing noticed for fourteen hours. So the deploy is not
# allowed to report success without one live query having worked.
#
# It runs after the purge and the mirror rather than before, because a failure
# here is a report, not a gate: the code is already out, and skipping the cache
# purge on the way past would only add a second problem. Nor does it roll back
# — restoring a commit does not fix a password, and pretending it might would
# hide the actual fault.
#
# The public host, not $TARGET_HOST: lith.aesthetic.computer is the SSH target
# and serves no vhost, so probing it only ever measures curl's disappointment.
set DB_PROBE_HOST "aesthetic.computer"
if set -q LITH_PROBE_HOST
    set DB_PROBE_HOST $LITH_PROBE_HOST
end
echo -e "$GREEN-> Verifying the database is answering...$NC"
set DB_CODE "000"
for attempt in 1 2 3 4 5 6 7 8 9 10
    set DB_CODE (curl -s -o /dev/null -w '%{http_code}' --max-time 15 \
        "https://$DB_PROBE_HOST/api/oskiewar-replays?limit=1")
    if test "$DB_CODE" = "200"
        break
    end
    sleep 3
end
if test "$DB_CODE" = "200"
    echo -e "$GREEN   database: $DB_PROBE_HOST is answering queries$NC"
else
    echo -e "$RED x Deployed, but the database is not answering (HTTP $DB_CODE).$NC"
    echo -e "$YELLOW   The code is live; queries are failing. Check the credentials in$NC"
    echo -e "$YELLOW   $SERVICE_ENV, then: ssh $LITH_USER@$TARGET_HOST journalctl -u lith -n 50$NC"
    exit 1
end
