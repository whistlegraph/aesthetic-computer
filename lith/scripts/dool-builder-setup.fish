#!/usr/bin/env fish
# One-time setup for lith's dool Aesel builder (lith/dool-builder.service).
#
# 1. Signs in to AC in the browser into a fresh token file. This gives lith its
#    own refresh token instead of a copy of this Mac's ~/.ac-token: Auth0
#    rotates refresh tokens, so two machines sharing one would log each other out.
# 2. Writes DOOL_BUILDER_TOKEN to /etc/aesthetic-computer/dool.env on lith if it
#    is not there yet (root-only, outside system/.env, which deploys replace),
#    moving a hand-set one out of system/.env with its value.
# 3. Installs the login as /var/lib/dool-builder/.ac-token and restarts the
#    builder. lith.service loads dool.env on its next restart (every deploy).
#
# First run: moves the secret, then asks for a deploy. Second run: the login.
# Re-run any time to replace an expired builder login.

set SCRIPT_DIR (dirname (status --current-filename))
set REPO_ROOT (realpath "$SCRIPT_DIR/../..")
set SSH_KEY "$REPO_ROOT/aesthetic-computer-vault/home/.ssh/id_rsa"
set HOST root@lith.aesthetic.computer

if not test -f $SSH_KEY
    echo "x SSH key not found: $SSH_KEY"
    exit 1
end
function remote
    ssh -i $SSH_KEY -o ConnectTimeout=10 $HOST $argv
end

# The secret first. A hand-set DOOL_BUILDER_TOKEN in system/.env is moved
# over with its value, so a builder already using it keeps working once a
# deploy (DEPLOY_ENV_DROP=DOOL_BUILDER_TOKEN) retires it from system/.env.
echo "-> Ensuring the builder secret on lith..."
remote "install -d -m 755 /etc/aesthetic-computer && \
if [ -s /etc/aesthetic-computer/dool.env ]; then echo '   already present'; \
elif grep -q '^DOOL_BUILDER_TOKEN=' /opt/ac/system/.env; then \
  umask 077 && grep '^DOOL_BUILDER_TOKEN=' /opt/ac/system/.env | tail -1 > /etc/aesthetic-computer/dool.env && echo '   moved from system/.env'; \
else \
  umask 077 && echo DOOL_BUILDER_TOKEN=\$(openssl rand -hex 32) > /etc/aesthetic-computer/dool.env && echo '   created'; \
fi"; or exit 1

if not remote "test -f /etc/systemd/system/dool-builder.service && id -u dool-builder >/dev/null"
    echo "-> dool-builder.service is not on lith yet. Deploy, then run this again:"
    echo "   DEPLOY_ENV_DROP=DOOL_BUILDER_TOKEN ac-deploy"
    exit 0
end

set tmp (mktemp -d)
function cleanup --on-event fish_exit
    rm -rf $tmp
end

echo "-> Sign in to AC as the builder's account (a browser opens)..."
if not node --input-type=module -e "
import { ACSession } from '$REPO_ROOT/aesel/src/ac-session.mjs';
const session = new ACSession({ file: process.argv[1] });
const handle = await session.login({ onUrl: (url) => console.log('   ' + url) });
console.log('   signed in as ' + (handle ? '@' + String(handle).replace(/^@/, '') : 'an account with no handle'));
" $tmp/ac-token
    echo "x Sign-in failed."
    exit 1
end

echo "-> Installing the builder login..."
scp -i $SSH_KEY -q $tmp/ac-token $HOST:/var/lib/dool-builder/.ac-token; or exit 1
remote "chown dool-builder:dool-builder /var/lib/dool-builder/.ac-token && chmod 600 /var/lib/dool-builder/.ac-token"; or exit 1

echo "-> Restarting the builder..."
remote "systemctl restart dool-builder"; or exit 1
sleep 4
remote "systemctl is-active dool-builder; journalctl -u dool-builder -n 5 --no-pager -o cat"
