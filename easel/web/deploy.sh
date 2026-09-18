#!/usr/bin/env bash
# Deploy only the browser client, built on lith from a pushed main commit.
set -euo pipefail
cd "$(dirname "$0")/../.."
revision=$(git rev-parse "${1:-HEAD}^{commit}")
host=${LITH_HOST:-lith.aesthetic.computer}
key=${LITH_SSH_KEY:-aesthetic-computer-vault/home/.ssh/id_rsa}
git fetch origin main --quiet
git merge-base --is-ancestor "$revision" origin/main || {
  echo 'Push this commit to main before deploying.' >&2; exit 1;
}
ssh -i "$key" -o BatchMode=yes -o ConnectTimeout=10 "root@$host" bash -s -- "$revision" <<'REMOTE'
set -euo pipefail
revision=$1
cd /opt/ac
git fetch origin main --quiet
git merge-base --is-ancestor "$revision" origin/main
release_root=/opt/ac/.aesel-web-releases
release="$release_root/$revision"
link=/opt/ac/system/public/aesel/try
if [ -e "$link" ] && [ ! -L "$link" ]; then
  echo "$link already exists as a real directory; refusing to replace it." >&2
  exit 1
fi
mkdir -p "$release_root"
work=$(mktemp -d "$release_root/.build.XXXXXX")
trap 'rm -rf "$work"' EXIT
if [ ! -f "$release/build.json" ]; then
  mkdir "$work/source"
  git archive "$revision" easel/web easel/phone easel/src easel/context \
    system/public/aesthetic.computer/dep/auth0-spa-js.production.js \
    | tar -x -C "$work/source"
  cd "$work/source"
  npm ci --prefix easel/web --include=dev --no-audit --no-fund
  AESEL_WEB_REVISION="$revision" node easel/web/build.mjs "$work/output"
  mv "$work/output" "$release"
fi
# GNU mv replaces the previous symlink atomically; retain earlier releases.
ln -s "$release" "$work/try"
mv -Tf "$work/try" "$link"
echo "Aesel web deployed: $revision → https://aesel.app/try/"
REMOTE
