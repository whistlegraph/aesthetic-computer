#!/usr/bin/env bash
# Deploy only the browser client, built on lith from a pushed main commit.
set -euo pipefail
cd "$(dirname "$0")/../.."
revision=$(git rev-parse "${1:-HEAD}^{commit}")
git cat-file -e "$revision:aesel/web/build.mjs"
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
  git archive "$revision" aesel/web aesel/phone aesel/src aesel/context \
    system/public/aesthetic.computer/dep/auth0-spa-js.production.js \
    | tar -x -C "$work/source"
  cd "$work/source"
  npm ci --prefix aesel/web --include=dev --no-audit --no-fund
  AESEL_WEB_REVISION="$revision" node aesel/web/build.mjs "$work/output"
  mv "$work/output" "$release"
fi
# GNU mv replaces the previous symlink atomically; retain earlier releases.
ln -s "$release" "$work/try"
mv -Tf "$work/try" "$link"
curl --fail --silent --show-error --max-time 20 "https://aesel.app/try/build.json?revision=$revision" |
  node -e 'let s="";process.stdin.on("data",x=>s+=x);process.stdin.on("end",()=>{if(JSON.parse(s).revision!==process.argv[1]){console.error("Served browser revision does not match deployment");process.exitCode=1;}})' "$revision"
echo "Aesel web deployed: $revision → https://aesel.app/try/"
REMOTE
