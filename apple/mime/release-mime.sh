#!/usr/bin/env bash
# One revision: website/config first, then the native MIME reader.
set -euo pipefail
app_dir=$(cd "$(dirname "$0")" && pwd)
repo_dir=$(cd "$app_dir/../.." && pwd)
cd "$repo_dir"
mode=${1:-device}
case "$mode" in device|testflight|web) ;; *) echo 'Usage: release-mime.sh device|testflight|web' >&2; exit 1 ;; esac
[[ $(git branch --show-current) == main ]] || { echo 'Release MIME from main.' >&2; exit 1; }
paths=(apple/mime apple/whistlegraph/Sources apple/whistlegraph/Resources/Web system/public/mime system/public/mime-layout.json system/backend/whistlegraph.mjs lith/Caddyfile)
git diff --quiet HEAD -- "${paths[@]}" || { echo 'Commit MIME changes before releasing.' >&2; exit 1; }
[[ -z $(git ls-files --others --exclude-standard -- "${paths[@]}") ]] || { echo 'Commit new MIME files before releasing.' >&2; exit 1; }
revision=$(git rev-parse HEAD)
ac-deploy
node --input-type=module - "$revision" <<'JS'
import {execFileSync} from 'node:child_process';
import assert from 'node:assert/strict';
const sha = process.argv[2];
const origin = 'https://aesthetic.computer';
const response = await fetch(`${origin}/mime-layout.json?release=${sha}`, {headers: {'cache-control': 'no-cache'}});
assert.equal(response.status, 200);
const have = await response.json();
const want = JSON.parse(execFileSync('git', ['show', `${sha}:system/public/mime-layout.json`], {encoding: 'utf8'}));
assert.deepEqual(have, want, 'Production must serve this revision’s layout before iOS ships');
const policy = await fetch(`https://mime.ac/mime/interface.js?release=${sha}`, {headers: {'cache-control': 'no-cache'}});
assert.equal(policy.status, 200);
assert.equal(await policy.text(), execFileSync('git', ['show', `${sha}:system/public/mime/interface.js`], {encoding: 'utf8'}),
  'Production and iOS must share the same interface policy');
const commit = await (await fetch(`${origin}/.commit-ref?release=${sha}`)).text();
assert.equal(commit.trim(), sha, 'Production and app source revisions must match');
console.log(`MIME layout ${have.revision} verified on production`);
JS
if [[ "$mode" != web ]]; then
  # Other seat work can advance the checkout while lith deploys. Build only
  # the revision just verified on production, with its bundled config.
  release_source_dir=$(mktemp -d /tmp/mime-release-source.XXXXXX)
  trap 'rm -rf "$release_source_dir"' EXIT
  git archive "$revision" system/public/mime/interface.js apple/mime apple/whistlegraph apple/aesel/Sources apple/aesel/Resources/ComicRelief-Bold.ttf system/public/mime-layout.json system/public/doom-player system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf system/public/aesthetic.computer/lib/canvas-tape.mjs shared/roblox-room.mjs roblox/rooms/src/server/Room.luau aesel | tar -x -C "$release_source_dir"
  app_dir="$release_source_dir/apple/mime"
fi
if [[ "$mode" == device ]]; then
  bash "$app_dir/run.sh" device
elif [[ "$mode" == testflight ]]; then
  cd "$app_dir"
  fastlane ios build
  fastlane ios upload
fi
