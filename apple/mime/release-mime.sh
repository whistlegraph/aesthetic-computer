#!/usr/bin/env bash
# One revision: website/config first, then the native MIME reader.
set -euo pipefail
app_dir=$(cd "$(dirname "$0")" && pwd)
repo_dir=$(cd "$app_dir/../.." && pwd)
cd "$repo_dir"
mode=${1:-device}
case "$mode" in device|testflight|web) ;; *) echo 'Usage: release-mime.sh device|testflight|web' >&2; exit 1 ;; esac
[[ $(git branch --show-current) == main ]] || { echo 'Release MIME from main.' >&2; exit 1; }
paths=(apple/mime system/public/mime system/public/mime-layout.json system/backend/whistlegraph.mjs lith/Caddyfile)
git diff --quiet HEAD -- "${paths[@]}" || { echo 'Commit MIME changes before releasing.' >&2; exit 1; }
[[ -z $(git ls-files --others --exclude-standard -- "${paths[@]}") ]] || { echo 'Commit new MIME files before releasing.' >&2; exit 1; }
revision=$(git rev-parse HEAD)
ac-deploy
node --input-type=module - "$revision" <<'JS'
import {readFile} from 'node:fs/promises';
import assert from 'node:assert/strict';
const sha = process.argv[2];
const origin = 'https://aesthetic.computer';
const response = await fetch(`${origin}/mime-layout.json?release=${sha}`, {headers: {'cache-control': 'no-cache'}});
assert.equal(response.status, 200);
const have = await response.json();
const want = JSON.parse(await readFile('system/public/mime-layout.json', 'utf8'));
assert.deepEqual(have, want, 'Production must serve this revision’s layout before iOS ships');
const commit = await (await fetch(`${origin}/.commit-ref?release=${sha}`)).text();
assert.equal(commit.trim(), sha, 'Production and app source revisions must match');
console.log(`MIME layout ${have.revision} verified on production`);
JS
[[ $(git rev-parse HEAD) == "$revision" ]] || { echo 'HEAD moved during deployment; release again from the new revision.' >&2; exit 1; }
if [[ "$mode" == device ]]; then
  bash "$app_dir/run.sh" device
elif [[ "$mode" == testflight ]]; then
  cd "$app_dir"
  fastlane ios build
  fastlane ios upload
fi
