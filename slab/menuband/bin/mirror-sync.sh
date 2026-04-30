#!/usr/bin/env bash
# mirror-sync.sh — Snapshot-mirror slab/menuband/ to its standalone
# GitHub repo at github.com/whistlegraph/menuband.
#
# Why snapshot, not subtree split:
#   `git subtree split` walks every commit in the entire monorepo
#   history (50k+ commits, 1.3 GB pack) to figure out which ones
#   touched the prefix — takes 10+ minutes per sync. The mirror
#   doesn't actually need that history; contributors care about
#   "what's the current source code, and how do I PR a change."
#   So this script just snapshots the current tree as a single new
#   commit titled "Mirror of <mono-hash> — <mono-subject>". The
#   mirror's git log becomes the release log instead of a granular
#   per-file history. Authorship is preserved from the upstream
#   commit so contributors who land changes in the mono get the
#   commit credit on the mirror too.
#
# Idempotent. Safe to call repeatedly: if the snapshot tree matches
# the mirror's current tip, no commit is created.
#
# Auth: uses HTTPS via the `gh` CLI's credential helper. Run once:
#   `gh auth setup-git` to install the helper if not already done.

set -euo pipefail

CYAN=$'\033[1;36m'
GREEN=$'\033[1;32m'
YELLOW=$'\033[1;33m'
RED=$'\033[1;31m'
RESET=$'\033[0m'
say()  { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()   { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
warn() { printf "%s! %s%s\n" "$YELLOW" "$1" "$RESET"; }
err()  { printf "%s✗ %s%s\n" "$RED" "$1" "$RESET" 1>&2; }

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [[ -z "${REPO_ROOT}" ]]; then
    err "not inside a git repository"
    exit 1
fi
cd "${REPO_ROOT}"

PREFIX="slab/menuband"
MIRROR_URL="https://github.com/whistlegraph/menuband.git"
MIRROR_BRANCH="main"

if [[ ! -d "${PREFIX}" ]]; then
    err "${PREFIX}/ not found in ${REPO_ROOT}"
    exit 1
fi

# Identify the most recent mono commit that touched the prefix —
# its hash + subject become the mirror's snapshot label, and its
# author lands on the mirror commit so contributors see their
# attribution preserved.
MONO_HASH="$(git log -1 --format=%H -- "${PREFIX}")"
MONO_SHORT="${MONO_HASH:0:7}"
MONO_SUBJECT="$(git log -1 --format=%s -- "${PREFIX}")"
MONO_AUTHOR="$(git log -1 --format='%aN <%aE>' -- "${PREFIX}")"
MONO_DATE="$(git log -1 --format='%aI' -- "${PREFIX}")"

# Working dir: clone the mirror if it has any commits, otherwise
# init from scratch and set the remote up. Using HTTPS so gh's
# credential helper can supply the token transparently.
WORK="$(mktemp -d)"
trap "rm -rf ${WORK}" EXIT

if git ls-remote --heads "${MIRROR_URL}" "${MIRROR_BRANCH}" 2>/dev/null | grep -q "refs/heads/${MIRROR_BRANCH}"; then
    say "cloning mirror"
    git clone --depth=1 --branch="${MIRROR_BRANCH}" "${MIRROR_URL}" "${WORK}" >/dev/null 2>&1
else
    say "initialising empty mirror"
    git init -q -b "${MIRROR_BRANCH}" "${WORK}"
    git -C "${WORK}" remote add origin "${MIRROR_URL}"
fi

# Replace the mirror's working tree with the current snapshot of
# slab/menuband/, preserving the .git directory. `find -delete` in
# two passes (files first, then empty dirs) handles deeply nested
# layouts without rmdir errors.
say "snapshotting ${PREFIX} → mirror tree"
find "${WORK}" -mindepth 1 -maxdepth 1 ! -name '.git' -exec rm -rf {} +
# `cp -R prefix/.` copies *contents* of prefix into WORK without
# nesting prefix as a subdir. The trailing `/.` is critical.
cp -R "${REPO_ROOT}/${PREFIX}/." "${WORK}/"

# Strip build artifacts, vendor caches, and signed/notarized
# release DMGs that have no business in source. The mirror is
# code only; release binaries live on assets.aesthetic.computer
# and the GitHub Releases page.
rm -rf "${WORK}/.build"
rm -f "${WORK}"/Menu-Band-*.dmg "${WORK}"/Menu-Band-*.zip
find "${WORK}" -name '.DS_Store' -delete

# Drop in / refresh the README so contributors see the mirror
# notice and contribution flow. The README lives in the mono so
# it's editable from the canonical side.
README_SRC="${REPO_ROOT}/${PREFIX}/MIRROR_README.md"
if [[ -f "${README_SRC}" ]]; then
    cp "${README_SRC}" "${WORK}/README.md"
fi

cd "${WORK}"
git add -A

# No-op if the snapshot tree matches the previous commit's tree.
if git diff --cached --quiet; then
    ok "mirror already up-to-date (${MONO_SHORT})"
    exit 0
fi

# Mirror commit message: human-readable header naming the upstream
# hash, plus the body of the upstream commit subject. Preserves
# upstream author so contributors who land changes via PR see their
# attribution carry over to the mirror.
git -c "user.name=${MONO_AUTHOR%% <*}" \
    -c "user.email=$(printf '%s' "${MONO_AUTHOR}" | sed -nE 's/.*<([^>]+)>.*/\1/p')" \
    commit -q \
        --author="${MONO_AUTHOR}" \
        --date="${MONO_DATE}" \
        -m "Mirror of ${MONO_SHORT}: ${MONO_SUBJECT}" \
        -m "Snapshot of slab/menuband/ from aesthetic.computer/core @ ${MONO_HASH}." >/dev/null

say "pushing to ${MIRROR_URL}"
git push -q origin "${MIRROR_BRANCH}"
ok "mirror updated → github.com/whistlegraph/menuband (${MONO_SHORT})"
