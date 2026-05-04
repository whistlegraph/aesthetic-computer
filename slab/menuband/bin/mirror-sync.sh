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

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

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

# Refuse if the working tree under PREFIX has uncommitted changes — otherwise
# we'd push files to the mirror that aren't yet committed in the monorepo,
# making the mirror tip ahead of (and inconsistent with) the canonical source.
if ! git diff --quiet -- "${PREFIX}" || ! git diff --cached --quiet -- "${PREFIX}"; then
    err "${PREFIX}/ has uncommitted changes — commit them before syncing"
    git status -s -- "${PREFIX}" 1>&2
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
    # Need enough depth to walk back to the previous "Mirror of <hash>" commit
    # for the unlanded-contributor pre-flight check below. Shallow=50 is plenty;
    # snapshots happen often enough that the last marker is always near the tip.
    git clone --depth=50 --branch="${MIRROR_BRANCH}" "${MIRROR_URL}" "${WORK}" >/dev/null 2>&1
else
    say "initialising empty mirror"
    git init -q -b "${MIRROR_BRANCH}" "${WORK}"
    git -C "${WORK}" remote add origin "${MIRROR_URL}"
fi

# Pre-flight: refuse to snapshot if the mirror has contributor commits since
# the last "Mirror of <hash>" point that haven't been landed in the monorepo
# yet. Without this check, the snapshot's tree would silently regress that
# contributor work — exactly what happened with Esteban's PR #2: the mirror
# had his consolidated palette merged, the monorepo didn't yet, and a sync
# from monorepo blew his work out of the tip tree.
#
# Override with SYNC_FORCE=1 if you really mean it (e.g. you've decided to
# revert a contributor change). Don't make this a habit.
LAST_MIRROR_COMMIT="$(git -C "${WORK}" log --format='%H' --grep='^Mirror of [0-9a-f]\{7\}:' -1 || true)"
if [[ -n "${LAST_MIRROR_COMMIT}" ]]; then
    LAST_MIRROR_MONO_HASH="$(git -C "${WORK}" log -1 --format='%s' "${LAST_MIRROR_COMMIT}" \
        | sed -nE 's/^Mirror of ([0-9a-f]+):.*/\1/p')"

    # Cache the monorepo's landed subjects once. We can't pipe directly into
    # `grep -Fxq` per-iteration: with `set -o pipefail`, grep -q exits early
    # on first match, git log catches SIGPIPE → exit 141, and the pipeline
    # reports failure even though the match succeeded. Cache + here-string
    # avoids the pipe entirely.
    LANDED_SUBJECTS="$(git log --format='%s' -- "${PREFIX}")"

    UNLANDED_COUNT=0
    UNLANDED_LINES=""
    while IFS=$'\t' read -r contrib_sha contrib_subj; do
        [[ -z "${contrib_sha}" ]] && continue
        # Skip future "Mirror of …" snapshots — those are our own.
        [[ "${contrib_subj}" == "Mirror of "* ]] && continue
        # Treat as landed (authorship/hash differ because of git am rewrites,
        # but subject is the stable identity).
        if ! grep -Fxq -- "${contrib_subj}" <<< "${LANDED_SUBJECTS}"; then
            UNLANDED_COUNT=$((UNLANDED_COUNT + 1))
            UNLANDED_LINES+="    ${contrib_sha:0:9}  ${contrib_subj}"$'\n'
        fi
    done < <(git -C "${WORK}" log --no-merges --format='%H%x09%s' \
                "${LAST_MIRROR_COMMIT}..HEAD" 2>/dev/null)

    if [[ ${UNLANDED_COUNT} -gt 0 ]]; then
        err "mirror has ${UNLANDED_COUNT} contributor commit(s) not landed in monorepo:"
        printf '%s' "${UNLANDED_LINES}" 1>&2
        echo 1>&2
        echo "  Land them first (preserves authorship, handles overlap):" 1>&2
        echo "    ${SCRIPT_DIR}/mirror-pull.sh" 1>&2
        echo 1>&2
        echo "  Override (DANGEROUS — will regress contributor work in mirror tip):" 1>&2
        echo "    SYNC_FORCE=1 $0" 1>&2
        if [[ "${SYNC_FORCE:-}" != "1" ]]; then
            exit 1
        fi
        warn "SYNC_FORCE=1 set — proceeding despite unlanded commits"
    else
        ok "no unlanded contributor commits on mirror"
    fi
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

# Compare staged tree to current HEAD tree by hash — unambiguous no-op
# detection. Earlier this branch relied on `git diff --cached --quiet` and
# the operator couldn't tell, after the fact, whether a "mirror already
# up-to-date" line meant "diff was empty" or "diff check went sideways
# but we never got around to actually pushing." Tree-hash compare leaves
# no room for that ambiguity, and we print both hashes so a future
# debugger can replay the decision from the log alone.
HEAD_TREE="$(git rev-parse HEAD^{tree} 2>/dev/null || echo "")"
STAGED_TREE="$(git write-tree)"
say "snapshot tree ${STAGED_TREE:0:12} ↔ mirror HEAD tree ${HEAD_TREE:0:12}"

# Stat the staged delta (A/M/D counts) so even the up-to-date branch
# proves the diff was actually computed. `git diff --cached --numstat`
# lists one line per changed file; counting lines is good enough for a
# headline, and we surface a few exemplar paths so a 0/0/0 sync isn't
# silently skipped without showing why.
DELTA_RAW="$(git diff --cached --name-status)"
DELTA_COUNT="$(printf '%s\n' "${DELTA_RAW}" | grep -c . || true)"
ADDED="$(printf '%s\n' "${DELTA_RAW}" | grep -c '^A' || true)"
MODIFIED="$(printf '%s\n' "${DELTA_RAW}" | grep -c '^M' || true)"
DELETED="$(printf '%s\n' "${DELTA_RAW}" | grep -c '^D' || true)"

if [[ "${HEAD_TREE}" == "${STAGED_TREE}" ]]; then
    if [[ "${DELTA_COUNT}" -ne 0 ]]; then
        # Tree-hash equality and a non-empty numstat would mean git is
        # lying to one of the two callers — bail loudly rather than
        # silently exiting "up-to-date" with a real diff sitting in the
        # index. This branch should be unreachable; if it ever fires,
        # the script needs a closer look before trusting another sync.
        err "tree hashes match but ${DELTA_COUNT} staged change(s) reported — refusing to skip"
        printf '%s\n' "${DELTA_RAW}" 1>&2
        exit 1
    fi
    ok "mirror already up-to-date (tip ${MONO_SHORT}, tree ${STAGED_TREE:0:12})"
    exit 0
fi

ok "${DELTA_COUNT} change(s) staged — added=${ADDED} modified=${MODIFIED} deleted=${DELETED}"
# Show up to 6 exemplar paths so the operator can sanity-check that the
# right files are about to land before push. Anything beyond that gets
# summarised — full list is in the resulting commit anyway.
printf '%s\n' "${DELTA_RAW}" | head -6 | sed 's/^/    /'
if [[ "${DELTA_COUNT}" -gt 6 ]]; then
    printf '    … and %d more\n' "$((DELTA_COUNT - 6))"
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

LOCAL_NEW_HEAD="$(git rev-parse HEAD)"
say "pushing ${LOCAL_NEW_HEAD:0:9} to ${MIRROR_URL}"
git push -q origin "${MIRROR_BRANCH}"

# Verify-after-push: re-query the remote tip and confirm it matches the
# commit we just made. Catches the "push silently no-op'd" failure mode
# (auth fallback to a stale credential, network hiccup that exited 0 on
# a retry, etc.) where the script otherwise would happily print
# "mirror updated" without the mirror actually advancing.
REMOTE_NEW_HEAD="$(git ls-remote "${MIRROR_URL}" "${MIRROR_BRANCH}" 2>/dev/null | awk '{print $1}')"
if [[ "${REMOTE_NEW_HEAD}" != "${LOCAL_NEW_HEAD}" ]]; then
    err "push appeared to succeed but mirror tip is ${REMOTE_NEW_HEAD:0:9}, not ${LOCAL_NEW_HEAD:0:9}"
    exit 1
fi
ok "mirror updated → github.com/whistlegraph/menuband (${LOCAL_NEW_HEAD:0:9}, mono ${MONO_SHORT})"
