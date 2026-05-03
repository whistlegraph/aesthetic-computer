#!/usr/bin/env bash
# mirror-pull.sh — Pull contributor commits from the mirror back into the monorepo.
#
# After someone lands a PR on github.com/whistlegraph/menuband, run this from
# anywhere inside the monorepo to apply their commits onto slab/menuband/,
# preserving their authorship. mirror-sync.sh will then snapshot back to the
# mirror cleanly without regressing their work.
#
# How it works:
#   1. Fetches the mirror remote (auto-adds it via HTTPS if missing).
#   2. Finds the last "Mirror of <hash>" snapshot commit on the mirror — that
#      marks the previous sync point.
#   3. Walks every non-merge, non-mirror commit since then and skips any whose
#      subject already appears in monorepo's slab/menuband history (treats
#      those as already landed — git am rewrites hashes, so subject is the
#      stable identity).
#   4. For each unlanded commit, runs `git format-patch -1` then
#      `git am --3way --directory=slab/menuband/`. The 3-way merge cleanly
#      handles the case where a contributor has manually merged main into
#      their branch (their commit may "re-add" files that the monorepo
#      already has — 3way reconciles instead of failing on "file already
#      exists in index").
#   5. On conflict, stops and prints how to resume.

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
MIRROR_REMOTE="menuband-mirror"
MIRROR_URL="https://github.com/whistlegraph/menuband.git"
MIRROR_BRANCH="main"

if [[ ! -d "${PREFIX}" ]]; then
    err "${PREFIX}/ not found in ${REPO_ROOT}"
    exit 1
fi

# Reject if the working tree has any modifications under PREFIX — git am
# would refuse anyway, and an unrelated dirty file could mask a real conflict.
if ! git diff --quiet -- "${PREFIX}" || ! git diff --cached --quiet -- "${PREFIX}"; then
    err "${PREFIX}/ has uncommitted changes — commit or stash first"
    git status -s -- "${PREFIX}" 1>&2
    exit 1
fi

# Add or refresh the remote (HTTPS so gh credentials work, mirroring sync.sh).
if ! git remote get-url "${MIRROR_REMOTE}" >/dev/null 2>&1; then
    say "adding remote ${MIRROR_REMOTE} → ${MIRROR_URL}"
    git remote add "${MIRROR_REMOTE}" "${MIRROR_URL}"
fi

say "fetching ${MIRROR_REMOTE}"
git fetch --quiet "${MIRROR_REMOTE}"

MIRROR_REF="${MIRROR_REMOTE}/${MIRROR_BRANCH}"

# Find the last "Mirror of <hash>" snapshot on the mirror — that's our sync point.
LAST_MIRROR_COMMIT="$(git log --format='%H' --grep='^Mirror of [0-9a-f]\{7\}:' -1 "${MIRROR_REF}" || true)"
if [[ -z "${LAST_MIRROR_COMMIT}" ]]; then
    err "no 'Mirror of <hash>' commit found on ${MIRROR_REF} — refusing to guess sync point"
    exit 1
fi
say "last sync point: $(git log -1 --format='%h %s' "${LAST_MIRROR_COMMIT}")"

# Cache the monorepo's landed subjects once (avoid per-iteration pipe — see
# mirror-sync.sh for the pipefail+grep -q SIGPIPE trap that bit us).
LANDED_SUBJECTS="$(git log --format='%s' -- "${PREFIX}")"

# Collect contributor commits in chronological order. Skip merges (format-patch
# can't represent them anyway) and our own snapshot commits.
CONTRIB_SHAS=()
while IFS=$'\t' read -r contrib_sha contrib_subj; do
    [[ -z "${contrib_sha}" ]] && continue
    [[ "${contrib_subj}" == "Mirror of "* ]] && continue
    # Skip if subject already exists in monorepo's slab/menuband history.
    if grep -Fxq -- "${contrib_subj}" <<< "${LANDED_SUBJECTS}"; then
        continue
    fi
    CONTRIB_SHAS+=("${contrib_sha}")
done < <(git log --no-merges --reverse --format='%H%x09%s' \
            "${LAST_MIRROR_COMMIT}..${MIRROR_REF}" 2>/dev/null)

if [[ ${#CONTRIB_SHAS[@]} -eq 0 ]]; then
    ok "monorepo is already up-to-date with mirror contributors"
    exit 0
fi

say "${#CONTRIB_SHAS[@]} contributor commit(s) to land:"
for sha in "${CONTRIB_SHAS[@]}"; do
    printf '    %s\n' "$(git log -1 --format='%h %an: %s' "${sha}")"
done

# Stage the patches under one numbered series so `git am` consumes them in
# order. Using --start-number to avoid filename collisions when the same
# default name (0001-…) repeats.
PATCH_DIR="$(mktemp -d)"
trap "rm -rf ${PATCH_DIR}" EXIT
i=1
for sha in "${CONTRIB_SHAS[@]}"; do
    git format-patch --quiet -o "${PATCH_DIR}" --start-number=$i -1 "${sha}" >/dev/null
    i=$((i + 1))
done

say "applying via git am --3way --directory=${PREFIX}/"
# Glob expansion gives us patches in numeric (sorted) order.
shopt -s nullglob
PATCHES=( "${PATCH_DIR}"/*.patch )
shopt -u nullglob

if ! git am --3way --directory="${PREFIX}/" "${PATCHES[@]}"; then
    err "git am stopped on conflict"
    echo "  resolve the conflict, then run:" 1>&2
    echo "    git am --3way --continue" 1>&2
    echo "  to skip this patch:" 1>&2
    echo "    git am --skip" 1>&2
    echo "  to abort the entire pull:" 1>&2
    echo "    git am --abort" 1>&2
    exit 1
fi

ok "landed ${#CONTRIB_SHAS[@]} commit(s) onto ${PREFIX}/"
echo
echo "  Next: re-snapshot to mirror so contributor branches see their work as base:"
echo "    ${REPO_ROOT}/${PREFIX}/bin/mirror-sync.sh"
