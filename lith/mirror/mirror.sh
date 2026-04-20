#!/usr/bin/env bash
# ac-mirror.sh — bidirectional knot ↔ github mirror for the core repo.
# Runs via systemd timer every 60s. Idempotent, exits fast when in sync.
set -euo pipefail

REPO=/opt/ac-mirror/core
KNOT_KEY=/root/.ssh/knot_push
GH_KEY=/root/.ssh/github_mirror
BRANCH=main

cd "$REPO"

export GIT_SSH_COMMAND="ssh -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new \
  -i $KNOT_KEY -i $GH_KEY"

# "+" prefix → allow non-fast-forward fetches. A mirror must always track
# wherever the remote actually is, even after force-pushes or rewinds.
git fetch --quiet knot   "+$BRANCH":refs/remotes/knot/$BRANCH
git fetch --quiet github "+$BRANCH":refs/remotes/github/$BRANCH

knot_head=$(git rev-parse "knot/$BRANCH")
gh_head=$(git rev-parse "github/$BRANCH")

if [ "$knot_head" = "$gh_head" ]; then
  exit 0
fi

if git merge-base --is-ancestor "$gh_head" "$knot_head"; then
  echo "$(date -Iseconds) → github behind; pushing $knot_head to github."
  git push --quiet github "$knot_head:refs/heads/$BRANCH"
elif git merge-base --is-ancestor "$knot_head" "$gh_head"; then
  echo "$(date -Iseconds) → knot behind; pushing $gh_head to knot."
  git push --quiet knot "$gh_head:refs/heads/$BRANCH"
else
  echo "$(date -Iseconds) ⚠️  divergent heads: knot=$knot_head github=$gh_head — skipping (manual resolution required)" >&2
  exit 2
fi
