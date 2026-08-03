#!/bin/bash
# Give Iris a real, sparse Git source checkout; keep runtime negatives/outbox
# in the separate installed workspace managed by install.sh.
set -euo pipefail

REMOTE="${CAPTUTOR_SOURCE_REMOTE:-https://github.com/whistlegraph/aesthetic-computer.git}"
PUSH_REMOTE="${CAPTUTOR_SOURCE_PUSH_REMOTE:-git@knot.aesthetic.computer:aesthetic.computer/core}"
CHECKOUT="${CAPTUTOR_SOURCE_CHECKOUT:-$HOME/Developer/aesthetic-computer}"
BRANCH="${CAPTUTOR_SOURCE_BRANCH:-iris/fuser-captutor}"
BASE_BRANCH="${CAPTUTOR_SOURCE_BASE_BRANCH:-agent/frame-hitl-action-trails}"
RUNTIME="${CAPTUTOR_HOME:-$HOME/Developer/captutor}"

if [ ! -d "$CHECKOUT/.git" ]; then
  git clone --filter=blob:none --no-checkout "$REMOTE" "$CHECKOUT"
fi

if ! git -C "$CHECKOUT" remote get-url knot >/dev/null 2>&1; then
  git -C "$CHECKOUT" remote add knot "$PUSH_REMOTE"
fi

git -C "$CHECKOUT" sparse-checkout init --cone
git -C "$CHECKOUT" sparse-checkout set captutor pop toolchain/shims
git -C "$CHECKOUT" fetch origin "$BASE_BRANCH" --quiet
if git -C "$CHECKOUT" show-ref --verify --quiet "refs/heads/$BRANCH"; then
  git -C "$CHECKOUT" checkout "$BRANCH"
else
  git -C "$CHECKOUT" checkout -b "$BRANCH" "origin/$BASE_BRANCH"
fi

CAPTUTOR_HOME="$RUNTIME" "$CHECKOUT/captutor/bin/install.sh"

echo "✓ Iris source checkout: $CHECKOUT ($BRANCH)"
echo "✓ Filming runtime: $RUNTIME"
echo "  Edit and commit in $CHECKOUT; rerun captutor/bin/install.sh to deploy."
echo "  Publish reviewed commits to the Tangled remote named knot."
