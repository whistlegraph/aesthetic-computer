#!/usr/bin/env bash
# Unify Aesthetic Computer repo history
#
# Grafts the four successive repositories into one continuous git history:
#   1. whistlegraph/system-ac       (Aug-Dec 2021, 38 commits)
#   2. whistlegraph/disks-ac        (Oct-Dec 2021, companion)
#   3. whistlegraph/2022.aesthetic.computer (Dec 2021-Dec 2022, ~500 commits)
#   4. whistlegraph/aesthetic-computer      (Dec 2022-present, 11k+ commits)
#
# After grafting, the unified history is pushed to the Tangled knot.
#
# Usage:
#   ./unify-repo-history.sh [--dry-run] [--work-dir /path/to/workdir]
#
# Requirements:
#   - git-filter-repo (pip install git-filter-repo)
#   - Access to all four GitHub repos

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

info()    { echo -e "${BLUE}i${NC} $1"; }
success() { echo -e "${GREEN}✓${NC} $1"; }
warning() { echo -e "${YELLOW}!${NC} $1"; }
error()   { echo -e "${RED}x${NC} $1"; exit 1; }

# Defaults
DRY_RUN=false
WORK_DIR="/tmp/ac-unified-history"
KNOT_REMOTE="git@knot.aesthetic.computer"

# The four repos in chronological order
REPO_1="https://github.com/whistlegraph/system-ac.git"
REPO_2="https://github.com/whistlegraph/disks-ac.git"
REPO_3="https://github.com/whistlegraph/2022.aesthetic.computer.git"
REPO_4="https://github.com/whistlegraph/aesthetic-computer.git"

# Parse args
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run) DRY_RUN=true; shift ;;
        --work-dir) WORK_DIR="$2"; shift 2 ;;
        --knot-remote) KNOT_REMOTE="$2"; shift 2 ;;
        *) error "Unknown option: $1" ;;
    esac
done

check_requirements() {
    if ! command -v git-filter-repo &>/dev/null; then
        error "git-filter-repo not found. Install: pip install git-filter-repo"
    fi
    info "Requirements OK"
}

clone_repos() {
    mkdir -p "$WORK_DIR"
    cd "$WORK_DIR"

    info "Cloning all four repositories..."

    if [ ! -d "system-ac" ]; then
        git clone "$REPO_1" system-ac
        success "Cloned system-ac"
    else
        warning "system-ac already cloned"
    fi

    if [ ! -d "disks-ac" ]; then
        git clone "$REPO_2" disks-ac
        success "Cloned disks-ac"
    else
        warning "disks-ac already cloned"
    fi

    if [ ! -d "2022.aesthetic.computer" ]; then
        git clone "$REPO_3" 2022.aesthetic.computer
        success "Cloned 2022.aesthetic.computer"
    else
        warning "2022.aesthetic.computer already cloned"
    fi

    if [ ! -d "aesthetic-computer" ]; then
        git clone --mirror "$REPO_4" aesthetic-computer.git
        # Also make a working copy for grafting
        git clone aesthetic-computer.git aesthetic-computer
        success "Cloned aesthetic-computer"
    else
        warning "aesthetic-computer already cloned"
    fi
}

find_boundary_commits() {
    info "Finding boundary commits..."
    echo ""

    # Last commit of each predecessor repo
    cd "$WORK_DIR/system-ac"
    SYSTEM_AC_LAST=$(git log --format="%H" -1)
    SYSTEM_AC_LAST_DATE=$(git log --format="%ci" -1)
    info "system-ac last: $SYSTEM_AC_LAST ($SYSTEM_AC_LAST_DATE)"

    cd "$WORK_DIR/disks-ac"
    DISKS_AC_LAST=$(git log --format="%H" -1)
    DISKS_AC_LAST_DATE=$(git log --format="%ci" -1)
    info "disks-ac last: $DISKS_AC_LAST ($DISKS_AC_LAST_DATE)"

    cd "$WORK_DIR/2022.aesthetic.computer"
    REPO_2022_LAST=$(git log --format="%H" -1)
    REPO_2022_LAST_DATE=$(git log --format="%ci" -1)
    info "2022.aesthetic.computer last: $REPO_2022_LAST ($REPO_2022_LAST_DATE)"

    # First (root) commit of each successor repo
    cd "$WORK_DIR/2022.aesthetic.computer"
    REPO_2022_FIRST=$(git rev-list --max-parents=0 HEAD | tail -1)
    REPO_2022_FIRST_DATE=$(git log --format="%ci" "$REPO_2022_FIRST" -1)
    info "2022.aesthetic.computer first: $REPO_2022_FIRST ($REPO_2022_FIRST_DATE)"

    cd "$WORK_DIR/aesthetic-computer"
    CURRENT_FIRST=$(git rev-list --max-parents=0 HEAD | tail -1)
    CURRENT_FIRST_DATE=$(git log --format="%ci" "$CURRENT_FIRST" -1)
    info "aesthetic-computer first: $CURRENT_FIRST ($CURRENT_FIRST_DATE)"

    echo ""
    info "Graft plan:"
    echo "  system-ac ($SYSTEM_AC_LAST_DATE)"
    echo "    + disks-ac ($DISKS_AC_LAST_DATE)"
    echo "      -> 2022.aesthetic.computer root ($REPO_2022_FIRST_DATE)"
    echo "           -> aesthetic-computer root ($CURRENT_FIRST_DATE)"
    echo ""
}

build_unified_repo() {
    info "Building unified repository..."

    cd "$WORK_DIR/aesthetic-computer"

    # Add predecessor repos as remotes
    git remote add system-ac "$WORK_DIR/system-ac" 2>/dev/null || true
    git remote add disks-ac "$WORK_DIR/disks-ac" 2>/dev/null || true
    git remote add repo-2022 "$WORK_DIR/2022.aesthetic.computer" 2>/dev/null || true

    git fetch system-ac
    git fetch disks-ac
    git fetch repo-2022

    success "Fetched all predecessor histories"

    # Graft: 2022 repo's root commit gets system-ac and disks-ac as parents
    info "Grafting 2022.aesthetic.computer onto system-ac + disks-ac..."
    git replace --graft "$REPO_2022_FIRST" "$SYSTEM_AC_LAST" "$DISKS_AC_LAST"

    # Graft: current repo's root commit gets 2022 repo's last commit as parent
    info "Grafting aesthetic-computer onto 2022.aesthetic.computer..."
    git replace --graft "$CURRENT_FIRST" "$REPO_2022_LAST"

    success "Grafts applied"

    # Verify the chain
    info "Verifying unified history..."
    TOTAL_COMMITS=$(git log --oneline | wc -l)
    EARLIEST=$(git log --format="%ci %s" --reverse | head -1)
    LATEST=$(git log --format="%ci %s" -1)
    echo "  Total commits: $TOTAL_COMMITS"
    echo "  Earliest: $EARLIEST"
    echo "  Latest: $LATEST"
    echo ""

    if [ "$DRY_RUN" = true ]; then
        warning "Dry run — grafts are temporary (git replace refs only)"
        warning "Run without --dry-run to make permanent with filter-repo"
        return
    fi

    # Make grafts permanent
    info "Making grafts permanent with filter-repo..."
    git filter-repo --force

    success "Unified history is permanent"

    # Clean up remotes
    git remote remove system-ac 2>/dev/null || true
    git remote remove disks-ac 2>/dev/null || true
    git remote remove repo-2022 2>/dev/null || true
}

push_to_knot() {
    if [ "$DRY_RUN" = true ]; then
        info "[dry-run] Would push to $KNOT_REMOTE:aesthetic.computer/core"
        return
    fi

    cd "$WORK_DIR/aesthetic-computer"

    info "Adding Tangled knot remote..."
    git remote add tangled "$KNOT_REMOTE:aesthetic.computer/core" 2>/dev/null || \
        git remote set-url tangled "$KNOT_REMOTE:aesthetic.computer/core"

    info "Pushing unified history to knot..."
    git push tangled --all --force
    git push tangled --tags

    success "Pushed to $KNOT_REMOTE"
}

main() {
    echo ""
    echo "╔════════════════════════════════════════════════════════╗"
    echo "║   Aesthetic Computer — Unify Repository History       ║"
    echo "╚════════════════════════════════════════════════════════╝"
    echo ""

    if [ "$DRY_RUN" = true ]; then
        warning "DRY RUN MODE — no permanent changes"
        echo ""
    fi

    info "Work directory: $WORK_DIR"
    echo ""

    check_requirements
    clone_repos
    find_boundary_commits
    build_unified_repo
    push_to_knot

    echo ""
    success "Done!"
    echo ""
    info "Unified history at: $WORK_DIR/aesthetic-computer"
    echo ""
    echo "To use in your working copy:"
    echo "  cd /workspaces/aesthetic-computer"
    echo "  git remote add tangled $KNOT_REMOTE:aesthetic.computer/core"
    echo "  git fetch tangled"
    echo "  git push tangled main"
    echo ""
}

main
