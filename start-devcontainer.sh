#!/usr/bin/env bash

# Start the Aesthetic Computer devcontainer using the devcontainer CLI
# instead of relying on VSCode's devcontainer mode.
#
# Prerequisites:
#   npm install -g @devcontainers/cli
#
# This script:
# 1. Brings up the devcontainer with `devcontainer up`
# 2. Executes into it with `devcontainer exec`
# 3. Runs entry.fish or artery-tui/emacs as needed

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

echo -e "${MAGENTA}"
echo "╔══════════════════════════════════════════════════════════════╗"
echo "║      🎨 Aesthetic Computer - Devcontainer Mode               ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo -e "${NC}"

# Check if devcontainer CLI is installed
if ! command -v devcontainer &> /dev/null; then
    echo -e "${YELLOW}⚠️  devcontainer CLI not found. Installing...${NC}"
    npm install -g @devcontainers/cli
fi

echo -e "${BLUE}📦 Checking devcontainer CLI version...${NC}"
devcontainer --version

# Check if Docker is running
if ! docker info &> /dev/null; then
    echo -e "${YELLOW}⚠️  Docker doesn't seem to be running. Please start Docker Desktop.${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Docker is running${NC}"

# Build and start the devcontainer
echo -e "${BLUE}🔨 Building and starting devcontainer...${NC}"
echo -e "${YELLOW}   (This may take a while on first run)${NC}"

# Use devcontainer up to build and start
# --workspace-folder specifies where devcontainer.json lives
devcontainer up --workspace-folder "$SCRIPT_DIR"

echo -e "${GREEN}✅ Devcontainer is up!${NC}"

# Now exec into the container and run entry.fish or a shell
echo -e "${BLUE}🚀 Connecting to devcontainer...${NC}"

# Check what mode to run in
case "${1:-shell}" in
    entry)
        echo -e "${MAGENTA}Running entry.fish (full platform startup)...${NC}"
        devcontainer exec --workspace-folder "$SCRIPT_DIR" /workspaces/aesthetic-computer/.devcontainer/entry.fish
        ;;
    artery)
        echo -e "${MAGENTA}Starting artery-tui...${NC}"
        devcontainer exec --workspace-folder "$SCRIPT_DIR" fish -c "cd /workspaces/aesthetic-computer && node artery/artery-tui.mjs"
        ;;
    emacs)
        echo -e "${MAGENTA}Starting emacsclient...${NC}"
        devcontainer exec --workspace-folder "$SCRIPT_DIR" fish -c "emacsclient -c"
        ;;
    fish|shell)
        echo -e "${MAGENTA}Starting fish shell...${NC}"
        devcontainer exec --workspace-folder "$SCRIPT_DIR" fish
        ;;
    *)
        echo -e "${YELLOW}Usage: $0 [entry|artery|emacs|fish|shell]${NC}"
        echo "  entry  - Run full entry.fish platform startup"
        echo "  artery - Start artery-tui"
        echo "  emacs  - Start emacsclient"
        echo "  fish   - Start fish shell (default)"
        exit 1
        ;;
esac
