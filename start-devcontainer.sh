#!/usr/bin/env bash

# Start the Aesthetic Computer devcontainer and open VS Code connected to it.
#
# Prerequisites:
#   npm install -g @devcontainers/cli
#
# This script:
# 1. Builds the devcontainer if needed
# 2. Opens VS Code connected to the devcontainer

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

# Open VS Code connected to the devcontainer (builds if needed)
echo -e "${BLUE}🚀 Building and starting devcontainer...${NC}"
echo -e "${YELLOW}   (This may take a while on first run to build the container)${NC}"

# First bring up the container
devcontainer up --workspace-folder "$SCRIPT_DIR"

echo -e "${GREEN}✅ Devcontainer is up!${NC}"

# Now open VS Code attached to it
echo -e "${BLUE}🚀 Opening VS Code...${NC}"
code --folder-uri "vscode-remote://dev-container+$(printf '%s' "$SCRIPT_DIR" | xxd -p | tr -d '\n')/workspaces/aesthetic-computer"

echo -e "${GREEN}✅ VS Code launched with devcontainer!${NC}"
