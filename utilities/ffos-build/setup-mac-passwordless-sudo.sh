#!/usr/bin/env bash
# Setup passwordless sudo for Docker on Mac
# Run this on your Mac terminal

set -euo pipefail

echo "🔐 Setting up passwordless sudo for Docker..."
echo ""

# Get current user
USER=$(whoami)

# Find docker path
DOCKER_PATH=$(which docker)

if [ -z "$DOCKER_PATH" ]; then
  echo "❌ Docker not found in PATH"
  exit 1
fi

echo "User: $USER"
echo "Docker: $DOCKER_PATH"
echo ""

# Create sudoers rule
SUDOERS_RULE="$USER ALL=(ALL) NOPASSWD: $DOCKER_PATH"

echo "Creating sudoers rule:"
echo "  $SUDOERS_RULE"
echo ""

# Write to sudoers.d
echo "$SUDOERS_RULE" | sudo tee /etc/sudoers.d/docker-nopasswd > /dev/null

# Set permissions
sudo chmod 0440 /etc/sudoers.d/docker-nopasswd

# Verify
echo "✅ Sudoers rule created!"
echo ""
echo "Verifying..."
sudo -l | grep -i docker || echo "⚠️  Rule not showing in sudo -l yet, but should work"
echo ""
echo "✅ Done! You can now run 'sudo docker' without a password"
