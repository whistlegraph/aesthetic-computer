#!/bin/bash
# Quick test of gh models
echo "Testing gh models..." > /workspaces/aesthetic-computer/ants/test-gh.txt
gh models run openai/gpt-4o-mini "Say hello in 3 words" >> /workspaces/aesthetic-computer/ants/test-gh.txt 2>&1
echo "Exit code: $?" >> /workspaces/aesthetic-computer/ants/test-gh.txt
echo "Done at $(date)" >> /workspaces/aesthetic-computer/ants/test-gh.txt
