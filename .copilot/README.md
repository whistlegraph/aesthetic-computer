# GitHub Copilot Configuration

This directory contains persistent Copilot settings for the aesthetic-computer devcontainer.

## Features
- **Default model**: Claude Opus 4.5
- **Auto-authentication**: Uses GH_TOKEN from `aesthetic-computer-vault/.env`
- **Session persistence**: Maintains login across container restarts

## Setup
Run `./aesthetic-computer-vault/devault.fish` on container start to initialize:
1. Exports GH_TOKEN from vault
2. Authenticates gh CLI
3. Configures VS Code Copilot settings

## Files
- `config.json` — Copilot preferences
- `init-copilot.fish` — Initialization script (run by devault.fish)
