#!/usr/bin/env fish

# Initialize fnm and use the specified Node.js version.
cd ~/micro/system

# Install latest npm version before doing anything.
npm install -g npm@latest --no-fund --no-audit --silent

# Install Node.js dependencies
npm install --no-fund --no-audit --silent

cd ..

clear

# Execute the provided command.
exec $argv
