#!/usr/bin/env fish

# Send a welcome message!
echo "*** Aesthetic Computer is Initializing... ***"

# Go to the user's directory.
cd /home/me

# Login to Github.
gh auth login --with-token

# TODO: Apply the 'vault' credentials to the mounted aesthetic-computer volume.
git clone https://github.com/whistlegraph/aesthetic-computer-vault

# Initialize fnm and use the specified Node.js version.
cd /home/me/aesthetic-computer

# Install latest npm version before doing anything.
npm install -g npm@latest --no-fund --no-audit

# Install npm packages for emacs support.
npm install -g prettier typescript-language-server javascript-typescript-langserver

# Install Node.js dependencies
npm install --no-fund --no-audit

cd ..

clear

# Execute the provided command.
exec $argv
