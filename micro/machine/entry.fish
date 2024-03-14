#!/usr/bin/env fish

# Send a welcome message!
echo "*** Aesthetic Computer is Initializing... ***"

# Go to the user's directory.
cd /home/me

# Login to Github.
if not gh auth status
  gh auth login --web
end

# TODO: Apply the 'vault' credentials to the mounted aesthetic-computer volume.
if not test -d /home/me/aesthetic-computer/aesthetic-computer-vault
  git clone https://github.com/whistlegraph/aesthetic-computer-vault
  cd aesthetic-computer/aesthetic-computer-vault
  sudo fish devault.fish
end

# Initialize fnm and use the specified Node.js version.
cd /home/me/aesthetic-computer

# Install latest npm version before doing anything.
npm install -g npm@latest --no-fund --no-audit

# Install npm packages for eglot emacs language support.
npm install -g prettier typescript-language-server

# Install Node.js dependencies
npm install --no-fund --no-audit

cd ..

clear

# Execute the provided command.
exec $argv
