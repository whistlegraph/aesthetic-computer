#!/usr/bin/env fish

set -gx TERM xterm-256color

# Send a welcome message!
echo "*** Aesthetic Computer is Initializing... ***"

# Go to the user's directory.
cd /home/me

# Login to Github.
if not gh auth status
  echo "Not logged into GitHub, exiting to shell."
  return
end

if test -n "$GIT_USER_EMAIL"
    git config --global user.email $GIT_USER_EMAIL
end

if test -n "$GIT_USER_NAME"
    git config --global user.name $GIT_USER_NAME
end

# Add aesthetic-computer as the "safe" directory.
git config --global --add safe.directory /home/me/aesthetic-computer

# Make sure git is setup and authorized for making commits via `gh`.
gh auth setup-git

# Apply the 'vault' credentials to the mounted aesthetic-computer volume, and make sure it exists.
if test -d /home/me/aesthetic-computer
  if not test -d /home/me/aesthetic-computer/aesthetic-computer-vault
    gh repo clone whistlegraph/aesthetic-computer-vault /home/me/aesthetic-computer/aesthetic-computer-vault
    cd /home/me/aesthetic-computer/aesthetic-computer-vault
    sudo fish devault.fish
  else
    echo "Vault mounted :)"
  end
else
  echo "Vault unmounted :("
end

# generate ssl certificates (if they don't already exist)
cd /home/me/aesthetic-computer/ssl-dev
[ ! -f localhost.pem ] && [ ! -f localhost-key.pem ] && sudo fish fedora-install.fish

# Initialize fnm and use the specified Node.js version.
cd /home/me/aesthetic-computer

# Check if node_modules directory is present and not empty
if not test -d node_modules || not count (ls node_modules) > /dev/null
  echo "node_modules directory is empty or missing, running npm install."
  # Install latest npm version before doing anything.
  npm install -g npm@latest --no-fund --no-audit
  # Install Node.js dependencies
  npm install --no-fund --no-audit
  npm run install:everything-else
else
  echo "node_modules directory is present, skipping npm install."
end

fish
