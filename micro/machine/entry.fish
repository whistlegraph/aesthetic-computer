#!/usr/bin/env fish

# Send a welcome message!
echo "*** Aesthetic Computer is Initializing... ***"

# Go to the user's directory.
cd /home/me

# Login to Github.
if not gh auth status
  gh auth login --web
end

# Login to Vercel
// ...

# Login to Netlify
// ...

# Apply the 'vault' credentials to the mounted aesthetic-computer volume.
if not test -d /home/me/aesthetic-computer/aesthetic-computer-vault
  git clone https://github.com/whistlegraph/aesthetic-computer-vault
  cd aesthetic-computer/aesthetic-computer-vault
  sudo fish devault.fish
end

# Initialize fnm and use the specified Node.js version.
cd /home/me/aesthetic-computer

# Check if node_modules directory is present and not empty
if not test -d node_modules || not count (ls node_modules) > /dev/null
  echo "node_modules directory is empty or missing, running npm install."
  # Install latest npm version before doing anything.
  npm install -g npm@latest --no-fund --no-audit
  # Install Node.js dependencies
  npm install --no-fund --no-audit --no-save
else
  echo "node_modules directory is present, skipping npm install."
end

cd ..

clear

# Boot straight into emacs,
# And execute the `aesthetic` command after my init.el runs. 
emacs -f aesthetic

# Execute the provided command.
# exec $argv
