#!/usr/bin/env fish

# Send a welcome message!
echo "*** Aesthetic Computer is Initializing... ***"

# Go to the user's directory.
cd /home/me

# Login to Github.
if not gh auth status
  echo "Not logged into GitHub, exiting to shell."
  return
end

# Apply the 'vault' credentials to the mounted aesthetic-computer volume, and make sure it exists.
if test -d /home/me/aesthetic-computer
  if not test -d /home/me/aesthetic-computer/aesthetic-computer-vault
    gh repo clone whistlegraph/aesthetic-computer-vault /home/me/aesthetic-computer/aesthetic-computer-vault
    cd /home/me/aesthetic-computer/aesthetic-computer-vault
    sudo fish devault.fish
  end
else
  echo "aesthetic-computer source code not mounted"
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
# emacs -f aesthetic
fish
# python3 -m http.server 8888

# Execute the provided command.
# exec $argv
