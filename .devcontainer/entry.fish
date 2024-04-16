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

if test -d /workspaces/aesthetic-computer
    echo "Symlinking /workspaces/aesthetic-computer to ~"
    ln -s /workspaces/aesthetic-computer ~
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

if not test -d /home/me/aesthetic-computer/aesthetic-computer-code
    gh repo clone whistlegraph/aesthetic-computer-code /home/me/aesthetic-computer/aesthetic-computer-code
end

# generate ssl certificates (if they don't already exist)
cd /home/me/aesthetic-computer/ssl-dev

# Check if the files exist in ssl-dev directory
if not test -f localhost.pem; or not test -f localhost-key.pem
    # Generate SSL certificates
    sudo fish fedora-install.fish
end

# Check if the nanos/ssl directory exists, create it if not
if not test -d /home/me/aesthetic-computer/nanos/ssl
    mkdir -p /home/me/aesthetic-computer/nanos/ssl
end

# Check if the files exist in nanos/ssl directory, copy if not
if not test -f /home/me/aesthetic-computer/nanos/ssl/localhost.pem; or not test -f /home/me/aesthetic-computer/nanos/ssl/localhost-key.pem
    cp localhost.pem /home/me/aesthetic-computer/nanos/ssl/
    cp localhost-key.pem /home/me/aesthetic-computer/nanos/ssl/
end

# Function to install and trust certificates
function install_and_trust_certificate
    set certificate_path $argv[1]
    set certificate_key_path $argv[2]
    set certificate_dir /etc/pki/ca-trust/source/anchors/

    # Copy the certificate to the system trust store
    sudo cp $certificate_path $certificate_dir
    sudo cp $certificate_key_path $certificate_dir

    # Update the trust store
    sudo update-ca-trust
end

# Install and trust certificates if they are in ssl-dev directory
if test -f /home/me/aesthetic-computer/ssl-dev/localhost.pem; and test -f /home/me/aesthetic-computer/ssl-dev/localhost-key.pem
    install_and_trust_certificate /home/me/aesthetic-computer/ssl-dev/localhost.pem /home/me/aesthetic-computer/ssl-dev/localhost-key.pem
end

# Initialize fnm and use the specified Node.js version.
cd /home/me/aesthetic-computer

# Check if node_modules directory is present and not empty
if not test -d node_modules || not count (ls node_modules) >/dev/null
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
