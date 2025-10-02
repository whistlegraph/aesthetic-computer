#!/usr/bin/env fish

# Function to ensure correct Docker socket permissions
function ensure_docker_socket_permissions
    set -l DOCKER_SOCKET /var/run/docker.sock
    if test -S $DOCKER_SOCKET
        set -l DOCKER_GID (stat -c '%g' $DOCKER_SOCKET)
        set -l CURRENT_DOCKER_GROUP_INFO (getent group $DOCKER_GID)
        set -l TARGET_GROUP ""

        if test -z "$CURRENT_DOCKER_GROUP_INFO" # No group with this GID exists
            if getent group docker > /dev/null
                echo "Modifying existing docker group to GID $DOCKER_GID..."
                sudo groupmod -g $DOCKER_GID docker
            else
                echo "Creating new docker group with GID $DOCKER_GID..."
                sudo groupadd -g $DOCKER_GID docker
            end
            set TARGET_GROUP docker
        else # A group with this GID already exists, check if it's named 'docker'
            set -l GROUP_NAME (echo $CURRENT_DOCKER_GROUP_INFO | cut -d: -f1)
            if test "$GROUP_NAME" != "docker"
                if test $DOCKER_GID -eq 0
                    echo "⚠️  Docker socket belongs to GID 0 (root). Applying ACL for user access instead of modifying groups."
                    sudo setfacl -m u:me:rw $DOCKER_SOCKET 2>/dev/null
                    or begin
                        echo "⚠️  setfacl unavailable; falling back to permissive chmod on docker socket."
                        sudo chmod 666 $DOCKER_SOCKET
                    end
                    set TARGET_GROUP "" # Access handled via ACL/chmod
                else
                    echo "ℹ️  Docker socket group is '$GROUP_NAME'; using existing group."
                    set TARGET_GROUP $GROUP_NAME
                end
            else
                echo "Docker group '$GROUP_NAME' with GID $DOCKER_GID already exists."
                set TARGET_GROUP docker
            end
        end

        if test -n "$TARGET_GROUP"
            set -l USER_GROUPS (id -Gn me | string split ' ')
            if not contains $TARGET_GROUP $USER_GROUPS
                echo "Adding user 'me' to $TARGET_GROUP group..."
                sudo usermod -aG $TARGET_GROUP me
            else
                echo "User 'me' is already in $TARGET_GROUP group."
            end
        else
            echo "ℹ️  Skipping group membership update; permissions handled directly on socket."
        end
    else
        echo "Docker socket $DOCKER_SOCKET not found."
    end
end

# Call the function to set up Docker socket permissions
ensure_docker_socket_permissions

# Start Docker daemon in the background if not already running
if not pgrep -x dockerd >/dev/null
    echo "Starting Docker daemon..."
    sudo dockerd >/tmp/dockerd.log 2>&1 &
else
    echo "Docker daemon already running."
end

# Ensure the envs directory exists and is accessible (fallback if mount fails)
if not test -d /home/me/envs
    echo "Creating missing /home/me/envs directory and linking to .devcontainer/envs"
    mkdir -p /home/me/envs
    ln -sf /workspaces/aesthetic-computer/.devcontainer/envs/* /home/me/envs/
end

set -l env_loader /home/me/envs/load_envs.fish
if test -f $env_loader
    source $env_loader
    if functions -q load_envs
        load_envs # Load devcontainer envs conditionally.
        echo "🔧 Environment variables loaded from devcontainer.env"
    else
        echo "⚠️  load_envs function not found in $env_loader"
    end
else
    echo "ℹ️  No env loader found at $env_loader"
end

# Set environment variables to prevent ETXTBSY errors and disable telemetry
set -gx NETLIFY_CLI_TELEMETRY_DISABLED 1
set -gx NODE_DISABLE_COMPILE_CACHE 1

set -gx TERM xterm-256color

# Start dbus-run-session for the keryring and execute the command passed to the script
# dbus-run-session -- sh -c "eval \$(echo \$DBUS_SESSION_BUS_ADDRESS); exec $argv"

# Send a welcome message!
toilet "Aesthetic Computer" -f future | lolcat -x -r

# Go to the user's directory.
cd /home/me

# Ensure git config is writable by the current user
sudo chown me:me /home/me 2>/dev/null
if test -f /home/me/.gitconfig
    sudo chown me:me /home/me/.gitconfig 2>/dev/null
end
if test -d /home/me/.config/git
    sudo chown -R me:me /home/me/.config/git 2>/dev/null
end

# Login to Github.
if not gh auth status
    echo "Not logged into GitHub, exiting to shell."
    return
end

if test -n "$GIT_USER_EMAIL"
    git config --global user.email $GIT_USER_EMAIL
    echo "Set git user email to: $GIT_USER_EMAIL"
end

if test -n "$GIT_USER_NAME"
    git config --global user.name $GIT_USER_NAME
    echo "Set git user name to: $GIT_USER_NAME"
end

# Add aesthetic-computer as the "safe" directory.
git config --global --add safe.directory /home/me/aesthetic-computer

# Set rebase as default for pull operations.
git config --global pull.rebase true

# Disable GPG signing to prevent commit issues in dev environment
git config --global commit.gpgsign false

# Make sure git is setup and authorized for making commits via `gh`.
gh auth setup-git

# Ensure GPG signing is disabled at both global and local levels (GitHub CLI might re-enable it)
git config --global commit.gpgsign false
if test -d /home/me/aesthetic-computer
    pushd /home/me/aesthetic-computer >/dev/null
    git config --local commit.gpgsign false 2>/dev/null
    popd >/dev/null
else
    echo "ℹ️  Skipping local git config; repository directory not found"
end

# Disable Netlify CLI telemetry to prevent ETXTBSY errors
echo "Disabling Netlify CLI telemetry..."
cd /home/me/aesthetic-computer/system && netlify --telemetry-disable 2>/dev/null || echo "Netlify telemetry disable completed"

# Add aesthetic.local and sotce.local to /etc/hosts if they don't already exist
if not grep -q "aesthetic.local" /etc/hosts
    echo "127.0.0.1 aesthetic.local" | sudo tee -a /etc/hosts
end

if not grep -q "sotce.local" /etc/hosts
    echo "127.0.0.1 sotce.local" | sudo tee -a /etc/hosts
end

# Apply the 'vault' credentials to the mounted aesthetic-computer volume, and make sure it exists.
if test -d /home/me/aesthetic-computer
    if not test -d /home/me/aesthetic-computer/aesthetic-computer-vault
        gh repo clone whistlegraph/aesthetic-computer-vault /home/me/aesthetic-computer/aesthetic-computer-vault
        cd /home/me/aesthetic-computer/aesthetic-computer-vault
        sudo fish devault.fish
        
        # Load environment variables after initial vault setup
        if test -d /home/me/envs
            source /home/me/envs/load_envs.fish
            load_envs # Load envs after initial vault setup
            echo "🔧 Environment variables loaded after initial vault setup"
        end
    else
        cd /home/me/aesthetic-computer/aesthetic-computer-vault
        git pull
        sudo fish devault.fish
        echo "🔓 Vault mounted."
    end
    
    # Reload environment variables after vault is mounted
    if test -d /home/me/envs
        source /home/me/envs/load_envs.fish
        load_envs # Reload envs after vault mount
        echo "🔧 Environment variables reloaded after vault mount"
    end
else
    echo "⚠️🔒 Vault unmounted!"
end

if not test -d /home/me/aesthetic-computer/aesthetic-computer-code
    gh repo clone whistlegraph/aesthetic-computer-code /home/me/aesthetic-computer/aesthetic-computer-code
else
    cd /home/me/aesthetic-computer/aesthetic-computer-vault
    git pull
end

# generate ssl certificates (if they don't already exist)
cd /home/me/aesthetic-computer/ssl-dev

# Check if the files exist in ssl-dev directory
if not test -f localhost.pem; or not test -f localhost-key.pem
    # Initialize mkcert CA if not already done
    if not test -d /home/me/.local/share/mkcert
        mkcert -install
        # Install nss-tools for browser support if not already installed
        if not command -v certutil >/dev/null
            sudo dnf install -y nss-tools
            mkcert -install
        end
    end
    # Generate SSL certificates (without --install-only flag)
    env nogreet=true fish ./ssl-install.fish
else
    # Install existing certificates
    env nogreet=true fish ./ssl-install.fish --install-only
end

# Check if the nanos/ssl directory exists, and transfer the ssl certs over.
#if not test -f /home/me/aesthetic-computer/nanos/ssl/localhost.pem
mkdir -p /home/me/aesthetic-computer/nanos/ssl
cp localhost.pem ../nanos/ssl
cp localhost-key.pem ../nanos/ssl
echo "Certificates copied to 'nanos'."
#end

sudo chown me:me -R /home/me/aesthetic-computer/ssl-dev
sudo chown me:me -R /home/me/aesthetic-computer/nanos/ssl

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

# Function to check and install npm dependencies in a directory
function install_npm_deps
    set -l dir $argv[1]
    set -l dir_name (basename $dir)
    
    if test -f $dir/package.json
        if not test -d $dir/node_modules || not count $dir/node_modules/* >/dev/null 2>/dev/null
            echo "📦 Installing dependencies in $dir_name..."
            cd $dir
            npm ci --no-fund --no-audit --silent
            if test $status -ne 0
                echo "⚠️  Failed to install dependencies in $dir_name, trying npm install..."
                npm install --no-fund --no-audit --silent
            end
        else
            echo "✅ $dir_name already has node_modules"
        end
    end
end

# Check and install dependencies in key directories
cd /home/me/aesthetic-computer

# Install root dependencies first
if not test -d /home/me/aesthetic-computer/node_modules || not count /home/me/aesthetic-computer/node_modules/* >/dev/null
    echo "📦 Installing root dependencies..."
    npm install -g npm@latest --no-fund --no-audit
    npm ci --no-fund --no-audit
else
    echo "✅ Root directory already has node_modules"
end

# Install dependencies in critical subdirectories (archive intentionally excluded)
set -l critical_dirs system session-server vscode-extension nanos daemon utilities shared

for dir in $critical_dirs
    if test -d /home/me/aesthetic-computer/$dir
        install_npm_deps /home/me/aesthetic-computer/$dir
    end
end

# Run the comprehensive install script for any remaining directories
echo "🔄 Running comprehensive install script for remaining directories..."
pushd /home/me/aesthetic-computer
npm run install:everything-else
popd

# Make sure this user owns the emacs directory.
sudo chown -R me:me ~/.emacs.d

# Link the prompts directory to VSCode User config
mkdir -p ~/.config/Code/User
rm -rf ~/.config/Code/User/prompts
ln -s /home/me/aesthetic-computer/prompts ~/.config/Code/User/prompts
echo "✨ Prompts directory linked to VSCode User config"

# Link the modes directory to .github for VSCode chatmodes
rm -rf /home/me/aesthetic-computer/.github
ln -s /home/me/aesthetic-computer/modes /home/me/aesthetic-computer/.github
echo "✨ Modes directory linked to .github for VSCode chatmodes"

# Source the devcontainer config to load AC development functions
source /workspaces/aesthetic-computer/.devcontainer/config.fish
echo "✨ AC development functions loaded (ac-pack, ac-unpack, etc.)"

if functions -q ensure-emacs-daemon-ready
    echo "🧠 Priming Emacs daemon for aesthetic launch..."
    if ensure-emacs-daemon-ready --timeout=480 --silent
        echo "✅ Emacs daemon primed and ready."
    else
        echo "⚠️  Emacs daemon prewarm failed; launch will retry automatically."
    end
end

# Note: .waiter file creation is handled by postAttachCommand in devcontainer.json

# echo "Initializing 📋 Clipboard Service" | lolcat -x -r

# ❤️‍🔥
# TODO: This may not be the best method because cdocker aesthetic needs to be
#       running in the user environment that has the clipboard.

# Run the isomorphic_copy clipboard on the host.
# Make sure the host allows ssh access from its own private key... `ssh-copy-id -i ~/.ssh/id_rsa.pub $USER@localhost`
# ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $HOST_USER@172.17.0.1

# ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $HOST_USER@172.17.0.1 "cdocker aesthetic"
