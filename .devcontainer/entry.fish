#!/usr/bin/env fish

# Log file for aesthetic function to tail during wait
set -g ENTRY_LOG /tmp/entry-fish.log
echo "" > $ENTRY_LOG

# Simple wrapper: append each echo to log file too
# We override echo within this script
function echo
    builtin echo $argv
    builtin echo $argv >> $ENTRY_LOG 2>/dev/null
end

# Fast path for VS Code "Reload Window" - if .waiter exists and key processes running, skip setup
if test -f /home/me/.waiter
    echo "🔄 Detected reload (found .waiter file)"
    
    # Check if key processes are already running
    set -l dockerd_running (pgrep -x dockerd 2>/dev/null)
    set -l emacs_running (pgrep -f "emacs.*daemon" 2>/dev/null)
    
    if test -n "$dockerd_running"
        echo "✅ Docker daemon already running (PID: $dockerd_running)"
        
        # Check if emacs daemon is running AND responsive
        if test -n "$emacs_running"
            if emacsclient -e t >/dev/null 2>&1
                echo "✅ Emacs daemon running and responsive (PID: $emacs_running)"
            else
                echo "⚠️  Emacs daemon zombie detected (PID: $emacs_running) - restarting..."
                pkill -9 -f "emacs.*daemon" 2>/dev/null
                pkill -9 emacs 2>/dev/null
                sleep 1
                emacs --daemon 2>&1
                sleep 1
                if emacsclient -e t >/dev/null 2>&1
                    echo "✅ Emacs daemon restarted successfully"
                else
                    echo "❌ Failed to restart emacs daemon"
                end
            end
        else
            echo "⚠️  Emacs daemon not running - starting..."
            emacs --daemon 2>&1
            sleep 1
        end
        
        # Ensure CDP tunnel is up (for artery-tui to control VS Code on host)
        set -l cdp_tunnel (pgrep -f "ssh.*9333.*host.docker.internal" 2>/dev/null)
        if test -n "$cdp_tunnel"
            echo "✅ CDP tunnel already running (PID: $cdp_tunnel)"
        else
            echo "🩸 Starting CDP tunnel..."
            ssh -f -N -o StrictHostKeyChecking=no -o BatchMode=yes -o ConnectTimeout=5 -L 9333:127.0.0.1:9333 me@host.docker.internal 2>/dev/null
            if test $status -eq 0
                echo "✅ CDP tunnel established (localhost:9333 → host:9333)"
            else
                echo "⚠️  CDP tunnel failed (artery may not control VS Code)"
            end
        end
        
        echo "⚡ Skipping full setup - container already configured"
        echo "✅ Ready! (fast reload path)"
        exit 0
    else
        echo "⚠️  Docker daemon not found, running full setup..."
    end
end

# Fix fish path if needed (Fedora installs to /usr/sbin instead of /usr/bin)
if not test -f /usr/bin/fish; and test -f /usr/sbin/fish
    sudo ln -sf /usr/sbin/fish /usr/bin/fish
    echo "🐟 Created fish symlink: /usr/bin/fish -> /usr/sbin/fish"
end

# Function to ensure fish config directory has correct permissions
function ensure_fish_config_permissions
    set -l fish_config_dir /home/me/.config/fish
    set -l fish_data_dir /home/me/.local/share/fish
    
    echo "🔧 Fixing permissions for fish directories..."
    
    # Fix the parent .config directory first (most important)
    if test -d /home/me/.config
        sudo chown -R me:me /home/me/.config 2>/dev/null
        sudo chmod -R 755 /home/me/.config 2>/dev/null
        echo "✅ Fixed permissions for /home/me/.config"
    end
    
    if test -d $fish_config_dir
        # Fix ownership and permissions for fish config directory - be VERY aggressive
        sudo chown -R me:me $fish_config_dir 2>/dev/null
        sudo chmod -R 755 $fish_config_dir 2>/dev/null
        sudo chmod 644 $fish_config_dir/*.fish 2>/dev/null
        # Also fix individual subdirectories explicitly
        sudo chown -R me:me $fish_config_dir/conf.d 2>/dev/null
        sudo chown -R me:me $fish_config_dir/functions 2>/dev/null
        sudo chown -R me:me $fish_config_dir/completions 2>/dev/null
        echo "✅ Fixed permissions for $fish_config_dir"
    end
    
    if test -d $fish_data_dir
        # Fix ownership and permissions for fish data directory
        sudo chown -R me:me $fish_data_dir 2>/dev/null
        sudo chmod -R 755 $fish_data_dir 2>/dev/null
        echo "✅ Fixed permissions for $fish_data_dir"
    end
    
    # Fix /home/me/.local directory too
    if test -d /home/me/.local
        sudo chown -R me:me /home/me/.local 2>/dev/null
        sudo chmod -R 755 /home/me/.local 2>/dev/null
        echo "✅ Fixed permissions for /home/me/.local"
    end
    
    # Disable fish's universal variable file daemon (fishd) which causes permission issues
    # We'll use fish_variables instead which is simpler and doesn't create temp files
    set -U fish_greeting ""  # Suppress greeting while we're at it
    
    echo "✨ All permissions fixed!"
end

# Function to ensure correct Docker socket permissions
function ensure_docker_socket_permissions
    set -l DOCKER_SOCKET /var/run/docker.sock
    if test -S $DOCKER_SOCKET
        set -l DOCKER_GID (stat -c '%g' $DOCKER_SOCKET)
        set -l CURRENT_DOCKER_GROUP_INFO (getent group $DOCKER_GID)

        if test -z "$CURRENT_DOCKER_GROUP_INFO" # No group with this GID exists
            if getent group docker > /dev/null
                echo "Modifying existing docker group to GID $DOCKER_GID..."
                sudo groupmod -g $DOCKER_GID docker
            else
                echo "Creating new docker group with GID $DOCKER_GID..."
                sudo groupadd -g $DOCKER_GID docker
            end
        else # A group with this GID already exists, check if it's named 'docker'
            set -l GROUP_NAME (echo $CURRENT_DOCKER_GROUP_INFO | cut -d: -f1)
            if test "$GROUP_NAME" != "docker"
                echo "Warning: Group with GID $DOCKER_GID exists but is named '$GROUP_NAME', not 'docker'. Manual intervention might be needed."
                # Optionally, you could decide to add 'me' to this group anyway,
                # or try to rename it if it's safe, or delete and recreate 'docker' group.
                # For now, we'll proceed assuming 'docker' is the target group name.
                if getent group docker > /dev/null
                    echo "Modifying existing docker group to GID $DOCKER_GID..."
                    sudo groupmod -g $DOCKER_GID docker
                else
                    echo "Creating new docker group with GID $DOCKER_GID..."
                    sudo groupadd -g $DOCKER_GID docker
                end
            else
                echo "Docker group '$GROUP_NAME' with GID $DOCKER_GID already exists."
            end
        end

        # Ensure 'me' user is in the 'docker' group
        if not groups me | string match -q -r '\\bdocker\\b'
            echo "Adding user 'me' to docker group..."
            sudo usermod -aG docker me
            # Changes to group membership may require a new login session or `newgrp docker`
            # to take effect immediately in the current shell.
            # For a script, subsequent commands in this same script might not see the new group immediately.
            # However, for processes spawned after this script, it should be fine.
        else
            echo "User 'me' is already in docker group."
        end
    else
        echo "Docker socket $DOCKER_SOCKET not found."
    end
end

function install_and_trust_certificate
    set -l certificate_path $argv[1]
    set -l certificate_key_path $argv[2]
    set -l certificate_dir /etc/pki/ca-trust/source/anchors/

    if test -z "$certificate_path"; or test -z "$certificate_key_path"
        return
    end

    if test -f $certificate_path
        sudo cp $certificate_path $certificate_dir
    end

    if test -f $certificate_key_path
        sudo cp $certificate_key_path $certificate_dir
    end

    sudo update-ca-trust
end

function ensure_ssl_dev_certs
    set -l ssl_dir /home/me/aesthetic-computer/ssl-dev
    set -l nanos_ssl_dir /home/me/aesthetic-computer/nanos/ssl

    if not test -d $ssl_dir
        return
    end

    if not type -q mkcert
        echo "⚠️ mkcert not available; skipping automatic certificate generation."
        return
    end

    set -l previous_dir (pwd)
    cd $ssl_dir

    set -l have_cert 1
    if not test -f localhost.pem; or not test -f localhost-key.pem
        set have_cert 0
    end

    if test $have_cert -eq 0
        echo "🔐 Generating mkcert certificates for the dev container..."
        if not test -d /home/me/.local/share/mkcert
            echo "📥 Installing mkcert local CA..."
            mkcert -install >/tmp/mkcert-install.log 2>&1
            if test $status -ne 0
                echo "⚠️ mkcert CA installation failed. See /tmp/mkcert-install.log for details."
            end
            if not type -q certutil
                echo "📦 Installing nss-tools for certificate trust..."
                sudo dnf install -y nss-tools >/tmp/nss-tools-install.log 2>&1
                mkcert -install >/tmp/mkcert-install.log 2>&1
            end
        end

        env nogreet=true fish ./ssl-install.fish
        if test $status -ne 0
            echo "⚠️ Automatic certificate generation failed using ssl-install.fish."
            cd $previous_dir
            return
        end
    else
        env nogreet=true fish ./ssl-install.fish --install-only
    end

    mkdir -p $nanos_ssl_dir
    cp localhost.pem $nanos_ssl_dir/localhost.pem
    cp localhost-key.pem $nanos_ssl_dir/localhost-key.pem

    sudo chown me:me -R $ssl_dir
    sudo chown me:me -R $nanos_ssl_dir

    install_and_trust_certificate $ssl_dir/localhost.pem $ssl_dir/localhost-key.pem

    cd $previous_dir
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

# Start Ollama daemon in the background if available and not already running
if type -q ollama
    if not pgrep -x ollama >/dev/null
        echo "Starting Ollama daemon..."
        ollama serve >/tmp/ollama.log 2>&1 &
    else
        echo "Ollama daemon already running."
    end
else
    echo "Ollama not found; skipping. Install via Dockerfile to enable." >&2
end

# Ensure the envs directory exists and is accessible (fallback if mount fails)
if not test -d /home/me/envs
    echo "Creating missing /home/me/envs directory and linking to .devcontainer/envs"
    mkdir -p /home/me/envs
    ln -sf /workspaces/aesthetic-computer/.devcontainer/envs/* /home/me/envs/
end

if test -d /home/me/envs
    source /home/me/envs/load_envs.fish
    load_envs # Load devcontainer envs conditionally.
    echo "🔧 Environment variables loaded from devcontainer.env"
end

# Set environment variables to prevent ETXTBSY errors and disable telemetry
set -gx NETLIFY_CLI_TELEMETRY_DISABLED 1
set -gx NODE_DISABLE_COMPILE_CACHE 1

set -gx TERM xterm-256color

# Create xdg-open wrapper to open URLs on Windows host from dev container
if not test -f /usr/local/bin/xdg-open
    echo "🌐 Creating xdg-open wrapper for host browser..."
    printf '#!/bin/bash\n"\$BROWSER" "\$@"\n' | sudo tee /usr/local/bin/xdg-open >/dev/null
    sudo chmod +x /usr/local/bin/xdg-open
end

# Start dbus-run-session for the keryring and execute the command passed to the script
# dbus-run-session -- sh -c "eval \$(echo \$DBUS_SESSION_BUS_ADDRESS); exec $argv"

# Send a welcome message!
toilet "Aesthetic Computer" -f future | lolcat -x -r

# Go to the user's directory.
cd /home/me

ensure_ssl_dev_certs

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
cd /home/me/aesthetic-computer
git config --local commit.gpgsign false
git config --local user.signingkey ""
echo "🔓 GPG signing disabled for commits"

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
    
    # Setup SSH keys from vault
    if test -d /home/me/aesthetic-computer/aesthetic-computer-vault/home/.ssh
        mkdir -p /home/me/.ssh
        cp -f /home/me/aesthetic-computer/aesthetic-computer-vault/home/.ssh/* /home/me/.ssh/
        chmod 700 /home/me/.ssh
        chmod 600 /home/me/.ssh/id_rsa 2>/dev/null
        chmod 644 /home/me/.ssh/id_rsa.pub 2>/dev/null
        echo "🔑 SSH keys restored from vault"
    end
else
    echo "⚠️🔒 Vault unmounted!"
end

# Fix all permissions AFTER vault setup (vault copies files as root with sudo)
echo "🔧 Fixing permissions after vault setup..."

# Fix fish config permissions (vault may have overwritten with root-owned files)
ensure_fish_config_permissions

# Fix SSH directory permissions
if test -d /home/me/.ssh
    sudo chown -R me:me /home/me/.ssh 2>/dev/null
    sudo chmod 700 /home/me/.ssh 2>/dev/null
    sudo chmod 600 /home/me/.ssh/* 2>/dev/null
    echo "✅ Fixed permissions for /home/me/.ssh (after vault setup)"
end

if not test -d /home/me/aesthetic-computer/aesthetic-computer-code
    gh repo clone whistlegraph/aesthetic-computer-code /home/me/aesthetic-computer/aesthetic-computer-code
else
    cd /home/me/aesthetic-computer/aesthetic-computer-vault
    git pull
end

# Function to check and install npm dependencies in a directory
set -g NODE_DEPS_CHECK_SCRIPT /workspaces/aesthetic-computer/.devcontainer/scripts/check-node-deps.mjs

function verify_npm_versions
    set -l dir $argv[1]
    set -l dir_name (basename $dir)

    if not test -f $NODE_DEPS_CHECK_SCRIPT
        return
    end

    if not test -f $dir/package.json
        return
    end

    if not test -d $dir/node_modules
        return
    end

    node $NODE_DEPS_CHECK_SCRIPT $dir
    set -l check_status $status

    if test $check_status -ne 0
        echo "♻️ Resyncing dependencies in $dir_name to match package.json versions..."
        cd $dir
        npm ci --no-fund --no-audit --silent
        if test $status -ne 0
            echo "⚠️ npm ci failed in $dir_name, falling back to npm install"
            npm install --no-fund --no-audit --silent
        end
    end
end

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
        verify_npm_versions $dir
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

verify_npm_versions /home/me/aesthetic-computer

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

# Copy feed secrets from vault to environment (if vault exists)
if test -f /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env
    echo "🔐 Loading feed system environment variables from vault..."
    # Copy to devcontainer envs directory for persistence
    cp /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env /home/me/envs/feed.env
    # Source it globally for the session
    for line in (cat /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env | grep -v '^#' | grep '=')
        set -gx (string split '=' $line)
    end
    echo "✅ Feed environment variables loaded"
else
    echo "⚠️  Feed vault not found - skipping feed environment setup"
end

# Set port 8888 to public in Codespaces (if applicable)
if test -f /workspaces/aesthetic-computer/.devcontainer/scripts/set-port-public.fish
    fish /workspaces/aesthetic-computer/.devcontainer/scripts/set-port-public.fish
end

# Create .waiter file to signal container is ready (moved from postAttachCommand to avoid terminal popup in Codespaces)
sudo touch /home/me/.waiter
sudo chmod 777 /home/me/.waiter
sudo chmod +w /home/me
echo "✅ Container ready signal created (.waiter)"

# 🩸 Setup SSH tunnel for CDP (Chrome DevTools Protocol) to host
# This allows artery-tui to control VS Code on the host machine
if test -n "$HOST_IP"; or test (uname -s) != "Linux"
    # Kill any existing CDP tunnels
    pkill -f "ssh.*9333.*host.docker.internal" 2>/dev/null
    
    # Create tunnel in background (port 9333 to avoid conflicts with svchost.exe on 9222)
    ssh -f -N -o StrictHostKeyChecking=no -o ConnectTimeout=5 -L 9333:127.0.0.1:9333 me@host.docker.internal 2>/dev/null
    if test $status -eq 0
        echo "🩸 CDP tunnel established (localhost:9333 → host:9333)"
    else
        echo "⚠️  CDP tunnel failed (artery may not work)"
    end
end

# echo "Initializing 📋 Clipboard Service" | lolcat -x -r

# ❤️‍🔥
# TODO: This may not be the best method because cdocker aesthetic needs to be
#       running in the user environment that has the clipboard.

# Run the isomorphic_copy clipboard on the host.
# Make sure the host allows ssh access from its own private key... `ssh-copy-id -i ~/.ssh/id_rsa.pub $USER@localhost`
# ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $HOST_USER@172.17.0.1

# ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $HOST_USER@172.17.0.1 "cdocker aesthetic"
