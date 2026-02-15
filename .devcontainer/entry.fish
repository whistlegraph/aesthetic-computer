#!/usr/bin/env fish

# Log file for aesthetic function to tail during wait
set -g ENTRY_LOG /tmp/entry-fish.log
set -g ENTRY_LOG_DIR /home/me/.entry-logs
if test -d /workspaces/aesthetic-computer/.devcontainer
    set -g ENTRY_LOG_DIR /workspaces/aesthetic-computer/.devcontainer/entry-logs
end
set -l entry_ts (date +%Y%m%d_%H%M%S)
set -g ENTRY_LOG_REAL $ENTRY_LOG_DIR/entry-$entry_ts.log

mkdir -p $ENTRY_LOG_DIR
echo "" > $ENTRY_LOG_REAL
ln -sf $ENTRY_LOG_REAL $ENTRY_LOG
ln -sf $ENTRY_LOG_REAL $ENTRY_LOG_DIR/latest.log
echo "" > $ENTRY_LOG

# Aggressive logging function - writes immediately with timestamp and syncs to disk
function log
    set -l timestamp (date '+%H:%M:%S.%3N')
    set -l msg "[$timestamp] $argv"
    builtin echo $msg
    builtin echo $msg >> $ENTRY_LOG_REAL 2>/dev/null
    # Force sync to disk immediately so we don't lose logs on crash
    sync 2>/dev/null
end

# Log with specific level prefix
function log_info
    log "ℹ️  $argv"
end

function log_ok
    log "✅ $argv"
end

function log_warn
    log "⚠️  $argv"
end

function log_error
    log "❌ $argv"
end

function log_step
    log "🔹 $argv"
end

# Simple wrapper: append each echo to log file too (keep for backward compat)
# We override echo within this script
function echo
    builtin echo $argv
    builtin echo "[" (date '+%H:%M:%S') "] $argv" >> $ENTRY_LOG_REAL 2>/dev/null
end

# Run a command and capture output to the entry log.
function log_cmd
    set -l cmd_str (string join ' ' -- $argv)
    log_step "Running: $cmd_str"
    command $argv >> $ENTRY_LOG_REAL 2>&1
    set -l cmd_status $status
    if test $cmd_status -ne 0
        log_error "Command failed ($cmd_status): $cmd_str"
    else
        log_ok "Command succeeded: $cmd_str"
    end
    sync 2>/dev/null
    return $cmd_status
end

# Run a command with a timeout if available.
function log_cmd_timeout
    set -l seconds $argv[1]
    set -l cmd $argv[2..-1]
    log_step "Running with "$seconds"s timeout: "(string join ' ' -- $cmd)
    if type -q timeout
        log_cmd timeout $seconds $cmd
    else
        log_cmd $cmd
    end
    return $status
end

# Start CDP tunnel without blocking startup if SSH auth is missing.
function start_cdp_tunnel
    set -l ssh_args -f -N -o StrictHostKeyChecking=no -o BatchMode=yes -o ExitOnForwardFailure=yes -o ConnectTimeout=5 -o ConnectionAttempts=1 -L 9333:127.0.0.1:9333 me@host.docker.internal
    log_cmd_timeout 8 ssh $ssh_args
    return $status
end

log "════════════════════════════════════════════════════════════════"
log "🚀 ENTRY.FISH STARTING"
log "════════════════════════════════════════════════════════════════"
log_info "Timestamp: "(date -Iseconds)
log_info "Log file: $ENTRY_LOG_REAL"
log_info "Log dir: $ENTRY_LOG_DIR"
log_info "Hostname: "(cat /etc/hostname 2>/dev/null; or echo "unknown")
log_info "User: "(whoami)
log_info "PWD: "(pwd)

# 🔍 Start Devcontainer Status Server EARLY (so VS Code can see boot progress)
# This allows the Welcome Panel to show the boot process in real-time
log_step "EARLY START: Devcontainer Status Server"
if test -f /workspaces/aesthetic-computer/artery/devcontainer-status.mjs
    # Kill any existing instance first
    pkill -f "devcontainer-status" 2>/dev/null
    sleep 0.1
    nohup node /workspaces/aesthetic-computer/artery/devcontainer-status.mjs --server >/tmp/devcontainer-status.log 2>&1 &
    disown
    log_ok "Status Server started early on http://127.0.0.1:7890"
else
    log_warn "devcontainer-status.mjs not found, will try later"
end

log_step "PHASE 1: Environment sync"

# --- Sync secrets from vault to devcontainer env ---
# The vault stores the canonical secrets; copy them to devcontainer on startup
set -l vault_env ~/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env
set -l devcontainer_env ~/aesthetic-computer/.devcontainer/envs/devcontainer.env
if test -f $vault_env
    if test -f $devcontainer_env
        # Merge: update devcontainer.env with any keys from vault (vault wins)
        for line in (cat $vault_env)
            set -l key (string split -m1 '=' -- $line)[1]
            if test -n "$key"
                # Remove old key if exists, then append new
                sed -i "/^$key=/d" $devcontainer_env 2>/dev/null
                echo $line >> $devcontainer_env
            end
        end
        log_ok "Synced secrets from vault to devcontainer.env"
    else
        # No devcontainer.env, just copy from vault
        cp $vault_env $devcontainer_env
        log_ok "Copied secrets from vault to devcontainer.env"
    end
else
    log_warn "Vault env not found at $vault_env"
end

# --- Setup session-server SSH key from vault ---
set -l ss_key_src ~/aesthetic-computer-vault/session-server/session_server
set -l ss_key_dst ~/.ssh/session_server
if test -f $ss_key_src; and not test -f $ss_key_dst
    # Ensure .ssh directory exists with correct ownership and permissions
    sudo mkdir -p ~/.ssh
    sudo chown me:me ~/.ssh
    sudo chmod 700 ~/.ssh

    cp $ss_key_src $ss_key_dst
    cp "$ss_key_src.pub" "$ss_key_dst.pub"
    chmod 600 $ss_key_dst
    chmod 644 "$ss_key_dst.pub"

    # Create config file if it doesn't exist with correct ownership
    if not test -f ~/.ssh/config
        touch ~/.ssh/config
        chmod 600 ~/.ssh/config
    end

    # Add SSH config entry if missing
    if not grep -q "Host session-server" ~/.ssh/config 2>/dev/null
        echo "" >> ~/.ssh/config
        echo "Host session-server" >> ~/.ssh/config
        echo "    HostName 157.245.134.225" >> ~/.ssh/config
        echo "    User root" >> ~/.ssh/config
        echo "    IdentityFile ~/.ssh/session_server" >> ~/.ssh/config
        echo "    IdentitiesOnly yes" >> ~/.ssh/config
        echo "    StrictHostKeyChecking accept-new" >> ~/.ssh/config
    end
    log_ok "Session-server SSH key installed"
else if not test -f $ss_key_src
    log_warn "Session-server SSH key not found in vault"
end

log_step "PHASE 2: Checking for fast reload path"

# Fast path for VS Code "Reload Window" - if .waiter exists and key processes running, skip setup
if test -f /home/me/.waiter
    log_info "Detected reload (found .waiter file)"
    
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
            start_cdp_tunnel
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
            log_cmd_timeout 60 mkcert -install
            if test $status -ne 0
                echo "⚠️ mkcert CA installation failed. See entry.fish log for details."
            end
            if not type -q certutil
                echo "📦 Installing nss-tools for certificate trust..."
                log_cmd_timeout 300 sudo dnf install -y nss-tools
                log_cmd_timeout 60 mkcert -install
            end
        end

        log_cmd_timeout 120 env nogreet=true fish ./ssl-install.fish
        if test $status -ne 0
            echo "⚠️ Automatic certificate generation failed using ssl-install.fish."
            cd $previous_dir
            return
        end
    else
        log_cmd_timeout 120 env nogreet=true fish ./ssl-install.fish --install-only
    end

    mkdir -p $nanos_ssl_dir
    cp localhost.pem $nanos_ssl_dir/localhost.pem
    cp localhost-key.pem $nanos_ssl_dir/localhost-key.pem

    sudo chown me:me -R $ssl_dir
    sudo chown me:me -R $nanos_ssl_dir

    install_and_trust_certificate $ssl_dir/localhost.pem $ssl_dir/localhost-key.pem

    cd $previous_dir
end

log_step "PHASE 3: Docker socket permissions"
# Call the function to set up Docker socket permissions
ensure_docker_socket_permissions

log_step "PHASE 4: Starting daemons"
# Start Docker daemon in the background if not already running
if not pgrep -x dockerd >/dev/null
    log_info "Starting Docker daemon..."
    sudo dockerd >/tmp/dockerd.log 2>&1 &
else
    log_ok "Docker daemon already running."
end

# Start Ollama daemon in the background if available and not already running
if type -q ollama
    if not pgrep -x ollama >/dev/null
        log_info "Starting Ollama daemon..."
        ollama serve >/tmp/ollama.log 2>&1 &
    else
        log_ok "Ollama daemon already running."
    end
else
    log_warn "Ollama not found; skipping."
end

log_step "PHASE 5: Environment setup"
# Ensure the envs directory exists and is accessible (fallback if mount fails)
if not test -d /home/me/envs
    log_info "Creating missing /home/me/envs directory and linking to .devcontainer/envs"
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

log_step "PHASE 6: SSL certificates"
ensure_ssl_dev_certs

log_step "PHASE 7: GitHub authentication"
# Login to Github - use GH_TOKEN from vault if available
if not gh auth status >/dev/null 2>&1
    if test -n "$GH_TOKEN"
        log_info "Authenticating to GitHub using GH_TOKEN..."
        echo $GH_TOKEN | gh auth login --with-token
        if gh auth status >/dev/null 2>&1
            log_ok "GitHub authentication successful"
        else
            log_error "GitHub authentication failed"
            log_error "Not logged into GitHub, exiting to shell."
            return
        end
    else
        log_warn "Not logged into GitHub and no GH_TOKEN available, exiting to shell."
        return
    end
else
    log_ok "Already authenticated to GitHub"
end

log_step "PHASE 8: Git configuration"
if test -n "$GIT_USER_EMAIL"
    git config --global user.email $GIT_USER_EMAIL
    log_info "Set git user email to: $GIT_USER_EMAIL"
end

if test -n "$GIT_USER_NAME"
    git config --global user.name $GIT_USER_NAME
    log_info "Set git user name to: $GIT_USER_NAME"
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

log_step "PHASE 9: Vault setup"
# Apply the 'vault' credentials to the mounted aesthetic-computer volume, and make sure it exists.
if test -d /home/me/aesthetic-computer
    if not test -d /home/me/aesthetic-computer/aesthetic-computer-vault
        log_info "Cloning vault repository..."
        gh repo clone whistlegraph/aesthetic-computer-vault /home/me/aesthetic-computer/aesthetic-computer-vault
        cd /home/me/aesthetic-computer/aesthetic-computer-vault
        log_info "Running devault.fish..."
        sudo fish devault.fish
        
        # Load environment variables after initial vault setup
        if test -d /home/me/envs
            source /home/me/envs/load_envs.fish
            load_envs # Load envs after initial vault setup
            log_ok "Environment variables loaded after initial vault setup"
        end
    else
        cd /home/me/aesthetic-computer/aesthetic-computer-vault
        log_info "Pulling latest vault..."
        git pull
        sudo fish devault.fish
        log_ok "Vault mounted."
    end
    
    # Reload environment variables after vault is mounted
    if test -d /home/me/envs
        source /home/me/envs/load_envs.fish
        load_envs # Reload envs after vault mount
        log_ok "Environment variables reloaded after vault mount"
    end
    
    # Setup SSH keys from vault (only if not already set up)
    set -l vault_ssh /home/me/aesthetic-computer/aesthetic-computer-vault/home/.ssh
    if test -d $vault_ssh
        # Only copy if id_rsa doesn't exist yet (first time setup)
        if not test -f /home/me/.ssh/id_rsa
            # Ensure .ssh directory exists with correct ownership and permissions
            sudo mkdir -p /home/me/.ssh
            sudo chown -R me:me /home/me/.ssh 2>/dev/null
            sudo chmod 700 /home/me/.ssh 2>/dev/null

            cp -f $vault_ssh/* /home/me/.ssh/ 2>/dev/null
            chmod 700 /home/me/.ssh 2>/dev/null
            chmod 600 /home/me/.ssh/id_rsa 2>/dev/null
            chmod 644 /home/me/.ssh/id_rsa.pub 2>/dev/null
            chmod 600 /home/me/.ssh/config 2>/dev/null
            chmod 600 /home/me/.ssh/aesthetic_pds 2>/dev/null
            chmod 644 /home/me/.ssh/aesthetic_pds.pub 2>/dev/null
            log_ok "SSH keys restored from vault"
        else
            log_info "SSH keys already exist, skipping vault copy"
        end
    end
    
    # Setup Copilot CLI config from vault (if volume is empty but vault has backup)
    set -l vault_copilot /home/me/aesthetic-computer/aesthetic-computer-vault/home/.copilot
    if test -d $vault_copilot
        # Only restore if volume mount is empty (no config.json)
        if not test -f /home/me/.copilot/config.json
            mkdir -p /home/me/.copilot
            cp -f $vault_copilot/config.json /home/me/.copilot/ 2>/dev/null
            log_ok "Copilot CLI config restored from vault"
        else
            log_info "Copilot CLI config already exists (using volume data)"
        end
    end
else
    log_error "Vault unmounted! /home/me/aesthetic-computer not found"
end

log_step "PHASE 10: Fixing permissions"
# Fix all permissions AFTER vault setup (vault copies files as root with sudo)
log_info "Fixing permissions after vault setup..."

# First, ensure the home directory itself is owned by me (critical for .emacs-logs etc)
sudo chown me:me /home/me 2>/dev/null
log_ok "Fixed ownership of /home/me"

# Create .emacs-logs directory if it doesn't exist
mkdir -p /home/me/.emacs-logs 2>/dev/null
log_ok "Ensured /home/me/.emacs-logs exists"

# Fix fish config permissions (vault may have overwritten with root-owned files)
log_info "Fixing fish config permissions..."
ensure_fish_config_permissions

# Fix SSH directory permissions (use sudo to fix ownership if needed)
if test -d /home/me/.ssh
    if not test -O /home/me/.ssh
        # SSH dir owned by root (probably from vault setup), fix ownership
        sudo chown -R me:me /home/me/.ssh 2>/dev/null
        log_ok "Fixed SSH directory ownership"
    end
    chmod 700 /home/me/.ssh 2>/dev/null
    chmod 600 /home/me/.ssh/* 2>/dev/null
    chmod 644 /home/me/.ssh/*.pub 2>/dev/null
    log_ok "Fixed permissions for /home/me/.ssh"
end

# Fix Copilot CLI directory permissions (volume mount may have root ownership)
if test -d /home/me/.copilot
    sudo chown -R me:me /home/me/.copilot 2>/dev/null
    sudo chmod 700 /home/me/.copilot 2>/dev/null
    # Ensure pkg directory exists and is writable for package extraction
    mkdir -p /home/me/.copilot/pkg 2>/dev/null
    chmod 755 /home/me/.copilot/pkg 2>/dev/null
    sudo chmod 600 /home/me/.copilot/config.json 2>/dev/null
    echo "✅ Fixed permissions for /home/me/.copilot (Copilot CLI config)"
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

    log_cmd node $NODE_DEPS_CHECK_SCRIPT $dir
    set -l check_status $status

    if test $check_status -ne 0
        echo "♻️ Resyncing dependencies in $dir_name to match package.json versions..."
        cd $dir
        log_cmd npm ci --no-fund --no-audit
        if test $status -ne 0
            echo "⚠️ npm ci failed in $dir_name, falling back to npm install"
            log_cmd npm install --no-fund --no-audit
        end
    end
end

# Function to check and fix esbuild architecture mismatches
function fix_esbuild_architecture
    set -l dir $argv[1]
    set -l dir_name (basename $dir)

    # Check if this directory has esbuild installed
    if test -d $dir/node_modules/@esbuild
        set -l arch (uname -m)
        set -l expected_esbuild

        # Determine expected esbuild platform package
        if test "$arch" = "x86_64"
            set expected_esbuild "linux-x64"
        else if test "$arch" = "aarch64"
            set expected_esbuild "linux-arm64"
        else
            return 0  # Unknown architecture, skip
        end

        # Check what's actually installed
        set -l installed_arch (ls $dir/node_modules/@esbuild 2>/dev/null | grep -E "^linux-")

        if test -n "$installed_arch"; and test "$installed_arch" != "$expected_esbuild"
            echo "🔧 Wrong esbuild architecture in $dir_name: $installed_arch (need $expected_esbuild)"
            echo "   Reinstalling dependencies to fix..."
            cd $dir
            rm -rf node_modules package-lock.json
            log_cmd npm install --no-fund --no-audit
            if test $status -eq 0
                echo "✅ Fixed esbuild architecture in $dir_name"
            else
                echo "⚠️  Failed to fix esbuild in $dir_name"
            end
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
            log_cmd npm ci --no-fund --no-audit
            if test $status -ne 0
                echo "⚠️  Failed to install dependencies in $dir_name, trying npm install..."
                log_cmd npm install --no-fund --no-audit
            end
        else
            echo "✅ $dir_name already has node_modules"
            # Check for architecture mismatches even if node_modules exists
            fix_esbuild_architecture $dir
        end
        verify_npm_versions $dir
    end
end

log_step "PHASE 11: NPM dependencies"
# Check and install dependencies in key directories
cd /home/me/aesthetic-computer

# Install root dependencies first
if not test -d /home/me/aesthetic-computer/node_modules || not count /home/me/aesthetic-computer/node_modules/* >/dev/null
    log_info "Installing root dependencies..."
    log_cmd npm install -g npm@latest --no-fund --no-audit
    log_cmd npm ci --no-fund --no-audit
else
    log_ok "Root directory already has node_modules"
end

log_info "Verifying npm versions..."
verify_npm_versions /home/me/aesthetic-computer

# Install dependencies in critical subdirectories (archive intentionally excluded)
set -l critical_dirs system session-server vscode-extension nanos daemon utilities shared

log_info "Installing dependencies in critical directories..."
for dir in $critical_dirs
    if test -d /home/me/aesthetic-computer/$dir
        log_info "Processing $dir..."
        install_npm_deps /home/me/aesthetic-computer/$dir
    end
end

# Run the comprehensive install script for any remaining directories
log_info "Running comprehensive install script for remaining directories..."
pushd /home/me/aesthetic-computer
log_cmd npm run install:everything-else
popd

log_step "PHASE 12: Final setup"
# Make sure this user owns the emacs directory.
sudo chown -R me:me ~/.emacs.d

# Link the prompts directory to VSCode User config
mkdir -p ~/.config/Code/User
rm -rf ~/.config/Code/User/prompts
ln -s /home/me/aesthetic-computer/prompts ~/.config/Code/User/prompts
log_ok "Prompts directory linked to VSCode User config"

# Link the modes directory to .github for VSCode chatmodes
rm -rf /home/me/aesthetic-computer/.github
ln -s /home/me/aesthetic-computer/modes /home/me/aesthetic-computer/.github
log_ok "Modes directory linked to .github for VSCode chatmodes"

# Source the devcontainer config to load AC development functions
source /workspaces/aesthetic-computer/.devcontainer/config.fish
log_ok "AC development functions loaded (ac-pack, ac-unpack, etc.)"

# Copy feed secrets from vault to environment (if vault exists)
if test -f /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env
    log_info "Loading feed system environment variables from vault..."
    # Copy to devcontainer envs directory for persistence
    cp /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env /home/me/envs/feed.env
    # Source it globally for the session
    for line in (cat /workspaces/aesthetic-computer/aesthetic-computer-vault/feed/.env | grep -v '^#' | grep '=')
        set -gx (string split '=' $line)
    end
    log_ok "Feed environment variables loaded"
else
    echo "⚠️  Feed vault not found - skipping feed environment setup"
end

# Set port 8888 to public in Codespaces (if applicable)
if test -f /workspaces/aesthetic-computer/.devcontainer/scripts/set-port-public.fish
    log_cmd fish /workspaces/aesthetic-computer/.devcontainer/scripts/set-port-public.fish
end

# Create .waiter file to signal container is ready (moved from postAttachCommand to avoid terminal popup in Codespaces)
sudo touch /home/me/.waiter
sudo chmod 777 /home/me/.waiter
sudo chmod +w /home/me
echo "✅ Container ready signal created (.waiter)"

# � Pre-start the emacs daemon so it's ready for the VS Code task
# The "💻 Aesthetic" task (runOn: folderOpen) may not always auto-start after
# devcontainer rebuilds. Pre-starting emacs here ensures it's always available
# so `ac-aesthetic` / `aesthetic-now` connects instantly when run manually.
log_step "PHASE 12b: Pre-starting emacs daemon"
set -l emacs_config /home/me/aesthetic-computer/dotfiles/dot_config/emacs.el
if test -f $emacs_config
    if not pgrep -f "emacs.*daemon" >/dev/null
        log_info "Starting emacs daemon in background..."
        set -l emacs_log_dir /home/me/.emacs-logs
        mkdir -p $emacs_log_dir 2>/dev/null
        set -l emacs_ts (date +%Y%m%d_%H%M%S)
        set -l emacs_log $emacs_log_dir/daemon_$emacs_ts.log
        command emacs -q --daemon -l $emacs_config > $emacs_log 2>&1 &
        disown
        # Wait briefly for daemon to become responsive
        set -l emacs_wait 0
        while test $emacs_wait -lt 30
            if timeout 3 emacsclient -e t >/dev/null 2>&1
                log_ok "Emacs daemon started and responsive"
                break
            end
            sleep 2
            set emacs_wait (math "$emacs_wait + 2")
        end
        if test $emacs_wait -ge 30
            log_warn "Emacs daemon started but not yet responsive (will continue in background)"
        end
    else
        log_ok "Emacs daemon already running"
    end
else
    log_warn "Emacs config not found at $emacs_config, skipping daemon start"
end

# �🩸 Setup SSH tunnel for CDP (Chrome DevTools Protocol) to host
# This allows artery-tui to control VS Code on the host machine
if test -n "$HOST_IP"; or test (uname -s) != "Linux"
    # Kill any existing CDP tunnels
    pkill -f "ssh.*9333.*host.docker.internal" 2>/dev/null
    
    # Create tunnel in background (port 9333 to avoid conflicts with svchost.exe on 9222)
    start_cdp_tunnel
    if test $status -eq 0
        echo "🩸 CDP tunnel established (localhost:9333 → host:9333)"
    else
        echo "⚠️  CDP tunnel failed (artery may not work)"
    end
end

# 🧊 Start TURN server for WebRTC relay (required for UDP in Docker)
if which turnserver >/dev/null 2>&1
    pkill -f "turnserver" 2>/dev/null
    sleep 0.5
    # Get host LAN IP from vault/machines.json or use default
    set -l HOST_LAN_IP "192.168.1.127"
    set -l HOST_LAN_IP_SOURCE "default"
    if test -f /workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json
        set -l detected_ip (cat /workspaces/aesthetic-computer/aesthetic-computer-vault/machines.json 2>/dev/null | jq -r '.["aesthetic-mac"].lan_ip // empty' 2>/dev/null)
        if test -n "$detected_ip"
            set HOST_LAN_IP $detected_ip
            set HOST_LAN_IP_SOURCE "vault"
        end
    end
    log_info "TURN host IP: $HOST_LAN_IP (source: $HOST_LAN_IP_SOURCE)"
    echo $HOST_LAN_IP > /tmp/host-lan-ip
    turnserver -c /workspaces/aesthetic-computer/.devcontainer/turnserver.conf --external-ip=$HOST_LAN_IP >> $ENTRY_LOG_REAL 2>&1 &
    disown
    log_ok "TURN server started (localhost:3478, external-ip: $HOST_LAN_IP)"
else
    log_warn "coturn not installed, UDP may not work across networks"
end

# echo "Initializing 📋 Clipboard Service" | lolcat -x -r

# ❤️‍🔥
# TODO: This may not be the best method because cdocker aesthetic needs to be
#       running in the user environment that has the clipboard.

# Run the isomorphic_copy clipboard on the host.
# Make sure the host allows ssh access from its own private key... `ssh-copy-id -i ~/.ssh/id_rsa.pub $USER@localhost`
# ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $HOST_USER@172.17.0.1

# ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $HOST_USER@172.17.0.1 "cdocker aesthetic"

# 🔍 Ensure Devcontainer Status Server is still running (it was started early)
log_info "Checking Devcontainer Status Server..."
if pgrep -f "devcontainer-status" >/dev/null
    log_ok "Status Server still running on http://127.0.0.1:7890"
else if test -f /workspaces/aesthetic-computer/artery/devcontainer-status.mjs
    log_warn "Status Server died, restarting..."
    nohup node /workspaces/aesthetic-computer/artery/devcontainer-status.mjs --server >/tmp/devcontainer-status.log 2>&1 &
    disown
    log_ok "Status Server restarted on http://127.0.0.1:7890"
end

log "════════════════════════════════════════════════════════════════"
log "🎉 ENTRY.FISH COMPLETED SUCCESSFULLY"
log "════════════════════════════════════════════════════════════════"
log_info "End time: "(date -Iseconds)
log_info "Log file: $ENTRY_LOG_REAL"

# Auto-launch ac-aesthetic if not already running
# Skip if AC_SKIP_AUTOLAUNCH is set (for manual control)
if not set -q AC_SKIP_AUTOLAUNCH
    if not pgrep -f "emacsclient.*aesthetic-backend" >/dev/null 2>&1
        log_step "Auto-launching aesthetic platform..."
        # Source config to load ac-aesthetic alias
        source /workspaces/aesthetic-computer/.devcontainer/config.fish
        # Run in background so entry.fish can complete
        nohup fish -c "sleep 2; aesthetic --no-wait" >/tmp/aesthetic-autolaunch.log 2>&1 &
        disown
        log_ok "Aesthetic platform auto-launch initiated"
    else
        log_info "Aesthetic platform already running, skipping auto-launch"
    end
else
    log_info "AC_SKIP_AUTOLAUNCH set, skipping auto-launch"
end
