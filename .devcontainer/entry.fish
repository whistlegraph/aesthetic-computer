#!/usr/bin/env fish

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

# Call the function to set up Docker socket permissions
ensure_docker_socket_permissions

# Start Docker daemon in the background if not already running
if not pgrep -x dockerd >/dev/null
    echo "Starting Docker daemon..."
    sudo dockerd >/tmp/dockerd.log 2>&1 &
else
    echo "Docker daemon already running."
end

if test -d /home/me/envs
    source /home/me/envs/load_envs.fish
    load_envs # Load devcontainer envs conditionally.
end

set -gx TERM xterm-256color

# Start dbus-run-session for the keryring and execute the command passed to the script
# dbus-run-session -- sh -c "eval \$(echo \$DBUS_SESSION_BUS_ADDRESS); exec $argv"

# Send a welcome message!
toilet "Aesthetic Computer" -f future | lolcat -x -r

# Go to the user's directory.
cd /home/me

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

# Set merge strategy.
git config pull.rebase false

# Make sure git is setup and authorized for making commits via `gh`.
gh auth setup-git

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
    else
        cd /home/me/aesthetic-computer/aesthetic-computer-vault
        git pull
        sudo fish devault.fish
        echo "🔓 Vault mounted."
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
#if not test -f localhost.pem; or not test -f localhost-key.pem
# Generate SSL certificates
sudo fish ssl-install.fish --install-only
#end

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

# Check if node_modules directory exists and is not empty
if not test -d /home/me/aesthetic-computer/node_modules || not count /home/me/aesthetic-computer/node_modules/* >/dev/null
    # Move to the project directory
    cd /home/me/aesthetic-computer
    # Informing about the empty or missing node_modules directory
    echo "node_modules directory is empty or missing, running npm install."
    # Install the latest npm version
    npm install -g npm@latest --no-fund --no-audit
    npm ci --no-fund --no-audit
    # Run additional installation scripts
    npm run install:everything-else
else
    # Notify that installation steps are being skipped
    echo "node_modules directory is present and not empty, skipping npm install."
end

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

# Trigger the 'waiter' alias to boot the platform.
# touch /home/me/.waiter

# echo "Initializing 📋 Clipboard Service" | lolcat -x -r

# ❤️‍🔥
# TODO: This may not be the best method because cdocker aesthetic needs to be
#       running in the user environment that has the clipboard.

# Run the isomorphic_copy clipboard on the host.
# Make sure the host allows ssh access from its own private key... `ssh-copy-id -i ~/.ssh/id_rsa.pub $USER@localhost`
# ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $HOST_USER@172.17.0.1

# ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null $HOST_USER@172.17.0.1 "cdocker aesthetic"
