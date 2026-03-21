#!/usr/bin/env fish
# help tunnel — keep an SSH reverse tunnel to help.aesthetic.computer
#
# Run on your macbook to expose NanoClaw's HTTP Help channel (:3004)
# on the help droplet as localhost:3005.
#
# Caddy on the droplet proxies help.aesthetic.computer → :3004 (proxy server) → :3005 (this tunnel)
#
# Usage:
#   fish tunnel.fish              # Start tunnel (foreground)
#   fish tunnel.fish --background # Start tunnel in background with autossh

set SCRIPT_DIR (dirname (status --current-filename))
set VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault"
set SSH_KEY "$VAULT_DIR/home/.ssh/id_rsa"
set HELP_HOST "help.aesthetic.computer"
set LOCAL_PORT 3004    # NanoClaw HTTP Help port on macbook
set REMOTE_PORT 3005   # Exposed on droplet via tunnel

if not test -f $SSH_KEY
    echo "SSH key not found: $SSH_KEY"
    echo "Using default SSH key"
    set SSH_KEY ""
end

set SSH_OPTS -o StrictHostKeyChecking=no -o ServerAliveInterval=30 -o ServerAliveCountMax=3 -o ExitOnForwardFailure=yes

if contains -- --background $argv
    echo "Starting tunnel in background (autossh)..."
    if command -q autossh
        set -x AUTOSSH_POLL 30
        if test -n "$SSH_KEY"
            autossh -M 0 -f -N -R $REMOTE_PORT:localhost:$LOCAL_PORT $SSH_OPTS -i $SSH_KEY root@$HELP_HOST
        else
            autossh -M 0 -f -N -R $REMOTE_PORT:localhost:$LOCAL_PORT $SSH_OPTS root@$HELP_HOST
        end
        echo "Tunnel running. Check: ssh root@$HELP_HOST curl -s localhost:$REMOTE_PORT/health"
    else
        echo "autossh not found. Install: brew install autossh"
        exit 1
    end
else
    echo "Starting SSH tunnel (Ctrl+C to stop)..."
    echo "  macbook :$LOCAL_PORT → droplet :$REMOTE_PORT"
    echo ""
    if test -n "$SSH_KEY"
        ssh -N -R $REMOTE_PORT:localhost:$LOCAL_PORT $SSH_OPTS -i $SSH_KEY root@$HELP_HOST
    else
        ssh -N -R $REMOTE_PORT:localhost:$LOCAL_PORT $SSH_OPTS root@$HELP_HOST
    end
end
