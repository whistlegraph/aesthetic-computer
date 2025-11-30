#!/usr/bin/env fish
# Remote build script - builds ac-vst on Mac via SSH
# Run from dev container: ./ac-vst/build-remote.fish

set -l script_dir (dirname (status -f))
set -l vault_env "$script_dir/../aesthetic-computer-vault/.env"

# Load SSH credentials from vault
if test -f $vault_env
    for line in (cat $vault_env | grep -v '^#' | grep '=')
        set -l kv (string split -m 1 '=' $line)
        if test (count $kv) -eq 2
            set -gx $kv[1] $kv[2]
        end
    end
end

if not set -q SSH_FRIEND_HOST; or not set -q SSH_FRIEND_USER; or not set -q SSH_FRIEND_PASSWORD
    echo "❌ SSH credentials not found in vault/.env"
    exit 1
end

set MAC_REPO "~/Desktop/code/aesthetic-computer"

echo "🎹 AC Notepat VST - Remote Build"
echo "================================="
echo "Host: $SSH_FRIEND_USER@$SSH_FRIEND_HOST"
echo "Repo: $MAC_REPO"
echo ""

# Test connection first
echo "🔗 Testing SSH connection..."
set -l test_result (sshpass -p "$SSH_FRIEND_PASSWORD" ssh -o StrictHostKeyChecking=no -o ConnectTimeout=5 $SSH_FRIEND_USER@$SSH_FRIEND_HOST "echo OK" 2>&1)

if test "$test_result" != "OK"
    echo "❌ SSH connection failed: $test_result"
    echo ""
    echo "Make sure:"
    echo "  1. Mac is on the same network (or use ngrok/tailscale)"
    echo "  2. Remote Login is enabled: System Preferences → Sharing → Remote Login"
    exit 1
end

echo "✅ Connected!"
echo ""

# Pull latest code on Mac
echo "📥 Pulling latest code on Mac..."
sshpass -p "$SSH_FRIEND_PASSWORD" ssh $SSH_FRIEND_USER@$SSH_FRIEND_HOST "cd $MAC_REPO && git pull"

# Run the build script
echo ""
echo "🏗️ Building on Mac..."
sshpass -p "$SSH_FRIEND_PASSWORD" ssh $SSH_FRIEND_USER@$SSH_FRIEND_HOST "cd $MAC_REPO/ac-vst && chmod +x build-mac.sh && ./build-mac.sh"

echo ""
echo "🎉 Remote build complete!"
