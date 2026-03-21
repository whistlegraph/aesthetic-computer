#!/usr/bin/env fish
# Setup SSH access for session server deployment
# Run this script to copy the SSH key and configure SSH

set SSH_KEY_SOURCE "/workspaces/aesthetic-computer/aesthetic-computer-vault/session-server/session_server"
set SSH_KEY_DEST "$HOME/.ssh/session_server"

echo "🔑 Setting up session server SSH access..."
echo ""

# Copy SSH key to ~/.ssh
if test -f $SSH_KEY_SOURCE
    cp $SSH_KEY_SOURCE "$SSH_KEY_DEST"
    cp "$SSH_KEY_SOURCE.pub" "$SSH_KEY_DEST.pub"
    chmod 600 $SSH_KEY_DEST
    chmod 644 "$SSH_KEY_DEST.pub"
    echo "✅ Copied SSH key to $SSH_KEY_DEST"
else
    echo "❌ SSH key not found at $SSH_KEY_SOURCE"
    exit 1
end

# Update SSH config
set SSH_CONFIG "$HOME/.ssh/config"

# Check if session-server entry already exists
if grep -q "Host session-server" $SSH_CONFIG 2>/dev/null
    echo "✅ SSH config already has session-server entry"
else
    echo "" >> $SSH_CONFIG
    echo "# Session Server (session-server.aesthetic.computer)" >> $SSH_CONFIG
    echo "Host session-server" >> $SSH_CONFIG
    echo "    HostName 157.245.134.225" >> $SSH_CONFIG
    echo "    User root" >> $SSH_CONFIG
    echo "    IdentityFile ~/.ssh/session_server" >> $SSH_CONFIG
    echo "    IdentitiesOnly yes" >> $SSH_CONFIG
    echo "    StrictHostKeyChecking accept-new" >> $SSH_CONFIG
    echo "✅ Added session-server to SSH config"
end

echo ""
echo "🧪 Testing SSH connection..."
if ssh -o ConnectTimeout=5 session-server "echo 'SSH connection successful!'" 2>/dev/null
    echo "✅ SSH access configured successfully!"
else
    echo ""
    echo "⚠️  SSH connection failed. You need to add the public key to the server:"
    echo ""
    echo "Public key:"
    cat "$SSH_KEY_DEST.pub"
    echo ""
    echo "Add this key to the server with:"
    echo "  1. Log into the droplet console"
    echo "  2. Run: echo 'PASTE_PUBLIC_KEY_HERE' >> ~/.ssh/authorized_keys"
    echo "  3. Run this script again to verify"
    exit 1
end

echo ""
echo "🎉 Setup complete! You can now deploy with:"
echo "   cd /workspaces/aesthetic-computer/aesthetic-computer-vault/session-server"
echo "   ./deploy.fish"
