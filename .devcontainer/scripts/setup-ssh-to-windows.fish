#!/usr/bin/env fish
# Setup SSH connection from devcontainer to Windows host

echo "🪟 Setting up SSH to Windows host..."
echo ""

# Detect Windows host IP (usually the Docker gateway)
set WINDOWS_IP (ip route | grep default | awk '{print $3}')
echo "Windows host IP: $WINDOWS_IP"

# Check if SSH keys exist
if not test -f ~/.ssh/id_ed25519
    and not test -f ~/.ssh/id_rsa
    echo ""
    echo "📝 No SSH key found. Generating one..."
    ssh-keygen -t ed25519 -C "devcontainer@aesthetic-computer" -f ~/.ssh/id_ed25519 -N ""
    echo "✅ SSH key generated!"
end

# Determine which key to use
set SSH_KEY_FILE ""
if test -f ~/.ssh/id_ed25519.pub
    set SSH_KEY_FILE ~/.ssh/id_ed25519.pub
else if test -f ~/.ssh/id_rsa.pub
    set SSH_KEY_FILE ~/.ssh/id_rsa.pub
end

echo ""
echo "📋 Your public key:"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
cat $SSH_KEY_FILE
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

echo ""
echo "📝 Next steps on Windows (run PowerShell as Administrator):"
echo ""
echo "1. Run the setup script:"
echo "   powershell -ExecutionPolicy Bypass -File (wslpath -w /workspaces/aesthetic-computer/.devcontainer/scripts/setup-windows-ssh.ps1)"
echo ""
echo "2. Or manually:"
echo "   - Make sure OpenSSH Server is installed and running"
echo "   - Create C:\\ProgramData\\ssh\\administrators_authorized_keys"
echo "   - Add the public key above to that file"
echo "   - Set permissions (see setup-windows-ssh.ps1)"
echo ""

# Create SSH config entry
set SSH_CONFIG ~/.ssh/config
echo ""
echo "🔧 Adding SSH config for Windows host..."

# Check if entry already exists
if grep -q "Host windows-host" $SSH_CONFIG 2>/dev/null
    echo "⚠️  SSH config entry already exists, skipping..."
else
    # Create config file if it doesn't exist
    touch $SSH_CONFIG
    chmod 600 $SSH_CONFIG

    # Add Windows host entry
    echo "" >> $SSH_CONFIG
    echo "# Windows Docker Host" >> $SSH_CONFIG
    echo "Host windows-host" >> $SSH_CONFIG
    echo "    HostName $WINDOWS_IP" >> $SSH_CONFIG
    echo "    User $USER" >> $SSH_CONFIG
    if test -f ~/.ssh/id_ed25519
        echo "    IdentityFile ~/.ssh/id_ed25519" >> $SSH_CONFIG
    else
        echo "    IdentityFile ~/.ssh/id_rsa" >> $SSH_CONFIG
    end
    echo "    IdentitiesOnly yes" >> $SSH_CONFIG
    echo "    StrictHostKeyChecking no" >> $SSH_CONFIG

    echo "✅ SSH config updated!"
end

echo ""
echo "🧪 Testing connection (this will fail until you complete Windows setup)..."
ssh -o ConnectTimeout=5 windows-host "echo 'Connected successfully!'" 2>&1
or echo "❌ Connection failed - complete Windows setup steps above first"

echo ""
echo "💡 After setup, you can connect with:"
echo "   ssh windows-host"
echo "   ssh windows-host 'dir C:\\'"
echo ""
echo "🧠 For Emacs MCP integration, you can use emacs_send_keys to execute:"
echo "   ssh windows-host 'powershell.exe -Command \"<your-command>\"'"
