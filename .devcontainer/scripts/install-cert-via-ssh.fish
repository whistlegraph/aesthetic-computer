#!/usr/bin/env fish
# Install SSL certificate on Windows host via SSH

echo "📜 Installing certificate on Windows host via SSH..."
echo ""

# Check if SSH is configured
if not ssh -o ConnectTimeout=3 windows-host "echo OK" >/dev/null 2>&1
    echo "❌ SSH to windows-host not configured"
    echo ""
    echo "Run this first: fish /workspaces/aesthetic-computer/.devcontainer/scripts/setup-ssh-to-windows.fish"
    echo "Then follow the Windows setup instructions"
    exit 1
end

# Copy certificate to Windows temp
echo "📋 Copying certificate to Windows..."
scp /workspaces/aesthetic-computer/ssl-dev/rootCA.pem windows-host:"%TEMP%\\mkcert-rootCA.pem"

# Install certificate
echo "📜 Installing certificate in Windows Trust Store..."
ssh windows-host 'powershell.exe -Command "Import-Certificate -FilePath \"$env:TEMP\\mkcert-rootCA.pem\" -CertStoreLocation Cert:\\LocalMachine\\Root; Remove-Item \"$env:TEMP\\mkcert-rootCA.pem\""'

echo ""
echo "✅ Certificate installed on Windows!"
echo ""
echo "⚠️  Please RESTART your browser (fully close Chrome and reopen)"
