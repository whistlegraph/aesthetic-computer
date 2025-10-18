#!/bin/bash
# Setup GNOME Connections for UE5 Builder
# GNOME Connections is the modern, native RDP client for GNOME

set -e

# Load credentials from vault
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -f "$SCRIPT_DIR/load-secrets.sh" ]; then
    source "$SCRIPT_DIR/load-secrets.sh"
else
    echo "❌ Error: Cannot find load-secrets.sh"
    echo "Please ensure aesthetic-computer-vault is configured"
    exit 1
fi

echo "=== Setting up GNOME Connections for UE5 Builder ==="
echo ""

# Check if GNOME Connections is installed
if command -v gnome-connections &> /dev/null; then
    echo "✓ GNOME Connections is already installed"
else
    echo "Installing GNOME Connections..."
    sudo dnf install -y gnome-connections
    echo "✓ GNOME Connections installed"
fi

echo ""
echo "=== GNOME Connections Setup Complete! ==="
echo ""
echo "📱 To Connect:"
echo ""
echo "1. Open 'Connections' app (search in Activities)"
echo "2. Click '+' (New Connection)"
echo "3. Select 'RDP' (Remote Desktop)"
echo "4. Enter details:"
echo "   Address: $VM_IP"
echo "   Username: $VM_USER"
echo "   Password: (from vault)"
echo "5. Click 'Connect'"
echo ""
echo "Or launch directly:"
echo "  gnome-connections rdp://$VM_IP"
echo ""
echo "💡 Tips:"
echo "  - Save the connection for quick access"
echo "  - Enable 'Fullscreen' for best experience"
echo "  - GNOME Connections is simpler than Remmina!"
echo ""
echo "Password is in: aesthetic-computer-vault/false.work/ue5-builder.env"
