#!/bin/bash
# Simple RDP connector for false.work UE5 Builder
# Works from dev container or host

# Load secrets from vault if available
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -f "$SCRIPT_DIR/load-secrets.sh" ]; then
    source "$SCRIPT_DIR/load-secrets.sh" 2>/dev/null || true
fi

# Check if required variables are set
if [ -z "$VM_IP" ] || [ -z "$VM_USER" ]; then
    echo "❌ Error: VM credentials not found!"
    echo "Please ensure aesthetic-computer-vault is configured with:"
    echo "  - VM_IP"
    echo "  - VM_USER"
    echo "  - VM_PASSWORD"
    echo ""
    echo "Run: source scripts/load-secrets.sh"
    exit 1
fi

echo "=== Connecting to UE5 Builder ==="
echo "IP: $VM_IP"
echo "User: $VM_USER"
echo ""

# Check if we're in dev container
if [ -f "/.dockerenv" ] || [ -n "$REMOTE_CONTAINERS" ]; then
    echo "📦 Running in dev container"
    echo ""
    echo "To connect from your host machine, use one of these:"
    echo ""
    echo "🪟 Windows:"
    echo "  mstsc /v:$VM_IP"
    echo ""
    echo "🍎 Mac:"
    echo "  open rdp://$VM_USER@$VM_IP"
    echo "  Or use Microsoft Remote Desktop app"
    echo ""
    echo "🐧 Linux:"
    echo "  remmina -c rdp://$VM_USER@$VM_IP"
    echo "  Or: xfreerdp /v:$VM_IP /u:$VM_USER /cert:ignore"
    echo ""
    echo "Password: Check aesthetic-computer-vault/false.work/ue5-builder.env"
    exit 0
fi

# Not in container, try to connect
if command -v gnome-connections &> /dev/null; then
    echo "🚀 Launching GNOME Connections..."
    gnome-connections "rdp://$VM_IP" &
elif command -v remmina &> /dev/null; then
    echo "🚀 Launching Remmina..."
    remmina -c "rdp://$VM_USER@$VM_IP" &
elif command -v xfreerdp &> /dev/null; then
    echo "🚀 Launching xfreerdp..."
    xfreerdp /v:$VM_IP /u:$VM_USER /cert:ignore /scale:140
elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "🚀 Opening with default RDP client..."
    open "rdp://$VM_USER@$VM_IP"
else
    echo "❌ No RDP client found!"
    echo ""
    echo "Install one:"
    echo "  Fedora/GNOME: sudo dnf install gnome-connections"
    echo "  Fedora/Other: sudo dnf install remmina remmina-plugins-rdp"
    echo "  Ubuntu: sudo apt install remmina remmina-plugin-rdp"
    echo "  Arch: sudo pacman -S gnome-connections"
    exit 1
fi
