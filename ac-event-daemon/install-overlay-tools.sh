#!/bin/bash

# Install recommended overlay tools for ac-event-daemon

echo "🔧 Installing overlay tools for better notifications..."

# Detect package manager
if command -v dnf >/dev/null 2>&1; then
    PKG_MANAGER="dnf"
    INSTALL_CMD="sudo dnf install -y"
elif command -v apt >/dev/null 2>&1; then
    PKG_MANAGER="apt"
    INSTALL_CMD="sudo apt install -y"
elif command -v pacman >/dev/null 2>&1; then
    PKG_MANAGER="pacman"
    INSTALL_CMD="sudo pacman -S --noconfirm"
else
    echo "❌ Unsupported package manager. Please install manually:"
    echo "   - chromium (or google-chrome)"
    echo "   - zenity"
    echo "   - figlet"
    exit 1
fi

echo "📦 Using $PKG_MANAGER package manager..."

packages_to_install=""

# Check and install chromium
if ! command -v chromium >/dev/null 2>&1 && ! command -v chromium-browser >/dev/null 2>&1 && ! command -v google-chrome >/dev/null 2>&1; then
    echo "🌐 Adding chromium..."
    case $PKG_MANAGER in
        dnf) packages_to_install="$packages_to_install chromium" ;;
        apt) packages_to_install="$packages_to_install chromium-browser" ;;
        pacman) packages_to_install="$packages_to_install chromium" ;;
    esac
fi

# Check and install zenity
if ! command -v zenity >/dev/null 2>&1; then
    echo "💬 Adding zenity..."
    packages_to_install="$packages_to_install zenity"
fi

# Check and install figlet
if ! command -v figlet >/dev/null 2>&1; then
    echo "🔤 Adding figlet..."
    packages_to_install="$packages_to_install figlet"
fi

# Install packages if any are needed
if [ -n "$packages_to_install" ]; then
    echo "📥 Installing:$packages_to_install"
    $INSTALL_CMD $packages_to_install
    
    if [ $? -eq 0 ]; then
        echo "✅ Installation complete!"
    else
        echo "❌ Installation failed. You may need to run with sudo or install manually."
        exit 1
    fi
else
    echo "✅ All recommended tools are already installed!"
fi

echo ""
echo "🧪 Testing installed tools..."
./test-overlays.sh
