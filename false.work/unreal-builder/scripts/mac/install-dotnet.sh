#!/bin/bash
# Install .NET 8 SDK on Mac (arm64)
set -e

echo "========================================="
echo "Installing .NET 8 SDK for macOS arm64"
echo "========================================="
echo ""

# Download .NET 8 SDK for macOS arm64
DOTNET_VERSION="8.0.11"
DOWNLOAD_URL="https://download.visualstudio.microsoft.com/download/pr/c29f1f61-e6b9-4f93-a5e1-e046bbf0e110/d9a23e0ca0ce715a8e8d52fe7e07dbdf/dotnet-sdk-8.0.111-osx-arm64.pkg"
PKG_FILE="$HOME/Downloads/dotnet-sdk-8.0.111-osx-arm64.pkg"

echo "📥 Downloading .NET SDK..."
cd ~/Downloads
curl -L -o "$PKG_FILE" "$DOWNLOAD_URL"

echo ""
echo "📦 Installing .NET SDK (requires password)..."
sudo installer -pkg "$PKG_FILE" -target /

echo ""
echo "🔗 Setting up symbolic links..."
sudo ln -sf /usr/local/share/dotnet/dotnet /usr/local/bin/dotnet

echo ""
echo "✅ .NET SDK installed!"
echo ""
echo "Verifying installation..."
/usr/local/share/dotnet/dotnet --version

echo ""
echo "🎉 Done! .NET is ready."
