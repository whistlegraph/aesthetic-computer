#!/usr/bin/fish

# Build Aesthetic Computer Electron app for all platforms from devcontainer
# Uses Docker for cross-platform builds and vault credentials for signing

set script_dir (dirname (status -f))
set vault_dir /workspaces/aesthetic-computer/aesthetic-computer-vault/ac-electron

echo "🔨 Aesthetic Computer Multi-Platform Electron Builder"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Check if vault credentials exist
if not test -d $vault_dir
    echo "❌ Vault directory not found: $vault_dir"
    echo "   Create it and add signing certificates (see vault README)"
    exit 1
end

# Source vault environment if it exists
if test -f $vault_dir/.env
    echo "🔐 Loading credentials from vault..."
    set -a
    source $vault_dir/.env
    set +a
else
    echo "⚠️  No .env found in vault - builds will be unsigned"
    echo "   Copy .env.template to .env and fill in credentials for signed builds"
end

echo ""
echo "📦 Building packages:"
echo "   • Linux (AppImage, deb, rpm) - native build"
echo "   • Windows (exe) - via Docker wine container"
echo "   • macOS (dmg) - requires macOS host or CI/CD"
echo ""

# Change to ac-electron directory
cd $script_dir

# Clean previous builds
echo "🧹 Cleaning previous builds..."
rm -rf dist/
mkdir -p dist/

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🐧 Building Linux packages..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Linux - native build (we're on Linux in devcontainer)
npm run build:linux

if test $status -eq 0
    echo "✅ Linux builds complete"
    ls -lh dist/*.AppImage dist/*.deb dist/*.rpm 2>/dev/null || true
else
    echo "❌ Linux build failed"
end

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🪟 Building Windows package..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Windows - use electron-builder's wine Docker container
# This allows building Windows .exe from Linux
if command -v docker >/dev/null 2>&1
    echo "Using Docker wine container for Windows build..."

    # Copy vault credentials into build directory for Docker access
    if test -f $vault_dir/.env
        cp $vault_dir/.env .env.build
        if test -f $vault_dir/windows-code-signing.pfx
            cp $vault_dir/windows-code-signing.pfx ./
        end
    end

    docker run --rm \
        -v (pwd):/project \
        -v ~/.cache/electron:/root/.cache/electron \
        -v ~/.cache/electron-builder:/root/.cache/electron-builder \
        electronuserland/builder:wine \
        bash -c "cd /project && npm run build:win"

    # Cleanup temp files
    rm -f .env.build windows-code-signing.pfx

    if test $status -eq 0
        echo "✅ Windows build complete"
        ls -lh dist/*.exe 2>/dev/null || true
    else
        echo "❌ Windows build failed"
    end
else
    echo "⚠️  Docker not available - skipping Windows build"
    echo "   Install Docker to enable cross-platform Windows builds"
end

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🍎 macOS build..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# macOS - can't build from Linux without a Mac
echo "⚠️  macOS builds require a macOS host or GitHub Actions"
echo "   Options:"
echo "   1. Run this script on a Mac"
echo "   2. Use GitHub Actions (push tag to trigger workflow)"
echo "   3. Use a macOS VM or cloud build service"

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Build Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Built artifacts:"
ls -lh dist/ | grep -v "^d" | grep -v "^total" || echo "  No artifacts found"
echo ""

# Calculate total size
set total_size (du -sh dist/ | cut -f1)
echo "Total size: $total_size"
echo ""

# Show next steps
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Next Steps"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "To publish to GitHub releases:"
echo "  gh release create v(cat package.json | jq -r .version) \\"
echo "    --title 'v(cat package.json | jq -r .version)' \\"
echo "    --notes 'Release notes here' \\"
echo "    dist/*.AppImage dist/*.deb dist/*.rpm dist/*.exe"
echo ""
echo "Or for macOS-only release from a Mac:"
echo "  npm run publish:mac"
echo ""
