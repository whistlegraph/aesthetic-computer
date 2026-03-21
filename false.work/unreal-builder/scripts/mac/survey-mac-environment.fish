#!/usr/bin/env fish
# Survey Mac Builder Environment
# Connects to Mac host and gathers UE5 + Perforce info
# Note: Password is stored in aesthetic-computer-vault/false.work/mac-builder-credentials.env

set MAC_USER falsework
set MAC_HOST host.docker.internal

# Load credentials from vault
set VAULT_FILE "/workspaces/aesthetic-computer/aesthetic-computer-vault/false.work/mac-builder-credentials.env"
if test -f $VAULT_FILE
    source $VAULT_FILE
    set SSHPASS $MAC_PASSWORD
else
    echo "❌ Error: Credentials file not found at $VAULT_FILE"
    echo "   Password should be stored there for security."
    exit 1
end

echo "🔍 Surveying Mac Builder Environment..."
echo ""

# Use sshpass if available, otherwise manual password entry
if type -q sshpass
    echo "✓ Using sshpass for authentication"
    set -x SSHPASS $SSHPASS
    set SSH_PREFIX "sshpass -e"
else
    echo "💡 Tip: Install sshpass to avoid password prompts"
    echo "   sudo dnf install sshpass"
    echo ""
    set SSH_PREFIX ""
end

echo "=== System Information ==="
eval $SSH_PREFIX ssh -o StrictHostKeyChecking=no $MAC_USER@$MAC_HOST 'hostname && sw_vers && uname -m && sysctl -n machdep.cpu.brand_string && echo "Cores: $(sysctl -n hw.ncpu) (Physical: $(sysctl -n hw.physicalcpu))" && echo "RAM: $(($(sysctl -n hw.memsize) / 1024 / 1024 / 1024)) GB"'

echo ""
echo "=== Disk Space ==="
eval $SSH_CMD "df -h / | tail -1"

echo ""
echo "=== Unreal Engine ==="
eval $SSH_CMD "
    if test -d '/Users/Shared/Epic Games'; then
        echo 'UE Installation found in /Users/Shared/Epic Games:'
        ls -d '/Users/Shared/Epic Games'/UE_* 2>/dev/null || echo '  No UE_* versions found'
    end
    
    if test -d ~/Library/Application\ Support/Epic; then
        echo 'Epic config found in ~/Library/Application Support/Epic'
        ls ~/Library/Application\ Support/Epic 2>/dev/null
    end
    
    if test -d /Applications/Epic\ Games\ Launcher.app; then
        echo '✓ Epic Games Launcher installed'
    end
"

echo ""
echo "=== Perforce ==="
eval $SSH_CMD "
    if which p4 >/dev/null 2>&1; then
        echo '✓ P4 installed:' \$(which p4)
        p4 -V 2>/dev/null | head -3
        echo ''
        echo 'P4 Environment:'
        p4 set | grep -E 'P4PORT|P4USER|P4CLIENT' || echo '  (not configured)'
    else
        echo '✗ P4 not found in PATH'
        echo 'Checking common locations...'
        test -f /usr/local/bin/p4 && echo '  Found: /usr/local/bin/p4'
        test -f /opt/homebrew/bin/p4 && echo '  Found: /opt/homebrew/bin/p4'
    end
"

echo ""
echo "=== Xcode ==="
eval $SSH_CMD "
    if xcode-select -p >/dev/null 2>&1; then
        echo '✓ Xcode Command Line Tools:' \$(xcode-select -p)
        if which xcodebuild >/dev/null 2>&1; then
            xcodebuild -version 2>/dev/null
        end
    else
        echo '✗ Xcode Command Line Tools not installed'
    end
"

echo ""
echo "=== Home Directory ==="
eval $SSH_CMD "
    echo 'Home:' ~
    echo 'User:' \$(whoami)
    echo ''
    echo 'Contents of ~:'
    ls -la ~ | head -20
"

echo ""
echo "✅ Survey complete!"
echo ""
echo "💡 Next steps:"
echo "   1. Locate UE5 installation path"
echo "   2. Configure Perforce workspace"
echo "   3. Test build scripts"
