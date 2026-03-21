#!/usr/bin/env fish
# sideload.fish - Sideload a .pdx to Playdate via SSH to host machine
# Usage: ./sideload.fish [game.pdx] [--eject]
#        ./sideload.fish --eject-only
#
# Requires:
# - Playdate in Data Disk mode and mounted on host
# - SSH access to host machine
# - Host credentials in aesthetic-computer-vault/playdate/host-ssh.fish

set -l SCRIPT_DIR (dirname (realpath (status filename)))
set -l VAULT_DIR "$SCRIPT_DIR/../aesthetic-computer-vault/playdate"
set -l BUILD_DIR "$SCRIPT_DIR/build"

# Default host IP (docker gateway to host)
set -l DEFAULT_HOST_IP "172.17.0.1"

# Load credentials from vault if available
set -l HOST_IP $DEFAULT_HOST_IP
set -l HOST_USER "jas"
set -l PLAYDATE_MOUNT "/run/media/jas/PLAYDATE"

if test -f "$VAULT_DIR/host-ssh.fish"
    source "$VAULT_DIR/host-ssh.fish"
    echo "🔐 Loaded credentials from vault"
else
    echo "⚠️  No vault credentials found, using defaults"
    echo "   Create $VAULT_DIR/host-ssh.fish to customize"
end

# Helper function to eject Playdate
function eject_playdate
    echo "⏏️  Ejecting Playdate..."
    # Use gio for GNOME/Fedora - cleanest way to eject
    ssh "$HOST_USER@$HOST_IP" "sync && gio mount -e '$PLAYDATE_MOUNT' 2>/dev/null || eject /dev/disk/by-label/PLAYDATE 2>/dev/null || udisksctl unmount -b /dev/disk/by-label/PLAYDATE"
    if test $status -eq 0
        echo "✅ Ejected! Safe to unplug or reboot Playdate."
        return 0
    else
        echo "⚠️  Eject may have issues - check device manually"
        return 1
    end
end

# Eject-only mode
if contains -- "--eject-only" $argv
    echo "🎮 KidLisp Playdate Eject"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "🖥️  Host: $HOST_USER@$HOST_IP"
    echo ""
    eject_playdate
    exit $status
end

# Determine which .pdx to sideload
set -l PDX_PATH ""
if test (count $argv) -ge 1
    set PDX_PATH $argv[1]
else
    # Find most recent .pdx in build directory
    set PDX_PATH (find $BUILD_DIR -maxdepth 1 -name "*.pdx" -type d | head -1)
end

if test -z "$PDX_PATH" -o ! -d "$PDX_PATH"
    echo "❌ No .pdx found!"
    echo ""
    echo "Usage: ./sideload.fish [path/to/game.pdx]"
    echo ""
    echo "Build a game first with:"
    echo "  fish build.fish examples/hello.lisp"
    exit 1
end

set -l PDX_NAME (basename $PDX_PATH)

echo "🎮 KidLisp Playdate Sideloader"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📦 Game: $PDX_NAME"
echo "🖥️  Host: $HOST_USER@$HOST_IP"
echo "💾 Mount: $PLAYDATE_MOUNT"
echo ""

# Check SSH connectivity
echo "🔗 Checking SSH connection..."
if not ssh -o ConnectTimeout=5 -o BatchMode=yes "$HOST_USER@$HOST_IP" "echo ok" 2>/dev/null
    echo "❌ Cannot SSH to host!"
    echo ""
    echo "Make sure:"
    echo "  1. SSH server is running on host (sudo systemctl start sshd)"
    echo "  2. SSH key is set up (ssh-copy-id $HOST_USER@$HOST_IP)"
    echo "  3. Host IP is correct ($HOST_IP)"
    echo ""
    echo "Or set up credentials in: $VAULT_DIR/host-ssh.fish"
    exit 1
end
echo "✅ SSH connection OK"

# Check if Playdate is mounted
echo "🔍 Checking Playdate mount..."
set -l MOUNT_CHECK (ssh "$HOST_USER@$HOST_IP" "test -d '$PLAYDATE_MOUNT/Games' && echo 'mounted' || echo 'not_mounted'" 2>/dev/null)

if test "$MOUNT_CHECK" != "mounted"
    echo "❌ Playdate not mounted at $PLAYDATE_MOUNT"
    echo ""
    echo "Make sure:"
    echo "  1. Playdate is in Data Disk mode"
    echo "     (Settings → System → Reboot to Data Disk)"
    echo "  2. It's mounted on the host"
    echo ""
    
    # Try to find it
    echo "🔍 Searching for Playdate mount..."
    set -l FOUND_MOUNT (ssh "$HOST_USER@$HOST_IP" "find /run/media -name 'PLAYDATE' -type d 2>/dev/null | head -1")
    if test -n "$FOUND_MOUNT"
        echo "   Found at: $FOUND_MOUNT"
        echo "   Update PLAYDATE_MOUNT in vault credentials"
    else
        echo "   Playdate not found. Is it plugged in and in Data Disk mode?"
    end
    exit 1
end
echo "✅ Playdate mounted"

# Copy the .pdx
echo "📤 Copying $PDX_NAME to Playdate..."
scp -r "$PDX_PATH" "$HOST_USER@$HOST_IP:$PLAYDATE_MOUNT/Games/"

if test $status -ne 0
    echo "❌ Copy failed!"
    exit 1
end

# Sync to ensure write completes
echo "💾 Syncing..."
ssh "$HOST_USER@$HOST_IP" "sync"

echo ""
echo "✅ Sideload complete!"
echo ""
echo "📱 You can now:"
echo "   1. Eject Playdate safely (or run: ./sideload.fish --eject)"
echo "   2. Reboot Playdate to normal mode"
echo "   3. Find '$PDX_NAME' in your games list!"
echo ""

# Optional: eject after sideload
if contains -- "--eject" $argv
    eject_playdate
end
