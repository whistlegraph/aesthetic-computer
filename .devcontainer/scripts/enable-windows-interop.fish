#!/usr/bin/env fish
# Enable Windows interop for calling PowerShell from WSL2

echo "🪟 Setting up Windows interop..."

# Check if running in WSL2
if not test -d /run/WSL
    echo "⚠️  Not running in WSL2, skipping Windows interop setup"
    exit 0
end

# Enable binfmt_misc if not already enabled
if not test -f /proc/sys/fs/binfmt_misc/WSLInterop
    echo "  Enabling WSL interop in binfmt_misc..."
    sudo sh -c 'echo :WSLInterop:M::MZ::/init:PF > /proc/sys/fs/binfmt_misc/register 2>/dev/null' || true
end

# Create symlinks for Windows executables if they exist
set windows_bins powershell.exe cmd.exe wsl.exe wslpath

for bin in $windows_bins
    if test -f /mnt/c/Windows/System32/$bin
        and not test -f /usr/local/bin/$bin
        echo "  Linking $bin..."
        sudo ln -sf /mnt/c/Windows/System32/$bin /usr/local/bin/$bin
    end
end

# Create symlink for PowerShell 7+ if available
if test -f "/mnt/c/Program Files/PowerShell/7/pwsh.exe"
    and not test -f /usr/local/bin/pwsh.exe
    echo "  Linking pwsh.exe (PowerShell 7+)..."
    sudo ln -sf "/mnt/c/Program Files/PowerShell/7/pwsh.exe" /usr/local/bin/pwsh.exe
end

# Test if it works
if type -q powershell.exe
    echo "✅ Windows interop enabled!"
    echo "  You can now run: powershell.exe, cmd.exe, etc."
else
    echo "⚠️  Windows interop setup complete but PowerShell not found"
    echo "  Make sure /mnt/c is properly mounted"
end
