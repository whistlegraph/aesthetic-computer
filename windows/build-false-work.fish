#!/usr/bin/env fish
# Call the Windows build script from WSL2

# Get the Windows path to the script
set WINDOWS_SCRIPT_PATH (wslpath -w /workspaces/aesthetic-computer/windows/build-false-work.ps1)

echo "🔨 Triggering Windows build on host machine..."
echo ""

# Run PowerShell on Windows host
powershell.exe -ExecutionPolicy Bypass -File "$WINDOWS_SCRIPT_PATH" $argv

if test $status -eq 0
    echo ""
    echo "✅ Build completed successfully!"
else
    echo ""
    echo "❌ Build failed with exit code: $status"
    exit $status
end
