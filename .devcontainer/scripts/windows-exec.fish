#!/usr/bin/env fish
# Execute commands on Windows host via SSH
# Usage: windows-exec.fish "powershell command here"

if test (count $argv) -eq 0
    echo "Usage: windows-exec.fish \"command\""
    echo ""
    echo "Examples:"
    echo "  windows-exec.fish 'dir C:\\'"
    echo "  windows-exec.fish 'powershell.exe -Command \"Get-Process\"'"
    echo "  windows-exec.fish 'powershell.exe -File C:\\path\\to\\script.ps1'"
    exit 1
end

# Execute command on Windows host
ssh windows-host $argv
