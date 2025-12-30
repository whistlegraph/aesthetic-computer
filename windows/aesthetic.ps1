<#
.SYNOPSIS
    Aesthetic Computer Platform Launcher for Windows

.DESCRIPTION
    Starts the Aesthetic Computer devcontainer with VS Code CDP debugging enabled.
    Sets up HOST_IP for WebRTC, launches devcontainer, and starts clipboard listener.

.NOTES
    To run this script by just typing `aesthetic` in PowerShell:

    1. Open your PowerShell profile:
       notepad $PROFILE

    2. Add the following function:

       function aesthetic {
           powershell.exe -ExecutionPolicy Bypass -File "\\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows\aesthetic.ps1"
       }

    3. Save the file, then restart your PowerShell session.
#>

$ErrorActionPreference = "Stop"
$workspace = "\\wsl.localhost\Ubuntu\home\me\aesthetic-computer"

# ═══════════════════════════════════════════════════════════════════════════════
# Banner
# ═══════════════════════════════════════════════════════════════════════════════
Write-Host @"

  ╔═══════════════════════════════════════════════════════════════╗
  ║                                                               ║
  ║   █▀█ █▀▀ █▀ ▀█▀ █ █ █▀▀ ▀█▀ █ █▀▀   █▀▀ █▀█ █▄▀▄█ █▀█       ║
  ║   █▀█ ██▄ ▄█  █  █▀█ ██▄  █  █ █▄▄ ▄ █▄▄ █▄█ █ ▀ █ █▀▀       ║
  ║                                                               ║
  ╚═══════════════════════════════════════════════════════════════╝

"@ -ForegroundColor Magenta

# ═══════════════════════════════════════════════════════════════════════════════
# Detect HOST_IP (for WebRTC TURN server)
# ═══════════════════════════════════════════════════════════════════════════════
Write-Host "› Detecting HOST_IP..." -ForegroundColor Magenta
$hostIp = (Get-NetIPAddress -AddressFamily IPv4 `
           | Where-Object { $_.InterfaceAlias -match 'Wi-Fi|Ethernet' -and $_.PrefixOrigin -eq 'Dhcp' }).IPAddress | Select-Object -First 1

if ($hostIp) {
    $env:HOST_IP = $hostIp
    "HOST_IP=$hostIp" | Out-File -FilePath "$workspace\.devcontainer\envs\host.env" -Encoding ASCII -NoNewline
    Write-Host "✓ HOST_IP is $hostIp" -ForegroundColor Green
} else {
    Write-Host "! Could not detect HOST_IP (WebRTC may not work)" -ForegroundColor Yellow
}

# ═══════════════════════════════════════════════════════════════════════════════
# Kill existing VS Code instances
# ═══════════════════════════════════════════════════════════════════════════════
Write-Host "› Stopping VS Code..." -ForegroundColor Magenta
Get-Process Code -ErrorAction SilentlyContinue | ForEach-Object {
    try {
        $_.CloseMainWindow() | Out-Null
        Start-Sleep -Milliseconds 500
        if (!$_.HasExited) { $_.Kill() }
    } catch {}
}
Start-Sleep -Seconds 1
Write-Host "✓ VS Code stopped" -ForegroundColor Green

# ═══════════════════════════════════════════════════════════════════════════════
# Remove old container (optional cleanup)
# ═══════════════════════════════════════════════════════════════════════════════
Write-Host "› Cleaning up old container..." -ForegroundColor Magenta
$ErrorActionPreference = "SilentlyContinue"
docker rm -f aesthetic 2>&1 | Out-Null
$ErrorActionPreference = "Stop"
Write-Host "✓ Cleanup done" -ForegroundColor Green

# ═══════════════════════════════════════════════════════════════════════════════
# Launch VS Code with CDP debugging
# ═══════════════════════════════════════════════════════════════════════════════
# VS Code will automatically start the devcontainer when opening with dev-container+ URI
Write-Host "› Launching VS Code with CDP on port 9333..." -ForegroundColor Magenta

# Use dev-container+ URI format (hex-encoded workspace path)
$hexPath = [System.BitConverter]::ToString([System.Text.Encoding]::UTF8.GetBytes("/home/me/aesthetic-computer")).Replace("-","").ToLower()
$uri = "vscode-remote://dev-container+$hexPath/workspaces/aesthetic-computer"
Write-Host "  URI: $uri" -ForegroundColor Gray

# Launch VS Code (use & to invoke directly, which worked in testing)
& code --folder-uri "$uri" --remote-debugging-port=9333 --disable-extension github.copilot-chat --disable-extension github.copilot

Write-Host "✓ VS Code launched" -ForegroundColor Green

# ═══════════════════════════════════════════════════════════════════════════════
# Clipboard listener (receives from container via netcat)
# ═══════════════════════════════════════════════════════════════════════════════
Write-Host ""
Write-Host "  ✨ aesthetic.computer ready" -ForegroundColor Magenta
Write-Host ""
Write-Host "› Starting clipboard listener on port 12345..." -ForegroundColor Gray

while ($true) {
    try {
        ncat -l -p 12345 | Set-Clipboard
        Start-Sleep -Milliseconds 200
    } catch {
        Start-Sleep -Seconds 1
    }
}
