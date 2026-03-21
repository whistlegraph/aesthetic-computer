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
# Remove old container and start devcontainer
# ═══════════════════════════════════════════════════════════════════════════════
Write-Host "› Starting devcontainer..." -ForegroundColor Magenta

# Remove old container (in WSL)
wsl -d Ubuntu -e docker rm -f aesthetic 2>$null

# Start devcontainer via CLI (in WSL)
$devcontainerResult = wsl -d Ubuntu -e bash -c "cd /home/me/aesthetic-computer && devcontainer up --workspace-folder . 2>&1 | tail -3"
Write-Host $devcontainerResult

if ($LASTEXITCODE -eq 0) {
    Write-Host "✓ Devcontainer ready" -ForegroundColor Green
} else {
    Write-Host "✗ Devcontainer failed to start" -ForegroundColor Red
    exit 1
}

# ═══════════════════════════════════════════════════════════════════════════════
# Launch VS Code with CDP debugging
# ═══════════════════════════════════════════════════════════════════════════════
Write-Host "› Launching VS Code with CDP on port 9333..." -ForegroundColor Magenta

# Use dev-container+ URI format (hex-encoded workspace path)
$hexPath = [System.BitConverter]::ToString([System.Text.Encoding]::UTF8.GetBytes("/home/me/aesthetic-computer")).Replace("-","").ToLower()
$uri = "vscode-remote://dev-container+$hexPath/workspaces/aesthetic-computer"

Start-Process -WindowStyle Hidden -FilePath "code" -ArgumentList `
    "--folder-uri", $uri, `
    "--remote-debugging-port=9333", `
    "--disable-extension", "github.copilot-chat", `
    "--disable-extension", "github.copilot"

Write-Host "✓ VS Code launched" -ForegroundColor Green

# ═══════════════════════════════════════════════════════════════════════════════
# Install SSL certificate (if needed)
# ═══════════════════════════════════════════════════════════════════════════════
Write-Host "› Checking SSL certificate..." -ForegroundColor Magenta

# Check if mkcert root CA is already trusted
$certExists = Get-ChildItem -Path Cert:\LocalMachine\Root | Where-Object { $_.Subject -match "mkcert.*@aesthetic" }

if (!$certExists) {
    Write-Host "  ! SSL certificate not installed" -ForegroundColor Yellow
    Write-Host "  › Installing SSL certificate from container..." -ForegroundColor Magenta

    try {
        # Copy certificate from container
        $tempCert = "$env:TEMP\mkcert-rootCA.pem"
        wsl -d Ubuntu -e docker cp aesthetic:/workspaces/aesthetic-computer/ssl-dev/rootCA.pem /tmp/rootCA.pem 2>$null
        wsl -d Ubuntu -e cat /tmp/rootCA.pem | Out-File -FilePath $tempCert -Encoding ASCII

        if (Test-Path $tempCert) {
            Import-Certificate -FilePath $tempCert -CertStoreLocation Cert:\LocalMachine\Root | Out-Null
            Remove-Item $tempCert
            Write-Host "  ✓ SSL certificate installed (restart browser to apply)" -ForegroundColor Green
        } else {
            Write-Host "  ! Could not copy certificate from container" -ForegroundColor Yellow
        }
    } catch {
        Write-Host "  ! Certificate installation failed: $_" -ForegroundColor Yellow
    }
} else {
    Write-Host "  ✓ SSL certificate already trusted" -ForegroundColor Green
}

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
