<#
To run this script by just typing `aesthetic` in PowerShell:

1. Open your PowerShell profile:
   notepad $PROFILE

2. Add the following function:

   function aesthetic {
       powershell.exe -ExecutionPolicy Bypass -File "\\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows\aesthetic.ps1"
   }

3. Save the file, then restart your PowerShell session.

Now, you can just type `start` to launch the Aesthetic Computer Platform.
#>

Write-Host @"
Aesthetic Computer Platform is starting...
"@ -ForegroundColor Cyan

# Detect and export HOST_IP
$hostIp = (Get-NetIPAddress -AddressFamily IPv4 `
           | Where-Object { $_.InterfaceAlias -match 'Wi-Fi' -and $_.PrefixOrigin -eq 'Dhcp' }).IPAddress

if ($hostIp) {
    $env:HOST_IP = $hostIp
    "HOST_IP=$hostIp" | Out-File -FilePath "$PSScriptRoot\..\.devcontainer\envs\host.env" -Encoding ASCII -NoNewline
    Write-Host "Local HOST_IP is $hostIp" -ForegroundColor Green
} else {
    Write-Host "Could not detect HOST_IP" -ForegroundColor Yellow
}


# Kill all running VS Code instances
Write-Host "Closing all running VS Code instances..." -ForegroundColor Magenta
Get-Process Code -ErrorAction SilentlyContinue | ForEach-Object {
    if (!$_.HasExited) {
        try {
            $_.CloseMainWindow() | Out-Null
            Start-Sleep -Milliseconds 500
            if (!$_.HasExited) {
                $_.Kill()
            }
        } catch {
            Write-Host "  Could not close process $($_.Id): $($_.Exception.Message)" -ForegroundColor DarkYellow
        }
    }
}

# Launch new VS Code instance in current directory
Write-Host "Launching fresh VS Code inside Dev Container..."
Start-Process -WindowStyle Hidden -FilePath "code" -ArgumentList "--folder", "\\wsl.localhost\Ubuntu\home\me\aesthetic-computer"

# Clipboard loop
while ($true) {
    Write-Host "[clipboard] Waiting for input on port 12345..."
    ncat -l -p 12345 | Set-Clipboard
    Start-Sleep -Milliseconds 200
}
