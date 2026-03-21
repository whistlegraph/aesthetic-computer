# Auto-restart wrapper for Electron app on Windows host
# This runs on the HOST machine, not in the devcontainer
# Usage: .\restart-electron.ps1

Set-Location $PSScriptRoot

Write-Host "🎨 Aesthetic Computer - Auto-restart wrapper" -ForegroundColor Cyan
Write-Host "→ Working directory: $(Get-Location)" -ForegroundColor Gray
Write-Host "→ Press Ctrl+C to stop" -ForegroundColor Gray
Write-Host ""

$restartCount = 0

while ($true) {
    if ($restartCount -gt 0) {
        Write-Host "♻️  Restarting Electron (restart #$restartCount)..." -ForegroundColor Yellow
        Start-Sleep -Seconds 1
    }
    
    npm start
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 42) {
        # Exit code 42 = intentional reboot request
        $restartCount++
        Write-Host ""
        Write-Host "✓ Reboot request received" -ForegroundColor Green
    } else {
        # Any other exit code = stop
        Write-Host ""
        Write-Host "✓ Electron exited with code $exitCode" -ForegroundColor Green
        break
    }
}

Write-Host "👋 Goodbye!" -ForegroundColor Cyan
