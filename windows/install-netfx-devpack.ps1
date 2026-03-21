# Quick install .NET Framework 4.8 Developer Pack using winget
# Run as Administrator

#Requires -RunAsAdministrator

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Installing .NET Framework 4.8 Dev Pack" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

# Check if already installed
$netfxRefPath = "C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.8"
if (Test-Path $netfxRefPath) {
    Write-Host "[OK] .NET Framework 4.8 Developer Pack already installed" -ForegroundColor Green
    exit 0
}

# Try winget first
if (Get-Command winget -ErrorAction SilentlyContinue) {
    Write-Host "Installing via winget..." -ForegroundColor Yellow
    winget install --id Microsoft.DotNet.Framework.DeveloperPack_4 --silent --accept-source-agreements --accept-package-agreements
    
    # Check if installed
    if (Test-Path $netfxRefPath) {
        Write-Host "[OK] Installation successful!" -ForegroundColor Green
        exit 0
    } else {
        Write-Host "[WARNING] winget completed but package not detected" -ForegroundColor Yellow
    }
} else {
    Write-Host "[INFO] winget not available" -ForegroundColor Cyan
}

Write-Host ""
Write-Host "=========================================" -ForegroundColor Yellow
Write-Host "Manual Installation Required" -ForegroundColor Yellow
Write-Host "=========================================" -ForegroundColor Yellow
Write-Host ""
Write-Host "Download from: https://dotnet.microsoft.com/download/dotnet-framework/thank-you/net48-developer-pack-offline-installer" -ForegroundColor Cyan
Write-Host ""
Write-Host "Or search for 'ndp48-devpack-enu.exe'" -ForegroundColor Gray
Write-Host ""

$openBrowser = Read-Host "Open download page now? (Y/n)"
if ($openBrowser -ne 'n' -and $openBrowser -ne 'N') {
    Start-Process "https://dotnet.microsoft.com/download/dotnet-framework/net48"
}
