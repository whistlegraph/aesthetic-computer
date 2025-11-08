# Update and Rebuild SpiderLily from Perforce
# Run this to pull latest changes and create a new build

param(
    [string]$Version = (Get-Date -Format "yyyy.MM.dd-HHmm")
)

$ErrorActionPreference = "Stop"

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Update & Rebuild SpiderLily" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

$ProjectRoot = "C:\Perforce\SpiderLily\SL_main"

# Check for Perforce
if (!(Get-Command p4 -ErrorAction SilentlyContinue)) {
    Write-Host "❌ ERROR: p4 command not found" -ForegroundColor Red
    Write-Host "   Please install Perforce P4V or P4 CLI"
    exit 1
}

# Sync latest from Perforce
Write-Host "📥 Syncing latest from Perforce..." -ForegroundColor Yellow
Set-Location $ProjectRoot

p4 sync ...
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Perforce sync failed!" -ForegroundColor Red
    exit 1
}

Write-Host "✓ Perforce sync complete" -ForegroundColor Green
Write-Host ""

# Show what changed
Write-Host "📋 Recent changes:" -ForegroundColor Cyan
p4 changes -m 5 ...
Write-Host ""

# Confirm rebuild
$response = Read-Host "Start new build with version $Version? (Y/n)"
if ($response -eq "n" -or $response -eq "N") {
    Write-Host "Build cancelled" -ForegroundColor Yellow
    exit 0
}

# Run build script
Write-Host ""
.\build-false-work.ps1 -Version $Version
