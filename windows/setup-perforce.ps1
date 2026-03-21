# Setup Perforce for SpiderLily on Windows
# Run this first to sync the project before building

param(
    [string]$WorkspacePath = "C:\Perforce\SpiderLily"
)

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Perforce Setup for SpiderLily" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

# Check if p4 is installed
if (!(Get-Command p4 -ErrorAction SilentlyContinue)) {
    Write-Host "[ERROR] Perforce (p4) not found!" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please install Perforce P4V client from:"
    Write-Host "  https://www.perforce.com/downloads/helix-visual-client-p4v"
    Write-Host ""
    Write-Host "Or install p4 command line:"
    Write-Host "  https://www.perforce.com/downloads/helix-command-line-client-p4"
    Write-Host ""
    exit 1
}

Write-Host "[OK] Found p4 command" -ForegroundColor Green
Write-Host ""

# Set Perforce environment
$P4PORT = "ssl:falsework.helixcore.io:1666"
$env:P4PORT = $P4PORT

Write-Host "Perforce Server: $P4PORT" -ForegroundColor Cyan
Write-Host ""

# Prompt for credentials
$P4USER = Read-Host "Enter your Perforce username"
$P4PASSWORD = Read-Host "Enter your Perforce password" -AsSecureString
$P4PASSWORD_Plain = [Runtime.InteropServices.Marshal]::PtrToStringAuto(
    [Runtime.InteropServices.Marshal]::SecureStringToBSTR($P4PASSWORD)
)

$env:P4USER = $P4USER
$env:P4PASSWD = $P4PASSWORD_Plain

# Test connection
Write-Host "Testing connection to $P4PORT..." -ForegroundColor Yellow
p4 info
if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "[ERROR] Could not connect to Perforce server!" -ForegroundColor Red
    Write-Host "Please check:"
    Write-Host "  - Server address: $P4PORT"
    Write-Host "  - Your username and password"
    Write-Host "  - Network/firewall settings"
    Write-Host ""
    exit 1
}

Write-Host ""
Write-Host "[OK] Connected to Perforce!" -ForegroundColor Green
Write-Host ""

# Create workspace directory
if (!(Test-Path $WorkspacePath)) {
    Write-Host "Creating workspace directory: $WorkspacePath" -ForegroundColor Yellow
    New-Item -ItemType Directory -Path $WorkspacePath -Force | Out-Null
}

# List available workspaces/clients
Write-Host "Your Perforce workspaces:" -ForegroundColor Cyan
p4 clients -u $P4USER

Write-Host ""
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "1. Create or select a Perforce workspace/client for SpiderLily"
Write-Host "2. Set your workspace root to: $WorkspacePath"
Write-Host "3. Sync the project: p4 sync"
Write-Host ""
Write-Host "Or use P4V GUI:"
Write-Host "  - Connection: $P4PORT"
Write-Host "  - User: $P4USER"
Write-Host "  - Create workspace mapping to: $WorkspacePath"
Write-Host ""
Write-Host "After syncing, update build-false-work.ps1:"
Write-Host "  Set ProjectRoot = `"$WorkspacePath`""
Write-Host ""

# Save connection info for future use
$p4ConfigPath = "$WorkspacePath\.p4config"
Write-Host "Saving Perforce config to: $p4ConfigPath" -ForegroundColor Yellow
@"
P4PORT=$P4PORT
P4USER=$P4USER
"@ | Out-File -FilePath $p4ConfigPath -Encoding ASCII

Write-Host "[OK] Setup complete!" -ForegroundColor Green
Write-Host ""
