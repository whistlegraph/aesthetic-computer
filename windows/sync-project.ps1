# Sync SpiderLily Project from Perforce
# Uses the existing spiderlily_build_workspace

$ErrorActionPreference = "Stop"

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Syncing SpiderLily from Perforce" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

# Set Perforce environment
$env:P4PORT = "ssl:falsework.helixcore.io:1666"
$env:P4USER = "machine"
$env:P4CLIENT = "spiderlily_build_workspace"

Write-Host "Server: $env:P4PORT" -ForegroundColor Cyan
Write-Host "User: $env:P4USER" -ForegroundColor Cyan
Write-Host "Workspace: $env:P4CLIENT" -ForegroundColor Cyan
Write-Host ""

# Check workspace info
Write-Host "Workspace details:" -ForegroundColor Yellow
p4 client -o $env:P4CLIENT | Select-String "Root:|View:"
Write-Host ""

# Ask for confirmation
$response = Read-Host "Sync latest revision? This may take a while (y/N)"
if ($response -ne "y" -and $response -ne "Y") {
    Write-Host "Cancelled." -ForegroundColor Yellow
    exit 0
}

Write-Host ""
Write-Host "Syncing files..." -ForegroundColor Yellow
Write-Host "This may take several minutes depending on project size..." -ForegroundColor Gray
Write-Host ""

# Sync the workspace
p4 sync

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "[ERROR] Sync failed!" -ForegroundColor Red
    Write-Host "Try running: p4 sync -f" -ForegroundColor Yellow
    exit 1
}

Write-Host ""
Write-Host "[OK] Sync complete!" -ForegroundColor Green
Write-Host ""

# Find the project file
Write-Host "Locating SpiderLily.uproject..." -ForegroundColor Yellow
$projectFiles = Get-ChildItem -Path "C:\Perforce" -Filter "SpiderLily.uproject" -Recurse -ErrorAction SilentlyContinue

if ($projectFiles.Count -eq 0) {
    Write-Host "[WARNING] SpiderLily.uproject not found in C:\Perforce" -ForegroundColor Yellow
    Write-Host "Check the workspace mapping or project structure." -ForegroundColor Yellow
} else {
    $projectPath = $projectFiles[0].FullName
    $projectDir = $projectFiles[0].DirectoryName
    
    Write-Host "[OK] Found project at: $projectPath" -ForegroundColor Green
    Write-Host ""
    Write-Host "Update build-false-work.ps1 with:" -ForegroundColor Cyan
    Write-Host "  `$ProjectRoot = `"$projectDir`"" -ForegroundColor White
}

Write-Host ""
Write-Host "Next step: Run build-false-work.ps1 to build the project" -ForegroundColor Yellow
Write-Host ""
