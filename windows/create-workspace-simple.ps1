# Create a new Perforce workspace for local Windows builds
# Simplified version with hardcoded depot mapping

$ErrorActionPreference = "Stop"

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Create Perforce Workspace for SpiderLily" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

# Set Perforce environment
$env:P4PORT = "ssl:falsework.helixcore.io:1666"
$env:P4USER = "machine"

# Get computer name for unique workspace
$hostname = $env:COMPUTERNAME.ToLower()
$workspaceName = "spiderlily_windows_$hostname"
$workspaceRoot = "C:\Perforce\SpiderLily"

Write-Host "Server: $env:P4PORT" -ForegroundColor Cyan
Write-Host "User: $env:P4USER" -ForegroundColor Cyan
Write-Host "New Workspace: $workspaceName" -ForegroundColor Cyan
Write-Host "Root Directory: $workspaceRoot" -ForegroundColor Cyan
Write-Host ""

# Create workspace directory
if (!(Test-Path $workspaceRoot)) {
    Write-Host "Creating directory: $workspaceRoot" -ForegroundColor Yellow
    New-Item -ItemType Directory -Path $workspaceRoot -Force | Out-Null
}

# Check if workspace already exists
$existingClient = p4 clients -e $workspaceName 2>$null
if ($existingClient) {
    Write-Host "[OK] Workspace '$workspaceName' already exists" -ForegroundColor Green
    $env:P4CLIENT = $workspaceName
    Write-Host ""
    Write-Host "Ready to sync! Run:" -ForegroundColor Cyan
    Write-Host "  p4 sync" -ForegroundColor White
    Write-Host ""
    Write-Host "Or continue with the build process." -ForegroundColor Yellow
    exit 0
}

Write-Host "Creating new workspace..." -ForegroundColor Yellow
Write-Host ""

# Create workspace spec with standard SpiderLily mapping
$clientSpec = @"
Client: $workspaceName

Owner: $env:P4USER

Host: $hostname

Description:
	Local Windows build workspace for SpiderLily

Root: $workspaceRoot

Options: noallwrite noclobber nocompress unlocked nomodtime normdir

SubmitOptions: submitunchanged

LineEnd: local

View:
	//SpiderLily/... //$workspaceName/...
"@

# Create client using piped input
$clientSpec | p4 client -i

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "[ERROR] Failed to create workspace!" -ForegroundColor Red
    Write-Host ""
    Write-Host "Try creating manually with P4V:" -ForegroundColor Yellow
    Write-Host "  1. Open P4V" -ForegroundColor Gray
    Write-Host "  2. Connection -> New Workspace" -ForegroundColor Gray
    Write-Host "  3. Name: $workspaceName" -ForegroundColor Gray
    Write-Host "  4. Root: $workspaceRoot" -ForegroundColor Gray
    Write-Host "  5. Add view: //SpiderLily/... //$workspaceName/..." -ForegroundColor Gray
    exit 1
}

Write-Host ""
Write-Host "[OK] Workspace created successfully!" -ForegroundColor Green
Write-Host ""
Write-Host "Workspace: $workspaceName" -ForegroundColor Cyan
Write-Host "Root: $workspaceRoot" -ForegroundColor Cyan
Write-Host ""
Write-Host "Next: Sync the files" -ForegroundColor Yellow
Write-Host "  Run: " -ForegroundColor Gray
Write-Host "    `$env:P4CLIENT = '$workspaceName'" -ForegroundColor White
Write-Host "    p4 sync" -ForegroundColor White
Write-Host ""

# Set for this session
$env:P4CLIENT = $workspaceName
Write-Host "[OK] Environment configured for this session" -ForegroundColor Green
Write-Host ""
