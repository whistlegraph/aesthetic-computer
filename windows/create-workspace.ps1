# Create a new Perforce workspace for local Windows builds

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
    Write-Host "[WARNING] Workspace '$workspaceName' already exists" -ForegroundColor Yellow
    $response = Read-Host "Delete and recreate? (y/N)"
    if ($response -eq "y" -or $response -eq "Y") {
        Write-Host "Deleting existing workspace..." -ForegroundColor Yellow
        p4 client -d $workspaceName
    } else {
        Write-Host "Using existing workspace." -ForegroundColor Yellow
        $env:P4CLIENT = $workspaceName
        Write-Host ""
        Write-Host "Run sync-project.ps1 to sync files" -ForegroundColor Cyan
        exit 0
    }
}

# Get the view mapping from the existing workspace
Write-Host "Getting depot view from existing workspace..." -ForegroundColor Yellow
$viewMapping = p4 client -o spiderlily_build_workspace | Select-String "^\s*//SL_main/" | ForEach-Object {
    $line = $_.ToString().Trim()
    # Replace the client name in the mapping
    $line -replace '//spiderlily_build_workspace/', "//$workspaceName/"
}

if (!$viewMapping) {
    Write-Host "[ERROR] Could not get view mapping from existing workspace!" -ForegroundColor Red
    Write-Host "Manually create the workspace using P4V" -ForegroundColor Yellow
    exit 1
}

Write-Host "Creating new workspace..." -ForegroundColor Yellow
Write-Host ""

# Create workspace spec
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
$($viewMapping -join "`n")
"@

# Write spec to temp file and create client
$tempSpec = "$env:TEMP\p4client_$workspaceName.txt"
$clientSpec | Out-File -FilePath $tempSpec -Encoding ASCII

Get-Content $tempSpec | p4 client -i

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "[ERROR] Failed to create workspace!" -ForegroundColor Red
    Remove-Item $tempSpec -ErrorAction SilentlyContinue
    exit 1
}

Remove-Item $tempSpec -ErrorAction SilentlyContinue

Write-Host ""
Write-Host "[OK] Workspace created successfully!" -ForegroundColor Green
Write-Host ""
Write-Host "Workspace name: $workspaceName" -ForegroundColor Cyan
Write-Host "Root directory: $workspaceRoot" -ForegroundColor Cyan
Write-Host ""
Write-Host "Next step: Run sync-project.ps1 to sync the files" -ForegroundColor Yellow
Write-Host "  (Update it to use P4CLIENT = '$workspaceName')" -ForegroundColor Gray
Write-Host ""

# Update the environment for this session
$env:P4CLIENT = $workspaceName
Write-Host "Environment updated for this session." -ForegroundColor Green
Write-Host ""
