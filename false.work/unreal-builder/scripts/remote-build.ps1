# Remote Build Script for Azure VM
# This script runs on the Azure VM to perform the build

param(
    [string]$p4Server,
    [string]$p4User,
    [string]$p4Password,
    [string]$p4Workspace,
    [string]$buildConfig = "Development",
    [string]$buildNumber = "0"
)

$ErrorActionPreference = "Stop"
$WorkspaceRoot = "D:\Perforce"
$BuildRoot = "D:\Builds"
$UE5Path = "C:\Program Files\Epic Games\UE_5.4"

Write-Host "=== UE5 Remote Build ===" -ForegroundColor Cyan
Write-Host "Build Number: $buildNumber"
Write-Host "Configuration: $buildConfig"

# Configure Perforce
Write-Host "`nConfiguring Perforce..." -ForegroundColor Yellow
$env:P4PORT = $p4Server
$env:P4USER = $p4User
$env:P4PASSWD = $p4Password
$env:P4CLIENT = $p4Workspace

# Test connection
p4 info
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to connect to Perforce"
    exit 1
}

# Sync workspace
Write-Host "`nSyncing workspace..." -ForegroundColor Yellow
cd $WorkspaceRoot
p4 sync -f
if ($LASTEXITCODE -ne 0) {
    Write-Error "Perforce sync failed"
    exit 1
}

# Find project file
$ProjectFile = Get-ChildItem -Path $WorkspaceRoot -Filter "*.uproject" -Recurse | Select-Object -First 1
if (-not $ProjectFile) {
    Write-Error "No .uproject file found in workspace"
    exit 1
}

Write-Host "Found project: $($ProjectFile.FullName)" -ForegroundColor Green

# Build
Write-Host "`nStarting build..." -ForegroundColor Yellow
& "$UE5Path\Engine\Build\BatchFiles\RunUAT.bat" BuildCookRun `
    -project="$($ProjectFile.FullName)" `
    -platform=Win64 `
    -clientconfig=$buildConfig `
    -cook `
    -build `
    -stage `
    -pak `
    -archive `
    -archivedirectory="$BuildRoot\$buildNumber" `
    -clean `
    -unattended

if ($LASTEXITCODE -ne 0) {
    Write-Error "Build failed"
    exit 1
}

Write-Host "`nBuild completed successfully!" -ForegroundColor Green
