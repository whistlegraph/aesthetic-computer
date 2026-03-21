# Setup Build Machine for UE5 + Perforce
# Run this script on your Windows build machine (self-hosted runner)

param(
    [string]$P4Server = "",
    [string]$P4User = "",
    [string]$P4Workspace = "ue5_build_workspace",
    [string]$WorkspaceRoot = "D:\Perforce",
    [string]$BuildRoot = "D:\Builds",
    [string]$UE5Path = "C:\Program Files\Epic Games\UE_5.4"
)

Write-Host "=== UE5 Build Machine Setup ===" -ForegroundColor Cyan

# Check if running as Administrator
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $isAdmin) {
    Write-Warning "This script should be run as Administrator for best results"
}

# 1. Check Prerequisites
Write-Host "`n[1/6] Checking Prerequisites..." -ForegroundColor Yellow

$prerequisites = @{
    "Unreal Engine" = Test-Path $UE5Path
    "Perforce CLI (p4)" = (Get-Command p4 -ErrorAction SilentlyContinue) -ne $null
    "Git" = (Get-Command git -ErrorAction SilentlyContinue) -ne $null
    "Visual Studio" = Test-Path "C:\Program Files\Microsoft Visual Studio"
}

foreach ($prereq in $prerequisites.GetEnumerator()) {
    $status = if ($prereq.Value) { "✓" } else { "✗" }
    $color = if ($prereq.Value) { "Green" } else { "Red" }
    Write-Host "  $status $($prereq.Key)" -ForegroundColor $color
}

if ($prerequisites.Values -contains $false) {
    Write-Host "`nMissing prerequisites. Please install:" -ForegroundColor Red
    $prerequisites.GetEnumerator() | Where-Object { -not $_.Value } | ForEach-Object {
        Write-Host "  - $($_.Key)" -ForegroundColor Red
    }
    Write-Host "`nSee README.md for installation instructions"
    exit 1
}

# 2. Create Directory Structure
Write-Host "`n[2/6] Creating Directory Structure..." -ForegroundColor Yellow

$directories = @($WorkspaceRoot, $BuildRoot, "$BuildRoot\Logs")
foreach ($dir in $directories) {
    if (-not (Test-Path $dir)) {
        New-Item -ItemType Directory -Path $dir -Force | Out-Null
        Write-Host "  Created: $dir" -ForegroundColor Green
    } else {
        Write-Host "  Exists: $dir" -ForegroundColor Gray
    }
}

# 3. Configure Perforce
Write-Host "`n[3/6] Configuring Perforce..." -ForegroundColor Yellow

if ($P4Server -and $P4User) {
    Write-Host "  Setting Perforce environment variables..."
    
    # Set user environment variables
    [Environment]::SetEnvironmentVariable("P4PORT", $P4Server, "User")
    [Environment]::SetEnvironmentVariable("P4USER", $P4User, "User")
    [Environment]::SetEnvironmentVariable("P4CLIENT", $P4Workspace, "User")
    
    # Set for current session
    $env:P4PORT = $P4Server
    $env:P4USER = $P4User
    $env:P4CLIENT = $P4Workspace
    
    Write-Host "  P4PORT: $P4Server" -ForegroundColor Green
    Write-Host "  P4USER: $P4User" -ForegroundColor Green
    Write-Host "  P4CLIENT: $P4Workspace" -ForegroundColor Green
    
    # Test connection
    Write-Host "  Testing Perforce connection..."
    $p4Info = p4 info 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host "  ✓ Perforce connection successful" -ForegroundColor Green
    } else {
        Write-Host "  ✗ Perforce connection failed. Check credentials and password." -ForegroundColor Red
        Write-Host "  Run: p4 set P4PASSWD=your_password" -ForegroundColor Yellow
    }
} else {
    Write-Host "  Skipping Perforce configuration (no credentials provided)" -ForegroundColor Gray
    Write-Host "  Configure manually with:" -ForegroundColor Yellow
    Write-Host "    p4 set P4PORT=ssl:your-server.com:1666" -ForegroundColor Yellow
    Write-Host "    p4 set P4USER=build_account" -ForegroundColor Yellow
    Write-Host "    p4 set P4PASSWD=your_password" -ForegroundColor Yellow
}

# 4. Create Perforce Workspace
Write-Host "`n[4/6] Creating Perforce Workspace..." -ForegroundColor Yellow

if ($P4Server -and $P4User) {
    $workspaceExists = p4 clients | Select-String -Pattern $P4Workspace
    
    if (-not $workspaceExists) {
        Write-Host "  Creating workspace: $P4Workspace"
        
        $workspaceSpec = @"
Client: $P4Workspace
Owner: $P4User
Root: $WorkspaceRoot
Options: noallwrite noclobber nocompress unlocked nomodtime normdir
SubmitOptions: submitunchanged
LineEnd: local
View:
    //depot/... //$P4Workspace/...
"@
        
        $workspaceSpec | p4 client -i
        
        if ($LASTEXITCODE -eq 0) {
            Write-Host "  ✓ Workspace created successfully" -ForegroundColor Green
        } else {
            Write-Host "  ✗ Failed to create workspace" -ForegroundColor Red
        }
    } else {
        Write-Host "  Workspace already exists: $P4Workspace" -ForegroundColor Gray
    }
} else {
    Write-Host "  Skipping workspace creation (no P4 credentials)" -ForegroundColor Gray
}

# 5. Configure Windows Performance Settings
Write-Host "`n[5/6] Configuring Windows Performance Settings..." -ForegroundColor Yellow

if ($isAdmin) {
    try {
        # Disable Windows Defender real-time scanning for build directories
        Write-Host "  Adding build directories to Windows Defender exclusions..."
        Add-MpPreference -ExclusionPath $WorkspaceRoot -ErrorAction SilentlyContinue
        Add-MpPreference -ExclusionPath $BuildRoot -ErrorAction SilentlyContinue
        Add-MpPreference -ExclusionPath $UE5Path -ErrorAction SilentlyContinue
        Write-Host "  ✓ Defender exclusions added" -ForegroundColor Green
    } catch {
        Write-Host "  Could not modify Defender settings: $_" -ForegroundColor Yellow
    }
} else {
    Write-Host "  Run as Administrator to configure performance settings" -ForegroundColor Yellow
}

# 6. Create Build Scripts
Write-Host "`n[6/6] Creating Helper Scripts..." -ForegroundColor Yellow

# Quick sync script
$syncScript = @"
# Quick Perforce Sync
Write-Host "Syncing Perforce workspace..." -ForegroundColor Cyan
p4 sync
Write-Host "Done!" -ForegroundColor Green
"@
Set-Content -Path "$WorkspaceRoot\sync.ps1" -Value $syncScript
Write-Host "  Created: $WorkspaceRoot\sync.ps1" -ForegroundColor Green

# Environment info script
$infoScript = @"
Write-Host "Build Machine Info" -ForegroundColor Cyan
Write-Host "==================" -ForegroundColor Cyan
Write-Host "Hostname: `$env:COMPUTERNAME"
Write-Host "OS: `$(Get-WmiObject Win32_OperatingSystem | Select-Object -ExpandProperty Caption)"
Write-Host "CPU: `$(Get-WmiObject Win32_Processor | Select-Object -ExpandProperty Name -First 1)"
Write-Host "RAM: `$([math]::Round((Get-WmiObject Win32_ComputerSystem).TotalPhysicalMemory / 1GB, 2)) GB"
Write-Host ""
Write-Host "UE5 Path: $UE5Path"
Write-Host "Workspace: $WorkspaceRoot"
Write-Host "Builds: $BuildRoot"
Write-Host ""
Write-Host "Perforce Info:" -ForegroundColor Yellow
p4 info
"@
Set-Content -Path "$WorkspaceRoot\info.ps1" -Value $infoScript
Write-Host "  Created: $WorkspaceRoot\info.ps1" -ForegroundColor Green

# Summary
Write-Host "`n=== Setup Complete! ===" -ForegroundColor Green
Write-Host "`nNext Steps:"
Write-Host "1. Configure Perforce password: p4 set P4PASSWD=your_password"
Write-Host "2. Verify connection: p4 info"
Write-Host "3. Edit workspace view: p4 client $P4Workspace"
Write-Host "4. Initial sync: p4 sync"
Write-Host "5. Configure GitHub Actions runner on this machine"
Write-Host "`nHelper scripts created in: $WorkspaceRoot"
Write-Host "  - sync.ps1 - Quick workspace sync"
Write-Host "  - info.ps1 - Display build machine info"
