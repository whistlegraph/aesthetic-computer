# Local Windows Build Script for false.work (SpiderLily)
# Run from Windows host to build UE5 project locally (not on GCP)

param(
    [string]$Platform = "Win64",
    [string]$Config = "Shipping",
    [string]$Version = (Get-Date -Format "yyyy.MM.dd-HHmm"),
    [switch]$AutoPackage
)

$ErrorActionPreference = "Stop"

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "false.work Local Windows Build" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Platform: $Platform"
Write-Host "Config: $Config"
Write-Host "Version: $Version"
Write-Host "AutoPackage: $AutoPackage"
Write-Host ""

# Configuration - Update these paths for your local setup
$UE5Path = "C:\Program Files\Epic Games\UE_5.7"
$ProjectRoot = "C:\Perforce\SpiderLily\SL_main"  # Updated to include SL_main subdirectory
$ProjectFile = "$ProjectRoot\SpiderLily.uproject"
$OutputDir = "$env:USERPROFILE\Desktop\SpiderLily-$Version"

# Check if UE5 exists
if (!(Test-Path $UE5Path)) {
    Write-Host "WARNING: UE5 not found at: $UE5Path" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Please update the `$UE5Path variable in this script to point to your UE5 installation."
    Write-Host ""
    Write-Host "Common locations:"
    Write-Host "  - C:\Program Files\Epic Games\UE_5.7"
    Write-Host "  - C:\Program Files\Epic Games\UE_5.6"
    Write-Host "  - C:\UE5"
    Write-Host ""
    exit 1
}

# Check if project exists
if (!(Test-Path $ProjectFile)) {
    Write-Host "WARNING: Project not found at: $ProjectFile" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Please update the `$ProjectRoot variable in this script to point to your SpiderLily project."
    Write-Host ""
    exit 1
}

Write-Host "[OK] Found UE5: $UE5Path" -ForegroundColor Green
Write-Host "[OK] Found Project: $ProjectFile" -ForegroundColor Green
Write-Host ""

# Optional: Sync from Perforce if configured
if (Get-Command p4 -ErrorAction SilentlyContinue) {
    if ($AutoPackage) {
        # Auto mode: sync without prompting
        Write-Host "Auto: Syncing from Perforce..." -ForegroundColor Yellow
        Set-Location $ProjectRoot
        p4 sync
        if ($LASTEXITCODE -ne 0) {
            Write-Warning "Perforce sync failed, continuing with existing files..."
        }
        # Show recent changes
        p4 changes -m 5 ...
        Write-Host ""
    } else {
        # Interactive mode: prompt user
        $response = Read-Host "Sync from Perforce? (y/N)"
        if ($response -eq "y" -or $response -eq "Y") {
            Write-Host "Syncing from Perforce..." -ForegroundColor Yellow
            Set-Location $ProjectRoot
            p4 sync
            if ($LASTEXITCODE -ne 0) {
                Write-Warning "Perforce sync failed, continuing with existing files..."
            }
            Write-Host ""
        }
    }
}

# Build
Write-Host "Building $Platform package..." -ForegroundColor Yellow
Write-Host "   This may take 10-30 minutes depending on your hardware..." -ForegroundColor Gray
Write-Host ""

$RunUAT = "$UE5Path\Engine\Build\BatchFiles\RunUAT.bat"

& $RunUAT BuildCookRun `
    "-project=$ProjectFile" `
    "-platform=$Platform" `
    "-clientconfig=$Config" `
    -cook `
    -build `
    -stage `
    -pak `
    -archive `
    "-archivedirectory=$OutputDir" `
    -noP4 `
    -utf8output `
    -NoLiveCoding `
    -nocompileeditor `
    -iterate `
    -IgnoreCookErrors

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "[ERROR] Build failed!" -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "[SUCCESS] Build complete!" -ForegroundColor Green
Write-Host ""
Write-Host "Output location:" -ForegroundColor Cyan
Write-Host "   $OutputDir"
Write-Host ""

# Optionally or automatically compress
if ($AutoPackage) {
    Write-Host ""
    Write-Host "Auto: Compressing build to ZIP..." -ForegroundColor Yellow
    $ArchiveName = "spiderlily-windows-$Version.zip"
    $ArchivePath = "$env:USERPROFILE\Desktop\$ArchiveName"

    if (Test-Path "C:\Program Files\7-Zip\7z.exe") {
        & "C:\Program Files\7-Zip\7z.exe" a -tzip "$ArchivePath" "$OutputDir\Windows\*" | Out-Default
    } else {
        Compress-Archive -Path "$OutputDir\Windows\*" -DestinationPath "$ArchivePath" -CompressionLevel Optimal -Force
    }

    if (Test-Path $ArchivePath) {
        $ArchiveSize = (Get-Item $ArchivePath).Length / 1MB
        $SizeRounded = [math]::Round($ArchiveSize, 2)
        Write-Host "Created: $ArchiveName ($SizeRounded MB)" -ForegroundColor Green
        Write-Host "   $ArchivePath"
    } else {
        Write-Host "Failed to create archive: $ArchivePath" -ForegroundColor Red
        exit 1
    }
} else {
    $response = Read-Host "Compress build to ZIP? (y/N)"
    if ($response -eq "y" -or $response -eq "Y") {
        Write-Host ""
        Write-Host "Compressing..." -ForegroundColor Yellow
        
        $ArchiveName = "SpiderLily-$Platform-$Version.zip"
        $BuildsDir = "$ProjectRoot\Builds"
        
        # Create Builds directory if it doesn't exist
        if (!(Test-Path $BuildsDir)) {
            New-Item -ItemType Directory -Path $BuildsDir -Force | Out-Null
        }
        
        $ArchivePath = "$BuildsDir\$ArchiveName"
        
        if (Test-Path "C:\Program Files\7-Zip\7z.exe") {
            & "C:\Program Files\7-Zip\7z.exe" a -tzip "$ArchivePath" "$OutputDir\*"
        } else {
            Compress-Archive -Path "$OutputDir\*" -DestinationPath "$ArchivePath" -CompressionLevel Optimal -Force
        }
        
        $ArchiveSize = (Get-Item $ArchivePath).Length / 1MB
        $SizeRounded = [math]::Round($ArchiveSize, 2)
        Write-Host "Created: $ArchiveName ($SizeRounded MB)" -ForegroundColor Green
        Write-Host "   $ArchivePath"
    }
}

Write-Host ""
Write-Host "Done!" -ForegroundColor Green
Write-Host ""
