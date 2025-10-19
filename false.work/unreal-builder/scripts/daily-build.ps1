# Daily Build Script for Spider Lily
# This script runs on the build VM to sync from Perforce and build the project
# Usage: .\daily-build.ps1 [-Package] [-Upload]

param(
    [switch]$Package = $false,  # Package for distribution (slower, ~30-60 min)
    [switch]$Upload = $false,   # Upload to cloud storage
    [string]$BuildNumber = (Get-Date -Format "yyyyMMdd-HHmm")
)

$ErrorActionPreference = "Stop"
$StartTime = Get-Date

# Configuration
$WorkspaceRoot = "C:\Perforce"
$BuildOutputDir = "C:\Builds\$BuildNumber"
$UE5Path = "C:\Program Files\Epic Games\UE_5.6"
$ProjectFile = "$WorkspaceRoot\SpiderLily.uproject"
$LogFile = "$BuildOutputDir\build.log"

# Create output directory
New-Item -ItemType Directory -Force -Path $BuildOutputDir | Out-Null

# Start logging
Start-Transcript -Path $LogFile -Append

Write-Host ""
Write-Host "╔════════════════════════════════════════════════╗" -ForegroundColor Cyan
Write-Host "║     Spider Lily Daily Build - $BuildNumber     ║" -ForegroundColor Cyan
Write-Host "╚════════════════════════════════════════════════╝" -ForegroundColor Cyan
Write-Host ""

# Step 1: Sync from Perforce
Write-Host "[1/4] Syncing from Perforce..." -ForegroundColor Yellow
cd $WorkspaceRoot

try {
    $syncOutput = p4 sync -f 2>&1
    Write-Host $syncOutput
    if ($LASTEXITCODE -ne 0) {
        throw "Perforce sync failed with exit code $LASTEXITCODE"
    }
    Write-Host "✓ Perforce sync complete" -ForegroundColor Green
} catch {
    Write-Error "Failed to sync from Perforce: $_"
    Stop-Transcript
    exit 1
}

# Step 2: Clean previous build artifacts
Write-Host ""
Write-Host "[2/4] Cleaning build artifacts..." -ForegroundColor Yellow
Remove-Item -Path "$WorkspaceRoot\Binaries" -Recurse -Force -ErrorAction SilentlyContinue
Remove-Item -Path "$WorkspaceRoot\Intermediate" -Recurse -Force -ErrorAction SilentlyContinue
Write-Host "✓ Build artifacts cleaned" -ForegroundColor Green

# Step 3: Build the project
Write-Host ""
Write-Host "[3/4] Building SpiderLilyEditor..." -ForegroundColor Yellow
$BuildTool = "$UE5Path\Engine\Binaries\DotNET\UnrealBuildTool\UnrealBuildTool.exe"

try {
    & $BuildTool SpiderLilyEditor Win64 Development -Project="$ProjectFile" -WaitMutex
    if ($LASTEXITCODE -ne 0) {
        throw "Build failed with exit code $LASTEXITCODE"
    }
    Write-Host "✓ Build complete" -ForegroundColor Green
} catch {
    Write-Error "Build failed: $_"
    Stop-Transcript
    exit 1
}

# Step 4: Package for distribution (optional)
if ($Package) {
    Write-Host ""
    Write-Host "[4/4] Packaging for distribution..." -ForegroundColor Yellow
    Write-Host "  This will take 30-60 minutes..." -ForegroundColor Gray
    
    $RunUAT = "$UE5Path\Engine\Build\BatchFiles\RunUAT.bat"
    $PackageDir = "$BuildOutputDir\Package"
    
    try {
        & $RunUAT BuildCookRun `
            -project="$ProjectFile" `
            -platform=Win64 `
            -clientconfig=Development `
            -cook `
            -build `
            -stage `
            -pak `
            -archive `
            -archivedirectory="$PackageDir" `
            -clean `
            -unattended
            
        if ($LASTEXITCODE -ne 0) {
            throw "Packaging failed with exit code $LASTEXITCODE"
        }
        
        Write-Host "✓ Packaging complete" -ForegroundColor Green
        Write-Host "  Package location: $PackageDir" -ForegroundColor Gray
        
        # Create a README for the team
        $readme = @"
Spider Lily Build - $BuildNumber
Built: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")

To run the game:
1. Extract this folder
2. Run WindowsNoEditor\SpiderLily\Binaries\Win64\SpiderLily.exe

System Requirements:
- Windows 10/11 64-bit
- DirectX 11 compatible GPU
- 8GB RAM minimum

Build Machine: $env:COMPUTERNAME
"@
        $readme | Out-File -FilePath "$PackageDir\README.txt" -Encoding UTF8
        
    } catch {
        Write-Error "Packaging failed: $_"
        Stop-Transcript
        exit 1
    }
} else {
    Write-Host ""
    Write-Host "[4/4] Skipping packaging (use -Package flag to enable)" -ForegroundColor Gray
    
    # Copy editor binaries for testing
    Write-Host "  Copying editor binaries..." -ForegroundColor Gray
    Copy-Item -Path "$WorkspaceRoot\Binaries" -Destination "$BuildOutputDir\Binaries" -Recurse -Force
    Write-Host "✓ Editor binaries copied" -ForegroundColor Green
}

# Upload to cloud storage (optional)
if ($Upload) {
    Write-Host ""
    Write-Host "Uploading to cloud storage..." -ForegroundColor Yellow
    
    # TODO: Configure your upload method here
    # Examples:
    # - Google Cloud Storage: gsutil cp -r $BuildOutputDir gs://your-bucket/builds/
    # - AWS S3: aws s3 cp $BuildOutputDir s3://your-bucket/builds/ --recursive
    # - Azure Blob: az storage blob upload-batch -d builds -s $BuildOutputDir
    
    Write-Host "⚠ Upload not configured yet - see daily-build.ps1" -ForegroundColor Yellow
}

# Build summary
$EndTime = Get-Date
$Duration = $EndTime - $StartTime

Write-Host ""
Write-Host "╔════════════════════════════════════════════════╗" -ForegroundColor Green
Write-Host "║            Build Completed Successfully        ║" -ForegroundColor Green
Write-Host "╚════════════════════════════════════════════════╝" -ForegroundColor Green
Write-Host ""
Write-Host "Build Number:  $BuildNumber" -ForegroundColor Cyan
Write-Host "Duration:      $($Duration.ToString('hh\:mm\:ss'))" -ForegroundColor Cyan
Write-Host "Output:        $BuildOutputDir" -ForegroundColor Cyan
Write-Host "Log:           $LogFile" -ForegroundColor Cyan
Write-Host ""

# Copy binaries info for team
$buildInfo = @"
Spider Lily Build $BuildNumber
Completed: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Duration: $($Duration.ToString('hh\:mm\:ss'))

Editor Launch Command:
C:\Perforce\Binaries\Win64\UnrealEditor.exe C:\Perforce\SpiderLily.uproject

Recent Changes:
$(p4 changes -m 5 2>&1)
"@
$buildInfo | Out-File -FilePath "$BuildOutputDir\BUILD_INFO.txt" -Encoding UTF8

Stop-Transcript

exit 0
