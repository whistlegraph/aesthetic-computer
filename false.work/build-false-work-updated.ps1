# Local Windows Build Script for false.work (SpiderLily)
# Run from Windows host to build UE5 project locally (not on GCP)

param(
    [switch]$NoSync,
    [string]$Platform = "Win64",
    [string]$Config = "Shipping",
    [string]$Version = (Get-Date -Format "yyyy.MM.dd-HHmm"),
    [ValidateSet("auto", "ultra", "high", "medium")]
    [string]$Quality = "auto",
    [switch]$AutoPackage
)

$ErrorActionPreference = "Stop"

# Quality preset suffix for filename
$QualitySuffix = if ($Quality -ne "auto") { "-$Quality" } else { "" }

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "false.work Local Windows Build" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Platform: $Platform"
Write-Host "Config: $Config"
Write-Host "Version: $Version"
Write-Host "Quality: $Quality" -ForegroundColor $(if ($Quality -eq "ultra") { "Magenta" } elseif ($Quality -eq "high") { "Yellow" } else { "White" })
Write-Host "AutoPackage: $AutoPackage"
Write-Host "NoSync: $NoSync" -ForegroundColor $(if ($NoSync) { "Green" } else { "Gray" })
Write-Host ""

# Configuration - Update these paths for your local setup
$UE5Path = "C:\Program Files\Epic Games\UE_5.6"
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

# Optional: Sync from Perforce if configured (skip if -NoSync)
if ($NoSync) {
    Write-Host "[SKIP] Perforce sync skipped (-NoSync flag). Using local changes." -ForegroundColor Yellow
    Write-Host ""
} elseif (Get-Command p4 -ErrorAction SilentlyContinue) {
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

# Create temporary GameUserSettings.ini for quality preset if not "auto"
$TempSettingsCreated = $false
$GameUserSettingsPath = "$ProjectRoot\Config\DefaultGameUserSettings.ini"

if ($Quality -ne "auto") {
    Write-Host "Applying $Quality quality preset..." -ForegroundColor Cyan
    
    # Quality level mapping: 0=Low, 1=Medium, 2=High, 3=Epic, 4=Cinematic
    $QualityLevel = switch ($Quality) {
        "medium" { 1 }
        "high"   { 3 }  # Epic
        "ultra"  { 4 }  # Cinematic
        default  { 3 }
    }
    
    $SettingsContent = @"
[ScalabilityGroups]
sg.ResolutionQuality=100
sg.ViewDistanceQuality=$QualityLevel
sg.AntiAliasingQuality=$QualityLevel
sg.ShadowQuality=$QualityLevel
sg.GlobalIlluminationQuality=$QualityLevel
sg.ReflectionQuality=$QualityLevel
sg.PostProcessQuality=$QualityLevel
sg.TextureQuality=$QualityLevel
sg.EffectsQuality=$QualityLevel
sg.FoliageQuality=$QualityLevel
sg.ShadingQuality=$QualityLevel

[/Script/Engine.GameUserSettings]
bUseDesiredScreenHeight=False
DesiredScreenWidth=1920
DesiredScreenHeight=1080
LastUserConfirmedDesiredScreenWidth=1920
LastUserConfirmedDesiredScreenHeight=1080
LastRecommendedScreenWidth=-1
LastRecommendedScreenHeight=-1
LastCPUBenchmarkResult=-1
LastGPUBenchmarkResult=-1
LastCPUBenchmarkSteps=0
LastGPUBenchmarkSteps=0
LastGPUBenchmarkMultiplier=1
bUseDynamicResolution=False
bUseHDRDisplayOutput=True
HDRDisplayOutputNits=1000
"@
    
    # Backup existing if present
    if (Test-Path $GameUserSettingsPath) {
        Copy-Item $GameUserSettingsPath "$GameUserSettingsPath.backup" -Force
    }
    
    Set-Content -Path $GameUserSettingsPath -Value $SettingsContent -Encoding UTF8
    $TempSettingsCreated = $true
    Write-Host "Created quality preset config: $GameUserSettingsPath" -ForegroundColor Green
}

# Run FMOD GenerateAssets commandlet before cooking
# This generates FMOD bank assets that normally get created when opening the editor
# Required for command-line builds: https://fmod.com/docs/2.03/unreal/user-guide.html#commandlet
$UnrealEditor = "$UE5Path\Engine\Binaries\Win64\UnrealEditor-Cmd.exe"
Write-Host ""
Write-Host "Running FMOD GenerateAssets commandlet..." -ForegroundColor Yellow
& $UnrealEditor $ProjectFile -run=FMODGenerateAssets
if ($LASTEXITCODE -ne 0) {
    Write-Warning "FMOD GenerateAssets commandlet returned non-zero exit code, continuing anyway..."
}

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
    # Cleanup temp settings on failure
    if ($TempSettingsCreated -and (Test-Path "$GameUserSettingsPath.backup")) {
        Move-Item "$GameUserSettingsPath.backup" $GameUserSettingsPath -Force
    } elseif ($TempSettingsCreated) {
        Remove-Item $GameUserSettingsPath -Force -ErrorAction SilentlyContinue
    }
    Write-Host ""
    Write-Host "[ERROR] Build failed!" -ForegroundColor Red
    exit 1
}

# Cleanup temp settings after successful build
if ($TempSettingsCreated) {
    if (Test-Path "$GameUserSettingsPath.backup") {
        Move-Item "$GameUserSettingsPath.backup" $GameUserSettingsPath -Force
    } else {
        Remove-Item $GameUserSettingsPath -Force -ErrorAction SilentlyContinue
    }
    Write-Host "Cleaned up temporary quality preset config" -ForegroundColor Gray
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
    $ArchiveName = "spiderlily-windows-$Version$QualitySuffix.zip"
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
