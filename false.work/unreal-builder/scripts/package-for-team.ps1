# Quick Package Script for Team Testing
# Creates a standalone game build that can run without Unreal Editor
# This is what your team needs to test the game

param(
    [string]$Config = "Development",  # or "Shipping" for final builds
    [string]$BuildNumber = (Get-Date -Format "yyyyMMdd-HHmm")
)

$ErrorActionPreference = "Stop"

$WorkspaceRoot = "C:\Perforce"
$ProjectFile = "$WorkspaceRoot\SpiderLily.uproject"
$UE5Path = "C:\Program Files\Epic Games\UE_5.6"
$PackageOutput = "C:\Builds\Package_$BuildNumber"

Write-Host ""
Write-Host "╔════════════════════════════════════════════════╗" -ForegroundColor Cyan
Write-Host "║    Packaging Spider Lily for Team Testing     ║" -ForegroundColor Cyan
Write-Host "╚════════════════════════════════════════════════╝" -ForegroundColor Cyan
Write-Host ""
Write-Host "Configuration: $Config" -ForegroundColor Yellow
Write-Host "Output:        $PackageOutput" -ForegroundColor Yellow
Write-Host ""

Write-Host "Build Steps:" -ForegroundColor Cyan
Write-Host "WORKAROUND: Spider Lily has LiveCoding enabled" -ForegroundColor Yellow
Write-Host "Using two-step process to bypass LiveCoding hang" -ForegroundColor Yellow
Write-Host ""
Write-Host "[1/6] Cook Content (~18 min)" -ForegroundColor Yellow
Write-Host "[2/6] Compile Game Target (~5 min)" -ForegroundColor Yellow
Write-Host "[3/6] Stage Files (~5 min)" -ForegroundColor Yellow
Write-Host "[4/6] Create PAK Files (~5 min)" -ForegroundColor Yellow
Write-Host "[5/6] Archive Build (~2 min)" -ForegroundColor Yellow
Write-Host "[6/6] Create README (~1 min)" -ForegroundColor Yellow
Write-Host ""
Write-Host "Total estimated time: 35-40 minutes" -ForegroundColor Cyan
Write-Host ""

# Check for required files
$ProjectFile = "C:\Perforce\SpiderLily.uproject"
$UE5 = "C:\Program Files\Epic Games\UE_5.6"

if (-not (Test-Path $ProjectFile)) {
    Write-Host "ERROR: Project file not found at $ProjectFile" -ForegroundColor Red
    exit 1
}

if (-not (Test-Path "$UE5\Engine\Build\BatchFiles\RunUAT.bat")) {
    Write-Host "ERROR: UE5 not found at $UE5" -ForegroundColor Red
    exit 1
}

# Create output directory
$Timestamp = Get-Date -Format "yyyyMMdd-HHmm"
$Output = "C:\Builds\Package_$Timestamp"
New-Item -ItemType Directory -Force -Path $Output | Out-Null

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Starting Package Build (Workaround)" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Output: $Output" -ForegroundColor White
Write-Host ""

# STEP 1: Cook content first (bypasses LiveCoding)
Write-Host "[1/6] Cooking content (this bypasses LiveCoding)..." -ForegroundColor Green
$CookStart = Get-Date
& "$UE5\Engine\Binaries\Win64\UnrealEditor-Cmd.exe" $ProjectFile `
    -run=Cook `
    -TargetPlatform=Windows `
    -unattended `
    -stdout `
    -CrashForUAT `
    -NoLogTimes `
    -UTF8Output

if ($LASTEXITCODE -ne 0) {
    Write-Host "ERROR: Cooking failed!" -ForegroundColor Red
    exit 1
}
$CookDuration = (Get-Date) - $CookStart
Write-Host "Cooking completed in $($CookDuration.ToString('mm\:ss'))" -ForegroundColor Green
Write-Host ""

# STEP 2: Package using already-cooked content
Write-Host "[2/6] Building and packaging with cooked content..." -ForegroundColor Green
& "$UE5\Engine\Build\BatchFiles\RunUAT.bat" BuildCookRun `
    -project=$ProjectFile `
    -platform=Win64 `
    -clientconfig=Development `
    -skipcook `
    -build `
    -stage `
    -pak `
    -archive `
    -archivedirectory=$Output `
    -unattended `
    -noP4 `
    -NoLiveCoding
Write-Host ""
Write-Host "⏱️  Total estimated time: 30-60 minutes" -ForegroundColor Yellow
Write-Host "💡 Tip: Go grab a coffee! ☕" -ForegroundColor Gray
Write-Host ""

$StartTime = Get-Date

# Run the packaging process
$RunUAT = "$UE5Path\Engine\Build\BatchFiles\RunUAT.bat"

Write-Host "Starting packaging process..." -ForegroundColor Cyan
Write-Host ""

& $RunUAT BuildCookRun `
    -project="$ProjectFile" `
    -platform=Win64 `
    -clientconfig=$Config `
    -serverconfig=$Config `
    -cook `
    -build `
    -stage `
    -pak `
    -archive `
    -archivedirectory="$PackageOutput" `
    -unattended `
    -noP4 `
    -NoLiveCoding

if ($LASTEXITCODE -ne 0) {
    Write-Host ""
    Write-Host "❌ Packaging failed!" -ForegroundColor Red
    exit 1
}

$EndTime = Get-Date
$Duration = $EndTime - $StartTime

Write-Host ""
Write-Host "╔════════════════════════════════════════════════╗" -ForegroundColor Green
Write-Host "║         Packaging Complete!                    ║" -ForegroundColor Green
Write-Host "╚════════════════════════════════════════════════╝" -ForegroundColor Green
Write-Host ""
Write-Host "Duration: $($Duration.ToString('hh\:mm\:ss'))" -ForegroundColor Cyan
Write-Host "Output:   $PackageOutput" -ForegroundColor Cyan
Write-Host ""

# Find the actual game executable
$gameExe = Get-ChildItem -Path $PackageOutput -Filter "SpiderLily.exe" -Recurse | Select-Object -First 1

if ($gameExe) {
    Write-Host "✅ Game executable: $($gameExe.FullName)" -ForegroundColor Green
    Write-Host ""
    Write-Host "To test: Run this command:" -ForegroundColor Yellow
    Write-Host "  & `"$($gameExe.FullName)`"" -ForegroundColor White
} else {
    Write-Host "⚠️  Could not find SpiderLily.exe in package" -ForegroundColor Yellow
}

# Create README for the team
$readmePath = "$PackageOutput\README.txt"
$readme = @"
╔════════════════════════════════════════════════╗
║        Spider Lily - Build $BuildNumber        ║
╚════════════════════════════════════════════════╝

Built: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Configuration: $Config

HOW TO RUN:
-----------
1. Locate SpiderLily.exe in the WindowsNoEditor folder
2. Double-click SpiderLily.exe to launch the game
3. No Unreal Engine installation required!

SYSTEM REQUIREMENTS:
-------------------
- Windows 10/11 64-bit
- DirectX 12 compatible GPU
- 8GB RAM minimum
- 20GB free disk space

TROUBLESHOOTING:
---------------
If the game doesn't launch:
- Make sure DirectX is up to date
- Install Visual C++ Redistributables (included with Windows 10+)
- Check Windows Event Viewer for crash details

BUILD INFO:
-----------
Build Machine: $env:COMPUTERNAME
Build Duration: $($Duration.ToString('hh\:mm\:ss'))
"@

$readme | Out-File -FilePath $readmePath -Encoding UTF8

Write-Host ""
Write-Host "📦 Package contents:" -ForegroundColor Cyan
Get-ChildItem -Path $PackageOutput -Directory | ForEach-Object {
    Write-Host "  - $($_.Name)" -ForegroundColor Gray
}

Write-Host ""
Write-Host "Next steps for distribution:" -ForegroundColor Yellow
Write-Host "1. Compress $PackageOutput to a .zip file" -ForegroundColor White
Write-Host "2. Upload to cloud storage (Google Drive, Dropbox, etc.)" -ForegroundColor White
Write-Host "3. Share download link with team" -ForegroundColor White
Write-Host ""

exit 0
