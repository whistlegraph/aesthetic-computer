# Comprehensive fix for FMOD build issues
# Run as Administrator

#Requires -RunAsAdministrator

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Fixing FMOD Build Issues" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

$ProjectRoot = "C:\Perforce\SpiderLily\SL_main"
$FMODPlugin = "$ProjectRoot\Plugins\FMODStudio"

# 1. Fix all corrupted generated files
Write-Host "Step 1: Fixing corrupted generated includes..." -ForegroundColor Yellow

$corruptedFiles = @(
    "$FMODPlugin\Intermediate\Build\Win64\UnrealGame\Inc\FMODStudio\UHT\FMODCallbackHandler.gen.cpp",
    "$FMODPlugin\Intermediate\Build\Win64\UnrealEditor\Inc\FMODStudio\UHT\FMODCallbackHandler.gen.cpp"
)

foreach ($file in $corruptedFiles) {
    if (Test-Path $file) {
        $content = Get-Content -Path $file -Raw
        if ($content -match 'allbackHandler\.h') {
            $fixed = $content -replace '"allbackHandler\.h"', '"FMODCallbackHandler.h"'
            Set-Content -Path $file -Value $fixed -NoNewline
            Write-Host "[OK] Fixed: $file" -ForegroundColor Green
        }
    }
}

Write-Host ""

# 2. Check for missing FMOD libraries
Write-Host "Step 2: Checking FMOD libraries..." -ForegroundColor Yellow

$fmodLibs = @(
    "$FMODPlugin\Binaries\Win64\fmodL_vc.lib",
    "$FMODPlugin\Binaries\Win64\fmodstudioL_vc.lib",
    "$FMODPlugin\Binaries\Win64\fmodL.dll",
    "$FMODPlugin\Binaries\Win64\fmodstudioL.dll"
)

$missingLibs = @()
foreach ($lib in $fmodLibs) {
    if (!(Test-Path $lib)) {
        Write-Host "[MISSING] $lib" -ForegroundColor Red
        $missingLibs += $lib
    } else {
        Write-Host "[OK] Found: $(Split-Path $lib -Leaf)" -ForegroundColor Green
    }
}

Write-Host ""

if ($missingLibs.Count -gt 0) {
    Write-Host "[ERROR] Missing FMOD library files!" -ForegroundColor Red
    Write-Host ""
    Write-Host "These files should be in Perforce. Force sync:" -ForegroundColor Yellow
    Write-Host "  cd C:\Perforce" -ForegroundColor Gray
    Write-Host "  p4 sync -f //SpiderLily/Plugins/FMODStudio/Binaries/Win64/..." -ForegroundColor Gray
    Write-Host ""
    Write-Host "Or check if you have the correct Perforce workspace view." -ForegroundColor Yellow
} else {
    Write-Host "[OK] All FMOD libraries present" -ForegroundColor Green
}

Write-Host ""
Write-Host "Step 3: Preventing regeneration..." -ForegroundColor Yellow

# Mark the fixed files as read-only to prevent UHT from regenerating them
foreach ($file in $corruptedFiles) {
    if (Test-Path $file) {
        Set-ItemProperty -Path $file -Name IsReadOnly -Value $true
        Write-Host "[OK] Marked read-only: $(Split-Path $file -Leaf)" -ForegroundColor Green
    }
}

Write-Host ""
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Fix Complete" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

if ($missingLibs.Count -eq 0) {
    Write-Host "Next: Retry the build" -ForegroundColor Green
    Write-Host "  powershell.exe -ExecutionPolicy Bypass -File ""\\wsl.localhost\Ubuntu\home\me\aesthetic-computer\windows\build-false-work.ps1""" -ForegroundColor Gray
} else {
    Write-Host "Next: Sync missing FMOD files from Perforce first" -ForegroundColor Yellow
}

Write-Host ""
