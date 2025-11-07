# Fix corrupted FMODCallbackHandler.gen.cpp file
# Run as Administrator

#Requires -RunAsAdministrator

Write-Host "=========================================" -ForegroundColor Cyan
Write-Host "Fixing Corrupted Generated File" -ForegroundColor Cyan
Write-Host "=========================================" -ForegroundColor Cyan
Write-Host ""

$corruptedFile = "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\Intermediate\Build\Win64\UnrealGame\Inc\FMODStudio\UHT\FMODCallbackHandler.gen.cpp"

if (Test-Path $corruptedFile) {
    Write-Host "Checking corrupted file..." -ForegroundColor Yellow
    
    # Read the file
    $content = Get-Content -Path $corruptedFile -Raw
    
    Write-Host "Content preview:" -ForegroundColor Gray
    Write-Host ($content.Substring(0, [Math]::Min(500, $content.Length))) -ForegroundColor DarkGray
    Write-Host ""
    
    # Fix the corrupted include
    if ($content -match 'allbackHandler\.h') {
        Write-Host "[FOUND] Corrupted include: 'allbackHandler.h'" -ForegroundColor Red
        
        $fixed = $content -replace '"allbackHandler\.h"', '"FMODCallbackHandler.h"'
        
        Set-Content -Path $corruptedFile -Value $fixed -NoNewline
        
        Write-Host "[OK] Fixed include to: 'FMODCallbackHandler.h'" -ForegroundColor Green
    } else {
        Write-Host "[INFO] No corrupted include found in this file" -ForegroundColor Cyan
    }
} else {
    Write-Host "[INFO] File doesn't exist yet - will be generated on next build" -ForegroundColor Cyan
}

Write-Host ""
Write-Host "Checking source header file..." -ForegroundColor Yellow

$headerFile = "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\Source\FMODStudio\Public\FMODCallbackHandler.h"
if (Test-Path $headerFile) {
    Write-Host "[OK] Source header exists: $headerFile" -ForegroundColor Green
} else {
    Write-Host "[ERROR] Source header missing: $headerFile" -ForegroundColor Red
    Write-Host ""
    Write-Host "This indicates a Perforce sync issue." -ForegroundColor Yellow
    Write-Host "Run: p4 sync -f //SpiderLily/...#head" -ForegroundColor Cyan
}

Write-Host ""
Write-Host "Next: Delete Intermediate again and rebuild" -ForegroundColor Cyan
Write-Host "  Remove-Item 'C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\Intermediate' -Recurse -Force" -ForegroundColor Gray
Write-Host ""
