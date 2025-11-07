# Fix FMOD Callback Handler Bug
# This fixes a UnrealHeaderTool bug that generates incorrect include statements

Write-Host "Fixing UnrealHeaderTool FMOD bug..." -ForegroundColor Cyan

$files = @(
    "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\Intermediate\Build\Win64\UnrealGame\Inc\FMODStudio\UHT\FMODCallbackHandler.gen.cpp",
    "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio\Intermediate\Build\Win64\UnrealEditor\Inc\FMODStudio\UHT\FMODCallbackHandler.gen.cpp"
)

foreach ($file in $files) {
    if (Test-Path $file) {
        $content = Get-Content $file -Raw
        if ($content -match '#include "allbackHandler\.h"') {
            $content = $content -replace '#include "allbackHandler\.h"', '#include "FMODCallbackHandler.h"'
            Set-Content -Path $file -Value $content -NoNewline
            Write-Host "  [FIXED] $file" -ForegroundColor Green
        } else {
            Write-Host "  [OK] $file (already correct)" -ForegroundColor Gray
        }
    } else {
        Write-Host "  [SKIP] $file (not found)" -ForegroundColor Yellow
    }
}

Write-Host "`nDone! You can now continue the build." -ForegroundColor Green
