# Build SpiderLily with automatic FMOD header fix
$ErrorActionPreference = "Continue"

$RunUAT = "C:\Program Files\Epic Games\UE_5.6\Engine\Build\BatchFiles\RunUAT.bat"
$Project = "C:\Perforce\SpiderLily\SL_main\SpiderLily.uproject"
$Output = "C:\Users\me\Desktop\SpiderLily-Build"

Write-Host "Starting build..." -ForegroundColor Cyan

& $RunUAT BuildCookRun -project="$Project" -platform=Win64 -clientconfig=Shipping -cook -build -stage -pak -archive -archivedirectory="$Output" -noP4 -utf8output -NoLiveCoding -nocompileeditor -iterate -IgnoreCookErrors -noshaderworker

if ($LASTEXITCODE -ne 0) {
    Write-Host "`nBuild failed! Checking for FMOD header bug..." -ForegroundColor Yellow
    
    # Fix any corrupted FMOD gen files
    $fmodFiles = Get-ChildItem -Path "C:\Perforce\SpiderLily\SL_main\Plugins\FMODStudio*\Intermediate\Build\Win64\UnrealGame\Inc" -Recurse -Filter "*.gen.cpp" | Where-Object { $_.Name -like "*CallbackHandler*" }
    
    if ($fmodFiles) {
        Write-Host "Found FMOD CallbackHandler files, applying fix..." -ForegroundColor Red
        
        foreach ($file in $fmodFiles) {
            # Remove read-only attribute
            attrib -R $file.FullName
            
            # Read and fix content
            $content = Get-Content -Path $file.FullName -Raw
            if ($content -match '"allbackHandler\.h"') {
                $fixed = $content -replace '"allbackHandler\.h"', '"FMODCallbackHandler.h"'
                Set-Content -Path $file.FullName -Value $fixed -NoNewline
                Write-Host "  Fixed: $($file.FullName)" -ForegroundColor Green
            }
        }
        
        Write-Host "`nRetrying build..." -ForegroundColor Cyan
        
        & $RunUAT BuildCookRun -project="$Project" -platform=Win64 -clientconfig=Shipping -cook -build -stage -pak -archive -archivedirectory="$Output" -noP4 -utf8output -NoLiveCoding -nocompileeditor -iterate -IgnoreCookErrors -noshaderworker
        
        if ($LASTEXITCODE -eq 0) {
            Write-Host "`nBuild SUCCESS after FMOD fix!" -ForegroundColor Green
        } else {
            Write-Host "`nBuild FAILED even after fix" -ForegroundColor Red
        }
    } else {
        Write-Host "No FMOD CallbackHandler files found" -ForegroundColor Yellow
    }
} else {
    Write-Host "`nBuild SUCCESS!" -ForegroundColor Green
}
